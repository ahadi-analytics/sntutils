#' Coordinate Validation and Cleaning
#'
#' Validates coordinate data (points) and returns a cleaned sf object
#' with standardized columns and geometry hashes. Accepts either an sf
#' object with POINT geometry or a data.frame with longitude and latitude
#' columns. Checks include missing coordinates, DMS detection with
#' optional conversion (via the suggested 'parzer' package), range,
#' precision, flip detection, and country containment.
#'
#' @param data An `sf` object with POINT geometry or a `data.frame` containing
#'   coordinate columns.
#' @param name Character string identifying the dataset (for reporting).
#' @param lon_col Character string, column name for longitude (if `data`
#'   is not sf).
#' @param lat_col Character string, column name for latitude (if `data`
#'   is not sf).
#' @param adm0_sf Optional `sf` polygon/multipolygon of the country to
#'   validate points against (EPSG:4326 preferred). When provided,
#'   points are checked for being inside the country and optionally
#'   assigned admin columns if present.
#' @param geometry_crs Target CRS for output geometry and for operations
#'   with `adm0_sf` (default EPSG:4326). Lat/Lon attributes remain in
#'   EPSG:4326.
#' @param min_decimals Minimum decimal places required for lon/lat to be
#'   considered precise (default 4).
#' @param id_col Optional character name of a unique identifier column
#'   (e.g., facility ID). When provided, duplicate reporting includes
#'   IDs for coordinate-duplicate rows to aid de-dup decisions.
#' @param fix_issues Logical, whether to attempt automatic fixes (drop
#'   invalids, set CRS, standardize columns, remove duplicates, attempt
#'   flip fixes when `adm0_sf` is available).
#' @param quiet Logical, whether to suppress progress messages.
#'
#' @return A list with validation results. Key elements:
#'   - `passed`: Logical indicating if validation passed.
#'   - `issues`: Character vector of issues detected.
#'   - `spat_vec$points`: Cleaned sf object of points with `geometry_hash`.
#'   - `invalid_rows`: sf/data.frame of rows failing basic coordinate checks.
#'   - `duplicate_rows`: List with `row_duplicates` and
#'     `coordinate_duplicates` (if detected).
#'   - `column_dictionary`: Data frame mapping old to standardized column
#'     names.
#'
#' @examples
#' \dontrun{
#' # Example data with common issues: missing, DMS, flipped, imprecise
#' df <- data.frame(
#'   hf = paste0("HF_", 1:6),
#'   lon = c(-13.26077, "13\u00B015'38\"W", 8.5, -12.09057, NA, 181),
#'   lat = c(8.464283, "8\u00B027'51\"N", -13.26, 8.770261, 8.1, 0.1)
#' )
#'
#' # Simple rectangle as country polygon (e.g., Sierra Leone-ish bbox)
#' adm0_sf <- sf::st_as_sfc(
#'   sf::st_bbox(c(xmin = -14, ymin = 6.9, xmax = -10, ymax = 10.1),
#'               crs = sf::st_crs(4326))
#' ) |>
#'   sf::st_sf()
#'
#' # Validate and auto-fix issues (DMS parsing requires 'parzer')
#' res <- validate_process_coordinates(
#'   data = df,
#'   name = "hf_points",
#'   lon_col = "lon",
#'   lat_col = "lat",
#'   adm0_sf = adm0_sf,
#'   min_decimals = 4,
#'   fix_issues = TRUE,
#'   quiet = TRUE
#' )
#'
#' # Cleaned points
#' res$spat_vec$points
#' }
#'
#' @export
validate_process_coordinates <- function(
  data,
  name = "points",
  lon_col = NULL,
  lat_col = NULL,
  adm0_sf = NULL,
  geometry_crs = 4326,
  min_decimals = 4,
  id_col = NULL,
  fix_issues = TRUE,
  quiet = FALSE
) {
  if (fix_issues) {
    if (!quiet) {
      cli::cli_h2(
        "Validating and Cleaning Coordinates for {name}"
      )
    }
  } else {
    if (!quiet) {
      cli::cli_h2("Validating Coordinates for {name}")
    }
  }

  results <- list(
    name = name,
    timestamp = Sys.time(),
    passed = TRUE,
    issues = character(),
    spat_vec = list(),
    invalid_rows = NULL,
    duplicate_rows = NULL,
    column_dictionary = NULL
  )

  # Helper: append invalid rows as plain data.frame ---------------------------
  append_invalid <- function(existing, new_rows) {
    nr <- if (inherits(new_rows, "sf")) {
      sf::st_drop_geometry(new_rows)
    } else {
      as.data.frame(new_rows)
    }
    if (is.null(existing)) return(nr)
    dplyr::bind_rows(existing, nr)
  }

  # Step 0: Ensure we have an sf POINT object --------------------------------
  if (inherits(data, "sf")) {
    pts <- data
  } else {
    # Try to coerce to sf if a geometry column exists
    pts_try <- tryCatch(sf::st_as_sf(data), error = function(e) NULL)
    if (inherits(pts_try, "sf") && !is.null(sf::st_geometry(pts_try))) {
      pts <- pts_try
    } else {
      # Attempt to auto-detect lon/lat columns by common names
      nm <- names(data)
      nm_low <- tolower(nm)
      pick_name <- function(cands) {
        idx <- match(cands, nm_low)
        if (all(is.na(idx))) return(NULL)
        nm[stats::na.omit(idx)[1]]
      }
      lon_guess <- pick_name(c(
        "lon", "long", "longitude", "lng", "x"
      ))
      lat_guess <- pick_name(c(
        "lat", "latitude", "y"
      ))

      lon_col <- if (is.null(lon_col)) lon_guess else lon_col
      lat_col <- if (is.null(lat_col)) lat_guess else lat_col

      if (is.null(lon_col) || is.null(lat_col)) {
        cli::cli_abort(
          paste0(
            "When 'data' is not an sf object, lon/lat columns must be ",
            "provided or auto-detected. If starting from an sf, ensure ",
            "geometry is retained (e.g., use '.keep_all = TRUE' with ",
            "dplyr::distinct)."
          )
        )
      }
      if (!all(c(lon_col, lat_col) %in% names(data))) {
        cli::cli_abort("Longitude/Latitude columns not found in data")
      }

    # Raw strings for checks
    lon_chr <- as.character(data[[lon_col]])
    lat_chr <- as.character(data[[lat_col]])

    # Missing checks (NA or empty)
    miss_idx <- which(
      is.na(lon_chr) | lon_chr == "" | is.na(lat_chr) | lat_chr == ""
    )
    if (length(miss_idx) > 0) {
      results$issues <- c(
        results$issues,
        sprintf("%d rows with missing coordinates", length(miss_idx))
      )
      miss_rows <- data[miss_idx, , drop = FALSE]
      results$invalid_rows <- append_invalid(
        results$invalid_rows, miss_rows
      )
      if (fix_issues) {
        data <- data[-miss_idx, , drop = FALSE]
        lon_chr <- lon_chr[-miss_idx]
        lat_chr <- lat_chr[-miss_idx]
      } else {
        results$passed <- FALSE
      }
    }
    }

    # Attempt parse (DMS or otherwise) via parzer if available
    if (requireNamespace("parzer", quietly = TRUE)) {
      lon_try <- suppressWarnings(parzer::parse_lon(lon_chr))
      lat_try <- suppressWarnings(parzer::parse_lat(lat_chr))
      # overwrite where parse succeeded for both lon and lat
      has_val <- !is.na(lon_try) & !is.na(lat_try)
      lon_chr[has_val] <- as.character(lon_try[has_val])
      lat_chr[has_val] <- as.character(lat_try[has_val])
      n_conv <- sum(has_val)
      if (n_conv > 0 && !quiet) {
        cli::cli_alert_success(
          "Parsed coordinates with parzer for {n_conv} rows"
        )
      }
    }

    # Coerce to numeric
    lon <- suppressWarnings(as.numeric(lon_chr))
    lat <- suppressWarnings(as.numeric(lat_chr))
    non_numeric <- which(is.na(lon) | is.na(lat))
    if (length(non_numeric) > 0) {
      results$issues <- c(
        results$issues,
        sprintf("%d non-numeric coordinates", length(non_numeric))
      )
      bad_rows <- data[non_numeric, , drop = FALSE]
      results$invalid_rows <- append_invalid(
        results$invalid_rows, bad_rows
      )
      if (fix_issues) {
        data <- data[-non_numeric, , drop = FALSE]
        lon <- lon[-non_numeric]
        lat <- lat[-non_numeric]
      } else {
        results$passed <- FALSE
      }
    }

    # Write back numeric values for sf conversion
    if (nrow(data) > 0) {
      data[[lon_col]] <- lon
      data[[lat_col]] <- lat
    }

    # Build sf from remaining rows
    if (nrow(data) > 0) {
      pts <- sf::st_as_sf(
        data, coords = c(lon_col, lat_col), crs = 4326
      )
    } else {
      # Create an empty sf with 4326 CRS
      pts <- sf::st_sf(data.frame(), geometry = sf::st_sfc(crs = 4326))
    }
  }

  if (!quiet) {
    cli::cli_alert_info(
      "Processing {nrow(pts)} rows, {ncol(pts)} columns"
    )
  }

  # Step 1: CRS check ----------------------------------------------------------
  if (!quiet) cli::cli_progress_step("Checking CRS...")
  crs_info <- sf::st_crs(pts)
  if (is.na(crs_info$input)) {
    results$issues <- c(results$issues, "Missing CRS")
    # Try to infer from ranges
    bbox <- sf::st_bbox(pts)
    if (fix_issues && length(bbox) == 4) {
      if (
        bbox[1] >= -180 && bbox[3] <= 180 &&
          bbox[2] >= -90 && bbox[4] <= 90
      ) {
        pts <- sf::st_set_crs(pts, 4326)
        if (!quiet) {
          cli::cli_alert_success("Set CRS to WGS84")
        }
      } else {
        results$passed <- FALSE
      }
    } else if (!fix_issues) {
      results$passed <- FALSE
    }
  } else {
    if (!quiet) cli::cli_alert_success("CRS: {crs_info$input}")
  }

  # Ensure a 4326 copy for lon/lat based checks
  if (!is.na(sf::st_crs(pts)$epsg) && sf::st_crs(pts)$epsg != 4326) {
    pts_4326 <- sf::st_transform(pts, 4326)
    if (!quiet) {
      cli::cli_alert_info("Transformed points to EPSG:4326 for checks")
    }
  } else {
    pts_4326 <- pts
  }

  # Prepare adm0_sf if provided
  country_bbox <- NULL
  if (!is.null(adm0_sf)) {
    if (!inherits(adm0_sf, "sf")) {
      cli::cli_abort("'adm0_sf' must be an sf object")
    }
    if (is.na(sf::st_crs(adm0_sf)$input)) {
      cli::cli_abort("'adm0_sf' must have a valid CRS")
    }
    # Make valid and prepare versions: 4326 (bbox) and target CRS (join)
    adm0_sf <- sf::st_make_valid(adm0_sf)
    adm0_4326 <- if (!is.na(sf::st_crs(adm0_sf)$epsg) &&
                      sf::st_crs(adm0_sf)$epsg != 4326) {
      if (!quiet) {
        cli::cli_alert_info(
          "Transformed adm0_sf to EPSG:4326 for checks"
        )
      }
      sf::st_transform(adm0_sf, 4326)
    } else {
      adm0_sf
    }
    country_bbox <- sf::st_bbox(adm0_4326)
    adm0_target <- if (!is.na(sf::st_crs(adm0_sf)$epsg) &&
                        sf::st_crs(adm0_sf)$epsg != geometry_crs) {
      sf::st_transform(adm0_sf, geometry_crs)
    } else {
      adm0_sf
    }
  }

  # Step 2: Geometry/coordinate validation ------------------------------------
  if (!quiet) {
    cli::cli_progress_step(
      "Validating point geometries and ranges..."
    )
  }

  geom_type <- unique(sf::st_geometry_type(pts_4326, by_geometry = TRUE))
  not_points <- !all(geom_type %in% c("POINT", "MULTIPOINT"))
  if (not_points) {
    results$issues <- c(
      results$issues, "Geometry must be POINT/MULTIPOINT"
    )
    results$invalid_rows <- append_invalid(results$invalid_rows, pts_4326)
    if (!fix_issues) results$passed <- FALSE
    if (fix_issues) {
      # Attempt to extract centroids for non-point geometries
      pts_4326 <- suppressWarnings(
        sf::st_centroid(sf::st_make_valid(pts_4326))
      )
      if (!quiet) {
        cli::cli_alert_warning(
          "Converted non-point geometries to centroids"
        )
      }
    }
  }

  # Remove empty geometries
  empty_idx <- which(sf::st_is_empty(pts_4326))
  if (length(empty_idx) > 0) {
    results$issues <- c(
      results$issues,
      sprintf("%d empty geometries", length(empty_idx))
    )
    results$invalid_rows <- append_invalid(
      results$invalid_rows, pts_4326[empty_idx, ]
    )
    if (fix_issues) {
      pts_4326 <- pts_4326[-empty_idx, ]
      if (!quiet) {
        cli::cli_alert_warning(
          "Dropped {length(empty_idx)} empty geometries"
        )
      }
    } else {
      results$passed <- FALSE
    }
  }

  # Coordinate range checks in EPSG:4326 only
  crs_info <- sf::st_crs(pts_4326)
  if (!is.na(crs_info$epsg) && crs_info$epsg == 4326) {
    coords <- sf::st_coordinates(sf::st_geometry(pts_4326))
    invalid_idx <- which(
      !is.finite(coords[, 1]) | !is.finite(coords[, 2]) |
        coords[, 1] < -180 | coords[, 1] > 180 |
        coords[, 2] < -90  | coords[, 2] > 90
    )
    if (length(invalid_idx) > 0) {
      results$issues <- c(
        results$issues,
        sprintf("%d coordinates out of range", length(invalid_idx))
      )
      invalid_rows_sf <- pts_4326[invalid_idx, ]
      results$invalid_rows <- append_invalid(
        results$invalid_rows, invalid_rows_sf
      )
      if (fix_issues) {
        pts_4326 <- pts_4326[-invalid_idx, ]
        if (!quiet) {
          cli::cli_alert_warning(
            "Dropped {length(invalid_idx)} out-of-range points"
          )
        }
      } else {
        results$passed <- FALSE
      }
    }
  }

  # Step 2b: Decimal precision check -----------------------------------------
  if (nrow(pts_4326) > 0 && !is.na(sf::st_crs(pts_4326)$epsg) &&
      sf::st_crs(pts_4326)$epsg == 4326) {
    if (!quiet) {
      cli::cli_progress_step("Checking decimal precision...")
    }
    coords <- sf::st_coordinates(pts_4326$geometry)
    count_dp <- function(x) {
      # count digits after decimal point in non-scientific format
      ch <- format(x, scientific = FALSE, trim = TRUE)
      dec <- sub("^[^\\.]*\\.?(.*)$", "\\1", ch)
      has_dot <- grepl("\\.", ch, fixed = TRUE)
      nchar(dec) * as.integer(has_dot)
    }
    lon_dp <- count_dp(coords[, 1])
    lat_dp <- count_dp(coords[, 2])
    precise_ok <- lon_dp >= min_decimals & lat_dp >= min_decimals
    n_imprecise <- sum(!precise_ok, na.rm = TRUE)
    if (n_imprecise > 0) {
      results$issues <- c(
        results$issues,
        sprintf("%d imprecise coordinates (< %d dp)",
                n_imprecise, min_decimals)
      )
    }
    pts_4326$lon_dp <- lon_dp
    pts_4326$lat_dp <- lat_dp
    pts_4326$precision_ok <- precise_ok
  }

  # Step 2c: Flip detection using country bbox --------------------------------
  if (!is.null(country_bbox) && nrow(pts_4326) > 0) {
    if (!quiet) {
      cli::cli_progress_step("Detecting flipped coordinates...")
    }
    coords <- sf::st_coordinates(pts_4326$geometry)
    in_bbox <- coords[, 1] >= country_bbox["xmin"] &
      coords[, 1] <= country_bbox["xmax"] &
      coords[, 2] >= country_bbox["ymin"] &
      coords[, 2] <= country_bbox["ymax"]
    swapped_in_bbox <- coords[, 2] >= country_bbox["xmin"] &
      coords[, 2] <= country_bbox["xmax"] &
      coords[, 1] >= country_bbox["ymin"] &
      coords[, 1] <= country_bbox["ymax"]
    maybe_flipped <- (!in_bbox) & swapped_in_bbox
    n_maybe <- sum(maybe_flipped, na.rm = TRUE)
    pts_4326$in_bbox <- in_bbox
    pts_4326$maybe_flipped <- maybe_flipped
    if (n_maybe > 0) {
      results$issues <- c(
        results$issues,
        sprintf("%d points maybe flipped", n_maybe)
      )
      if (fix_issues) {
        # swap lon/lat for flagged rows
        coords2 <- coords
        coords2[maybe_flipped, 1] <- coords[maybe_flipped, 2]
        coords2[maybe_flipped, 2] <- coords[maybe_flipped, 1]
        new_geom <- sf::st_sfc(
          lapply(seq_len(nrow(coords2)), function(i) {
            sf::st_point(coords2[i, 1:2])
          }),
          crs = 4326
        )
        pts_4326$geometry <- new_geom
        if (!quiet) {
          cli::cli_alert_success(
            "Swapped lon/lat for {n_maybe} points"
          )
        }
      }
    }
  }

  # Step 3: Duplicate removal --------------------------------------------------
  if (fix_issues && nrow(pts_4326) > 0) {
    if (!quiet) cli::cli_progress_step("Removing duplicates...")

    # Exact row duplicates
    n_before <- nrow(pts_4326)
    row_dups_idx <- which(duplicated(pts_4326))
    if (length(row_dups_idx) > 0) {
      results$duplicate_rows <- c(
        results$duplicate_rows,
        list(row_duplicates = pts_4326[row_dups_idx, ])
      )
      pts_4326 <- dplyr::distinct(pts_4326)
      if (!quiet) {
        cli::cli_alert_success(
          "Removed {length(row_dups_idx)} duplicate rows"
        )
      }
      results$issues <- c(
        results$issues,
        sprintf("%d duplicate rows removed", length(row_dups_idx))
      )
    }

    # Coordinate duplicates (geometry-only)
    geom_binary <- sf::st_as_binary(pts_4326$geometry)
    geom_dups_idx <- which(duplicated(geom_binary))
    if (length(geom_dups_idx) > 0) {
      results$duplicate_rows <- c(
        results$duplicate_rows,
        list(coordinate_duplicates = pts_4326[geom_dups_idx, ])
      )
      # Enrich duplicate reporting with ID column if available
      if (!is.null(id_col) && id_col %in% names(pts_4326)) {
        pts_4326_dup <- sf::st_transform(pts_4326[geom_dups_idx, ], 4326)
        coords_dup <- sf::st_coordinates(pts_4326_dup)
        dup_info <- pts_4326[geom_dups_idx, ] |>
          dplyr::mutate(
            lon = coords_dup[, 1],
            lat = coords_dup[, 2]
          ) |>
          dplyr::select(dplyr::all_of(id_col), lon, lat) |>
          sf::st_drop_geometry()
        results$duplicate_rows$coordinate_duplicates_by_id <- dup_info
      }
      pts_4326 <- pts_4326[!duplicated(geom_binary), ]
      if (!quiet) {
        cli::cli_alert_success(
          "Removed {length(geom_dups_idx)} duplicate coordinates"
        )
      }
      results$issues <- c(
        results$issues,
        sprintf(
          "%d duplicate coordinates removed",
          length(geom_dups_idx)
        )
      )
    }
  }

  # Step 4: Country containment and admin assignment --------------------------
  if (!is.null(adm0_sf) && nrow(pts_4326) > 0) {
    # within test and optional admin columns
    join_cols <- intersect(
      c("adm0", "adm1", "adm2", "adm3"), names(adm0_sf)
    )
    if (!quiet) {
      cli::cli_progress_step(
        if (length(join_cols) > 0) {
          "Checking country containment and assigning admins..."
        } else {
          "Checking country containment..."
        }
      )
    }
    admin_small <- if (length(join_cols) > 0) {
      dplyr::select(adm0_target, dplyr::all_of(join_cols))
    } else {
      dplyr::select(adm0_target)
    }
    # transform points to target geometry CRS for join
    pts_geom <- if (!is.na(sf::st_crs(pts_4326)$epsg) &&
                      sf::st_crs(pts_4326)$epsg != geometry_crs) {
      sf::st_transform(pts_4326, geometry_crs)
    } else {
      pts_4326
    }
    pts_join <- sf::st_join(
      pts_geom, admin_small, join = sf::st_within, left = TRUE
    )
    # outside_country robust to missing admin columns
    if (length(join_cols) == 0) {
      rel <- sf::st_within(pts_geom, adm0_target)
      outside_country <- lengths(rel) == 0
      pts_join$outside_country <- outside_country
    } else {
      miss <- setdiff(c("adm0", "adm1", "adm2", "adm3"), names(pts_join))
      if (length(miss) > 0) {
        for (m in miss) pts_join[[m]] <- NA_character_
      }
      pts_join$outside_country <- rowSums(!is.na(
        pts_join[, c("adm0", "adm1", "adm2", "adm3"), drop = FALSE]
      )) == 0
    }
    n_out <- sum(pts_join$outside_country, na.rm = TRUE)
    if (n_out > 0) {
      results$issues <- c(
        results$issues,
        sprintf("%d points outside country", n_out)
      )
    }
    pts <- pts_join
  } else {
    # no admin join; ensure geometry in desired CRS for output
    pts <- if (!is.na(sf::st_crs(pts_4326)$epsg) &&
                sf::st_crs(pts_4326)$epsg != geometry_crs) {
      sf::st_transform(pts_4326, geometry_crs)
    } else {
      pts_4326
    }
  }

  # Step 5: Standardize columns and add geometry hash -------------------------
  if (fix_issues && nrow(pts) > 0) {
    # Try to add lon/lat columns (in 4326) to sf data for clarity
    if (!quiet) {
      cli::cli_progress_step(
        "Standardizing columns and hashing geometry..."
      )
    }
    add_lonlat <- TRUE
    # derive lon/lat from 4326 regardless of geometry_crs
    pts4326_tmp <- sf::st_transform(pts, 4326)
    coords_lonlat <- sf::st_coordinates(pts4326_tmp)
    pts <- pts |>
      dplyr::mutate(
        lon = coords_lonlat[, 1],
        lat = coords_lonlat[, 2]
      )

    # Drop diagnostic columns from final output
    diag_cols <- c(
      "lon_dp", "lat_dp", "precision_ok", "in_bbox",
      "maybe_flipped", "outside_country"
    )
    pts_out <- pts |>
      dplyr::select(-dplyr::any_of(diag_cols))

    # Geometry hash on clean output
    pts_out <- pts_out |>
      dplyr::mutate(
        geometry_hash = sntutils::vdigest(geometry, algo = "xxhash64")
      )

    # Reorder columns: keep other attrs, then lon/lat, geometry_hash, geometry
    other_cols <- setdiff(
      names(pts_out), c("lon", "lat", "geometry_hash", "geometry")
    )
    pts_out <- pts_out |>
      dplyr::select(
        dplyr::any_of(other_cols),
        dplyr::any_of(c("lon", "lat", "geometry_hash")),
        geometry
      )

    # Build column dictionary
    dict_rows <- list()
    for (col_name in names(pts_out)) {
      description <- if (col_name %in% c("lon", "lat") && add_lonlat) {
        ifelse(
          col_name == "lon",
          "Longitude (EPSG:4326)",
          "Latitude (EPSG:4326)"
        )
      } else if (col_name == "geometry_hash") {
        "Geometry unique identifier (xxhash64)"
      } else if (col_name == "geometry") {
        "Spatial geometry (sf POINT)"
      } else {
        "Data column"
      }
      dict_rows[[length(dict_rows) + 1]] <- data.frame(
        old_name = col_name,
        new_name = col_name,
        description = description,
        shapefiles = "points",
        stringsAsFactors = FALSE
      )
    }
    results$column_dictionary <- do.call(rbind, dict_rows)
  }

  # Save cleaned sf under spat_vec$points when fixing issues; otherwise
  # only on success intent
  if (fix_issues) {
    # Ensure pts_out exists even for empty input
    if (!exists("pts_out")) pts_out <- pts
    results$spat_vec$points <- pts_out
  }

  # Final check: geometry_hash uniqueness
  if (fix_issues && !is.null(results$spat_vec$points) &&
      nrow(results$spat_vec$points) > 0) {
    if (!quiet) {
      cli::cli_progress_step(
        msg = "Check hash uniqueness...",
        msg_done = "Point hashes are unique"
      )
    }
    gh <- results$spat_vec$points$geometry_hash
    if (any(duplicated(gh))) {
      dup_n <- sum(duplicated(gh))
      results$issues <- c(
        results$issues, sprintf("%d duplicate hashes", dup_n)
      )
      results$passed <- FALSE
    } else {
      cli::cli_process_done()
    }
  }

  # Summary --------------------------------------------------------------------
  if (!quiet) {
    if (results$passed) {
      if (fix_issues) {
        cli::cli_h2("Summary of Coordinate Validation and Cleaning")
        cli::cli_alert_info("Points retained: {nrow(pts)}")
      } else {
        cli::cli_h2("Summary of Coordinate Validation")
        cli::cli_alert_info(
          "Validation completed - no cleaning performed"
        )
      }
    } else {
      cli::cli_h2("\u2717 Issues found:")
      for (issue in unique(results$issues)) {
        cli::cli_text("  \u2022 {issue}")
      }
    }

    if (!is.null(results$invalid_rows)) {
      cli::cli_alert_info(
        paste0(
          "Invalid rows stored in results$invalid_rows ",
          "({nrow(results$invalid_rows)} rows)"
        )
      )
    }
    if (!is.null(results$duplicate_rows)) {
      total_dups <- 0
      if (!is.null(results$duplicate_rows$row_duplicates)) {
        total_dups <- total_dups +
          nrow(results$duplicate_rows$row_duplicates)
      }
      if (!is.null(results$duplicate_rows$coordinate_duplicates)) {
        total_dups <- total_dups +
          nrow(results$duplicate_rows$coordinate_duplicates)
      }
      cli::cli_alert_info(
        paste0(
          "Duplicate rows stored in results$duplicate_rows ",
          "({total_dups} total duplicates)"
        )
      )
    }
  }

  return(results)
}
