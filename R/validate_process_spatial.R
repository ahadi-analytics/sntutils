#' Spatial Vector Validation and Cleaning
#'
#' Validates spatial vector data and creates admin-level aggregations with
#' geometry hashes. Follows the same validation approach as
#' validate_process_coordinates() with modular checks and standardized outputs.
#'
#' @param shp An sf object to validate
#' @param name Character string identifying the dataset (for reporting)
#' @param adm0_col Character string, column name for country level
#' @param adm1_col Character string, column name for region/province level
#' @param adm2_col Character string, column name for district level
#' @param adm3_col Character string, column name for chiefdom/commune level
#' @param fix_issues Logical, whether to attempt automatic fixes
#' @param quiet Logical, whether to suppress progress messages
#' @param geometry_crs Target CRS for output geometry (default EPSG:4326).
#'   After CRS validation/fixes, geometries are transformed to this CRS
#'   for subsequent processing and output.
#' @param drop_z Logical, whether to drop Z/M coordinates before validation.
#'   Defaults to TRUE to avoid s2 warnings for 2D processing.
#'
#' @return A list with validation results. Key elements:
#'   - `issues`: Character vector of issues detected.
#'   - `invalid_rows`: sf/data.frame of rows failing basic geometry checks.
#'   - `checks$duplicate_rows`: Duplicates by admin names + geometry.
#'   - `checks$all_shared_locations`: All records sharing geometry.
#'   - `checks$all_shared_locations_ungrouped`: Only the duplicate rows at
#'     shared geometry.
#'   - `checks$invalid_geometries`: Rows with invalid geometries before fixing.
#'   - `checks$empty_geometries`: Rows with empty geometries.
#'   - `checks$self_intersecting`: Rows with self-intersecting geometries.
#'   - `column_dictionary`: Data frame with column_name and description.
#'   - `final_spat_vec`: List of admin-level aggregations
#'     (adm0, adm1, adm2, adm3).
#'   - `geometry_types`: Tibble summarizing geometry types and counts.
#'   - `spatial_extent`: Named vector with xmin, ymin, xmax, ymax.
#'
#' @examples
#' \dontrun{
#' # Basic validation with Sierra Leone data
#' result <- validate_process_spatial(
#'   shp = shp_raw,
#'   name = "Sierra Leone boundaries",
#'   adm0_col = "country",
#'   adm1_col = "region",
#'   adm2_col = "district",
#'   adm3_col = "chiefdom"
#' )
#'
#' # Access cleaned admin levels
#' shp_adm0 <- result$final_spat_vec$adm0
#' shp_adm1 <- result$final_spat_vec$adm1
#' shp_adm2 <- result$final_spat_vec$adm2
#' shp_adm3 <- result$final_spat_vec$adm3
#'
#' # Check validation status
#' if (length(result$issues) == 0) {
#'   cli::cli_alert_success("Ready for SNT analysis!")
#' }
#'
#' # Access invalid rows (if any)
#' invalid_rows <- result$invalid_rows
#'
#' # Access duplicate rows (if any)
#' duplicate_rows <- result$checks$duplicate_rows
#' shared_locations <- result$checks$all_shared_locations
#'
#' # Check column structure as data frame
#' col_dict <- result$column_dictionary
#' # Shows data frame with shapefile, old_name, new_name, description
#' print(col_dict)
#' }
#'
#' @export
validate_process_spatial <- function(
  shp,
  name = "shapefile",
  adm0_col = NULL,
  adm1_col = NULL,
  adm2_col = NULL,
  adm3_col = NULL,
  fix_issues = TRUE,
  quiet = FALSE,
  geometry_crs = 4326,
  drop_z = TRUE
) {
  # Initialize results structure
  results <- .init_spatial_validation_results(name, fix_issues, quiet)

  # Convert input data to sf format with validation
  sf_data <- .prepare_spatial_sf_input(
    shp,
    results,
    fix_issues,
    quiet,
    drop_z
  )
  results <- sf_data$results
  shp_clean <- sf_data$shp
  original_input_cols <- sf_data$original_cols

  if (nrow(shp_clean) == 0) {
    return(.finalize_spatial_results(results, fix_issues, quiet))
  }

  # Validate admin columns
  admin_mapping <- .validate_admin_columns(
    shp_clean,
    adm0_col,
    adm1_col,
    adm2_col,
    adm3_col,
    quiet
  )

  # Handle CRS validation and transformation
  crs_result <- .validate_and_transform_spatial_crs(
    shp_clean,
    geometry_crs,
    quiet
  )
  results$issues <- c(results$issues, crs_result$issues)
  shp_clean <- crs_result$shp

  # Validate geometries
  geom_validation <- .validate_spatial_geometry(
    shp_clean,
    results,
    fix_issues,
    quiet
  )
  results <- geom_validation$results
  shp_clean <- geom_validation$shp

  # Handle duplicates
  duplicate_result <- .handle_spatial_duplicates(
    shp_clean,
    admin_mapping,
    results,
    fix_issues,
    quiet
  )
  results <- duplicate_result$results
  shp_clean <- duplicate_result$shp

  # Standardize columns and create admin levels
  if (fix_issues && nrow(shp_clean) > 0) {
    results <- .standardize_spatial_output(
      shp_clean,
      admin_mapping,
      original_input_cols,
      results,
      duplicate_result,
      geometry_crs,
      quiet
    )
  }

  return(.finalize_spatial_results(results, fix_issues, quiet))
}

# Initialize spatial validation results structure
# @param name Character string identifying the dataset for reporting
# @param fix_issues Logical, whether automatic fixes will be attempted
# @param quiet Logical, whether to suppress progress messages
# @return List with empty results structure and user metadata
.init_spatial_validation_results <- function(name, fix_issues, quiet) {
  if (fix_issues) {
    if (!quiet) cli::cli_h2("Validating and Cleaning Spatial Vector for {name}")
  } else {
    if (!quiet) cli::cli_h2("Validating Spatial Vector for {name}")
  }

  list(
    name = name,
    timestamp = Sys.time(),
    user = get_user_identity(),
    issues = character(),
    final_spat_vec = list(),
    geometry_types = NULL,
    spatial_extent = NULL,
    checks = NULL,
    invalid_rows = NULL,
    duplicate_rows = NULL,
    column_dictionary = NULL
  )
}

# Prepare input data as sf object
# @param shp Input data (sf object or convertible to sf)
# @param results Results list to update with validation issues
# @param fix_issues Logical, whether to attempt automatic fixes
# @param quiet Logical, whether to suppress progress messages
# @param drop_z Logical, whether to drop Z/M coordinates before validation
# @return List with sf object, updated results, and original column names
.prepare_spatial_sf_input <- function(shp, results, fix_issues, quiet, drop_z) {
  original_input_cols <- names(shp)

  # Try to coerce to sf if needed
  if (!inherits(shp, "sf")) {
    shp_try <- tryCatch(
      sf::st_as_sf(shp),
      error = function(e) NULL
    )
    if (inherits(shp_try, "sf")) {
      shp <- shp_try
    } else {
      cli::cli_abort(
        "Input must be an sf object or convertible via st_as_sf()"
      )
    }
  }

  n_rows <- nrow(shp)
  n_cols <- ncol(shp)

  if (drop_z) {
    # Drop Z/M coordinates because downstream s2 operations require 2D geometry
    geom_classes <- base::vapply(
      sf::st_geometry(shp),
      function(geom) base::class(geom)[1],
      character(1)
    )
    has_z <- base::any(geom_classes %in% c("XYZ", "XYZM"))
    has_m <- base::any(geom_classes %in% c("XYM", "XYZM"))
    if (has_z || has_m) {
      if (!quiet) {
        cli::cli_alert_info("Dropping Z/M geometry dimensions for validation")
      }
      shp <- sf::st_zm(shp, drop = TRUE, what = "ZM")
    }
  }

  if (!quiet) {
    cli::cli_alert_info("Processing {n_rows} rows, {n_cols} columns")
  }

  list(
    shp = shp,
    results = results,
    original_cols = original_input_cols
  )
}

# Validate admin column existence
# @param shp_clean sf object with spatial data
# @param adm0_col Character, ADM0 column name (or NULL)
# @param adm1_col Character, ADM1 column name (or NULL)
# @param adm2_col Character, ADM2 column name (or NULL)
# @param adm3_col Character, ADM3 column name (or NULL)
# @param quiet Logical, whether to suppress progress messages
# @return List mapping standard admin names to actual column names
.validate_admin_columns <- function(
  shp_clean,
  adm0_col,
  adm1_col,
  adm2_col,
  adm3_col,
  quiet
) {
  available_cols <- names(shp_clean)
  admin_mapping <- list()

  # Map provided column names to standard names
  if (!is.null(adm0_col) && adm0_col %in% available_cols) {
    admin_mapping$adm0 <- adm0_col
  }
  if (!is.null(adm1_col) && adm1_col %in% available_cols) {
    admin_mapping$adm1 <- adm1_col
  }
  if (!is.null(adm2_col) && adm2_col %in% available_cols) {
    admin_mapping$adm2 <- adm2_col
  }
  if (!is.null(adm3_col) && adm3_col %in% available_cols) {
    admin_mapping$adm3 <- adm3_col
  }

  if (length(admin_mapping) == 0) {
    cli::cli_abort("No valid admin columns found. Check column names.")
  }

  if (!quiet) {
    # Format admin levels properly
    level_names <- paste(names(admin_mapping), collapse = ", ")
    if (length(admin_mapping) > 1) {
      # Replace last comma with "and"
      level_names <- sub(", ([^,]*)$", ", and \\1", level_names)
    }
    cli::cli_alert_success(
      "Found {length(admin_mapping)} admin level{?s}: {level_names}"
    )
  }

  admin_mapping
}

# Validate and transform CRS for spatial data
# @param shp sf object with spatial data
# @param geometry_crs Target CRS for output geometry
# @param quiet Logical, whether to suppress progress messages
# @return List with issues detected and transformed shapefile
.validate_and_transform_spatial_crs <- function(shp, geometry_crs, quiet) {
  if (!quiet) {
    cli::cli_progress_step("Checking CRS...")
  }

  issues <- character()
  crs_info <- sf::st_crs(shp)

  if (is.na(crs_info$input)) {
    issues <- c(issues, "Missing CRS")

    # Auto-detect if coordinates look like lat/lon
    bbox <- tryCatch(sf::st_bbox(shp), error = function(e) NULL)

    if (!is.null(bbox) && length(bbox) == 4) {
      if (
        bbox[1] >= -180 && bbox[3] <= 180 && bbox[2] >= -90 && bbox[4] <= 90
      ) {
        shp <- sf::st_set_crs(shp, 4326)
      }
    }
  } else {
    if (!quiet) cli::cli_alert_success("CRS: {crs_info$input}")
  }

  # Transform to target CRS if needed
  if (
    !is.na(sf::st_crs(shp)$epsg) &&
      !is.na(geometry_crs) &&
      sf::st_crs(shp)$epsg != geometry_crs
  ) {
    shp <- sf::st_transform(shp, geometry_crs)
    if (!quiet) {
      cli::cli_alert_info("Transformed geometries to EPSG:{geometry_crs}")
    }
  }

  if (!quiet) {
    cli::cli_progress_done()
  }

  list(issues = issues, shp = shp)
}

# Validate spatial geometries
# @param shp sf object with spatial data
# @param results Results list to update with issues
# @param fix_issues Logical, whether to attempt automatic fixes
# @param quiet Logical, whether to suppress progress messages
# @return List with updated results and validated shapefile
.validate_spatial_geometry <- function(
  shp,
  results,
  fix_issues,
  quiet
) {
  if (!quiet) {
    cli::cli_progress_step("Validating geometries...")
  }

  # Check for empty geometries
  empty_idx <- which(sf::st_is_empty(shp))

  if (length(empty_idx) > 0) {
    results$issues <- c(
      results$issues,
      sprintf("%d empty geometries", length(empty_idx))
    )
    results$invalid_rows <- .append_spatial_invalid_rows(
      results$invalid_rows,
      shp[empty_idx, ]
    )

    # Store for checks output
    if (!exists("checks", results)) {
      results$checks <- list()
    }
    results$checks$empty_geometries <- shp[empty_idx, ]

    if (fix_issues) {
      shp <- shp[-empty_idx, ]
    }
  }

  # Check geometry validity
  validity_check <- sf::st_is_valid(shp)
  invalid_idx <- which(!validity_check)

  if (length(invalid_idx) > 0) {
    results$issues <- c(
      results$issues,
      sprintf("%d invalid geometries", length(invalid_idx))
    )
    results$invalid_rows <- .append_spatial_invalid_rows(
      results$invalid_rows,
      shp[invalid_idx, ]
    )

    # Store for checks output
    if (!exists("checks", results)) {
      results$checks <- list()
    }
    results$checks$invalid_geometries <- shp[invalid_idx, ]

    # Check for specific geometry issues
    if (length(invalid_idx) > 0) {
      # Check for self-intersections
      self_intersect_idx <- tryCatch(
        {
          reason <- sf::st_is_valid(shp[invalid_idx, ], reason = TRUE)
          which(grepl("self-intersection", reason, ignore.case = TRUE))
        },
        error = function(e) integer(0)
      )

      if (length(self_intersect_idx) > 0) {
        results$checks$self_intersecting <-
          shp[invalid_idx[self_intersect_idx], ]
        results$issues <- c(
          results$issues,
          sprintf("%d self-intersecting geometries", length(self_intersect_idx))
        )
      }
    }

    if (fix_issues) {
      shp <- sf::st_make_valid(shp)
    }
  } else {
    if (!quiet) cli::cli_alert_success("All geometries valid")
  }

  # count and optionally remove interior holes per feature
  geoms <- sf::st_geometry(shp)
  hole_counts <- base::vapply(
    geoms,
    function(g) {
      base::sum(base::lengths(sf::st_geometry(g))) -
        base::length(sf::st_geometry(g))
    },
    integer(1)
  )
  hole_idx <- base::which(hole_counts > 0)

  if (base::length(hole_idx) > 0) {
    results$issues <- c(
      results$issues,
      sprintf(
        "%d geometries with interior holes",
        base::length(hole_idx)
      )
    )

    if (!exists("checks", results)) {
      results$checks <- list()
    }
    results$checks$geometries_with_holes <- shp[hole_idx, ]

    if (fix_issues) {
      if (!rlang::is_installed("nngeo")) {
        cli::cli_abort(
          c(
            "Package 'nngeo' is required to remove interior holes.",
            "i" = "Install nngeo or set fix_issues = FALSE."
          )
        )
      }
      shp <- nngeo::st_remove_holes(shp)
    }
  }

  # Check geometry types (for internal use only)
  geom_types <- unique(sf::st_geometry_type(shp))

  if (!quiet) {
    cli::cli_progress_done()
  }

  list(results = results, shp = shp)
}

# Handle duplicate detection and removal for spatial data
# @param shp sf object with spatial data
# @param admin_mapping List mapping admin levels to column names
# @param results Results list to update with issues
# @param fix_issues Logical, whether to remove duplicates
# @param quiet Logical, whether to suppress progress messages
# @return List with updated results, shapefile, and duplicate_info metadata
.handle_spatial_duplicates <- function(
  shp,
  admin_mapping,
  results,
  fix_issues,
  quiet
) {
  if (nrow(shp) == 0) {
    return(list(results = results, shp = shp))
  }

  if (!quiet) {
    cli::cli_progress_step("Checking duplicates...")
  }

  duplicate_info <- list(
    row_dups_idx = integer(0),
    geom_dups_idx = integer(0),
    admin_geom_dups_idx = integer(0),
    all_shared_locations_idx = integer(0),
    shared_locations_only_idx = integer(0)
  )

  # Check for exact row duplicates
  n_before <- nrow(shp)
  row_dup_check <- duplicated(shp)
  duplicate_info$row_dups_idx <- which(row_dup_check)

  if (length(duplicate_info$row_dups_idx) > 0) {
    results$issues <- c(
      results$issues,
      sprintf("%d duplicate rows", length(duplicate_info$row_dups_idx))
    )

    if (fix_issues) {
      shp <- dplyr::distinct(shp)
    }
  }

  # Check for geometry-only duplicates if we have remaining rows
  if (nrow(shp) > 0) {
    # Use geometry hashes for efficient duplicate detection
    geom_hashes <- tryCatch(
      {
        sntutils::vdigest(shp$geometry, algo = "xxhash64")
      },
      error = function(e) {
        # If hashing fails, return NULL
        NULL
      }
    )

    if (!is.null(geom_hashes)) {
      geom_dups_idx <- which(duplicated(geom_hashes))
      duplicate_info$geom_dups_idx <- geom_dups_idx

      if (length(geom_dups_idx) > 0) {
        # Find all records that share geometries
        dup_hashes <- geom_hashes[geom_dups_idx]
        duplicate_info$all_shared_locations_idx <-
          which(geom_hashes %in% dup_hashes)
        duplicate_info$shared_locations_only_idx <- geom_dups_idx

        results$issues <- c(
          results$issues,
          sprintf(
            "%d records at shared geometries (possible duplicates)",
            length(geom_dups_idx)
          )
        )

        if (fix_issues) {
          # Only remove pure geometry duplicates, not admin+geometry
          shp <- shp[!duplicated(geom_hashes), ]
        }
      }
    }
  }

  # Check for admin name + geometry duplicates
  if (
    length(admin_mapping) > 0 &&
      fix_issues &&
      nrow(shp) > 0 &&
      !is.null(geom_hashes)
  ) {
    admin_cols <- names(shp)[names(shp) %in% unlist(admin_mapping)]
    if (length(admin_cols) > 0) {
      # Create composite key from admin columns and geometry hash
      admin_key <- apply(
        shp[admin_cols],
        1,
        function(x) paste(x, collapse = "_")
      )
      admin_geom_key <- paste(admin_key, geom_hashes, sep = "||")
      admin_geom_dups_idx <- which(duplicated(admin_geom_key))
      duplicate_info$admin_geom_dups_idx <- admin_geom_dups_idx

      if (length(admin_geom_dups_idx) > 0) {
        results$issues <- c(
          results$issues,
          sprintf(
            "%d duplicates by admin names and geometry",
            length(admin_geom_dups_idx)
          )
        )

        if (fix_issues) {
          shp <- shp[!duplicated(admin_geom_key), ]
        }
      }
    }
  }

  if (!quiet) {
    cli::cli_progress_done()
  }

  list(results = results, shp = shp, duplicate_info = duplicate_info)
}

# Standardize output columns and create admin level aggregations
# @param shp sf object with validated spatial data
# @param admin_mapping List mapping admin levels to column names
# @param original_input_cols Character vector, original column names from input
# @param results Results list to update with final outputs
# @param duplicate_result List with duplicate detection metadata
# @param geometry_crs Target CRS for output
# @param quiet Logical, whether to suppress progress messages
# @return Updated results list with final outputs and admin levels
.standardize_spatial_output <- function(
  shp,
  admin_mapping,
  original_input_cols,
  results,
  duplicate_result,
  geometry_crs,
  quiet
) {
  if (!quiet) {
    cli::cli_progress_step("Standardizing admin columns...")
  }

  # Standardize column names
  shp_std <- .standardize_admin_columns(shp, admin_mapping)

  # Create geometry hash and admin GUIDs
  shp_std <- .create_spatial_identifiers(shp_std, admin_mapping, quiet)

  # Summarize geometry types and spatial extent for outputs
  geom_counts <- base::table(
    base::as.character(
      sf::st_geometry_type(shp_std, by_geometry = TRUE)
    )
  )
  results$geometry_types <- tibble::tibble(
    geometry_type = base::names(geom_counts),
    count = base::as.integer(geom_counts)
  )
  results$spatial_extent <- sf::st_bbox(shp_std)

  # Create admin level aggregations
  results$final_spat_vec <-
    .create_admin_aggregations(shp_std, admin_mapping, quiet)

  # remove holes across all admin levels when detected earlier
  if (
    !is.null(results$checks) &&
      !is.null(results$checks$geometries_with_holes) &&
      nrow(results$checks$geometries_with_holes) > 0
  ) {
    results$final_spat_vec <- lapply(
      results$final_spat_vec,
      nngeo::st_remove_holes
    )
  }

  # Create column dictionary
  results$column_dictionary <- .create_spatial_column_dictionary(
    results$final_spat_vec,
    admin_mapping
  )

  # Create checks output
  results$checks <- .create_spatial_checks_output(
    shp,
    shp_std,
    duplicate_result,
    results$checks
  )

  if (!quiet) {
    cli::cli_progress_done()
  }

  results
}

# Standardize admin column names
# @param shp sf object with spatial data
# @param admin_mapping List mapping standard names to actual column names
# @return sf object with standardized column names
.standardize_admin_columns <- function(shp, admin_mapping) {
  # Select relevant columns
  shp_clean <- shp |>
    dplyr::select(
      dplyr::any_of(unlist(admin_mapping)),
      geometry
    )

  # Rename columns to standard format
  for (std_name in names(admin_mapping)) {
    orig_name <- admin_mapping[[std_name]]
    if (orig_name != std_name && orig_name %in% names(shp_clean)) {
      shp_clean <- shp_clean |>
        dplyr::rename(!!std_name := !!orig_name)
    }
  }

  shp_clean
}

# Create geometry hashes and admin GUIDs
# @param shp sf object with standardized columns
# @param admin_mapping List mapping admin levels
# @param quiet Logical, whether to suppress progress messages
# @return sf object with added identifiers
.create_spatial_identifiers <- function(shp, admin_mapping, quiet) {
  if (!quiet) {
    cli::cli_progress_step("Creating geometry hashes and admin GUIDs...")
  }

  # Create geometry hash
  shp <- shp |>
    dplyr::mutate(
      geometry_hash = sntutils::vdigest(geometry, algo = "xxhash64")
    )

  # Create admin-level GUIDs for each available admin level
  available_admin_level <- names(admin_mapping)

  for (admin_level in available_admin_level) {
    guid_col <- paste0(admin_level, "_guid")

    if (admin_level %in% names(shp)) {
      # Get admin level number
      level_num <- as.numeric(stringr::str_extract(admin_level, "\\d"))

      # Build concatenation columns (all admin levels up to current)
      concat_cols <- paste0("adm", 0:level_num)
      concat_cols <- concat_cols[concat_cols %in% names(shp)]

      if (length(concat_cols) > 0) {
        shp <- shp |>
          dplyr::mutate(
            !!guid_col := sntutils::vdigest(
              paste(!!!rlang::syms(concat_cols), sep = "_"),
              algo = "xxhash32"
            )
          )
      }
    }
  }

  # Ensure proper column ordering
  admin_cols_ordered <- c()
  for (admin_level in c("adm0", "adm1", "adm2", "adm3")) {
    if (admin_level %in% names(shp)) {
      admin_cols_ordered <- c(
        admin_cols_ordered,
        admin_level,
        paste0(admin_level, "_guid")
      )
    }
  }

  shp |>
    dplyr::select(
      dplyr::any_of(admin_cols_ordered),
      geometry_hash,
      geometry
    )
}

# Create admin level aggregations
# @param shp sf object with standardized columns and identifiers
# @param admin_mapping List mapping admin levels
# @param quiet Logical, whether to suppress progress messages
# @return List of admin-level sf objects
.create_admin_aggregations <- function(shp, admin_mapping, quiet) {
  if (!quiet) {
    cli::cli_progress_step("Creating admin level aggregations...")
  }

  spat_vec <- list()

  # Get highest admin level
  highest_level <- max(as.numeric(stringr::str_extract(
    names(admin_mapping),
    "\\d"
  )))

  # Base level (finest resolution)
  base_level_name <- paste0("adm", highest_level)
  # Ensure base level has valid geometries
  if (any(!sf::st_is_valid(shp))) {
    shp <- sf::st_make_valid(shp)
  }
  spat_vec[[base_level_name]] <- shp

  # Create higher-level aggregations
  for (level in (highest_level - 1):0) {
    level_name <- paste0("adm", level)

    if (level_name %in% names(admin_mapping)) {
      # Get grouping columns
      group_cols <- paste0("adm", 0:level)
      group_cols <- group_cols[group_cols %in% names(shp)]

      if (length(group_cols) > 0) {
        # Get corresponding GUID columns
        guid_cols <- paste0(group_cols, "_guid")
        guid_cols <- guid_cols[guid_cols %in% names(shp)]

        # Create aggregation
        all_group_cols <- c(group_cols, guid_cols)

        # Ensure geometries are valid before aggregation
        shp_valid <- shp
        if (any(!sf::st_is_valid(shp_valid))) {
          shp_valid <- sf::st_make_valid(shp_valid)
        }

        agg_shp <- shp_valid |>
          dplyr::group_by(dplyr::across(dplyr::all_of(all_group_cols))) |>
          dplyr::summarise(
            geometry = sf::st_union(geometry),
            .groups = "drop"
          ) |>
          dplyr::mutate(
            geometry_hash = sntutils::vdigest(geometry, algo = "xxhash64")
          )

        # Ensure proper column ordering
        level_admin_cols <- c()
        for (admin_level in paste0("adm", 0:level)) {
          if (admin_level %in% names(agg_shp)) {
            level_admin_cols <- c(level_admin_cols, admin_level)
            guid_col <- paste0(admin_level, "_guid")
            if (guid_col %in% names(agg_shp)) {
              level_admin_cols <- c(level_admin_cols, guid_col)
            }
          }
        }

        agg_shp <- agg_shp |>
          dplyr::select(
            dplyr::any_of(level_admin_cols),
            geometry_hash,
            geometry
          ) |>
          sf::st_sf()

        spat_vec[[level_name]] <- agg_shp
      }
    }
  }

  if (!quiet) {
    cli::cli_progress_done()
  }

  spat_vec
}

# Create column dictionary for spatial outputs
# @param spat_vec List of admin-level sf objects
# @param admin_mapping List mapping admin levels to original column names
# @return Data.frame with unique columns and their descriptions
.create_spatial_column_dictionary <- function(spat_vec, admin_mapping) {
  # Collect all unique columns across all levels
  all_cols <- character()
  col_to_levels <- list()

  # Track which shapefile levels have each column
  for (level_name in names(spat_vec)) {
    cols_in_level <- names(spat_vec[[level_name]])
    for (col_name in cols_in_level) {
      if (!col_name %in% all_cols) {
        all_cols <- c(all_cols, col_name)
      }
      if (is.null(col_to_levels[[col_name]])) {
        col_to_levels[[col_name]] <- character()
      }
      col_to_levels[[col_name]] <- c(col_to_levels[[col_name]], level_name)
    }
  }

  # Create collapsed dictionary
  dict_rows <- list()

  for (col_name in all_cols) {
    # Get levels that have this column
    levels_with_col <- unique(col_to_levels[[col_name]])
    levels_str <- paste(sort(levels_with_col), collapse = ", ")

    # Create description
    description <- if (stringr::str_detect(col_name, "^adm\\d$")) {
      paste(
        "Administrative level",
        stringr::str_extract(col_name, "\\d"),
        "name"
      )
    } else if (stringr::str_detect(col_name, "^adm\\d_guid$")) {
      paste(
        "Administrative level",
        stringr::str_extract(col_name, "\\d"),
        "unique identifier (xxhash32)"
      )
    } else if (col_name == "geometry_hash") {
      "Geometry unique identifier (xxhash64)"
    } else if (col_name == "geometry") {
      "Spatial geometry (sf geometry column)"
    } else {
      "Data column"
    }

    dict_rows[[length(dict_rows) + 1]] <- data.frame(
      shapefile = levels_str,
      column_name = col_name,
      description = description,
      stringsAsFactors = FALSE
    )
  }

  # Combine and sort by column hierarchy
  result <- do.call(rbind, dict_rows)

  # Custom sort to put admin columns in order
  col_order <- c(
    "adm0",
    "adm0_guid",
    "adm1",
    "adm1_guid",
    "adm2",
    "adm2_guid",
    "adm3",
    "adm3_guid",
    "geometry_hash",
    "geometry"
  )

  # Reorder based on predefined order
  result$order <- match(result$column_name, col_order)
  result$order[is.na(result$order)] <- 999
  result <- result[order(result$order), ]
  result$order <- NULL
  rownames(result) <- NULL

  result
}

# Create standardized checks output for spatial data
# @param shp_original Original sf object before processing
# @param shp_final Final processed sf object
# @param duplicate_result List with duplicate detection metadata
# @param existing_checks Existing checks from geometry validation
# @return List of validation check results
.create_spatial_checks_output <- function(
  shp_original,
  shp_final,
  duplicate_result,
  existing_checks
) {
  checks <- if (!is.null(existing_checks)) existing_checks else list()
  duplicate_info <- duplicate_result$duplicate_info

  # Add duplicate rows check
  if (!is.null(duplicate_info)) {
    # Row duplicates
    if (length(duplicate_info$row_dups_idx) > 0) {
      checks$duplicate_rows <- shp_original[duplicate_info$row_dups_idx, ]
    }

    # Geometry-only duplicates
    if (length(duplicate_info$shared_locations_only_idx) > 0) {
      checks$all_shared_locations <- shp_original[
        duplicate_info$shared_locations_only_idx,
      ]
    }

    # All records at shared locations
    if (length(duplicate_info$all_shared_locations_idx) > 0) {
      checks$all_shared_locations_ungrouped <- shp_original[
        duplicate_info$all_shared_locations_idx,
      ]
    }

    # Admin + geometry duplicates
    if (length(duplicate_info$admin_geom_dups_idx) > 0) {
      checks$admin_geometry_duplicates <- shp_original[
        duplicate_info$admin_geom_dups_idx,
      ]
    }
  }

  # Remove empty checks
  checks <- Filter(
    function(x) {
      (inherits(x, "sf") || inherits(x, "data.frame")) && nrow(x) > 0
    },
    checks
  )

  if (length(checks) == 0) {
    return(NULL)
  }

  checks
}

# Finalize spatial results and generate summary
# @param results Complete results list from validation process
# @param fix_issues Logical, whether fixes were attempted
# @param quiet Logical, whether to suppress summary output
# @return Cleaned results list with summary printed (if not quiet)
.finalize_spatial_results <- function(results, fix_issues, quiet) {
  # Clean up intermediate results
  results$invalid_rows <- NULL
  results$duplicate_rows <- NULL

  if (!quiet) {
    .print_spatial_validation_summary(results, fix_issues)
  }

  results
}

# Print spatial validation summary
# @param results Complete results list from validation process
# @param fix_issues Logical, whether fixes were attempted
# @return NULL (side effect: prints summary to console)
.print_spatial_validation_summary <- function(results, fix_issues) {
  issues_summary <- results$issues

  if (length(issues_summary) == 0) {
    if (fix_issues) {
      cli::cli_h2("Summary of Spatial Vector Validation and Cleaning")
      if (length(results$final_spat_vec) > 0) {
        cli::cli_alert_info(
          "Created {length(results$final_spat_vec)} admin levels:"
        )
        for (level_name in names(results$final_spat_vec)) {
          n_features <- nrow(results$final_spat_vec[[level_name]])
          cli::cli_text("  - {level_name}: {n_features} features")
        }
      }
    } else {
      cli::cli_h2("Summary of Spatial Vector Validation")
      cli::cli_alert_info("Validation completed - no cleaning performed")
    }
  } else {
    cli::cli_h2("Issues found:")
    for (issue in unique(issues_summary)) {
      cli::cli_alert_warning(issue)
    }
    cli::cli_text("")
  }

  if (fix_issues && length(results$issues) > 0) {
    .print_spatial_fixed_actions(results$issues)
  }

 # Report column dictionary
  if (!is.null(results$column_dictionary)) {
    cli::cli_alert_info(
      "All admin shapefiles are available in results$final_spat_vec"
    )
  }

   # Report column dictionary
  if (!is.null(results$column_dictionary)) {
    cli::cli_alert_info(
      "Column dictionary available in results$column_dictionary"
    )
  }

  if (!is.null(results$spatial_extent)) {
    cli::cli_alert_info(
      "Spatial extent available in results$spatial_extent"
    )
  }

  if (!is.null(results$geometry_types)) {
    cli::cli_alert_info(
      "Geometry types table available in results$geometry_types"
    )
  }

    # Report checks if any
  if (!is.null(results$checks) && length(results$checks) > 0) {
    cli::cli_alert_info(
      paste0(
        "Detailed checks available in results$checks ",
        "({length(results$checks)} check{?s})"
      )
    )
  }

  # Report column dictionary
  if (!is.null(results$column_dictionary)) {
    cli::cli_alert_info(
      "Column dictionary available in results$column_dictionary"
    )
  }

  cat("\n")

}

# Print actions taken during spatial fixes
# @param issues Character vector of detected issues
# @return NULL (side effect: prints fixed actions to console)
.print_spatial_fixed_actions <- function(issues) {
  fixed_actions <- character()

  if (any(grepl("Missing CRS", issues))) {
    fixed_actions <- c(fixed_actions, "Set CRS to WGS84 (EPSG:4326)")
  }
  if (any(grepl("invalid geometries", issues))) {
    fixed_actions <- c(
      fixed_actions,
      "Fixed invalid geometries with st_make_valid()"
    )
  }
  if (any(grepl("empty geometries", issues))) {
    fixed_actions <- c(fixed_actions, "Removed empty geometries")
  }
  if (any(grepl("self-intersecting geometries", issues))) {
    fixed_actions <- c(fixed_actions, "Fixed self-intersecting geometries")
  }
  hole_msgs <- issues[grepl("interior holes", issues, ignore.case = TRUE)]

  if (length(hole_msgs) > 0) {
    fixed_actions <- c(
      fixed_actions,
      "Removed interior holes from geometries"
    )
  }
  if (any(grepl("duplicate rows", issues))) {
    fixed_actions <- c(fixed_actions, "Removed duplicate rows")
  }
  if (any(grepl("geometry duplicates", issues))) {
    fixed_actions <- c(fixed_actions, "Removed geometry duplicates")
  }
  if (any(grepl("admin\\+geometry duplicates", issues))) {
    fixed_actions <- c(
      fixed_actions,
      "Removed admin name + geometry duplicates"
    )
  }
  if (any(grepl("records at shared geometries", issues))) {
    fixed_actions <- c(
      fixed_actions,
      "Detected records at shared geometries (flagged for review)"
    )
  }

  if (length(fixed_actions) > 0) {
    cli::cli_h2("Fixed issues:")
    for (action in fixed_actions) {
      cli::cli_alert_success(action)
    }
    cli::cli_text("")
  }
}

# Helper function to append invalid rows for spatial data
# @param existing Existing invalid rows data.frame (or NULL)
# @param new_rows New invalid rows (sf object) to append
# @return Combined sf object or data.frame
.append_spatial_invalid_rows <- function(existing, new_rows) {
  if (is.null(existing)) {
    return(new_rows)
  }

  # Ensure both are sf objects or both are data.frames
  if (inherits(existing, "sf") && inherits(new_rows, "sf")) {
    return(dplyr::bind_rows(existing, new_rows))
  } else {
    # Convert both to data.frames
    existing_df <- if (inherits(existing, "sf")) {
      sf::st_drop_geometry(existing)
    } else {
      existing
    }
    new_df <- if (inherits(new_rows, "sf")) {
      sf::st_drop_geometry(new_rows)
    } else {
      new_rows
    }
    return(dplyr::bind_rows(existing_df, new_df))
  }
}
