#' Coordinate Validation and Cleaning
#'
#' Validates coordinate data (points) and returns a cleaned sf object
#' with standardized columns. Accepts either an sf
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
#'   the function tests that all points fall within the given shape.
#'   No admin attributes are joined to the points.
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
#'   - `issues`: Character vector of issues detected.
#'   - `final_points_df`: Cleaned sf object of points.
#'   - `invalid_rows`: sf/data.frame of rows failing basic coordinate checks.
#'   - `checks$duplicate_rows`: Duplicates by ID + coordinates
#'     (standardized sf).
#'   - `checks$all_shared_locations`: All records sharing a location.
#'   - `checks$all_shared_locations_ungrouped`: Only the duplicate rows
#'     at shared locations.
#'   - `checks$coordinates_adm0`: Points outside ADM0 with original
#'     lon/lat restored from geometry.
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
#' res$final_points_df
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
  # Initialize results structure
  results <- .init_validation_results(name, fix_issues, quiet)

  # Convert input data to sf format with validation
  sf_data <- .prepare_sf_input(
    data, lon_col, lat_col, results, fix_issues, quiet
  )
  results <- sf_data$results
  pts <- sf_data$pts
  original_input_cols <- sf_data$original_cols

  if (nrow(pts) == 0) {
    return(.finalize_results(results, fix_issues, quiet))
  }

  # Handle CRS validation and transformation
  crs_result <- .validate_and_transform_crs(pts, geometry_crs, quiet)
  results$issues <- c(results$issues, crs_result$issues)
  pts_4326 <- crs_result$pts_4326

  # Prepare ADM0 polygon if provided
  adm0_data <- .prepare_adm0_polygon(adm0_sf, geometry_crs, quiet)

  # Validate coordinate geometry and ranges
  coord_validation <- .validate_coordinate_geometry(
    pts_4326, adm0_data, results, fix_issues, quiet
  )
  results <- coord_validation$results
  pts_4326 <- coord_validation$pts

  # Check decimal precision
  precision_result <- .check_decimal_precision(
    pts_4326, min_decimals, results, quiet
  )
  results <- precision_result$results
  pts_4326 <- precision_result$pts

  # Detect and fix flipped coordinates
  flip_result <- .detect_and_fix_flips(
    pts_4326, adm0_data, results, fix_issues, quiet
  )
  results <- flip_result$results
  pts_4326 <- flip_result$pts

  # Handle duplicates
  duplicate_result <- .handle_duplicates(
    pts_4326, id_col, results, fix_issues, quiet
  )
  results <- duplicate_result$results
  pts_4326 <- duplicate_result$pts

  # Check ADM0 containment
  adm0_result <- .check_adm0_containment(
    pts_4326, adm0_data, geometry_crs, results, quiet
  )
  results <- adm0_result$results
  pts <- adm0_result$pts

  # Standardize output and create final results
  if (fix_issues && nrow(pts) > 0) {
    results <- .standardize_output(
      pts, original_input_cols, results, duplicate_result
    )
  }

  return(.finalize_results(results, fix_issues, quiet))
}

# Initialize validation results structure
# @param name Character string identifying the dataset for reporting
# @param fix_issues Logical, whether automatic fixes will be attempted
# @param quiet Logical, whether to suppress progress messages
# @return List with empty results structure and user metadata
.init_validation_results <- function(name, fix_issues, quiet) {
  if (fix_issues) {
    if (!quiet) cli::cli_h2("Validating and Cleaning Coordinates for {name}")
  } else {
    if (!quiet) cli::cli_h2("Validating Coordinates for {name}")
  }

  list(
    name = name,
    timestamp = Sys.time(),
    user = get_user_identity(),
    issues = character(),
    final_points_df = NULL,
    checks = NULL,
    invalid_rows = NULL,
    missing_coord_rows = NULL,
    duplicate_rows = NULL,
    coordinates_adm0 = NULL,
    column_dictionary = NULL
  )
}

# Prepare input data as sf object
# @param data Input data (sf object or data.frame)
# @param lon_col Character, longitude column name (if data is data.frame)
# @param lat_col Character, latitude column name (if data is data.frame)
# @param results Results list to update with validation issues
# @param fix_issues Logical, whether to attempt automatic fixes
# @param quiet Logical, whether to suppress progress messages
# @return List with sf object, updated results, and original column names
.prepare_sf_input <- function(data, lon_col, lat_col, results,
                               fix_issues, quiet) {
  original_input_cols <- names(data)

  if (inherits(data, "sf")) {
    return(list(
      pts = data, results = results, original_cols = original_input_cols
    ))
  }

  # Try to coerce to sf if geometry column exists
  pts_try <- tryCatch(sf::st_as_sf(data), error = function(e) NULL)
  if (inherits(pts_try, "sf") && !is.null(sf::st_geometry(pts_try))) {
    return(list(
      pts = pts_try, results = results, original_cols = original_input_cols
    ))
  }

  # Auto-detect coordinate columns
  coords_detected <- .detect_coordinate_columns(data, lon_col, lat_col)
  lon_col <- coords_detected$lon_col
  lat_col <- coords_detected$lat_col

  # Process coordinate strings and create sf object
  coord_processing <- .process_coordinate_strings(
    data, lon_col, lat_col, results, fix_issues
  )

  list(
    pts = coord_processing$pts,
    results = coord_processing$results,
    original_cols = original_input_cols
  )
}

# Detect longitude and latitude columns
# @param data Data.frame to search for coordinate columns
# @param lon_col Character, provided longitude column name (or NULL)
# @param lat_col Character, provided latitude column name (or NULL)
# @return List with detected lon_col and lat_col names
.detect_coordinate_columns <- function(data, lon_col, lat_col) {
  nm <- names(data)
  nm_low <- tolower(nm)

  pick_name <- function(candidates) {
    idx <- match(candidates, nm_low)
    if (all(is.na(idx))) return(NULL)
    nm[stats::na.omit(idx)[1]]
  }

  lon_guess <- pick_name(c("lon", "long", "longitude", "lng", "x"))
  lat_guess <- pick_name(c("lat", "latitude", "y"))

  lon_col <- if (is.null(lon_col)) lon_guess else lon_col
  lat_col <- if (is.null(lat_col)) lat_guess else lat_col

  if (is.null(lon_col) || is.null(lat_col)) {
    cli::cli_abort(paste0(
      "When 'data' is not an sf object, lon/lat columns must be ",
      "provided or auto-detected. If starting from an sf, ensure ",
      "geometry is retained (e.g., use '.keep_all = TRUE' with ",
      "dplyr::distinct)."
    ))
  }

  if (!all(c(lon_col, lat_col) %in% names(data))) {
    cli::cli_abort("Longitude/Latitude columns not found in data")
  }

  list(lon_col = lon_col, lat_col = lat_col)
}

# Process coordinate strings with DMS parsing and validation
# @param data Data.frame with coordinate columns
# @param lon_col Character, longitude column name
# @param lat_col Character, latitude column name
# @param results Results list to update with validation issues
# @param fix_issues Logical, whether to attempt automatic fixes
# @return List with sf object and updated results
.process_coordinate_strings <- function(data, lon_col, lat_col, results,
                                        fix_issues) {
  lon_chr <- as.character(data[[lon_col]])
  lat_chr <- as.character(data[[lat_col]])

  # Handle missing coordinates
  missing_result <- .handle_missing_coordinates(
    data, lon_col, lat_col, lon_chr, lat_chr, results, fix_issues
  )
  results <- missing_result$results
  data <- missing_result$data
  lon_chr <- missing_result$lon_chr
  lat_chr <- missing_result$lat_chr

  # Parse DMS if parzer is available
  dms_result <- .parse_dms_coordinates(lon_chr, lat_chr)
  lon_chr <- dms_result$lon_chr
  lat_chr <- dms_result$lat_chr

  # Convert to numeric and handle non-numeric
  numeric_result <- .convert_to_numeric_coords(
    data, lon_col, lat_col, lon_chr, lat_chr, results, fix_issues
  )
  results <- numeric_result$results
  data <- numeric_result$data
  lon <- numeric_result$lon
  lat <- numeric_result$lat

  # Calculate decimal precision from original strings
  precision_data <- .calculate_decimal_precision(lon_chr, lat_chr)
  data[["lon_dp_text"]] <- precision_data$lon_dp_text
  data[["lat_dp_text"]] <- precision_data$lat_dp_text

  # Create sf object handling NA coordinates
  pts <- .create_sf_from_coords(data, lon_col, lat_col, lon, lat)

  list(pts = pts, results = results)
}

# Handle missing coordinate values
# @param data Data.frame with coordinate data
# @param lon_col Character, longitude column name
# @param lat_col Character, latitude column name
# @param lon_chr Character vector, longitude values as strings
# @param lat_chr Character vector, latitude values as strings
# @param results Results list to update with issues
# @param fix_issues Logical, whether to set missing values to NA
# @return List with updated data, results, and coordinate strings
.handle_missing_coordinates <- function(data, lon_col, lat_col, lon_chr,
                                        lat_chr, results, fix_issues) {
  miss_idx <- which(
    is.na(lon_chr) | lon_chr == "" | is.na(lat_chr) | lat_chr == ""
  )

  if (length(miss_idx) > 0) {
    results$issues <- c(
      results$issues,
      sprintf("%d rows with missing coordinates", length(miss_idx))
    )
    results$invalid_rows <- .append_invalid_rows(
      results$invalid_rows, data[miss_idx, , drop = FALSE]
    )
    # Track missing coordinate indices for later exclusion
    results$missing_coord_idx <- miss_idx
    # Store the rows with missing coordinates
    results$missing_coord_rows <- data[miss_idx, , drop = FALSE]

    if (fix_issues) {
      data[[lon_col]][miss_idx] <- NA
      data[[lat_col]][miss_idx] <- NA
      lon_chr[miss_idx] <- NA_character_
      lat_chr[miss_idx] <- NA_character_
    }
  }

  list(results = results, data = data, lon_chr = lon_chr, lat_chr = lat_chr)
}

# Parse DMS coordinates if parzer package is available
# @param lon_chr Character vector, longitude values as strings
# @param lat_chr Character vector, latitude values as strings
# @return List with parsed coordinate strings (converted from DMS if applicable)
.parse_dms_coordinates <- function(lon_chr, lat_chr) {
  if (requireNamespace("parzer", quietly = TRUE)) {
    lon_try <- suppressWarnings(parzer::parse_lon(lon_chr))
    lat_try <- suppressWarnings(parzer::parse_lat(lat_chr))

    has_val <- !is.na(lon_try) & !is.na(lat_try)
    lon_chr[has_val] <- as.character(lon_try[has_val])
    lat_chr[has_val] <- as.character(lat_try[has_val])
  }

  list(lon_chr = lon_chr, lat_chr = lat_chr)
}

# Convert coordinate strings to numeric
# @param data Data.frame with coordinate data
# @param lon_col Character, longitude column name
# @param lat_col Character, latitude column name
# @param lon_chr Character vector, longitude values as strings
# @param lat_chr Character vector, latitude values as strings
# @param results Results list to update with issues
# @param fix_issues Logical, whether to set non-numeric values to NA
# @return List with updated data, results, and numeric coordinates
.convert_to_numeric_coords <- function(data, lon_col, lat_col, lon_chr,
                                       lat_chr, results, fix_issues) {
  lon <- suppressWarnings(as.numeric(lon_chr))
  lat <- suppressWarnings(as.numeric(lat_chr))

  non_numeric <- which(is.na(lon) | is.na(lat))
  
  # Exclude rows already identified as having missing coordinates
  if (!is.null(results$missing_coord_idx)) {
    non_numeric <- setdiff(non_numeric, results$missing_coord_idx)
  }
  
  if (length(non_numeric) > 0) {
    results$issues <- c(
      results$issues,
      sprintf("%d non-numeric coordinates", length(non_numeric))
    )
    results$invalid_rows <- .append_invalid_rows(
      results$invalid_rows, data[non_numeric, , drop = FALSE]
    )

    if (fix_issues) {
      data[[lon_col]][non_numeric] <- NA
      data[[lat_col]][non_numeric] <- NA
      lon[non_numeric] <- NA_real_
      lat[non_numeric] <- NA_real_
    }
  }

  if (nrow(data) > 0) {
    data[[lon_col]] <- lon
    data[[lat_col]] <- lat
  }

  list(results = results, data = data, lon = lon, lat = lat)
}

# Calculate decimal precision from text representation
# @param lon_chr Character vector, longitude values as strings
# @param lat_chr Character vector, latitude values as strings
# @return List with decimal precision counts for lon and lat
.calculate_decimal_precision <- function(lon_chr, lat_chr) {
  count_dp_text <- function(x) {
    sapply(x, function(ch) {
      if (is.na(ch)) return(NA_integer_)
      if (!grepl(".", ch, fixed = TRUE)) return(0L)
      parts <- strsplit(ch, ".", fixed = TRUE)[[1]]
      if (length(parts) < 2) return(0L)
      nchar(gsub("0+$", "", parts[2]))
    })
  }

  list(
    lon_dp_text = count_dp_text(lon_chr),
    lat_dp_text = count_dp_text(lat_chr)
  )
}

# Create sf object from coordinates, handling NA values
# @param data Data.frame with coordinate data
# @param lon_col Character, longitude column name
# @param lat_col Character, latitude column name
# @param lon Numeric vector, longitude values
# @param lat Numeric vector, latitude values
# @return sf object with POINT geometry (NA coordinates handled as NA points)
.create_sf_from_coords <- function(data, lon_col, lat_col, lon, lat) {
  if (nrow(data) == 0) {
    return(sf::st_sf(data.frame(), geometry = sf::st_sfc(crs = 4326)))
  }

  na_coords_global <- is.na(lon) | is.na(lat)

  if (any(na_coords_global)) {
    # Use temporary coordinates for NA values to create sf object
    lon_temp <- lon
    lat_temp <- lat
    lon_temp[na_coords_global] <- 0
    lat_temp[na_coords_global] <- 0

    data[[lon_col]] <- lon_temp
    data[[lat_col]] <- lat_temp

    pts <- sf::st_as_sf(data, coords = c(lon_col, lat_col), crs = 4326)

    # Set NA geometries for rows with NA coordinates
    if (any(na_coords_global)) {
      na_geom <- sf::st_sfc(
        lapply(
          which(na_coords_global),
          function(x) sf::st_point(c(NA_real_, NA_real_))
        ),
        crs = sf::st_crs(pts)
      )
      sf::st_geometry(pts)[na_coords_global] <- na_geom
    }
    
    # Store indices of NA coordinates as an attribute
    attr(pts, "na_coord_indices") <- which(na_coords_global)
  } else {
    pts <- sf::st_as_sf(data, coords = c(lon_col, lat_col), crs = 4326)
  }

  pts
}

# Validate and transform CRS
# @param pts sf object with coordinate data
# @param geometry_crs Target CRS for output geometry
# @param quiet Logical, whether to suppress progress messages
# @return List with issues detected, 4326 version of points, and
#   assumed_wgs84 flag
.validate_and_transform_crs <- function(pts, geometry_crs, quiet) {
  if (!quiet) cli::cli_progress_step("Checking CRS...")

  issues <- character()
  crs_info <- sf::st_crs(pts)
  assumed_wgs84 <- FALSE

  if (is.na(crs_info$input)) {
    issues <- c(issues, "Missing CRS")
    bbox <- tryCatch(sf::st_bbox(pts), error = function(e) NULL)

    if (!is.null(bbox) && length(bbox) == 4) {
      if (bbox[1] >= -180 && bbox[3] <= 180 &&
          bbox[2] >= -90 && bbox[4] <= 90) {
        pts <- sf::st_set_crs(pts, 4326)
        assumed_wgs84 <- TRUE
        if (!quiet) {
          cli::cli_alert_warning(
            "CRS missing; assuming WGS84 for validation only"
          )
        }
      }
    }
  } else {
    if (!quiet) cli::cli_alert_success("CRS: {crs_info$input}")
  }

  # Create 4326 copy for lat/lon based checks
  if (!is.na(sf::st_crs(pts)$epsg) && sf::st_crs(pts)$epsg != 4326) {
    pts_4326 <- sf::st_transform(pts, 4326)
    if (!quiet) {
      cli::cli_alert_info("Transformed points to EPSG:4326 for checks")
    }
  } else {
    pts_4326 <- pts
  }

  if (!quiet) cli::cli_progress_done()

  list(issues = issues, pts_4326 = pts_4326, assumed_wgs84 = assumed_wgs84)
}

# Prepare ADM0 polygon for validation
# @param adm0_sf sf polygon object for country boundary (or NULL)
# @param geometry_crs Target CRS for operations
# @param quiet Logical, whether to suppress progress messages
# @return List with adm0 in 4326 and target CRS, plus country bbox (or NULL)
.prepare_adm0_polygon <- function(adm0_sf, geometry_crs, quiet) {
  if (is.null(adm0_sf)) return(NULL)

  if (!inherits(adm0_sf, "sf")) {
    cli::cli_abort("'adm0_sf' must be an sf object")
  }
  if (is.na(sf::st_crs(adm0_sf)$input)) {
    cli::cli_abort("'adm0_sf' must have a valid CRS")
  }

  adm0_sf <- sf::st_make_valid(adm0_sf)

  # Create 4326 version for bbox checks
  adm0_4326 <- if (!is.na(sf::st_crs(adm0_sf)$epsg) &&
                   sf::st_crs(adm0_sf)$epsg != 4326) {
    if (!quiet) {
      cli::cli_alert_info("Transformed adm0_sf to EPSG:4326 for checks")
    }
    sf::st_transform(adm0_sf, 4326)
  } else {
    adm0_sf
  }

  # Create target CRS version for containment operations
  adm0_target <- if (!is.na(sf::st_crs(adm0_sf)$epsg) &&
                     sf::st_crs(adm0_sf)$epsg != geometry_crs) {
    sf::st_transform(adm0_sf, geometry_crs)
  } else {
    adm0_sf
  }

  list(
    adm0_4326 = adm0_4326,
    adm0_target = adm0_target,
    country_bbox = sf::st_bbox(adm0_4326)
  )
}

# Validate coordinate geometry and ranges
# @param pts_4326 sf object in EPSG:4326
# @param adm0_data ADM0 polygon data (or NULL)
# @param results Results list to update with issues
# @param fix_issues Logical, whether to attempt automatic fixes
# @param quiet Logical, whether to suppress progress messages
# @return List with updated results and validated points
.validate_coordinate_geometry <- function(pts_4326, adm0_data, results,
                                          fix_issues, quiet) {
  if (!quiet) {
    cli::cli_progress_step("Validating point geometries and ranges...")
  }

  # Check geometry types
  geom_type <- unique(sf::st_geometry_type(pts_4326, by_geometry = TRUE))
  not_points <- !all(geom_type %in% c("POINT", "MULTIPOINT"))

  if (not_points) {
    results$issues <- c(results$issues, "Geometry must be POINT/MULTIPOINT")
    results$invalid_rows <- .append_invalid_rows(results$invalid_rows, pts_4326)

    if (fix_issues) {
      pts_4326 <- suppressWarnings(sf::st_centroid(sf::st_make_valid(pts_4326)))
      if (!quiet) {
        cli::cli_alert_warning(
          "Converted non-point geometries to centroids"
        )
      }
    }
  }

  # Remove empty geometries (but preserve intentional NA geometries)
  # Note: Empty geometries from NA coordinates are already reported as "missing coordinates"
  # so we don't report them again here
  empty_idx <- which(sf::st_is_empty(pts_4326))

  if (length(empty_idx) > 0 && fix_issues) {
    pts_4326 <- pts_4326[-empty_idx, ]
  }

  # Coordinate range checks for EPSG:4326
  if (.is_wgs84_crs(pts_4326)) {
    range_result <- .check_coordinate_ranges(pts_4326, results, fix_issues)
    results <- range_result$results
    pts_4326 <- range_result$pts
  }

  if (!quiet) cli::cli_progress_done()

  list(results = results, pts = pts_4326)
}

# Check if CRS is WGS84
# @param pts sf object to check CRS
# @return Logical, TRUE if CRS is WGS84 (EPSG:4326)
.is_wgs84_crs <- function(pts) {
  crs_info <- sf::st_crs(pts)
  (!is.na(crs_info$epsg) && crs_info$epsg == 4326) ||
    (!is.na(crs_info$input) && grepl("4326", crs_info$input))
}

# Check coordinate ranges for validity
# @param pts_4326 sf object in EPSG:4326
# @param results Results list to update with issues
# @param fix_issues Logical, whether to set out-of-range coordinates to NA
# @return List with updated results and points
.check_coordinate_ranges <- function(pts_4326, results, fix_issues) {
  coords <- sf::st_coordinates(sf::st_geometry(pts_4326))
  
  # First check which coordinates are NA (missing)
  na_coords <- is.na(coords[, 1]) | is.na(coords[, 2])
  
  # Only check range for non-NA coordinates
  invalid_idx <- which(
    !na_coords & (
      !is.finite(coords[, 1]) | !is.finite(coords[, 2]) |
      coords[, 1] < -180 | coords[, 1] > 180 |
      coords[, 2] < -90  | coords[, 2] > 90
    )
  )

  if (length(invalid_idx) > 0) {
    results$issues <- c(
      results$issues,
      sprintf("%d coordinates out of range", length(invalid_idx))
    )
    results$invalid_rows <- .append_invalid_rows(
      results$invalid_rows, pts_4326[invalid_idx, ]
    )

    if (fix_issues) {
      # Set out-of-range coordinates to NA
      na_geom <- sf::st_sfc(
        lapply(invalid_idx, function(x) sf::st_point(c(NA_real_, NA_real_))),
        crs = sf::st_crs(pts_4326)
      )
      sf::st_geometry(pts_4326)[invalid_idx] <- na_geom
    }
  }

  list(results = results, pts = pts_4326)
}

# Check decimal precision of coordinates
# @param pts_4326 sf object in EPSG:4326
# @param min_decimals Minimum decimal places required for precision
# @param results Results list to update with issues
# @param quiet Logical, whether to suppress progress messages
# @return List with updated results and points (with precision metadata)
.check_decimal_precision <- function(pts_4326, min_decimals, results, quiet) {
  if (nrow(pts_4326) == 0 || !.is_wgs84_crs(pts_4326)) {
    return(list(results = results, pts = pts_4326))
  }

  if (!quiet) cli::cli_progress_step("Checking decimal precision...")

  coords <- sf::st_coordinates(pts_4326$geometry)
  
  # Check for NA coordinates first
  na_coords <- is.na(coords[, 1]) | is.na(coords[, 2])

  # Use text-based precision if available, otherwise calculate from numeric
  if ("lon_dp_text" %in% names(pts_4326) &&
      "lat_dp_text" %in% names(pts_4326)) {
    lon_dp <- suppressWarnings(as.integer(pts_4326$lon_dp_text))
    lat_dp <- suppressWarnings(as.integer(pts_4326$lat_dp_text))

    # Fill missing with numeric calculation
    na_lon <- which(is.na(lon_dp))
    na_lat <- which(is.na(lat_dp))
    if (length(na_lon)) {
      lon_dp[na_lon] <- .count_decimal_places_numeric(coords[na_lon, 1])
    }
    if (length(na_lat)) {
      lat_dp[na_lat] <- .count_decimal_places_numeric(coords[na_lat, 2])
    }
  } else {
    lon_dp <- .count_decimal_places_numeric(coords[, 1])
    lat_dp <- .count_decimal_places_numeric(coords[, 2])
  }

  # Only check precision for non-NA coordinates
  precise_ok <- na_coords | (lon_dp >= min_decimals & lat_dp >= min_decimals)
  n_imprecise <- sum(!na_coords & !precise_ok, na.rm = TRUE)

  if (n_imprecise > 0) {
    results$issues <- c(
      results$issues,
      sprintf("%d imprecise coordinates (< %d dp)", n_imprecise, min_decimals)
    )
  }

  pts_4326$lon_dp <- lon_dp
  pts_4326$lat_dp <- lat_dp
  pts_4326$precision_ok <- precise_ok

  # Check for (0,0) coordinates
  zero_result <- .check_zero_coordinates(pts_4326, coords, results)
  results <- zero_result$results
  pts_4326 <- zero_result$pts

  if (!quiet) cli::cli_progress_done()

  list(results = results, pts = pts_4326)
}

# Count decimal places from numeric values
# @param x Numeric vector to count decimal places for
# @return Integer vector with decimal place counts
.count_decimal_places_numeric <- function(x) {
  sapply(x, function(val) {
    if (is.na(val)) return(0)
    ch <- format(
      abs(val), scientific = FALSE, trim = TRUE, drop0trailing = FALSE
    )
    if (!grepl(".", ch, fixed = TRUE)) return(0)
    parts <- strsplit(ch, ".", fixed = TRUE)[[1]]
    if (length(parts) < 2) return(0)
    nchar(gsub("0+$", "", parts[2]))
  })
}

# Check for (0,0) coordinates which are often placeholders
# @param pts_4326 sf object in EPSG:4326
# @param coords Matrix of coordinates from sf object
# @param results Results list to update with issues
# @return List with updated results and points (with zero_coords flag)
.check_zero_coordinates <- function(pts_4326, coords, results) {
  # Only check for (0,0) if coordinates are not NA
  zero_coords_idx <- which(!is.na(coords[, 1]) & !is.na(coords[, 2]) & 
                          coords[, 1] == 0 & coords[, 2] == 0)

  if (length(zero_coords_idx) > 0) {
    if (!"zero_coords" %in% names(pts_4326)) pts_4326$zero_coords <- FALSE
    pts_4326$zero_coords[zero_coords_idx] <- TRUE

    results$issues <- c(
      results$issues,
      sprintf("%d points at (0,0) coordinates", length(zero_coords_idx))
    )
    results$invalid_rows <- .append_invalid_rows(
      results$invalid_rows, pts_4326[zero_coords_idx, ]
    )
  }

  list(results = results, pts = pts_4326)
}

# Detect and fix flipped coordinates using polygon containment
# @param pts_4326 sf object in EPSG:4326
# @param adm0_data ADM0 polygon data for containment testing (or NULL)
# @param results Results list to update with issues
# @param fix_issues Logical, whether to automatically swap flipped coordinates
# @param quiet Logical, whether to suppress progress messages
# @return List with updated results and points (with flipped coordinates
#   fixed if applicable)
.detect_and_fix_flips <- function(pts_4326, adm0_data, results,
                                   fix_issues, quiet) {
  if (is.null(adm0_data) || nrow(pts_4326) == 0) {
    return(list(results = results, pts = pts_4326))
  }

  if (!quiet) cli::cli_progress_step("Detecting flipped coordinates...")

  coords <- sf::st_coordinates(pts_4326$geometry)
  valid_coords <- !is.na(coords[,1]) & !is.na(coords[,2])

  # Check containment in original positions
  in_poly <- lengths(sf::st_within(pts_4326, adm0_data$adm0_4326)) > 0

  # Create swapped coordinate version
  coords_sw <- coords
  coords_sw[,1] <- coords[,2]
  coords_sw[,2] <- coords[,1]

  swapped_sfc <- sf::st_sfc(
    lapply(
      seq_len(nrow(coords_sw)),
      function(i) sf::st_point(coords_sw[i, 1:2])
    ),
    crs = 4326
  )
  swapped_sf <- sf::st_sf(geometry = swapped_sfc)

  # Check containment for swapped coordinates
  swap_in_poly <- lengths(sf::st_within(swapped_sf, adm0_data$adm0_4326)) > 0

  # Identify potentially flipped coordinates
  maybe_flipped <- (!in_poly) & swap_in_poly & valid_coords
  n_maybe <- sum(maybe_flipped, na.rm = TRUE)

  pts_4326$in_bbox <- in_poly
  pts_4326$maybe_flipped <- maybe_flipped

  if (n_maybe > 0) {
    results$issues <- c(
      results$issues, sprintf("%d points maybe flipped", n_maybe)
    )

    if (fix_issues) {
      coords2 <- coords
      flip_valid <- maybe_flipped & valid_coords

      if (any(flip_valid)) {
        coords2[flip_valid, 1] <- coords[flip_valid, 2]
        coords2[flip_valid, 2] <- coords[flip_valid, 1]
      }

      new_geom <- sf::st_sfc(
        lapply(
          seq_len(nrow(coords2)),
          function(i) sf::st_point(coords2[i, 1:2])
        ),
        crs = 4326
      )
      pts_4326$geometry <- new_geom
    }
  }

  if (!quiet) cli::cli_progress_done()

  list(results = results, pts = pts_4326)
}

# Handle duplicate detection and removal
# @param pts_4326 sf object in EPSG:4326
# @param id_col Character, name of ID column (or NULL)
# @param results Results list to update with issues
# @param fix_issues Logical, whether to remove ID+coordinate duplicates
# @param quiet Logical, whether to suppress progress messages
# @return List with updated results, points, and duplicate_info metadata
.handle_duplicates <- function(pts_4326, id_col, results, fix_issues, quiet) {
  if (!fix_issues || nrow(pts_4326) == 0) {
    return(list(results = results, pts = pts_4326))
  }

  if (!quiet) cli::cli_progress_step("Checking duplicates...")

  duplicate_info <- list(
    shared_locations_only_idx = integer(0),
    all_shared_locations_idx = integer(0),
    id_dup_sf_pre = NULL
  )

  # Handle duplicates by ID + coordinates
  if (!is.null(id_col) && id_col %in% names(pts_4326)) {
    id_dup_result <- .remove_id_coordinate_duplicates(pts_4326, id_col, results)
    results <- id_dup_result$results
    pts_4326 <- id_dup_result$pts
    duplicate_info$id_dup_sf_pre <- id_dup_result$removed_dups
  }

  # Detect coordinate-only duplicates (report but don't remove)
  coord_dup_result <- .detect_coordinate_duplicates(pts_4326, results)
  results <- coord_dup_result$results
  duplicate_info$shared_locations_only_idx <-
    coord_dup_result$shared_locations_only_idx
  duplicate_info$all_shared_locations_idx <-
    coord_dup_result$all_shared_locations_idx

  if (!quiet) cli::cli_progress_done()

  list(results = results, pts = pts_4326, duplicate_info = duplicate_info)
}

# Remove duplicates by ID and coordinates
# @param pts_4326 sf object in EPSG:4326
# @param id_col Character, name of ID column
# @param results Results list to update with issues
# @return List with updated results, deduplicated points, and removed duplicates
.remove_id_coordinate_duplicates <- function(pts_4326, id_col, results) {
  geom_binary <- sf::st_as_binary(pts_4326$geometry)
  id_geom_key <- paste(pts_4326[[id_col]], geom_binary, sep = "_")
  id_coord_dups_idx <- which(duplicated(id_geom_key))

  removed_dups <- NULL
  if (length(id_coord_dups_idx) > 0) {
    removed_dups <- pts_4326[id_coord_dups_idx, ]
    pts_4326 <- pts_4326[!duplicated(id_geom_key), ]

    results$issues <- c(
      results$issues,
      sprintf(
        "%d duplicates by ID and coordinates removed",
        length(id_coord_dups_idx)
      )
    )
  }

  list(results = results, pts = pts_4326, removed_dups = removed_dups)
}

# Detect coordinate-only duplicates (shared locations)
# @param pts_4326 sf object in EPSG:4326
# @param results Results list to update with issues
# @return List with updated results and indices of shared/duplicate locations
.detect_coordinate_duplicates <- function(pts_4326, results) {
  geom_binary <- sf::st_as_binary(pts_4326$geometry)
  geom_dups_idx <- which(duplicated(geom_binary))

  shared_locations_only_idx <- integer(0)
  all_shared_locations_idx <- integer(0)

  if (length(geom_dups_idx) > 0) {
    shared_locations_only_idx <- geom_dups_idx

    # Find all records that share coordinates
    dup_geoms <- geom_binary[geom_dups_idx]
    all_shared_locations_idx <- which(geom_binary %in% dup_geoms)

    results$issues <- c(
      results$issues,
      sprintf(
        "%d records at shared locations (possible centroids)",
        length(geom_dups_idx)
      )
    )
  }

  list(
    results = results,
    shared_locations_only_idx = shared_locations_only_idx,
    all_shared_locations_idx = all_shared_locations_idx
  )
}

# Check ADM0 containment
# @param pts_4326 sf object in EPSG:4326
# @param adm0_data ADM0 polygon data for containment testing (or NULL)
# @param geometry_crs Target CRS for output geometry
# @param results Results list to update with issues
# @param quiet Logical, whether to suppress progress messages
# @return List with updated results and points (in target CRS with
#   outside_adm0 flag)
.check_adm0_containment <- function(pts_4326, adm0_data, geometry_crs,
                                    results, quiet) {
  if (is.null(adm0_data) || nrow(pts_4326) == 0) {
    # Transform to target CRS even without ADM0
    pts <- if (!is.na(sf::st_crs(pts_4326)$epsg) &&
              sf::st_crs(pts_4326)$epsg != geometry_crs) {
      sf::st_transform(pts_4326, geometry_crs)
    } else {
      pts_4326
    }
    return(list(results = results, pts = pts))
  }

  if (!quiet) cli::cli_progress_step("Checking all points fall within ADM0...")

  # Transform points to target geometry CRS
  pts_geom <- if (!is.na(sf::st_crs(pts_4326)$epsg) &&
                 sf::st_crs(pts_4326)$epsg != geometry_crs) {
    sf::st_transform(pts_4326, geometry_crs)
  } else {
    pts_4326
  }

  # Compute containment, excluding NA geometries
  non_na_idx <- !sf::st_is_empty(pts_geom)
  rel <- sf::st_within(pts_geom, adm0_data$adm0_target)
  inside <- lengths(rel) > 0
  # Only count non-NA geometries that are outside
  n_out <- sum(!inside & non_na_idx, na.rm = TRUE)

  if (n_out > 0) {
    results$issues <- c(
      results$issues, sprintf("%d points lie outside ADM0", n_out)
    )
  } else {
    if (!quiet) cli::cli_alert_success("All points lie within ADM0")
  }

  pts_geom$outside_adm0 <- !inside

  if (!quiet) cli::cli_progress_done()

  list(results = results, pts = pts_geom)
}

# Standardize output columns and create final results
# @param pts sf object in target CRS with all validation flags
# @param original_input_cols Character vector, original column names from input
# @param results Results list to update with final outputs
# @param duplicate_result List with duplicate detection metadata
# @return Updated results list with final_points_df, column_dictionary,
#   and checks
.standardize_output <- function(pts, original_input_cols, results,
                                duplicate_result) {
  # Define processing columns to exclude
  processing_cols <- c(
    "lon_dp", "lat_dp", "precision_ok", "in_bbox",
    "maybe_flipped", "outside_country", "outside_adm0", "zero_coords",
    "lon_dp_text", "lat_dp_text"
  )

  # Get columns to keep from original input
  non_geom_original <- setdiff(original_input_cols, "geometry")
  cols_to_keep <- intersect(names(pts), c(non_geom_original, "geometry"))
  cols_to_keep <- setdiff(cols_to_keep, processing_cols)

  # Create standardized output with lon/lat
  pts4326_tmp <- sf::st_transform(pts, 4326)
  coords_lonlat <- sf::st_coordinates(pts4326_tmp)

  # Set coordinates to NA for flagged points
  coords_lonlat <- .handle_flagged_coordinates(pts, coords_lonlat)

  # Build final output
  pts_out <- pts |>
    dplyr::select(dplyr::any_of(cols_to_keep)) |>
    dplyr::mutate(
      "lon" = coords_lonlat[, 1],
      "lat" = coords_lonlat[, 2]
    )

  # Reorder columns
  cols_final <- setdiff(
    cols_to_keep, c("lon", "lat", "geometry")
  )
  pts_out <- pts_out |>
    dplyr::select(
      dplyr::any_of(cols_final),
      "lon", "lat", geometry
    )

  results$final_points_df <- pts_out
  results$column_dictionary <- .create_column_dictionary(pts_out)
  results$checks <- .create_checks_output(pts_out, pts, duplicate_result, results)

  results
}

# Handle coordinates flagged for NA treatment
# @param pts sf object with validation flags (outside_adm0, zero_coords)
# @param coords_lonlat Matrix of lon/lat coordinates in EPSG:4326
# @return Matrix with flagged coordinates set to NA
.handle_flagged_coordinates <- function(pts, coords_lonlat) {
  if ("outside_adm0" %in% names(pts)) {
    outside_idx <- which(
      isTRUE(pts$outside_adm0) |
        (!is.na(pts$outside_adm0) & pts$outside_adm0)
    )
    if (length(outside_idx) > 0) {
      coords_lonlat[outside_idx, 1] <- NA_real_
      coords_lonlat[outside_idx, 2] <- NA_real_
    }
  }

  if ("zero_coords" %in% names(pts)) {
    zero_idx <- which(!is.na(pts$zero_coords) & pts$zero_coords)
    if (length(zero_idx) > 0) {
      coords_lonlat[zero_idx, 1] <- NA_real_
      coords_lonlat[zero_idx, 2] <- NA_real_
    }
  }

  coords_lonlat
}

# Create column dictionary for output
# @param pts_out Final sf output object with standardized columns
# @return Data.frame mapping column names to descriptions
.create_column_dictionary <- function(pts_out) {
  dict_rows <- list()

  for (col_name in names(pts_out)) {
    description <- switch(col_name,
      "lon" = "Longitude (EPSG:4326)",
      "lat" = "Latitude (EPSG:4326)",
      "geometry" = "Spatial geometry (sf POINT)",
      "Data column"
    )

    dict_rows[[length(dict_rows) + 1]] <- data.frame(
      old_name = col_name,
      new_name = col_name,
      description = description,
      stringsAsFactors = FALSE
    )
  }

  do.call(rbind, dict_rows)
}

# Create standardized checks output
# @param pts_out Final sf output object with standardized columns
# @param pts sf object with validation flags
# @param duplicate_result List with duplicate detection metadata
# @param results Results list containing missing_coord_rows
# @return List of validation check results (or NULL if empty)
.create_checks_output <- function(pts_out, pts, duplicate_result, results) {
  duplicate_info <- duplicate_result$duplicate_info
  checks <- list()

  # Shared locations (duplicate coordinates only)
  if (exists("shared_locations_only_idx", duplicate_info) &&
      length(duplicate_info$shared_locations_only_idx) > 0) {
    checks$all_shared_locations <-
      pts_out[duplicate_info$shared_locations_only_idx, ]
  }

  # All records at shared locations
  if (exists("all_shared_locations_idx", duplicate_info) &&
      length(duplicate_info$all_shared_locations_idx) > 0) {
    checks$all_shared_locations_ungrouped <- dplyr::arrange(
      pts_out[duplicate_info$all_shared_locations_idx, ], "lon", "lat"
    )
  }

  # ID + coordinate duplicates
  if (!is.null(duplicate_info$id_dup_sf_pre)) {
    checks$duplicate_rows <- .standardize_duplicate_output(
      duplicate_info$id_dup_sf_pre, pts_out
    )
  }

  # Other validation checks
  checks <- c(checks, .create_validation_checks(pts_out, pts, results))

  # Remove empty checks
  checks <- Filter(function(x) (inherits(x, "sf") || inherits(x, "data.frame")) && nrow(x) > 0, checks)
  if (length(checks) == 0) return(NULL)

  checks
}

# Create validation-specific checks
# @param pts_out Final sf output object with standardized columns
# @param pts sf object with validation flags
# @param results Results list containing missing_coord_rows
# @return List with validation check results (flipped, imprecise, outside ADM0)
.create_validation_checks <- function(pts_out, pts, results) {
  checks <- list()

  # Missing coordinates - convert to sf with empty/NA geometry
  if (!is.null(results$missing_coord_rows) && nrow(results$missing_coord_rows) > 0) {
    missing_sf <- results$missing_coord_rows
    
    # Create NA point geometries
    na_points <- sf::st_sfc(
      lapply(1:nrow(missing_sf), function(x) sf::st_point(c(NA_real_, NA_real_))),
      crs = 4326
    )
    
    # Convert to sf object with NA geometry
    missing_sf <- sf::st_sf(
      missing_sf,
      geometry = na_points
    )
    
    # Add lon/lat columns as NA
    missing_sf$lon <- NA_real_
    missing_sf$lat <- NA_real_
    
    checks$missing_coordinates <- missing_sf
  }

  # Maybe flipped coordinates
  if ("maybe_flipped" %in% names(pts)) {
    idx_flip <- which(!is.na(pts$maybe_flipped) & pts$maybe_flipped)
    if (length(idx_flip) > 0) {
      checks$maybe_flipped <- pts_out[idx_flip, ]
    }
  }

  # Imprecise coordinates
  if ("precision_ok" %in% names(pts)) {
    idx_imp <- which(!is.na(pts$precision_ok) & !pts$precision_ok)
    if (length(idx_imp) > 0) {
      checks$imprecise <- pts_out[idx_imp, ]
    }
  }

  # Points outside ADM0
  if ("outside_adm0" %in% names(pts)) {
    outside_idx <- which(!is.na(pts$outside_adm0) & pts$outside_adm0)
    if (length(outside_idx) > 0) {
      checks$coordinates_adm0 <- pts_out[outside_idx, ]
      # Restore original coordinates for reporting
      coords_old <- sf::st_coordinates(
        sf::st_transform(checks$coordinates_adm0, 4326)
      )
      checks$coordinates_adm0[["lon"]] <- coords_old[, 1]
      checks$coordinates_adm0[["lat"]] <- coords_old[, 2]
    }
  }

  checks
}

# Standardize duplicate output format
# @param dup_sf_pre sf object with duplicate records to standardize
# @param pts_out Final sf output object (for matching column structure)
# @return Standardized sf object with same column structure as pts_out
.standardize_duplicate_output <- function(dup_sf_pre, pts_out) {
  # Transform and standardize duplicate records to match pts_out format
  dup_tmp <- sf::st_transform(dup_sf_pre, sf::st_crs(pts_out))
  dup4326_tmp <- sf::st_transform(dup_tmp, 4326)
  dup_coords_lonlat <- sf::st_coordinates(dup4326_tmp)

  # Get matching columns
  cols_to_keep <- intersect(names(dup_tmp), names(pts_out))
  cols_final <- setdiff(
    cols_to_keep, c("lon", "lat", "geometry")
  )

  dup_tmp |>
    dplyr::select(dplyr::any_of(cols_to_keep)) |>
    dplyr::mutate(
      "lon" = dup_coords_lonlat[, 1],
      "lat" = dup_coords_lonlat[, 2]
    ) |>
    dplyr::select(
      dplyr::any_of(cols_final),
      "lon", "lat", geometry
    ) |>
    dplyr::arrange("lon", "lat")
}

# Finalize results and generate summary
# @param results Complete results list from validation process
# @param fix_issues Logical, whether fixes were attempted
# @param quiet Logical, whether to suppress summary output
# @return Cleaned results list with summary printed (if not quiet)
.finalize_results <- function(results, fix_issues, quiet) {
  # Clean up intermediate results
  results$invalid_rows <- NULL
  results$missing_coord_idx <- NULL
  results$missing_coord_rows <- NULL
  results$duplicate_rows <- NULL
  results$coordinates_adm0 <- NULL

  if (!quiet) {
    .print_validation_summary(results, fix_issues)
  }

  results
}

# Print validation summary
# @param results Complete results list from validation process
# @param fix_issues Logical, whether fixes were attempted
# @return NULL (side effect: prints summary to console)
.print_validation_summary <- function(results, fix_issues) {
  if (length(results$issues) == 0) {
    if (fix_issues) {
      cli::cli_h2("Summary of Coordinate Validation and Cleaning")
      cli::cli_alert_info(
        "Points retained: {nrow(rlang::`%||%`(results$final_points_df, data.frame()))}"
      )
    } else {
      cli::cli_h2("Summary of Coordinate Validation")
      cli::cli_alert_info("Validation completed - no cleaning performed")
    }
  } else {
    cli::cli_h2("Issues found:")
    for (issue in unique(results$issues)) {
      cli::cli_alert_warning(issue)
    }
    cli::cli_text("")

    if (fix_issues) {
      .print_fixed_actions(results$issues)
    }
  }
}

# Print actions taken during fixes
# @param issues Character vector of detected issues
# @return NULL (side effect: prints fixed actions to console)
.print_fixed_actions <- function(issues) {
  fixed_actions <- character()

  if (any(grepl("rows with missing coordinates", issues))) {
    fixed_actions <- c(fixed_actions, "Set missing coordinates to NA")
  }
  if (any(grepl("non-numeric coordinates", issues))) {
    fixed_actions <- c(fixed_actions, "Set non-numeric coordinates to NA")
  }
  if (any(grepl("coordinates out of range", issues))) {
    fixed_actions <- c(fixed_actions, "Set out-of-range coordinates to NA")
  }
  # Empty geometries are removed when fixing missing coordinates, so no separate action needed
  if (any(grepl("Missing CRS", issues))) {
    fixed_actions <- c(fixed_actions, "Set CRS to WGS84 (EPSG:4326)")
  }
  if (any(grepl("imprecise coordinates", issues))) {
    fixed_actions <- c(
      fixed_actions, "Detected imprecise coordinates (flagged for review)"
    )
  }
  if (any(grepl("points at \\(0,0\\) coordinates", issues))) {
    fixed_actions <- c(fixed_actions, "Flagged points at (0,0) coordinates")
  }
  if (any(grepl("points maybe flipped", issues))) {
    fixed_actions <- c(fixed_actions, "Swapped lon/lat for flipped coordinates")
  }
  if (any(grepl("duplicates by ID and coordinates", issues))) {
    fixed_actions <- c(
      fixed_actions, "Removed duplicate records by ID and coordinates"
    )
  }
  if (any(grepl("records at shared locations", issues))) {
    fixed_actions <- c(
      fixed_actions, "Detected records at shared locations (not removed)"
    )
  }
  if (any(grepl("points lie outside ADM0", issues))) {
    fixed_actions <- c(fixed_actions, "Flagged points outside ADM0 boundary")
  }

  if (length(fixed_actions) > 0) {
    cli::cli_h2("Fixed issues:")
    for (action in fixed_actions) {
      cli::cli_alert_success(action)
    }
    cli::cli_text("")
  }
}

# Helper function to append invalid rows
# @param existing Existing invalid rows data.frame (or NULL)
# @param new_rows New invalid rows (sf object or data.frame) to append
# @return Combined data.frame with geometry dropped from sf objects
.append_invalid_rows <- function(existing, new_rows) {
  nr <- if (inherits(new_rows, "sf")) {
    sf::st_drop_geometry(new_rows)
  } else {
    as.data.frame(new_rows)
  }
  if (is.null(existing)) return(nr)
  dplyr::bind_rows(existing, nr)
}
