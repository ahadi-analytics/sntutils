#' Clean Filenames
#'
#' This function cleans a vector of filenames by removing common standalone
#' numbers, condensing multiple separators, and ensuring clean formats.
#'
#' @param filenames A character vector of full file paths or basenames.
#'
#' @return A character vector of cleaned filenames. If full paths are
#' provided, the function returns the cleaned filenames with the full path
#' intact.
clean_filenames <- function(filenames) {

  if (all(dirname(filenames) != ".")) {
    full_path <- filenames
    filenames <- basename(filenames)
    is_full_path <- TRUE
  } else {is_full_path <- FALSE}

  all_numbers <- filenames |>
    stringr::str_extract_all("(?<![A-Za-z])\\d+(?![A-Za-z])") |>
    unlist()

  common_numbers <- all_numbers |>
    table() |>
    as.data.frame() |>
    dplyr::rename(Number = all_numbers) |>
    dplyr::mutate(Number = as.character(Number)) |>
    dplyr::arrange(dplyr::desc(Freq)) |>
    dplyr::filter(Freq > length(filenames) / 2) |>
    dplyr::pull(Number)

  # Return original filenames if no common numbers are found
  if (is.null(common_numbers) || length(common_numbers) == 0) {
    return(if (is_full_path) full_path else filenames)
  }

  pattern_common_numbers <- if (length(common_numbers) > 0) {
    paste0("(?<![A-Za-z\\d])(", paste(common_numbers, collapse = "|"),
           ")(?![A-Za-z\\d])")
  } else {
    NULL
  }

  cleaned_filenames <- filenames |>
    stringr::str_replace_all(pattern = pattern_common_numbers,
                             replacement = "") |>
    stringr::str_replace_all(pattern = "\\.{2,}", replacement = ".") |>
    stringr::str_replace_all(pattern = "_{2,}", replacement = "_") |>
    stringr::str_replace_all(pattern = "_+\\.|\\._", replacement = "") |>
    stringr::str_replace_all(pattern = "-+", replacement = "-") |>
    stringr::str_replace_all(pattern = "^[_\\.]+|[_\\.]+$", replacement = "")

  if (is_full_path) {
    cleaned_filenames <- file.path(dirname(full_path), cleaned_filenames)
  }

  return(cleaned_filenames)
}



#' Detect time pattern in filenames
#'
#' Supports daily (YYYY-MM-DD), monthly (YYYY-MM) and yearly (YYYY) formats,
#' using all formats declared in `available_date_formats`.
#'
#' @param filenames Character vector of file paths or names.
#' @return A list with two elements:
#'   \item{pattern}{One of `"daily"`, `"monthly"`, or `"yearly"`.}
#'   \item{parser}{Function(x) that parses character vector `x`
#'    into Date/POSIXct, via `lubridate::parse_date_time()`.}
#' @export
detect_time_pattern <- function(filenames) {
  formats <- sntutils::available_date_formats
  b <- basename(filenames)

  # Enhanced pattern matching for different date formats
  patterns <- list(
    daily = "\\d{4}[._-]\\d{2}[._-]\\d{2}",
    monthly_iso = "\\d{4}[._-]\\d{2}", # YYYY-MM
    monthly_euro = "\\d{2}[._-]\\d{4}", # MM-YYYY
    yearly = "\\d{4}"
  )

  # Check patterns in order of specificity
  if (any(grepl(patterns$daily, b))) {
    pattern <- "daily"
  } else if (any(grepl(patterns$monthly_iso, b)) ||
             any(grepl(patterns$monthly_euro, b))) {
    pattern <- "monthly"
  } else if (any(grepl(patterns$yearly, b))) {
    pattern <- "yearly"
  } else {
    cli::cli_abort("No date pattern found in filenames")
  }

  parser <- function(x) {
    # Add specific orders for European format
    formats <- c(
      "Ymd", "dmY", # daily formats
      "Ym", "mY", # monthly formats (both ISO and European)
      "Y" # yearly format
    )
    lubridate::parse_date_time(x, orders = formats, quiet = TRUE)
  }

  list(
    pattern = pattern,
    parser = parser
  )
}

#' Extract time components from a filename
#'
#' Uses the pattern and parser returned by \code{detect_time_pattern()}
#' to parse the date embedded in \code{filename} and computes derived
#' units (month, semester, quarter).
#'
#' @param filename Character string of file path or name
#' @param info List as returned by \code{detect_time_pattern()}, containing:
#'   \describe{
#'     \item{pattern}{One of \code{"monthly"} or \code{"yearly"}.}
#'     \item{parser}{Function to parse character dates into Date/POSIXct.}
#'   }
#' @return A list with:
#'   \describe{
#'     \item{year}{Integer year}
#'     \item{month}{Integer month (1–12) or \code{NA} for yearly data}
#'     \item{semester}{Integer semester (1–2) or \code{NA}}
#'     \item{quarter}{Integer quarter (1–4) or \code{NA}}
#'     \item{date}{Date object for the first day of the period}
#'   }
#' @examples
#' info <- detect_time_pattern("rainfall_2022-01.tif")
#' extract_time_components("rainfall_2022-01.tif", info)
#' info <- detect_time_pattern("temp_2020.tif")
#' extract_time_components("temp_2020.tif", info)
#' @export
extract_time_components <- function(filename, info) {
  # Enhanced pattern matching for both ISO and European formats
  if (info$pattern == "monthly") {
    # Try European format first (since that's our input)
    euro_match <- stringr::str_extract(
      basename(filename),
      "(\\d{2})[._-](\\d{4})"
    )
    if (!is.na(euro_match)) {
      # Reformat European date to ISO format for parsing
      month <- stringr::str_extract(euro_match, "^\\d{2}")
      year <- stringr::str_extract(euro_match, "\\d{4}$")
      ds <- paste(year, month, sep = "-")
    } else {
      # Try ISO format
      iso_match <- stringr::str_extract(
        basename(filename),
        "(\\d{4})[._-](\\d{2})"
      )
      ds <- iso_match
    }
  } else {
    ds <- stringr::str_extract(
      basename(filename),
      "\\d{4}"
    )
  }

  d <- info$parser(ds)

  if (is.na(d)) {
    return(list(
      year     = NA_integer_,
      month    = NA_integer_,
      semester = NA_integer_,
      quarter  = NA_integer_,
      date     = as.Date(NA)
    ))
  }

  y <- lubridate::year(d)
  if (info$pattern == "monthly") {
    m <- lubridate::month(d)
    return(list(
      year     = y,
      month    = m,
      semester = ceiling(m / 6),
      quarter  = ceiling(m / 3),
      date     = as.Date(d)
    ))
  }

  list(
    year     = y,
    month    = NA_integer_,
    semester = NA_integer_,
    quarter  = NA_integer_,
    date     = as.Date(paste0(y, "-01-01"))
  )
}


#' Process raster data with administrative boundaries
#'
#' This function processes a single raster file by extracting zonal statistics
#' within administrative boundaries. It handles CHIRPS rainfall data format by
#' default, extracting dates from filenames, but works with any raster data.
#'
#' @param raster_file Path to raster file (.tif format)
#' @param shapefile sf/SpatVector object with administrative boundaries. Must be
#'   in the same coordinate reference system (CRS) as the raster data
#' @param id_cols Vector of column names from the shapefile to include in
#'   output. Default is c("adm0", "adm1") for country and first admin level
#' @param aggregations Vector of aggregation methods to calculate. Supported
#'   values are "mean", "sum", and "median". Default is c("mean")
#' @param raster_is_density Logical indicating if raster values represent density.
#'   If TRUE, values are converted from density to counts using cell area.
#'   Default is FALSE.
#'
#' @return A data frame containing:
#'   \itemize{
#'     \item file_name: Name of processed raster file
#'     \item Selected ID columns from shapefile (specified by id_cols)
#'     \item Time columns based on detected pattern:
#'       - For monthly data: year, month
#'       - For yearly data: year
#'     \item Aggregated values with column names matching aggregation methods
#'   }
#'
#' @details
#' The function performs the following steps:
#' 1. Validates requested aggregation methods
#' 2. Detects and extracts date information from filename
#' 3. Loads raster data and handles no-data values (-9999)
#' 4. If raster_is_density is TRUE, converts density to counts using cell area
#' 5. Calculates zonal statistics using exactextractr
#' 6. Combines results with shapefile attributes
#' 7. Returns a clean data frame without geometry
#'
#' @examples
#' \dontrun{
#' # Process single CHIRPS file with district-level boundaries
#' districts <- sf::st_read("districts.shp")
#' results <- process_raster_with_boundaries(
#'   raster_file = "chirps-v2.0.2022.01.tif",
#'   shapefile = districts,
#'   id_cols = c("COUNTRY", "DISTRICT"),
#'   aggregations = c("mean", "sum", "median")
#' )
#' }
#' @export
process_raster_with_boundaries <- function(raster_file,
                                           shapefile,
                                           id_cols = c("adm0", "adm1"),
                                           aggregations = c("mean"),
                                           raster_is_density = FALSE) {

  valid_aggs <- c("mean", "sum", "median")
  if (!all(aggregations %in% valid_aggs)) {
    cli::cli_abort("Invalid aggregation method. Use: {valid_aggs}")
  }

  pattern_info <- detect_time_pattern(raster_file)
  components <- extract_time_components(raster_file, pattern_info)

  rast <- terra::rast(raster_file)[[1]]
  rast[rast == -9999] <- NA

  if (raster_is_density) {
    # Convert density to counts using cell area in km²
    cell_area_km2 <- terra::cellSize(rast, unit = "km")
    rast <- rast * cell_area_km2
  }

  rast_crs <- terra::crs(rast)
  shp_crs <- sf::st_crs(shapefile)

  if (!identical(rast_crs, shp_crs$wkt)) {
    shapefile <- sf::st_transform(shapefile, rast_crs)
  }

  zonal_stats <- exactextractr::exact_extract(
    rast, shapefile,
    fun = aggregations,
    progress = FALSE
  )

  if (length(aggregations) == 1) {
    extracted <- unlist(zonal_stats)
    if (length(extracted) != nrow(shapefile)) {
      cli::cli_abort("Mismatch in extracted values and shapefile rows.")
    }
    result_df <- shapefile |>
      dplyr::mutate(!!rlang::sym(aggregations) := extracted)
  } else {
    stats_df <- as.data.frame(
      zonal_stats)[, !grepl("coverage_fraction", names(zonal_stats))]
    result_df <- dplyr::bind_cols(shapefile, stats_df)
  }

  result_df <- result_df |>
    dplyr::mutate(
      file_name = basename(raster_file),
      file_date = components$date,
      year = components$year
    ) |>
    dplyr::mutate(
      dplyr::across(
        .cols = where(is.numeric) & dplyr::matches("sum"),
        .fns = ~ ifelse(.x > 1, round(.x), .x)
      )
    )

  if (pattern_info$pattern == "monthly") {
    result_df <- result_df |> dplyr::mutate(month = components$month)
  }

  time_cols <- if (
    pattern_info$pattern == "monthly") c("year", "month") else "year"

  result_df |>
    dplyr::select(
      file_name,
      dplyr::all_of(id_cols),
      dplyr::all_of(time_cols),
      dplyr::any_of(aggregations)
    ) |>
    sf::st_drop_geometry()
}

#' Process multiple raster files from a directory
#'
#' This function processes multiple raster files in a directory by applying
#' process_raster_with_boundaries() to each file and combining the results.
#' Includes progress tracking and error handling.
#'
#' @param directory Directory containing raster files to process
#' @param shapefile sf/SpatVector object with administrative boundaries. Must be
#'   in the same coordinate reference system (CRS) as the raster data
#' @param id_cols Vector of column names from shapefile to include in output.
#'   Default is c("adm0", "adm1", "adm2") for country and first admin level
#' @param pattern Regular expression pattern to match raster files. Default is
#'   "\\.tif$" to match all .tif files
#' @param aggregations Vector of aggregation methods to calculate. Supported
#'   values are "mean", "sum", and "median". Default is c("mean")
#' @param raster_is_density Logical indicating if raster values represent density.
#'   If TRUE, values are converted from density to counts using cell area.
#'   Default is FALSE.
#'
#' @return A combined data frame containing results from all processed files,
#'   sorted by time unit if available. Has the same structure as output from
#'   process_raster_with_boundaries() but with multiple files combined.
#'
#' @details
#' The function:
#' 1. Lists all files in directory matching the pattern
#' 2. Creates a progress bar for tracking
#' 3. Processes each file using process_raster_with_boundaries()
#' 4. Combines results into a single data frame
#' 5. Sorts by time unit if available
#'
#' @examples
#' \dontrun{
#' # Process all CHIRPS files in a directory
#' districts <- sf::st_read("districts.shp")
#' results <- process_raster_collection(
#'   directory = "chirps_data",
#'   shapefile = districts,
#'   id_cols = c("COUNTRY", "DISTRICT"),
#'   pattern = "chirps.*\\.tif$",
#'   aggregations = c("mean", "sum")
#' )
#' }
#' @export
process_raster_collection <- function(directory,
                                      shapefile,
                                      id_cols = c("adm0", "adm1", "adm2"),
                                      pattern = "\\.tif$",
                                      aggregations = c("mean"),
                                      raster_is_density = FALSE) {

  # Get list of raster files in directory
  raster_files <- list.files(directory, pattern = pattern, full.names = TRUE)

  # Apply the cleaning function to filenames if there are multiple files
  # then change the names accordingly
  if (length(raster_files) > 1) {
    cleaned_filenames <- clean_filenames(raster_files)

    # Iterate over filenames and rename if necessary
    for (i in seq_along(raster_files)) {
      if (raster_files[i] != cleaned_filenames[i]) {
        file.rename(raster_files[i], cleaned_filenames[i])
      }
    }

    # Update raster_files with the cleaned filenames
    raster_files <- cleaned_filenames
  }

  if (length(raster_files) == 0) {
    cli::cli_abort("No files matching pattern found in: {directory}")
  }

  # Create progress bar
  pb <- progress::progress_bar$new(
    format = "Processing raster files [:bar] :percent | ETA: :eta",
    total = length(raster_files)
  )

  # Process each file and combine results
  results <- lapply(raster_files, function(x) {
    result <- process_raster_with_boundaries(
      x,
      shapefile = shapefile,
      id_cols = id_cols,
      aggregations = aggregations,
      raster_is_density = raster_is_density
    )
    pb$tick()
    result
  }) |> dplyr::bind_rows()

  # Sort by appropriate time unit
  sort_cols <- c("year", "month")

  if (all(sort_cols %in% names(results))) {
    results |>
      dplyr::arrange(dplyr::across(dplyr::all_of(sort_cols)))
  } else {
    results
  }
}
