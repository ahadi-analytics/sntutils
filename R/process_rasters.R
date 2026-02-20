#' Clean Filenames
#'
#' Cleans a vector of filenames by removing common standalone numbers
#' that appear in more than half of the filenames, condensing multiple
#' separators, and removing leading/trailing separators. This is a
#' pure string operation — no files are renamed on disk.
#'
#' @param filenames A character vector of file paths or basenames.
#' @return A character vector of cleaned filenames (same length as
#'   input). Directory paths are preserved if provided.
#'
#' @examples
#' clean_filenames(c("file_001.txt", "file_002.txt"))
#' clean_filenames(c("/path/to/file_001.txt", "/path/to/file_002.txt"))
#' @export
clean_filenames <- function(filenames) {
  dirs <- dirname(filenames)
  fnames <- basename(filenames)

  # separate extension before cleaning to protect it
  extensions <- tools::file_ext(fnames)
  basenames <- tools::file_path_sans_ext(fnames)

  all_numbers <- basenames |>
    stringr::str_extract_all(
      "(?<![A-Za-z])\\d+(?![A-Za-z])"
    ) |>
    unlist()

  num_freq <- table(all_numbers)
  common_numbers <- names(
    num_freq[num_freq > length(filenames) / 2]
  )

  # never strip 4-digit years (1980-2099)
  is_year <- grepl("^(19[89]\\d|20\\d{2})$", common_numbers)
  common_numbers <- common_numbers[!is_year]

  # return originals if no common numbers to strip
  if (length(common_numbers) == 0) {
    return(filenames)
  }

  pattern_common_numbers <- paste0(
    "(?<![A-Za-z\\d])(",
    paste(common_numbers, collapse = "|"),
    ")(?![A-Za-z\\d])"
  )

  cleaned_basenames <- basenames |>
    stringr::str_replace_all(
      pattern = pattern_common_numbers,
      replacement = ""
    ) |>
    stringr::str_replace_all(pattern = "_{2,}", replacement = "_") |>
    stringr::str_replace_all(
      pattern = "_+\\.|\\._+", replacement = "."
    ) |>
    stringr::str_replace_all(
      pattern = "\\.{2,}", replacement = "."
    ) |>
    stringr::str_replace_all(pattern = "-+", replacement = "-") |>
    stringr::str_replace_all(
      pattern = "^[_\\.]+|[_\\.]+$", replacement = ""
    )

  # reattach extension and directory
  cleaned <- paste0(cleaned_basenames, ".", extensions)
  has_dir <- dirs != "."
  cleaned[has_dir] <- file.path(dirs[has_dir], cleaned[has_dir])

  cleaned
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
    monthly_compact = "(?<!\\d)\\d{6}(?!\\d)", # YYYYMM
    monthly_euro = "\\d{2}[._-]\\d{4}", # MM-YYYY
    yearly = "\\d{4}"
  )

  # Check patterns in order of specificity
  if (any(grepl(patterns$daily, b))) {
    pattern <- "daily"
  } else if (any(grepl(patterns$monthly_iso, b, perl = TRUE)) ||
             any(grepl(patterns$monthly_compact, b, perl = TRUE)) ||
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
      # Try ISO format (YYYY-MM)
      iso_match <- stringr::str_extract(
        basename(filename),
        "(\\d{4})[._-](\\d{2})"
      )
      if (!is.na(iso_match)) {
        ds <- iso_match
      } else {
        # Try compact format (YYYYMM)
        compact <- stringr::str_extract(
          basename(filename), "\\d{6}"
        )
        ds <- paste0(
          substr(compact, 1, 4), "-", substr(compact, 5, 6)
        )
      }
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
#' @param layer_to_process Integer or character. Specifies which layer in a
#'   multi-layer raster to extract. If the raster contains multiple layers
#'   (e.g., different years or indicators), this argument selects the layer to
#'   be processed. Default is 1
#' @param pattern_info Optional. Pre-parsed time pattern from
#'   \code{detect_time_pattern()}. If NULL, detected from filename.
#' @param time_components Optional. Pre-parsed time components from
#'   \code{extract_time_components()}. If NULL, extracted from filename.
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
                                           raster_is_density = FALSE,
                                           layer_to_process = 1,
                                           pattern_info = NULL,
                                           time_components = NULL) {

  valid_aggs <- c("mean", "sum", "median")
  if (!all(aggregations %in% valid_aggs)) {
    cli::cli_abort("Invalid aggregation method. Use: {valid_aggs}")
  }

  # use pre-parsed time info if provided, otherwise detect
  if (is.null(pattern_info)) {
    pattern_info <- detect_time_pattern(raster_file)
  }
  if (is.null(time_components)) {
    time_components <- extract_time_components(
      raster_file, pattern_info
    )
  }
  components <- time_components

  rast <- terra::rast(raster_file)[[layer_to_process]]
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

  time_cols <- if (pattern_info$pattern == "monthly") {
    c("year", "month")
  } else {
    "year"
  }

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
#' @param layer_to_process Integer or character. Specifies which layer in a
#'   multi-layer raster to extract. If the raster contains multiple layers
#'   (e.g., different years or indicators), this argument selects the layer to
#'   be processed. Default is 1.
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
                                      layer_to_process = 1,
                                      raster_is_density = FALSE) {

  # get raster files (original paths stay untouched)
  raster_files <- list.files(
    directory, pattern = pattern, full.names = TRUE
  )

  if (length(raster_files) == 0) {
    cli::cli_abort("No files matching pattern found in: {directory}")
  }

  # clean names for date detection only; never rename on disk
  cleaned_names <- clean_filenames(raster_files)
  pattern_info <- detect_time_pattern(cleaned_names)

  pb <- progress::progress_bar$new(
    format = "Processing raster files [:bar] :percent | ETA: :eta",
    total = length(raster_files)
  )

  results <- lapply(seq_along(raster_files), function(i) {
    components <- extract_time_components(
      cleaned_names[i], pattern_info
    )
    result <- process_raster_with_boundaries(
      raster_file = raster_files[i],
      shapefile = shapefile,
      id_cols = id_cols,
      aggregations = aggregations,
      raster_is_density = raster_is_density,
      layer_to_process = layer_to_process,
      pattern_info = pattern_info,
      time_components = components
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

#' Batch Extraction of Weighted Mean from Raster Using Population Weights
#'
#' Computes population-weighted means of a value raster (e.g., PfPR2-10)
#' using a corresponding population raster as weights, across administrative
#' boundaries.
#'
#' @param value_raster_file Path to the raster file containing the values to
#'   weight (e.g., PfPR2-10)
#' @param pop_raster_file Path to the population raster file used for weighting
#' @param shapefile sf or SpatVector object of administrative boundaries
#' @param id_cols Character vector of administrative columns to keep
#' @param value_layer_to_process Integer or character. Specifies which layer in a
#'   multi-layer raster to extract. If the raster contains multiple layers
#'   (e.g., different years or indicators), this argument selects the layer to
#'   be processed. Default is 1.
#' @param weight_na_as_zero Logical; if TRUE, treats NA weights as zero
#'   (default: TRUE)
#' @param stat_type Character string specifying statistic type:
#'   - "mean": Population-weighted mean using exactextractr's weighted_mean
#'       function
#'   - "median": Population-weighted median using matrixStats::weightedMedian()
#'     approach that finds the value where 50% of the population weight lies
#'     below and 50% above, interpolating between pixel values when needed
#'   - "both": Returns both weighted mean and weighted median statistics
#'   (default: "mean")
#' @param pattern_info Optional. Pre-parsed time pattern from
#'   \code{detect_time_pattern()}. If NULL, detected from filename.
#' @param time_components Optional. Pre-parsed time components from
#'   \code{extract_time_components()}. If NULL, extracted from filename.
#'
#' @return Data frame with id_cols and population-weighted mean values
batch_extract_weighted_stats <- function(
  value_raster_file,
  pop_raster_file,
  shapefile,
  id_cols = c("adm0", "adm1", "adm2"),
  value_layer_to_process = 1,
  weight_na_as_zero = TRUE,
  stat_type = "mean",
  pattern_info = NULL,
  time_components = NULL
) {
  # validate stat_type input
  if (!stat_type %in% c("mean", "median", "both")) {
    cli::cli_abort("stat_type must be 'mean', 'median', or 'both'")
  }

  # use pre-parsed time info if provided, otherwise detect
  if (is.null(pattern_info)) {
    pattern_info <- detect_time_pattern(value_raster_file)
  }
  if (is.null(time_components)) {
    time_components <- extract_time_components(
      value_raster_file, pattern_info
    )
  }
  components <- time_components

  # load rasters
  value_rast <- terra::rast(value_raster_file)[[value_layer_to_process]]
  pop_rast <- terra::rast(pop_raster_file)[[1]]

  # reproject and resample population raster to match value raster exactly
  pop_rast_proj <- terra::project(pop_rast, terra::crs(value_rast))
  pop_rast_proj <- terra::resample(pop_rast_proj, value_rast, method = "bilinear")

  # reproject shapefile if needed
  if (!sf::st_crs(shapefile)$wkt == terra::crs(value_rast)) {
    shapefile <- sf::st_transform(shapefile, terra::crs(value_rast))
  }

  # calculate polygon population sum
  suppressWarnings(
    pop_sum <- exactextractr::exact_extract(
      pop_rast_proj,
      shapefile,
      fun = "sum",
      progress = FALSE
    ) |>
      round()
  )

  # calculate population-weighted mean
  suppressWarnings(
    weighted_mean <- exactextractr::exact_extract(
      value_rast,
      shapefile,
      fun = "weighted_mean",
      weights = pop_rast_proj,
      default_weight = if (weight_na_as_zero) 0 else NA,
      progress = FALSE
    )
  )

  # define weighted median function
  weighted_median_fun <- function(values, coverage_fractions, weights) {
    # compute weights adjusted by coverage fraction
    total_weights <- weights * coverage_fractions

    # identify valid values
    valid_idx <- !is.na(values) &
      !is.na(total_weights) &
      total_weights > 0 &
      coverage_fractions > 0

    # return missing if no valid data
    if (sum(valid_idx) == 0) {
      return(NA_real_)
    }

    # compute weighted median
    matrixStats::weightedMedian(
      values[valid_idx],
      w = total_weights[valid_idx],
      na.rm = TRUE
    )
  }

  # calculate population-weighted median
  suppressWarnings(
    weighted_median <- exactextractr::exact_extract(
      value_rast,
      shapefile,
      fun = weighted_median_fun,
      weights = pop_rast_proj,
      default_weight = if (weight_na_as_zero) 0 else NA,
      progress = FALSE
    )
  )

  # calculate unweighted mean
  suppressWarnings(
    unweighted_mean <- exactextractr::exact_extract(
      value_rast,
      shapefile,
      fun = "mean",
      progress = FALSE
    )
  )

  # calculate unweighted median
  suppressWarnings(
    unweighted_median <- exactextractr::exact_extract(
      value_rast,
      shapefile,
      fun = "median",
      progress = FALSE
    )
  )

  # decide which summary columns to return
  if (stat_type == "mean") {
    aggregations <- c("pop_sum", "weighted_mean", "unweighted_mean")
  } else if (stat_type == "median") {
    aggregations <- c(
      "pop_sum",
      "weighted_median",
      "unweighted_median"
    )
  } else if (stat_type == "both") {
    aggregations <- c(
      "pop_sum",
      "weighted_mean",
      "weighted_median",
      "unweighted_mean",
      "unweighted_median"
    )
  }

  # construct output dataframe
  result_df <- dplyr::bind_cols(
    shapefile |>
      dplyr::select(dplyr::all_of(id_cols)) |>
      sf::st_drop_geometry(),
    tibble::tibble(
      year = components$year,
      pop_sum = pop_sum,
      weighted_mean = weighted_mean,
      weighted_median = weighted_median,
      unweighted_mean = unweighted_mean,
      unweighted_median = unweighted_median
    )
  )

  # add month column if needed
  if (pattern_info$pattern == "monthly") {
    result_df <- result_df |> dplyr::mutate(month = components$month)
  }

  # identify time columns
  time_cols <- if (pattern_info$pattern == "monthly") {
    c("year", "month")
  } else {
    "year"
  }

  # return final results
  result_df |>
    dplyr::select(
      dplyr::all_of(id_cols),
      dplyr::all_of(time_cols),
      dplyr::any_of(aggregations)
    )
}

#' Process Weighted Raster Data in Batch
#'
#' @description
#' Processes multiple raster files with population weights, matching value
#' rasters with corresponding population rasters by year.
#'
#' @param value_raster_dir Character. Directory containing value raster files
#' @param pop_raster_dir Character. Directory containing population raster files
#' @param shapefile sf/SpatVector object. Shapefile for spatial aggregation
#' @param id_cols Character vector. Column names for spatial identifiers
#'        (default: c("adm0", "adm1", "adm2"))
#' @param value_pattern Character. Regex pattern to match value raster files
#'        (default: "\\.tiff$")
#' @param pop_pattern Character. Regex pattern to match population raster files
#'        (default: "\\.tif$")
#' @param value_layer_to_process Integer or character. Specifies which layer in
#'   a multi-layer raster to extract. If the raster contains multiple layers
#'   (e.g., different years or indicators), this argument selects the layer to
#'   be processed. Default is 1.
#' @param weight_na_as_zero Logical. Treat NA weights as 0 (default: TRUE)
#' @param stat_type Character string specifying statistic type:
#'   - "mean": Population-weighted mean using exactextractr's weighted_mean
#'       function
#'   - "median": Population-weighted median using matrixStats::weightedMedian()
#'     approach that finds the value where 50% of the population weight lies
#'     below and 50% above, interpolating between pixel values when needed
#'   - "both": Returns both weighted mean and weighted median statistics
#'   (default: "mean")
#'
#' @return A data frame containing:
#'   - Spatial identifiers from id_cols
#'   - Weighted mean values
#'   - Metadata (filename, year, month, semester, quarter, date)
#'
#' @examples
#' \dontrun{
#' results <- process_weighted_raster_collection(
#'   value_raster_dir = "path/to/values",
#'   pop_raster_dir = "path/to/population",
#'   shapefile = my_shapefile,
#'   id_cols = c("adm0", "adm1")
#' )
#' }
#' @export
process_weighted_raster_collection <- function(
  value_raster_dir,
  pop_raster_dir,
  shapefile,
  id_cols = c("adm0", "adm1", "adm2"),
  value_pattern = "\\.tiff$",
  pop_pattern = "\\.tif$",
  value_layer_to_process = 1,
  weight_na_as_zero = TRUE,
  stat_type = "mean"
) {
  # input validation
  if (!stat_type %in% c("mean", "median", "both")) {
    cli::cli_abort("stat_type must be 'mean', 'median', or 'both'")
  }

  # list value rasters (original paths stay untouched)
  value_raster_files <- list.files(
    value_raster_dir,
    pattern = value_pattern,
    full.names = TRUE
  )

  if (length(value_raster_files) == 0) {
    cli::cli_abort(
      "No value raster files found in '{value_raster_dir}'."
    )
  }

  # list population rasters (original paths stay untouched)
  pop_raster_files <- list.files(
    pop_raster_dir,
    pattern = pop_pattern,
    full.names = TRUE
  )

  if (length(pop_raster_files) == 0) {
    cli::cli_abort(
      "No population raster files found in '{pop_raster_dir}'."
    )
  }

  # clean names for date detection only; never rename on disk
  cleaned_value_names <- clean_filenames(value_raster_files)
  pattern_info <- detect_time_pattern(cleaned_value_names)

  single_pop <- length(pop_raster_files) == 1

  # initialize progress bar
  cli::cli_progress_bar(
    "Extracting population-weighted statistics",
    total = length(value_raster_files)
  )

  results <- vector("list", length(value_raster_files))

  for (i in seq_along(value_raster_files)) {
    # parse time from cleaned name, read from original path
    components <- extract_time_components(
      cleaned_value_names[i], pattern_info
    )
    year_str <- as.character(components$year)

    # match population raster: use single pop for all if only one
    if (single_pop) {
      matched_pop <- pop_raster_files
    } else {
      matched_pop <- pop_raster_files[
        grepl(year_str, basename(pop_raster_files))
      ]
    }

    if (length(matched_pop) == 0) {
      cli::cli_abort(paste0(
        "No population raster matching year {year_str}",
        " found for {basename(value_raster_files[i])}."
      ))
    } else if (length(matched_pop) > 1) {
      cli::cli_abort(
        "Multiple population rasters found for year {year_str}."
      )
    }

    results[[i]] <- batch_extract_weighted_stats(
      value_raster_file = value_raster_files[i],
      pop_raster_file = matched_pop,
      shapefile = shapefile,
      id_cols = id_cols,
      value_layer_to_process = value_layer_to_process,
      weight_na_as_zero = weight_na_as_zero,
      stat_type = stat_type,
      pattern_info = pattern_info,
      time_components = components
    )

    cli::cli_progress_update()
  }

  cli::cli_progress_done()

  # combine and sort results
  combined_results <- dplyr::bind_rows(results)
  sort_cols <- c("year", "month", id_cols)
  sort_cols <- sort_cols[sort_cols %in% names(combined_results)]

  if (length(sort_cols) > 0) {
    combined_results <- combined_results |>
      dplyr::arrange(dplyr::across(dplyr::all_of(sort_cols)))
  }

  return(combined_results)
}

#' Normalize Raster Values by Polygon Regions
#'
#' This function normalizes raster values within each polygon region by dividing
#' the values by the total sum for that region. This creates proportional values
#' that sum to 1 within each polygon.
#'
#' @param raster A SpatRaster object containing the values to be normalized
#' @param shp A spatial vector (SpatVector) containing the polygon regions
#' @param id_col Character string specifying the column name in shp that contains
#'   the region identifiers
#'
#' @return A SpatRaster object with normalized values where the sum within each
#'   polygon region equals 1
#'
#' @details The function processes each polygon region separately by:
#'   1. Masking the raster to the region boundaries
#'   2. Calculating the total sum for the region
#'   3. Dividing all values in that region by the total
#'   The results are then mosaicked back together into a single raster.
#'
#' @examples
#' \dontrun{
#' normalized <- normalize_raster_by_polygon(
#'   raster = population_raster,
#'   shp = admin_boundaries,
#'   id_col = "region_id"
#' )
#' }
#' @export
normalize_raster_by_polygon <- function(raster, shp, id_col) {
  # Pre-calculate all masks at once
  all_masks <- terra::mask(raster, shp)

  # Get unique region IDs and convert to vector for faster access
  region_ids <- as.vector(unique(shp[[id_col]]))

  # Pre-allocate list for better memory management
  normalized_rasters <- vector("list", length(region_ids))

  # Process each region
  for (i in seq_along(region_ids)) {
    id <- region_ids[i]
    this_region <- shp[shp[[id_col]] == id, ]
    masked_pop <- terra::mask(all_masks, this_region)
    total_pop <- terra::global(masked_pop, sum, na.rm = TRUE)[1, 1]
    normalized_rasters[[i]] <- masked_pop / total_pop
  }

  # Combine normalized rasters into one
  normalized_collection <- terra::sprc(normalized_rasters)
  terra::mosaic(normalized_collection)
}


#' Extract Under-5 Mortality Values from IHME Raster Stack
#'
#' This function extracts under-5 mortality values from IHME (Institute for
#' Health Metrics and Evaluation) raster data for specified geographic areas
#' using exact extraction methods. The data source is the IHME's geospatial
#' estimates for under-5 mortality rates in low and middle-income countries
#' (LMICs) from 2000-2017.
#'
#' @param shape An sf object containing the geographic areas for extraction
#' @param raster_stack A terra SpatRaster or raster stack containing IHME
#'        mortality data
#' @param id_col Character vector of column names in shape to use as identifiers
#'        (default: c("adm1", "adm2"))
#' @param rates Logical indicating whether values are rates (TRUE) or counts
#'        (TRUE, default)
#' @param stat_type Character string specifying statistic type: "mean", "median",
#'        or "both". Only applies when rates = TRUE. For counts, sum is always used.
#'
#' @return A tibble containing extracted values for each area and year with
#'         columns for area identifiers, year, and mortality values. For rates,
#'         values represent deaths per 1,000 live births. For counts, values
#'         represent total number of deaths.
#'
#' @details The function processes each layer in the IHME raster stack, which
#'          represents annual estimates starting from 2000. For rates, mean
#'          or median values are extracted based on stat_type; for counts,
#'          sums are used. The IHME data provides high-resolution (5 x 5 km)
#'          estimates of under-5 mortality across LMICs.
#'
#' @source \url{https://ghdx.healthdata.org/record/ihme-data/lmic-under5-mortality-rate-geospatial-estimates-2000-2017}
#'
#' @examples
#' \dontrun{
#' # Load required libraries and data
#' library(sf)
#' library(terra)
#'
#' # Example with rates using mean (default)
#' mortality_rates_mean <- process_ihme_u5m_raster(
#'   shape = admin_boundaries,
#'   raster_stack = mortality_raster,
#'   id_col = c("adm1", "adm2"),
#'   rates = TRUE,
#'   stat_type = "mean"
#' )
#'
#' # Example with rates using median
#' mortality_rates_median <- process_ihme_u5m_raster(
#'   shape = admin_boundaries,
#'   raster_stack = mortality_raster,
#'   id_col = c("adm1", "adm2"),
#'   rates = TRUE,
#'   stat_type = "median"
#' )
#'
#' # Example with both mean and median
#' mortality_rates_both <- process_ihme_u5m_raster(
#'   shape = admin_boundaries,
#'   raster_stack = mortality_raster,
#'   id_col = c("adm1", "adm2"),
#'   rates = TRUE,
#'   stat_type = "both"
#' )
#'
#' # Example with counts (total deaths - always uses sum)
#' mortality_counts <- process_ihme_u5m_raster(
#'   shape = admin_boundaries,
#'   raster_stack = mortality_raster,
#'   id_col = c("adm1"),
#'   rates = FALSE
#' )
#' }
#'
#' @note The IHME raster data represents model-based estimates and should be
#'        usedwith consideration of the underlying methodology and uncertainties
#'       described in the source documentation.
#'
#' @export
process_ihme_u5m_raster <- function(
  shape,
  raster_stack,
  id_col = c("adm1", "adm2"),
  rates = TRUE,
  stat_type = "mean"
) {
  # input validation
  if (!stat_type %in% c("mean", "median", "both")) {
    stop("stat_type must be 'mean', 'median', or 'both'")
  }

  if (!rates && stat_type != "mean") {
    cli::cli_warn(
      "stat_type is ignored when rates = FALSE. using sum for counts."
    )
  }

  type <- if (rates) "rate" else "count"

  # define extraction functions
  median_fun <- function(values, coverage_fractions) {
    valid_idx <- !is.na(values) & coverage_fractions > 0

    if (sum(valid_idx) == 0 || length(values[valid_idx]) == 0) {
      return(NA_real_)
    }

    # use coverage fractions as weights for median
    matrixStats::weightedMedian(
      values[valid_idx],
      w = coverage_fractions[valid_idx],
      na.rm = TRUE
    )
  }

  both_stats_fun <- function(values, coverage_fractions) {
    valid_idx <- !is.na(values) & coverage_fractions > 0

    if (sum(valid_idx) == 0 || length(values[valid_idx]) == 0) {
      return(data.frame(mean_val = NA_real_, median_val = NA_real_))
    }

    clean_values <- values[valid_idx]
    clean_fractions <- coverage_fractions[valid_idx]

    data.frame(
      mean_val = sum(clean_values * clean_fractions) / sum(clean_fractions),
      median_val = matrixStats::weightedMedian(
        clean_values,
        w = clean_fractions,
        na.rm = TRUE
      )
    )
  }

  cli::cli_h2("starting u5 mortality {type} extraction from raster stack")

  stopifnot(inherits(shape, "sf"))
  stopifnot(length(id_col) >= 1)
  stopifnot(all(id_col %in% colnames(shape)))

  shape <- sf::st_transform(shape, crs = terra::crs(raster_stack))
  n_layers <- terra::nlyr(raster_stack)
  years <- 2000 + seq_len(n_layers) - 1

  results <- vector("list", length = n_layers)

  for (i in seq_len(n_layers)) {
    yr <- years[i]
    layer <- raster::raster(raster_stack[[i]])

    cli::cli_progress_step("extracting year {yr} ({i}/{n_layers})")

    # extract values based on rates and stat_type
    if (!rates) {
      # for counts, always use sum
      u5_vals <- exactextractr::exact_extract(
        layer,
        shape,
        "sum",
        progress = FALSE
      )

      ids <- shape |>
        sf::st_drop_geometry() |>
        dplyr::select(dplyr::all_of(id_col))

      results[[i]] <- dplyr::bind_cols(
        ids,
        tibble::tibble(year = yr, !!paste0("u5m_", type) := u5_vals)
      )
    } else {
      # for rates, use specified stat_type
      if (stat_type == "mean") {
        u5_vals <- exactextractr::exact_extract(
          layer,
          shape,
          "mean",
          progress = FALSE
        )

        ids <- shape |>
          sf::st_drop_geometry() |>
          dplyr::select(dplyr::all_of(id_col))

        results[[i]] <- dplyr::bind_cols(
          ids,
          tibble::tibble(year = yr, !!paste0("u5m_", type) := u5_vals)
        )
      } else if (stat_type == "median") {
        u5_vals <- exactextractr::exact_extract(
          layer,
          shape,
          median_fun,
          progress = FALSE
        )

        ids <- shape |>
          sf::st_drop_geometry() |>
          dplyr::select(dplyr::all_of(id_col))

        results[[i]] <- dplyr::bind_cols(
          ids,
          tibble::tibble(year = yr, !!paste0("u5m_", type) := u5_vals)
        )
      } else if (stat_type == "both") {
        stats_results <- exactextractr::exact_extract(
          layer,
          shape,
          both_stats_fun,
          progress = FALSE
        )

        ids <- shape |>
          sf::st_drop_geometry() |>
          dplyr::select(dplyr::all_of(id_col))

        results[[i]] <- dplyr::bind_cols(
          ids,
          tibble::tibble(
            year = yr,
            !!paste0("u5m_", type, "_mean") := stats_results$mean_val,
            !!paste0("u5m_", type, "_median") := stats_results$median_val
          )
        )
      }
    }
  }

  cli::cli_alert_success("extraction complete for {n_layers} years.")

  dplyr::bind_rows(results)
}

#' Process Weighted Raster Stacks
#'
#' This function processes raster stacks by calculating weighted statistics
#' based on population data for each shape in the input shapefile across
#' multiple years.
#'
#' @param value_raster A SpatRaster object containing the values to be weighted
#' @param pop_raster A SpatRaster object containing population weights
#' @param shape A sf/shapefile object containing geometries and shape_id column
#' @param value_var Character string for naming the weighted value column
#' @param start_year Numeric indicating the starting year for the time series
#' @param stat_type Character string specifying statistic type:
#'   - "mean": Population-weighted mean using exactextractr's weighted_mean
#'       function
#'   - "median": Population-weighted median using matrixStats::weightedMedian()
#'     approach that finds the value where 50% of the population weight lies
#'     below and 50% above, interpolating between pixel values when needed
#'   - "both": Returns both weighted mean and weighted median statistics
#'   (default: "mean")
#'
#' @return A tibble with columns for shape_id, year, and the weighted values
#' @export
process_weighted_raster_stacks <- function(
  value_raster,
  pop_raster,
  shape,
  value_var = "indicator_weighted",
  start_year = 2000,
  stat_type = "mean"
) {
  # Input validation
  if (
    !inherits(value_raster, "SpatRaster") ||
      !inherits(pop_raster, "SpatRaster")
  ) {
    stop("value_raster and pop_raster must be SpatRaster objects")
  }
  if (!inherits(shape, "sf")) {
    stop("shape must be an sf object")
  }
  if (terra::nlyr(value_raster) != terra::nlyr(pop_raster)) {
    stop("value_raster and pop_raster must have the same number of layers")
  }
  if (!stat_type %in% c("mean", "median", "both")) {
    stop("stat_type must be 'mean', 'median', or 'both'")
  }

  # Step 1: Crop and resample value raster to match population raster
  value_raster_cropped <- terra::crop(value_raster, pop_raster)
  value_raster_resampled <- terra::resample(value_raster_cropped, pop_raster)

  # Define weighted median function
  weighted_median_fun <- function(values, coverage_fractions, weights) {
    total_weights <- weights * coverage_fractions
    valid_idx <- !is.na(values) &
      !is.na(total_weights) &
      total_weights > 0 &
      coverage_fractions > 0

    if (sum(valid_idx) == 0 || length(values[valid_idx]) == 0) {
      return(NA_real_)
    }

    matrixStats::weightedMedian(
      values[valid_idx],
      w = total_weights[valid_idx],
      na.rm = TRUE
    )
  }

  # Define combined statistics function
  combined_stats_fun <- function(values, coverage_fractions, weights) {
    total_weights <- weights * coverage_fractions
    valid_idx <- !is.na(values) &
      !is.na(total_weights) &
      total_weights > 0 &
      coverage_fractions > 0

    if (sum(valid_idx) == 0 || length(values[valid_idx]) == 0) {
      return(data.frame(
        weighted_mean = NA_real_,
        weighted_median = NA_real_
      ))
    }

    clean_values <- values[valid_idx]
    clean_weights <- total_weights[valid_idx]

    if (sum(clean_weights) == 0) {
      return(data.frame(
        weighted_mean = NA_real_,
        weighted_median = NA_real_
      ))
    }

    data.frame(
      weighted_mean = sum(clean_values * clean_weights) / sum(clean_weights),
      weighted_median = matrixStats::weightedMedian(
        clean_values,
        w = clean_weights,
        na.rm = TRUE
      )
    )
  }

  # Step 2: Extract weighted statistics per year
  results_list <- purrr::map(
    seq_len(terra::nlyr(value_raster_resampled)),
    ~ {
      year_value <- start_year - 1 + .x
      shape_data <- sf::st_drop_geometry(shape)

      if (stat_type == "mean") {
        vals <- exactextractr::exact_extract(
          x = value_raster_resampled[[.x]],
          y = shape,
          weights = pop_raster[[.x]],
          default_weight = 0,
          fun = "weighted_mean"
        )

        dplyr::bind_cols(
          shape_data,
          tibble::tibble(
            year = year_value,
            !!value_var := vals
          )
        )
      } else if (stat_type == "median") {
        vals <- exactextractr::exact_extract(
          x = value_raster_resampled[[.x]],
          y = shape,
          weights = pop_raster[[.x]],
          default_weight = 0,
          fun = weighted_median_fun
        )

        dplyr::bind_cols(
          shape_data,
          tibble::tibble(
            year = year_value,
            !!value_var := vals
          )
        )
      } else if (stat_type == "both") {
        stats_results <- exactextractr::exact_extract(
          x = value_raster_resampled[[.x]],
          y = shape,
          weights = pop_raster[[.x]],
          default_weight = 0,
          fun = combined_stats_fun
        )

        # Create column names based on value_var
        mean_col <- paste0(value_var, "_mean")
        median_col <- paste0(value_var, "_median")

        dplyr::bind_cols(
          shape_data,
          tibble::tibble(
            year = year_value,
            !!mean_col := stats_results$weighted_mean,
            !!median_col := stats_results$weighted_median
          )
        )
      }
    }
  )

  dplyr::bind_rows(results_list)
}
