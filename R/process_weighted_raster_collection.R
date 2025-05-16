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
#' @param weight_na_as_zero Logical; if TRUE, treats NA weights as zero
#'   (default: TRUE)
#'
#' @return Data frame with id_cols and population-weighted mean values
#' @export
batch_extract_weighted_mean <- function(value_raster_file,
                                        pop_raster_file,
                                        shapefile,
                                        id_cols = c("adm0", "adm1", "adm2"),
                                        weight_na_as_zero = TRUE) {
  # Detect time pattern and extract components
  pattern_info <- detect_time_pattern(value_raster_file)
  components <- extract_time_components(value_raster_file, pattern_info)

  # Load rasters
  value_rast <- terra::rast(value_raster_file)[[1]]
  pop_rast <- terra::rast(pop_raster_file)[[1]]

  # Reproject population raster to match value raster CRS
  pop_rast_proj <- terra::project(pop_rast, terra::crs(value_rast))

  # Reproject shapefile to match raster CRS if needed
  if (!sf::st_crs(shapefile)$wkt == terra::crs(value_rast)) {
    shapefile <- sf::st_transform(shapefile, terra::crs(value_rast))
  }

  # Align population raster grid with value raster grid
  terra::origin(pop_rast_proj) <- terra::origin(value_rast)

  # Resample population raster to value raster resolution
  pop_rast_match <- terra::resample(pop_rast_proj,
                                    value_rast,
                                    method = "near"
  )

  # Calculate population sum per polygon (for fallback detection)
  pop_sum <- exactextractr::exact_extract(
    pop_rast_match,
    shapefile,
    fun = "sum"
  ) |> round()

  # Calculate population-weighted mean
  weighted_mean <- exactextractr::exact_extract(
    value_rast,
    shapefile,
    fun = "weighted_mean",
    weights = pop_rast_match,
    default_weight = if (weight_na_as_zero) 0 else NA
  )

  # Calculate unweighted mean for fallback
  unweighted_mean <- exactextractr::exact_extract(
    value_rast,
    shapefile,
    fun = "mean"
  )

  # Fallback to unweighted mean if pop_sum is 0
  final_mean <- ifelse(pop_sum == 0, unweighted_mean, weighted_mean)

  # Define aggregation columns
  aggregations <- c("pop_sum", "weighted_mean", "unweighted_mean", "final_mean")

  # Create initial results dataframe
  result_df <- dplyr::bind_cols(
    shapefile |>
      dplyr::select(dplyr::all_of(id_cols)) |>
      sf::st_drop_geometry(),
    tibble::tibble(
      year = components$year,
      pop_sum = pop_sum,
      weighted_mean = weighted_mean,
      unweighted_mean = unweighted_mean,
      final_mean = final_mean
    )
  )

  # Add month if pattern is monthly
  if (pattern_info$pattern == "monthly") {
    result_df <- result_df |> dplyr::mutate(month = components$month)
  }

  # Define time columns based on pattern
  time_cols <- if (pattern_info$pattern == "monthly") {
    c("year", "month")
  } else {
    "year"
  }

  # Return final results
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
#' @param weight_na_as_zero Logical. Treat NA weights as 0 (default: TRUE)
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
process_weighted_raster_collection <- function(value_raster_dir,
                                               pop_raster_dir,
                                               shapefile,
                                               id_cols = c("adm0", "adm1",
                                                           "adm2"),
                                               value_pattern = "\\.tiff$",
                                               pop_pattern = "\\.tif$",
                                               weight_na_as_zero = TRUE) {
  # List and clean value raster filenames
  value_raster_files <- list.files(
    value_raster_dir,
    pattern = value_pattern,
    full.names = TRUE
  ) |>
    clean_filenames()

  if (length(value_raster_files) == 0) {
    cli::cli_abort("No value raster files found in '{value_raster_dir}'.")
  }

  # List and clean population raster filenames
  pop_raster_files <- list.files(
    pop_raster_dir,
    pattern = pop_pattern,
    full.names = TRUE
  ) |>
    clean_filenames()

  if (length(pop_raster_files) == 0) {
    cli::cli_abort("No population raster files found in '{pop_raster_dir}'.")
  }

  # Detect time pattern once for value rasters
  pattern_info <- detect_time_pattern(value_raster_files)

  # Progress bar
  pb <- progress::progress_bar$new(
    format = "Processing [:bar] :percent | ETA: :eta",
    total = length(value_raster_files)
  )

  # Process each value raster
  results <- lapply(value_raster_files, function(value_file) {
    # Extract time components
    time_components <- extract_time_components(value_file, pattern_info)
    year_str <- as.character(time_components$year)

    # Find matching population raster by year
    matched_pop <- pop_raster_files[grepl(year_str,
                                          basename(pop_raster_files))]

    if (length(matched_pop) == 0) {
      cli::cli_abort(
        paste0(
          "No population raster matching year {year_str} found",
          " for {basename(value_file)}."
        )
      )
    } else if (length(matched_pop) > 1) {
      cli::cli_abort(
        paste0(
          "Multiple population rasters found for year {year_str}. ",
          "Please resolve duplicates."
        )
      )
    }

    # Process with matched population raster
    result <- batch_extract_weighted_mean(
      value_raster_file = value_file,
      pop_raster_file = matched_pop,
      shapefile = shapefile,
      id_cols = id_cols,
      weight_na_as_zero = weight_na_as_zero
    )

    pb$tick()
    return(result)
  })

  # Combine and sort results
  combined_results <- dplyr::bind_rows(results)
  sort_cols <- c("year", "month", id_cols)
  sort_cols <- sort_cols[sort_cols %in% names(combined_results)]

  if (length(sort_cols) > 0) {
    combined_results <- combined_results |>
      dplyr::arrange(dplyr::across(dplyr::all_of(sort_cols)))
  }

  return(combined_results)
}
