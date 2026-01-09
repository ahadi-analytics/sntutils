#' Download NASA POWER data for a single location
#'
#' Internal helper that downloads daily climate data from NASA POWER API for
#' one administrative unit. Uses exponential backoff retry logic on failure.
#'
#' @param row Single-row data frame with `lon`, `lat`, and admin columns.
#' @param power_community NASA POWER community (e.g., "ag", "re", "sb").
#' @param start_date Start date as Date object.
#' @param end_date End date as Date object.
#' @param power_vars Character vector of POWER variable codes.
#' @param unit_col Name of the column identifying the unit (for error messages).
#' @param admin_cols Character vector of admin column names to bind to result.
#' @param max_retries Maximum retry attempts before giving up.
#'
#' @return Data frame with POWER data and admin columns, or NULL on failure.
#'
#' @noRd
.download_power_location <- function(
    row,
    power_community,
    start_date,
    end_date,
    power_vars,
    unit_col,
    admin_cols,
    max_retries
) {
  for (attempt in seq_len(max_retries)) {
    out <- tryCatch(
      {
        data <- nasapower::get_power(
          community = power_community,
          temporal_api = "daily",
          lonlat = c(row$lon, row$lat),
          dates = c(start_date, end_date),
          pars = power_vars
        )

        if (!"YYYYMMDD" %in% names(data)) {
          cli::cli_abort(
            "POWER response missing YYYYMMDD for {row[[unit_col]]}"
          )
        }

        dplyr::bind_cols(data, row[admin_cols])
      },
      error = function(e) {
        if (attempt < max_retries) {
          Sys.sleep(2^attempt)
          NULL
        } else {
          NULL
        }
      }
    )

    if (!is.null(out)) {
      return(out)
    }
  }

  NULL
}

#' Download and process NASA POWER daily climate data
#'
#' Downloads daily climate variables from the NASA POWER API for representative
#' administrative units, processes them into daily and monthly datasets, and
#' generates data dictionaries for each output.
#'
#' Representative locations are derived using a point-on-surface approach
#' applied to administrative polygons. Returned values correspond to NASA POWER
#' grid-cell averages (approximately 0.5 degree resolution) and should be
#' interpreted as regional conditions rather than point measurements.
#'
#' The function is robust to partial download failures. Administrative units
#' that fail to download after the specified number of retries are skipped with
#' a warning. Execution stops only if no units return data.
#'
#' @param adm_sf An sf object containing administrative polygons. Must include
#'   columns specified in \code{admin_cols} and have a valid CRS.
#'
#' @param admin_cols Character vector of administrative identifier columns,
#'   ordered from highest to lowest level. The last column is treated as the
#'   unit identifier. Default is \code{c("adm0", "adm1", "adm2")}.
#'
#' @param start_date Start date in \code{"YYYY-MM-DD"} format.
#'
#' @param end_date End date in \code{"YYYY-MM-DD"} format.
#'
#' @param power_vars Character vector of NASA POWER variable codes to download.
#'
#' @param point_crs EPSG code used for representative point extraction.
#'   Default is 6933 (World Cylindrical Equal Area).
#'
#' @param power_community NASA POWER community parameter. Default is "ag"
#'   (agroclimatology).
#'
#' @param max_retries Maximum retry attempts per unit. Default is 3.
#'
#' @param dict_language Language for data dictionaries. Default is "fr".
#'
#' @return A list with four elements:
#'   \describe{
#'     \item{daily}{Tibble of daily climate data with admin columns}
#'     \item{monthly}{Tibble of monthly aggregated climate data}
#'     \item{dict_daily}{Data dictionary for daily data}
#'     \item{dict_monthly}{Data dictionary for monthly data}
#'   }
#'
#' @examples
#' \dontrun{
#' # load administrative boundaries
#' adm_sf <- sf::st_read("path/to/admin_boundaries.shp")
#'
#' # download NASA POWER data for 2024
#' result <- download_process_nasapower(
#'   adm_sf = adm_sf,
#'   admin_cols = c("adm0", "adm1", "adm2"),
#'   start_date = "2024-01-01",
#'   end_date = "2024-12-31"
#' )
#'
#' # access daily and monthly data
#' daily_data <- result$daily
#' monthly_data <- result$monthly
#' }
#'
#' @export
download_process_nasapower <- function(
    adm_sf,
    admin_cols = c("adm0", "adm1", "adm2"),
    start_date = "2025-01-01",
    end_date = "2025-12-31",
    power_vars = c(
      "PRECTOTCORR",
      "T2M_MAX",
      "T2M_MIN",
      "T2M",
      "TS",
      "TS_MAX",
      "TS_MIN",
      "RH2M"
    ),
    point_crs = 6933,
    power_community = "ag",
    max_retries = 3,
    dict_language = "fr"
) {
  cli::cli_h1("Download and process NASA POWER climate data")

  # -------------------------------------------------------------------------
  # input validation
  # -------------------------------------------------------------------------

  if (!inherits(adm_sf, "sf")) {
    cli::cli_abort("adm_sf must be an sf object")
  }

  missing_cols <- setdiff(admin_cols, names(adm_sf))

  if (length(missing_cols) > 0) {
    cli::cli_abort(
      "adm_sf is missing required columns: {missing_cols}"
    )
  }

  if (is.na(sf::st_crs(adm_sf))) {
    cli::cli_abort("adm_sf must have a defined CRS")
  }

  start_date <- tryCatch(
    as.Date(start_date),
    error = function(e) NA
  )
  end_date <- tryCatch(
    as.Date(end_date),
    error = function(e) NA
  )

  if (is.na(start_date) || is.na(end_date)) {
    cli::cli_abort("start_date and end_date must be valid dates")
  }

  if (start_date > end_date) {
    cli::cli_abort("start_date must be before end_date")
  }

  unit_col <- admin_cols[length(admin_cols)]

  # -------------------------------------------------------------------------
  # show request summary
  # -------------------------------------------------------------------------

  cli::cli_h2("Request summary")
  cli::cli_alert_info("Date range: {start_date} to {end_date}")
  cli::cli_alert_info("Variables: {paste(power_vars, collapse = ', ')}")
  cli::cli_alert_info("Community: {power_community}")

  # -------------------------------------------------------------------------
  # representative points
  # -------------------------------------------------------------------------

  cli::cli_h2("Deriving representative locations")

  cli::cli_alert_info(
    "Using point-on-surface (attributes assumed constant within polygons)"
  )

  # suppress sf warning since we inform user above
  admin_coords <- suppressWarnings({
    adm_sf |>
      sf::st_transform(point_crs) |>
      sf::st_point_on_surface() |>
      sf::st_transform(4326) |>
      dplyr::mutate(
        lon = sf::st_coordinates(geometry)[, 1],
        lat = sf::st_coordinates(geometry)[, 2]
      ) |>
      sf::st_drop_geometry()
  })

  cli::cli_alert_info(
    "NASA POWER returns grid-cell averages (~0.5 deg resolution)"
  )

  if (nrow(admin_coords) == 0) {
    cli::cli_abort("No administrative units produced")
  }

  cli::cli_alert_success(
    "Derived {nrow(admin_coords)} representative locations"
  )

  # -------------------------------------------------------------------------
  # download
  # -------------------------------------------------------------------------

  cli::cli_h2("Downloading POWER data")

  n_units <- nrow(admin_coords)
  success_count <- 0
  fail_count <- 0
  failed_units <- character(0)

  results_list <- vector("list", n_units)

  cli::cli_progress_bar(
    "Downloading",
    total = n_units,
    format = "{cli::pb_spin} Downloading [{cli::pb_current}/{cli::pb_total}] | "
  )

  for (i in seq_len(n_units)) {
    result <- .download_power_location(
      row = admin_coords[i, ],
      power_community = power_community,
      start_date = start_date,
      end_date = end_date,
      power_vars = power_vars,
      unit_col = unit_col,
      admin_cols = admin_cols,
      max_retries = max_retries
    )

    if (!is.null(result)) {
      results_list[[i]] <- result
      success_count <- success_count + 1
    } else {
      fail_count <- fail_count + 1
      failed_units <- c(failed_units, admin_coords[[unit_col]][i])
    }

    cli::cli_progress_update()
  }

  cli::cli_progress_done()

  # report download results
  if (success_count > 0) {
    cli::cli_alert_success(
      "Downloaded data for {success_count}/{n_units} units"
    )
  }

  if (fail_count > 0) {
    cli::cli_alert_warning(
      "Failed to download {fail_count} units: {.val {failed_units}}"
    )
  }

  power_data_raw <- dplyr::bind_rows(results_list)

  if (nrow(power_data_raw) == 0) {
    cli::cli_abort("No POWER data downloaded")
  }


  # -------------------------------------------------------------------------
  # daily processing
  # -------------------------------------------------------------------------

  cli::cli_h2("Processing daily data")

  power_daily <- power_data_raw |>
    dplyr::mutate(
      date = as.Date(YYYYMMDD),
      year = YEAR,
      month = MM,
      location = paste(adm1, "~", adm2),
      yearmon = factor(
        sntutils::translate_yearmon(date),
        levels = unique(
          sntutils::translate_yearmon(sort(unique(date)))
        )
      )
    ) |>
    dplyr::select(
      adm0,
      adm1,
      adm2,
      location,
      date,
      year,
      month,
      yearmon,
      rainfall_mm = PRECTOTCORR,
      max_air_temperature_c = T2M_MAX,
      min_air_temperature_c = T2M_MIN,
      mean_air_temperature_c = T2M,
      max_land_temperature_c = TS_MAX,
      min_land_temperature_c = TS_MIN,
      mean_land_temperature_c = TS,
      humidity_pct = RH2M
    )

  cli::cli_alert_success("Processed {nrow(power_daily)} daily records")
  cli::cli_alert_info(
    "Date coverage: {min(power_daily$date)} to {max(power_daily$date)}"
  )

  # -------------------------------------------------------------------------
  # monthly aggregation
  # -------------------------------------------------------------------------

  cli::cli_h2("Aggregating to monthly")

  power_monthly <- power_daily |>
    dplyr::group_by(adm0, adm1, adm2, location, year, month, yearmon) |>
    dplyr::summarise(
      # rainfall
      total_rainfall_mm = sum(rainfall_mm, na.rm = TRUE),
      mean_rainfall_mm = mean(rainfall_mm, na.rm = TRUE),
      median_rainfall_mm = median(rainfall_mm, na.rm = TRUE),
      min_rainfall_mm = min(rainfall_mm, na.rm = TRUE),
      max_rainfall_mm = max(rainfall_mm, na.rm = TRUE),
      # air temperature - maximum (2m)
      mean_max_air_temperature_c = mean(max_air_temperature_c, na.rm = TRUE),
      median_max_air_temperature_c =
        median(max_air_temperature_c, na.rm = TRUE),
      min_max_air_temperature_c = min(max_air_temperature_c, na.rm = TRUE),
      max_max_air_temperature_c = max(max_air_temperature_c, na.rm = TRUE),
      # air temperature - minimum (2m)
      mean_min_air_temperature_c = mean(min_air_temperature_c, na.rm = TRUE),
      median_min_air_temperature_c =
        median(min_air_temperature_c, na.rm = TRUE),
      min_min_air_temperature_c = min(min_air_temperature_c, na.rm = TRUE),
      max_min_air_temperature_c = max(min_air_temperature_c, na.rm = TRUE),
      # air temperature - mean (2m)
      mean_air_temperature_c = mean(mean_air_temperature_c, na.rm = TRUE),
      median_air_temperature_c = median(mean_air_temperature_c, na.rm = TRUE),
      min_air_temperature_c = min(mean_air_temperature_c, na.rm = TRUE),
      max_air_temperature_c = max(mean_air_temperature_c, na.rm = TRUE),
      # land/surface temperature - maximum
      mean_max_land_temperature_c = mean(max_land_temperature_c, na.rm = TRUE),
      median_max_land_temperature_c =
        median(max_land_temperature_c, na.rm = TRUE),
      min_max_land_temperature_c = min(max_land_temperature_c, na.rm = TRUE),
      max_max_land_temperature_c = max(max_land_temperature_c, na.rm = TRUE),
      # land/surface temperature - minimum
      mean_min_land_temperature_c = mean(min_land_temperature_c, na.rm = TRUE),
      median_min_land_temperature_c =
        median(min_land_temperature_c, na.rm = TRUE),
      min_min_land_temperature_c = min(min_land_temperature_c, na.rm = TRUE),
      max_min_land_temperature_c = max(min_land_temperature_c, na.rm = TRUE),
      # land/surface temperature - mean
      mean_land_temperature_c = mean(mean_land_temperature_c, na.rm = TRUE),
      median_land_temperature_c = median(mean_land_temperature_c, na.rm = TRUE),
      min_land_temperature_c = min(mean_land_temperature_c, na.rm = TRUE),
      max_land_temperature_c = max(mean_land_temperature_c, na.rm = TRUE),
      # humidity
      mean_humidity_pct = mean(humidity_pct, na.rm = TRUE),
      median_humidity_pct = median(humidity_pct, na.rm = TRUE),
      min_humidity_pct = min(humidity_pct, na.rm = TRUE),
      max_humidity_pct = max(humidity_pct, na.rm = TRUE),
      .groups = "drop"
    )

  cli::cli_alert_success("Aggregated to {nrow(power_monthly)} monthly records")

  # -------------------------------------------------------------------------
  # dictionaries
  # -------------------------------------------------------------------------

  cli::cli_h2("Building data dictionaries")

  dict_daily <- sntutils::build_dictionary(
    data = power_daily,
    language = dict_language
  )

  dict_monthly <- sntutils::build_dictionary(
    data = power_monthly,
    language = dict_language
  )

  cli::cli_alert_success("Created dictionaries in '{dict_language}'")

  # -------------------------------------------------------------------------
  # summary
  # -------------------------------------------------------------------------

  cli::cli_rule(
    left = "NASA POWER processing complete",
    right = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  )

  list(
    daily = power_daily,
    monthly = power_monthly,
    dict_daily = dict_daily,
    dict_monthly = dict_monthly
  )
}
