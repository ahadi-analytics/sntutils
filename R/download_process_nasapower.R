#' Parse grid-cell elevation from a NASA POWER response
#'
#' Pulls the MERRA-2 grid-cell elevation from the `POWER.Elevation` attribute
#' attached to objects returned by [nasapower::get_power()]. Free metadata,
#' no extra API call.
#'
#' @param power_obj Object returned by [nasapower::get_power()].
#'
#' @return Numeric scalar elevation in metres, or `NA_real_` if missing.
#'
#' @noRd
.parse_power_elevation <- function(power_obj) {
  meta <- attr(power_obj, "POWER.Elevation")
  if (is.null(meta)) return(NA_real_)
  matched <- regmatches(
    meta,
    regexpr("([0-9]+\\.?[0-9]*)\\s*meters", meta)
  )
  if (length(matched) == 0L) return(NA_real_)
  as.numeric(sub("\\s*meters", "", matched))
}

#' Download NASA POWER data for a single location
#'
#' Internal helper that downloads daily climate data from NASA POWER API for
#' one administrative unit. Uses exponential backoff retry logic on failure.
#' Also parses the grid-cell elevation from the response metadata and attaches
#' it to every row as a static `elevation_m` column.
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
#' @return Data frame with POWER data, admin columns, and `elevation_m`, or
#'   NULL on failure.
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

        elevation_m <- .parse_power_elevation(data)

        # keep only needed columns to avoid unnamed metadata
        keep_cols <- intersect(
          c("YYYYMMDD", "YEAR", "MM", power_vars),
          names(data)
        )
        result <- dplyr::bind_cols(data[keep_cols], row[admin_cols])
        result$elevation_m <- elevation_m
        result
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
#' For each polygon, a single representative point (centroid) is used to
#' download data. Returned values correspond to NASA POWER grid-cell averages
#' (approximately 0.5 degree resolution) and should be interpreted as regional
#' conditions rather than point measurements.
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
#'   Drop a code to exclude that variable from the outputs; add a code to opt
#'   in. Codes recognised by the rename map (clean column name in parentheses):
#'   \code{PRECTOTCORR} (rainfall_mm), \code{T2M} (air_temperature_c),
#'   \code{T2M_MAX}/\code{T2M_MIN} (max/min_air_temperature_c), \code{TS}
#'   (land_temperature_c), \code{TS_MAX}/\code{TS_MIN}
#'   (max/min_land_temperature_c), \code{RH2M} (humidity_pct),
#'   \code{T2MDEW} (dewpoint_c), \code{T2MWET} (wet_bulb_temperature_c),
#'   \code{QV2M} (specific_humidity_gkg), \code{PS} (surface_pressure_kpa),
#'   \code{WS2M}/\code{WS2M_MAX}/\code{WS2M_MIN} (wind_speed_ms),
#'   \code{ALLSKY_SFC_SW_DWN} (solar_radiation_kwhm2), \code{CLOUD_AMT}
#'   (cloud_amount_pct), \code{GWETTOP} (surface_soil_wetness),
#'   \code{GWETROOT} (root_soil_wetness), \code{GWETPROF}
#'   (profile_soil_wetness). Unrecognised codes are downloaded but dropped
#'   from the cleaned daily/monthly outputs.
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
#'     \item{daily}{Tibble of daily climate data with admin columns plus an
#'       \code{elevation_m} column (MERRA-2 grid-cell elevation, static per
#'       polygon, parsed from the POWER response metadata).}
#'     \item{monthly}{Tibble of monthly aggregated climate data. For each
#'       requested variable: \code{mean_}, \code{median_}, \code{min_},
#'       \code{max_} versions; rainfall additionally gets \code{total_}.
#'       \code{elevation_m} is carried through as a static column.}
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
      "RH2M",
      "T2MDEW"
    ),
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
  # extract centroids
  # -------------------------------------------------------------------------

  cli::cli_h2("Extracting polygon centroids")

  cli::cli_alert_info(
    "NASA POWER returns grid-cell averages (~0.5 deg resolution)"
  )

  # extract centroids and convert to WGS84
  # suppress the "st_centroid assumes attributes are constant" warning:
  # we only use the geometry for coordinate extraction below
  centroids <- adm_sf |>
    (\(x) base::suppressWarnings(sf::st_centroid(x)))() |>
    sf::st_transform(4326)

  coords <- sf::st_coordinates(centroids)

  admin_coords <- sf::st_drop_geometry(adm_sf)
  admin_coords$lon <- coords[, 1]
  admin_coords$lat <- coords[, 2]

  n_polygons <- nrow(adm_sf)

  cli::cli_alert_success(
    "Extracted centroids for {n_polygons} polygons"
  )

  # -------------------------------------------------------------------------
  # download
  # -------------------------------------------------------------------------

  cli::cli_h2("Downloading POWER data")

  n_units <- nrow(admin_coords)
  success_count <- 0
  fail_count <- 0

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
    }

    cli::cli_progress_update()
  }

  cli::cli_progress_done()

  # report download results
  if (success_count > 0) {
    cli::cli_alert_success(
      "Downloaded data for {success_count}/{n_units} admin units"
    )
  }

  if (fail_count > 0) {
    cli::cli_alert_warning(
      "Failed to download {fail_count} admin units"
    )
  }

  power_data_agg <- dplyr::bind_rows(results_list)

  if (nrow(power_data_agg) == 0) {
    cli::cli_abort("No POWER data downloaded")
  }

  # -------------------------------------------------------------------------
  # daily processing
  # -------------------------------------------------------------------------

  cli::cli_h2("Processing daily data")

  # rename map: clean column name <- POWER variable code. Anything the user
  # excluded from `power_vars` is simply absent from `power_data_agg`, so
  # `dplyr::any_of()` quietly drops it. To extend coverage, add a row here.
  power_rename <- c(
    rainfall_mm = "PRECTOTCORR",
    max_air_temperature_c = "T2M_MAX",
    min_air_temperature_c = "T2M_MIN",
    air_temperature_c = "T2M",
    max_land_temperature_c = "TS_MAX",
    min_land_temperature_c = "TS_MIN",
    land_temperature_c = "TS",
    humidity_pct = "RH2M",
    dewpoint_c = "T2MDEW",
    wet_bulb_temperature_c = "T2MWET",
    specific_humidity_gkg = "QV2M",
    surface_pressure_kpa = "PS",
    wind_speed_ms = "WS2M",
    max_wind_speed_ms = "WS2M_MAX",
    min_wind_speed_ms = "WS2M_MIN",
    solar_radiation_kwhm2 = "ALLSKY_SFC_SW_DWN",
    cloud_amount_pct = "CLOUD_AMT",
    surface_soil_wetness = "GWETTOP",
    root_soil_wetness = "GWETROOT",
    profile_soil_wetness = "GWETPROF"
  )

  power_daily <- power_data_agg |>
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
      adm0, adm1, adm2,
      location, date, year, month, yearmon,
      dplyr::any_of(power_rename),
      dplyr::any_of("elevation_m")
    )

  cli::cli_alert_success("Processed {nrow(power_daily)} daily records")
  cli::cli_alert_info(
    "Date coverage: {min(power_daily$date)} to {max(power_daily$date)}"
  )

  # -------------------------------------------------------------------------
  # monthly aggregation
  # -------------------------------------------------------------------------

  cli::cli_h2("Aggregating to monthly")

  # variables to aggregate. rainfall additionally gets a monthly total; every
  # variable in the rename map gets mean/median/min/max. elevation is static
  # per grid cell, so we just carry the first value through.
  agg_cols <- as.character(names(power_rename))
  rain_col <- "rainfall_mm"

  power_monthly <- power_daily |>
    dplyr::group_by(adm0, adm1, adm2, location, year, month, yearmon) |>
    dplyr::summarise(
      dplyr::across(
        dplyr::any_of(rain_col),
        list(total = \(x) sum(x, na.rm = TRUE)),
        .names = "{.fn}_{.col}"
      ),
      dplyr::across(
        dplyr::any_of(agg_cols),
        list(
          mean   = \(x) mean(x, na.rm = TRUE),
          median = \(x) stats::median(x, na.rm = TRUE),
          min    = \(x) min(x, na.rm = TRUE),
          max    = \(x) max(x, na.rm = TRUE)
        ),
        .names = "{.fn}_{.col}"
      ),
      dplyr::across(
        dplyr::any_of("elevation_m"),
        list(value = dplyr::first),
        .names = "{.col}"
      ),
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
