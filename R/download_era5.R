#' List Available ERA5 Dataset Options
#'
#' Returns a tibble with supported ERA5 datasets available for download
#' from the Copernicus Climate Data Store (CDS). Each entry includes
#' the dataset code, descriptive label, CDS identifier, and common
#' variables.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{dataset}{Machine-readable dataset code (e.g.,
#'       `"daily_single_levels"`)}
#'     \item{label}{Descriptive label for user display}
#'     \item{cds_id}{CDS dataset identifier}
#'     \item{product_type}{Product type for CDS request (if applicable)}
#'     \item{common_vars}{Example variable names for this dataset}
#'     \item{description}{Brief description of the dataset type}
#'   }
#'
#' @examples
#' era5_options()
#'
#' @export
era5_options <- function() {
  tibble::tibble(
    dataset = c(
      "daily_single_levels",
      "monthly_single_levels",
      "hourly_single_levels",
      "hourly_pressure_levels"
    ),
    label = c(
      "Daily Statistics - Single Levels",
      "Monthly Means - Single Levels",
      "Hourly Data - Single Levels",
      "Hourly Data - Pressure Levels"
    ),
    cds_id = c(
      "derived-era5-single-levels-daily-statistics",
      "reanalysis-era5-single-levels-monthly-means",
      "reanalysis-era5-single-levels",
      "reanalysis-era5-pressure-levels"
    ),
    product_type = c(
      NA_character_,
      "monthly_averaged_reanalysis",
      "reanalysis",
      "reanalysis"
    ),
    common_vars = c(
      paste(
        "2m_temperature, total_precipitation,",
        "10m_u_component_of_wind, 10m_v_component_of_wind,",
        "surface_pressure, 2m_dewpoint_temperature"
      ),
      paste(
        "2m_temperature, total_precipitation,",
        "10m_u_component_of_wind, 10m_v_component_of_wind,",
        "surface_pressure, mean_sea_level_pressure"
      ),
      paste(
        "2m_temperature, total_precipitation,",
        "10m_u_component_of_wind, 10m_v_component_of_wind,",
        "surface_pressure, surface_solar_radiation_downwards,",
        "surface_thermal_radiation_downwards, total_cloud_cover,",
        "soil_temperature_level_1, volumetric_soil_water_layer_1"
      ),
      paste(
        "temperature, geopotential, u_component_of_wind,",
        "v_component_of_wind, specific_humidity, relative_humidity"
      )
    ),
    description = c(
      "Pre-aggregated daily statistics (mean, min, max, sum)",
      "Pre-aggregated monthly means",
      "Hourly atmospheric, land-surface and sea-state params",
      "Hourly upper air fields on pressure levels (1000-1 hPa)"
    )
  )
}

#' List Available ERA5 Files for a Dataset
#'
#' Checks what ERA5 data is available from CDS for a given dataset
#' and time period. This is informational only - actual availability
#' depends on CDS server status.
#'
#' @param dataset_code Character. One of the dataset codes from
#'   `era5_options()`.
#' @param years Integer vector. Years to check (e.g., `2020:2023`).
#' @param months Integer vector. Months to check (1-12). Default is
#'   all months.
#' @param variables Character vector. CDS variable names to display
#'   info for.
#'
#' @return A tibble with dataset information.
#'
#' @examples
#' \dontrun{
#' check_era5_available("daily_single_levels", years = 2020, months = 1:3,
#'                      variables = c("2m_temperature"))
#' }
#'
#' @export
check_era5_available <- function(
  dataset_code = "daily_single_levels",
  years = 2020,
  months = 1:12,
  variables = NULL
) {
  # Validate dataset
  opts <- era5_options()
  if (!dataset_code %in% opts$dataset) {
    cli::cli_abort(
      "Invalid dataset. Use `era5_options()` to see available options."
    )
  }

  sel <- opts[opts$dataset == dataset_code, ]

  # Validate years and months
  years <- suppressWarnings(as.integer(years))
  months <- suppressWarnings(as.integer(months))
  if (any(is.na(years))) {
    cli::cli_abort("years must be numeric YYYY.")
  }
  if (any(is.na(months)) || any(months < 1 | months > 12)) {
    cli::cli_abort("months must be in 1..12.")
  }

  cli::cli_h2(glue::glue("ERA5 Dataset Info: {sel$label}"))
  cli::cli_alert_info(glue::glue("CDS ID: {sel$cds_id}"))
  cli::cli_alert_info(glue::glue("Common variables: {sel$common_vars}"))

  # Create summary
  date_range <- paste(
    format(as.Date(paste(min(years), min(months), "01", sep = "-")), "%b %Y"),
    "-",
    format(as.Date(paste(max(years), max(months), "01", sep = "-")), "%b %Y")
  )

  cli::cli_alert_success(glue::glue(
    "Requested period: {date_range}"
  ))

  if (!is.null(variables)) {
    cli::cli_alert_info(glue::glue(
      "Variables: {paste(variables, collapse = ', ')}"
    ))
  }

  return(tibble::tibble(
    dataset = dataset_code,
    cds_id = sel$cds_id,
    years = list(years),
    months = list(months),
    variables = list(variables)
  ))
}

#' Download ERA5 Data from Copernicus CDS
#'
#' Downloads ERA5 data from the Copernicus Climate Data Store (CDS)
#' using the ecmwfr package. Supports daily and monthly pre-aggregated
#' products. Requires CDS API credentials.
#'
#' Use [era5_options()] to view all available datasets and their
#' metadata.
#'
#' @param dataset Character. One of the dataset codes listed in
#'   [era5_options()].
#' @param cds_key Character. CDS API credentials. Defaults to
#'   `Sys.getenv("ERA5_API_KEY")`. For new CDS (ecmwfr >= 2.0.0),
#'   use just the Personal Access Token (PAT). For old CDS, use
#'   "uid:apikey" format. Get from
#'   https://cds.climate.copernicus.eu/profile
#' @param years Integer vector. Years to download (e.g., `2020:2022`).
#' @param months Integer vector. Months to download (1-12).
#' @param variables Character vector. CDS variable names (e.g.,
#'   `c("2m_temperature", "total_precipitation")`).
#' @param daily_stat Character. For daily dataset: `"mean"`,
#'   `"maximum"`, `"minimum"`, or `"sum"`. Note: The daily statistics
#'   dataset can be unreliable. Consider using hourly data instead.
#' @param daily_hfreq Character. For daily dataset: `"1_hourly"`,
#'   `"3_hourly"`, or `"6_hourly"`.
#' @param daily_tz Character. Time zone for daily data (e.g.,
#'   `"UTC+00:00"`).
#' @param hours Integer vector. Hours to download for hourly datasets
#'   (0-23). Default is all hours.
#' @param pressure_levels Integer vector. Pressure levels in hPa for
#'   pressure level data (e.g., `c(1000, 850, 500)`).
#' @param bbox Numeric vector. Bounding box as `c(xmin, ymin, xmax,
#'   ymax)` in lon/lat.
#' @param out_dir Character. Directory path where files will be
#'   saved.
#' @param overwrite Logical. If `TRUE`, overwrite existing files.
#' @param aggregated_dir Character. Optional directory to save
#'   aggregated data files. If NULL (default), no aggregation is
#'   performed. If specified, downloads are read and aggregated by
#'   variable, with filenames like
#'   "era5_precip_Jan2020-Dec2024_v20250919.qs2"
#' @param aggregated_format Character. Format for aggregated files:
#'   "qs2" (default), "parquet", "feather", "rds", "csv"
#'
#' @return A tibble with download information including file paths
#'   and status.
#'
#' @examples
#' # View available datasets
#' era5_options()
#'
#' \dontrun{
#' # Download monthly temperature data (uses ERA5_API_KEY env var)
#' download_era5(
#'   dataset = "monthly_single_levels",
#'   years = 2020,
#'   months = 1:3,
#'   variables = c("2m_temperature"),
#'   bbox = c(29.0, -4.5, 30.9, -2.3),
#'   out_dir = "era5_data"
#' )
#'
#' # Download daily precipitation totals
#' download_era5(
#'   dataset = "daily_single_levels",
#'   years = 2020,
#'   months = 1,
#'   variables = c("total_precipitation"),
#'   daily_stat = "sum",
#'   daily_hfreq = "1_hourly",
#'   daily_tz = "UTC+00:00",
#'   bbox = c(29.0, -4.5, 30.9, -2.3),
#'   out_dir = "era5_data"
#' )
#'
#' # Download hourly temperature and wind data
#' download_era5(
#'   dataset = "hourly_single_levels",
#'   years = 2020,
#'   months = 1,
#'   variables = c(
#'     "2m_temperature",
#'     "10m_u_component_of_wind",
#'     "10m_v_component_of_wind"
#'   ),
#'   hours = c(0, 6, 12, 18),  # Every 6 hours
#'   bbox = c(29.0, -4.5, 30.9, -2.3),
#'   out_dir = "era5_data"
#' )
#'
#' # Download pressure level data
#' download_era5(
#'   dataset = "hourly_pressure_levels",
#'   years = 2020,
#'   months = 1,
#'   variables = c("temperature", "relative_humidity"),
#'   pressure_levels = c(1000, 850, 500),  # Surface, 850hPa, 500hPa
#'   hours = c(0, 12),  # Twice daily
#'   bbox = c(29.0, -4.5, 30.9, -2.3),
#'   out_dir = "era5_data"
#' )
#' }
#'
#' @export
download_era5 <- function(
  dataset,
  cds_key = Sys.getenv("ERA5_API_KEY"),
  years,
  months,
  variables,
  daily_stat = c("mean", "maximum", "minimum", "sum"),
  daily_hfreq = c("1_hourly", "3_hourly", "6_hourly"),
  daily_tz = "UTC+00:00",
  hours = 0:23,
  pressure_levels = NULL,
  bbox = NULL,
  out_dir = "era5_data",
  overwrite = FALSE,
  aggregated_dir = NULL,
  aggregated_format = "qs2"
) {
  # Check required packages
  if (!requireNamespace("ecmwfr", quietly = TRUE)) {
    cli::cli_abort(c(
      "Package {.pkg ecmwfr} is required for ERA5 downloads.",
      "i" = "Install: {.code install.packages('ecmwfr')}"
    ))
  }

  # Validate arguments
  daily_stat <- match.arg(daily_stat)
  daily_hfreq <- match.arg(daily_hfreq)
  aggregated_format <- match.arg(
    aggregated_format,
    c("qs2", "parquet", "feather", "rds", "csv")
  )

  # Check for API key and prompt if missing
  if (is.null(cds_key) || cds_key == "") {
    if (interactive()) {
      cli::cli_alert_warning(
        "ERA5_API_KEY environment variable not set."
      )
      cli::cli_alert_info(
        "Get key: {.url https://cds.climate.copernicus.eu/profile}"
      )
      cds_key <- readline(prompt = "Enter your CDS API key: ")

      if (is.null(cds_key) || cds_key == "") {
        cli::cli_abort("API key required to download ERA5 data.")
      }

      # Ask if user wants to save it
      save_prompt <- "Save key to .Renviron for future use? (y/n): "
      save_key <- readline(prompt = save_prompt)
      if (tolower(trimws(save_key)) == "y") {
        renviron_path <- file.path(Sys.getenv("HOME"), ".Renviron")

        # Read existing .Renviron or create new
        if (file.exists(renviron_path)) {
          renviron_lines <- readLines(renviron_path)
          # Remove any existing ERA5_API_KEY line
          renviron_lines <- renviron_lines[
            !grepl("^ERA5_API_KEY=", renviron_lines)
          ]
        } else {
          renviron_lines <- character(0)
        }

        # Add new key
        renviron_lines <- c(
          renviron_lines,
          paste0("ERA5_API_KEY=", cds_key)
        )
        writeLines(renviron_lines, renviron_path)

        cli::cli_alert_success(
          "API key saved to {.file {renviron_path}}"
        )
        cli::cli_alert_info(
          "Restart R or run: {.code Sys.setenv(ERA5_API_KEY='{cds_key}')}"
        )
      }
    } else {
      cli::cli_abort(c(
        "CDS API key not found or invalid.",
        "i" = "Set ERA5_API_KEY env var or provide cds_key arg.",
        "i" = "Get key: https://cds.climate.copernicus.eu/profile"
      ))
    }
  }

  # Validate dataset
  opts <- era5_options()
  if (!dataset %in% opts$dataset) {
    cli::cli_abort(
      "Invalid dataset. Use `era5_options()` to see available options."
    )
  }

  sel <- opts[opts$dataset == dataset, ]

  # Parse CDS credentials
  # New CDS (ecmwfr >= 2.0.0): just PAT token
  # Old CDS (ecmwfr < 2.0.0): uid:apikey format
  if (!is.character(cds_key) || length(cds_key) != 1 || cds_key == "") {
    cli::cli_abort(c(
      "CDS API key not found or invalid.",
      "i" = "Set ERA5_API_KEY env var or provide cds_key arg.",
      "i" = "Get key: https://cds.climate.copernicus.eu/profile"
    ))
  }

  # Check if it contains a colon (old format)
  if (grepl(":", cds_key, fixed = TRUE)) {
    creds <- strsplit(cds_key, ":", fixed = TRUE)[[1]]
    if (length(creds) != 2 || any(creds == "")) {
      cli::cli_abort(
        "cds_key in 'uid:apikey' format must have both parts."
      )
    }
    uid <- creds[1]
    key <- creds[2]
  } else {
    # New CDS format: just the PAT token
    uid <- "ecmwfr" # default user for new CDS
    key <- cds_key
  }

  # Validate years and months
  years <- suppressWarnings(as.integer(years))
  months <- suppressWarnings(as.integer(months))
  if (any(is.na(years))) {
    cli::cli_abort("years must be numeric YYYY.")
  }
  if (any(is.na(months)) || any(months < 1 | months > 12)) {
    cli::cli_abort("months must be in 1..12.")
  }

  # Validate variables
  if (!is.character(variables) || length(variables) < 1) {
    cli::cli_abort("variables must be a non-empty character vector.")
  }

  # Validate bbox if provided
  if (!is.null(bbox)) {
    if (!is.numeric(bbox) || length(bbox) != 4) {
      cli::cli_abort("bbox must be c(xmin, ymin, xmax, ymax).")
    }
    if (bbox[1] >= bbox[3]) {
      cli::cli_abort("bbox xmin must be less than xmax.")
    }
    if (bbox[2] >= bbox[4]) {
      cli::cli_abort("bbox ymin must be less than ymax.")
    }
  }

  # Create output directory
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
  }

  # Register CDS credentials (ecmwfr 2.0+)
  # Suppress R6 cloneable warnings from ecmwfr package
  suppressMessages(suppressWarnings(
    ecmwfr::wf_set_key(
      user = uid,
      key = key
    )
  ))

  # CDS area format: [N, W, S, E]
  area_vec <- if (!is.null(bbox)) {
    c(bbox[4], bbox[1], bbox[2], bbox[3])
  } else {
    NULL
  }

  # Build requests for each year-month combination
  cli::cli_h1(glue::glue("Downloading ERA5: {sel$label}"))
  combos <- tidyr::expand_grid(year = years, month = months)
  n_files <- nrow(combos)

  cli::cli_alert_info(glue::glue(
    "Requesting {n_files} file{ifelse(n_files == 1, '', 's')}"
  ))

  # Shorten variable names for cleaner filenames (do this once)
  var_short <- purrr::map_chr(variables, function(v) {
    dplyr::case_when(
      grepl("temperature", v, ignore.case = TRUE) ~
        gsub("temperature", "temp", v, ignore.case = TRUE),
      grepl("precipitation", v, ignore.case = TRUE) ~
        gsub("precipitation", "precip", v, ignore.case = TRUE),
      grepl("component_of_wind", v) ~
        gsub("_component_of_wind", "", v),
      TRUE ~ v
    )
  })
  var_string <- paste(
    gsub("[^a-zA-Z0-9]", "_", var_short),
    collapse = "-"
  )

  # Extract frequency from dataset (e.g., "monthly", "daily", "hourly")
  frequency <- if (grepl("monthly", dataset)) {
    "monthly"
  } else if (grepl("daily", dataset)) {
    "daily"
  } else if (grepl("hourly", dataset)) {
    "hourly"
  } else {
    dataset
  }

  # Format variables with color for display
  var_display <- paste(cli::col_cyan(variables), collapse = ", ")

  # Build list of all requests and track which already exist
  batch_requests <- list()
  existing_files <- tibble::tibble(
    year = integer(),
    month = integer(),
    dataset = character(),
    target = character(),
    status = character()
  )

  for (i in 1:n_files) {
    yr <- sprintf("%04d", combos$year[i])
    mo <- sprintf("%02d", combos$month[i])

    stem <- glue::glue("era5_{var_string}_{frequency}_{yr}{mo}")
    target <- file.path(out_dir, glue::glue("{stem}.nc"))

    # Check if file exists
    if (file.exists(target) && !overwrite) {
      month_name <- format(
        as.Date(paste(yr, mo, "01", sep = "-")),
        "%b %Y"
      )
      cli::cli_alert_info(
        "Skipping {.strong {month_name}}: {var_display} (exists)"
      )

      existing_files <- dplyr::bind_rows(
        existing_files,
        tibble::tibble(
          year = as.integer(yr),
          month = as.integer(mo),
          dataset = dataset,
          target = target,
          status = "exists"
        )
      )
      next
    }

    # Build request based on dataset type
    req <- if (dataset == "monthly_single_levels") {
      list(
        dataset_short_name = sel$cds_id,
        product_type = sel$product_type,
        variable = variables,
        year = yr,
        month = mo,
        time = sprintf("%02d:00", 0:23),
        format = "netcdf",
        area = area_vec,
        target = basename(target)
      )
    } else if (dataset == "daily_single_levels") {
      # daily dataset requires statistic/frequency fields in CDS naming
      list(
        dataset_short_name = sel$cds_id,
        product_type = "reanalysis",
        variable = variables,
        year = yr,
        month = mo,
        day = sprintf("%02d", 1:31),  # all days in the month
        time = "00:00",  # daily statistics use 00:00 as reference
        statistic = daily_stat,
        time_zone = base::tolower(base::trimws(daily_tz)),
        frequency = daily_hfreq,
        format = "netcdf",
        area = area_vec,
        target = basename(target)
      )
    } else if (dataset == "hourly_single_levels") {
      # hourly single level dataset
      list(
        dataset_short_name = sel$cds_id,
        product_type = sel$product_type,
        variable = variables,
        year = yr,
        month = mo,
        day = sprintf("%02d", 1:31),
        time = sprintf("%02d:00", hours),
        format = "netcdf",
        area = area_vec,
        target = basename(target)
      )
    } else if (dataset == "hourly_pressure_levels") {
      # hourly pressure level dataset
      if (is.null(pressure_levels)) {
        cli::cli_abort(
          "pressure_levels required for hourly_pressure_levels dataset"
        )
      }
      list(
        dataset_short_name = sel$cds_id,
        product_type = sel$product_type,
        variable = variables,
        pressure_level = as.character(pressure_levels),
        year = yr,
        month = mo,
        day = sprintf("%02d", 1:31),
        time = sprintf("%02d:00", hours),
        format = "netcdf",
        area = area_vec,
        target = basename(target)
      )
    }

    # Remove NULL elements (like area if not specified)
    req <- req[!sapply(req, is.null)]

    # Store request with metadata
    batch_requests[[length(batch_requests) + 1]] <- list(
      request = req,
      year = as.integer(yr),
      month = as.integer(mo),
      target = target
    )
  }

  # Submit batch requests if any
  downloaded_files <- tibble::tibble(
    year = integer(),
    month = integer(),
    dataset = character(),
    target = character(),
    status = character()
  )

  if (length(batch_requests) > 0) {
    n_req <- length(batch_requests)
    req_plural <- ifelse(n_req == 1, '', 's')
    cli::cli_alert_info(
      "Submitting {n_req} request{req_plural} in parallel (batch mode)"
    )

    # Determine number of workers (max 20 as per CDS limits)
    n_workers <- min(n_req, 20)
    worker_plural <- ifelse(n_workers == 1, '', 's')
    cli::cli_alert_info(
      "Using {n_workers} parallel worker{worker_plural}"
    )

    # Extract just the requests for wf_request_batch
    requests_only <- purrr::map(batch_requests, ~.x$request)

    # Submit batch request with error handling
    batch_result <- tryCatch(
      {
        suppressMessages(suppressWarnings(
          ecmwfr::wf_request_batch(
            request_list = requests_only,
            user = uid,
            path = out_dir,
            workers = n_workers
          )
        ))
      },
      error = function(e) {
        cli::cli_alert_danger("Batch download failed: {e$message}")
        NULL
      }
    )

    # Process results
    for (i in seq_along(batch_requests)) {
      req_info <- batch_requests[[i]]
      month_name <- format(
        as.Date(paste(
          req_info$year,
          sprintf("%02d", req_info$month),
          "01",
          sep = "-"
        )),
        "%b %Y"
      )

      n_batch <- length(batch_requests)
      if (file.exists(req_info$target)) {
        cli::cli_alert_success(
          "[{i}/{n_batch}] Downloaded {.strong {month_name}}: {var_display}"
        )
        status <- "downloaded"
      } else {
        cli::cli_alert_danger(
          "[{i}/{n_batch}] Failed {.strong {month_name}}: {var_display}"
        )
        status <- "failed"
      }

      downloaded_files <- dplyr::bind_rows(
        downloaded_files,
        tibble::tibble(
          year = req_info$year,
          month = req_info$month,
          dataset = dataset,
          target = req_info$target,
          status = status
        )
      )
    }
  }

  # Combine results
  results <- dplyr::bind_rows(existing_files, downloaded_files)

  cli::cli_text("")
  cli::cli_alert_success("All ERA5 files processed")

  # Aggregate data if requested
  if (!is.null(aggregated_dir)) {
    cli::cli_h1("Aggregating Downloaded Data")

    # Create aggregated directory if needed
    if (!dir.exists(aggregated_dir)) {
      dir.create(aggregated_dir, recursive = TRUE)
    }

    # Get all successfully downloaded files
    downloaded_files <- results |>
      dplyr::filter(status %in% c("downloaded", "exists")) |>
      dplyr::pull(target)

    if (length(downloaded_files) == 0) {
      cli::cli_alert_warning("No files to aggregate")
      return(results)
    }

    # Create date range string from download results
    downloaded_results <- results |>
      dplyr::filter(status %in% c("downloaded", "exists"))

    min_year <- min(downloaded_results$year)
    max_year <- max(downloaded_results$year)
    min_month <- min(
      downloaded_results$month[downloaded_results$year == min_year]
    )
    max_month <- max(
      downloaded_results$month[downloaded_results$year == max_year]
    )

    min_date <- format(
      as.Date(paste(min_year, min_month, "01", sep = "-")),
      "%b%Y"
    )
    max_date <- format(
      as.Date(paste(max_year, max_month, "01", sep = "-")),
      "%b%Y"
    )
    version <- format(Sys.Date(), "%Y%m%d")

    # Read all files
    n_files <- length(downloaded_files)
    file_plural <- ifelse(n_files == 1, '', 's')
    cli::cli_alert_info(
      "Reading {n_files} NetCDF file{file_plural}"
    )
    cli::cli_alert_info("Date range: {min_date} to {max_date}")

    all_data <- read_era5(
      nc_files = downloaded_files,
      variables = variables,
      bbox = bbox,
      convert_units = TRUE
    )

    # Split by variable and save
    for (var in unique(all_data$variable)) {
      var_data <- all_data |> dplyr::filter(variable == var)

      # Shorten variable name for filename
      var_short <- dplyr::case_when(
        grepl("temperature", var, ignore.case = TRUE) ~ "temp",
        grepl("precipitation", var, ignore.case = TRUE) ~ "precip",
        grepl("wind", var, ignore.case = TRUE) ~
          gsub("_component_of_wind", "", var),
        TRUE ~ var
      )
      var_short <- gsub("[^a-zA-Z0-9]", "_", var_short)

      # Create filename
      filename <- glue::glue(
        "era5_{var_short}_{min_date}-{max_date}_v{version}",
        ".{aggregated_format}"
      )
      filepath <- file.path(aggregated_dir, filename)

      # Save using sntutils::write
      cli::cli_alert_info("Saving {var}: {filename}")
      sntutils::write(var_data, filepath)
      cli::cli_alert_success("Saved: {filepath}")
    }

    cli::cli_alert_success(
      "Aggregation complete! Files saved to {aggregated_dir}"
    )
  }

}

#' Read and Process ERA5 NetCDF Files to Tidy Data
#'
#' Reads ERA5 NetCDF files and converts them to a tidy tibble format with
#' automatic country detection from bounding box coordinates.
#'
#' @param nc_files Character vector. Paths to ERA5 NetCDF files to read.
#' @param variables Character vector. Variable names to extract. If
#'   NULL, extracts all variables.
#' @param bbox Numeric vector. Bounding box as `c(xmin, ymin, xmax,
#'   ymax)` for country detection.
#' @param country Character. Optional country name to use instead of
#'   auto-detection.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{country}{Country name (detected or provided)}
#'     \item{date}{Date/datetime of observation}
#'     \item{year}{Year}
#'     \item{month}{Month}
#'     \item{variable}{Climate variable name}
#'     \item{value}{Variable value}
#'     \item{lon}{Longitude}
#'     \item{lat}{Latitude}
#'     \item{units}{Variable units}
#'   }
#'
#' @examples
#' \dontrun{
#' # Read downloaded ERA5 files
#' files <- list.files("era5_data", pattern = "\\.nc$", full.names = TRUE)
#'
#' # Extract data with auto country detection
#' data <- read_era5(
#'   nc_files = files,
#'   variables = c("2m_temperature", "total_precipitation"),
#'   bbox = c(29.0, -4.5, 30.9, -2.3)
#' )
#'
#' # Or specify country manually
#' data <- read_era5(
#'   nc_files = files,
#'   country = "Rwanda"
#' )
#' }
#'
#' @export
read_era5 <- function(
  nc_files,
  variables = NULL,
  bbox = NULL,
  country = NULL,
  convert_units = TRUE
) {
  # Check required packages
  if (!requireNamespace("ncdf4", quietly = TRUE)) {
    cli::cli_abort(c(
      "Package {.pkg ncdf4} is required for reading NetCDF files.",
      "i" = "Install with: {.code install.packages('ncdf4')}"
    ))
  }

  # Check if files exist
  if (!all(file.exists(nc_files))) {
    missing <- nc_files[!file.exists(nc_files)]
    cli::cli_abort("Files not found: {paste(missing, collapse = ', ')}")
  }

  # Detect country from bbox if not provided
  if (is.null(country) && !is.null(bbox)) {
    country <- detect_country_from_bbox(bbox)
  }

  if (is.null(country)) {
    country <- "Unknown"
  }

  cli::cli_h1("Reading ERA5 NetCDF Files")
  cli::cli_alert_info("Country: {country}")
  n_nc <- length(nc_files)
  nc_plural <- ifelse(n_nc == 1, '', 's')
  cli::cli_alert_info("Processing {n_nc} file{nc_plural}")

  # Process each file
  results <- purrr::map_dfr(nc_files, function(nc_file) {
    cli::cli_alert_info("Reading {basename(nc_file)}")

    # Open NetCDF file
    nc <- ncdf4::nc_open(nc_file)
    on.exit(ncdf4::nc_close(nc), add = TRUE)

    # Get available variables (exclude dimensions)
    all_vars <- names(nc$var)

    # Get variable mapping
    var_mapping <- get_era5_variable_mapping()

    # Filter to requested variables if specified
    if (!is.null(variables)) {
      # Map requested CDS names to NetCDF names
      nc_names <- var_mapping[variables]
      nc_names <- nc_names[!is.na(nc_names)]

      # Also allow direct NetCDF names
      vars_to_extract <- union(
        intersect(all_vars, nc_names),
        intersect(all_vars, variables)
      )

      # Create reverse mapping for display names
      var_name_lookup <- c(
        setNames(names(nc_names), nc_names),
        setNames(variables, variables)
      )
    } else {
      vars_to_extract <- all_vars
      # Create reverse mapping - prefer long names
      var_name_lookup <- c(
        setNames(names(var_mapping), var_mapping),
        setNames(all_vars, all_vars)
      )
    }

    if (length(vars_to_extract) == 0) {
      cli::cli_alert_warning("No matching variables in {basename(nc_file)}")
      return(NULL)
    }

    # Extract dimensions
    lon <- ncdf4::ncvar_get(nc, "longitude")
    lat <- ncdf4::ncvar_get(nc, "latitude")

    # Get time variable
    time_var <- if ("time" %in% names(nc$dim)) {
      ncdf4::ncvar_get(nc, "time")
    } else if ("valid_time" %in% names(nc$dim)) {
      ncdf4::ncvar_get(nc, "valid_time")
    } else {
      NULL
    }

    # Get time information from filename if time dimension exists
    pattern_info <- detect_time_pattern(nc_file)
    time_components <- extract_time_components(nc_file, pattern_info)

    # Create dates based on pattern
    if (pattern_info$pattern == "monthly") {
      dates <- seq(
        from = as.POSIXct(
          paste0(
            time_components$year, "-",
            sprintf("%02d", time_components$month), "-01"
          ),
          tz = "UTC"
        ),
        by = "hour",
        length.out = if (!is.null(time_var)) length(time_var) else 1
      )
    } else if (pattern_info$pattern == "daily") {
      dates <- seq(
        from = as.POSIXct(time_components$date, tz = "UTC"),
        by = "hour",
        length.out = if (!is.null(time_var)) length(time_var) else 1
      )
    } else {
      # Yearly
      dates <- seq(
        from = as.POSIXct(paste0(time_components$year, "-01-01"), tz = "UTC"),
        by = "hour",
        length.out = if (!is.null(time_var)) length(time_var) else 1
      )
    }

    # Extract each variable
    var_data <- purrr::map_dfr(vars_to_extract, function(nc_var_name) {
      var_obj <- nc$var[[nc_var_name]]
      values <- ncdf4::ncvar_get(nc, nc_var_name)
      units <- var_obj$units

      # Get display name (prefer long CDS name)
      display_name <- if (!is.null(var_name_lookup[[nc_var_name]])) {
        var_name_lookup[[nc_var_name]]
      } else {
        nc_var_name
      }

      # Convert array to long format
      if (length(dim(values)) == 3) {
        # 3D: lon x lat x time
        expanded <- tidyr::expand_grid(
          lon_idx = seq_along(lon),
          lat_idx = seq_along(lat),
          time_idx = seq_along(dates)
        ) |>
          dplyr::mutate(
            lon = lon[lon_idx],
            lat = lat[lat_idx],
            date = dates[time_idx],
            value = purrr::pmap_dbl(
              list(lon_idx, lat_idx, time_idx),
              ~values[..1, ..2, ..3]
            )
          )
      } else if (length(dim(values)) == 2) {
        # 2D: lon x lat
        expanded <- tidyr::expand_grid(
          lon_idx = seq_along(lon),
          lat_idx = seq_along(lat)
        ) |>
          dplyr::mutate(
            lon = lon[lon_idx],
            lat = lat[lat_idx],
            date = dates[1],
            value = purrr::pmap_dbl(
              list(lon_idx, lat_idx),
              ~values[..1, ..2]
            )
          )
      } else {
        cli::cli_abort("Unexpected dimensions for variable {nc_var_name}")
      }

      expanded |>
        dplyr::select(lon, lat, date, value) |>
        dplyr::mutate(
          variable = display_name,
          units = units
        )
    })

    var_data
  })

  # Add metadata
  if (nrow(results) > 0) {
    results <- results |>
      dplyr::mutate(
        country = country,
        year = lubridate::year(date),
        month = lubridate::month(date),
        .before = 1
      ) |>
      dplyr::select(
        country, date, year, month, variable, value, lon, lat, units
      )

    # Convert units if requested
    if (convert_units) {
      results <- results |>
        dplyr::mutate(
          value = dplyr::case_when(
            # Temperature: Kelvin to Celsius
            units == "K" &
              grepl("temperature", variable, ignore.case = TRUE) ~
              value - 273.15,
            # Precipitation: meters to mm
            units == "m" &
              grepl("precipitation|rain", variable, ignore.case = TRUE) ~
              value * 1000,
            # Default: no conversion
            TRUE ~ value
          ),
          units = dplyr::case_when(
            # Update units for converted values
            units == "K" &
              grepl("temperature", variable, ignore.case = TRUE) ~
              "°C",
            units == "m" &
              grepl("precipitation|rain", variable, ignore.case = TRUE) ~
              "mm",
            # Keep original units
            TRUE ~ units
          )
        )
    }
  }

  cli::cli_alert_success("Extracted {nrow(results)} records")
  if (convert_units) {
    cli::cli_alert_info("Units converted: K → °C, m → mm")
  }

  return(results)
}

#' Get ERA5 Variable Name Mapping
#'
#' Returns a mapping between CDS variable names and NetCDF short names.
#'
#' @return A named character vector where names are CDS variable names
#'   and values are NetCDF short names.
#'
#' @keywords internal
#' @noRd
get_era5_variable_mapping <- function() {
  c(
    # Temperature variables
    "2m_temperature" = "t2m",
    "2m_dewpoint_temperature" = "d2m",
    "skin_temperature" = "skt",

    # Precipitation
    "total_precipitation" = "tp",
    "convective_precipitation" = "cp",

    # Wind components
    "10m_u_component_of_wind" = "u10",
    "10m_v_component_of_wind" = "v10",
    "u_component_of_wind" = "u",
    "v_component_of_wind" = "v",

    # Pressure
    "surface_pressure" = "sp",
    "mean_sea_level_pressure" = "msl",

    # Radiation
    "surface_solar_radiation_downwards" = "ssrd",
    "surface_thermal_radiation_downwards" = "strd",
    "surface_net_solar_radiation" = "ssr",
    "surface_net_thermal_radiation" = "str",
    "top_net_solar_radiation" = "tsr",
    "top_net_thermal_radiation" = "ttr",

    # Cloud cover
    "total_cloud_cover" = "tcc",
    "low_cloud_cover" = "lcc",
    "medium_cloud_cover" = "mcc",
    "high_cloud_cover" = "hcc",

    # Soil variables
    "soil_temperature_level_1" = "stl1",
    "soil_temperature_level_2" = "stl2",
    "soil_temperature_level_3" = "stl3",
    "soil_temperature_level_4" = "stl4",
    "volumetric_soil_water_layer_1" = "swvl1",
    "volumetric_soil_water_layer_2" = "swvl2",
    "volumetric_soil_water_layer_3" = "swvl3",
    "volumetric_soil_water_layer_4" = "swvl4",

    # Humidity
    "relative_humidity" = "r",
    "specific_humidity" = "q",

    # Other atmospheric
    "geopotential" = "z",
    "temperature" = "t",
    "evaporation" = "e",
    "runoff" = "ro",
    "snow_depth" = "sd"
  )
}

#' Detect Country from Bounding Box
#'
#' Uses the rnaturalearth package to detect which country a bounding box
#' primarily overlaps with.
#'
#' @param bbox Numeric vector. Bounding box as `c(xmin, ymin, xmax, ymax)`.
#'
#' @return Character. Country name.
#'
#' @keywords internal
#' @noRd
detect_country_from_bbox <- function(bbox) {
  # Check if required packages are available
  missing_pkgs <- character()

  if (!requireNamespace("rnaturalearth", quietly = TRUE)) {
    missing_pkgs <- c(missing_pkgs, "rnaturalearth")
  }
  if (!requireNamespace("sf", quietly = TRUE)) {
    missing_pkgs <- c(missing_pkgs, "sf")
  }

  if (length(missing_pkgs) > 0) {
    pkgs_str <- paste(shQuote(missing_pkgs), collapse = ', ')
    cli::cli_alert_warning(c(
      "Automatic country detection requires additional packages.",
      "i" = "Install missing: {.code install.packages(c({pkgs_str}))}",
      "i" = "Returning 'Unknown' for country."
    ))
    return("Unknown")
  }

  # Create bbox polygon
  bbox_poly <- sf::st_sfc(
    sf::st_polygon(list(matrix(c(
      bbox[1], bbox[2],
      bbox[3], bbox[2],
      bbox[3], bbox[4],
      bbox[1], bbox[4],
      bbox[1], bbox[2]
    ), ncol = 2, byrow = TRUE))),
    crs = 4326
  )

  # Get world countries
  countries <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

  # Find intersecting countries
  intersects <- sf::st_intersects(bbox_poly, countries, sparse = FALSE)[1, ]

  if (sum(intersects) == 0) {
    return("Unknown")
  }

  # Calculate intersection areas
  intersecting_countries <- countries[intersects, ]
  intersection_areas <- sf::st_area(
    sf::st_intersection(bbox_poly, intersecting_countries)
  )

  # Return country with largest intersection
  country_name <- intersecting_countries$name[which.max(intersection_areas)]

  return(as.character(country_name))
}

#' Get Metadata from ERA5 NetCDF File
#'
#' Extracts comprehensive metadata from an ERA5 NetCDF file including
#' resolution, variables, units, dimensions, and global attributes.
#'
#' @param nc_file Character. Path to ERA5 NetCDF file.
#'
#' @return A list containing:
#'   \describe{
#'     \item{resolution}{Spatial resolution in degrees (lon, lat)}
#'     \item{extent}{Bounding box (lon_min, lon_max, lat_min, lat_max)}
#'     \item{dimensions}{List of dimension information}
#'     \item{variables}{Tibble of variable metadata (name,
#'       long_name, units, dimensions)}
#'     \item{global_attrs}{List of global attributes}
#'   }
#'
#' @examples
#' \dontrun{
#' file <- "era5_data/monthly_single_levels_era5_202001.nc"
#' metadata <- get_era5_metadata(file)
#' print(metadata$resolution)
#' print(metadata$variables)
#' }
#'
#' @export
get_era5_metadata <- function(nc_file) {
  # Check required packages
  if (!requireNamespace("ncdf4", quietly = TRUE)) {
    cli::cli_abort(c(
      "Package {.pkg ncdf4} is required for reading NetCDF metadata.",
      "i" = "Install with: {.code install.packages('ncdf4')}"
    ))
  }

  if (!file.exists(nc_file)) {
    cli::cli_abort("File not found: {nc_file}")
  }

  nc <- ncdf4::nc_open(nc_file)
  on.exit(ncdf4::nc_close(nc), add = TRUE)

  # Get spatial resolution and extent
  lon <- ncdf4::ncvar_get(nc, "longitude")
  lat <- ncdf4::ncvar_get(nc, "latitude")

  lon_res <- if (length(lon) > 1) unique(diff(lon))[1] else NA
  lat_res <- if (length(lat) > 1) abs(unique(diff(lat))[1]) else NA

  resolution <- list(
    lon_degrees = lon_res,
    lat_degrees = lat_res,
    lon_km = round(lon_res * 111.32, 2),  # Approx km at equator
    lat_km = round(lat_res * 111.32, 2)
  )

  extent <- list(
    lon_min = min(lon),
    lon_max = max(lon),
    lat_min = min(lat),
    lat_max = max(lat)
  )

  # Get dimension information
  dimensions <- purrr::map(names(nc$dim), function(dim_name) {
    dim_obj <- nc$dim[[dim_name]]
    list(
      name = dim_name,
      length = dim_obj$len,
      units = dim_obj$units,
      values = if (dim_obj$len <= 100) {
        ncdf4::ncvar_get(nc, dim_name)
      } else {
        c(head(ncdf4::ncvar_get(nc, dim_name), 3), "...",
          tail(ncdf4::ncvar_get(nc, dim_name), 3))
      }
    )
  })
  names(dimensions) <- names(nc$dim)

  # Get variable information
  var_mapping <- get_era5_variable_mapping()
  reverse_mapping <- setNames(names(var_mapping), var_mapping)

  variables <- purrr::map_dfr(names(nc$var), function(var_name) {
    var_obj <- nc$var[[var_name]]

    # Get long name from mapping or use NetCDF long name
    long_name <- if (!is.null(reverse_mapping[[var_name]])) {
      reverse_mapping[[var_name]]
    } else {
      var_obj$longname
    }

    # Get all attributes (suppress 8-byte warnings)
    attrs <- suppressWarnings(ncdf4::ncatt_get(nc, var_name))

    tibble::tibble(
      nc_name = var_name,
      long_name = long_name,
      standard_name = attrs$standard_name %||% NA,
      units = var_obj$units,
      dimensions = paste(names(var_obj$dim), collapse = " × "),
      shape = paste(sapply(var_obj$dim, function(d) d$len), collapse = " × "),
      grib_short_name = attrs$GRIB_shortName %||% NA,
      grib_param_id = attrs$GRIB_paramId %||% NA,
      grib_type_of_level = attrs$GRIB_typeOfLevel %||% NA,
      grib_step_type = attrs$GRIB_stepType %||% NA,
      scale_factor = attrs$scale_factor %||% NA,
      add_offset = attrs$add_offset %||% NA,
      missing_value = attrs$missing_value %||% attrs$GRIB_missingValue %||% attrs$`_FillValue` %||% NA
    )
  })

  # Get global attributes
  global_attrs <- suppressWarnings(ncdf4::ncatt_get(nc, 0))

  # Extract GRIB metadata from first data variable
  data_vars <- names(nc$var)[!names(nc$var) %in% c("number", "expver")]
  grib_metadata <- NULL

  if (length(data_vars) > 0) {
    first_var_attrs <- suppressWarnings(ncdf4::ncatt_get(nc, data_vars[1]))

    grib_metadata <- list(
      grid_type = first_var_attrs$GRIB_gridType %||% NA,
      data_type = first_var_attrs$GRIB_dataType %||% NA,
      step_type = first_var_attrs$GRIB_stepType %||% NA,
      spatial_resolution = list(
        lon_increment_deg =
          first_var_attrs$GRIB_iDirectionIncrementInDegrees %||% NA,
        lat_increment_deg =
          first_var_attrs$GRIB_jDirectionIncrementInDegrees %||% NA,
        nx_points = first_var_attrs$GRIB_Nx %||% NA,
        ny_points = first_var_attrs$GRIB_Ny %||% NA
      ),
      spatial_extent = list(
        lat_first =
          first_var_attrs$GRIB_latitudeOfFirstGridPointInDegrees %||% NA,
        lat_last =
          first_var_attrs$GRIB_latitudeOfLastGridPointInDegrees %||% NA,
        lon_first =
          first_var_attrs$GRIB_longitudeOfFirstGridPointInDegrees %||% NA,
        lon_last =
          first_var_attrs$GRIB_longitudeOfLastGridPointInDegrees %||% NA
      ),
      scan_mode = list(
        i_scans_negatively = first_var_attrs$GRIB_iScansNegatively %||% NA,
        j_scans_positively = first_var_attrs$GRIB_jScansPositively %||% NA,
        j_points_consecutive =
          first_var_attrs$GRIB_jPointsAreConsecutive %||% NA
      )
    )
  }

  # Create summary
  list(
    file = basename(nc_file),
    resolution = resolution,
    extent = extent,
    dimensions = dimensions,
    variables = variables,
    global_attrs = global_attrs,
    grib_metadata = grib_metadata,
    summary = list(
      n_variables = nrow(variables),
      n_dimensions = length(dimensions),
      total_cells = prod(sapply(
        nc$dim[c("longitude", "latitude")], function(d) d$len)),
      institution = global_attrs$institution %||% NA,
      conventions = global_attrs$Conventions %||% NA
    )
  )
}

#' Print ERA5 Metadata Summary
#'
#' Prints a formatted summary of ERA5 NetCDF metadata.
#'
#' @param metadata List returned by `get_era5_metadata()`.
#'
#' @export
print_era5_metadata <- function(metadata) {
  cli::cli_h1("ERA5 NetCDF Metadata: {metadata$file}")

  # Summary
  cli::cli_h2("Dataset Summary")
  cli::cli_alert_info("Institution: {metadata$summary$institution}")
  cli::cli_alert_info("Conventions: {metadata$summary$conventions}")
  if (!is.null(metadata$grib_metadata)) {
    cli::cli_alert_info("GRIB Type: {metadata$grib_metadata$grid_type} ({metadata$grib_metadata$data_type})")
    cli::cli_alert_info("Step Type: {metadata$grib_metadata$step_type}")
  }

  # Spatial Information
  cli::cli_h2("Spatial Information")
  cli::cli_alert_info("Resolution: {metadata$resolution$lon_degrees}° × {metadata$resolution$lat_degrees}° (~{metadata$resolution$lon_km} × {metadata$resolution$lat_km} km)")
  cli::cli_alert_info("Extent: Lon [{metadata$extent$lon_min}, {metadata$extent$lon_max}], Lat [{metadata$extent$lat_min}, {metadata$extent$lat_max}]")
  cli::cli_alert_info("Grid points: {metadata$summary$total_cells} cells")

  if (!is.null(metadata$grib_metadata$spatial_resolution)) {
    grib_res <- metadata$grib_metadata$spatial_resolution
    cli::cli_alert_info("Grid size: {grib_res$nx_points} × {grib_res$ny_points} (lon × lat)")
  }

  # Dimensions
  cli::cli_h2("Dimensions ({metadata$summary$n_dimensions})")
  for (dim in metadata$dimensions) {
    cli::cli_li("{dim$name}: {dim$length} values ({dim$units})")
  }

  # Variables
  cli::cli_h2("Variables ({metadata$summary$n_variables})")
  print(metadata$variables)

  # GRIB Metadata details
  if (!is.null(metadata$grib_metadata)) {
    cli::cli_h2("GRIB Metadata")
    grib <- metadata$grib_metadata

    if (!is.null(grib$spatial_extent)) {
      cli::cli_h3("Spatial Extent (GRIB)")
      cli::cli_li("Latitude: {grib$spatial_extent$lat_first}° to {grib$spatial_extent$lat_last}°")
      cli::cli_li("Longitude: {grib$spatial_extent$lon_first}° to {grib$spatial_extent$lon_last}°")
    }

    if (!is.null(grib$scan_mode)) {
      cli::cli_h3("Scan Mode")
      cli::cli_li("i scans negatively: {grib$scan_mode$i_scans_negatively}")
      cli::cli_li("j scans positively: {grib$scan_mode$j_scans_positively}")
      cli::cli_li("j points consecutive: {grib$scan_mode$j_points_consecutive}")
    }
  }

  invisible(metadata)
}

#' Migrate ERA5 Filenames to New Format
#'
#' Renames ERA5 NetCDF files downloaded with the old naming scheme (without
#' variable names) to the new format that includes variable names in the
#' filename. This prevents conflicts when downloading different variables for
#' the same time period.
#'
#' @param dir Character. Directory containing ERA5 NetCDF files. Default is
#'     "era5_data".
#' @param dry_run Logical. If TRUE, shows what would be renamed without actually
#'   renaming files. Default is TRUE for safety.
#'
#' @return A tibble showing old and new filenames.
#'
#' @examples
#' \dontrun{
#' # Preview what would be renamed
#' migrate_era5_filenames(dir = "era5_data", dry_run = TRUE)
#'
#' # Actually rename the files
#' migrate_era5_filenames(dir = "era5_data", dry_run = FALSE)
#' }
#'
#' @export
migrate_era5_filenames <- function(dir = "era5_data", dry_run = TRUE) {
  # Check required packages
  if (!requireNamespace("ncdf4", quietly = TRUE)) {
    cli::cli_abort(c(
      "Package {.pkg ncdf4} is required for reading NetCDF files.",
      "i" = "Install with: {.code install.packages('ncdf4')}"
    ))
  }

  if (!dir.exists(dir)) {
    cli::cli_abort("Directory not found: {dir}")
  }

  # Find all NetCDF files
  nc_files <- list.files(dir, pattern = "\\.nc$", full.names = TRUE)

  if (length(nc_files) == 0) {
    cli::cli_alert_info("No NetCDF files found in {dir}")
    return(invisible(NULL))
  }

  cli::cli_h1("Migrating ERA5 Filenames")
  if (dry_run) {
    cli::cli_alert_warning("DRY RUN MODE - No files will be renamed")
  }

  # Get variable mapping
  var_mapping <- get_era5_variable_mapping()
  reverse_mapping <- setNames(names(var_mapping), var_mapping)

  results <- purrr::map_dfr(nc_files, function(nc_file) {
    basename_file <- basename(nc_file)

    # Check if file already has variable name in new format
    # New format: era5_variable_frequency_YYYYMM.nc
    # Old format: dataset_era5_YYYYMM.nc or dataset_era5_YYYYMM_variable.nc
    if (grepl(
      "^era5_[a-zA-Z0-9_-]+_(monthly|daily|hourly)_\\d{6}\\.nc$",
      basename_file)) {
      # Already in new format
      return(tibble::tibble(
        old_file = basename_file,
        new_file = basename_file,
        status = "already_migrated"
      ))
    }

    # Extract variables from NetCDF file
    nc <- tryCatch(
      ncdf4::nc_open(nc_file),
      error = function(e) {
        cli::cli_alert_warning("Could not open {basename_file}: {e$message}")
        return(NULL)
      }
    )

    if (is.null(nc)) {
      return(tibble::tibble(
        old_file = basename_file,
        new_file = NA_character_,
        status = "failed"
      ))
    }

    on.exit(ncdf4::nc_close(nc), add = TRUE)

    # Get data variables (exclude metadata variables)
    all_vars <- names(nc$var)
    data_vars <- all_vars[!all_vars %in% c("number", "expver")]

    if (length(data_vars) == 0) {
      cli::cli_alert_warning("No data variables found in {basename_file}")
      return(tibble::tibble(
        old_file = basename_file,
        new_file = NA_character_,
        status = "no_variables"
      ))
    }

    # Map NetCDF names to long names
    var_long_names <- purrr::map_chr(data_vars, function(var) {
      if (!is.null(reverse_mapping[[var]])) {
        reverse_mapping[[var]]
      } else {
        var
      }
    })

    # Shorten variable names for cleaner filenames
    var_short <- purrr::map_chr(var_long_names, function(v) {
      dplyr::case_when(
        grepl("temperature",
        v, ignore.case = TRUE) ~ gsub(
          "temperature", "temp", v, ignore.case = TRUE),
        grepl("precipitation",
        v, ignore.case = TRUE) ~ gsub(
          "precipitation", "precip", v, ignore.case = TRUE),
        grepl("component_of_wind", v) ~ gsub("_component_of_wind", "", v),
        TRUE ~ v
      )
    })

    # Sanitize variable names for filename
    var_string <- paste(gsub("[^a-zA-Z0-9]", "_", var_short), collapse = "-")

    # Extract date from old filename
    # Pattern: anything_YYYYMM.nc or anything_era5_YYYYMM.nc or anything_era5_YYYYMM_anything.nc
    date_pattern <- "\\d{6}"
    date_match <- stringr::str_extract(basename_file, date_pattern)

    if (is.na(date_match)) {
      cli::cli_alert_warning("Could not extract date from: {basename_file}")
      return(tibble::tibble(
        old_file = basename_file,
        new_file = NA_character_,
        status = "no_date_found"
      ))
    }

    # Extract frequency from old filename
    frequency <- if (grepl("monthly", basename_file)) {
      "monthly"
    } else if (grepl("daily", basename_file)) {
      "daily"
    } else if (grepl("hourly", basename_file)) {
      "hourly"
    } else {
      "monthly"  # default to monthly if not specified
    }

    # New format: era5_variable_frequency_YYYYMM.nc
    new_basename <- glue::glue("era5_{var_string}_{frequency}_{date_match}.nc")
    new_file <- file.path(dir, new_basename)

    # Check if new filename already exists
    if (file.exists(new_file) && new_file != nc_file) {
      cli::cli_alert_warning("Target file already exists: {new_basename}")
      return(tibble::tibble(
        old_file = basename_file,
        new_file = new_basename,
        status = "target_exists"
      ))
    }

    tibble::tibble(
      old_file = basename_file,
      new_file = new_basename,
      status = "to_rename"
    )
  })

  # Show summary
  cli::cli_h2("Migration Summary")
  status_counts <- table(results$status)
  for (status in names(status_counts)) {
    count <- status_counts[[status]]
    if (status == "to_rename") {
      cli::cli_alert_info(
        "{count} file{ifelse(count == 1, '', 's')} to rename")
    } else if (status == "already_migrated") {
      cli::cli_alert_success(
        "{count} file{ifelse(count == 1, '', 's')} already in new format")
    } else if (status == "target_exists") {
      cli::cli_alert_warning(
        "{count} file{ifelse(count == 1, '', 's')} - target exists")
    } else {
      cli::cli_alert_danger(
        "{count} file{ifelse(count == 1, '', 's')} - {status}")
    }
  }

  # Show files to rename
  to_rename <- results |> dplyr::filter(status == "to_rename")

  if (nrow(to_rename) > 0) {
    cli::cli_h2("Files to Rename")
    for (i in 1:nrow(to_rename)) {
      cli::cli_li("{to_rename$old_file[i]} → {to_rename$new_file[i]}")
    }

    # Perform rename if not dry run
    if (!dry_run) {
      cli::cli_h2("Renaming Files")
      for (i in 1:nrow(to_rename)) {
        old_path <- file.path(dir, to_rename$old_file[i])
        new_path <- file.path(dir, to_rename$new_file[i])

        success <- tryCatch(
          {
            file.rename(old_path, new_path)
            cli::cli_alert_success("Renamed: {to_rename$old_file[i]}")
            TRUE
          },
          error = function(e) {
            cli::cli_alert_danger(
              "Failed to rename {to_rename$old_file[i]}: {e$message}")
            FALSE
          }
        )

        results$status[results$old_file == to_rename$old_file[i]] <-
          if (success) "renamed" else "failed"
      }

      cli::cli_alert_success("Migration complete!")
    } else {
      cli::cli_alert_info(
        "Run with {.code dry_run = FALSE} to perform the migration")
    }
  } else {
    cli::cli_alert_success("All files are already in the new format!")
  }

  return(results)
}
