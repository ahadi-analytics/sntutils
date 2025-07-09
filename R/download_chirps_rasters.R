#' List Available Monthly CHIRPS Dataset Options
#'
#' Returns a tibble with supported **monthly** CHIRPS datasets available for
#' download using the `download_chirps()` function. Each entry includes the
#' dataset code, descriptive label, and the subdirectory path on the CHIRPS
#' FTP server where `.tif.gz` files are stored.
#'
#' @return A tibble with 3 columns:
#'   \describe{
#'     \item{dataset}{Machine-readable dataset code (e.g., `"africa_monthly"`)}
#'     \item{label}{Descriptive label for user display}
#'     \item{subdir}{Subdirectory path to the CHIRPS TIFF archive}
#'   }
#'
#' @examples
#' chirps_options()
#'
#' @export
chirps_options <- function() {
  tibble::tibble(
    dataset = c(
      "global_monthly",
      "africa_monthly",
      "camer-carib_monthly",
      "EAC_monthly"
    ),
    frequency = c(
      "monthly"
    ),
    label = c(
      "Global (Monthly)",
      "Africa (Monthly)",
      "Caribbean & Central America (Monthly)",
      "East African Community (Monthly)"
    ),
    subdir = c(
      "global_monthly/tifs",
      "africa_monthly/tifs",
      "camer-carib_monthly/tifs",
      "EAC_monthly/tifs"
    )
  )
}

#' List Available CHIRPS Raster Files for a Dataset
#'
#' Scrapes the UCSB CHIRPS archive to list available `.tif.gz` raster files
#' for a given dataset (e.g., `"africa_monthly"`). Extracts year and month
#' from filenames where possible.
#'
#' @param dataset_code Character. One of the dataset codes from
#'   `chirps_options()`, such as `"africa_monthly"`.
#'
#' @return A tibble with the following columns:
#' \describe{
#'   \item{file_name}{The filename of the `.tif.gz` raster.}
#'   \item{year}{Extracted year (YYYY) from filename.}
#'   \item{month}{Extracted month (MM) from filename, if available.}
#'   \item{dataset}{The dataset code queried.}
#' }
#' Returns `NULL` if the dataset cannot be accessed.
#'
#' @examples
#' \dontrun{
#' # List available Africa monthly files
#' check_chirps_available("africa_monthly")
#' }
#'
#' @export
check_chirps_available <- function(dataset_code = "africa_monthly") {
  base_url <- paste0(
    "https://data.chc.ucsb.edu/products/CHIRPS-2.0/",
    dataset_code, "/tifs/"
  )

  tryCatch({
    page <- xml2::read_html(base_url)
    links <- rvest::html_elements(page, "a")
    files <- rvest::html_text(links)
    files <- files[grepl("\\.tif\\.gz$", files)]

    df <- tibble::tibble(
      file_name = files,
      year = stringr::str_extract(files, "\\d{4}"),
      month = stringr::str_extract(files, "\\d{4}\\.(\\d{2})") |>
        stringr::str_remove("\\d{4}\\."),
      dataset = dataset_code
    ) |>
      dplyr::filter(!is.na(year)) |>
      dplyr::arrange(desc(year), month)

    if (nrow(df) == 0) {
      cli::cli_alert_info(glue::glue(
        "No valid CHIRPS files found for {dataset_code}."
      ))
      return(NULL)
    }

    dates <- as.Date(paste(df$year, df$month, "01", sep = "-"))
    start_date <- format(min(dates, na.rm = TRUE), "%b %Y")
    end_date <- format(max(dates, na.rm = TRUE), "%b %Y")

    if (all(!is.na(start_date))) {
      cli::cli_alert_success(glue::glue(
        "{dataset_code}: Data available from {start_date} to {end_date}."
      ))
    } else {
      cli::cli_alert_success(glue::glue(
        "{dataset_code}: Data available {yr_range[1]} - {yr_range[2]}."
      ))
    }

    return(df)

  }, error = function(e) {
    cli::cli_alert_danger(glue::glue("Could not access {base_url}"))
    return(NULL)
  })
}

#' Download CHIRPS Raster Data from UCSB Archive
#'
#' Downloads `.tif.gz` CHIRPS rainfall data files from the UCSB Climate Hazards
#' Group archive for a specified dataset and date range. Supports monthly,
#' dekadal (3 per month), and pentadal (6 per month) resolutions. Files are
#' downloaded and optionally unzipped to a local directory.
#'
#' Use [chirps_options()] to view all available datasets and their metadata.
#'
#' @param dataset Character. One of the dataset codes listed in
#'     [chirps_options()].
#' @param start Character. Start date in `"YYYY-MM"` format (e.g., `"2020-01"`).
#' @param end Character. End date in `"YYYY-MM"` format. If `NULL`, only
#'   `start` month is downloaded.
#' @param out_dir Directory path where downloaded files will be saved. Will be
#'   created if it does not exist.
#' @param unzip Logical. If `TRUE`, the `.tif.gz` files will be unzipped after
#'   download.
#'
#' @return No return value. Side effects include downloading CHIRPS `.tif.gz`
#'   and `.tif` files to the specified folder.
#'
#' @examples
#' # View available datasets
#' chirps_options()
#'
#' # Download Africa monthly CHIRPS for Jan–Mar 2022
#' \dontrun{
#' download_chirps2.0(
#'   dataset = "africa_monthly",
#'   start = "2022-01",
#'   end = "2022-03",
#'   out_dir = "chirps_data"
#' )
#' }
#'
#' @export
download_chirps2.0 <- function(dataset, start, end = NULL,
                               out_dir = ".", unzip = TRUE) {
  opts <- chirps_options()
  if (!dataset %in% opts$dataset) {
    cli::cli_abort(
      "Invalid dataset. Use `chirps_options()` to see available options.")
  }

  sel <- opts[opts$dataset == dataset, ]
  freq <- sel$frequency
  subdir <- sel$subdir
  base_url <- glue::glue(
    "https://data.chc.ucsb.edu/products/CHIRPS-2.0/{subdir}")

  if (is.null(end)) {
    dates <- as.Date(paste0(start, "-01"))
  } else {
    dates <- seq(as.Date(paste0(start, "-01")),
                 as.Date(paste0(end, "-01")), by = "month")
  }

  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  cli::cli_h1(glue::glue("Downloading CHIRPS: {sel$label}"))
  cli::cli_progress_bar("Downloading", total = length(dates))
  cli::cli_text("")

  for (i in seq_along(dates)) {
    d <- dates[i]
    year <- format(d, "%Y")
    month <- format(d, "%m")

    if (freq == "monthly") {
      orig_name <- glue::glue("chirps-v2.0.{year}.{month}.tif.gz")
      custom_name <- glue::glue("{dataset}_chirps-v2.0.{year}.{month}.tif.gz")
      url <- glue::glue("{base_url}/{orig_name}")
      dest <- file.path(out_dir, custom_name)
      tif  <- sub(".gz$", "", dest)

      if (!file.exists(tif)) {
        tryCatch({
          curl::curl_download(url, dest, mode = "wb")
          cli::cli_alert_success("Downloaded {custom_name}")
          if (unzip && file.exists(dest)) R.utils::gunzip(dest,
                                                          overwrite = FALSE)
        }, error = function(e) {
          cli::cli_alert_danger("Failed {custom_name}: {e$message}")
        })
      } else {
        cli::cli_alert_info("Skipping {basename(tif)}, already exists.")
      }

    } else {
      for (subperiod in 1:6) {
        orig_name <- glue::glue(
          "chirps-v2.0.{year}.{month}.{subperiod}.tif.gz")
        custom_name <- glue::glue(
          "{dataset}_chirps-v2.0.{year}.{month}.{subperiod}.tif.gz")
        url <- glue::glue("{base_url}/{orig_name}")
        dest <- file.path(out_dir, custom_name)
        tif  <- sub(".gz$", "", dest)

        if (!file.exists(tif)) {
          tryCatch({
            curl::curl_download(url, dest, mode = "wb")
            cli::cli_alert_success("Downloaded {custom_name}")
            if (unzip && file.exists(dest)) R.utils::gunzip(dest,
                                                            overwrite = FALSE)
          }, error = function(e) {
            cli::cli_alert_danger("Failed {custom_name}: {e$message}")
          })
        } else {
          cli::cli_alert_info("Skipping {basename(tif)}, already exists.")
        }
      }
    }

    cli::cli_progress_update()
  }

  cli::cli_progress_done()
  cli::cli_text("")
  cli::cli_alert_success("All CHIRPS files downloaded")
}
