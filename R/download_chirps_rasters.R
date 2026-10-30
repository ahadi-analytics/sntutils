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
    cli::cli_alert_danger(paste0("Could not access ", base_url))
    return(NULL)
  })
}

#' Helper function to find existing CHIRPS files with naming variations
#' @noRd
.find_chirps_variations <- function(out_dir, year, month, subperiod = NULL) {
  # Build pattern to match various naming conventions
  # Match patterns like:
  # - chirps-v2.0.2018.04.tif
  # - africa_monthly_chirps-v2.0.2018.04.tif
  # - chirps-v2.0.2018.04.tif.gz
  # - africa_monthly_chirps-v2.0.2018.04.tif.gz

  if (is.null(subperiod)) {
    # Monthly pattern
    pattern <- sprintf("chirps-v2\\.0\\.%s\\.%s\\.(tif|tif\\.gz)$", year, month)
  } else {
    # Pentadal pattern
    pattern <- sprintf("chirps-v2\\.0\\.%s\\.%s\\.%s\\.(tif|tif\\.gz)$",
                      year, month, subperiod)
  }

  # List all files in directory
  if (!dir.exists(out_dir)) return(NULL)

  files <- list.files(out_dir, pattern = pattern, full.names = TRUE,
                      ignore.case = TRUE)

  if (length(files) == 0) return(NULL)

  # Return first matching file (prioritize .tif over .tif.gz)
  tif_files <- files[grepl("\\.tif$", files)]
  if (length(tif_files) > 0) {
    return(list(path = tif_files[1], type = "tif"))
  }

  gz_files <- files[grepl("\\.tif\\.gz$", files)]
  if (length(gz_files) > 0) {
    return(list(path = gz_files[1], type = "gz"))
  }

  return(NULL)
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
#' @param max_workers Integer. Maximum concurrent downloads. Defaults to `6`.
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
#' download_chirps(
#'   dataset = "africa_monthly",
#'   start = "2022-01",
#'   end = "2022-03",
#'   out_dir = "chirps_data"
#' )
#' }
#'
#' @export
download_chirps <- function(dataset, start, end = NULL,
                            out_dir = ".", unzip = TRUE,
                            max_workers = 6L) {
  opts <- chirps_options()
  if (!dataset %in% opts$dataset) {
    cli::cli_abort(
      "Invalid dataset. Use `chirps_options()` to see available options.")
  }

  sel <- opts[opts$dataset == dataset, ]
  freq <- sel$frequency
  subdir <- sel$subdir
  base_url <- paste0(
    "https://data.chc.ucsb.edu/products/CHIRPS-2.0/", subdir)

  if (is.null(end)) {
    dates <- as.Date(paste0(start, "-01"))
  } else {
    dates <- seq(as.Date(paste0(start, "-01")),
                 as.Date(paste0(end, "-01")), by = "month")
  }

  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  cli::cli_h1(paste0("Downloading CHIRPS: ", sel$label))

  # build list of (url, dest) pairs to download; handle skips/unzips inline
  todo <- list()
  add_todo <- function(url, dest) {
    todo[[length(todo) + 1L]] <<- list(url = url, dest = dest)
  }

  subperiods <- if (freq == "monthly") list(NULL) else as.list(1:6)

  for (d in dates) {
    d <- as.Date(d, origin = "1970-01-01")
    year <- format(d, "%Y")
    month <- format(d, "%m")

    for (sp in subperiods) {
      if (is.null(sp)) {
        orig_name <- sprintf("chirps-v2.0.%s.%s.tif.gz", year, month)
        custom_name <- sprintf(
          "%s_chirps-v2.0.%s.%s.tif.gz", dataset, year, month)
        tag <- sprintf("%s-%s", year, month)
      } else {
        orig_name <- sprintf(
          "chirps-v2.0.%s.%s.%s.tif.gz", year, month, sp)
        custom_name <- sprintf(
          "%s_chirps-v2.0.%s.%s.%s.tif.gz", dataset, year, month, sp)
        tag <- sprintf("%s-%s.%s", year, month, sp)
      }
      url <- paste0(base_url, "/", orig_name)
      dest <- file.path(out_dir, custom_name)

      existing <- .find_chirps_variations(out_dir, year, month, sp)

      # fallback: match against cleaned filenames already on disk
      if (is.null(existing)) {
        all_files <- list.files(
          out_dir, pattern = "chirps.*\\.tif(\\.gz)?$",
          full.names = TRUE, ignore.case = TRUE)
        if (length(all_files) > 0) {
          cleaned_names <- clean_filenames(all_files)
          target_pattern <- if (is.null(sp)) {
            sprintf(
              "chirps.*v2.*%s\\.%s\\.(tif|tif\\.gz)$", year, month)
          } else {
            sprintf(
              "chirps.*v2.*%s\\.%s\\.%s\\.(tif|tif\\.gz)$",
              year, month, sp)
          }
          matching <- grep(target_pattern, cleaned_names, value = TRUE)
          if (length(matching) > 0) {
            idx <- which(cleaned_names == matching[1])
            if (length(idx) > 0 && file.exists(all_files[idx[1]])) {
              file_type <- ifelse(
                grepl("\\.tif$", all_files[idx[1]]), "tif", "gz")
              existing <- list(
                path = all_files[idx[1]], type = file_type)
            }
          }
        }
      }

      if (!is.null(existing)) {
        if (existing$type == "gz" && unzip) {
          tif_check <- sub("\\.gz$", "", existing$path)
          if (!file.exists(tif_check)) {
            cli::cli_alert_info(
              "Unzipping existing {basename(existing$path)}")
            R.utils::gunzip(
              existing$path, overwrite = FALSE, remove = FALSE)
          } else {
            cli::cli_alert_info(
              "Skipping {tag}, found existing: {basename(tif_check)}")
          }
        } else {
          cli::cli_alert_info(
            "Skipping {tag}, found existing: {basename(existing$path)}")
        }
      } else {
        add_todo(url, dest)
      }
    }
  }

  if (length(todo) == 0) {
    cli::cli_alert_success("All CHIRPS files already present")
    return(invisible(NULL))
  }

  urls <- vapply(todo, `[[`, character(1), "url")
  dests <- vapply(todo, `[[`, character(1), "dest")

  reqs <- lapply(urls, function(u) {
    httr2::request(u) |>
      httr2::req_error(body = function(resp) {
        paste("HTTP", httr2::resp_status(resp))
      })
  })

  resps <- httr2::req_perform_parallel(
    reqs,
    paths = dests,
    max_active = max_workers,
    on_error = "continue",
    progress = sprintf(
      "Downloading %d CHIRPS file(s)", length(urls)))

  # report per-file success/failure, then unzip successful downloads
  for (i in seq_along(resps)) {
    r <- resps[[i]]
    if (inherits(r, "error")) {
      cli::cli_alert_danger(
        "Failed {basename(dests[[i]])}: {conditionMessage(r)}")
      next
    }
    cli::cli_alert_success("Downloaded {basename(dests[[i]])}")
    if (unzip && file.exists(dests[[i]])) {
      tryCatch(
        R.utils::gunzip(dests[[i]], overwrite = FALSE),
        error = function(e) {
          cli::cli_alert_danger(
            "Failed to unzip {basename(dests[[i]])}: {e$message}")
        })
    }
  }

  cli::cli_alert_success("All CHIRPS files downloaded")
  invisible(NULL)
}
