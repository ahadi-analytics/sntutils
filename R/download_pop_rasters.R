#' Download Population Rasters from WorldPop
#'
#' Downloads population raster files (either density in persons per square
#' kilometer or total count) from WorldPop for specified countries and years.
#' The function handles downloading multiple files, skips existing files, and
#' provides progress updates.
#'
#' @param country_codes Character vector of ISO country codes (e.g., "GBR",
#'   "USA")
#' @param years Numeric vector of years to download data for
#'   (default: 2000-2020)
#' @param type Character; either "density" for persons per sq km or "count" for
#'   total population count (default: "count")
#' @param dest_dir Destination directory for downloaded files
#'   (default: current dir)
#' @param quiet Logical; if TRUE, suppresses progress messages
#'   (default: FALSE)
#'
#' @return Invisible list containing:
#'   \itemize{
#'     \item files: Vector of paths to downloaded/existing files
#'     \item counts: Named vector of successful downloads per country
#'   }
#'
#' @details
#' Downloads 1km resolution UN-adjusted population rasters from WorldPop. For
#' density type, values represent persons per square kilometer. For count type,
#' values represent total population count per pixel. Files are downloaded to
#' the specified directory, with existing files skipped. Progress is shown
#' during downloads and a summary is provided upon completion.
#'
#' @examples
#' \dontrun{
#' # Download population density data for UK and France for 2019-2020
#' download_worldpop(c("GBR", "FRA"), years = 2019:2020, type = "density")
#'
#' # Download population count data
#' download_worldpop(c("GBR", "FRA"), years = 2019:2020, type = "count")
#' }
#' @export
download_worldpop <- function(
    country_codes,
    years = 2000:2020,
    type = "count",
    dest_dir = here::here(),
    quiet = FALSE) {

  type <- match.arg(type)
  if (!dir.exists(dest_dir)) dir.create(dest_dir, recursive = TRUE)

  base_url <- "https://data.worldpop.org/GIS/Population"
  if (type == "density") {
    base_url <- paste0(base_url, "_Density")
  }
  base_url <- paste0(base_url, "/Global_2000_2020_1km_UNadj/")

  params <- expand.grid(
    country = country_codes,
    year = years,
    stringsAsFactors = FALSE
  )

  # download (or skip if exists) one file per row
  results <- mapply(
    function(cc, yr) {
      suffix <- if (type == "density") "pd" else "ppp"
      fn <- sprintf(
        "%s_%s_%s_1km_UNadj.tif",
        tolower(cc), suffix, yr
      )
      if (type == "count") {
        fn <- sprintf(
          "%s_ppp_%s_1km_Aggregated_UNadj.tif",
          tolower(cc), yr
        )
      }
      dest <- file.path(dest_dir, fn)
      if (file.exists(dest)) {
        if (!quiet) cli::cli_alert_info("Exists: {fn}")
        return(dest)
      }
      url <- sprintf("%s%s/%s/%s", base_url, yr, toupper(cc), fn)
      httr2::request(url) |>
        httr2::req_timeout(600) |>
        httr2::req_progress() |>
        httr2::req_perform(path = dest)
      dest
    },
    params$country,
    params$year,
    USE.NAMES = FALSE
  )

  # mark which were truly present on disk
  success <- !is.na(results) & file.exists(results)
  params$success <- success

  # count successes per country
  counts <- tapply(params$success, params$country, sum)

  cli::cli_alert_success(
    "Download of Worldpop {type} rasters is complete!"
  )

  # report
  for (cc in names(counts)) {
    cli::cli_alert_info(
      glue::glue(
        "{cc}: {counts[[cc]]} of {length(years)} years downloaded"
      )
    )
  }

  invisible(list(
    files = results,
    counts = counts
  ))
}
