# internal helper to build worldpop url and filename
#' @noRd
.build_worldpop_url <- function(cc, year, type, dataset, resolution) {
  if (dataset == "legacy") {
    base_url <- "https://data.worldpop.org/GIS/Population"
    if (type == "density") base_url <- paste0(base_url, "_Density")
    base_url <- paste0(base_url, "/Global_2000_2020_1km_UNadj/")

    fn <- if (type == "count") {
      sprintf("%s_ppp_%s_1km_Aggregated_UNadj.tif", tolower(cc), year)
    } else {
      sprintf("%s_pd_%s_1km_UNadj.tif", tolower(cc), year)
    }

    url <- sprintf("%s%s/%s/%s", base_url, year, toupper(cc), fn)
    return(list(url = url, filename = fn))
  }

  # r2025a dataset
  base_url <- "https://data.worldpop.org/GIS/Population/Global_2015_2030/R2025A/"

  if (resolution == "1km") {
    res_path <- "1km_ua"
    res_suffix <- "1km"
    ua_suffix <- "_UA"
  } else {
    res_path <- "100m"
    res_suffix <- "100m"
    ua_suffix <- ""
  }

  # remote filename includes R2025A, local filename does not
 remote_fn <- sprintf("%s_pop_%s_CN_%s_R2025A%s_v1.tif",
                       tolower(cc), year, res_suffix, ua_suffix)
  local_fn <- sprintf("%s_pop_%s_CN_%s%s_v1.tif",
                      tolower(cc), year, res_suffix, ua_suffix)

  url <- sprintf("%s%s/%s/v1/%s/constrained/%s",
                 base_url, year, toupper(cc), res_path, remote_fn)

  return(list(url = url, filename = local_fn))
}

#' Download Population Rasters from WorldPop
#'
#' Downloads population raster files from WorldPop for specified countries and
#' years. Automatically selects the appropriate dataset based on years:
#' legacy (2000-2020) or R2025A (2015-2030).
#'
#' @param country_codes Character vector of ISO country codes (e.g., "GBR",
#'   "GIN")
#' @param years Numeric vector of years to download data for (2000-2030).
#' @param type Character; either "density" for persons per sq km or "count" for
#'   total population count (default: "count"). Note: density is only available
#'   for years 2000-2020.
#' @param resolution Character; either "1km" (default) or "100m". The 100m
#'   resolution is only available for years 2015-2030.
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
#' The function automatically selects the appropriate WorldPop dataset:
#'
#' ## Legacy Dataset (years < 2015)
#' Downloads UN-adjusted population rasters from WorldPop's Global 2000-2020
#' dataset. Available at 1km resolution only. Supports both count and density.
#'
#' ## R2025A Dataset (years >= 2015)
#' Downloads constrained population count rasters from WorldPop's Global
#' 2015-2030 dataset (R2025A release). Available at 1km and 100m resolution.
#' Population is constrained to built-up areas.
#'
#' If your year range spans both datasets (e.g., 2010:2020), the function will
#' automatically download from both sources.
#'
#' Files are downloaded to the specified directory, with existing files skipped.
#' Progress is shown during downloads and a summary is provided upon completion.
#'
#' @examples
#' \dontrun{
#' # Download population data for Guinea (auto-selects dataset)
#' download_worldpop("GIN", years = 2015:2020)
#'
#' # Download legacy data (2000-2014)
#' download_worldpop("GIN", years = 2000:2010)
#'
#' # Download spanning both datasets
#' download_worldpop("GIN", years = 2010:2020)
#'
#' # Download 100m resolution data (R2025A only)
#' download_worldpop("GIN", years = 2020, resolution = "100m")
#' }
#' @export
download_worldpop <- function(
    country_codes,
    years = 2000:2020,
    type = "count",
    resolution = "1km",
    dest_dir = here::here(),
    quiet = FALSE) {

  type <- match.arg(type, choices = c("count", "density"))
  resolution <- match.arg(resolution, choices = c("1km", "100m"))

  # split years into legacy (<2015) and r2025a (>=2015)
  legacy_years <- years[years < 2015]
  r2025a_years <- years[years >= 2015]

  # validate year ranges

  if (length(legacy_years) > 0 && (min(legacy_years) < 2000)) {
    cli::cli_abort("Legacy dataset only available for years 2000-2020")
  }
  if (length(r2025a_years) > 0 && max(r2025a_years) > 2030) {
    cli::cli_abort("R2025A dataset only available for years 2015-2030")
  }

  # density only available in legacy dataset
  if (type == "density" && length(r2025a_years) > 0) {
    cli::cli_abort(c(
      "Density data only available for years 2000-2020",
      "i" = "Use {.code type = 'count'} for years >= 2015"
    ))
  }

  # 100m resolution only available in r2025a
  if (resolution == "100m" && length(legacy_years) > 0) {
    cli::cli_abort(c(
      "100m resolution only available for years >= 2015",
      "i" = "Use {.code resolution = '1km'} for years < 2015"
    ))
  }

  if (!dir.exists(dest_dir)) dir.create(dest_dir, recursive = TRUE)

  all_results <- list()
  all_params <- list()


  # download legacy years (< 2015)
  if (length(legacy_years) > 0) {
    cli::cli_alert_info(
      "Using legacy dataset (2000-2020 UN-adjusted) for years: {.val {legacy_years}}"
    )
    legacy_res <- .download_worldpop_batch(
      country_codes, legacy_years, type, "legacy", "1km", dest_dir, quiet
    )
    all_results <- c(all_results, legacy_res$results)
    all_params <- c(all_params, list(legacy_res$params))
  }

  # download r2025a years (>= 2015)
  if (length(r2025a_years) > 0) {
    cli::cli_alert_info(
      "Using R2025A dataset (2015-2030 constrained) for years: {.val {r2025a_years}}"
    )
    r2025a_res <- .download_worldpop_batch(
      country_codes, r2025a_years, type, "r2025a", resolution, dest_dir, quiet
    )
    all_results <- c(all_results, r2025a_res$results)
    all_params <- c(all_params, list(r2025a_res$params))
  }

  # combine params for counting
  combined_params <- do.call(rbind, all_params)
  counts <- tapply(combined_params$success, combined_params$country, sum)

  cli::cli_alert_success(
    "Download of WorldPop {type} rasters is complete!"
  )

  for (cc in names(counts)) {
    cli::cli_alert_info(
      glue::glue("{cc}: {counts[[cc]]} of {length(years)} years downloaded")
    )
  }

  invisible(list(
    files = unlist(all_results),
    counts = counts
  ))
}

#' @noRd
.download_worldpop_batch <- function(
    country_codes, years, type, dataset, resolution, dest_dir, quiet) {

  params <- expand.grid(
    country = country_codes,
    year = years,
    stringsAsFactors = FALSE
  )

  results <- mapply(
    function(cc, yr) {
      url_info <- .build_worldpop_url(cc, yr, type, dataset, resolution)
      fn <- url_info$filename
      url <- url_info$url

      dest <- file.path(dest_dir, fn)
      if (file.exists(dest)) {
        if (!quiet) cli::cli_alert_info("Exists: {fn}")
        return(dest)
      }

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

  success <- !is.na(results) & file.exists(results)
  params$success <- success

  list(results = results, params = params)
}

#' Download WorldPop Population Raster Data for Specific Age Bands
#'
#' Downloads and processes WorldPop population raster data for specified age
#' bands and years. Combines male and female population data for the requested
#' age range. Automatically selects legacy (2000-2014) or R2024B (2015-2030)
#' dataset based on year.
#'
#' @param country_codes Character vector. ISO3 country codes (e.g., "TUN", "BDI")
#' @param years Numeric vector. Years for which to download data (2000-2030)
#' @param age_range Numeric vector of length 2 specifying the lower and upper
#'   age range bounds (e.g., c(1, 9) for ages 1-9). Default: c(1, 9)
#' @param resolution Character. Either "1km" (default) or "100m". The 100m
#'   resolution is only available for years >= 2015 (R2024B dataset).
#' @param out_dir Character string. Directory where downloaded files will be
#'   saved. Default: "."
#'
#' @details
#' ## Data Source
#' The function automatically selects the appropriate dataset:
#'
#' ### Legacy Dataset (years < 2015)
#' - URL: https://data.worldpop.org/GIS/AgeSex_structures/Global_2000_2020_1km/
#' - Resolution: 1km only
#' - Files: `{iso3}_{sex}_{code}_{year}_1km.tif`
#'
#' ### R2024B Dataset (years >= 2015)
#' - URL: https://data.worldpop.org/GIS/AgeSex_structures/Global_2015_2030/R2024B/
#' - Resolution: 1km or 100m
#' - Files: `{iso3}_{sex}_{code}_{year}_UC_{res}_UA_v1.tif` (R2024B removed locally)
#'
#' ## Band Combination Logic
#' If the exact age range is not covered by a single WorldPop band, the function
#' combines adjacent bands. For example:
#' - Requesting ages 2-9 will combine bands 1-4 and 5-9
#' - Requesting ages 0-10 will combine bands 0-1, 1-4, 5-9, and 10-14
#'
#' ## Age Bands
#' Available bands: 0, 1-4, 5-9, 10-14, 15-19, 20-24, 25-29, 30-34, 35-39,
#' 40-44, 45-49, 50-54, 55-59, 60-64, 65-69, 70-74, 75-79, 80+
#'
#' @return No return value. Files saved to output directory with pattern:
#'   `{iso3}_total_{lower}_{upper}_{year}.tif`
#'
#' @examples
#' \dontrun{
#' # Download age 1-9 data for Burundi
#' download_worldpop_age_band(
#'   country_codes = "BDI",
#'   years = 2020:2024,
#'   age_range = c(1, 9),
#'   out_dir = "data/worldpop"
#' )
#' }
#' @export
download_worldpop_age_band <- function(
    country_codes,
    years,
    age_range = c(1, 9),
    resolution = "1km",
    out_dir = ".") {

  resolution <- match.arg(resolution, choices = c("1km", "100m"))

  # 100m only available for years >= 2015
  legacy_years <- years[years < 2015]
  if (resolution == "100m" && length(legacy_years) > 0) {
    cli::cli_abort(c(
      "100m resolution only available for years >= 2015",
      "i" = "Use {.code resolution = '1km'} for years < 2015"
    ))
  }

  base::dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  bands <- base::data.frame(
    code = c(0, 1, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80),
    lower = c(0, 1, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80),
    upper = c(1, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, Inf)
  )

  sexes <- c("m", "f")

  # build url and local filename based on year and resolution
  make_url_info <- function(cc, sex, code, yr, res) {
    cc_lo <- tolower(cc)
    cc_up <- toupper(cc)
    # age band code needs leading zero for single digits
    code_str <- sprintf("%02d", code)

    if (yr < 2015) {
      # legacy dataset (2000-2020, 1km only)
      url_base <- paste0(
        "https://data.worldpop.org/GIS/AgeSex_structures/",
        "Global_2000_2020_1km/unconstrained"
      )
      url <- sprintf("%s/%d/%s/%s_%s_%s_%d_1km.tif",
                     url_base, yr, cc_up, cc_lo, sex, code, yr)
      local_fn <- sprintf("%s_%s_%s_%d_1km.tif", cc_lo, sex, code, yr)
    } else {
      # r2024b dataset (2015-2030, 1km or 100m)
      url_base <- paste0(
        "https://data.worldpop.org/GIS/AgeSex_structures/",
        "Global_2015_2030/R2024B"
      )

      if (res == "1km") {
        res_path <- "1km_ua"
        res_suffix <- "1km"
        ua_suffix <- "_UA"
      } else {
        res_path <- "100m"
        res_suffix <- "100m"
        ua_suffix <- ""
      }

      remote_fn <- sprintf("%s_%s_%s_%d_UC_%s_R2024B%s_v1.tif",
                           cc_lo, sex, code_str, yr, res_suffix, ua_suffix)
      url <- sprintf("%s/%d/%s/v1/%s/unconstrained/%s",
                     url_base, yr, cc_up, res_path, remote_fn)
      # local filename without R2024B
      local_fn <- sprintf("%s_%s_%s_%d_UC_%s%s_v1.tif",
                          cc_lo, sex, code_str, yr, res_suffix, ua_suffix)
    }

    list(url = url, filename = local_fn)
  }

  combos <- expand.grid(
    country_code = country_codes,
    year = years,
    stringsAsFactors = FALSE
  )

  for (i in seq_len(nrow(combos))) {
    cc <- combos$country_code[i]
    yr <- combos$year[i]
    cc_lo <- tolower(cc)

    # inform user which dataset
    if (yr < 2015) {
      cli::cli_alert_info("Using legacy dataset (1km) for {cc}, {yr}")
    } else {
      cli::cli_alert_info("Using R2024B dataset ({resolution}) for {cc}, {yr}")
    }

    matching_bands <- subset(
      bands,
      lower < (age_range[2] + 1) & upper > age_range[1]
    )

    if (nrow(matching_bands) == 0) {
      cli::cli_alert_danger(
        "No bands found covering {age_range[1]}-{age_range[2]} for {cc}"
      )
      next
    }

    adjusted_upper <- ifelse(
      matching_bands$upper == Inf,
      matching_bands$upper,
      matching_bands$upper - 1
    )
    band_labels <- paste(matching_bands$lower, adjusted_upper, sep = "-")

    covered_lower <- min(matching_bands$lower)
    covered_upper <- max(adjusted_upper)

    if (covered_upper < age_range[2]) {
      cli::cli_alert_warning(
        paste0(
          "Requested {age_range[1]}-{age_range[2]} not fully covered. ",
          "Downloading {covered_lower}-{covered_upper} instead."
        )
      )
    } else {
      cli::cli_alert_info(
        "Combining bands: {paste(band_labels, collapse = ', ')}"
      )
    }

    out_fname <- file.path(
      out_dir,
      sprintf("%s_total_%02d_%02d_%d.tif", cc_lo, covered_lower, covered_upper, yr)
    )

    if (file.exists(out_fname)) {
      cli::cli_alert_info("Skipping {basename(out_fname)} (already exists)")
      next
    }

    acc <- NULL
    for (band_code in matching_bands$code) {
      for (sex in sexes) {
        url_info <- make_url_info(cc, sex, band_code, yr, resolution)
        temp_fname <- file.path(out_dir, url_info$filename)

        if (!file.exists(temp_fname)) {
          cli::cli_alert_info(
            "Downloading {sex} band {band_code} for {cc}, {yr}"
          )
          utils::download.file(
            url_info$url,
            temp_fname,
            mode = "wb",
            quiet = TRUE
          )
        } else {
          cli::cli_alert_info("Using cached file {basename(temp_fname)}")
        }

        r <- suppressWarnings(terra::rast(temp_fname))
        acc <- if (is.null(acc)) r else acc + r
      }
    }

    suppressWarnings(terra::writeRaster(acc, out_fname, overwrite = TRUE))
    cli::cli_alert_success("Written: {basename(out_fname)}")
  }
}
