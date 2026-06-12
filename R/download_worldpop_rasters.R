# internal helper to build worldpop url and filename
# cc may be an ISO3 code or the literal "GLOBAL" to target the worldwide
# mosaic under 0_Mosaicked (r2025a + 1km + count only — legacy has no mosaic)
#' @noRd
.build_worldpop_url <- function(cc, year, type, dataset, resolution) {
  is_global <- toupper(cc) == "GLOBAL"

  if (dataset == "legacy") {
    if (is_global) {
      cli::cli_abort(
        "{.val GLOBAL} mosaic not available for legacy years (2000-2014)"
      )
    }
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
  if (is_global && resolution != "1km") {
    cli::cli_abort(
      "{.val GLOBAL} mosaic only available at 1km resolution"
    )
  }

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

  if (is_global) {
    cc_lo <- "global"
    country_segment <- "0_Mosaicked"
  } else {
    cc_lo <- tolower(cc)
    country_segment <- toupper(cc)
  }

  # remote filename includes R2025A, local filename does not
  remote_fn <- sprintf(
    "%s_pop_%s_CN_%s_R2025A%s_v1.tif",
    cc_lo, year, res_suffix, ua_suffix
  )
  local_fn <- sprintf(
    "%s_pop_%s_CN_%s%s_v1.tif",
    cc_lo, year, res_suffix, ua_suffix
  )

  url <- sprintf(
    "%s%s/%s/v1/%s/constrained/%s",
    base_url, year, country_segment, res_path, remote_fn
  )

  return(list(url = url, filename = local_fn))
}

# internal helper: download many urls to dests concurrently. skips files
# that already exist, writes to a .part file and renames on success so an
# interrupted download never leaves a corrupt raster behind. tries the
# worldpop ftp mirror first (same /GIS tree, ~60x faster than the
# per-connection throttle on data.worldpop.org), then falls back to https
# per file for networks that block ftp. returns a logical vector, TRUE
# where the file is present after the call.
# requires httr2 >= 1.1.0 so req_retry() is honoured in parallel.
#' @noRd
.worldpop_fetch <- function(urls, dests, quiet = FALSE, max_active = 6) {
  missing_idx <- which(!file.exists(dests))
  if (length(missing_idx) == 0) {
    return(rep(TRUE, length(dests)))
  }

  part_paths <- paste0(dests[missing_idx], ".part")
  unlink(part_paths)

  ftp_urls <- sub(
    "^https://data\\.worldpop\\.org/",
    "ftp://ftp.worldpop.org.uk/",
    urls[missing_idx]
  )

  # up to 3 ftp attempts: transient ftp errors are cheap to retry at full
  # mirror speed, whereas the https fallback is throttled server-side
  fetched <- rep(FALSE, length(missing_idx))
  not_on_server <- rep(FALSE, length(missing_idx))
  for (attempt in 1:3) {
    todo <- which(!fetched & !not_on_server)
    if (length(todo) == 0) break
    if (attempt > 1) Sys.sleep(2)
    ftp_results <- curl::multi_download(
      ftp_urls[todo], part_paths[todo],
      resume = FALSE, progress = !quiet, timeout = 600
    )
    fetched[todo] <- ftp_results$success %in% TRUE &
      file.exists(part_paths[todo])
    # a 550 means the file is not on the server; retrying cannot help
    not_on_server[todo] <- !fetched[todo] &
      grepl("550", ftp_results$error)
  }

  # fall back to https for anything the ftp mirror could not deliver
  retry_idx <- which(!fetched)
  if (length(retry_idx) > 0) {
    unlink(part_paths[retry_idx])
    requests <- lapply(urls[missing_idx][retry_idx], function(url) {
      httr2::request(url) |>
        httr2::req_timeout(600) |>
        httr2::req_retry(max_tries = 3, backoff = ~ 5)
    })
    responses <- httr2::req_perform_parallel(
      requests,
      paths = part_paths[retry_idx],
      max_active = max_active,
      on_error = "continue",
      progress = !quiet
    )
    fetched[retry_idx] <- vapply(
      responses,
      function(response) inherits(response, "httr2_response"),
      logical(1)
    )
  }

  file.rename(part_paths[fetched], dests[missing_idx][fetched])
  unlink(part_paths[!fetched])

  file.exists(dests)
}

#' Download Population Rasters from WorldPop
#'
#' Downloads population raster files from WorldPop for specified countries and
#' years. Automatically selects the appropriate dataset based on years:
#' legacy (2000-2020) or R2025A (2015-2030).
#'
#' @param country_codes Character vector of ISO country codes (e.g., "GBR",
#'   "GIN"). Pass `"GLOBAL"` (case-insensitive) to download the worldwide
#'   mosaic from the `0_Mosaicked` directory instead of a per-country
#'   raster. `"GLOBAL"` only works for years >= 2015 at 1km resolution and
#'   `type = "count"`.
#' @param years Numeric vector of years to download data for (2000-2030).
#' @param type Character; either "density" for persons per sq km or "count" for
#'   total population count (default: "count"). Note: density is only available
#'   for years 2000-2020.
#' @param resolution Character; either "1km" (default) or "100m". The 100m
#'   resolution is only available for years 2015-2030 and is not available
#'   for the GLOBAL mosaic.
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

  # global mosaic only for r2025a years, count, and 1km
  has_global <- any(toupper(country_codes) == "GLOBAL")
  if (has_global) {
    if (length(legacy_years) > 0) {
      cli::cli_abort(c(
        "{.val GLOBAL} mosaic only available for years >= 2015",
        "i" = "Drop legacy years or use ISO3 country codes for them"
      ))
    }
    if (resolution != "1km") {
      cli::cli_abort(c(
        "{.val GLOBAL} mosaic only available at 1km resolution",
        "i" = "Use {.code resolution = '1km'} when requesting GLOBAL"
      ))
    }
    if (type != "count") {
      cli::cli_abort(c(
        "{.val GLOBAL} mosaic only available for {.val count} rasters",
        "i" = "Density is legacy-only and has no global mosaic"
      ))
    }
    if (!quiet) {
      cli::cli_alert_warning(
        "{.val GLOBAL} downloads each year as a worldwide raster (~290 MB per year)"
      )
    }
  }

  # normalise codes: keep "GLOBAL" (case-insensitive); uppercase ISO3 otherwise
  country_codes <- vapply(
    country_codes,
    function(c) if (toupper(c) == "GLOBAL") "GLOBAL" else toupper(c),
    character(1)
  )

  if (!dir.exists(dest_dir)) dir.create(dest_dir, recursive = TRUE)

  all_results <- list()
  all_params <- list()


  # download legacy years (< 2015)
  if (length(legacy_years) > 0) {
    if (!quiet) {
      cli::cli_alert_info(
        "Using legacy dataset (2000-2020 UN-adjusted) for years: {.val {legacy_years}}"
      )
    }
    legacy_res <- .download_worldpop_batch(
      country_codes, legacy_years, type, "legacy", "1km", dest_dir, quiet
    )
    all_results <- c(all_results, legacy_res$results)
    all_params <- c(all_params, list(legacy_res$params))
  }

  # download r2025a years (>= 2015)
  if (length(r2025a_years) > 0) {
    if (!quiet) {
      cli::cli_alert_info(
        "Using R2025A dataset (2015-2030 constrained) for years: {.val {r2025a_years}}"
      )
    }
    r2025a_res <- .download_worldpop_batch(
      country_codes, r2025a_years, type, "r2025a", resolution, dest_dir, quiet
    )
    all_results <- c(all_results, r2025a_res$results)
    all_params <- c(all_params, list(r2025a_res$params))
  }

  # combine params for counting
  combined_params <- do.call(rbind, all_params)
  counts <- tapply(combined_params$success, combined_params$country, sum)

  if (!quiet) {
    cli::cli_alert_success(
      "Download of WorldPop {type} rasters is complete!"
    )
    for (cc in names(counts)) {
      cli::cli_alert_info(
        glue::glue("{cc}: {counts[[cc]]} of {length(years)} years downloaded")
      )
    }
  }

  downloaded_files <- unlist(all_results)
  invisible(list(
    files = downloaded_files[!is.na(downloaded_files)],
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

  url_infos <- Map(
    function(cc, yr) .build_worldpop_url(cc, yr, type, dataset, resolution),
    params$country,
    params$year
  )
  urls <- vapply(url_infos, function(info) info$url, character(1))
  dests <- file.path(
    dest_dir,
    vapply(url_infos, function(info) info$filename, character(1))
  )

  cached <- file.exists(dests)
  if (!quiet && any(cached)) {
    cli::cli_alert_info("Exists: {.file {basename(dests[cached])}}")
  }

  success <- .worldpop_fetch(urls, dests, quiet)
  if (any(!success)) {
    n_failed <- sum(!success)
    cli::cli_alert_warning(paste0(
      "Failed to download {n_failed} file{?s}: ",
      "{.file {basename(dests[!success])}}"
    ))
  }
  params$success <- success

  list(results = ifelse(success, dests, NA_character_), params = params)
}

# internal helper to build worldpop age-band URL and local filename
# release ("R2024B" or "R2025A") only applies for year >= 2015; ignored for
# legacy years. cc may be an ISO3 code or the literal "GLOBAL" to target the
# worldwide mosaic under 0_Mosaicked.
#' @noRd
.build_worldpop_age_band_url <- function(
    cc, sex, code, year, resolution, release = "R2025A") {

  is_global <- toupper(cc) == "GLOBAL"

  if (year < 2015) {
    # legacy 2000-2020: 1km only, unconstrained, m/f only, no global mosaic
    if (is_global) {
      cli::cli_abort(
        "{.val GLOBAL} mosaic not available for years < 2015"
      )
    }
    if (resolution != "1km") {
      cli::cli_abort(
        "100m resolution not available for years < 2015"
      )
    }
    if (sex == "t") {
      cli::cli_abort(
        "{.val t} (total) variant not available for years < 2015"
      )
    }

    cc_lo <- tolower(cc)
    cc_up <- toupper(cc)
    url <- sprintf(
      paste0(
        "https://data.worldpop.org/GIS/AgeSex_structures/",
        "Global_2000_2020_1km/unconstrained/%d/%s/%s_%s_%s_%d_1km.tif"
      ),
      year, cc_up, cc_lo, sex, code, year
    )
    local_fn <- sprintf("%s_%s_%s_%d_1km.tif", cc_lo, sex, code, year)
    return(list(url = url, filename = local_fn))
  }

  # year >= 2015: R2024B (unconstrained, UC) or R2025A (constrained, CN)
  if (is_global && resolution != "1km") {
    cli::cli_abort(
      "{.val GLOBAL} mosaic only available at 1km resolution"
    )
  }

  if (release == "R2025A") {
    constraint <- "constrained"
    constraint_tag <- "CN"
  } else {
    constraint <- "unconstrained"
    constraint_tag <- "UC"
  }

  if (resolution == "1km") {
    res_path <- "1km_ua"
    res_suffix <- "1km"
    ua_suffix <- "_UA"
  } else {
    res_path <- "100m"
    res_suffix <- "100m"
    ua_suffix <- ""
  }

  if (is_global) {
    cc_lo <- "global"
    country_segment <- "0_Mosaicked"
  } else {
    cc_lo <- tolower(cc)
    country_segment <- toupper(cc)
  }

  # age band code uses leading zero for r2024b/r2025a filenames
  code_str <- sprintf("%02d", code)
  remote_fn <- sprintf(
    "%s_%s_%s_%d_%s_%s_%s%s_v1.tif",
    cc_lo, sex, code_str, year, constraint_tag,
    res_suffix, release, ua_suffix
  )
  url <- sprintf(
    paste0(
      "https://data.worldpop.org/GIS/AgeSex_structures/",
      "Global_2015_2030/%s/%d/%s/v1/%s/%s/%s"
    ),
    release, year, country_segment, res_path, constraint, remote_fn
  )
  # local filename drops release tag (keeps cache stable across release flips)
  local_fn <- sprintf(
    "%s_%s_%s_%d_%s_%s%s_v1.tif",
    cc_lo, sex, code_str, year, constraint_tag, res_suffix, ua_suffix
  )

  list(url = url, filename = local_fn)
}

# internal helper: build urls + dests for one (cc, yr) combo's band files
# across bands * sexes
#' @noRd
.age_band_file_set <- function(
    cc, yr, matching_bands, sexes, resolution, release, out_dir) {

  combos <- expand.grid(
    code = matching_bands$code,
    sex = sexes,
    stringsAsFactors = FALSE
  )
  url_infos <- Map(
    function(band_code, sx) {
      .build_worldpop_age_band_url(cc, sx, band_code, yr, resolution, release)
    },
    combos$code,
    combos$sex
  )

  list(
    urls = vapply(url_infos, function(info) info$url, character(1)),
    dests = file.path(
      out_dir,
      vapply(url_infos, function(info) info$filename, character(1))
    )
  )
}

# internal helper: fetch every combo's band files in one parallel batch.
# repairs corrupt cached files first so they are re-downloaded. returns a
# logical per combo, TRUE when all of that combo's files are present.
#' @noRd
.fetch_age_band_files <- function(plan_files, quiet) {
  all_urls <- unlist(lapply(plan_files, function(file_set) file_set$urls))
  all_dests <- unlist(lapply(plan_files, function(file_set) file_set$dests))

  # repair cached files if corrupt
  for (cached_file in unique(all_dests[file.exists(all_dests)])) {
    valid <- tryCatch(
      { suppressWarnings(terra::rast(cached_file)); TRUE },
      error = function(e) FALSE
    )
    if (!valid) {
      cli::cli_alert_warning(
        "Corrupt file {basename(cached_file)}, re-downloading"
      )
      unlink(cached_file)
    }
  }

  # dedupe so the same file is never fetched twice concurrently
  unique_idx <- !duplicated(all_dests)
  success <- .worldpop_fetch(
    all_urls[unique_idx], all_dests[unique_idx], quiet
  )
  ok_by_dest <- stats::setNames(success, all_dests[unique_idx])

  vapply(
    plan_files,
    function(file_set) all(ok_by_dest[file_set$dests]),
    logical(1)
  )
}

#' Download WorldPop Population Raster Data for Specific Age Bands
#'
#' Downloads WorldPop population rasters for the specified age bands and
#' years. When `sex = "both"` and `year >= 2015`, downloads the pre-summed
#' `"t"` (total) raster instead of fetching male and female separately and
#' summing locally. Automatically selects legacy (2000-2014) or R2025A /
#' R2024B (2015-2030) dataset based on year.
#'
#' @param country_codes Character vector. ISO3 country codes (e.g., "TUN",
#'   "BDI"). Pass `"GLOBAL"` (case-insensitive) to download the worldwide
#'   mosaic from the `0_Mosaicked` directory instead of a per-country
#'   raster. `"GLOBAL"` only works for years >= 2015 at 1km resolution.
#' @param years Numeric vector. Years for which to download data (2000-2030).
#' @param age_range Numeric vector of length 2 specifying the lower and upper
#'   age range bounds (e.g., c(1, 9) for ages 1-9). Default: c(1, 9).
#' @param sex Character. Which sex to download: "both" (default, total
#'   population), "m" (male only), or "f" (female only). For years >= 2015
#'   `"both"` is satisfied with the single `t` (total) variant; for years
#'   < 2015 male and female are downloaded separately and summed locally.
#' @param resolution Character. Either "1km" (default) or "100m". The 100m
#'   resolution is only available for years >= 2015 and is not available for
#'   the GLOBAL mosaic.
#' @param release Character. WorldPop release for years >= 2015: "R2025A"
#'   (default, constrained / built-up areas only) or "R2024B" (unconstrained,
#'   covers all populated areas via interpolation). Ignored for years < 2015
#'   (the legacy dataset is always used). The two releases answer different
#'   methodological questions and are not directly comparable.
#' @param out_dir Character string. Directory where downloaded files will be
#'   saved. Default: ".".
#' @param quiet Logical; if TRUE, suppresses progress messages
#'   (default: FALSE).
#'
#' @details
#' ## Data Source
#' The function automatically selects the appropriate dataset:
#'
#' ### Legacy Dataset (years < 2015)
#' - URL: https://data.worldpop.org/GIS/AgeSex_structures/Global_2000_2020_1km/
#' - Resolution: 1km only
#' - Sex variants: `m`, `f` only (no `t` total available)
#' - Files: `{iso3}_{sex}_{code}_{year}_1km.tif`
#'
#' ### R2025A Dataset (years >= 2015, default)
#' - URL: https://data.worldpop.org/GIS/AgeSex_structures/Global_2015_2030/R2025A/
#' - Constrained to built-up areas
#' - Resolution: 1km or 100m
#' - Sex variants: `m`, `f`, `t` (total)
#' - Files: `{iso3}_{sex}_{code}_{year}_CN_{res}_R2025A{_UA}_v1.tif`
#'
#' ### R2024B Dataset (years >= 2015, optional)
#' - URL: https://data.worldpop.org/GIS/AgeSex_structures/Global_2015_2030/R2024B/
#' - Unconstrained
#' - Resolution: 1km or 100m
#' - Sex variants: `m`, `f`, `t` (total)
#' - Files: `{iso3}_{sex}_{code}_{year}_UC_{res}_R2024B{_UA}_v1.tif`
#'
#' ### Global Mosaic
#' Passing `country_codes = "GLOBAL"` swaps the `{ISO3}` URL segment for
#' `0_Mosaicked` and uses `global_` instead of an ISO3 prefix in the
#' filename. Available for years >= 2015 at 1km only. The worldwide files
#' are large (R2025A: ~280 MB, R2024B: ~650 MB per band).
#'
#' ## Band Combination Logic
#' If the exact age range is not covered by a single WorldPop band, the
#' function combines adjacent bands. For example:
#' - Requesting ages 2-9 will combine bands 1-4 and 5-9
#' - Requesting ages 0-10 will combine bands 0-1, 1-4, 5-9, and 10-14
#'
#' ## Age Bands
#' Available bands: 0, 1-4, 5-9, 10-14, 15-19, 20-24, 25-29, 30-34, 35-39,
#' 40-44, 45-49, 50-54, 55-59, 60-64, 65-69, 70-74, 75-79, 80+
#'
#' @return No return value. Files saved to output directory with pattern:
#'   `{iso3}_{sex}_{lower}_{upper}_{year}.tif` where sex is "total", "m",
#'   or "f", and upper is "80plus" when the 80+ band is included. For
#'   GLOBAL requests, `iso3` is `global`.
#'
#' @examples
#' \dontrun{
#' # Download age 1-9 data for Burundi (both sexes; uses R2025A 't' variant)
#' download_worldpop_age_band(
#'   country_codes = "BDI",
#'   years = 2020:2024,
#'   age_range = c(1, 9),
#'   out_dir = "data/worldpop"
#' )
#'
#' # Download female-only age 1-9 data
#' download_worldpop_age_band(
#'   country_codes = "BDI",
#'   years = 2020:2024,
#'   age_range = c(1, 9),
#'   sex = "f",
#'   out_dir = "data/worldpop"
#' )
#'
#' # Use R2024B unconstrained release instead of the R2025A default
#' download_worldpop_age_band(
#'   country_codes = "BDI",
#'   years = 2020,
#'   release = "R2024B",
#'   out_dir = "data/worldpop"
#' )
#'
#' # Download the worldwide mosaic (large file, ~280 MB per band)
#' download_worldpop_age_band(
#'   country_codes = "GLOBAL",
#'   years = 2020,
#'   age_range = c(0, 4),
#'   out_dir = "data/worldpop"
#' )
#' }
#' @export
download_worldpop_age_band <- function(
    country_codes,
    years,
    age_range = c(1, 9),
    sex = "both",
    resolution = "1km",
    release = "R2025A",
    out_dir = ".",
    quiet = FALSE) {

  sex <- match.arg(sex, choices = c("both", "m", "f"))
  resolution <- match.arg(resolution, choices = c("1km", "100m"))
  release <- match.arg(release, choices = c("R2025A", "R2024B"))

  # 100m only available for years >= 2015
  legacy_years <- years[years < 2015]
  if (resolution == "100m" && length(legacy_years) > 0) {
    cli::cli_abort(c(
      "100m resolution only available for years >= 2015",
      "i" = "Use {.code resolution = '1km'} for years < 2015"
    ))
  }

  # global mosaic only for years >= 2015 and only at 1km
  has_global <- any(toupper(country_codes) == "GLOBAL")
  if (has_global) {
    if (length(legacy_years) > 0) {
      cli::cli_abort(c(
        "{.val GLOBAL} mosaic only available for years >= 2015",
        "i" = "Drop legacy years or use ISO3 country codes for them"
      ))
    }
    if (resolution != "1km") {
      cli::cli_abort(c(
        "{.val GLOBAL} mosaic only available at 1km resolution",
        "i" = "Use {.code resolution = '1km'} when requesting GLOBAL"
      ))
    }
    if (!quiet) {
      cli::cli_alert_warning(
        paste0(
          "{.val GLOBAL} downloads each band as a worldwide raster ",
          "(R2025A: ~280 MB, R2024B: ~650 MB per band)"
        )
      )
    }
  }

  base::dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  bands <- base::data.frame(
    code = c(0, 1, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80),
    lower = c(0, 1, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80),
    upper = c(1, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, Inf)
  )

  sex_label <- if (sex == "both") "total" else sex

  # normalise codes: keep "GLOBAL" (case-insensitive); uppercase ISO3 otherwise
  country_codes <- vapply(
    country_codes,
    function(c) if (toupper(c) == "GLOBAL") "GLOBAL" else toupper(c),
    character(1)
  )

  combos <- expand.grid(
    country_code = country_codes,
    year = years,
    stringsAsFactors = FALSE
  )

  # planning pass: work out bands, output file, and variant per combo
  plans <- list()
  for (i in seq_len(nrow(combos))) {
    cc <- combos$country_code[i]
    yr <- combos$year[i]
    cc_lo <- tolower(cc)

    # inform user which dataset
    if (!quiet) {
      if (yr < 2015) {
        cli::cli_alert_info("Using legacy dataset (1km) for {cc}, {yr}")
      } else {
        cli::cli_alert_info(
          "Using {release} dataset ({resolution}) for {cc}, {yr}"
        )
      }
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
      "plus",
      as.character(matching_bands$upper - 1)
    )
    band_labels <- paste(matching_bands$lower, adjusted_upper, sep = "-")

    covered_lower <- min(matching_bands$lower)
    has_80plus <- any(matching_bands$upper == Inf)
    covered_upper_num <- max(
      matching_bands$upper[matching_bands$upper != Inf] - 1,
      -Inf
    )

    if (!has_80plus && covered_upper_num < age_range[2]) {
      if (!quiet) {
        cli::cli_alert_warning(
          paste0(
            "Requested {age_range[1]}-{age_range[2]} not fully covered. ",
            "Downloading {covered_lower}-{covered_upper_num} instead."
          )
        )
      }
    } else if (!quiet) {
      cli::cli_alert_info(
        "Combining bands: {paste(band_labels, collapse = ', ')}"
      )
    }

    upper_str <- if (has_80plus) "80plus" else sprintf("%02d", covered_upper_num)
    out_fname <- file.path(
      out_dir,
      sprintf(
        "%s_%s_%02d_%s_%d.tif",
        cc_lo, sex_label, covered_lower, upper_str, yr
      )
    )

    if (file.exists(out_fname)) {
      if (!quiet) cli::cli_alert_info(
        "Skipping {basename(out_fname)} (already exists)"
      )
      next
    }

    plans[[length(plans) + 1]] <- list(
      cc = cc, yr = yr, out_fname = out_fname,
      matching_bands = matching_bands,
      use_total = sex == "both" && yr >= 2015
    )
  }

  if (length(plans) == 0) {
    return(invisible(NULL))
  }

  sex_set <- if (sex == "both") c("m", "f") else sex

  # round 1: fetch all combos' files in one parallel batch. eligible combos
  # try the 't' (total) variant first — halves downloads and skips the
  # local m+f sum
  plan_files <- lapply(plans, function(plan) {
    plan_sexes <- if (plan$use_total) "t" else sex_set
    .age_band_file_set(
      plan$cc, plan$yr, plan$matching_bands, plan_sexes,
      resolution, release, out_dir
    )
  })
  fetched <- .fetch_age_band_files(plan_files, quiet)

  # round 2: fall back to m+f where the 't' variant is unavailable
  retry_idx <- which(
    vapply(plans, function(plan) plan$use_total, logical(1)) & !fetched
  )
  if (length(retry_idx) > 0) {
    for (i in retry_idx) {
      if (!quiet) {
        cli::cli_alert_warning(paste0(
          "Total ('t') variant unavailable for {plans[[i]]$cc} ",
          "{plans[[i]]$yr}; falling back to m+f"
        ))
      }
    }
    plan_files[retry_idx] <- lapply(plans[retry_idx], function(plan) {
      .age_band_file_set(
        plan$cc, plan$yr, plan$matching_bands, sex_set,
        resolution, release, out_dir
      )
    })
    fetched[retry_idx] <- .fetch_age_band_files(plan_files[retry_idx], quiet)
  }

  # assemble: sum each combo's band files locally and write its output
  for (i in seq_along(plans)) {
    plan <- plans[[i]]
    if (!fetched[i]) {
      cli::cli_alert_danger(
        "Failed to download age-band files for {plan$cc}, {plan$yr}; skipping"
      )
      next
    }

    acc <- NULL
    for (band_file in plan_files[[i]]$dests) {
      band_raster <- suppressWarnings(terra::rast(band_file))
      acc <- if (is.null(acc)) band_raster else acc + band_raster
    }

    suppressWarnings(
      terra::writeRaster(acc, plan$out_fname, overwrite = TRUE)
    )
    if (!quiet) cli::cli_alert_success("Written: {basename(plan$out_fname)}")
  }
}

# build the expected output filename for an age band raster
#' @noRd
.build_age_band_filename <- function(cc, year, age_range, sex = "both") {
  cc_lo <- tolower(cc)
  sex_label <- if (sex == "both") "total" else sex

  bands <- data.frame(
    lower = c(0, 1, 5, 10, 15, 20, 25, 30, 35,
              40, 45, 50, 55, 60, 65, 70, 75, 80),
    upper = c(1, 5, 10, 15, 20, 25, 30, 35, 40,
              45, 50, 55, 60, 65, 70, 75, 80, Inf)
  )

  matching <- subset(
    bands, lower < (age_range[2] + 1) & upper > age_range[1]
  )
  covered_lower <- min(matching$lower)
  has_80plus <- any(matching$upper == Inf)
  covered_upper_num <- max(
    matching$upper[matching$upper != Inf] - 1, -Inf
  )

  upper_str <- if (has_80plus) {
    "80plus"
  } else {
    sprintf("%02d", covered_upper_num)
  }

  sprintf(
    "%s_%s_%02d_%s_%d.tif",
    cc_lo, sex_label, covered_lower, upper_str, year
  )
}

#' Download WorldPop Rasters and Get Paths
#'
#' Downloads WorldPop population rasters for multiple age groups in one
#' call and returns file paths as a nested named list. Each group is a
#' named list keyed by year.
#'
#' @param country_code Character. Single ISO3 country code (e.g., "TGO"),
#'   or `"GLOBAL"` (case-insensitive) to download worldwide mosaics
#'   instead of per-country rasters. `"GLOBAL"` requires years >= 2015 and
#'   1km resolution.
#' @param years Numeric vector of years to download (2000-2030)
#' @param groups Named list of age group specifications. Each element
#'   should be a list with `age_range` (length-2 numeric vector or NULL
#'   for total population) and optionally `sex` ("both", "m", or "f").
#'   Default includes total, u5, and wra groups.
#' @param resolution Character. "1km" (default) or "100m".
#' @param release Character. WorldPop release for age-band downloads at
#'   years >= 2015: "R2025A" (default, constrained) or "R2024B"
#'   (unconstrained). Ignored for years < 2015 and for total-population
#'   downloads (which always use R2025A for years >= 2015).
#' @param dest_dir Character. Base directory for downloaded files.
#'   Total population goes to `dest_dir`, age bands go to
#'   `dest_dir/aged_rasters`.
#' @param download Logical. If TRUE (default), downloads rasters before
#'   building paths. Set FALSE to just get expected paths.
#' @param quiet Logical. If TRUE (default), suppresses per-file progress
#'   messages during download. A compact summary is always shown.
#'
#' @return Named list of groups, each a named list of file paths keyed
#'   by year.
#'
#' @examples
#' \dontrun{
#' # default groups: total, u5, wra
#' paths <- get_worldpop_paths(
#'   "TGO", years = c(2013, 2017),
#'   dest_dir = here::here("data/worldpop/raw")
#' )
#' paths$total$`2013`  # total population raster
#' paths$u5$`2017`     # under-5 raster
#' paths$wra$`2013`    # women of reproductive age raster
#'
#' # custom groups
#' paths <- get_worldpop_paths(
#'   "TGO", years = c(2013, 2017),
#'   groups = list(
#'     total = list(age_range = NULL),
#'     u5 = list(age_range = c(0, 4)),
#'     "5_9" = list(age_range = c(5, 9)),
#'     "10_19" = list(age_range = c(10, 19)),
#'     "20plus" = list(age_range = c(20, Inf)),
#'     wra = list(age_range = c(15, 49), sex = "f")
#'   ),
#'   dest_dir = here::here("data/worldpop/raw")
#' )
#' }
#' @export
get_worldpop_paths <- function(
    country_code,
    years,
    groups = list(
      total = list(age_range = NULL),
      u5 = list(age_range = c(0, 4)),
      wra = list(age_range = c(15, 49), sex = "f")
    ),
    resolution = "1km",
    release = "R2025A",
    dest_dir = here::here(),
    download = TRUE,
    quiet = TRUE) {

  release <- match.arg(release, choices = c("R2025A", "R2024B"))

  result <- list()

  for (group_name in names(groups)) {
    spec <- groups[[group_name]]
    age_range <- spec$age_range
    sex <- spec$sex %||% "both"

    # total pop goes to dest_dir, age bands to aged_rasters subdir
    if (is.null(age_range)) {
      out_dir <- dest_dir
    } else {
      out_dir <- file.path(dest_dir, "aged_rasters")
    }

    # download
    if (download) {
      if (is.null(age_range)) {
        download_worldpop(
          country_code, years,
          resolution = resolution, dest_dir = out_dir, quiet = quiet
        )
      } else {
        download_worldpop_age_band(
          country_code, years, age_range,
          sex = sex, resolution = resolution, release = release,
          out_dir = out_dir, quiet = quiet
        )
      }
    }

    # build year-keyed path list
    path_list <- lapply(years, function(yr) {
      if (is.null(age_range)) {
        dataset <- if (yr < 2015) "legacy" else "r2025a"
        fn <- .build_worldpop_url(
          country_code, yr, "count", dataset, resolution
        )$filename
      } else {
        fn <- .build_age_band_filename(
          country_code, yr, age_range, sex
        )
      }
      file.path(out_dir, fn)
    })

    result[[group_name]] <- stats::setNames(
      path_list, as.character(years)
    )
  }

  if (!quiet) {
    n_groups <- length(result)
    n_ready <- sum(vapply(result, function(paths) {
      all(vapply(paths, file.exists, logical(1)))
    }, logical(1)))
    group_names <- paste(names(result), collapse = ", ")
    cli::cli_alert_success(
      "Population rasters ready: {n_ready}/{n_groups} groups ({group_names})"
    )
  }

  result
}

# internal helper: list ISO3 codes available in the DUG tree for a year
# by scraping the server's directory listing (no mosaic product exists)
#' @noRd
.worldpop_dug_countries <- function(year, release, version) {
  listing_url <- sprintf(
    "https://data.worldpop.org/GIS/DUG/Global_2015_2030/%s/%s/%s/",
    release, version, year
  )
  listing_html <- httr2::request(listing_url) |>
    httr2::req_timeout(60) |>
    httr2::req_retry(max_tries = 3, backoff = ~ 5) |>
    httr2::req_perform() |>
    httr2::resp_body_string()

  matches <- regmatches(
    listing_html,
    gregexpr('href="[A-Z]{3}/"', listing_html)
  )[[1]]
  unique(substr(matches, 7, 9))
}

# internal helper to build worldpop DUG urbanicity url and filename
#' @noRd
.build_worldpop_urbanicity_url <- function(cc, year, layer, release, version) {
  base_url <- "https://data.worldpop.org/GIS/DUG/Global_2015_2030"
  cc_up <- toupper(cc)

  fn <- sprintf(
    "%s_DUG_%s_GRID_%s_%s_%s.tif",
    cc_up, year, layer, release, version
  )
  url <- sprintf(
    "%s/%s/%s/%s/%s/%s",
    base_url, release, version, year, cc_up, fn
  )

  list(url = url, filename = fn)
}

#' Download WorldPop DUG Urbanicity Rasters
#'
#' Downloads Degree of Urbanisation Grid (DUG) rasters from WorldPop for
#' the specified countries, years, and classification layers. Both L1
#' (DEGURBA 3-class) and L2 (sub-classified) layers are downloaded by
#' default.
#'
#' @param country_codes Character vector of ISO3 country codes (e.g.,
#'   "DZA", "GIN"). Case-insensitive; uppercased for URL building.
#'   Pass `"GLOBAL"` to download every country available on the server.
#'   Unlike other WorldPop products there is no worldwide DUG mosaic, so
#'   `"GLOBAL"` expands to per-country rasters for all available
#'   countries (~240 per year; files are small, roughly 50-500 KB each).
#' @param years Numeric vector of years to download (2015-2030).
#'   Default: 2015:2024 (full available historical range).
#' @param layers Character vector. Subset of `c("L1", "L2")`.
#'   Default: both. L1 is the DEGURBA 3-class scheme endorsed by the
#'   UN Statistical Commission (3 = urban centre, 2 = urban cluster,
#'   1 = rural). L2 is the finer-grained sub-classification.
#' @param release Character. WorldPop release tag. Default: "R2025A".
#' @param version Character. WorldPop version tag. Default: "v1".
#' @param dest_dir Destination directory for downloaded files
#'   (default: `here::here()`).
#' @param quiet Logical; if TRUE, suppresses per-file progress messages
#'   (default: FALSE).
#'
#' @return Invisible list containing:
#'   \itemize{
#'     \item files: Character vector of paths to downloaded/existing files
#'     \item counts: Named integer vector of successful files per country
#'   }
#'
#' @details
#' Files are pulled from
#' `https://data.worldpop.org/GIS/DUG/Global_2015_2030/{release}/{version}/{year}/{ISO3}/`
#' with filenames of the form
#' `{ISO3}_DUG_{year}_GRID_{layer}_{release}_{version}.tif`.
#'
#' Coverage on the WorldPop mirror is limited to **2015-2030**. Pre-2015
#' historical years and post-2030 projections from the underlying JRC /
#' Copernicus GHS-DUG product are hosted elsewhere and are out of scope
#' for this function.
#'
#' Existing files are skipped (idempotent). Per-file failures (e.g., a
#' transient 404 for one country/year combination) are soft-failed with
#' a warning so the rest of the batch completes.
#'
#' @examples
#' \dontrun{
#' # Download both L1 and L2 for Algeria, full historical range
#' download_worldpop_urbanicity("DZA")
#'
#' # Multiple countries, narrower year range, L1 only
#' download_worldpop_urbanicity(
#'   country_codes = c("DZA", "GIN"),
#'   years = 2020:2024,
#'   layers = "L1",
#'   dest_dir = here::here("data/worldpop/dug")
#' )
#'
#' # Every available country for one year (no mosaic exists, so GLOBAL
#' # expands to ~240 per-country rasters)
#' download_worldpop_urbanicity(
#'   country_codes = "GLOBAL",
#'   years = 2020,
#'   layers = "L1",
#'   dest_dir = here::here("data/worldpop/dug")
#' )
#' }
#' @export
download_worldpop_urbanicity <- function(
    country_codes,
    years = 2015:2024,
    layers = c("L1", "L2"),
    release = "R2025A",
    version = "v1",
    dest_dir = here::here(),
    quiet = FALSE) {

  if (!is.character(country_codes) || length(country_codes) == 0) {
    cli::cli_abort("{.arg country_codes} must be a non-empty character vector")
  }

  if (!all(layers %in% c("L1", "L2"))) {
    cli::cli_abort(c(
      "{.arg layers} must be a subset of {.val {c('L1', 'L2')}}",
      "i" = "Got: {.val {layers}}"
    ))
  }

  if (min(years) < 2015 || max(years) > 2030) {
    cli::cli_abort(c(
      "WorldPop DUG is only available for years 2015-2030",
      "i" = "Pre-2015 / post-2030 data lives on the JRC/Copernicus server"
    ))
  }

  if (!dir.exists(dest_dir)) {
    dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  }

  country_codes <- toupper(country_codes)

  # GLOBAL: no worldwide DUG mosaic exists, so expand to every country
  # available on the server for the first requested year
  if ("GLOBAL" %in% country_codes) {
    country_codes <- .worldpop_dug_countries(min(years), release, version)
    if (!quiet) {
      cli::cli_alert_warning(
        paste0(
          "No worldwide DUG mosaic exists; downloading all ",
          "{length(country_codes)} available countries instead"
        )
      )
    }
  }

  if (!quiet) {
    cli::cli_alert_info(
      paste0(
        "Downloading WorldPop DUG urbanicity rasters ",
        "({min(years)}-{max(years)}) for layers {.val {layers}}"
      )
    )
  }

  jobs <- tidyr::expand_grid(
    iso3 = country_codes,
    year = years,
    layer = layers
  )

  url_infos <- purrr::pmap(jobs, function(iso3, year, layer) {
    .build_worldpop_urbanicity_url(iso3, year, layer, release, version)
  })
  urls <- vapply(url_infos, function(info) info$url, character(1))
  dests <- file.path(
    dest_dir,
    vapply(url_infos, function(info) info$filename, character(1))
  )

  cached <- file.exists(dests)
  if (!quiet && any(cached)) {
    cli::cli_alert_info("Exists: {.file {basename(dests[cached])}}")
  }

  success <- .worldpop_fetch(urls, dests, quiet)
  if (any(!success)) {
    n_failed <- sum(!success)
    cli::cli_alert_warning(paste0(
      "Failed to download {n_failed} DUG file{?s}: ",
      "{.file {basename(dests[!success])}}"
    ))
  }
  results <- ifelse(success, dests, NA_character_)

  jobs$success <- success
  counts <- tapply(jobs$success, jobs$iso3, sum)

  if (!quiet) {
    cli::cli_alert_success(
      "Download of WorldPop DUG urbanicity rasters is complete!"
    )
    total_per_country <- length(years) * length(layers)
    for (cc in names(counts)) {
      cli::cli_alert_info(
        glue::glue(
          "{cc}: {counts[[cc]]} of {total_per_country} files downloaded"
        )
      )
    }
  }

  invisible(list(
    files = results[!is.na(results)],
    counts = counts
  ))
}
