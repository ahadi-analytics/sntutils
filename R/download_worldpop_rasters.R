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

#' Download and Process WorldPop Age Range Raster Data with Fractional Age
#' Bands
#'
#' This function downloads WorldPop age/sex structure raster data and processes
#' it to extract precise age ranges by calculating fractional contributions from
#' overlapping age bands. For example, to get ages 2-10, it will take partial
#' contributions from the 1-5 and 5-10 bands.
#'
#' @param iso3 Character string. Three-letter ISO country code (e.g., "TUN")
#' @param years Numeric vector. Years for which to download data. Note that
#'   currently only 2020 data is available but will be used as base for other
#'   years
#' @param age_range Numeric vector of length 2 specifying the \[lower, upper\]
#'   age range bounds. Default: c(2, 10)
#' @param out_dir Character string. Directory where downloaded and processed
#'   files will be saved. Default is current directory (".")
#'
#' @details
#' ## Data Source
#' This function connects to the WorldPop Unconstrained 1km Age-Sex Structured
#' Population dataset, publicly available at:
#'
#' "https://data.worldpop.org/GIS/AgeSex_structures/
#' Global_2000_2020_1km/unconstrained"
#'
#' ## Overview
#' The function processes data in several steps:
#' 1. Identifies which WorldPop age bands overlap with the requested age range
#' 2. Calculates the fraction needed from each overlapping band
#'    (e.g., for age 2, it takes 3/4 of the 1-5 band)
#' 3. Downloads 1km resolution raster files for required age bands and both
#'   sexes
#' 4. Applies calculated fractions to each band
#' 5. Combines all bands and both sexes into a single raster
#'
#' The age bands in WorldPop data are:
#' 0-1, 1-5, 5-10, 10-15, ..., 75-80, 80+
#'
#' Files are cached in the output directory to avoid repeated downloads. The
#' function will use cached files if they exist.
#'
#' @return No return value. Files are saved with naming pattern:
#'   \code{iso3_lo_total_covered_lower_covered_upper_yr.tif} where:
#'   - iso3_lo: lowercase country code (e.g. "tun")
#'   - covered_lower: lower age bound with leading zeros (e.g. "02")
#'   - covered_upper: upper age bound with leading zeros (e.g. "10")
#'   - yr: year (e.g. "2020")
#'
#' @examples
#' \dontrun{
#' # Download Tunisia data for ages 2-10 for year 2020
#' # This will use fractions of the 1-5, 5-10, and 10-15 bands
#' download_fractional_age_range_raster(
#'   "TUN", 2020, c(2, 10), "data/worldpop"
#' )
#'
#' # Download data for ages 0-15 (exact band boundaries)
#' download_fractional_age_range_raster(
#'   "TUN", 2020:2025, c(0, 15), "data/worldpop"
#' )
#' }
#' @export
download_fractional_age_range_raster <- function(
    iso3,
    years,
    age_range = c(2, 10),
    out_dir = ".") {
  url_base <- paste0(
    "https://data.worldpop.org/GIS/AgeSex_structures/",
    "Global_2000_2020_1km/unconstrained"
  )

  iso3_up <- base::toupper(iso3)
  iso3_lo <- base::tolower(iso3)
  base::dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  bands <- base::data.frame(
    code = c(
      0,
      1,
      5,
      10,
      15,
      20,
      25,
      30,
      35,
      40,
      45,
      50,
      55,
      60,
      65,
      70,
      75,
      80
    ),
    lower = c(
      0,
      1,
      5,
      10,
      15,
      20,
      25,
      30,
      35,
      40,
      45,
      50,
      55,
      60,
      65,
      70,
      75,
      80
    ),
    upper = c(
      1,
      5,
      10,
      15,
      20,
      25,
      30,
      35,
      40,
      45,
      50,
      55,
      60,
      65,
      70,
      75,
      80,
      Inf
    )
  )

  sexes <- c("m", "f")

  make_url <- function(sex, code) {
    fixed_year <- "2020"
    fname <- base::sprintf(
      "%s_%s_%s_%s_1km.tif",
      iso3_lo,
      sex,
      code,
      fixed_year
    )
    base::file.path(url_base, fixed_year, iso3_up, fname)
  }

  for (yr in years) {
    out_fname <- base::file.path(
      out_dir,
      base::sprintf(
        "%s_total_%02d_%02d_%d.tif",
        iso3_lo,
        age_range[1],
        age_range[2],
        yr
      )
    )

    if (base::file.exists(out_fname)) {
      cli::cli_alert_info("Skipping {basename(out_fname)} (already exists)")
      next
    }

    sel <- base::subset(
      bands,
      upper > age_range[1] & lower < (age_range[2] + 1)
    )
    sel$fraction <- base::with(
      sel,
      (base::pmin(upper, age_range[2] + 1) - base::pmax(lower, age_range[1])) /
        (upper - lower)
    )

    acc <- NULL
    for (i in base::seq_len(base::nrow(sel))) {
      code <- sel$code[i]
      frac <- sel$fraction[i]

      for (sex in sexes) {
        url <- make_url(sex, code)
        temp_fname <- base::file.path(out_dir, base::basename(url))

        if (!base::file.exists(temp_fname)) {
          cli::cli_alert_info("Fetching {sex} band {code} ({yr})")
          utils::download.file(url, temp_fname, mode = "wb", quiet = TRUE)
        } else {
          cli::cli_alert_info("Using cached file {base::basename(temp_fname)}")
        }

        r <- terra::rast(temp_fname) * frac
        acc <- if (base::is.null(acc)) r else acc + r
      }
    }

    terra::writeRaster(acc, out_fname, overwrite = TRUE)
    cli::cli_alert_success("Written: {base::basename(out_fname)}")
  }
}

#' Download WorldPop Population Raster Data for Specific Age Bands
#'
#' This function downloads and processes WorldPop population raster data for
#' specified age bands and years. It combines male and female population data
#' for the requested age range.
#'
#' @param iso3 Character string. Three-letter ISO country code (e.g., "TUN"
#'     for Tunisia)
#' @param years Numeric vector. Years for which to download data (e.g.,
#'     c(2020, 2021))
#' @param age_range Numeric vector of length 2 specifying the \[lower, upper\]
#'   age range bounds. Default: c(1, 9)
#' @param out_dir Character string. Directory where downloaded files will be
#'        saved Default: "."
#'
#' @details
#' ## Data Source
#' This function connects to the WorldPop Unconstrained 1km Age-Sex Structured
#' Population dataset, publicly available at:
#'
#' "https://data.worldpop.org/GIS/AgeSex_structures/
#' Global_2000_2020_1km/unconstrained"
#'
#' - **Unconstrained**: Includes all populated areas, not limited to built-up
#'     areas.
#' - **1 km Resolution**: Approximately 1km x 1km pixel size at the equator.
#' - **Age-Sex Structured**: Population counts by sex (male/female) and broad
#'     age bands.
#' - **2020 Base Year**: All data references WorldPop's 2020 population
#'     estimates.
#'
#' ## Overview
#' This function:
#' 1. Connects to WorldPop's unconstrained 1km resolution population data
#' 2. Identifies all available age bands that overlap with the requested range
#' 3. Automatically **combines adjacent bands** when no single band fully covers
#'    the range
#' 4. Downloads and combines male and female population rasters for these bands
#' 5. Saves the combined raster for each requested year
#'
#' ## Band Combination Logic
#' If the exact age range is not covered by a single WorldPop band, the function:
#' - Finds **all bands that partially or fully cover any part** of the
#'     requested range.
#' - **Combines these bands together** into a single raster.
#' - **Informs the user** of the actual bands combined, and adjusts the output
#'     file name accordingly.
#'
#' For example:
#' - Requesting **ages 2-9** will combine **bands 1-4 and 5-9**.
#' - Requesting **ages 0-10** will combine **bands 0-1, 1-4, 5-9, and 10-14**.
#'
#' ## WorldPop File Naming and Band Mapping
#' WorldPop filenames follow the pattern:
#' `{iso3}_{sex}_{code}_2020_1km.tif`
#'
#' Where:
#' - `{iso3}` is the lowercase ISO3 country code (e.g., "tun" for Tunisia)
#' - `{sex}` is:
#'   - `"m"` for male population
#'   - `"f"` for female population
#' - `{code}` is the **starting age** of the band, defined as:
#'   - `"0"`  = ages **0**
#'   - `"1"`  = ages **1-4**
#'   - `"5"`  = ages **5-9**
#'   - `"10"` = ages **10-14**
#'   - `"15"` = ages **15-19**
#'   - `"20"` = ages **20-24**
#'   - `"25"` = ages **25-29**
#'   - `"30"` = ages **30-34**
#'   - `"35"` = ages **35-39**
#'   - `"40"` = ages **40-44**
#'   - `"45"` = ages **45-49**
#'   - `"50"` = ages **50-54**
#'   - `"55"` = ages **55-59**
#'   - `"60"` = ages **60-64**
#'   - `"65"` = ages **65-69**
#'   - `"70"` = ages **70-74**
#'   - `"75"` = ages **75-79**
#'   - `"80"` = ages **80+**
#'
#' ## Example Mappings
#' - `tun_m_1_2020_1km.tif`  = Tunisia, Male, Ages 1-4
#' - `tun_f_5_2020_1km.tif`  = Tunisia, Female, Ages 5-9
#' - `tun_m_10_2020_1km.tif` = Tunisia, Male, Ages 10-14
#'
#' @return No return value. Files are saved to the specified output directory
#'         with naming pattern: "tun_total_0_10_2010.tif" for Tunisia
#'        population between 0 and 10 for 2010.
#'
#' @examples
#' \dontrun{
#' # Download Tunisia data for ages 1-9 for years 2020-2021
#' download_nearest_age_band_raster(
#'   iso3 = "TUN",
#'   years = 2020:2021,
#'   age_range = c(0, 9),
#'   out_dir = "data/worldpop"
#' )
#' }
#' @export
download_nearest_age_band_raster <- function(
    iso3,
    years,
    age_range = c(1, 9),
    out_dir = ".") {
  url_base <- paste0(
    "https://data.worldpop.org/GIS/AgeSex_structures/",
    "Global_2000_2020_1km/unconstrained"
  )

  iso3_up <- base::toupper(iso3)
  iso3_lo <- base::tolower(iso3)
  base::dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  bands <- base::data.frame(
    code = c(
      0,
      1,
      5,
      10,
      15,
      20,
      25,
      30,
      35,
      40,
      45,
      50,
      55,
      60,
      65,
      70,
      75,
      80
    ),
    lower = c(
      0,
      1,
      5,
      10,
      15,
      20,
      25,
      30,
      35,
      40,
      45,
      50,
      55,
      60,
      65,
      70,
      75,
      80
    ),
    upper = c(
      1,
      5,
      10,
      15,
      20,
      25,
      30,
      35,
      40,
      45,
      50,
      55,
      60,
      65,
      70,
      75,
      80,
      Inf
    )
  )

  sexes <- c("m", "f")

  make_url <- function(sex, code) {
    base::file.path(
      url_base,
      "2020",
      iso3_up,
      base::sprintf("%s_%s_%s_2020_1km.tif", iso3_lo, sex, code)
    )
  }

  for (yr in years) {
    # Find bands that intersect the requested range
    matching_bands <- base::subset(
      bands,
      lower < (age_range[2] + 1) & upper > age_range[1]
    )

    if (base::nrow(matching_bands) == 0) {
      cli::cli_alert_danger(
        "No bands found covering {age_range[1]}-{age_range[2]}"
      )
      next
    }

    # Adjust upper bounds for accurate labeling (exclusive boundaries)
    adjusted_upper <- ifelse(
      matching_bands$upper == Inf,
      matching_bands$upper,
      matching_bands$upper - 1
    )
    band_labels <- paste(matching_bands$lower, adjusted_upper, sep = "-")

    # Determine combined bounds for naming
    covered_lower <- base::min(matching_bands$lower)
    covered_upper <- base::max(adjusted_upper)

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

    out_fname <- base::file.path(
      out_dir,
      base::sprintf(
        "%s_total_%02d_%02d_%d.tif",
        iso3_lo,
        covered_lower,
        covered_upper,
        yr
      )
    )

    if (base::file.exists(out_fname)) {
      cli::cli_alert_info("Skipping {basename(out_fname)} (already exists)")
      next
    }

    acc <- NULL
    for (band_code in matching_bands$code) {
      for (sex in sexes) {
        temp_fname <- base::file.path(
          out_dir,
          base::sprintf("%s_%s_%s_%d_1km.tif", iso3_lo, sex, band_code, yr)
        )

        if (!base::file.exists(temp_fname)) {
          cli::cli_alert_info(
            "Downloading {sex} band {band_code} for year {yr}"
          )
          utils::download.file(
            make_url(sex, band_code),
            temp_fname,
            mode = "wb",
            quiet = TRUE
          )
        } else {
          cli::cli_alert_info("Using cached file {base::basename(temp_fname)}")
        }

        r <- terra::rast(temp_fname)
        acc <- if (base::is.null(acc)) r else acc + r
      }
    }

    terra::writeRaster(acc, out_fname, overwrite = TRUE)
    cli::cli_alert_success("Written: {base::basename(out_fname)}")
  }
}
