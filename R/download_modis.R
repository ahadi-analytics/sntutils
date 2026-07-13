#' Look up MODIS scale factor and nodata value for a band
#'
#' Returns the scale factor and fill value for known MODIS
#' subdatasets. Unknown bands return `NULL` (no scaling).
#'
#' @param band Character. Full or partial subdataset name.
#' @return A list with `scale` and `nodata`, or `NULL`.
#' @noRd
.modis_scale_info <- function(band) {
  band_lower <- tolower(band)

  # known scale factors and fill values for common bands
  lookup <- list(
    evi   = list(scale = 0.0001, nodata = -3000),
    ndvi  = list(scale = 0.0001, nodata = -3000),
    lst_day   = list(scale = 0.02, nodata = 0),
    lst_night = list(scale = 0.02, nodata = 0),
    lst   = list(scale = 0.02, nodata = 0)
  )

  for (key in names(lookup)) {
    if (grepl(key, band_lower, fixed = TRUE)) {
      return(lookup[[key]])
    }
  }

  NULL
}

#' Extract short band name from a MODIS subdataset name
#'
#' @param band Character. Full subdataset name like
#'   `"1 km monthly EVI"`.
#' @return Character. Short lowercase name like `"evi"`.
#' @noRd
.clean_band_name <- function(band) {
  words <- regmatches(band, gregexpr("[A-Za-z]+", band))[[1]]
  filler <- c("km", "monthly", "daily", "yearly", "m")
  words <- words[!tolower(words) %in% filler]
  if (length(words) == 0) return("band")
  tolower(paste(words, collapse = "_"))
}

#' Parse date from a MODIS HDF filename
#'
#' Extracts year/month/day from the `AYYYYDDD` pattern embedded
#' in standard MODIS filenames.
#'
#' @param filename Character. HDF filename or full path.
#' @return A list with `year`, `month`, `day`, `date`.
#' @noRd
.parse_modis_date <- function(filename) {
  bn <- basename(filename)
  match <- regmatches(bn, regexpr("A\\d{7}", bn))

  if (length(match) == 0 || nchar(match) == 0) {
    return(list(
      year = NA_integer_, month = NA_integer_,
      day = NA_integer_, date = as.Date(NA)
    ))
  }

  yr  <- as.integer(substr(match, 2, 5))
  doy <- as.integer(substr(match, 6, 8))
  dt  <- as.Date(paste(yr, doy), format = "%Y %j")

  list(
    year  = yr,
    month = as.integer(format(dt, "%m")),
    day   = as.integer(format(dt, "%d")),
    date  = dt
  )
}

#' Search NASA CMR for MODIS tile download URLs
#'
#' @param product Character. MODIS short name.
#' @param version Character. Collection version.
#' @param start POSIXct. Start date.
#' @param end POSIXct. End date.
#' @param bbox Numeric length-4. `c(xmin, ymin, xmax, ymax)`.
#' @return Character vector of HDF download URLs.
#' @noRd
.search_cmr_granules <- function(product, version, start, end,
                                 bbox) {
  # CMR caps a single response at 2000 granules. Multi-year tiled
  # searches (e.g. MOD13A3 over a region spanning many MODIS tiles)
  # blow past that, so we paginate with CMR-Search-After — NASA's
  # recommended header-based scroll. Missing this silently truncated
  # NDVI downloads at ~2015.
  base_req <- httr2::request(
    "https://cmr.earthdata.nasa.gov/search/granules.json"
  ) |>
    httr2::req_url_query(
      short_name = product,
      version = version,
      `temporal[]` = paste0(
        format(start, "%Y-%m-%dT00:00:00Z"), ",",
        format(end, "%Y-%m-%dT23:59:59Z")
      ),
      bounding_box = paste(bbox, collapse = ","),
      page_size = 2000
    )

  all_entries <- list()
  search_after <- NULL

  repeat {
    req <- base_req
    if (!is.null(search_after)) {
      req <- req |>
        httr2::req_headers(`CMR-Search-After` = search_after)
    }

    resp <- httr2::req_perform(req)
    entries <- httr2::resp_body_json(resp)$feed$entry

    if (length(entries) == 0) break

    all_entries <- c(all_entries, entries)

    # CMR returns this header only while more results remain
    search_after <- httr2::resp_header(resp, "CMR-Search-After")
    if (is.null(search_after) || nchar(search_after) == 0) break
  }

  if (length(all_entries) == 0) return(character(0))

  # extract data download URLs (HDF files)
  urls <- vapply(all_entries, function(e) {
    links <- e$links
    data_links <- Filter(function(l) {
      grepl("data#", l$rel %||% "") &&
        grepl("\\.(hdf|HDF)$", l$href %||% "")
    }, links)
    if (length(data_links) > 0) {
      data_links[[1]]$href
    } else {
      NA_character_
    }
  }, character(1))

  urls[!is.na(urls)]
}

#' Download files from NASA Earthdata with authentication
#'
#' Uses a temporary netrc file for Earthdata Login auth. Runs
#' downloads concurrently via [httr2::req_perform_parallel()].
#' Files that already exist on disk are skipped unless
#' `overwrite = TRUE`.
#'
#' @param urls Character vector of download URLs.
#' @param dest_dir Character. Directory to save files.
#' @param username Character. Earthdata username.
#' @param password Character. Earthdata password.
#' @param overwrite Logical. Re-download existing files?
#' @param max_workers Integer. Maximum concurrent downloads.
#' @return Character vector of local file paths.
#' @noRd
.download_earthdata <- function(urls, dest_dir, username,
                                password, overwrite,
                                max_workers = 6) {
  netrc <- tempfile("netrc_")
  writeLines(
    sprintf(
      "machine urs.earthdata.nasa.gov login %s password %s",
      username, password
    ),
    netrc
  )
  on.exit(unlink(netrc), add = TRUE)

  dests <- file.path(dest_dir, basename(urls))

  # skip files already on disk unless overwrite = TRUE
  needs_download <- overwrite | !file.exists(dests)
  n_skipped <- sum(!needs_download)

  if (n_skipped > 0) {
    cli::cli_alert_info(
      "Skipping {n_skipped} already-downloaded HDF file(s)"
    )
  }

  urls_todo <- urls[needs_download]
  dests_todo <- dests[needs_download]

  if (length(urls_todo) == 0) return(dests)

  reqs <- lapply(urls_todo, function(u) {
    httr2::request(u) |>
      httr2::req_options(
        cookiefile = "",
        cookiejar = "",
        netrc = 1L,
        netrc_file = netrc
      ) |>
      httr2::req_error(
        body = function(resp) {
          paste("HTTP", httr2::resp_status(resp))
        }
      )
  })

  httr2::req_perform_parallel(
    reqs,
    paths = dests_todo,
    max_active = max_workers,
    progress = sprintf(
      "Downloading %d MODIS HDF file(s)", length(urls_todo)
    )
  )

  dests
}

#' Extract a subdataset from an HDF file by name
#'
#' Parses raw GDAL metadata to find `SUBDATASET_*_NAME` entries,
#' matches the requested `band`, and loads via the full GDAL path.
#' Falls back to `terra::describe(sds = TRUE)` if no SUBDATASET
#' lines are found.
#'
#' @param hdf_file Character. Path to HDF file.
#' @param band Character. Subdataset name to extract.
#' @return A `SpatRaster` (single layer).
#' @noRd
.extract_subdataset <- function(hdf_file, band) {
  # raw GDAL metadata contains SUBDATASET_N_NAME=<full path>
  info <- terra::describe(hdf_file)
  name_lines <- grep(
    "SUBDATASET_\\d+_NAME=", info, value = TRUE
  )

  if (length(name_lines) > 0) {
    idx <- grep(band, name_lines, ignore.case = TRUE)

    # keyword fallback (last word: "EVI", "NDVI", "LST")
    if (length(idx) == 0) {
      key <- sub(".*\\s", "", trimws(band))
      idx <- grep(key, name_lines, ignore.case = TRUE)
    }

    if (length(idx) > 0) {
      sds_path <- sub(
        ".*SUBDATASET_\\d+_NAME=", "", name_lines[idx[1]]
      )
      return(terra::rast(sds_path, raw = TRUE))
    }

    # band not found — show available names
    available <- sub(
      ".*SUBDATASET_\\d+_NAME=", "", name_lines
    )
    cli::cli_abort(c(
      "Band {.val {band}} not found in {.file {basename(hdf_file)}}.",
      "i" = "Available subdatasets:",
      stats::setNames(available, rep("*", length(available)))
    ))
  }

  # fallback: terra::describe(sds = TRUE) data.frame
  desc <- terra::describe(hdf_file, sds = TRUE)

  if (is.data.frame(desc) && nrow(desc) > 0) {
    char_cols <- vapply(desc, is.character, logical(1))
    search_text <- apply(
      desc[, char_cols, drop = FALSE], 1,
      paste, collapse = " "
    )

    idx <- grep(band, search_text, ignore.case = TRUE)
    if (length(idx) == 0) {
      key <- sub(".*\\s", "", trimws(band))
      idx <- grep(key, search_text, ignore.case = TRUE)
    }

    if (length(idx) > 0) {
      return(terra::rast(hdf_file, subds = idx[1], raw = TRUE))
    }
  }

  cli::cli_abort(
    "Could not read subdatasets from {.file {basename(hdf_file)}}."
  )
}

#' Mosaic multiple MODIS HDF tiles for one date
#'
#' @param hdf_files Character vector of HDF file paths.
#' @param band Character. Subdataset name to extract.
#' @return A `SpatRaster` (single layer, mosaicked).
#' @noRd
.mosaic_modis_tiles <- function(hdf_files, band) {
  tiles <- lapply(hdf_files, function(f) {
    .extract_subdataset(f, band)
  })
  if (length(tiles) == 1) return(tiles[[1]])
  terra::mosaic(terra::sprc(tiles))
}

#' List Available MODIS Products
#'
#' Queries NASA's Common Metadata Repository (CMR) for MODIS
#' products available from the LPDAAC archive. Returns a tibble
#' you can filter to find the `product` value needed by
#' [download_modis()].
#'
#' Requires internet access.
#'
#' @param search Character or `NULL`. Optional search term to
#'   filter results (case-insensitive). Searches product names
#'   and descriptions.
#'
#' @return A tibble with columns `product`, `version`, and
#'   `description`.
#'
#' @examples
#' \dontrun{
#' # list all MODIS products
#' modis_options()
#'
#' # search for vegetation index products
#' modis_options("vegetation")
#'
#' # search for land surface temperature
#' modis_options("LST")
#' }
#' @export
modis_options <- function(search = NULL) {
  rlang::check_installed(
    "httr2",
    reason = "to query NASA's product catalog"
  )

  resp <- httr2::request(
    "https://cmr.earthdata.nasa.gov/search/collections.json"
  ) |>
    httr2::req_url_query(
      provider = "LPDAAC_ECS",
      page_size = 500
    ) |>
    httr2::req_perform()

  entries <- httr2::resp_body_json(resp)$feed$entry

  if (length(entries) == 0) {
    cli::cli_warn("No products found.")
    return(tibble::tibble(
      product = character(),
      version = character(),
      description = character()
    ))
  }

  result <- tibble::tibble(
    product = vapply(
      entries, function(e) e$short_name %||% "",
      character(1)
    ),
    version = vapply(
      entries, function(e) e$version_id %||% "",
      character(1)
    ),
    description = vapply(
      entries, function(e) e$summary %||% "",
      character(1)
    )
  )

  # keep only MODIS products (MOD*, MYD*, MCD*)
  result <- result[grepl("^(MOD|MYD|MCD)", result$product), ]

  if (!is.null(search)) {
    pattern <- tolower(search)
    matches <- grepl(pattern, tolower(result$product)) |
      grepl(pattern, tolower(result$description))
    result <- result[matches, ]
  }

  result
}

#' Download MODIS Data from NASA
#'
#' Searches NASA's CMR catalog, downloads MODIS HDF files via
#' `httr2`, extracts the requested band, mosaics tiles, crops to
#' the area of interest, scales values, and saves one GeoTIFF per
#' time step. Output files are named `modis_{band}_{YYYY}_{MM}.tif`
#' and work directly with [process_raster_collection()].
#'
#' Use [modis_options()] to browse available products.
#'
#' @param shapefile An `sf` object defining the area of interest.
#'   Reprojected to WGS84 (EPSG:4326) for the NASA query.
#' @param start Character. Start date in any common format
#'   (e.g. `"2023-01-01"`).
#' @param end Character. End date in any common format.
#' @param product Character. MODIS product short name (default
#'   `"MOD13A3"` for monthly vegetation indices at 1 km).
#' @param band Character. HDF subdataset name to extract (default
#'   `"1 km monthly EVI"`). Use `terra::describe(hdf_file)` on a
#'   downloaded HDF to see available subdatasets.
#' @param out_dir Character. Output directory for GeoTIFFs
#'   (default `"."`). Created if it does not exist.
#' @param username Character or `NULL`. NASA Earthdata username.
#'   Falls back to `Sys.getenv("EARTHDATA_USERNAME")`.
#' @param password Character or `NULL`. NASA Earthdata password.
#'   Falls back to `Sys.getenv("EARTHDATA_PASSWORD")`.
#' @param version Character. MODIS collection version (default
#'   `"061"`).
#' @param scale_factor `"auto"`, numeric, or `NULL`. When `"auto"`
#'   (default), the scale factor is looked up from a built-in table
#'   of known MODIS bands (e.g. 0.0001 for EVI/NDVI, 0.02 for
#'   LST). Pass a number to override, or `NULL` to skip scaling.
#' @param keep_hdf Logical. Keep raw HDF files after processing?
#'   Default `FALSE` (deletes them). When `TRUE`, saved to
#'   `{out_dir}/hdf/`.
#' @param overwrite Logical. Re-download and reprocess existing
#'   files? Default `FALSE`.
#' @param max_workers Integer. Maximum concurrent HDF downloads.
#'   Defaults to `parallel::detectCores() - 1`. Note: MODIS
#'   downloads are network-bound, so more workers is not always
#'   faster — NASA LPDAAC throttles above ~8 concurrent
#'   connections. Cap manually if you see connection errors.
#'
#' @return Invisible character vector of output GeoTIFF paths.
#'
#' @examples
#' \dontrun{
#' boundary <- sf::st_read("guinea_adm0.geojson")
#'
#' # download monthly EVI for 2023
#' paths <- download_modis(
#'   shapefile = boundary,
#'   start     = "2023-01-01",
#'   end       = "2023-12-31",
#'   out_dir   = "data/evi",
#'   username  = "my_user",
#'   password  = "my_pass"
#' )
#'
#' # then use the existing raster processing pipeline
#' result <- process_raster_collection(
#'   directory    = "data/evi",
#'   shapefile    = sf::st_read("guinea_adm3.geojson"),
#'   id_cols      = c("adm0", "adm1", "adm2", "adm3"),
#'   aggregations = "mean"
#' )
#'
#' # download NDVI instead
#' download_modis(
#'   shapefile = boundary,
#'   start     = "2023-01-01",
#'   end       = "2023-12-31",
#'   band      = "1 km monthly NDVI",
#'   out_dir   = "data/ndvi"
#' )
#'
#' # keep raw HDF files for inspection
#' download_modis(
#'   shapefile = boundary,
#'   start     = "2023-01-01",
#'   end       = "2023-12-31",
#'   out_dir   = "data/evi",
#'   keep_hdf  = TRUE
#' )
#' }
#' @export
download_modis <- function(
    shapefile,
    start,
    end,
    product = "MOD13A3",
    band = "1 km monthly EVI",
    out_dir = ".",
    username = NULL,
    password = NULL,
    version = "061",
    scale_factor = "auto",
    keep_hdf = FALSE,
    overwrite = FALSE,
    max_workers = max(1L, parallel::detectCores() - 1L)) {

  rlang::check_installed(
    "httr2",
    reason = "to download MODIS data from NASA"
  )

  # ── validate inputs ───────────────────────────────────────
  if (!inherits(shapefile, "sf")) {
    cli::cli_abort(
      "{.arg shapefile} must be an {.cls sf} object."
    )
  }

  if (any(sf::st_is_empty(shapefile))) {
    cli::cli_abort(
      "{.arg shapefile} contains empty geometries."
    )
  }

  parsed_start <- lubridate::parse_date_time(
    start,
    orders = sntutils::available_date_formats,
    quiet = TRUE
  )
  parsed_end <- lubridate::parse_date_time(
    end,
    orders = sntutils::available_date_formats,
    quiet = TRUE
  )

  if (is.na(parsed_start)) {
    cli::cli_abort("Could not parse {.arg start} ({.val {start}}).")
  }
  if (is.na(parsed_end)) {
    cli::cli_abort("Could not parse {.arg end} ({.val {end}}).")
  }

  # ── credentials ───────────────────────────────────────────
  username <- username %||% Sys.getenv("EARTHDATA_USERNAME")
  password <- password %||% Sys.getenv("EARTHDATA_PASSWORD")

  if (nchar(username) == 0 || nchar(password) == 0) {
    cli::cli_abort(c(
      "NASA Earthdata credentials are missing.",
      "i" = paste(
        "Pass {.arg username}/{.arg password} or set",
        "{.envvar EARTHDATA_USERNAME} /",
        "{.envvar EARTHDATA_PASSWORD}."
      )
    ))
  }

  # ── directories ───────────────────────────────────────────
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
  }

  # HDF cache survives across runs so that an interrupted
  # download can resume — cleanup happens only after a full
  # successful run (see end of function) when keep_hdf = FALSE.
  hdf_dir <- if (keep_hdf) {
    file.path(out_dir, "hdf")
  } else {
    file.path(out_dir, ".hdf_cache")
  }
  dir.create(hdf_dir, recursive = TRUE, showWarnings = FALSE)

  # ── search CMR ────────────────────────────────────────────
  # sf::st_bbox → c(xmin, ymin, xmax, ymax) — same as CMR
  bbox <- as.numeric(
    sf::st_bbox(sf::st_transform(shapefile, 4326))
  )

  cli::cli_alert_info(
    "Searching NASA for {product} tiles..."
  )

  urls <- .search_cmr_granules(
    product, version, parsed_start, parsed_end, bbox
  )

  if (length(urls) == 0) {
    cli::cli_warn("No files found for the given parameters.")
    return(invisible(character(0)))
  }

  cli::cli_alert_success("Found {length(urls)} tile(s)")

  # ── skip months whose GeoTIFF already exists ──────────────
  # parse dates from URLs so we can filter before downloading
  band_clean <- .clean_band_name(band)
  url_yearmons <- vapply(urls, function(u) {
    d <- .parse_modis_date(u)
    sprintf("%04d_%02d", d$year, d$month)
  }, character(1))

  existing_paths <- character(0)
  done_ym <- character(0)

  for (ym in unique(url_yearmons)) {
    ym_compact <- gsub("_", "", ym)
    out_name <- sprintf(
      "modis_%s_%s.tif", band_clean, ym_compact
    )
    out_path <- file.path(out_dir, out_name)
    if (file.exists(out_path) && !overwrite) {
      cli::cli_alert_info(
        "Skipping {out_name} (already exists)"
      )
      existing_paths <- c(existing_paths, out_path)
      done_ym <- c(done_ym, ym)
    }
  }

  urls_todo <- urls[!url_yearmons %in% done_ym]

  if (length(urls_todo) == 0) {
    if (!keep_hdf) unlink(hdf_dir, recursive = TRUE)
    cli::cli_alert_success(
      "Done. {length(existing_paths)} GeoTIFF(s) in {.path {out_dir}}"
    )
    return(invisible(existing_paths))
  }

  # ── download HDF files ────────────────────────────────────
  cli::cli_alert_info(
    "Downloading {length(urls_todo)} HDF file(s) with {max_workers} worker(s)..."
  )

  hdf_files <- .download_earthdata(
    urls_todo, hdf_dir, username, password, overwrite, max_workers
  )

  cli::cli_alert_success(
    "Downloaded {length(hdf_files)} HDF file(s)"
  )

  # ── group downloaded HDFs by month ────────────────────────
  dates <- lapply(hdf_files, .parse_modis_date)
  yearmons <- vapply(dates, function(d) {
    sprintf("%04d_%02d", d$year, d$month)
  }, character(1))

  date_df <- data.frame(
    filename = hdf_files,
    yearmon = yearmons,
    stringsAsFactors = FALSE
  )

  unique_dates <- unique(date_df$yearmon)
  aoi_vect <- terra::vect(shapefile)

  # resolve scale factor
  if (identical(scale_factor, "auto")) {
    info <- .modis_scale_info(band)
    if (!is.null(info)) {
      scale_factor <- info$scale
      nodata_val <- info$nodata
      cli::cli_alert_info(
        "Auto scale for {.val {band}}: {scale_factor}"
      )
    } else {
      scale_factor <- NULL
      nodata_val <- -3000
      cli::cli_alert_warning(
        "No known scale factor for {.val {band}}. Saving raw values."
      )
    }
  } else {
    nodata_val <- -3000
  }

  # ── process each date ─────────────────────────────────────
  # months with existing GeoTIFFs were skipped before download
  output_paths <- existing_paths

  for (ym in unique_dates) {
    # compact YYYYMM avoids clean_filenames() stripping the year
    ym_compact <- gsub("_", "", ym)
    out_name <- sprintf("modis_%s_%s.tif", band_clean, ym_compact)
    out_path <- file.path(out_dir, out_name)

    tile_files <- date_df$filename[date_df$yearmon == ym]

    cli::cli_alert_info(
      "Processing {out_name} ({length(tile_files)} tile(s))"
    )

    mosaic <- .mosaic_modis_tiles(tile_files, band)

    # crop to bounding box (reproject AOI to raster CRS)
    aoi_reproj <- terra::project(aoi_vect, terra::crs(mosaic))
    cropped <- terra::crop(mosaic, aoi_reproj)

    # force values into memory, apply nodata + scale
    vals <- terra::values(cropped)
    vals[vals == nodata_val | vals < -2000] <- NA

    if (!is.null(scale_factor)) {
      vals <- vals * scale_factor
    }

    terra::values(cropped) <- vals

    terra::writeRaster(
      cropped, out_path,
      overwrite = TRUE,
      datatype = "FLT4S"
    )

    cli::cli_alert_success("Saved {out_name}")
    output_paths <- c(output_paths, out_path)
  }

  # ── cleanup + summary ─────────────────────────────────────
  # only delete the HDF cache after a full successful run —
  # partial runs leave it in place so a rerun resumes
  if (!keep_hdf) {
    unlink(hdf_dir, recursive = TRUE)
  } else {
    cli::cli_alert_info(
      "HDF files kept in {.path {hdf_dir}}"
    )
  }

  cli::cli_alert_success(
    "Done. {length(output_paths)} GeoTIFF(s) in {.path {out_dir}}"
  )

  invisible(output_paths)
}
