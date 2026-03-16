#' List Available AppEEARS Products and Layers
#'
#' Queries the NASA AppEEARS API for all available products and
#' their layers. Returns a tibble you can filter to find the
#' `product` and `layer` values needed by [download_appeears()].
#'
#' @param search Character or `NULL`. Optional search term to filter
#'   results (case-insensitive). Searches across product ID,
#'   description, and layer names.
#'
#' @return A tibble with columns `product`, `description`,
#'   `layer`, and `layer_description`.
#'
#' @examples
#' \dontrun{
#' # see all products
#' appeears_options()
#'
#' # search for EVI layers
#' appeears_options("EVI")
#'
#' # search for MODIS land surface temperature
#' appeears_options("LST")
#' }
#' @export
appeears_options <- function(search = NULL) {
  base_url <- "https://appeears.earthdatacloud.nasa.gov/api"

  # fetch product list
  prod_resp <- httr2::request(
    paste0(base_url, "/product")
  ) |>
    httr2::req_perform()

  products <- httr2::resp_body_json(prod_resp)

  # build tibble of products
  product_tbl <- tibble::tibble(
    product = vapply(
      products, function(p) p$ProductAndVersion, character(1)
    ),
    description = vapply(
      products, function(p) p$Description %||% "", character(1)
    )
  )

  # fetch layers for each product
  cli::cli_alert_info(
    "Fetching layers for {nrow(product_tbl)} products..."
  )

  rows <- lapply(product_tbl$product, function(prod) {
    layer_resp <- tryCatch(
      httr2::request(
        paste0(base_url, "/product/", prod)
      ) |>
        httr2::req_perform(),
      error = function(e) NULL
    )

    if (is.null(layer_resp)) return(NULL)

    layers <- httr2::resp_body_json(layer_resp)

    if (length(layers) == 0) return(NULL)

    tibble::tibble(
      product = prod,
      layer = names(layers),
      layer_description = vapply(
        layers,
        function(l) l$Description %||% "",
        character(1)
      )
    )
  })

  layer_tbl <- do.call(rbind, rows)

  result <- dplyr::left_join(product_tbl, layer_tbl, by = "product")

  # filter if search term provided
  if (!is.null(search)) {
    pattern <- tolower(search)
    matches <- grepl(pattern, tolower(result$product)) |
      grepl(pattern, tolower(result$description)) |
      grepl(pattern, tolower(result$layer)) |
      grepl(pattern, tolower(result$layer_description))
    result <- result[matches, ]
  }

  result
}

#' Download Raster Data from NASA AppEEARS
#'
#' Submits an area-based extraction task to the NASA AppEEARS API,
#' polls until processing completes, and downloads all GeoTIFF
#' results. Each file is saved with a clean name:
#' `nasa_{layer}_{YYYYMM}.tif`.
#'
#' Use [appeears_options()] to find valid `product` and `layer`
#' values.
#'
#' @param shapefile An `sf` object defining the area of interest.
#'   Reprojected to WGS84 (EPSG:4326) if needed.
#' @param start Character. Start date in any common format
#'   (e.g., `"2023-01-01"`, `"01-01-2023"`, `"01/01/2023"`).
#' @param end Character. End date in any common format.
#' @param product Character. AppEEARS product ID (e.g.,
#'   `"MOD13A3.061"`).
#' @param layer Character. AppEEARS layer name (e.g.,
#'   `"_1_km_monthly_EVI"`).
#' @param out_dir Character. Output directory for GeoTIFFs
#'   (default `"."`). Created if it does not exist.
#' @param username Character or `NULL`. NASA Earthdata username.
#'   Falls back to `Sys.getenv("EARTHDATA_USERNAME")` when `NULL`.
#' @param password Character or `NULL`. NASA Earthdata password.
#'   Falls back to `Sys.getenv("EARTHDATA_PASSWORD")` when `NULL`.
#' @param scale_factor Numeric or `NULL`. Multiply raster values by
#'   this before saving. `NULL` skips scaling (default).
#' @param task_name Character or `NULL`. Custom AppEEARS task name.
#'   Auto-generated when `NULL`.
#' @param poll_interval Numeric. Seconds between status checks
#'   (default 30).
#' @param max_wait Numeric. Maximum seconds to wait for the task
#'   (default 3600).
#'
#' @return A character vector of output file paths (invisibly).
#'
#' @examples
#' \dontrun{
#' boundary <- sf::st_read("boundary.geojson")
#'
#' # download 12 months of EVI — one TIF per month
#' paths <- download_appeears(
#'   shapefile = boundary,
#'   start     = "2023-01-01",
#'   end       = "2023-12-31",
#'   product   = "MOD13A3.061",
#'   layer     = "_1_km_monthly_EVI",
#'   out_dir   = "data/evi",
#'   username  = "my_user",
#'   password  = "my_pass",
#'   scale_factor = 0.0001
#' )
#'
#' # download land surface temperature
#' download_appeears(
#'   shapefile = boundary,
#'   start     = "2023-01-01",
#'   end       = "2023-12-31",
#'   product   = "MOD11A2.061",
#'   layer     = "LST_Day_1km",
#'   out_dir   = "data/lst",
#'   scale_factor = 0.02
#' )
#' }
#' @export
download_appeears <- function(
    shapefile,
    start,
    end,
    product,
    layer,
    out_dir = ".",
    username = NULL,
    password = NULL,
    scale_factor = NULL,
    task_name = NULL,
    poll_interval = 30,
    max_wait = 3600
) {

  base_url <- "https://appeears.earthdatacloud.nasa.gov/api"

  # ── validate shapefile ─────────────────────────────────────────
  if (!inherits(shapefile, "sf")) {
    cli::cli_abort("{.arg shapefile} must be an {.cls sf} object.")
  }

  if (any(sf::st_is_empty(shapefile))) {
    cli::cli_abort("{.arg shapefile} contains empty geometries.")
  }

  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
  }

  # ── authenticate ─────────────────────────────────────────────
  cli::cli_alert_info("Authenticating with AppEEARS")

  username <- username %||% Sys.getenv("EARTHDATA_USERNAME")
  password <- password %||% Sys.getenv("EARTHDATA_PASSWORD")

  if (nchar(username) == 0 || nchar(password) == 0) {
    cli::cli_abort(c(
      "NASA Earthdata credentials are missing.",
      "i" = paste(
        "Pass {.arg username}/{.arg password} or set",
        "{.envvar EARTHDATA_USERNAME}/{.envvar EARTHDATA_PASSWORD}."
      )
    ))
  }

  login_resp <- httr2::request(
    paste0(base_url, "/login")
  ) |>
    httr2::req_method("POST") |>
    httr2::req_auth_basic(username, password) |>
    httr2::req_body_raw(raw()) |>
    httr2::req_error(body = .appeears_error_body) |>
    httr2::req_perform()

  token <- httr2::resp_body_json(login_resp)$token

  if (is.null(token)) {
    cli::cli_abort("Authentication failed.")
  }

  cli::cli_alert_success("Authenticated")

  # ── build bounding box GeoJSON from shapefile ─────────────────
  bbox <- as.numeric(sf::st_bbox(sf::st_transform(shapefile, 4326)))
  xmin <- bbox[1]; ymin <- bbox[2]
  xmax <- bbox[3]; ymax <- bbox[4]

  ring <- list(
    c(xmin, ymin), c(xmax, ymin),
    c(xmax, ymax), c(xmin, ymax),
    c(xmin, ymin)
  )

  geo_payload <- list(
    type = "FeatureCollection",
    features = list(
      list(
        type = "Feature",
        properties = list(),
        geometry = list(
          type = "Polygon",
          coordinates = list(ring)
        )
      )
    )
  )

  # ── parse dates flexibly, convert to MM-DD-YYYY for API ───────
  parsed_start <- lubridate::parse_date_time(
    start, orders = sntutils::available_date_formats, quiet = TRUE
  )
  parsed_end <- lubridate::parse_date_time(
    end, orders = sntutils::available_date_formats, quiet = TRUE
  )

  if (is.na(parsed_start) || is.na(parsed_end)) {
    cli::cli_abort(
      "Could not parse {.arg start} or {.arg end} as dates."
    )
  }

  api_start <- format(parsed_start, "%m-%d-%Y")
  api_end   <- format(parsed_end, "%m-%d-%Y")

  # ── task name ────────────────────────────────────────────────
  if (is.null(task_name)) {
    task_name <- paste0(
      gsub("\\.", "_", product), "_",
      format(parsed_start, "%Y%m%d")
    )
  }

  # ── build task body ──────────────────────────────────────────
  task_body <- list(
    task_type = "area",
    task_name = task_name,
    params = list(
      dates = list(
        list(
          startDate = api_start,
          endDate = api_end,
          recurring = FALSE
        )
      ),
      layers = list(
        list(
          product = product,
          layer = layer
        )
      ),
      output = list(
        format = list(type = "geotiff"),
        projection = "geographic"
      ),
      geo = geo_payload
    )
  )

  # ── submit task ──────────────────────────────────────────────
  cli::cli_alert_info("Submitting task")

  submit_resp <- httr2::request(
    paste0(base_url, "/task")
  ) |>
    httr2::req_method("POST") |>
    httr2::req_headers(
      Authorization = paste("Bearer", token)
    ) |>
    httr2::req_body_json(task_body, auto_unbox = TRUE) |>
    httr2::req_error(body = .appeears_error_body) |>
    httr2::req_timeout(120) |>
    httr2::req_perform()

  task_id <- httr2::resp_body_json(submit_resp)$task_id

  cli::cli_alert_success("Task ID: {task_id}")

  # ── poll until done ──────────────────────────────────────────
  status <- "pending"
  elapsed <- 0

  while (status %in% c("pending", "queued", "processing")) {
    Sys.sleep(poll_interval)
    elapsed <- elapsed + poll_interval

    poll_resp <- httr2::request(
      paste0(base_url, "/task/", task_id)
    ) |>
      httr2::req_headers(
        Authorization = paste("Bearer", token)
      ) |>
      httr2::req_error(body = .appeears_error_body) |>
      httr2::req_perform()

    status <- httr2::resp_body_json(poll_resp)$status

    cli::cli_alert_info(
      "Status: {status} ({elapsed}s elapsed)"
    )

    if (elapsed > max_wait) {
      cli::cli_abort("Task timed out after {max_wait}s.")
    }
  }

  if (status != "done") {
    cli::cli_abort("Task failed with status: {status}")
  }

  cli::cli_alert_success("Task completed")

  # ── download bundle ──────────────────────────────────────────
  bundle_resp <- httr2::request(
    paste0(base_url, "/bundle/", task_id)
  ) |>
    httr2::req_headers(
      Authorization = paste("Bearer", token)
    ) |>
    httr2::req_error(body = .appeears_error_body) |>
    httr2::req_perform()

  files <- httr2::resp_body_json(bundle_resp)$files

  tif_files <- Filter(
    function(f) grepl("\\.tif$", f$file_name, ignore.case = TRUE),
    files
  )

  if (length(tif_files) == 0) {
    cli::cli_abort("No GeoTIFF files found in bundle.")
  }

  cli::cli_alert_info(
    "Downloading {length(tif_files)} GeoTIFF file(s)..."
  )

  # ── build clean name prefix from layer ────────────────────────
  # e.g. "_1_km_monthly_EVI" → "nasa_1_km_monthly_evi"
  layer_clean <- layer |>
    sub("^_+", "", x = _) |>
    tolower()
  layer_clean <- paste0("nasa_", layer_clean)

  # ── download each tif to out_dir ─────────────────────────────
  paths <- vapply(tif_files, function(f) {
    # extract YYYYMM from doy pattern in filename
    # e.g. "...doy2023001..." → "202301"
    doy_match <- regmatches(
      f$file_name,
      regexpr("doy(\\d{7})", f$file_name)
    )
    yearmon <- if (
      length(doy_match) == 1 && nchar(doy_match) == 10
    ) {
      yr  <- substr(doy_match, 4, 7)
      doy <- substr(doy_match, 8, 10)
      format(
        as.Date(paste(yr, doy), format = "%Y %j"),
        "%Y%m"
      )
    } else {
      format(Sys.Date(), "%Y%m")
    }

    clean_name <- paste0(layer_clean, "_", yearmon, ".tif")
    dest <- file.path(out_dir, clean_name)

    httr2::request(
      paste0(base_url, "/bundle/", task_id, "/", f$file_id)
    ) |>
      httr2::req_headers(
        Authorization = paste("Bearer", token)
      ) |>
      httr2::req_error(body = .appeears_error_body) |>
      httr2::req_perform(path = dest)

    if (!is.null(scale_factor)) {
      raster <- terra::rast(dest) * scale_factor
      terra::writeRaster(raster, dest, overwrite = TRUE)
    }

    cli::cli_alert_success("Saved: {clean_name}")
    dest
  }, character(1))

  cli::cli_alert_success(
    "Done. {length(paths)} file(s) saved to {.path {out_dir}}"
  )

  invisible(paths)
}

# ── internal helper ─────────────────────────────────────────────

#' extract error message from an appeears api response
#' @noRd
.appeears_error_body <- function(resp) {
  tryCatch(
    {
      body <- httr2::resp_body_json(resp)
      body$message %||% paste("HTTP", httr2::resp_status(resp))
    },
    error = function(e) {
      paste("HTTP", httr2::resp_status(resp))
    }
  )
}
