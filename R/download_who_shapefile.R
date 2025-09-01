#' Download WHO Administrative Boundaries with Partial Update
#'
#' Downloads administrative boundaries from WHO's ArcGIS services for specified
#' countries and administrative levels. The function supports incremental
#' updates, downloading only missing data when appending to existing files. It
#' automatically converts date fields from milliseconds to proper Date format
#' and can filter for current boundaries or include historical boundaries.
#'
#' @details
#' The function retrieves boundary data from WHO's Global Administrative
#' Boundaries service hosted on ArcGIS. It uses the httr2 package for API
#' requests and sf for spatial data handling. Date fields are automatically
#' converted from milliseconds since epoch to R Date objects.
#'
#' When `latest = TRUE` (default), only active boundaries are returned (those
#' with ENDDATE of 9999-12-31). When `latest = FALSE`, all historical boundaries
#' are included, useful for tracking boundary changes over time (e.g., country
#' splits or administrative reorganizations).
#'
#' The incremental update feature checks existing files for already downloaded
#' countries and only fetches missing ones, making it efficient for building
#' large boundary datasets.
#'
#' @param country_codes Character vector of ISO3 country codes
#'   (e.g. c("KEN","UGA")).
#' @param admin_level Character string specifying administrative level
#'   ("ADM0", "ADM1", or "ADM2"). Default is "ADM2".
#' @param latest Logical. If TRUE (default), returns only the latest/active
#'   boundaries. If FALSE, returns all historical boundaries.
#' @param dest_path File path where data is saved. If NULL (default),
#'   data is returned without saving to disk. When `latest = TRUE`, the output
#'   filename will include the suffix `_latest` before the extension.
#'
#' @return An `sf` object containing administrative boundaries with the
#'   following columns:
#'   \describe{
#'     \item{adm0_code}{ISO3 country code}
#'     \item{adm0}{Country name}
#'     \item{adm1}{Admin level 1 name (regions/states) - only for ADM1 and ADM2}
#'     \item{adm2}{Admin level 2 name (districts) - only for ADM2}
#'     \item{start_date}{Date when the boundary became effective}
#'     \item{end_date}{Date when the boundary ceased (9999-12-31 for current)}
#'     \item{geometry}{Spatial geometry column}
#'   }
#'   If `dest_path` is provided, the data is also saved to that location.
#' @examples
#'
#' \donttest{
#'
#' tf <- file.path(tempdir(), "test_env")
#'
#' # Download latest ADM2 boundaries without saving to disk
#' kenya_districts <- download_shapefile(
#'   country_codes = "KEN",
#'   admin_level = "ADM2",
#'   latest = TRUE  # Only current boundaries
#' )
#'
#' # Download all historical boundaries (e.g., to track boundary changes)
#' sudan_historical <- download_shapefile(
#'   country_codes = "SDN",
#'   admin_level = "ADM0",
#'   latest = FALSE  # Include historical boundaries
#' )
#' # This would show Sudan before and after South Sudan independence
#' # (2011-07-11)
#'
#' # Download multiple countries and save to directory
#' # File will be saved as "who_shapefile_ken_uga_tza_adm0_latest.gpkg" (latest=TRUE)
#' africa_countries <- download_shapefile(
#'   country_codes = c("KEN", "UGA", "TZA"),
#'   admin_level = "ADM0",
#'   dest_path = tf
#' )
#'
#' # Incremental update - add more countries to existing file
#' # Will automatically use the same filename for the same countries+admin combo
#' download_shapefile(
#'   country_codes = c("KEN", "UGA", "TZA", "RWA", "BDI"),
#'   admin_level = "ADM0",
#'   dest_path = tf
#' )
#' # Creates "who_shapefile_ken_uga_tza_rwa_bdi_adm0_latest.gpkg" (latest=TRUE)
#'
#' # Different admin levels saved to different files automatically
#' # Saves as "who_shapefile_ken_adm1_latest.gpkg" (latest=TRUE)
#' download_shapefile("KEN", "ADM1", dest_path = tf)
#'
#' # Saves as "who_shapefile_ken_adm2_latest.gpkg" (latest=TRUE)
#' download_shapefile("KEN", "ADM2", dest_path = tf)
#'
#' # Multiple countries at ADM2 level
#' # Saves as "who_shapefile_com_syc_mus_adm2_latest.gpkg" (latest=TRUE)
#' download_shapefile(c("COM", "SYC", "MUS"), "ADM2", dest_path = tf)
#' }
#'
#' @note
#' Requires the `httr2` package for API requests. Install with:
#' `install.packages("httr2")`
#'
#' @seealso
#' \code{\link[sf]{st_read}} for reading spatial data,
#' \code{\link[sf]{st_write}} for writing spatial data.
#' WHO boundary services: \url{https://services.arcgis.com/5T5nSi527N4F7luB/ArcGIS/rest/services}
#'
#' @export
download_shapefile <- function(
  country_codes,
  admin_level = "ADM2",
  latest = TRUE,
  dest_path = NULL
) {
  # Check for required packages
  if (!requireNamespace("httr2", quietly = TRUE)) {
    stop(
      paste0(
        "Package 'httr2' is required for this function. Please install it ",
        "with install.packages('httr2')"
      )
    )
  }

  # Validate admin_level
  admin_level <- toupper(admin_level)
  valid_levels <- c("ADM0", "ADM1", "ADM2")
  if (!admin_level %in% valid_levels) {
    stop(
      paste0(
        "Invalid admin_level. Must be one of: ",
        paste(valid_levels, collapse = ", ")
      )
    )
  }

  # Initialize existing codes if dest_path is provided
  dest_file <- NULL
  if (!is.null(dest_path)) {
    # Ensure directory exists
    dir.create(dest_path, recursive = TRUE, showWarnings = FALSE)

    # Construct filename based on country codes and admin level
    countries_str <- tolower(paste(country_codes, collapse = "_"))
    admin_str <- tolower(admin_level)
    # Build base filename and append suffix when latest = TRUE
    file_stem <- paste0("who_shapefile_", countries_str, "_", admin_str)
    if (isTRUE(latest)) {
      file_stem <- paste0(file_stem, "_latest")
    }
    dest_file <- file.path(dest_path, paste0(file_stem, ".gpkg"))

    if (file.exists(dest_file)) {
      existing_sf <- sf::st_read(dest_file, quiet = TRUE)
      existing_codes <- unique(existing_sf$adm0_code)

      # Calculate missing codes
      diff_codes <- setdiff(country_codes, existing_codes)

      # Assign only the missing codes back to `country_codes`
      country_codes <- diff_codes

      if (length(country_codes) == 0) {
        cli::cli_alert_info(
          glue::glue(
            "All requested country codes are already in {basename(dest_file)}. No updates needed."
          )
        )
        # Return the existing data
        return(existing_sf)
      }
    } else {
      existing_codes <- character(0) # No existing codes if file doesn't exist
    }
  }

  # Configure service URLs and fields based on admin level
  service_configs <- list(
    ADM0 = list(
      service = "GLOBAL_ADM0",
      fields = "ISO_3_CODE,ADM0_NAME,STARTDATE,ENDDATE"
    ),
    ADM1 = list(
      service = "Detailed_Boundary_ADM1",
      fields = "ISO_3_CODE,ADM0_NAME,ADM1_NAME,STARTDATE,ENDDATE"
    ),
    ADM2 = list(
      service = "Detailed_Boundary_ADM2",
      fields = "ISO_3_CODE,ADM0_NAME,ADM1_NAME,ADM2_NAME,STARTDATE,ENDDATE"
    )
  )

  config <- service_configs[[admin_level]]

  # Download missing country codes
  codes_str <- paste0("'", paste(country_codes, collapse = "','"), "'")
  where_clause <- paste0("ISO_3_CODE IN (", codes_str, ")")
  base_url <- paste0(
    "https://services.arcgis.com/5T5nSi527N4F7luB",
    "/arcgis/rest/services/", config$service, "/",
    "FeatureServer/0/query"
  )
  params <- list(
    where = where_clause,
    outFields = config$fields,
    outSR = 4326,
    f = "geojson"
  )

  cli::cli_alert_info(
    glue::glue(
      "Downloading WHO {admin_level} data for: ",
      "{crayon::blue(paste(country_codes, collapse=', '))}"
    )
  )

  # Build the URL with query parameters
  request_url <- httr2::request(base_url) |>
    httr2::req_url_query(!!!params)

  # Get the final URL
  final_url <- request_url$url

  # get shapefile
  new_sf <- base::suppressWarnings(
    base::suppressMessages({
      base::invisible(
        utils::capture.output(
          out <- sf::st_read(
            final_url,
            quiet = TRUE
          )
        )
      )
      out
    })
  )

  # Filter for active boundaries if latest = TRUE
  if (latest && "ENDDATE" %in% names(new_sf)) {
    new_sf <- new_sf |>
      dplyr::filter(ENDDATE == "253402214400000" | is.na(ENDDATE))
  }

  # Transform fields based on admin level
  new_sf <- switch(
    admin_level,
    ADM0 = new_sf |>
      dplyr::transmute(
        adm0_code = ISO_3_CODE,
        adm0 = ADM0_NAME,
        start_date = as.Date(as.POSIXct(as.numeric(STARTDATE)/1000, origin = "1970-01-01")),
        end_date = as.Date(as.POSIXct(as.numeric(ENDDATE)/1000, origin = "1970-01-01"))
      ),
    ADM1 = new_sf |>
      dplyr::transmute(
        adm0_code = ISO_3_CODE,
        adm0 = ADM0_NAME,
        adm1 = ADM1_NAME,
        start_date = as.Date(as.POSIXct(as.numeric(STARTDATE)/1000, origin = "1970-01-01")),
        end_date = as.Date(as.POSIXct(as.numeric(ENDDATE)/1000, origin = "1970-01-01"))
      ),
    ADM2 = new_sf |>
      dplyr::transmute(
        adm0_code = ISO_3_CODE,
        adm0 = ADM0_NAME,
        adm1 = ADM1_NAME,
        adm2 = ADM2_NAME,
        start_date = as.Date(as.POSIXct(as.numeric(STARTDATE)/1000, origin = "1970-01-01")),
        end_date = as.Date(as.POSIXct(as.numeric(ENDDATE)/1000, origin = "1970-01-01"))
      )
  )

  # Save data if dest_path was provided
  if (!is.null(dest_path) && !is.null(dest_file)) {

    if (file.exists(dest_file)) {
      # Append to existing file
      sf::st_write(
        sf::st_make_valid(new_sf),
        dest_file,
        append = TRUE,
        quiet = TRUE
      )
      cli::cli_alert_success(
        glue::glue(
          "Appended missing country codes to existing shapefile ({basename(dest_file)}): ",
          "{paste(country_codes, collapse=', ')}"
        )
      )
      # Read and return the complete dataset
      return(sf::st_read(dest_file, quiet = TRUE))
    } else {
      # Create new file
      sf::st_write(
        sf::st_make_valid(new_sf),
        dest_file,
        append = FALSE,
        quiet = TRUE
      )
      cli::cli_alert_success(
        glue::glue(
          "Created new shapefile ({basename(dest_file)}) with country codes: ",
          "{paste(country_codes, collapse=', ')}"
        )
      )
      return(new_sf)
    }
  } else {
    # Return data without saving
    cli::cli_alert_success(
      glue::glue(
        "Downloaded {admin_level} data for: ",
        "{paste(country_codes, collapse=', ')}"
      )
    )
    return(new_sf)
  }
}
