#' Check DHS Indicator List from API
#'
#' @param countryIds DHS country code(s), e.g., "EG"
#' @param indicatorIds Specific indicator ID(s)
#' @param surveyIds Survey ID(s)
#' @param surveyYear Exact year
#' @param surveyYearStart Start of year range
#' @param surveyYearEnd End of year range
#' @param surveyType DHS survey type (e.g., "DHS", "MIS")
#' @param surveyCharacteristicIds Filter by survey characteristic ID
#' @param tagIds Filter by tag ID
#' @param returnFields Fields to return (default: IndicatorId, Label, Definition)
#' @param perPage Max results per page (default = 500)
#' @param page Specific page to return (default = 1)
#' @param f Format (default = "json")
#'
#' @return A data.frame of indicators
#' @export
check_dhs_indicators <- function(
  countryIds = NULL,
  indicatorIds = NULL,
  surveyIds = NULL,
  surveyYear = NULL,
  surveyYearStart = NULL,
  surveyYearEnd = NULL,
  surveyType = NULL,
  surveyCharacteristicIds = NULL,
  tagIds = NULL,
  returnFields = c("IndicatorId", "Label", "Definition", "MeasurementType"),
  perPage = NULL,
  page = NULL,
  f = "json"
) {
  # Base URL
  base_url <- "https://api.dhsprogram.com/rest/dhs/indicators?"

  # Build query string
  params <- list(
    countryIds = countryIds,
    indicatorIds = indicatorIds,
    surveyIds = surveyIds,
    surveyYear = surveyYear,
    surveyYearStart = surveyYearStart,
    surveyYearEnd = surveyYearEnd,
    surveyType = surveyType,
    surveyCharacteristicIds = surveyCharacteristicIds,
    tagIds = tagIds,
    returnFields = paste(returnFields, collapse = ","),
    perPage = perPage,
    page = page,
    f = f
  )

  # Drop NULLs and encode
  query <- paste(
    purrr::compact(params) |>
      purrr::imap_chr(
        ~ paste0(.y, "=", URLencode(as.character(.x), reserved = TRUE))
      ),
    collapse = "&"
  )

  # Full URL
  full_url <- paste0(base_url, query)

  # Fetch with progress bar
  response <- httr2::request(full_url) |>
    httr2::req_progress() |>
    httr2::req_perform()

# Parse and extract the `$Data` element
response |>
  httr2::resp_body_string() |>
  jsonlite::fromJSON(simplifyVector = TRUE) |>
  purrr::pluck("Data")
}

#' Query DHS API Directly via URL Parameters
#'
#' Builds and queries DHS API for indicator data using URL-based access
#' instead of rdhs package.
#'
#' @param countryIds Comma-separated DHS country code(s), e.g., "SL"
#' @param indicatorIds Comma-separated DHS indicator ID(s), e.g., "CM_ECMR_C_U5M"
#' @param surveyIds Optional comma-separated survey ID(s), e.g., "SL2016DHS"
#' @param surveyYear Optional exact survey year, e.g., "2016"
#' @param surveyYearStart Optional survey year range start
#' @param surveyYearEnd Optional survey year range end
#' @param breakdown One of: "national", "subnational", "background", "all"
#' @param f Format to return (default is "json")
#'
#' @return A data.frame containing the `Data` portion of the API response.
#' @export
download_dhs_indicators <- function(
  countryIds,
  indicatorIds,
  surveyIds = NULL,
  surveyYear = NULL,
  surveyYearStart = NULL,
  surveyYearEnd = NULL,
  breakdown = "subnational",
  f = "json"
) {
  # Base URL
  base_url <- "https://api.dhsprogram.com/rest/dhs/data?"

  # Assemble query string
  query <- paste0(
    "breakdown=",
    breakdown,
    "&indicatorIds=",
    indicatorIds,
    "&countryIds=",
    countryIds,
    if (!is.null(surveyIds)) paste0("&surveyIds=", surveyIds),
    if (!is.null(surveyYear)) paste0("&surveyYear=", surveyYear),
    if (!is.null(surveyYearStart)) paste0("&surveyYearStart=", surveyYearStart),
    if (!is.null(surveyYearEnd)) paste0("&surveyYearEnd=", surveyYearEnd),
    "&lang=en&f=",
    f
  )

  full_url <- paste0(base_url, query)

  cli::cli_alert_info("Downloading DHS data...")

  response <- httr2::request(full_url) |>
    httr2::req_progress() |>
    httr2::req_perform()

  if (httr2::resp_is_error(response)) {
    stop("API request failed: ", httr2::resp_status(response))
  }

  content_raw <- httr2::resp_body_string(response)
  data <- jsonlite::fromJSON(content_raw)$Data

  cli::cli_alert_success("Download complete: {nrow(data)} records retrieved.")
  return(data)
}
