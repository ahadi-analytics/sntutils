#' Get active facilities from a dataset
#'
#' This helper function identifies and returns only active facilities from a
#' dataset based on their reporting patterns. It's a convenience wrapper around
#' \code{\link{classify_facility_activity}} that filters the data to include
#' only facilities classified as "Active".
#'
#' @param data A data frame containing health facility data
#' @param hf_col Character. Name of the column containing unique facility IDs
#' @param date_col Character. Name of the date column. Default is "date"
#' @param key_indicators Character vector of indicator columns used to determine
#'   reporting activity. Default is c("allout", "conf", "test", "treat", "pres")
#' @param method Numeric or character. Classification method (1, 2, 3, "method1",
#'   "method2", "method3"). Default is 3 (dynamic activation/inactivation)
#' @param nonreport_window Integer. Number of consecutive non-reporting periods
#'   before a facility is considered inactive (for method 3). Default is 6
#' @param reporting_rule Character. What counts as reporting: "any_non_na"
#'   (default, 0 counts as reported) or "positive_only" (requires >0)
#' @param return_summary Logical. If TRUE, returns a summary tibble instead of
#'   filtered data. Default is FALSE
#'
#' @return If `return_summary = FALSE` (default): Returns the input data frame
#'   filtered to include only rows for active facilities, with an added
#'   `activity_status` column.
#'
#'   If `return_summary = TRUE`: Returns a summary tibble with columns:
#'   - `n_total`: Total number of unique facilities
#'   - `n_active`: Number of active facilities
#'   - `n_inactive`: Number of inactive facilities
#'   - `pct_active`: Percentage of facilities that are active
#'   - `pct_inactive`: Percentage of facilities that are inactive
#'
#' @details
#' This function is particularly useful for:
#' - Pre-filtering data before calculating reporting rates
#' - Understanding the proportion of operational facilities
#' - Ensuring denominators only include facilities that should be reporting
#'
#' The function uses \code{\link{classify_facility_activity}} internally to
#' determine facility status based on reporting patterns in the key indicators.
#'
#' @examples
#' # Create sample data
#' sample_data <- data.frame(
#'   date = rep(as.Date(c("2024-01-01", "2024-02-01", "2024-03-01")), each = 4),
#'   facility_id = rep(c("HF001", "HF002", "HF003", "HF004"), 3),
#'   district = rep(c("North", "North", "South", "South"), 3),
#'   allout = c(
#'     100, 150, 0, NA,    # Month 1: HF001 and HF002 active,
#'                         #  HF003 reports 0, HF004 never reports
#'     110, 160, 0, NA,    # Month 2
#'     105, 155, NA, NA    # Month 3: HF003 stops reporting
#'   ),
#'   conf = c(
#'     10, 15, 0, NA,
#'     11, 16, 0, NA,
#'     10, 15, NA, NA
#'   )
#' )
#'
#' # Get only active facilities
#' active_only <- get_active_facilities(
#'   data = sample_data,
#'   hf_col = "facility_id",
#'   date_col = "date",
#'   key_indicators = c("allout", "conf")
#' )
#'
#' # Get summary of facility activity
#' activity_summary <- get_active_facilities(
#'   data = sample_data,
#'   hf_col = "facility_id",
#'   date_col = "date",
#'   key_indicators = c("allout", "conf"),
#'   return_summary = TRUE
#' )
#'
#' @export
#' @seealso \code{\link{classify_facility_activity}},
#'   \code{\link{calculate_reporting_metrics}}
get_active_facilities <- function(
  data,
  hf_col,
  date_col = "date",
  key_indicators = c("allout", "conf", "test", "treat", "pres"),
  method = 3,
  nonreport_window = 6,
  reporting_rule = "any_non_na",
  return_summary = FALSE
) {

  # Input validation
  if (!is.data.frame(data)) {
    cli::cli_abort(c(
      "!" = "'data' must be a data frame",
      "i" = "You provided an object of class {class(data)[1]}"
    ))
  }

  if (!is.character(hf_col) || length(hf_col) != 1) {
    cli::cli_abort(c(
      "!" = "'hf_col' must be a single character string",
      "i" = "This should be the column name containing facility IDs"
    ))
  }

  if (!(hf_col %in% names(data))) {
    cli::cli_abort(c(
      "!" = "Column '{hf_col}' not found in data",
      "i" = "Available columns: {paste(names(data), collapse = ', ')}"
    ))
  }

  if (!(date_col %in% names(data))) {
    cli::cli_abort(c(
      "!" = "Date column '{date_col}' not found in data",
      "i" = "Available columns: {paste(names(data), collapse = ', ')}"
    ))
  }

  # Check if key indicators exist
  available_indicators <- key_indicators[key_indicators %in% names(data)]
  if (length(available_indicators) == 0) {
    cli::cli_abort(c(
      "!" = "None of the key indicators found in data",
      "i" = "Looking for: {paste(key_indicators, collapse = ', ')}",
      "i" = "Available columns: {paste(names(data), collapse = ', ')}"
    ))
  }

  if (length(available_indicators) < length(key_indicators)) {
    cli::cli_warn(c(
      "!" = "Some key indicators not found in data",
      "i" = "Using only: {paste(available_indicators, collapse = ', ')}"
    ))
  }

  # Classify facility activity
  classified_data <- classify_facility_activity(
    data = data,
    hf_col = hf_col,
    date_col = date_col,
    key_indicators = available_indicators,
    method = method,
    nonreport_window = nonreport_window,
    reporting_rule = reporting_rule,
    binary_classification = TRUE
  )

  # Calculate summary statistics
  n_total <- dplyr::n_distinct(classified_data[[hf_col]])
  activity_counts <- classified_data |>
    dplyr::group_by(activity_status) |>
    dplyr::summarise(
      n = dplyr::n_distinct(.data[[hf_col]]),
      .groups = "drop"
    )

  n_active <- activity_counts |>
    dplyr::filter(activity_status == "Active") |>
    dplyr::pull(n) |>
    sum()

  n_inactive <- activity_counts |>
    dplyr::filter(activity_status == "Inactive") |>
    dplyr::pull(n) |>
    sum()

  # Handle case where all facilities are one type
  if (length(n_active) == 0) n_active <- 0
  if (length(n_inactive) == 0) n_inactive <- 0

  pct_active <- round(100 * n_active / n_total, 1)
  pct_inactive <- round(100 * n_inactive / n_total, 1)

  # Provide informative message
  cli::cli_inform(c(
    "i" = "Facility activity classification:",
    " " = "{n_active} of {n_total} facilities are active ({pct_active}%)",
    " " = "{n_inactive} facilities are inactive ({pct_inactive}%)"
  ))

  if (return_summary) {
    # Return summary tibble
    return(tibble::tibble(
      n_total = n_total,
      n_active = n_active,
      n_inactive = n_inactive,
      pct_active = pct_active,
      pct_inactive = pct_inactive
    ))
  } else {
    # Return filtered data with only active facilities
    active_data <- classified_data |>
      dplyr::filter(activity_status == "Active")

    cli::cli_inform(c(
      "v" = "Returning data filtered to {n_active} active facilities"
    ))

    return(active_data)
  }
}

#' Check if facilities should be excluded from denominators
#'
#' Internal helper function that determines whether a dataset has enough
#' information to properly exclude inactive facilities from reporting
#' rate calculations.
#'
#' @param data Data frame to check
#' @param hf_col Facility ID column name
#' @param key_indicators Vector of indicator columns
#'
#' @return Logical indicating if inactive facility exclusion is possible
#' @noRd
.can_exclude_inactive <- function(data, hf_col = NULL, key_indicators = NULL) {
  # Can exclude inactive if we have facility IDs and key indicators
  if (is.null(hf_col)) return(FALSE)
  if (is.null(key_indicators) || length(key_indicators) == 0) return(FALSE)

  # Check if key indicators exist in data
  indicators_exist <- all(key_indicators %in% names(data))

  return(indicators_exist)
}
