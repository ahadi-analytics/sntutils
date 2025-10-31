#' Calculate reporting/missing rate and proportion of reporting facilities
#'
#' This function calculates reporting metrics for health facility data across
#' three common scenarios:
#'
#' 1) **Proportion of facilities reporting any data**
#'    - Calculates the proportion of active facilities (as defined by reporting
#'      on `key_indicators`) that reported at least one of the
#'      `vars_of_interest` in a given period and group.
#'
#' 2) **Reporting rate by group (x_var, y_var)**
#'    - Computes reporting/missing rates for each variable in
#'      `vars_of_interest`, grouped by `x_var` and `y_var`.
#'
#' 3) **Reporting trends over time (x_var only)**
#'    - Computes reporting/missing rates for each variable over time
#'      (x_var only).
#'
#' @param data A data frame containing health facility data.
#' @param vars_of_interest Character vector of variable names to assess
#'   reporting (used for numerator).
#' @param x_var Character. Name of the primary grouping variable (e.g., time
#'    period).
#' @param y_var Character. Optional. Name of the second grouping variable
#'   (e.g., district). Required for scenarios 1 and 3.
#' @param hf_col Character. Name of the column containing unique health facility
#'   IDs. Required only for scenario 1.
#' @param key_indicators Optional. Character vector of indicators used to define
#'   facility activity in scenario 1. Defaults to
#'   `c("allout", "conf", "test", "treat", "pres")`.
#' @param weighting Logical. If TRUE, compute weighted reporting metrics using
#'   facility size as weights.
#' @param weight_var Character. Column used as proxy for facility size when
#'   weighting is TRUE.
#' @param weight_window Integer. Number of periods used to compute the rolling
#'   typical size for weights.
#' @param exclude_current_x Logical. If TRUE, exclude the current period when
#'   averaging weights.
#' @param cold_start Character. Strategy for filling weights when history is
#'   insufficient. Either "median_within_y" or "median_global".
#' @param method Character or numeric. Classification method for facility activity
#'   status. Can be numeric (1, 2, 3) or character ("method1", "method2", "method3").
#'   Defaults to 3. See \code{\link{classify_facility_activity}} for details.
#' @param nonreport_window Integer. Minimum number of consecutive non-reporting
#'   months to classify a facility as inactive in method 3. Defaults to 6.
#' @param reporting_rule Character. Defines what counts as reporting:
#'   `"any_non_na"` (default, counts NA as non-reporting, 0 counts as reported)
#'   or `"positive_only"` (requires >0 value to count as reported).
#' @param weighting Logical. Whether to use weighted reporting rates. When TRUE,
#'   facilities are weighted by their typical size, giving more importance to
#'   larger facilities in the overall reporting rate calculation. This provides
#'   a volume-adjusted measure of data completeness. Default is FALSE.
#' @param weight_var Character. Name of the variable to use as proxy for
#'   facility size (e.g., "allout" for total outpatients, "test" for tests done).
#'   This should be a count variable that reflects facility activity/size.
#'   If NULL and weighting is TRUE, will auto-select from allout, test, conf
#'   (in that order).
#' @param weight_window Integer. Number of periods for rolling window to
#'   calculate typical facility size. A facility's weight is based on its
#'   average size over the past weight_window periods. Larger windows provide
#'   more stable weights but may miss recent changes. Default is 12.
#' @param exclude_current_x Logical. Whether to exclude current period when
#'   calculating weights. If TRUE, prevents current reporting from influencing
#'   its own weight (avoids circularity). Default is TRUE.
#' @param cold_start Character. Method for handling facilities with insufficient
#'   history (< weight_window periods). Options:
#'   - "median_within_y" (default): Uses median size of facilities within the
#'     same y_var group (e.g., same district)
#'   - "median_global": Uses median size across all facilities
#'
#' @return A tibble with the number of reporting (`rep`) and expected (`exp`)
#' facilities or records, and the computed `reprate` and `missrate`.
#'
#' If weighting is TRUE, additional columns are included:
#' - `reprate_w`: Weighted reporting rate (0-1)
#' - `missrate_w`: Weighted missing rate (0-1)
#' - `avg_<weight_var>`: Average raw value of the weight variable (e.g., avg_allout)
#' - `min_<weight_var>`: Minimum raw value of the weight variable
#' - `max_<weight_var>`: Maximum raw value of the weight variable
#'
#' @details
#' ## Weighted Reporting Rate Calculation
#'
#' When `weighting = TRUE`, the function calculates volume-adjusted reporting
#' rates that give more importance to larger facilities. This is useful when
#' you want the overall reporting rate to reflect the proportion of patient
#' visits or services covered rather than just the proportion of facilities.
#'
#' The weighting algorithm works as follows:
#'
#' 1. **Calculate typical facility size**: For each facility, compute the
#'    rolling mean of `weight_var` over the past `weight_window` periods.
#'    This represents the facility's typical size/volume.
#'
#' 2. **Handle cold starts**: For facilities with insufficient history:
#'    - If `cold_start = "median_within_y"`: Use the median typical size
#'      of facilities in the same group (y_var)
#'    - If `cold_start = "median_global"`: Use the overall median typical size
#'
#' 3. **Normalize weights**: Within each time period and group, weights are
#'    normalized to sum to 1. This ensures that larger facilities get
#'    proportionally more weight.
#'
#' 4. **Calculate weighted rates**:
#'    - `reprate_w = sum(weight * reported) / sum(weight)`
#'    - `missrate_w = 1 - reprate_w`
#'
#' ## Example Interpretation
#'
#' If a district has 10 facilities where:
#' - 3 large facilities (80% of patient volume) all report
#' - 7 small facilities (20% of patient volume) with only 4 reporting
#'
#' Then:
#' - Unweighted reporting rate = 7/10 = 70%
#' - Weighted reporting rate \\u2248 84% (reflecting that most patient volume is covered)
#'
#' @examples
#' # Example with dates instead of month names for compatibility
#' hf_data <- data.frame(
#'   month = rep(as.Date(c("2024-01-01", "2024-02-01", "2024-03-01")), each = 10),
#'   district = rep(c("North", "South"), each = 5, times = 3),
#'   facility_id = rep(1:5, times = 6),
#'   conf = c(
#'     10, 0, 15, NA, 8, 12, 0, NA, 7, 9,
#'     11, 0, 14, 6, NA, 13, 8, 10, 0, 12,
#'     9, 7, 0, 11, 14, 8, NA, 12, 10, 15
#'   ),
#'   pres = c(
#'     5, 0, NA, 7, 3, 6, 0, 4, NA, 2,
#'     8, 0, 6, NA, 4, 7, 3, 0, 5, 6,
#'     4, 0, 7, 5, NA, 6, 0, 8, 4, 3
#'   ),
#'   allout = c(
#'     5, 0, NA, 7, 3, 6, 0, 4, NA, 2,
#'     8, 0, 6, NA, 4, 7, 3, 0, 5, 6,
#'     4, 0, 7, 5, NA, 6, 0, 8, 4, 3
#'   )
#' )
#'
#' # Scenario 1: Proportion of active facilities reporting
#' # any data (using numeric method)
#' calculate_reporting_metrics(
#'   data = hf_data,
#'   vars_of_interest = c("conf"),
#'   x_var = "month",
#'   y_var = "district",
#'   hf_col = "facility_id",
#'   key_indicators = c("allout", "conf", "pres"),
#'   method = 3  # Can also use "method3"
#' )
#'
#' # Scenario 2: Reporting rate by month and district
#' calculate_reporting_metrics(
#'   data = hf_data,
#'   vars_of_interest = c("conf"),
#'   x_var = "month",
#'   y_var = "district"
#' )
#'
#' # Scenario 3: Reporting trends over time
#' calculate_reporting_metrics(
#'   data = hf_data,
#'   vars_of_interest = c("conf"),
#'   x_var = "month"
#' )
#'
#'
#' # Example with weighted reporting rate
#' # Create data with facilities of different sizes
#' weighted_data <- data.frame(
#'   month = rep(1:6, each = 5),
#'   district = rep("A", 30),
#'   hf_id = rep(c("Large1", "Large2", "Small1", "Small2", "Small3"), 6),
#'   # Large facilities see ~1000 patients, small ones ~100
#'   allout = c(
#'     # Month 1-3: Historical data for weight calculation
#'     1050, 980, 95, 110, 105,  # Month 1
#'     1100, 1020, 100, 98, 112,  # Month 2
#'     990, 1080, 105, 102, 108,  # Month 3
#'     # Month 4-6: Current periods
#'     1070, 1050, 98, 105, 110,  # Month 4
#'     1020, 990, 102, 108, 95,   # Month 5
#'     1100, 1030, 110, 100, 105  # Month 6
#'   ),
#'   # Reporting pattern: large facilities always report, small ones sporadic
#'   malaria = c(
#'     # Month 1-3
#'     50, 48, 5, NA, 6,    # Month 1
#'     55, 51, NA, 4, NA,   # Month 2
#'     49, 54, 6, NA, 5,    # Month 3
#'     # Month 4-6
#'     52, 53, NA, NA, 6,   # Month 4
#'     51, 49, 5, 6, NA,    # Month 5
#'     54, 52, NA, 5, NA    # Month 6
#'   )
#' )
#'
#' # Compare unweighted vs weighted reporting rates
#' unweighted_result <- calculate_reporting_metrics(
#'   data = weighted_data,
#'   vars_of_interest = "malaria",
#'   x_var = "month",
#'   y_var = "district",
#'   hf_col = "hf_id",
#'   weighting = FALSE
#' )
#'
#' weighted_result <- calculate_reporting_metrics(
#'   data = weighted_data,
#'   vars_of_interest = "malaria",
#'   x_var = "month",
#'   y_var = "district",
#'   hf_col = "hf_id",
#'   weighting = TRUE,
#'   weight_var = "allout",
#'   weight_window = 3,
#'   exclude_current_x = TRUE
#' )
#'
#' # Unweighted: counts facilities equally (e.g., 3/5 = 60%)
#' # Weighted: reflects patient volume covered (e.g., ~88% if large facilities report)
#' @export
calculate_reporting_metrics <- function(
  data,
  vars_of_interest,
  x_var,
  y_var = NULL,
  hf_col = NULL,
  key_indicators = c("allout", "conf", "test", "treat", "pres"),
  method = 3,
  nonreport_window = 6,
  reporting_rule = "any_non_na",
  weighting = FALSE,
  weight_var = NULL,
  weight_window = 12,
  exclude_current_x = TRUE,
  cold_start = "median_within_y"
) {
  # ensure_packages("dtplyr")
  if (weighting && !requireNamespace("slider", quietly = TRUE)) {
    cli::cli_abort("'slider' package is required when weighting = TRUE")
  }

  # Normalize method parameter to accept both numeric and character
  method <- .normalize_method(method)

  if (!is.data.frame(data)) {
    cli::cli_abort(c(
      "!" = "'data' must be a data frame",
      "i" = "Run `rlang::last_trace()` to see where the error occurred."
    ))
  }
  if (!is.character(vars_of_interest) || length(vars_of_interest) == 0) {
    cli::cli_abort(c(
      "!" = "'vars_of_interest' must be a non-empty character vector",
      "i" = "Run `rlang::last_trace()` to see where the error occurred."
    ))
  }
  if (!is.character(x_var) || length(x_var) != 1) {
    cli::cli_abort(c(
      "!" = "'x_var' must be a single character string",
      "i" = "Run `rlang::last_trace()` to see where the error occurred."
    ))
  }
  if (!all(c(x_var, vars_of_interest) %in% names(data))) {
    cli::cli_abort(c(
      "!" = "Required columns missing from data",
      "i" = "Run `rlang::last_trace()` to see where the error occurred."
    ))
  }
  if (!is.null(y_var) && !(y_var %in% names(data))) {
    cli::cli_abort(c(
      "!" = "y_var not found in data",
      "i" = "Run `rlang::last_trace()` to see where the error occurred."
    ))
  }
  if (!is.null(hf_col) && !(hf_col %in% names(data))) {
    cli::cli_abort(c(
      "!" = "hf_col not found in data",
      "i" = "Run `rlang::last_trace()` to see where the error occurred."
    ))
  }
  if (is.null(y_var) && !is.null(hf_col)) {
    cli::cli_abort(c(
      "!" = "y_var required when hf_col is provided",
      "i" = "Run `rlang::last_trace()` to see where the error occurred."
    ))
  }

  # Validate weighting parameters
  if (weighting) {
    if (is.null(hf_col)) {
      cli::cli_abort(c(
        "!" = "hf_col is required when weighting is TRUE",
        "i" = "Weights need facility-level data"
      ))
    }

    # Auto-select weight_var if not provided
    if (is.null(weight_var)) {
      weight_var_candidates <- c("allout", "test", "conf")
      weight_var <- weight_var_candidates[weight_var_candidates %in% names(data)][1]
      if (is.na(weight_var)) {
        cli::cli_abort(c(
          "!" = "No suitable weight_var found in data",
          "i" = "Tried: allout, test, conf. Specify weight_var manually."
        ))
      }
      cli::cli_inform(c(
        "i" = "Auto-selected weight_var: {weight_var}"
      ))
    } else if (!(weight_var %in% names(data))) {
      cli::cli_abort(c(
        "!" = "weight_var '{weight_var}' not found in data"
      ))
    }

    if (!cold_start %in% c("median_within_y", "median_global")) {
      cli::cli_abort(c(
        "!" = "cold_start must be 'median_within_y' or 'median_global'"
      ))
    }
  }


  calculate_rates <- function(df) {
    df |>
      dplyr::mutate(
        reprate = dplyr::if_else(exp > 0, rep / exp, NA_real_),
        missrate = dplyr::if_else(!is.na(reprate), 1 - reprate, NA_real_)
      )
  }

  # Build weight data if weighting is enabled
  weight_data <- NULL
  if (weighting) {
    # Order data by x_var for rolling calculations
    data <- data |> dplyr::arrange(.data[[x_var]], .data[[hf_col]])

    # Calculate rolling mean for each facility
    weight_data <- data |>
      dplyr::group_by(.data[[hf_col]]) |>
      dplyr::arrange(.data[[x_var]]) |>
      dplyr::mutate(
        # Calculate rolling mean excluding current if specified
        # With .after = -1, window size = .before exactly (excludes current observation)
        typical_size = if (exclude_current_x) {
          slider::slide_dbl(
            .data[[weight_var]],
            base::mean,
            na.rm = TRUE,
            .before = weight_window,
            .after = -1,
            .complete = FALSE
          )
        } else {
          slider::slide_dbl(
            .data[[weight_var]],
            base::mean,
            na.rm = TRUE,
            .before = weight_window - 1,
            .complete = FALSE
          )
        }
      ) |>
      dplyr::ungroup()

    # Mark cold starts before filling NAs
    weight_data <- weight_data |>
      dplyr::mutate(is_cold_start = is.na(typical_size))

    # Handle cold starts
    if (cold_start == "median_within_y" && !is.null(y_var)) {
      cold_start_values <- weight_data |>
        dplyr::filter(!is.na(typical_size)) |>
        dplyr::group_by(.data[[y_var]]) |>
        dplyr::summarise(
          cold_start_value = stats::median(typical_size, na.rm = TRUE),
          .groups = "drop"
        )

      # Calculate global median as fallback for y-groups with all cold starts
      global_median <- stats::median(weight_data$typical_size[!weight_data$is_cold_start], na.rm = TRUE)

      weight_data <- weight_data |>
        dplyr::left_join(cold_start_values, by = y_var) |>
        dplyr::mutate(
          typical_size = dplyr::coalesce(typical_size, cold_start_value, global_median)
        )
    } else {
      # Global median
      global_median <- stats::median(weight_data$typical_size[!weight_data$is_cold_start], na.rm = TRUE)
      weight_data <- weight_data |>
        dplyr::mutate(
          typical_size = dplyr::coalesce(typical_size, global_median)
        )
    }

    # Normalize weights within each period and group
    group_vars <- c(x_var)
    if (!is.null(y_var)) {
      group_vars <- c(group_vars, y_var)
    }

    weight_data <- weight_data |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
      dplyr::mutate(
        weight = typical_size / sum(typical_size, na.rm = TRUE),
        weight = dplyr::if_else(is.na(weight) | is.infinite(weight), 0, weight)
      ) |>
      dplyr::ungroup()

    # QA check: weights should sum to 1
    weight_check <- weight_data |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
      dplyr::summarise(
        weight_sum = sum(weight, na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::filter(abs(weight_sum - 1) > 1e-6)

    if (nrow(weight_check) > 0) {
      cli::cli_warn(c(
        "!" = "Weights do not sum to 1 in {nrow(weight_check)} groups",
        "i" = "Check data quality and weight calculations"
      ))
    }

    # Check cold start proportion
    cold_start_check <- weight_data |>
      dplyr::group_by(.data[[x_var]]) |>
      dplyr::summarise(
        n_total = dplyr::n(),
        n_cold_start = sum(is_cold_start),
        prop_cold_start = n_cold_start / n_total,
        .groups = "drop"
      ) |>
      dplyr::filter(prop_cold_start > 0.25)

    if (nrow(cold_start_check) > 0) {
      cli::cli_warn(c(
        "!" = ">25% cold starts in {nrow(cold_start_check)} periods",
        "i" = "Consider adjusting weight_window or data range"
      ))
    }

    # Select only needed columns for joining
    weight_data <- weight_data |>
      dplyr::select(dplyr::all_of(c(hf_col, x_var, y_var, "weight", "typical_size")))
  }

  # Check if key_indicators columns exist in the data
  key_indicators_available <- !is.null(key_indicators) && length(key_indicators) > 0 && all(key_indicators %in% names(data))

  if (!is.null(hf_col) && !is.null(key_indicators) && key_indicators_available) {

    # Use classify_facility_activity to determine facility status
    # Need to ensure data has a date column for classify_facility_activity
    if (x_var == "date" || "date" %in% names(data)) {
      # If x_var is already "date" or data has a "date" column, use it
      date_col_name <- if (x_var == "date") x_var else "date"
    } else {
      # Create a temporary date column from x_var
      # This handles cases where x_var might be "month", "yearmon", etc.
      date_col_name <- ".temp_date_col"

      # Try to convert x_var to date format
      temp_dates <- tryCatch({
        # First check if it's already a Date
        if (inherits(data[[x_var]], "Date")) {
          data[[x_var]]
        } else if (is.numeric(data[[x_var]]) || inherits(data[[x_var]], "yearmon")) {
          # Handle numeric years or zoo::yearmon
          as.Date(paste0(data[[x_var]], "-01-01"))
        } else {
          # Try to parse as date string
          as.Date(data[[x_var]])
        }
      }, error = function(e) {
        # If direct conversion fails, try with common month formats
        tryCatch({
          # Check for month names like "Jan", "Feb", etc.
          if (all(data[[x_var]] %in% month.abb) || all(data[[x_var]] %in% month.name)) {
            # Need year information - look for a year column
            if ("year" %in% names(data)) {
              month_nums <- match(data[[x_var]], month.abb)
              if (any(is.na(month_nums))) {
                month_nums <- match(data[[x_var]], month.name)
              }
              return(as.Date(paste(data$year, month_nums, "01", sep = "-")))
            } else {
              # Use current year as default
              month_nums <- match(data[[x_var]], month.abb)
              if (any(is.na(month_nums))) {
                month_nums <- match(data[[x_var]], month.name)
              }
              return(as.Date(paste(format(Sys.Date(), "%Y"), month_nums, "01", sep = "-")))
            }
          } else {
            # Try lubridate-style parsing if available
            if (requireNamespace("lubridate", quietly = TRUE)) {
              parsed <- lubridate::parse_date_time(data[[x_var]], orders = c("ymd", "dmy", "mdy", "ym", "my"))
              return(as.Date(parsed))  # Convert POSIXct to Date
            } else {
              stop("Cannot parse dates")
            }
          }
        }, error = function(e2) {
          cli::cli_abort(c(
            "!" = "Cannot convert {x_var} to date format for facility classification",
            "i" = "The column contains values like: {paste(utils::head(unique(data[[x_var]]), 3), collapse = ', ')}",
            "i" = "Consider providing data with a proper date column or ensure {x_var} is in a standard date format"
          ))
        })
      })

      data[[date_col_name]] <- temp_dates
    }

    # Get activity classification for facilities
    classified_data <- classify_facility_activity(
      data = data,
      hf_col = hf_col,
      date_col = date_col_name,
      key_indicators = key_indicators,
      method = method,
      nonreport_window = nonreport_window,
      reporting_rule = reporting_rule,
      binary_classification = TRUE  # We want Active/Inactive for denominator
    )

    # Merge back the activity status
    # We need to handle the join based on whether we created a temp column
    if (date_col_name == ".temp_date_col") {
      # For temporary date columns, we need to match on both hf_col and the original x_var
      # The classified_data has the temp column, but we join on the original data structure
      data <- data |>
        dplyr::left_join(
          classified_data |>
            dplyr::select(dplyr::all_of(c(hf_col, date_col_name, "activity_status"))),
          by = c(hf_col, date_col_name)
        ) |>
        dplyr::mutate(
          # Facility is in denominator if it's active
          include_in_denom = activity_status == "Active"
        )
    } else {
      # For regular date columns, join normally
      join_cols <- c(hf_col, date_col_name)
      data <- data |>
        dplyr::left_join(
          classified_data |>
            dplyr::select(dplyr::all_of(c(join_cols, "activity_status"))),
          by = join_cols
        ) |>
        dplyr::mutate(
          # Facility is in denominator if it's active
          include_in_denom = activity_status == "Active"
        )
    }

    # Clean up temporary column if we created one
    if (date_col_name == ".temp_date_col" && date_col_name %in% names(data)) {
      data[[date_col_name]] <- NULL
    }
  } else if (!is.null(hf_col)) {
    # If key_indicators aren't available but we have hf_col,
    # include all facilities in the denominator
    data <- data |>
      dplyr::mutate(include_in_denom = TRUE)
  }

  if (!is.null(hf_col)) {
    # Aggregate
    if (weighting && !is.null(weight_data)) {
      # Join with weight data
      data_with_weights <- data |>
        dplyr::filter(include_in_denom) |>
        dplyr::left_join(
          weight_data,
          by = dplyr::join_by(!!!rlang::syms(c(x_var, y_var, hf_col)))
        ) |>
        dplyr::mutate(
          reported_any_var = dplyr::if_any(dplyr::all_of(vars_of_interest), ~ !is.na(.x))
        )

      # First aggregate by facility within each group to get facility-level reporting
      facility_summary <- data_with_weights |>
        dplyr::group_by(dplyr::across(dplyr::all_of(c(x_var, y_var, hf_col)))) |>
        dplyr::summarise(
          reported_facility = any(reported_any_var),
          weight_facility = mean(weight, na.rm = TRUE),
          weight_value_facility = mean(.data[[weight_var]], na.rm = TRUE),
          .groups = "drop"
        )

      # Then aggregate to group level
      result <- facility_summary |>
        dplyr::group_by(dplyr::across(dplyr::all_of(c(x_var, y_var)))) |>
        dplyr::summarise(
          # Count distinct facilities that reported
          rep = sum(reported_facility),
          exp = dplyr::n(),
          # Calculate weighted rates
          w_num = sum(weight_facility * reported_facility, na.rm = TRUE),
          w_den = sum(weight_facility, na.rm = TRUE),
          reprate_w = dplyr::if_else(w_den > 0, w_num / w_den, NA_real_),
          missrate_w = dplyr::if_else(!is.na(reprate_w), 1 - reprate_w, NA_real_),
          # Add raw weight_var statistics
          !!paste0("avg_", weight_var) := base::mean(weight_value_facility, na.rm = TRUE),
          !!paste0("min_", weight_var) := min(weight_value_facility, na.rm = TRUE),
          !!paste0("max_", weight_var) := max(weight_value_facility, na.rm = TRUE),
          .groups = "drop"
        ) |>
        dplyr::mutate(
          reprate = dplyr::if_else(exp > 0, rep / exp, NA_real_),
          missrate = dplyr::if_else(!is.na(reprate), 1 - reprate, NA_real_)
        ) |>
        dplyr::select(
          dplyr::all_of(c(x_var, y_var)),
          rep, exp, reprate, missrate,
          reprate_w, missrate_w,
          dplyr::starts_with("avg_"),
          dplyr::starts_with("min_"),
          dplyr::starts_with("max_")
        )
    } else {
      result <- data |>
        dplyr::filter(include_in_denom) |>
        dplyr::mutate(
          reported_any_var = dplyr::if_any(dplyr::all_of(vars_of_interest), ~ !is.na(.x))
        ) |>
        dplyr::group_by(dplyr::across(dplyr::all_of(c(x_var, y_var)))) |>
        dplyr::summarise(
          # Count distinct facilities that reported any of vars_of_interest
          rep = dplyr::n_distinct(.data[[hf_col]][reported_any_var]),
          exp = dplyr::n_distinct(.data[[hf_col]]),
          .groups = "drop"
        ) |>
        calculate_rates()
    }
  } else if (!is.null(y_var)) {
    long_data <- data |>
      dplyr::select(
        dplyr::all_of(c(x_var, y_var, vars_of_interest))
      ) |>
      tidyr::pivot_longer(
        cols = dplyr::all_of(vars_of_interest),
        names_to = "variable",
        values_to = "value"
      ) |>
      dplyr::mutate(
        variable = factor(variable, levels = rev(vars_of_interest))
      )

    result <- long_data |>
      dplyr::group_by(
        dplyr::across(dplyr::all_of(c(x_var, y_var, "variable")))
      ) |>
      dplyr::summarise(
        exp = dplyr::n(),
        rep = sum(!is.na(value)),
        .groups = "drop"
      ) |>
      calculate_rates() |>
      dplyr::select(
        dplyr::all_of(
          c(
            x_var,
            y_var,
            "variable",
            "exp",
            "rep",
            "reprate",
            "missrate"
          )
        )
      )
  } else if (!is.null(x_var)) {
    long_data <- data |>
      dplyr::select(dplyr::all_of(c(x_var, vars_of_interest))) |>
      tidyr::pivot_longer(
        cols = dplyr::all_of(vars_of_interest),
        names_to = "variable",
        values_to = "value"
      ) |>
      dplyr::mutate(
        variable = factor(variable, levels = rev(vars_of_interest))
      )

    result <- long_data |>
      dplyr::group_by(dplyr::across(dplyr::all_of(c(x_var, "variable")))) |>
      dplyr::summarise(
        exp = dplyr::n(),
        rep = sum(!is.na(value)),
        .groups = "drop"
      ) |>
      calculate_rates() |>
      dplyr::select(
        dplyr::all_of(
          c(
            x_var,
            "variable",
            "exp",
            "rep",
            "reprate",
            "missrate"
          )
        )
      )
  } else {
    stop("At minimum, x_var must be provided")
  }

  result
}

# Internal helper to normalize method parameter
#' @noRd
.normalize_method <- function(method) {
  if (is.numeric(method)) {
    if (!method %in% 1:3) {
      cli::cli_abort(c(
        "!" = "method must be 1, 2, or 3 when numeric",
        "i" = "You provided: {method}"
      ))
    }
    paste0("method", method)
  } else if (is.character(method)) {
    # Accept both "method1" and "1" formats
    if (method %in% c("1", "2", "3")) {
      paste0("method", method)
    } else if (!method %in% c("method1", "method2", "method3")) {
      cli::cli_abort(c(
        "!" = "method must be 'method1', 'method2', 'method3', or numeric 1, 2, 3",
        "i" = "You provided: {method}"
      ))
    } else {
      method
    }
  } else {
    cli::cli_abort("method must be numeric (1, 2, 3) or character")
  }
}

#' Prepare data for reporting rate or missing data visualization
#'
#' This function processes health facility data to prepare it for visualizing
#' reporting rates or missing data patterns. It supports three scenarios:
#' 1) reporting rate by time and admin unit (x + y, single var)
#' 2) missing rate by variable over time (x only, multiple vars)
#' 3) proportion of facilities reporting in each (x, y) group (needs hf)
#'
#' @param data Original data frame
#' @param x_var Character. Time variable (e.g. yearmon)
#' @param y_var Character. Optional grouping variable (e.g. district)
#' @param vars_of_interest Character vector of variables to analyze
#' @param by_facility Logical. If TRUE, compute by facility
#' @param hf_col Character. Name of health facility ID column (required
#'    if by_facility)
#' @param use_reprate Logical. If TRUE, return reporting rate. Else, missing
#'    rate
#' @param key_indicators Optional. Character vector of indicators used to define
#'   facility activity in scenario 1. Defaults to
#'   `c("allout", "conf", "test", "treat", "pres")`.
#' @param weighting Logical. Whether to use weighted reporting rates. When TRUE,
#'   facilities are weighted by their typical size, giving more importance to
#'   larger facilities in the overall reporting rate calculation. Default is FALSE.
#' @param weight_var Character. Name of the variable to use as proxy for
#'   facility size (e.g., "allout" for total outpatients). Required when
#'   weighting is TRUE. Default is NULL.
#' @param weight_window Integer. Number of periods for rolling window to
#'   calculate typical facility size. Default is 12.
#' @param exclude_current_x Logical. Whether to exclude current period when
#'   calculating weights. If TRUE, prevents current reporting from influencing
#'   its own weight. Default is TRUE.
#' @param cold_start Character. Method for handling facilities with insufficient
#'   history. Options: "median_within_y" (default) or "median_global".
#' @param method Character or numeric. Classification method for facility activity
#'   status. Can be numeric (1, 2, 3) or character ("method1", "method2", "method3").
#'   Defaults to 3. See \code{\link{classify_facility_activity}} for details.
#' @param nonreport_window Integer. Minimum number of consecutive non-reporting
#'   months to classify a facility as inactive in method 3. Defaults to 6.
#' @param reporting_rule Character. Defines what counts as reporting:
#'   `"any_non_na"` (default, counts NA as non-reporting, 0 counts as reported)
#'   or `"positive_only"` (requires >0 value to count as reported).
#'
#' @return A list with plot_data and plotting metadata
#' @examples
#' # Sample data
#' hf_data <- data.frame(
#'   month = rep(as.Date(c("2024-01-01", "2024-02-01", "2024-03-01")), each = 10),
#'   district = rep(c("North", "South"), each = 5, times = 3),
#'   facility_id = rep(1:5, times = 6),
#'   malaria = c(
#'     10, 0, 15, NA, 8, 12, 0, NA, 7, 9,
#'     11, 0, 14, 6, NA, 13, 8, 10, 0, 12,
#'     9, 7, 0, 11, 14, 8, NA, 12, 10, 15
#'   ),
#'   pneumonia = c(
#'     5, 0, NA, 7, 3, 6, 0, 4, NA, 2,
#'     8, 0, 6, NA, 4, 7, 3, 0, 5, 6,
#'     4, 0, 7, 5, NA, 6, 0, 8, 4, 3
#'   )
#' )
#'
#' # Scenario 1: Reporting rate by district and month for each variable
#' district_plot_data <- prepare_plot_data(
#'   data = hf_data,
#'   x_var = "month",
#'   y_var = "district",
#'   vars_of_interest = c("malaria", "pneumonia"),
#'   use_reprate = TRUE
#' )
#' # Returns list with plot_data and metadata for district-level visualization
#'
#' # Scenario 2: Missing rate of variables over time (months only)
#' variable_plot_data <- prepare_plot_data(
#'   data = hf_data,
#'   x_var = "month",
#'   vars_of_interest = c("malaria", "pneumonia"),
#'   use_reprate = FALSE
#' )
#' # Returns list with plot_data and metadata for variable-level visualization
#'
#' # Scenario 3: Proportion of facilities reporting in each district by month
#' facility_plot_data <- prepare_plot_data(
#'   data = hf_data,
#'   x_var = "month",
#'   y_var = "district",
#'   vars_of_interest = "malaria",
#'   by_facility = TRUE,
#'   hf_col = "facility_id"
#' )
#' # Returns list with plot_data and metadata for facility-level visualization
#' @export
prepare_plot_data <- function(
  data,
  x_var,
  y_var = NULL,
  vars_of_interest,
  by_facility = FALSE,
  hf_col = NULL,
  use_reprate = TRUE,
  key_indicators = c("allout", "conf", "test", "treat", "pres"),
  method = 3,
  nonreport_window = 6,
  reporting_rule = "any_non_na",
  weighting = FALSE,
  weight_var = NULL,
  weight_window = 12,
  exclude_current_x = TRUE,
  cold_start = "median_within_y"
) {

  # Normalize method parameter to accept both numeric and character
  method <- .normalize_method(method)

  # Input validation
  if (!is.data.frame(data)) {
    cli::cli_abort(c(
      "!" = "'data' must be a data frame",
      "i" = "Run `rlang::last_trace()` to see where the error occurred."
    ))
  }
  if (!is.character(x_var) || length(x_var) != 1 || !(x_var %in% names(data))) {
    cli::cli_abort(c(
      "!" = "'x_var' must be a single column name that exists in data",
      "i" = "Run `rlang::last_trace()` to see where the error occurred."
    ))
  }
  if (!is.character(vars_of_interest) || length(vars_of_interest) == 0) {
    cli::cli_abort(c(
      "!" = "'vars_of_interest' must be a non-empty character vector",
      "i" = "Run `rlang::last_trace()` to see where the error occurred."
    ))
  }
  if (!all(vars_of_interest %in% names(data))) {
    cli::cli_abort(c(
      "!" = "All variables in 'vars_of_interest' must exist in data",
      "i" = "Run `rlang::last_trace()` to see where the error occurred."
    ))
  }
  if (!is.null(y_var) && (!is.character(y_var) || !(y_var %in% names(data)))) {
    cli::cli_abort(c(
      "!" = "'y_var' must be a column name that exists in data",
      "i" = "Run `rlang::last_trace()` to see where the error occurred."
    ))
  }

  # Facility-specific validations
  if (by_facility) {
    if (is.null(y_var)) {
      cli::cli_abort(c(
        "!" = "'y_var' is required when by_facility = TRUE",
        "i" = "Run `rlang::last_trace()` to see where the error occurred."
      ))
    }
    if (is.null(hf_col) || !(hf_col %in% names(data))) {
      cli::cli_abort(c(
        "!" = paste0(
          "'hf_col' must be provided and ",
          "exist in data when by_facility = TRUE"
        ),
        "i" = "Run `rlang::last_trace()` to see where the error occurred."
      ))
    }
    if (length(vars_of_interest) > 1) {
      cli::cli_abort(c(
        "!" = "Only one variable can be used when by_facility = TRUE",
        "i" = "Run `rlang::last_trace()` to see where the error occurred."
      ))
    }
  }

  # Determine fill variable and labels based on
  # reporting/missing rate choice and weighting
  if (weighting) {
    fill_var <- if (use_reprate) {
      "reprate_w"
    } else {
      "missrate_w"
    }
  } else {
    fill_var <- if (use_reprate) {
      "reprate"
    } else {
      "missrate"
    }
  }

  fill_label <- if (use_reprate) {
    if (weighting) {
      "Weighted reporting rate (%)"
    } else {
      "Reporting rate (%)"
    }
  } else {
    if (weighting) {
      "Weighted missing rate (%)"
    } else {
      "Missing rate (%)"
    }
  }
  y_axis_label <- if (!is.null(y_var)) {
    tools::toTitleCase(y_var)
  } else {
    "Variable"
  }

  # Determine prefix based on what we're showing
  save_title_prefix <- if (by_facility) {
    "Health facility reporting rate"
  } else if (use_reprate) {
    "reporting rate"
  } else {
    "missing rate"
  }

  title_prefix <- if (by_facility) {
    "Health facility reporting rate of "
  } else if (use_reprate) {
    "Reporting rate of "
  } else {
    "Percentage of HF that reported monthly data by variable out of all HF in the country"
  }

  # Create title components
  title_vars <- if (length(vars_of_interest) <= 5) {
    paste(paste(vars_of_interest, collapse = ", "), "by", x_var)
  } else {
    paste("various variables by", x_var)
  }
  title_suffix <- if (!is.null(y_var)) paste("and", y_var) else ""

  # Call appropriate reporting metric scenario
  plot_data <- calculate_reporting_metrics(
    data = data,
    vars_of_interest = vars_of_interest,
    x_var = x_var,
    y_var = y_var,
    hf_col = if (by_facility) hf_col else NULL,
    key_indicators = key_indicators,
    method = method,
    nonreport_window = nonreport_window,
    reporting_rule = reporting_rule,
    weighting = weighting,
    weight_var = weight_var,
    weight_window = weight_window,
    exclude_current_x = exclude_current_x,
    cold_start = cold_start
  )

  # Convert rates to percentages for plotting
  plot_data <- plot_data |>
    dplyr::mutate(
      reprate = reprate * 100,
      missrate = missrate * 100,
      dplyr::across(
        dplyr::matches("reprate_w|missrate_w"),
        ~ .x * 100
      )
    )

  list(
    plot_data = plot_data,
    vars_of_interest = vars_of_interest,
    fill_var = fill_var,
    fill_label = fill_label,
    y_axis_label = y_axis_label,
    title_prefix = title_prefix,
    title_vars = title_vars,
    title_suffix = title_suffix,
    save_title_prefix = save_title_prefix
  )
}

#' Plot Missing data or Reporting Rate over time
#'
#' This function visualizes the proportion of missing data or reporting rate for
#' specified variables in a dataset. It creates a tile plot where the x-axis can
#' represent any categorical time such as time (e.g., year, month). The function
#' can handle three different scenarios:
#' 1) District-level analysis with specified variables
#' 2) Variable-level analysis without district grouping
#' 3) Facility-level analysis
#'
#' @param data A data frame containing the data to be visualized
#' @param x_var A character string specifying the time variable in 'data'
#'   (e.g., "year", "month"). Must be provided.
#' @param y_var Optional grouping variable name (if any)
#' @param vars_of_interest An optional character vector specifying the variables
#'   to be visualized in 'data'. If NULL, all variables except 'x_var' and
#'   'y_var' will be used.
#' @param hf_col Facility variable name if needed
#' @param key_indicators Optional. Character vector of indicators used to define
#'   facility activity in scenario 1. Defaults to
#'   `c("allout", "conf", "test", "treat", "pres")`.
#' @param use_reprate A logical value. If TRUE, the reporting rate is
#'   visualized; otherwise, the proportion of missing data is visualized.
#'   Defaults to TRUE
#' @param full_range A logical value. If TRUE, the fill scale will use the full
#'   range from 0 to 100. If FALSE, the fill scale will use the range of values
#'   present in the data. Defaults to TRUE.
#' @param weighting Logical. If TRUE, calculate weighted reporting rates based on
#'   facility size. Defaults to FALSE.
#' @param weight_var Character. Column name containing the weight variable
#'   (e.g., "allout" for outpatient volume). Required if weighting = TRUE.
#' @param weight_window Integer. Number of periods for rolling weight calculation.
#'   Defaults to 12.
#' @param exclude_current_x Logical. If TRUE, exclude current period from weight
#'   calculation. Defaults to TRUE.
#' @param cold_start Character. Method for handling initial periods:
#'   "median_within_y" (default) or "median_global".
#' @param method Character or numeric. Classification method for facility activity
#'   status. Can be numeric (1, 2, 3) or character ("method1", "method2", "method3").
#'   Defaults to 3. See \code{\link{classify_facility_activity}} for details.
#' @param nonreport_window Integer. Minimum number of consecutive non-reporting
#'   months to classify a facility as inactive in method 3. Defaults to 6.
#' @param reporting_rule Character. Defines what counts as reporting:
#'   `"any_non_na"` (default, counts NA as non-reporting, 0 counts as reported)
#'   or `"positive_only"` (requires >0 value to count as reported).
#' @param target_language A character string specifying the language for plot
#'   labels. Defaults to "en" (English). Use ISO 639-1 language codes.
#' @param source_language Source language code. If NULL, auto-detection is used.
#'   Defaults to NULL.
#' @param lang_cache_path Path to directory for storing translation cache.
#'   Defaults to tempdir().
#' @param plot_path A character string specifying the path where the plot should
#'   be saved. If NULL (default), plot is not saved.
#' @param compress_image Logical. If TRUE, will compress the saved plot.
#'   Defaults to FALSE
#' @param image_overwrite Logical. If TRUE, will overwrite existing files.
#'   Defaults to TRUE.
#' @param compression_speed Integer. Speed/quality trade-off from 1
#'   (brute-force) to 10 (fastest). Default is 1.
#' @param compression_verbose Logical. Controls output verbosity.
#'   FALSE = silent, TRUE = verbose. Defaults to TRUE.
#' @param plot_scale Numeric. Scaling factor for saved plots. Values > 1
#'   increase size, < 1 decrease size. Default is 1.
#' @param plot_width Numeric. Width of saved plot in inches. If NULL (default),
#'   width is calculated automatically based on data.
#' @param plot_height Numeric. Height of saved plot in inches. If NULL (default),
#'   height is calculated automatically based on data.
#' @param plot_dpi Numeric. Resolution of saved plot in dots per inch.
#'   Default is 300.
#' @param show_plot Logical. If FALSE, the plot is returned invisibly (not displayed).
#'   Useful when only saving plots. Default is TRUE.
#' @param y_axis_label Optional character string for y-axis label. If NULL,
#'   defaults to y_var name or "Variable" for variable scenario.
#' @param ... Additional arguments passed to internal functions.
#' @return A ggplot2 object. When show_plot is FALSE, returns invisibly.
#' @examples
#' # Sample data
#' hf_data <- data.frame(
#'   month = rep(as.Date(c("2024-01-01", "2024-02-01", "2024-03-01")), each = 10),
#'   district = rep(c("North", "South"), each = 5, times = 3),
#'   facility_id = rep(1:5, times = 6),
#'   malaria = c(
#'     10, 0, 15, NA, 8, 12, 0, NA, 7, 9,
#'     11, 0, 14, 6, NA, 13, 8, 10, 0, 12,
#'     9, 7, 0, 11, 14, 8, NA, 12, 10, 15
#'   ),
#'   pneumonia = c(
#'     5, 0, NA, 7, 3, 6, 0, 4, NA, 2,
#'     8, 0, 6, NA, 4, 7, 3, 0, 5, 6,
#'     4, 0, 7, 5, NA, 6, 0, 8, 4, 3
#'   )
#' )
#'
#' # Scenario 1: District-level analysis - reporting rate by district and month
#' reporting_rate_plot(
#'   data = hf_data,
#'   x_var = "month",
#'   y_var = "district",
#'   vars_of_interest = c("malaria", "pneumonia")
#' )
#'
#' # Scenario 2: Variable-level analysis - missing rate by variable over time
#' reporting_rate_plot(
#'   data = hf_data,
#'   x_var = "month",
#'   vars_of_interest = c("malaria", "pneumonia"),
#'   use_reprate = FALSE
#' )
#'
#' # Scenario 3: Facility-level analysis - reporting rate by facility
#' reporting_rate_plot(
#'   data = hf_data,
#'   x_var = "month",
#'   y_var = "district",
#'   vars_of_interest = "malaria",
#'   hf_col = "facility_id"
#' )
#' @export
reporting_rate_plot <- function(data, x_var, y_var = NULL,
                                vars_of_interest = NULL,
                                hf_col = NULL,
                                key_indicators = c("allout", "conf",
                                                  "test", "treat",
                                                  "pres"),
                                method = 3,
                                nonreport_window = 6,
                                reporting_rule = "any_non_na",
                                use_reprate = TRUE,
                                full_range = TRUE,
                                weighting = FALSE,
                                weight_var = NULL,
                                weight_window = 12,
                                exclude_current_x = TRUE,
                                cold_start = "median_within_y",
                                target_language = "en",
                                source_language = "en",
                                lang_cache_path = tempdir(),
                                plot_path = NULL,
                                compress_image = FALSE,
                                image_overwrite = TRUE,
                                compression_speed = 1,
                                compression_verbose = TRUE,
                                plot_scale = 1,
                                plot_width = NULL,
                                plot_height = NULL,
                                plot_dpi = 300,
                                show_plot = TRUE,
                                y_axis_label = NULL,
                                ...) {

  # Normalize method parameter to accept both numeric and character
  method <- .normalize_method(method)

  # Input validation
  if (is.null(x_var) || !x_var %in% names(data)) {
    cli::cli_abort(c(
      "!" = "A valid 'x_var' must be provided and must exist in the data.",
      "i" = "Run `rlang::last_trace()` to see where the error occurred."
    ))
  }


  # Determine the scenario based on input parameters
  scenario <- if (!is.null(hf_col)) {
    "facility"
  } else if (!is.null(y_var)) {
    "district"
  } else {
    "variable"
  }

  # Validate scenario-specific requirements
  if (scenario == "facility") {
    if (is.null(y_var)) {
      cli::cli_abort(c(
        "!" = paste0(
          "For facility-level analysis, both 'hf_col'",
          "and 'y_var' must be provided."
        ),
        "i" = "Run `rlang::last_trace()` to see where the error occurred."
      ))
    }

    if (!is.null(vars_of_interest) && length(vars_of_interest) > 1) {
      cli::cli_abort(c(
        "!" = "Only one variable can be used when by_facility = TRUE",
        "i" = "Run `rlang::last_trace()` to see where the error occurred."
      ))
    }
  }

  # Prepare data and get plotting variables
  prepared_data <- prepare_plot_data(
    data = data,
    x_var = x_var,
    y_var = y_var,
    vars_of_interest = vars_of_interest,
    by_facility = scenario == "facility",
    hf_col = hf_col,
    use_reprate = use_reprate,
    key_indicators = key_indicators,
    method = method,
    nonreport_window = nonreport_window,
    reporting_rule = reporting_rule,
    weighting = weighting,
    weight_var = weight_var,
    weight_window = weight_window,
    exclude_current_x = exclude_current_x,
    cold_start = cold_start
  )

  # Extract prepared data components
  plot_data <- prepared_data$plot_data
  vars_of_interest <- prepared_data$vars_of_interest
  fill_var <- prepared_data$fill_var
  fill_label <- prepared_data$fill_label
  title_prefix <- prepared_data$title_prefix
  save_title_prefix <- prepared_data$save_title_prefix

  # Set default y_axis_label if not provided
  if (is.null(y_axis_label)) {
    y_axis_label <- prepared_data$y_axis_label
  }

  # Create subtitle when doing district/facility level analysis with key_indicators
  subtitle <- NULL
  if (!is.null(y_var) && !is.null(key_indicators) && length(key_indicators) > 0) {
    # Provide proper translations for the base text
    subtitle_base <- switch(target_language,
      "fr" = "Rapports bas\u00e9s sur les indicateurs cl\u00e9s:",
      "es" = "Informes basados en indicadores clave:",
      "pt" = "Relat\u00f3rios baseados em indicadores-chave:",
      # Default to English or use translation service for other languages
      translate_text(
        "Reporting based on key indicators:",
        target_language = target_language,
        source_language = source_language,
        cache_path = lang_cache_path
      )
    )
    # Keep indicator names untranslated and join them
    indicators_display <- paste(key_indicators, collapse = ", ")
    subtitle <- paste(subtitle_base, indicators_display)
  }

  # Set fill scale limits
  fill_limits <- if (full_range) {
    c(0, 100)
  } else {
    fill_var_values <- plot_data[[fill_var]]
    c(
      floor(min(fill_var_values, na.rm = TRUE)),
      ceiling(max(fill_var_values, na.rm = TRUE))
    )
  }

  # Create common plot elements
  common_elements <- create_common_elements(
    fill_var = fill_var,
    fill_limits = fill_limits,
    use_reprate = use_reprate
  )

  # Create plot based on scenario
  plot <- switch(scenario,
    "variable" = variables_plot(
      plot_data = plot_data,
      x_var = x_var,
      vars_of_interest = vars_of_interest,
      fill_var = fill_var,
      fill_label = fill_label,
      title_prefix = title_prefix,
      subtitle = subtitle,
      common_elements = common_elements,
      target_language = target_language,
      source_language = source_language,
      lang_cache_path = lang_cache_path
    ),
    "district" = group_plot(
      plot_data = plot_data,
      x_var = x_var,
      y_var = y_var,
      vars_of_interest = vars_of_interest,
      fill_var = fill_var,
      fill_label = fill_label,
      title_prefix = title_prefix,
      subtitle = subtitle,
      y_axis_label = y_axis_label,
      common_elements = common_elements,
      target_language = target_language,
      source_language = source_language,
      lang_cache_path = lang_cache_path
    ),
    "facility" = group_plot(
      plot_data = plot_data,
      x_var = x_var,
      y_var = y_var,
      vars_of_interest = vars_of_interest,
      fill_var = fill_var,
      fill_label = fill_label,
      title_prefix = title_prefix,
      subtitle = subtitle,
      y_axis_label = y_axis_label,
      common_elements = common_elements,
      target_language = target_language,
      source_language = source_language,
      lang_cache_path = lang_cache_path
    )
  )

  # Save plot if requested
  if (!is.null(plot_path)) {
    compression_options <- list(
      compress_image = compress_image,
      compression_verbose = compression_verbose,
      compression_speed = compression_speed,
      image_overwrite = image_overwrite
    )

    save_single_plot(
      plot = plot,
      plot_data = plot_data,
      plot_path = plot_path,
      x_var = x_var,
      y_var = y_var,
      y_axis_label = y_axis_label,
      vars_of_interest = vars_of_interest,
      target_language = target_language,
      source_language = source_language,
      lang_cache_path = lang_cache_path,
      data = data,
      compression_options = compression_options,
      use_reprate = use_reprate,
      save_title_prefix = save_title_prefix,
      plot_scale = plot_scale,
      plot_width = plot_width,
      plot_height = plot_height,
      plot_dpi = plot_dpi
    )
  }

  # Return invisibly if show_plot is FALSE
  if (show_plot) {
    return(plot)
  } else {
    return(invisible(plot))
  }
}

#' Calculate and visualize reporting rates
#'
#' Calculates and visualizes two key metrics:
#' 1) The reporting/missing rate of variables based on x and y dimensions
#' 2) The proportion of health facilities reporting a given variable
#'
#' @param plot_data The data frame containing summarized health facility data
#' @param x_var The time variable for plotting (e.g., "year", "month")
#' @param vars_of_interest Variables to analyze for reporting rates
#' @param fill_var The column to use for fill values ("reprate" or "missrate")
#' @param fill_label Label for the fill scale
#' @param title_prefix Title prefix based on whether showing reporting or
#'    missing rates
#' @param subtitle Optional subtitle text to display under the title. Default NULL.
#' @param common_elements Common ggplot elements to apply to all plots
#' @param target_language Language code for labels (ISO 639-1), defaults to "en"
#' @param source_language Source language code, defaults to NULL
#' @param lang_cache_path Path for translation cache, defaults to tempdir()
#'
#' @return A ggplot2 object
variables_plot <- function(plot_data, x_var, vars_of_interest,
                           fill_var, fill_label, title_prefix,
                           subtitle = NULL, common_elements,
                           target_language = "en", source_language = "en",
                           lang_cache_path = tempdir()) {
  # Use x_var directly as factor for consistent discrete plotting
  plot_data <- plot_data |>
    dplyr::mutate(
      .x_axis_var = as.factor(.data[[x_var]])
    )

  # Create plot with variables on y-axis
  plot <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = .x_axis_var,
      y = variable, # Use variable names on y-axis
      fill = !!rlang::sym(fill_var)
    )
  ) +
    common_elements +
    ggplot2::labs(
      title = if (grepl("Percentage of HF", title_prefix)) {
        title_prefix  # Use as-is for the new standardized title
      } else {
        paste0(
          title_prefix,
          " selected variables by ", tolower(x_var)
        )
      },
      subtitle = subtitle,
      x = "",
      y = "Variable",
      fill = fill_label
    )

  # Let ggplot2 handle x-axis automatically

  # Translate labels if needed
  if (target_language != "en") {
    plot <- translate_plot_labels(
      plot,
      target_language = target_language,
      source_language = source_language,
      lang_cache_path = lang_cache_path
    )
  }

  plot
}

#' Create plots with a grouping variable on the y-axis
#'
#' Creates reporting rate or missing data plots with a grouping variable on the
#' y-axis and facets for each variable being analyzed.
#'
#' @param plot_data A prepared data frame containing summarized missing data
#'    information
#' @param x_var The time variable name (e.g., "year", "month")
#' @param y_var The grouping variable name (e.g., "district", "state")
#' @param vars_of_interest Variables being visualized for missing data
#' @param fill_var The column to use for fill values ("reprate" or "missrate")
#' @param fill_label Label for the fill scale
#' @param title_prefix Title prefix based on whether showing reporting or
#'    missing rates
#' @param subtitle Optional subtitle text to display under the title. Default NULL.
#' @param y_axis_label Label for the y-axis
#' @param common_elements Common ggplot elements to apply to all plots
#' @param target_language Language code for labels (ISO 639-1)
#' @param source_language Source language code, defaults to NULL
#' @param lang_cache_path Path for translation cache, defaults to tempdir()
#'
#' @return A ggplot2 object
group_plot <- function(plot_data, x_var, y_var, vars_of_interest,
                       fill_var, fill_label, title_prefix,
                       subtitle = NULL, y_axis_label, common_elements,
                       target_language = "en", source_language = "en",
                       lang_cache_path = tempdir()) {
  vars_label <- if (length(vars_of_interest) <= 5) {
    paste(vars_of_interest, collapse = ", ")
  } else {
    "multiple variables"
  }

  # Use x_var directly as factor for consistent discrete plotting
  plot_data <- plot_data |>
    dplyr::mutate(
      .x_axis_var = as.factor(.data[[x_var]])
    )

  # Create plot with grouping variable on y-axis
  plot <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = .x_axis_var,
      y = !!rlang::sym(y_var),
      fill = !!rlang::sym(fill_var)
    )
  ) +
    common_elements +
    ggplot2::labs(
      title = paste0(
        title_prefix,
        " ", vars_label, " by ",
        tolower(x_var), " and ", tolower(y_var)
      ),
      subtitle = subtitle,
      x = "",
      y = y_axis_label,
      fill = fill_label
    )

  # Let ggplot2 handle x-axis automatically

  # Translate labels if needed
  if (target_language != "en") {
    plot <- translate_plot_labels(
      plot,
      target_language = target_language,
      source_language = source_language,
      lang_cache_path = lang_cache_path
    )
  }

  plot
}

#' Save a single plot to a file
#'
#' @param plot The ggplot2 object to save
#' @param plot_data Prepared data frame for dimension calculations
#' @param plot_path Directory path to save the plot
#' @param x_var The time variable name
#' @param y_var The grouping variable name (if any)
#' @param y_axis_label Label for the y-axis
#' @param vars_of_interest Variables being visualized
#' @param target_language Language code for translation (default: "en")
#' @param source_language Source language code (default: NULL for
#'    auto-detection)
#' @param lang_cache_path Path for translation cache (default: tempdir())
#' @param data Original data for extracting year range
#' @param compression_options List with compression settings
#' @param use_reprate A logical value. If TRUE, the reporting rate is
#'   visualized; otherwise, the proportion of missing data is visualized.
#'   Defaults to TRUE
#' @param save_title_prefix A string prefix for the plot title and filename.
#'   If NULL, a default prefix will be used based on the visualization type.
#' @param plot_scale Numeric. Scaling factor for saved plots. Values > 1
#'   increase size, < 1 decrease size. Default is 1.
#' @param plot_width Numeric. Width of saved plot in inches. If NULL (default),
#'   width is calculated based on content.
#' @param plot_height Numeric. Height of saved plot in inches. If NULL (default),
#'   height is calculated based on content.
#' @param plot_dpi Numeric. Resolution of saved plot in dots per inch.
#'   Default is 300.
#'
#' @return Invisible path to the saved file
save_single_plot <- function(plot, plot_data, plot_path,
                             x_var, y_var, y_axis_label,
                             vars_of_interest,
                             target_language = "en",
                             source_language = "en",
                             lang_cache_path = tempdir(),
                             data, compression_options,
                             use_reprate, save_title_prefix,
                             plot_scale = 1,
                             plot_width = NULL,
                             plot_height = NULL,
                             plot_dpi = 300) {
  # Create directory if it doesn't exist
  if (!dir.exists(plot_path)) {
    dir_created <- dir.create(plot_path,
      recursive = TRUE, showWarnings = FALSE
    )
    if (!dir_created) {
      cli::cli_warn("Could not create directory: {plot_path}")
      return(invisible(NULL))
    }
  }

  # Get common translated terms for filenames
  translated_terms <- get_translated_terms(
    target_language = target_language,
    source_language = source_language,
    lang_cache_path = lang_cache_path,
    x_var = x_var,
    vars_of_interest = vars_of_interest,
    save_title_prefix = save_title_prefix,
    data = data
  )

  # Add y_var to filename if provided
  y_var_part <- if (!is.null(y_var)) {
    paste0("_&_", tolower(
      gsub(
        " ", "_",
        translate_text(
          y_var,
          target_language = target_language,
          source_language = source_language,
          cache_path = lang_cache_path
        )
      )
    ))
  } else {
    ""
  }

  # Simplify vars_of_interest for filename if there are too many
  vars_of_interest_str <- if (length(vars_of_interest) > 3) {
    translated_str <- translate_text(
      "multiple variables",
      target_language = target_language,
      source_language = source_language,
      cache_path = lang_cache_path
    )
    translated_str <- tolower(translated_str)
    gsub(" ", "_", translated_str)
  } else {
    tolower(translated_terms$vars_of_interest_str)
  }

  # Construct filename
  save_path <- glue::glue(
    "{translated_terms$prefix}_{translated_terms$for_word}_",
    "{vars_of_interest_str}_{translated_terms$by_word}_",
    "{tolower(translated_terms$x_title)}{y_var_part}_",
    "{translated_terms$year_range}_v{format(Sys.Date(), '%Y-%m-%d')}.png"
  )

  full_path <- file.path(plot_path, save_path)

  # Calculate dimensions or use provided values
  if (is.null(plot_width) || is.null(plot_height)) {
    dims <- calculate_plot_dimensions(plot_data, x_var, y_var)
    width <- if (is.null(plot_width)) dims$width else plot_width
    height <- if (is.null(plot_height)) dims$height else plot_height
  } else {
    width <- plot_width
    height <- plot_height
  }

  # Try to save the plot
  tryCatch(
    {
      ggplot2::ggsave(
        filename = full_path,
        plot = plot,
        width = width,
        height = height,
        dpi = plot_dpi,
        scale = plot_scale
      )

      # Close device to prevent warnings
      if (grDevices::dev.cur() > 1) {
        grDevices::dev.off()
      }

      # Compress if requested
      if (compression_options$compress_image && file.exists(full_path)) {
        compress_png(
          full_path,
          verbosity = compression_options$compression_verbose,
          speed = compression_options$compression_speed,
          png_overwrite = compression_options$image_overwrite
        )
      }

      success_msg <- translate_text(
        "Plot saved to:",
        target_language = target_language,
        source_language = source_language,
        cache_path = lang_cache_path
      )
      # Show only relative path from current directory if it's a subdirectory
      display_path <- full_path
      if (startsWith(full_path, getwd())) {
        display_path <- sub(paste0("^", getwd(), "/"), "", full_path)
      } else if (grepl("03_outputs", full_path)) {
        # Extract from 03_outputs onward if present
        display_path <- sub(".*/(03_outputs/.*)", "\\1", full_path)
      }
      cli::cli_alert_success(paste(success_msg, display_path))
    },
    error = function(e) {
      # Close device on error
      if (grDevices::dev.cur() > 1) {
        grDevices::dev.off()
      }
      cli::cli_warn("Failed to save plot to {full_path}: {e$message}")
    }
  )

  invisible(full_path)
}

#' Calculate appropriate plot dimensions based on data
#'
#' @param plot_data Data frame with the plotted data
#' @param x_var X-axis variable
#' @param y_var Y-axis variable (can be NULL)
#'
#' @return List with width and height values
calculate_plot_dimensions <- function(plot_data, x_var, y_var = NULL) {
  # Base dimensions
  base_width <- 8
  base_height <- 6

  # Get count of unique x values
  x_count <- length(unique(plot_data[[x_var]]))

  # Calculate width based on x values
  width <- max(base_width, x_count * 0.3)

  # Calculate height based on y values
  if (!is.null(y_var) && y_var %in% names(plot_data)) {
    # When y_var is specified (e.g., "state" or "facility")
    y_count <- length(unique(plot_data[[y_var]]))
    height <- max(base_height, y_count * 0.3)
  } else {
    # When using variables on y-axis (y_var is NULL)
    if ("variable" %in% names(plot_data)) {
      var_count <- length(unique(plot_data[["variable"]]))
      height <- max(base_height, var_count * 0.3)
    } else {
      height <- base_height
    }
  }

  # Return dimensions
  list(width = width, height = height)
}
#' Get translated terms for plot filenames
#'
#' @param target_language Target language code
#' @param source_language Source language code
#' @param lang_cache_path Path for translation cache
#' @param x_var X-axis variable
#' @param vars_of_interest Variables being visualized
#' @param save_title_prefix A string prefix for the plot title and filename.
#'   If NULL, a default prefix will be used based on the visualization type.
#' @param data Original data for year range
#'
#' @return List of translated terms
get_translated_terms <- function(target_language, source_language,
                                 lang_cache_path, x_var,
                                 vars_of_interest, save_title_prefix, data) {
  # Translate and format the prefix
  save_title_prefix_tr <- translate_text(
    save_title_prefix,
    target_language = target_language,
    source_language = source_language,
    cache_path = lang_cache_path
  )
  save_title_prefix_tr <- tolower(save_title_prefix_tr)
  save_title_prefix_tr <- gsub(" ", "_", save_title_prefix_tr)

  # Format x_var for title
  x_title <- if (x_var == "yearmon") "year and month" else x_var
  x_title <- translate_text(
    x_title,
    target_language = target_language,
    source_language = source_language,
    cache_path = lang_cache_path
  )
  x_title <- tolower(x_title)
  x_title <- gsub(" ", "_", x_title)

  # Translate common words
  for_word <- translate_text(
    "for",
    target_language = target_language,
    source_language = source_language,
    cache_path = lang_cache_path
  )
  by_word <- translate_text(
    "by",
    target_language = target_language,
    source_language = source_language,
    cache_path = lang_cache_path
  )
  in_word <- translate_text(
    "in",
    target_language = target_language,
    source_language = source_language,
    cache_path = lang_cache_path
  )

  # Format vars_of_interest for filename
  vars_of_interest_str <- if (length(vars_of_interest) > 1) {
    paste(vars_of_interest, collapse = "_")
  } else {
    vars_of_interest
  }

  # Get year range
  year_range <- if (!is.null(data$year) && length(unique(data$year)) > 1) {
    glue::glue(
      "{min(data$year, na.rm = TRUE)}-{max(data$year, na.rm = TRUE)}"
    )
  } else if (!is.null(data$year)) {
    as.character(min(data$year, na.rm = TRUE))
  } else {
    format(Sys.Date(), "%Y")
  }

  list(
    prefix = save_title_prefix_tr,
    for_word = for_word,
    by_word = by_word,
    in_word = in_word,
    vars_of_interest_str = vars_of_interest_str,
    x_title = x_title,
    year_range = year_range
  )
}

#' Translate plot labels to specified language
#'
#' @param plot A ggplot2 object
#' @param target_language Target language code
#' @param source_language Source language code (default: NULL)
#' @param lang_cache_path Path for translation cache (default: tempdir())
#'
#' @return The plot with translated labels
translate_plot_labels <- function(plot, target_language,
                                  source_language = "en",
                                  lang_cache_path = tempdir()) {
  # get gtranslate if missing
  ensure_packages("gtranslate")

  # Extract labels from the plot
  plot_labs <- plot$labels

  # Translate each label if it exists
  for (lab_name in c("title", "subtitle", "x", "y", "fill", "color")) {
    if (!is.null(plot_labs[[lab_name]]) && plot_labs[[lab_name]] != "") {
      # Special handling for markdown-formatted title
      if (lab_name == "title") {
        manual_title <- .manual_title_translation(
          plot_labs[[lab_name]],
          target_language = target_language,
          source_language = source_language,
          lang_cache_path = lang_cache_path
        )

        orig_title <- if (!is.null(manual_title)) {
          manual_title
        } else {
          translate_text(
            plot_labs[[lab_name]],
            target_language = target_language,
            source_language = source_language,
            cache_path = lang_cache_path
          )
        }

        # Apply title case - capitalize first letter
        orig_title <- paste0(
          toupper(substring(orig_title, 1, 1)),
          substring(orig_title, 2)
        )

        plot_labs[[lab_name]] <- gsub(
          "\\*\\*\\s+(.*?)\\s+\\*\\*", "**\\1**",
          orig_title
        )
      } else if (lab_name == "subtitle") {
        # Skip translating subtitle to keep manual translations.
        # This preserves variable names for readability.
      } else {
        # Standard translation for other labels
        translated_text <- translate_text(
          plot_labs[[lab_name]],
          target_language = target_language,
          source_language = source_language,
          cache_path = lang_cache_path
        )

        # Apply title case for fill label (legend title)
        if (lab_name == "fill") {
          # Convert first letter to uppercase
          translated_text <- paste0(
            toupper(substring(translated_text, 1, 1)),
            substring(translated_text, 2)
          )
        }

        plot_labs[[lab_name]] <- translated_text
      }
    }
  }

  # Update the plot with translated labels
  plot + do.call(ggplot2::labs, plot_labs)
}

# helper: manual tweaks for tricky title translations
#' @noRd
.manual_title_translation <- function(title_text,
                                      target_language,
                                      source_language = "en",
                                      lang_cache_path = tempdir()) {
  if (target_language != "fr") {
    return(NULL)
  }

  pattern <- "^Reporting rate of (.+) by (.+) and (.+)$"
  match <- base::regexec(pattern, title_text)
  captured <- base::regmatches(title_text, match)[[1]]
  if (base::length(captured) != 4L) {
    return(NULL)
  }

  variable_en <- captured[[2]]
  x_en <- captured[[3]]
  y_en <- captured[[4]]

  variable_fr <- translate_text(
    variable_en,
    target_language = target_language,
    source_language = source_language,
    cache_path = lang_cache_path
  )
  x_fr <- translate_text(
    x_en,
    target_language = target_language,
    source_language = source_language,
    cache_path = lang_cache_path
  )
  y_fr <- translate_text(
    y_en,
    target_language = target_language,
    source_language = source_language,
    cache_path = lang_cache_path
  )

  # choose article based on leading vowel/consonant
  variable_clean <- trimws(variable_fr)
  has_article <- grepl(
    "^(la|le|les|l'|du|de la|des|de l')\\s",
    tolower(variable_clean)
  )
  leading_vowel <- grepl("^[aeiouh]", tolower(variable_clean))
  article <- if (leading_vowel) "de l'" else "du "
  variable_phrase <- if (has_article) {
    variable_clean
  } else {
    paste0(article, variable_clean)
  }

  french_title <- paste(
    "Taux de rapport",
    variable_phrase,
    "par",
    trimws(x_fr),
    "et",
    trimws(y_fr)
  )

  french_title <- gsub("\\s+", " ", french_title)
  french_title <- gsub("de l' ", "de l'", french_title, fixed = TRUE)
  trimws(french_title)
}

#' Create common ggplot elements
#'
#' @param fill_var Fill variable name
#' @param fill_limits Limits for the fill scale
#' @param use_reprate Whether to use reporting rate colors
#'
#' @return List of ggplot elements to apply to plots
create_common_elements <- function(fill_var, fill_limits, use_reprate = TRUE) {
  # Set up plot aesthetics
  color_pal <- if (use_reprate) {
    if (requireNamespace("wesanderson", quietly = TRUE)) {
      rev(wesanderson::wes_palette("Zissou1", 100, type = "continuous"))
    } else {
      rev(grDevices::heat.colors(100))
    }
  } else {
    if (requireNamespace("wesanderson", quietly = TRUE)) {
      wesanderson::wes_palette("Zissou1", 100, type = "continuous")
    } else {
      grDevices::heat.colors(100)
    }
  }

  # Define common theme elements
  base_theme <- ggplot2::theme_bw() +
    ggplot2::theme(
      legend.title = ggplot2::element_text(
        size = 12,
        face = "bold",
        family = "sans"
      ),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "horizontal",
      legend.box.just = "center",
      legend.margin = ggplot2::margin(t = 0, unit = "cm"),
      legend.text = ggplot2::element_text(
        size = 8,
        family = "sans"
      ),
      axis.title.x = ggplot2::element_text(
        margin = ggplot2::margin(t = 5, unit = "pt")
      ),
      axis.title.y = ggplot2::element_text(
        margin = ggplot2::margin(r = 10, unit = "pt")
      ),
      axis.text.x = ggplot2::element_text(
        angle = 75,
        hjust = 1,
        family = "sans"
      ),
      axis.text = ggplot2::element_text(family = "sans"),
      axis.title = ggplot2::element_text(family = "sans"),
      plot.title = ggtext::element_markdown(
        size = 12,
        family = "sans",
        margin = ggplot2::margin(b = 10)
      ),
      strip.text = ggplot2::element_text(
        family = "sans",
        face = "bold"
      ),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      strip.background = ggplot2::element_rect(fill = "grey90")
    )

  # Define common plot elements
  common_elements <- list(
    ggplot2::geom_tile(colour = "white", linewidth = .2),
    ggplot2::scale_fill_gradientn(
      colours = color_pal,
      limits = fill_limits
    ),
    ggplot2::scale_y_discrete(expand = c(0, 0)),
    base_theme,
    ggplot2::guides(
      fill = ggplot2::guide_colorbar(
        title.position = "top",
        label.position = "bottom",
        direction = "horizontal",
        barheight = ggplot2::unit(0.3, "cm"),
        barwidth = ggplot2::unit(4, "cm"),
        ticks = TRUE,
        draw.ulim = TRUE,
        draw.llim = TRUE
      )
    )
  )

  # Let ggplot2 handle x-axis scale automatically

  common_elements
}

#' Plot reporting rate maps over time
#'
#' @description
#' Creates faceted maps of reporting rates (or missing rates) by administrative
#' unit and time. Designed to complement `reporting_rate_plot()` with consistent
#' styling and color schemes. The function displays the variables of interest in
#' both the title and subtitle for clarity.
#'
#' @param data Data frame of health facility data.
#' @param shapefile sf object containing administrative boundaries (adm1 or adm2).
#' @param x_var Character. Time variable (e.g., "yearmon", "year", "month").
#' @param adm_var Character. Administrative variable in both `data` and `shapefile`
#'   (e.g., "adm1" or "adm2").
#' @param vars_of_interest Character vector. Variable(s) to compute reporting rates for.
#'   These will be displayed in the plot title and subtitle.
#' @param hf_col Character. Health facility ID column, if required. Default is NULL.
#' @param use_reprate Logical. If TRUE, plot reporting rate (facilities that reported);
#'   if FALSE, plot missing rate (facilities that didn't report). Default is TRUE.
#' @param full_range Logical. If TRUE, the fill scale will use the full range from 0 to 100.
#'   If FALSE, the fill scale will use the range of values present in the data. Default is TRUE.
#' @param weighting Logical. If TRUE, compute weighted reporting rates based on
#'   facility size. Default is FALSE.
#' @param weight_var Character. Weight variable if weighting = TRUE.
#' @param weight_window Integer. Number of periods for typical size.
#'   Default is 12.
#' @param exclude_current_x Logical. Exclude current period in weighting.
#'   Default is TRUE.
#' @param cold_start Character. Cold-start strategy ("median_within_y" or "median_global").
#'   Default is "median_within_y".
#' @param method Character or numeric. Classification method for facility activity
#'   status. Can be numeric (1, 2, 3) or character ("method1", "method2", "method3").
#'   Defaults to 3. See \code{\link{classify_facility_activity}} for details.
#' @param nonreport_window Integer. Minimum number of consecutive non-reporting
#'   months to classify a facility as inactive in method 3. Defaults to 6.
#' @param reporting_rule Character. Defines what counts as reporting:
#'   `"any_non_na"` (default, counts NA as non-reporting, 0 counts as reported)
#'   or `"positive_only"` (requires >0 value to count as reported).
#' @param fill_palette Character. Not used - kept for backward compatibility.
#'   Color palette is automatically selected based on use_reprate parameter
#'   to match reporting_rate_plot() styling.
#' @param facet_label Character. Optional custom label for the administrative level
#'   in the title. If NULL, uses adm_var value. Default is NULL.
#' @param facet_ncol Integer. Number of facet columns. Default is 4.
#' @param target_language Character. ISO 639-1 code for translation. Default "en".
#' @param source_language Character. Source language for translation. Default "en".
#' @param lang_cache_path Path to directory for storing translation cache.
#'   Default is tempdir().
#' @param plot_path Character. Path to save the plot. Can be either a directory path
#'   (filename will be auto-generated) or a full file path ending in .png.
#'   If NULL, plot is not saved.
#' @param compress_image Logical. If TRUE, compress the saved plot.
#'   Default is FALSE.
#' @param image_overwrite Logical. If TRUE, overwrite existing files.
#'   Default is TRUE.
#' @param compression_speed Integer. Speed/quality trade-off from 1
#'   (brute-force) to 10 (fastest). Default is 1.
#' @param compression_verbose Logical. Controls output verbosity.
#'   FALSE = silent, TRUE = verbose. Default is TRUE.
#' @param plot_scale Numeric. Scaling factor for saved plots. Values > 1
#'   increase size, < 1 decrease size. Default is 1.
#' @param plot_width Numeric. Width of saved plot in inches. If NULL,
#'   width is calculated automatically based on data.
#' @param plot_height Numeric. Height of saved plot in inches. If NULL,
#'   height is calculated automatically based on data.
#' @param plot_dpi Numeric. Resolution of saved plot in dots per inch.
#'   Default is 300.
#' @param show_plot Logical. Display plot (TRUE) or return invisibly (FALSE).
#'   Default is TRUE.
#' @param ... Additional arguments passed to internal functions.
#'
#' @return A ggplot2 object.
#'
#' @examples
#' \dontrun{
#' # Example: adm1-level map over months
#' reporting_rate_map(
#'   data = hf_data,
#'   shapefile = adm1_sf,
#'   x_var = "month",
#'   adm_var = "district",
#'   vars_of_interest = "malaria"
#' )
#' }
#' @export
reporting_rate_map <- function(
  data,
  shapefile,
  x_var,
  adm_var,
  vars_of_interest,
  hf_col = NULL,
  use_reprate = TRUE,
  full_range = TRUE,
  method = 3,
  nonreport_window = 6,
  reporting_rule = "any_non_na",
  weighting = FALSE,
  weight_var = NULL,
  weight_window = 12,
  exclude_current_x = TRUE,
  cold_start = "median_within_y",
  fill_palette = "Zissou1",
  facet_label = NULL,
  facet_ncol = 4,
  target_language = "en",
  source_language = "en",
  lang_cache_path = tempdir(),
  plot_path = NULL,
  compress_image = FALSE,
  image_overwrite = TRUE,
  compression_speed = 1,
  compression_verbose = TRUE,
  plot_scale = 1,
  plot_width = NULL,
  plot_height = NULL,
  plot_dpi = 300,
  show_plot = TRUE,
  ...
) {
  # Normalize method parameter to accept both numeric and character
  method <- .normalize_method(method)

  # Ensure required packages
  ensure_packages(c("ggplot2", "sf"))

  # Validate inputs
  if (!inherits(shapefile, "sf")) {
    cli::cli_abort("'shapefile' must be an sf object")
  }

  if (!adm_var %in% names(shapefile)) {
    cli::cli_abort("'{adm_var}' not found in shapefile")
  }

  if (!adm_var %in% names(data)) {
    cli::cli_abort("'{adm_var}' not found in data")
  }

  # Prepare summarized data using calculate_reporting_metrics
  summary_data <- calculate_reporting_metrics(
    data = data,
    vars_of_interest = vars_of_interest,
    x_var = x_var,
    y_var = adm_var,
    hf_col = hf_col,
    method = method,
    nonreport_window = nonreport_window,
    reporting_rule = reporting_rule,
    weighting = weighting,
    weight_var = weight_var,
    weight_window = weight_window,
    exclude_current_x = exclude_current_x,
    cold_start = cold_start
  )

  # Determine which rate variable to use
  rate_var <- if (use_reprate) {
    if (weighting) "reprate_w" else "reprate"
  } else {
    if (weighting) "missrate_w" else "missrate"
  }

  # Join shapefile and reporting data
  merged <- shapefile |>
    dplyr::left_join(
      summary_data, by = dplyr::join_by(!!rlang::sym(adm_var)))

  # Ensure rate is in percentage
  merged <- merged |>
    dplyr::mutate(
      !!rate_var := !!rlang::sym(rate_var) * 100,
      time_factor = forcats::as_factor(.data[[x_var]])
    )

  # Palette setup - match reporting_rate_plot color scheme
  color_pal <- if (use_reprate) {
    if (requireNamespace("wesanderson", quietly = TRUE)) {
      rev(wesanderson::wes_palette("Zissou1", 100, type = "continuous"))
    } else {
      rev(grDevices::heat.colors(100))
    }
  } else {
    if (requireNamespace("wesanderson", quietly = TRUE)) {
      wesanderson::wes_palette("Zissou1", 100, type = "continuous")
    } else {
      grDevices::heat.colors(100)
    }
  }

  # Translate labels if needed
  should_translate <- target_language != "en"

  # Create title prefix matching reporting_rate_plot style
  title_prefix <- if (use_reprate) {
    "Reporting rate of"
  } else {
    "The proportion of missing data for"
  }

  fill_label <- if (use_reprate) {
    if (weighting) {
      "Weighted reporting rate (%)"
    } else {
      "Reporting rate (%)"
    }
  } else {
    if (weighting) {
      "Weighted missing rate (%)"
    } else {
      "Missing rate (%)"
    }
  }

  by_text <- "by"
  and_text <- "and"

  # Use facet_label if provided, otherwise use adm_var
  admin_label <- if (!is.null(facet_label)) facet_label else adm_var

  if (should_translate) {
    ensure_packages("gtranslate")

    title_prefix <- translate_text(
      title_prefix,
      target_language = target_language,
      source_language = source_language,
      cache_path = lang_cache_path
    )

    fill_label <- translate_text(
      fill_label,
      target_language = target_language,
      source_language = source_language,
      cache_path = lang_cache_path
    )

    by_text <- translate_text(
      by_text,
      target_language = target_language,
      source_language = source_language,
      cache_path = lang_cache_path
    )

    and_text <- translate_text(
      and_text,
      target_language = target_language,
      source_language = source_language,
      cache_path = lang_cache_path
    )

    # Translate admin_label if it's a common term and facet_label was provided
    if (!is.null(facet_label)) {
      admin_label <- translate_text(
        admin_label,
        target_language = target_language,
        source_language = source_language,
        cache_path = lang_cache_path
      )
    }
  }

  # Create title components matching reporting_rate_plot style
  vars_display <- if (length(vars_of_interest) <= 5) {
    paste(vars_of_interest, collapse = ", ")
  } else {
    paste0(length(vars_of_interest), " variables")
  }

  # Build title in the same format as reporting_rate_plot
  title_vars <- paste(vars_display, by_text, x_var)
  title_suffix <- paste(and_text, admin_label)

  # Create full title
  title_full <- paste(title_prefix, title_vars, title_suffix)

  # Apply sentence case to the title prefix only, preserve other capitalizations
  # This ensures proper nouns and variable names keep their case
  if (grepl("^[A-Z]", title_prefix)) {
    # Split the title into components
    title_parts <- strsplit(title_full, " ")[[1]]

    # Find where title_prefix ends
    prefix_parts <- strsplit(title_prefix, " ")[[1]]
    prefix_length <- length(prefix_parts)

    # Apply sentence case only to the prefix parts
    for (i in 1:min(prefix_length, length(title_parts))) {
      if (i == 1) {
        # First word stays capitalized
        title_parts[i] <- paste0(toupper(substring(title_parts[i], 1, 1)),
                                 substring(title_parts[i], 2))
      } else {
        # Other prefix words become lowercase
        title_parts[i] <- tolower(title_parts[i])
      }
    }

    # Reconstruct the title
    title_full <- paste(title_parts, collapse = " ")
  }


  # Set fill scale limits based on full_range parameter
  fill_limits <- if (full_range) {
    c(0, 100)
  } else {
    # Calculate limits from the actual data
    rate_values <- merged[[rate_var]]
    c(
      floor(min(rate_values, na.rm = TRUE)),
      ceiling(max(rate_values, na.rm = TRUE))
    )
  }

  # Create plot
  p <- ggplot2::ggplot(merged) +
    ggplot2::geom_sf(
      ggplot2::aes(fill = .data[[rate_var]]),
      color = "white",
      size = 0.2
    ) +
    ggplot2::facet_wrap(
      ~time_factor,
      ncol = facet_ncol,
      labeller = ggplot2::labeller(
        time_factor = function(x) {
          if (x_var %in% c("yearmon", "month", "date")) {
            # Try to parse as date and use translate_yearmon
            dates <- tryCatch(
              as.Date(as.character(x)),
              error = function(e) NA
            )
            if (!any(is.na(dates))) {
              return(sntutils::translate_yearmon(
                dates,
                language = target_language
              ))
            }
          }
          return(as.character(x))
        }
      )
    ) +
    ggplot2::scale_fill_gradientn(
      colours = color_pal,
      limits = fill_limits,
      na.value = "grey90",
      name = fill_label
    ) +
    ggplot2::labs(
      title = stringr::str_to_sentence(title_full),
      fill = stringr::str_to_sentence(fill_label)
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      # Legend styling to match create_common_elements exactly
      legend.title = ggplot2::element_text(
        size = 12,
        face = "bold",
        family = "sans"
      ),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "horizontal",
      legend.box.just = "center",
      legend.margin = ggplot2::margin(t = 0, unit = "cm"),
      legend.text = ggplot2::element_text(
        size = 8,
        family = "sans"
      ),
      # Title styling - use element_markdown to match reporting_rate.R
      plot.title = ggtext::element_markdown(
        size = 12,
        family = "sans",
        margin = ggplot2::margin(b = 10)
      ),
      # Map-specific elements (no axis for maps)
      axis.text = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      # Grid styling to match reporting_rate.R
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      # Facet strip styling to match reporting_rate.R exactly
      strip.text = ggplot2::element_text(
        family = "sans",
        face = "bold"
      ),
      # Panel spacing
      panel.spacing = grid::unit(0.5, "lines")
    ) +
    # Guide colorbar to match create_common_elements exactly
    ggplot2::guides(
      fill = ggplot2::guide_colorbar(
        title.position = "top",
        label.position = "bottom",
        direction = "horizontal",
        barheight = ggplot2::unit(0.3, "cm"),
        barwidth = ggplot2::unit(4.5, "cm"),
        ticks = TRUE,
        draw.ulim = TRUE,
        draw.llim = TRUE
      )
    )

  # Save plot if requested
  if (!is.null(plot_path)) {
    # Check if plot_path is directory or file
    is_directory <- !grepl("\\.png$", plot_path, ignore.case = TRUE)

    if (is_directory) {
      # Create directory if needed
      if (!dir.exists(plot_path)) {
        dir_created <- dir.create(plot_path, recursive = TRUE, showWarnings = FALSE)
        if (!dir_created) {
          cli::cli_warn("Could not create directory: {plot_path}")
          return(invisible(p))
        }
      }

      # Get translated terms for filename construction
      # Translate save title prefix
      save_title_prefix <- if (use_reprate) {
        "reporting rate map"
      } else {
        "missing rate map"
      }

      save_title_prefix_tr <- translate_text(
        save_title_prefix,
        target_language = target_language,
        source_language = source_language,
        cache_path = lang_cache_path
      )
      save_title_prefix_tr <- tolower(save_title_prefix_tr)
      save_title_prefix_tr <- gsub(" ", "_", save_title_prefix_tr)

      # Translate common words for filename
      for_word <- translate_text(
        "for",
        target_language = target_language,
        source_language = source_language,
        cache_path = lang_cache_path
      )

      by_word <- translate_text(
        "by",
        target_language = target_language,
        source_language = source_language,
        cache_path = lang_cache_path
      )

      # Format x_var for filename
      x_title <- if (x_var == "yearmon") "year_and_month" else x_var
      x_title <- translate_text(
        x_title,
        target_language = target_language,
        source_language = source_language,
        cache_path = lang_cache_path
      )
      x_title <- tolower(gsub(" ", "_", x_title))

      # Format vars for filename
      vars_str <- if (length(vars_of_interest) > 3) {
        translated_str <- translate_text(
          "multiple_variables",
          target_language = target_language,
          source_language = source_language,
          cache_path = lang_cache_path
        )
        tolower(gsub(" ", "_", translated_str))
      } else {
        tolower(paste(vars_of_interest, collapse = "_"))
      }

      # Add admin level to filename
      admin_part <- paste0("_", tolower(gsub(" ", "_", admin_label)))

      # Get year range from data
      year_range <- tryCatch({
        if ("year" %in% names(data) && length(unique(data$year)) > 1) {
          paste0(min(data$year, na.rm = TRUE), "-", max(data$year, na.rm = TRUE))
        } else if ("year" %in% names(data)) {
          as.character(min(data$year, na.rm = TRUE))
        } else {
          # Try to extract year from x_var values
          years <- unique(format(as.Date(merged[[x_var]]), "%Y"))
          if (length(years) > 1) {
            paste0(min(years), "-", max(years))
          } else {
            years[1]
          }
        }
      }, error = function(e) {
        format(Sys.Date(), "%Y")
      })

      # Construct filename
      filename <- sprintf(
        "%s_%s_%s_%s_%s%s_%s_v%s.png",
        save_title_prefix_tr,
        for_word,
        vars_str,
        by_word,
        x_title,
        admin_part,
        year_range,
        format(Sys.Date(), "%Y-%m-%d")
      )

      full_path <- file.path(plot_path, filename)
    } else {
      # plot_path is already a full file path
      full_path <- plot_path
      plot_dir <- dirname(full_path)
      if (!dir.exists(plot_dir)) {
        dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)
      }
    }

    # Calculate plot dimensions if not provided
    if (is.null(plot_width)) {
      n_facets <- length(unique(merged$time_factor))
      plot_width <- min(20, max(10, 4 * min(facet_ncol, n_facets)))
    }

    if (is.null(plot_height)) {
      n_facets <- length(unique(merged$time_factor))
      n_rows <- ceiling(n_facets / facet_ncol)
      plot_height <- min(15, max(8, 3 * n_rows))
    }

    # Save plot
    tryCatch({
      ggplot2::ggsave(
        filename = full_path,
        plot = p,
        width = plot_width,
        height = plot_height,
        dpi = plot_dpi,
        scale = plot_scale,
        limitsize = FALSE
      )

      # Close device to prevent warnings
      if (grDevices::dev.cur() > 1) {
        grDevices::dev.off()
      }

      # Compress if requested
      if (compress_image && endsWith(tolower(full_path), ".png")) {
        if (requireNamespace("R.utils", quietly = TRUE)) {
          compress_png(
            path = full_path,
            png_overwrite = image_overwrite,
            speed = compression_speed,
            verbosity = compression_verbose
          )
        } else {
          cli::cli_warn("R.utils package required for compression. Skipping compression.")
        }
      }

      # Success message
      success_msg <- translate_text(
        "Plot saved to:",
        target_language = target_language,
        source_language = source_language,
        cache_path = lang_cache_path
      )

      # Show only relative path from current directory if it's a subdirectory
      display_path <- full_path
      if (startsWith(full_path, getwd())) {
        display_path <- sub(paste0("^", getwd(), "/"), "", full_path)
      } else if (grepl("03_outputs", full_path)) {
        # Extract from 03_outputs onward if present
        display_path <- sub(".*/(03_outputs/.*)", "\\1", full_path)
      }

      cli::cli_alert_success(paste(success_msg, display_path))
    }, error = function(e) {
      # Close device on error
      if (grDevices::dev.cur() > 1) {
        grDevices::dev.off()
      }
      cli::cli_warn("Failed to save plot to {full_path}: {e$message}")
    })
  }

  # Return plot
  if (show_plot) {
    return(p)
  } else {
    return(invisible(p))
  }
}
