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
#' Note: By default, 0 values are treated as missing (`NA`) in both
#' `vars_of_interest` and `key_indicators`, unless `na_to_zero = FALSE`.
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
#' @param na_to_zero Logical. If TRUE (default), treat zero values as missing
#'   (`NA`) in both `vars_of_interest` and `key_indicators`.
#'
#' @return A tibble with the number of reporting (`rep`) and expected (`exp`)
#' facilities or records, and the computed `reprate` and `missrate`.
#'
#' @examples
#' hf_data <- data.frame(
#'   month = rep(c("Jan", "Feb", "Mar"), each = 10),
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
#' # any data
#' calculate_reporting_metrics(
#'   data = hf_data,
#'   vars_of_interest = c("conf"),
#'   x_var = "month",
#'   y_var = "district",
#'   hf_col = "facility_id",
#'   key_indicators = c("allout", "conf", "pres")
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
calculate_reporting_metrics <- function(
  data,
  vars_of_interest,
  x_var,
  y_var = NULL,
  hf_col = NULL,
  key_indicators = c("allout", "conf", "test", "treat", "pres"),
  na_to_zero = TRUE
) {
  # ensure_packages("dtplyr")

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

  # Treat 0s as NA
  if (na_to_zero) {
    if (!is.null(key_indicators)) {
      data <- dplyr::mutate(
        data,
        dplyr::across(
          dplyr::any_of(key_indicators),
          ~ dplyr::if_else(.x == 0, NA, .x)
        )
      )
    }

    data <- dplyr::mutate(
      data,
      dplyr::across(
        dplyr::any_of(vars_of_interest),
        ~ dplyr::if_else(.x == 0, NA_real_, .x)
      )
    )
  }

  calculate_rates <- function(df) {
    df |>
      dplyr::mutate(
        reprate = (rep / exp) * 100,
        missrate = ((exp - rep) / exp) * 100
      )
  }

  if (!is.null(hf_col) && !is.null(key_indicators)) {

    # Reported any for key_indicators
    data <- data |>
      dplyr::mutate(
        reported_any = dplyr::if_any(
          dplyr::any_of(key_indicators),
          ~ !is.na(.x)
        )
      )

    # Get first reporting month per HF
    first_reporting <- data |>
      dplyr::filter(reported_any) |>
      dplyr::group_by(dplyr::across(dplyr::any_of(hf_col))) |>
      dplyr::summarise(
        first_report_month = min(.data[[x_var]], na.rm = TRUE),
        .groups = "drop"
      )

    # Join and filter only those eligible for that month
    data <- data |>
      dplyr::left_join(first_reporting, by = hf_col) |>
      dplyr::mutate(include_in_denom = first_report_month <= .data[[x_var]])

    # Aggregate
    result <- data |>
      dplyr::filter(include_in_denom) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(c(x_var, y_var)))) |>
      dplyr::summarise(
        rep = sum(reported_any, na.rm = TRUE),
        exp = dplyr::n_distinct(.data[[hf_col]]),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        reprate = (rep / exp) * 100,
        missrate = 100 - reprate
      )
  } else if (!is.null(y_var)) {
    long_data <- data |>
      dplyr::select(
        dplyr::all_of(c(x_var, y_var, vars_of_interest))
      ) |>
      tidyr::pivot_longer(
        cols = dplyr::all_of(vars_of_interest),
        names_to = "variable",
        values_to = "value"
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
#' @param na_to_zero Logical. If TRUE (default), treat zero values as missing
#'   (`NA`) in both `vars_of_interest` and `key_indicators`.
#'
#' @return A list with plot_data and plotting metadata
#' @examples
#' # Sample data
#' hf_data <- data.frame(
#'   month = rep(c("Jan", "Feb", "Mar"), each = 10),
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
  na_to_zero = TRUE
) {

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
  # reporting/missing rate choice
  fill_var <- if (use_reprate) {
    "reprate"
  } else {
    "missrate"
  }
  fill_label <- if (use_reprate) {
    "Reporting rate (%)"
  } else {
    "Missing rate (%)"
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
    "The proportion of missing data for "
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
    na_to_zero = na_to_zero
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
#' @param na_to_zero Logical. If TRUE (default), treat zero values as missing
#'   (`NA`) in both `vars_of_interest` and `key_indicators`.
#' @param use_reprate A logical value. If TRUE, the reporting rate is
#'   visualized; otherwise, the proportion of missing data is visualized.
#'   Defaults to TRUE
#' @param full_range A logical value. If TRUE, the fill scale will use the full
#'   range from 0 to 100. If FALSE, the fill scale will use the range of values
#'   present in the data. Defaults to TRUE.
#' @param target_language A character string specifying the language for plot
#'   labels. Defaults to "en" (English). Use ISO 639-1 language codes.
#' @param source_language Source language code. If NULL, auto-detection is used.
#'   Defaults to NULL.
#' @param lang_cache_path Path to directory for storing translation cache.
#'   Defaults to tempdir().
#' @param save_plot A logical value. If TRUE, the plot will be saved to the
#'   specified path. Defaults to FALSE.
#' @param plot_path A character string specifying the path where the plot should
#'   be saved. Required if save_plot is TRUE.
#' @param compress_image Logical. If TRUE, will compress the saved plot.
#'   Defaults to FALSE
#' @param image_overwrite Logical. If TRUE, will overwrite existing files.
#'   Defaults to TRUE.
#' @param compression_speed Integer. Speed/quality trade-off from 1
#'   (brute-force) to 10 (fastest). Default is 1.
#' @param compression_verbose Logical. Controls output verbosity.
#'   FALSE = silent, TRUE = verbose. Defaults to TRUE.
#' @param y_axis_label Optional character string for y-axis label. If NULL,
#'   defaults to y_var name or "Variable" for variable scenario.
#' @return A ggplot2 object
#' @examples
#' # Sample data
#' hf_data <- data.frame(
#'   month = rep(c("Jan", "Feb", "Mar"), each = 10),
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
                                na_to_zero = TRUE,
                                use_reprate = TRUE,
                                full_range = TRUE,
                                target_language = "en",
                                source_language = "en",
                                lang_cache_path = tempdir(),
                                save_plot = FALSE,
                                plot_path = NULL,
                                compress_image = FALSE,
                                image_overwrite = TRUE,
                                compression_speed = 1,
                                compression_verbose = TRUE,
                                y_axis_label = NULL) {
  # Input validation
  if (is.null(x_var) || !x_var %in% names(data)) {
    cli::cli_abort(c(
      "!" = "A valid 'x_var' must be provided and must exist in the data.",
      "i" = "Run `rlang::last_trace()` to see where the error occurred."
    ))
  }

  if (save_plot && is.null(plot_path)) {
    cli::cli_abort(c(
      "!" = "When 'save_plot' is TRUE, 'plot_path' must be provided.",
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
    na_to_zero = na_to_zero
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
      y_axis_label = y_axis_label,
      common_elements = common_elements,
      target_language = target_language,
      source_language = source_language,
      lang_cache_path = lang_cache_path
    )
  )

  # Save plot if requested
  if (save_plot) {
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
      save_title_prefix = save_title_prefix
    )
  }

  plot
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
#' @param common_elements Common ggplot elements to apply to all plots
#' @param target_language Language code for labels (ISO 639-1), defaults to "en"
#' @param source_language Source language code, defaults to NULL
#' @param lang_cache_path Path for translation cache, defaults to tempdir()
#'
#' @return A ggplot2 object
variables_plot <- function(plot_data, x_var, vars_of_interest,
                           fill_var, fill_label, title_prefix,
                           common_elements, target_language = "en",
                           source_language = "en",
                           lang_cache_path = tempdir()) {
  # Create plot with variables on y-axis
  plot <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = as.factor(!!rlang::sym(x_var)),
      y = variable, # Use variable names on y-axis
      fill = !!rlang::sym(fill_var)
    )
  ) +
    common_elements +
    ggplot2::labs(
      title = paste0(
        title_prefix,
        " selected variables by ", tolower(x_var)
      ),
      x = "",
      y = "Variable",
      fill = fill_label
    )

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
#' @param y_axis_label Label for the y-axis
#' @param common_elements Common ggplot elements to apply to all plots
#' @param target_language Language code for labels (ISO 639-1)
#' @param source_language Source language code, defaults to NULL
#' @param lang_cache_path Path for translation cache, defaults to tempdir()
#'
#' @return A ggplot2 object
group_plot <- function(plot_data, x_var, y_var, vars_of_interest,
                       fill_var, fill_label, title_prefix,
                       y_axis_label, common_elements, target_language = "en",
                       source_language = "en",
                       lang_cache_path = tempdir()) {
  # Create plot with grouping variable on y-axis
  plot <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = as.factor(!!rlang::sym(x_var)),
      y = !!rlang::sym(y_var),
      fill = !!rlang::sym(fill_var)
    )
  ) +
    common_elements +
    ggplot2::labs(
      title = paste0(
        title_prefix,
        " ", tolower(vars_of_interest), " by ",
        tolower(x_var), " and ", tolower(y_var)
      ),
      x = "",
      y = y_axis_label,
      fill = fill_label
    )

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
#'
#' @return Invisible path to the saved file
save_single_plot <- function(plot, plot_data, plot_path,
                             x_var, y_var, y_axis_label,
                             vars_of_interest,
                             target_language = "en",
                             source_language = "en",
                             lang_cache_path = tempdir(),
                             data, compression_options,
                             use_reprate, save_title_prefix) {
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
    translate_text(
      "multiple variables",
      target_language = target_language,
      source_language = source_language,
      cache_path = lang_cache_path
    ) |>
      tolower() |>
      gsub(" ", "_", x = _)
  } else {
    translated_terms$vars_of_interest_str |> tolower()
  }

  # Construct filename
  save_path <- glue::glue(
    "{translated_terms$prefix}_{translated_terms$for_word}_",
    "{vars_of_interest_str}_{translated_terms$by_word}_",
    "{tolower(translated_terms$x_title)}{y_var_part}_",
    "{translated_terms$year_range}_{format(Sys.Date(), '%Y-%m-%d')}.png"
  )

  full_path <- file.path(plot_path, save_path)

  # Calculate dimensions
  dims <- calculate_plot_dimensions(plot_data, x_var, y_var)

  # Try to save the plot
  tryCatch(
    {
      ggplot2::ggsave(
        filename = full_path,
        plot = plot,
        width = dims$width,
        height = dims$height,
        dpi = 300
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
      cli::cli_alert_success(paste(success_msg, full_path))
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
  ) |>
    tolower() |>
    gsub(" ", "_", x = _)

  # Format x_var for title
  x_title <- if (x_var == "yearmon") "year and month" else x_var
  x_title <- translate_text(
    x_title,
    target_language = target_language,
    source_language = source_language,
    cache_path = lang_cache_path
  ) |>
    tolower() |>
    gsub(" ", "_", x = _)

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
        orig_title <- translate_text(
          plot_labs[[lab_name]],
          target_language = target_language,
          source_language = source_language,
          cache_path = lang_cache_path
        )

        # Convert title to sentence case
        orig_title <- tools::toTitleCase(tolower(orig_title))

        plot_labs[[lab_name]] <- gsub(
          "\\*\\*\\s+(.*?)\\s+\\*\\*", "**\\1**",
          orig_title
        )
      } else {
        # Standard translation for other labels
        plot_labs[[lab_name]] <- translate_text(
          plot_labs[[lab_name]],
          target_language = target_language,
          source_language = source_language,
          cache_path = lang_cache_path
        )
      }
    }
  }

  # Update the plot with translated labels
  plot + do.call(ggplot2::labs, plot_labs)
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
    rev(wesanderson::wes_palette("Zissou1", 100, type = "continuous"))
  } else {
    wesanderson::wes_palette("Zissou1", 100, type = "continuous")
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
    ggplot2::scale_x_discrete(expand = c(0, 0)),
    ggplot2::scale_y_discrete(expand = c(0, 0)),
    base_theme,
    ggplot2::guides(
      fill = ggplot2::guide_colorbar(
        title.position = "top",
        nrow = 1,
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

  common_elements
}
