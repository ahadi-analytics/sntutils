#' Correct outliers using temporal neighbors
#'
#' @description
#' Corrects outlier values by replacing them with the median of neighboring
#' months (month before and month after). Applies a consistency rule to ensure
#' corrected values do not exceed an upper bound variable.
#'
#' @details
#' For each target variable flagged as an outlier:
#' 1. Retrieve the value from the month before (lag) and month after (lead)
#' 2. If either neighbor is missing, no correction is applied
#' 3. Compute the median of the two neighbors
#' 4. Check if the median exceeds the consistency variable for that row
#' 5. If the consistency check fails, the corrected value is set to NA
#' 6. Otherwise, replace the outlier with the median
#'
#' @param data Data frame containing outlier flags and variables to correct.
#' @param target_vars Character vector. Columns to correct (e.g., c("conf",
#'   "maltreat")). Each target is paired with the corresponding element in
#'   `consistency_vars`.
#' @param consistency_vars Character vector. Upper bound columns (e.g.,
#'   c("test", "conf")). Must be the same length as `target_vars`. For each
#'   pair, the corrected value must be <= consistency variable.
#' @param outlier_flag_col Character. Column containing outlier flags. Values
#'   of "outlier" are corrected. Default: "outlier_flag_consensus".
#' @param group_cols Character vector. Columns defining the time series groups
#'   (e.g., c("hf_uid") for facility-level). Neighbors are found within groups.
#' @param date_col Character. Date column name. Default: "date".
#' @param verbose Logical. Print summary messages. Default: TRUE.
#'
#' @return The input data frame with two new columns per target variable:
#' \describe{
#'   \item{\code{{target}_corrected}}{The corrected value, or original if no
#'     correction was applied.
#'   }
#'   \item{\code{{target}_correction_flag}}{Reason for correction status:
#'     "corrected", "failed_consistency", "missing_neighbors", or "not_outlier".
#'   }
#' }
#'
#' @examples
#' \dontrun{
#' # After running detect_outliers()
#' result <- detect_outliers(data, column = "conf", ...)
#'
#' # Correct single variable: conf bounded by test
#' corrected <- correct_outliers(
#'   data = result,
#'   target_vars = "conf",
#'   consistency_vars = "test",
#'   group_cols = c("hf_uid"),
#'   date_col = "date"
#' )
#'
#' # Correct multiple variables at once
#' corrected <- correct_outliers(
#'   data = result,
#'   target_vars = c("conf", "maltreat", "maladm"),
#'   consistency_vars = c("test", "conf", "conf"),
#'   group_cols = c("hf_uid"),
#'   date_col = "date"
#' )
#' }
#'
#' @export
correct_outliers <- function(
    data,
    target_vars,
    consistency_vars,
    outlier_flag_col = "outlier_flag_consensus",
    group_cols,
    date_col = "date",
    verbose = TRUE
) {
  # ---- input validation ----
  if (!inherits(data, "data.frame")) {
    cli::cli_abort("'data' must be a data frame.")
  }

  if (length(target_vars) != length(consistency_vars)) {
    cli::cli_abort(
      "'target_vars' and 'consistency_vars' must have the same length."
    )
  }

  if (length(target_vars) == 0) {
    cli::cli_abort("'target_vars' must contain at least one variable.")
  }


  missing_targets <- target_vars[!target_vars %in% names(data)]
  if (length(missing_targets) > 0) {
    cli::cli_abort(
      "Target variable(s) not found in data: {.val {missing_targets}}"
    )
  }

  missing_consistency <- consistency_vars[!consistency_vars %in% names(data)]
  if (length(missing_consistency) > 0) {
    cli::cli_abort(
      "Consistency variable(s) not found in data: {.val {missing_consistency}}"
    )
  }

  if (!outlier_flag_col %in% names(data)) {
    cli::cli_abort(
      "Outlier flag column '{outlier_flag_col}' not found in data."
    )
  }

  missing_groups <- group_cols[!group_cols %in% names(data)]
  if (length(missing_groups) > 0) {
    cli::cli_abort(
      "Grouping column(s) not found in data: {.val {missing_groups}}"
    )
  }

  if (!date_col %in% names(data)) {
    cli::cli_abort("Date column '{date_col}' not found in data.")
  }

  # ---- process each target/consistency pair ----
  result <- data

  for (i in seq_along(target_vars)) {
    target_var <- target_vars[i]
    consistency_var <- consistency_vars[i]

    corrected_col <- paste0(target_var, "_corrected")
    flag_col <- paste0(target_var, "_correction_flag")

    result <- .correct_single_var(
      data = result,
      target_var = target_var,
      consistency_var = consistency_var,
      outlier_flag_col = outlier_flag_col,
      group_cols = group_cols,
      date_col = date_col,
      corrected_col = corrected_col,
      flag_col = flag_col
    )

    if (verbose) {
      .print_correction_summary(
        data = result,
        target_var = target_var,
        consistency_var = consistency_var,
        flag_col = flag_col
      )
    }
  }

  result
}


# ---- internal helper: correct a single variable ----
.correct_single_var <- function(
    data,
    target_var,
    consistency_var,
    outlier_flag_col,
    group_cols,
    date_col,
    corrected_col,
    flag_col
) {
  data |>
    dplyr::arrange(dplyr::across(dplyr::all_of(c(group_cols, date_col)))) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
    dplyr::mutate(
      .lag_val = dplyr::lag(.data[[target_var]], n = 1),
      .lead_val = dplyr::lead(.data[[target_var]], n = 1),
      .is_outlier = .data[[outlier_flag_col]] == "outlier",
      # median of 2 values = mean, rounded to integer for count data
      .neighbor_median = dplyr::if_else(
        is.na(.lag_val) | is.na(.lead_val),
        NA_real_,
        round((.lag_val + .lead_val) / 2)
      ),
      !!flag_col := dplyr::case_when(
        !.is_outlier ~ "not_outlier",
        is.na(.lag_val) | is.na(.lead_val) ~ "missing_neighbors",
        .neighbor_median > .data[[consistency_var]] ~ "failed_consistency",
        TRUE ~ "corrected"
      ),
      !!corrected_col := dplyr::case_when(
        !.is_outlier ~ .data[[target_var]],
        .data[[flag_col]] == "corrected" ~ .neighbor_median,
        .data[[flag_col]] == "failed_consistency" ~ NA_real_,
        TRUE ~ .data[[target_var]]
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-".lag_val", -".lead_val", -".is_outlier", -".neighbor_median")
}


# ---- internal helper: print summary ----
.print_correction_summary <- function(
    data,
    target_var,
    consistency_var,
    flag_col
) {
  counts <- table(data[[flag_col]], useNA = "ifany")

  cli::cli_h3("Outlier Correction: {target_var} (bounded by {consistency_var})")

  n_corrected <- if ("corrected" %in% names(counts)) counts["corrected"] else 0
  n_failed <- if ("failed_consistency" %in% names(counts)) {
    counts["failed_consistency"]
  } else {
    0
  }
  n_missing <- if ("missing_neighbors" %in% names(counts)) {
    counts["missing_neighbors"]
  } else {
    0
  }
  n_not_outlier <- if ("not_outlier" %in% names(counts)) {
    counts["not_outlier"]
  } else {
    0
  }

  cli::cli_bullets(c(
    "*" = "Corrected: {.val {n_corrected}}",
    "*" = "Failed consistency check: {.val {n_failed}}",
    "*" = "Missing neighbors: {.val {n_missing}}",
    "*" = "Not outliers: {.val {n_not_outlier}}"
  ))
}
