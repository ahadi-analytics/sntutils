#' Detect outliers with guardrails and consensus
#'
#' @description
#' `detect_outliers()` evaluates a numeric indicator by administrative unit and
#' time using three methods (mean + SD, median + MAD, Tukey upper fence) to
#' identify unusually HIGH values only (potential outbreaks or data anomalies).
#' Low values are NOT flagged as outliers. It enforces reporting and sample-size
#' guardrails, supports strictness presets. Optional outbreak classification
#' distinguishes between isolated outliers and sustained outbreak patterns using
#' gap-aware clustering that can bridge short interruptions in the outbreak
#' signal. It returns per-method flags (optional), a consensus classification,
#' scale statistics, and metadata.
#'
#' @param data Data frame containing the indicator to analyse.
#' @param column Name of the numeric column to evaluate.
#' @param record_id Unique record identifier column.
#' @param admin_level Character vector of administrative level columns for
#'   parallel grouping, ordered from higher to lower resolution.
#'   Defaults to `c("adm1", "adm2")`.
#' @param date Date column (Date, POSIXt, or parseable character string).
#'   Year, month, and yearmon are automatically derived from this column.
#' @param time_mode Pooling strategy: `"across_time"` or `"within_year"`.
#' @param value_type Indicator type: `"count"` or `"rate"`. Counts floor lower
#'   bounds at 0.
#'
#' @param strictness Strictness preset: `"lenient"`, `"balanced"`, `"strict"`,
#'   or `"advanced"`. Presets map to method multipliers. If not `"advanced"`,
#'   any manual multipliers are **ignored**.
#' @param sd_multiplier Width (in SD units) for the mean method
#'   (used only when `strictness = "advanced"`).
#' @param mad_constant Constant passed to `stats::mad()` in advanced mode
#'   (default 1.4826).
#' @param mad_multiplier Width multiplier for the MAD method (advanced mode).
#' @param iqr_multiplier Tukey fence multiplier for the IQR method
#'   (advanced mode).
#'
#' @param min_n Minimum observations required in the active comparison bucket
#'   before flagging is attempted (applies to any seasonal bucket or fallback).
#' @param reporting_rate_col Optional column with reporting completeness in
#'   `[0, 1]`.
#' @param reporting_rate_min Minimum acceptable reporting rate. Rows below the
#'   threshold receive `reason = "low_reporting"` and are not flagged.
#'
#' @param key_indicators_hf Optional character vector of indicator names used to
#'   determine facility activeness. If supplied, the function uses a fast path
#'   to filter out inactive facility-months. A facility-month is considered
#'   active if ANY of the specified key indicators have non-NA values.
#'   Inactive facility-months are excluded from outlier detection. If `NULL`
#'   (default), activeness filtering is skipped. Typical indicators include
#'   `"allout"`, `"test"`, or `"conf"`. This adjustment prevents false positives
#'   caused by facilities that start or stop reporting mid-period.
#'
#' @param methods Character vector specifying which outlier detection
#'   methods to use: "iqr" (Interquartile Range), "median" (Median Absolute
#'   Deviation), "mean" (Mean +/- SD), and/or "consensus".
#'   Default is `c("iqr", "median", "mean", "consensus")`.
#'   For consensus, at least two other methods must be selected.
#' @param consensus_rule Number of methods that must agree (`1`, `2`, or `3`)
#'   for the consensus flag to call an outlier. Default `2`.
#' @param output_profile Controls the amount of detail returned:
#'   `"lean"` (minimal columns: id, admin, date, value, consensus flag, reason),
#'   `"standard"` (lean + per-method flags + bounds + seasonality mode),
#'   `"audit"` (all columns for full reproducibility). Default `"standard"`.
#'
#' @param verbose Logical. When `TRUE`, prints an informative summary showing
#'   which methods are being applied, the pooling strategy, strictness settings,
#'   guardrails, and consensus rule. Default is `FALSE`.
#' @param spatial_level Character string specifying the finest spatial unit
#'   for analysis (e.g., "hf_uid" for facility-level). When specified,
#'   `admin_level` defines grouping boundaries while `spatial_level` defines
#'   the unit of analysis. This prevents excessive grouping while maintaining
#'   spatial granularity. Default is `hf_uid`.
#' @param classify_outbreaks Logical. When `TRUE` (default), applies outbreak
#'   classification to distinguish between isolated outliers and sustained
#'   outbreak patterns. Consecutive outliers meeting the outbreak criteria are
#'   reclassified from "outlier" to "outbreak". This is particularly useful
#'   for epidemiological surveillance to identify disease outbreak patterns.
#'   Set to `FALSE` to disable outbreak classification.
#' @param outbreak_min_run Integer. Minimum number of consecutive outliers
#'   required to classify as an outbreak (default `2`). Must be >= 2.
#' @param outbreak_prop_tolerance Numeric. Proportional tolerance for outbreak
#'   consistency (default `0.9`). Values within this tolerance of the run
#'   median are considered consistent. Range: (0, 1).
#' @param outbreak_max_gap Integer. Maximum allowed gap (non-outlier months)
#'   between outliers that can still be considered part of the same outbreak
#'   (default `1`). For example, with `outbreak_max_gap = 12`, the pattern
#'   "outlier-normal-outlier-outlier" would be classified as one outbreak
#'   of length 3, rather than separate incidents. Set to `0` for strict
#'   consecutive-only outbreaks. Useful for real-world data with reporting gaps.
#'
#' @details
#' **Workflow**
#' 1) Validation & prep: confirm required columns, coerce target to numeric
#'    safely, derive month from `yearmon`.
#' 2) Activeness filtering (if `key_indicators_hf` is supplied): call
#'    `classify_facility_activity()` to tag inactive facilities. Inactive
#'    facility-months are excluded from detection and assigned
#'    `reason = "inactive_facility"`.
#' 3) Strictness: presets map to (SD, MAD, IQR) multipliers; advanced mode
#'    honours manual multipliers. On across-time fallback the strictness shifts
#'    one step toward lenient.
#' 4) Guardrails: rows failing `reporting_rate_min` or `min_n` bypass flagging
#'    and record `reason` (`low_reporting`, `insufficient_n`,
#'    `insufficient_data`).
#' 6) Flagging: each method checks if values exceed the UPPER threshold only
#'    (e.g., mean + multiplier x SD). Low values are never flagged. Methods
#'    are suppressed when scales are unstable (`sd`, `mad`, or `iqr`
#'    equals zero).
#' 7) Consensus: final `outlier_flag_consensus` requires at least
#'    `consensus_rule` methods to agree over available (non-suppressed) methods.
#'
#' **Facility activeness adjustment**
#'
#' When inactive or newly activated health facilities are included in aggregated
#' totals, apparent spikes or dips can occur that do not represent real
#' epidemiological changes. For example, if ten new facilities start reporting
#' in Matoto in mid-2022, the total number of confirmed cases rises even if
#' incidence per facility remains constant. To prevent such artefacts,
#' `detect_outliers()` uses a fast, optimized approach for activeness filtering.
#' When `key_indicators_hf` is specified, the function checks if each
#' facility-month has ANY non-NA values in the specified key indicators.
#' Only facility-months with at least one reported key indicator contribute to
#' the comparison pool for outlier detection. This lightweight approach avoids
#' the computational overhead of full activity classification while still
#' preventing false positives from inactive facilities.
#'
#' **Presets** (for high outliers only)
#' - lenient: values > mean + 4.0 x SD, median + 12 x MAD,
#'   or Q3 + 3.0 x IQR
#' - balanced: values > mean + 3.0 x SD, median + 9 x MAD,
#'   or Q3 + 2.0 x IQR
#' - strict: values > mean + 2.5 x SD, median + 6 x MAD,
#'   or Q3 + 1.5 x IQR
#' - advanced: use user-supplied multipliers
#'
#' Note: Only values ABOVE the upper thresholds are flagged. Low values are
#' always classified as "normal" regardless of how far below the mean/median.
#'
#' The returned tibble always contains identifiers, scale statistics, bounds,
#' strictness metadata, and the guardrail reason. When `output_profile =
#' "standard"` or `"audit"`, method-specific flags are included alongside
#' the consensus.
#'
#' @return Tibble with outlier classifications and metadata. Columns include:
#'   identifiers (`record_id`, admin levels, `yearmon`, `year`, derived
#'   `month`), `column_name`, `value`, `value_type`, scale stats (`mean`,
#'   `sd`, `median`, `mad`, `q1`, `q3`, `iqr`), method bounds, `n_in_group`,
#'   guardrail `reason`, method flags (optional), `outlier_flag_consensus`,
#'   strictness multipliers, and (if activeness filtering was applied)
#'   `activeness_applied` and `key_indicators_used`.
#'
#'   **Outlier flag categories are simplified to three intuitive groups:**
#'   - `"normal"`: Values within expected statistical bounds
#'   - `"outlier"`: Values exceeding thresholds (potential anomalies/outbreaks)
#'   - `"insufficient_data"`: Various data quality issues preventing
#'    determination (consolidates insufficient_n, insufficient_evidence,
#'    unstable_scale, etc.)
#'
#' @examples
#' \dontrun{
#' # NOTE: Only HIGH values are flagged as outliers (e.g., disease outbreaks).
#' # Low values are always considered "normal".
#'
#' # 1) Minimal consensus output at adm1-only level
#' detect_outliers(
#'   data = malaria_data,
#'   column = "confirmed_cases",
#'   date = "date",
#'   record_id = "facility_id",
#'   admin_level = c("adm1"),        # ignore adm2 if not present
#'   time_mode = "across_time"
#' )
#'
#' # 2) Within-year comparison
#' detect_outliers(
#'   data = malaria_data,
#'   column = "confirmed_cases",
#'   date = "date",
#'   admin_level = c("adm1"),
#'   time_mode = "within_year",
#'   consensus_rule = 2,
#'   output_profile = "audit"
#' )
#'
#' # 3) Advanced overrides for rates
#' detect_outliers(
#'   data = malaria_data,
#'   column = "positivity_rate",
#'   date = "date",
#'   value_type = "rate",
#'   strictness = "advanced",
#'   sd_multiplier = 2.5,
#'   mad_multiplier = 7,
#'   iqr_multiplier = 1.8,
#'   min_n = 10,
#'   output_profile = "audit"
#' )
#'
#' # 4) With facility activeness filtering
#' detect_outliers(
#'   data = malaria_data,
#'   column = "conf",
#'   date = "date",
#'   admin_level = c("adm1", "adm2"),
#'   time_mode = "across_time",
#'   key_indicators_hf = c("allout", "test", "conf")
#' )
#'
#' # 5) With binary activeness classification
#' detect_outliers(
#'   data = malaria_data,
#'   column = "conf",
#'   date = "date",
#'   admin_level = c("adm1", "adm2"),
#'   time_mode = "across_time",
#'   key_indicators_hf = c("allout", "test", "conf")
#' )
#' }
#' @export
detect_outliers <- function(
  data,
  column,
  record_id = "record_id",
  admin_level = c("adm1", "adm2"),
  spatial_level = "hf_uid",
  date = "date",
  time_mode = c("across_time", "within_year"),
  value_type = c("count", "rate"),
  strictness = c("balanced", "lenient", "strict", "advanced"),
  methods = c("iqr", "median", "mean", "consensus"),
  sd_multiplier = 3,
  mad_constant = 1.4826,
  mad_multiplier = 9,
  iqr_multiplier = 2,
  consensus_rule = 3,
  output_profile = c("standard", "lean", "audit"),
  min_n = 8,
  reporting_rate_col = NULL,
  reporting_rate_min = 0.5,
  key_indicators_hf = NULL,
  classify_outbreaks = FALSE,
  outbreak_min_run = 2,
  outbreak_prop_tolerance = 0.9,
  outbreak_max_gap = 12,
  verbose = TRUE
) {
  if (column %in% names(data) && !is.numeric(data[[column]])) {
    cli::cli_abort("Column {.val {column}} must be numeric.")
  }

  activeness_applied <- FALSE
  n_inactive_skipped <- 0L
  key_indicators_used <- NA_character_

  if (!is.null(key_indicators_hf)) {
    missing_indicators <- setdiff(key_indicators_hf, names(data))
    if (length(missing_indicators) > 0) {
      cli::cli_abort(
        "key_indicators_hf columns missing: {.val {missing_indicators}}"
      )
    }

    # Fast path for outlier detection: simply check if ANY key indicator is non-NA
    # This avoids the expensive classify_facility_activity() function which creates
    # cartesian products and computes 3 different classification methods
    data <- data |>
      dplyr::mutate(
        # Check if any of the key indicators have non-NA values
        .has_any_indicator = dplyr::if_any(
          dplyr::all_of(key_indicators_hf),
          ~ !is.na(.)
        ),
        # Facility is active if it reported any key indicator
        .is_active = .has_any_indicator
      )

    # Defensive check: ensure we're not about to filter out all data
    n_active_with_values <- sum(data$.is_active & !is.na(data[[column]]), na.rm = TRUE)
    if (n_active_with_values == 0) {
      cli::cli_abort(c(
        "No non-missing {.field {column}} values found in active facilities.",
        "i" = "Check if {.field {column}} is one of your key_indicators_hf.",
        "i" = "Consider including {.field {column}} in key_indicators_hf or removing activity filtering."
      ))
    }

    n_inactive_skipped <- sum(!data$.is_active, na.rm = TRUE)
    data <- data |>
      dplyr::filter(.is_active) |>
      dplyr::select(-.has_any_indicator, -.is_active)
    activeness_applied <- TRUE
    key_indicators_used <- paste(key_indicators_hf, collapse = ", ")
  }

  # execute detection
  .core_outlier_detection(
    data = data,
    column = column,
    record_id = record_id,
    admin_level = admin_level,
    date = date,
    time_mode = time_mode,
    value_type = value_type,
    strictness = strictness,
    sd_multiplier = sd_multiplier,
    mad_constant = mad_constant,
    mad_multiplier = mad_multiplier,
    iqr_multiplier = iqr_multiplier,
    min_n = min_n,
    reporting_rate_col = reporting_rate_col,
    reporting_rate_min = reporting_rate_min,
    key_indicators_hf = key_indicators_hf,
    methods = methods,
    consensus_rule = consensus_rule,
    output_profile = output_profile,
    verbose = verbose,
    spatial_level = spatial_level,
    activeness_applied = activeness_applied,
    n_inactive_skipped = n_inactive_skipped,
    key_indicators_used = key_indicators_used,
    classify_outbreaks = classify_outbreaks,
    outbreak_min_run = outbreak_min_run,
    outbreak_prop_tolerance = outbreak_prop_tolerance,
    outbreak_max_gap = outbreak_max_gap
  )
}


# option normalization --------------------------------------------------------

#' harmonize outlier detection options
#'
#' @param time_mode time pooling mode
#' @param value_type indicator type ("count" or "rate")
#' @param strictness strictness preset
#' @param methods methods requested for flagging
#' @param output_profile output profile string
#' @return list with normalized options for the detection pipeline
#' @keywords internal
#' @noRd
.normalize_outlier_options <- function(
    time_mode,
    value_type,
    strictness,
    methods,
    output_profile) {
  resolved_time_mode <- base::match.arg(
    time_mode,
    c("across_time", "within_year")
  )
  resolved_value_type <- base::match.arg(
    value_type,
    c("count", "rate")
  )
  resolved_strictness <- base::match.arg(
    strictness,
    c("balanced", "lenient", "strict", "advanced")
  )
  resolved_profile <- base::match.arg(
    output_profile,
    c("standard", "lean", "audit")
  )

  valid_methods <- c("iqr", "median", "mean", "consensus")
  resolved_methods <- base::match.arg(
    methods,
    valid_methods,
    several.ok = TRUE
  )

  if ("consensus" %in% resolved_methods) {
    other_methods <- setdiff(resolved_methods, "consensus")
    if (base::length(other_methods) < 2) {
      cli::cli_abort(c(
        "Consensus requires at least 2 other methods.",
        "i" = "Include two of: 'iqr', 'median', 'mean'."
      ))
    }
  }

  list(
    time_mode = resolved_time_mode,
    value_type = resolved_value_type,
    strictness = resolved_strictness,
    methods = resolved_methods,
    output_profile = resolved_profile
  )
}

# context preparation ---------------------------------------------------------

#' prepare data, admin context, and strictness settings
#'
#' @param data raw input data
#' @param column numeric column to evaluate
#' @param record_id unique record identifier
#' @param admin_level admin hierarchy columns
#' @param date_column date column name
#' @param options normalized option list
#' @param consensus_rule integer consensus rule
#' @param sd_multiplier sd multiplier
#' @param mad_constant mad constant
#' @param mad_multiplier mad multiplier
#' @param iqr_multiplier iqr multiplier
#' @param min_n minimum observations per bucket
#' @param reporting_rate_col optional reporting rate column
#' @param reporting_rate_min minimum reporting rate threshold
#' @return list containing prepared data and pipeline context
#' @keywords internal
#' @noRd
.prepare_outlier_context <- function(
    data,
    column,
    record_id,
    admin_level,
    date_column,
    options,
    consensus_rule,
    sd_multiplier,
    mad_constant,
    mad_multiplier,
    iqr_multiplier,
    min_n,
    reporting_rate_col,
    reporting_rate_min,
    spatial_level = NULL) {
  if (!date_column %in% names(data)) {
    cli::cli_abort("Date column {.val {date_column}} not found in data.")
  }

  if (!is.null(reporting_rate_col)) {
    if (!reporting_rate_col %in% names(data)) {
      cli::cli_abort(
        "Reporting rate column {.val {reporting_rate_col}} not found in data."
      )
    }
    if (!is.numeric(data[[reporting_rate_col]])) {
      cli::cli_abort(
        "Reporting rate column {.val {reporting_rate_col}} must be numeric."
      )
    }
    rates <- data[[reporting_rate_col]]
    rates_valid <- rates[!is.na(rates)]
    if (any(rates_valid < 0 | rates_valid > 1)) {
      n_invalid <- base::sum(rates_valid < 0 | rates_valid > 1)
      cli::cli_abort(c(
        paste0(
          "Reporting rate column {.val {reporting_rate_col}} contains ",
          "{n_invalid} invalid value{?s}."
        ),
        "i" = "Values must fall within [0, 1]."
      ))
    }
  }

  parsed_dates <- .parse_date_column(data[[date_column]], date_column)
  year_col <- ".detect_year"
  month_col <- ".detect_month"
  yearmon_col <- ".detect_yearmon"

prepared <- data |>
  dplyr::mutate(
    value = .data[[column]],
    .detect_year = parsed_dates$year,
    .detect_month = parsed_dates$month,
    .detect_yearmon = parsed_dates$yearmon
  )

  validated <- .validate_outlier_inputs(
    data = prepared,
    column = column,
    record_id = record_id,
    admin_level = admin_level,
    year = year_col,
    yearmon = yearmon_col,
    month = month_col,
    value_type = options$value_type,
    time_mode = options$time_mode,
    output_profile = options$output_profile,
    strictness = options$strictness,
    consensus_rule = consensus_rule,
    min_n = min_n,
    reporting_rate_col = reporting_rate_col,
    reporting_rate_min = reporting_rate_min,
    spatial_level = spatial_level
  )
  prepared_data <- validated$data

  # Handle spatial_level parameter for proper detection vs grouping
  if (!is.null(spatial_level) && spatial_level %in% names(prepared_data)) {
    # Detection happens at spatial_level, grouping for efficiency at admin_level
    detection_admin_level <- c(admin_level, spatial_level)
    parallel_grouping_levels <- admin_level
  } else {
    # Original behavior - use all admin_level for both detection and grouping
    # (either spatial_level is NULL or the column doesn't exist)
    detection_admin_level <- admin_level
    parallel_grouping_levels <- admin_level
    if (!is.null(spatial_level) && !spatial_level %in% names(prepared_data)) {
      cli::cli_warn(
        "spatial_level column {.field {spatial_level}} not found in data, using admin_level only"
      )
    }
  }

  admin_info <- .resolve_admin_level(
    prepared_data,
    detection_admin_level,
    record_id
  )

  # Store both levels for different purposes - preserve our carefully
  # crafted levels
  admin_info$detection_levels <- detection_admin_level   # What we detect outliers on
  admin_info$parallel_grouping_levels <- parallel_grouping_levels
  # How we group for efficiency

  # Override admin_level with detection_levels to ensure consistency
  # (.resolve_admin_level adds record_id which we don't want in our levels)
  admin_info$admin_level <- detection_admin_level

  strictness_info <- .resolve_strictness(
    options$strictness,
    sd_multiplier,
    mad_constant,
    mad_multiplier,
    iqr_multiplier
  )

  grouping_cols <- .build_group_keys(
    prepared_data,
    admin_info$detection_levels,  # Use detection_levels for actual outlier detection
    options$time_mode,
    ".outlier_year",
    validated$month
  )

  list(
    prepared_data = prepared_data,
    admin_info = admin_info,
    strictness = strictness_info,
    grouping_cols = grouping_cols,
    options = options,
    constraints = list(
      min_n = min_n,
      reporting_rate_min = reporting_rate_min
    ),
    spatial_level = spatial_level
  )
}

# detection execution ---------------------------------------------------------

#' execute appropriate detection workflow
#'
#' @param context context list returned by `.prepare_outlier_context()`
#' @param methods methods requested
#' @param consensus_rule integer consensus rule
#' @return tibble with detection results
#' @keywords internal
#' @noRd
.execute_outlier_detection <- function(
    context,
    methods,
    consensus_rule) {
  time_mode <- context$options$time_mode


  desc <- switch(
    time_mode,
    within_year = "Within-year pooling",
    across_time = "Across-time pooling",
    ""
  )
  metadata <- list(
    seasonality_mode_requested = time_mode,
    seasonality_mode_used = time_mode,
    seasonal_window_desc = desc
  )

  .run_non_seasonal_detection(
    data = context$prepared_data,
    grouping_cols = context$grouping_cols,
    strictness = context$strictness,
    min_n = context$constraints$min_n,
    reporting_rate_min = context$constraints$reporting_rate_min,
    methods = methods,
    consensus_rule = consensus_rule,
    value_type = context$options$value_type,
    metadata = metadata
  )
}

# result finalization ---------------------------------------------------------

#' finalise detection output with metadata and column selection
#'
#' @param detection detection tibble
#' @param context context list from `.prepare_outlier_context()`
#' @param methods methods vector
#' @param record_id record id column
#' @param output_profile output profile string
#' @param column column under evaluation
#' @param verbose logical flag
#' @param consensus_rule consensus threshold
#' @return tibble ready for export
#' @keywords internal
#' @noRd
.finalize_outlier_detection <- function(
    detection,
    context,
    methods,
    record_id,
    output_profile,
    column,
    verbose,
    consensus_rule,
    activeness_applied = FALSE,
    n_inactive_skipped = 0L,
    key_indicators_used = NA_character_,
    classify_outbreaks = TRUE,
    outbreak_min_run = 2,
    outbreak_prop_tolerance = 0.9,
    outbreak_max_gap = 1) {
  if (!"residual_sd" %in% names(detection)) {
    detection <- detection |>
      dplyr::mutate(
        residual_sd = NA_real_,
        residual_mad = NA_real_,
        residual_iqr = NA_real_
      )
  }

  strictness_info <- context$strictness
  admin_info <- context$admin_info
  time_mode <- context$options$time_mode
  value_type <- context$options$value_type
  min_n <- context$constraints$min_n
  reporting_rate_min <- context$constraints$reporting_rate_min
  strictness_label <- strictness_info$strictness

  if (!"strictness_label" %in% names(detection)) {
    detection$strictness_label <- strictness_label
  } else {
    detection$strictness_label <- dplyr::coalesce(
      detection$strictness_label,
      strictness_label
    )
  }
  if (!"strictness_shifted" %in% names(detection)) {
    detection$strictness_shifted <- strictness_info$strictness_shifted
  } else {
    detection$strictness_shifted <- dplyr::coalesce(
      detection$strictness_shifted,
      strictness_info$strictness_shifted
    )
  }

  detection <- detection |>
    dplyr::mutate(
      column_name = column,
      value = .outlier_value,
      value_type = value_type,
      month = .outlier_month,
      sd_multiplier = strictness_info$sd_multiplier,
      mad_constant = strictness_info$mad_constant,
      mad_multiplier = strictness_info$mad_multiplier,
      iqr_multiplier = strictness_info$iqr_multiplier,
      time_mode = time_mode,
      reporting_rate = .outlier_reporting,
      admin_level_used = paste(admin_info$admin_level, collapse = ", "),
      activeness_applied = activeness_applied,
      key_indicators_used = key_indicators_used
    )

  if ("mean" %in% methods && "mean_flag" %in% names(detection)) {
    detection <- detection |> dplyr::rename(outlier_flag_mean = mean_flag)
  }
  if ("median" %in% methods && "median_flag" %in% names(detection)) {
    detection <- detection |> dplyr::rename(outlier_flag_median = median_flag)
  }
  if ("iqr" %in% methods && "iqr_flag" %in% names(detection)) {
    detection <- detection |> dplyr::rename(outlier_flag_iqr = iqr_flag)
  }

  # verbose summary will be displayed after outbreak classification

  select_cols <- c(
    admin_info$admin_level,
    ".outlier_year",
    ".outlier_month",
    ".outlier_yearmon",
    record_id,
    "column_name",
    "value",
    "value_type",
    "reporting_rate",
    "n_in_group",
    "mean",
    "sd",
    "mean_lower",
    "mean_upper",
    "median",
    "mad",
    "median_lower",
    "median_upper",
    "q1",
    "q3",
    "iqr",
    "iqr_lower",
    "iqr_upper",
    "residual_sd",
    "residual_mad",
    "residual_iqr"
  )

  if ("mean" %in% methods) {
    select_cols <- c(select_cols, "outlier_flag_mean")
  }
  if ("median" %in% methods) {
    select_cols <- c(select_cols, "outlier_flag_median")
  }
  if ("iqr" %in% methods) {
    select_cols <- c(select_cols, "outlier_flag_iqr")
  }
  if ("consensus" %in% methods) {
    select_cols <- c(select_cols, "outlier_flag_consensus")
  }

  select_cols <- c(
    select_cols,
    "reason",
    "seasonality_mode_requested",
    "seasonality_mode_used",
    "seasonal_window_desc",
    "fallback_applied",
    "fallback_reason",
    "strictness_label",
    "strictness_shifted",
    "sd_multiplier",
    "mad_constant",
    "mad_multiplier",
    "iqr_multiplier",
    "time_mode",
    "admin_level_used",
    "activeness_applied",
    "key_indicators_used"
  )

  final <- detection |>
    dplyr::select(
      dplyr::all_of(select_cols[select_cols %in% names(detection)])
    ) |>
    dplyr::mutate(
      year = .outlier_year,
      month = .outlier_month,
      yearmon = .outlier_yearmon
    ) |>
    .filter_by_output_profile(
      output_profile,
      record_id,
      admin_info$admin_level,
      "yearmon",
      "year"
    )

  # Apply outbreak classification if requested
  flag_columns <- grep("^outlier_flag_", names(final), value = TRUE)
  if (classify_outbreaks && length(flag_columns) > 0) {
    # identify grouping columns for outbreak classification
    # Note: record_id is unique per row, so we use facility-level grouping instead
    group_columns <- context$admin_info$admin_level
    if ("hf_uid" %in% names(final)) {
      group_columns <- c(group_columns, "hf_uid")
    } else if (record_id %in% names(final) && record_id != "record_id") {
      # only include record_id if it represents facility/location, not unique row id
      group_columns <- c(group_columns, record_id)
    }
    group_columns <- group_columns[group_columns %in% names(final)]

    if (length(group_columns) > 0) {
      final <- .classify_outbreaks(
        data = final,
        group_by_columns = group_columns,
        outlier_columns = flag_columns,
        min_run = outbreak_min_run,
        prop_tolerance = outbreak_prop_tolerance,
        outbreak_max_gap = outbreak_max_gap,
        verbose = verbose
      )

      # replace original flag column values with outbreak-classified values
      outbreak_flag_columns <- grep("^outlier_flag_.*_outbreak_classified$", names(final), value = TRUE)

      if (length(outbreak_flag_columns) > 0) {
        # replace original column values with outbreak-classified values
        for (outbreak_col in outbreak_flag_columns) {
          original_col <- stringr::str_replace(outbreak_col, "_outbreak_classified$", "")
          if (original_col %in% names(final)) {
            # replace original column values with outbreak-classified values
            final[[original_col]] <- final[[outbreak_col]]
            # remove the temporary outbreak-classified column
            final <- final |> dplyr::select(-dplyr::all_of(outbreak_col))
          }
        }
        # flag_columns remain the same (original column names)
        flag_columns <- grep("^outlier_flag_", names(final), value = TRUE)
      }
    }
  }

  # Simplify outlier categories for better interpretability
  if (length(flag_columns) > 0) {
    final <- .simplify_outlier_categories(final, flag_columns)
  }

  # Display summary after outbreak classification if verbose
  if (isTRUE(verbose)) {
    .display_outlier_summary_box(
      detection = final,
      methods = methods,
      column = column,
      time_mode = time_mode,
      admin_level = admin_info$admin_level,
      strictness = strictness_label,
      strictness_info = strictness_info,
      min_n = min_n,
      reporting_rate_min = reporting_rate_min,
      consensus_rule = consensus_rule,
      activeness_applied = activeness_applied,
      n_inactive_skipped = n_inactive_skipped,
      key_indicators_used = key_indicators_used,
      classify_outbreaks = classify_outbreaks
    )
  }

  final
}

#' detect if console/IDE is using dark theme
#'
#' @description
#' attempts to detect if the console or IDE is using a dark theme.
#' checks cli options, RStudio theme, and environment variables.
#'
#' @return logical. TRUE if dark theme detected, FALSE otherwise
#' @keywords internal
#' @noRd
.detect_dark_theme <- function() {
  # 1. explicit user override ---------------------------------------------
  cli_option <- getOption("cli.theme_dark", default = "auto")
  if (is.logical(cli_option)) return(cli_option)

  # 2. rstudio detection ---------------------------------------------------
  if (requireNamespace("rstudioapi", quietly = TRUE)) {
    if (rstudioapi::isAvailable() && rstudioapi::hasFun("getThemeInfo")) {
      dark_rstudio <- tryCatch({
        info <- rstudioapi::getThemeInfo()
        if (!is.null(info$dark)) info$dark else NULL
      }, error = function(e) NULL)
      if (!is.null(dark_rstudio)) return(dark_rstudio)
    }
  }

  # 3. vscode detection ----------------------------------------------------
  term_program <- Sys.getenv("TERM_PROGRAM", unset = "")
  vscode_theme <- Sys.getenv("VSCODE_THEME", unset = "")
  vscode_color <- Sys.getenv("VSCODE_COLOR_THEME", unset = "")

  if (identical(term_program, "vscode")) {
    # infer from theme name if possible
    theme_name <- tolower(paste(vscode_theme, vscode_color))
    if (grepl("dark|night|midnight|black", theme_name)) return(TRUE)
    if (grepl("light|white|day", theme_name)) return(FALSE)
    # fallback: assume dark since most VS Code terminals are dark
    return(TRUE)
  }

  # 4. emacs detection -----------------------------------------------------
  emacs_mode <- Sys.getenv("EMACS_DARK_MODE", unset = "")
  if (emacs_mode == "dark") return(TRUE)
  if (emacs_mode == "light") return(FALSE)

  # 5. terminal color fallback ----------------------------------------------
  color_fg_bg <- Sys.getenv("COLORFGBG", unset = "")
  if (!identical(color_fg_bg, "")) {
    parts <- base::strsplit(color_fg_bg, ";", fixed = TRUE)[[1]]
    bg_code <- utils::tail(parts, 1)
    if (bg_code %in% c("0", "8")) return(TRUE)
    if (bg_code %in% c("7", "15")) return(FALSE)
  }

  # 6. default fallback ----------------------------------------------------
  FALSE
}

#' display outlier detection summary in CLI box
#'
#' @description
#' render a formatted CLI box summarizing the outlier detection setup
#' and results. includes seasonality, strictness, guardrails, and per-method
#' detection statistics for a quick QA snapshot.
#'
#' @param detection data frame with outlier results
#' @param column character. name of column checked
#' @param time_mode pooling mode used
#' @param admin_level character vector of admin level columns
#' @param strictness_info list of sd, mad, and iqr multipliers
#' @param strictness strictness label used
#' @param min_n minimum observations for flagging
#' @param reporting_rate_min minimum reporting rate threshold
#' @param methods methods applied ("iqr", "median", "mean", "consensus")
#' @param consensus_rule number of agreeing methods for consensus
#'
#' @keywords internal
#' @noRd
.display_outlier_summary_box <- function(
  detection,
  column,
  time_mode,
  admin_level,
  strictness_info,
  strictness,
  min_n,
  reporting_rate_min,
  methods,
  consensus_rule,
  activeness_applied = FALSE,
  n_inactive_skipped = 0L,
  key_indicators_used = NA_character_,
  classify_outbreaks = TRUE
) {
  time_desc <- switch(
    time_mode,
    "by_month" = "months",
    "within_year" = "within each year",
    "across_time" = "across all time",
    "unknown"
  )
  level_used <- tail(admin_level, 1)

  fmt_num <- function(x) {
    cli::col_blue(cli::style_bold(formatC(x, big.mark = ",", digits = 0)))
  }
  fmt_pct <- function(x) {
    cli::col_blue(cli::style_bold(formatC(x * 100, format = "f", digits = 0)))
  }

  # guardrail diagnostics
  yearmon_col <- if (".outlier_yearmon" %in% names(detection)) ".outlier_yearmon" else "yearmon"

  n_skipped_n <- detection |>
    dplyr::filter(reason == "insufficient_n") |>
    dplyr::distinct(
      dplyr::across(all_of(admin_level)),
      .data[[yearmon_col]]
    ) |>
    nrow()
  n_skipped_rep <- detection |>
    dplyr::filter(reason == "low_reporting") |>
    dplyr::distinct(
      dplyr::across(all_of(admin_level)),
      .data[[yearmon_col]]
    ) |>
    nrow()

  # header lines
  lines <- c(
    glue::glue(
      "You are checking outliers for ",
      "{cli::col_red(cli::style_bold(column))} by ",
      "{cli::style_underline(cli::style_bold(time_desc))} ",
      "at {cli::style_underline(cli::style_bold(level_used))} level"
    ),
    ""
  )


  lines <- c(
    lines,
    glue::glue(
      "{cli::col_magenta(cli::style_bold('Strictness preset'))}: ",
      "{cli::col_green(cli::style_bold(strictness))} ",
      "(SD = {fmt_num(strictness_info$sd_multiplier)}, ",
      "MAD mult = {fmt_num(strictness_info$mad_multiplier)}, ",
      "IQR = {fmt_num(strictness_info$iqr_multiplier)})"
    ),
    glue::glue(
      "{cli::col_magenta(cli::style_bold('Guardrails'))}: ",
      "{fmt_num(n_skipped_n)} groups skipped (n < {fmt_num(min_n)}), ",
      "{fmt_num(n_skipped_rep)} groups skipped ",
      "(reporting < {fmt_pct(reporting_rate_min)}%)"
    )
  )

  if (activeness_applied) {
    lines <- c(
      lines,
      glue::glue(
        "{cli::col_magenta(cli::style_bold('Activeness filtering'))}: ",
        "{fmt_num(n_inactive_skipped)} inactive facility-months excluded ",
        "(indicators: {key_indicators_used})"
      )
    )
  }

  lines <- c(
    lines,
    "",
    cli::col_yellow(cli::style_bold('Method results:'))
  )

  .append_method_line <- function(method) {
    flag_candidates <- switch(
      method,
      "iqr" = c("outlier_flag_iqr", "iqr_flag"),
      "median" = c("outlier_flag_median", "median_flag"),
      "mean" = c("outlier_flag_mean", "mean_flag"),
      "consensus" = c("outlier_flag_consensus")
    )
    flag_col <- flag_candidates[flag_candidates %in% names(detection)][1]
    if (is.na(flag_col)) {
      return(NULL)
    }

    flags <- detection[[flag_col]]
    guard_reasons <- c(
      "low_reporting",
      "insufficient_n",
      "insufficient_seasonal_history"
    )
    out_n <- sum(flags == "outlier", na.rm = TRUE)
    eligible_n <- sum(
      !flags %in% c(guard_reasons, "unstable_scale", "insufficient_evidence") &
        !is.na(flags)
    )
    total_n <- nrow(detection)

    mdisp <- switch(
      method,
      "iqr" = glue::glue(cli::col_yellow(cli::style_bold(
        "IQR (multiplier = {strictness_info$iqr_multiplier})"
      ))),
      "median" = glue::glue(cli::col_yellow(cli::style_bold(
        "Median (MAD x {strictness_info$mad_multiplier})"
      ))),
      "mean" = glue::glue(cli::col_yellow(cli::style_bold(
        "Mean (+/- {strictness_info$sd_multiplier} SD)"
      ))),
      "consensus" = glue::glue(cli::col_yellow(cli::style_bold(
        "Consensus (>={consensus_rule} methods agree)"
      )))
    )

    glue::glue(
      "  - {cli::style_bold(mdisp)} -> ",
      "{fmt_num(out_n)} outliers among eligible ",
      "(total = {fmt_num(total_n)})"
    )
  }

  method_lines <- unlist(lapply(methods, .append_method_line))
  lines <- c(lines, method_lines)

  # Add outbreak classification statistics if enabled
  if (classify_outbreaks) {
    outbreak_cols <- grep("^outlier_flag_.*_outbreak_classified$", names(detection), value = TRUE)
    if (length(outbreak_cols) > 0) {
      lines <- c(lines, "")
      lines <- c(lines, cli::col_yellow(cli::style_bold('Outbreak classification:')))

      for (outbreak_col in outbreak_cols) {
        method_name <- stringr::str_extract(outbreak_col, "(?<=outlier_flag_).*(?=_outbreak_classified)")
        flags <- detection[[outbreak_col]]

        n_outbreak <- sum(flags == "outbreak", na.rm = TRUE)
        n_isolated <- sum(flags == "outlier", na.rm = TRUE)
        n_total_outliers <- n_outbreak + n_isolated

        if (n_total_outliers > 0) {
          outbreak_line <- glue::glue(
            "  - {cli::style_bold(cli::col_yellow(toupper(method_name)))} outliers: ",
            "{fmt_num(n_outbreak)} outbreaks, ",
            "{fmt_num(n_isolated)} isolated ",
            "(total outliers = {fmt_num(n_total_outliers)})"
          )
          lines <- c(lines, outbreak_line)
        }
      }
    }
  }

  is_dark_theme <- .detect_dark_theme()

  # apply base color to all lines while preserving existing colors
  if (is_dark_theme) {
    # For dark theme, use bright white for better visibility
    styled_lines <- sapply(lines, function(line) {
      # Apply white color to the entire line
      # The existing ANSI color codes will override this for colored parts
      cli::col_br_white(line)
    })
  } else {
    # For light theme, use black
    styled_lines <- sapply(lines, function(line) {
      # Apply black color to the entire line
      # The existing ANSI color codes will override this for colored parts
      cli::col_black(line)
    })
  }

  box_width <- min(
    max(cli::ansi_nchar(styled_lines)) + 8L,
    as.integer(cli::console_width())
  )

  cli::cat_line("")
  cli::cat_line(
    cli::boxx(
      styled_lines,
      header = cli::style_bold("Outlier Detection Summary"),
      border_style = "double",
      col = if (is_dark_theme) "white" else "black",
      padding = 1L,
      width = box_width
    )
  )
  cli::cat_line("")
  invisible(NULL)
}

#' Simplify outlier detection categories for better interpretability
#'
#' @description
#' Consolidates detailed outlier detection categories into three intuitive groups:
#' - "normal": Values within expected statistical bounds
#' - "outlier": Values exceeding thresholds (potential anomalies)
#' - "insufficient_data": Various data quality issues preventing determination
#'
#' Classify outbreak patterns from outlier flags
#'
#' Distinguishes between sustained outbreak patterns and isolated outliers
#' using run-length encoding to detect consecutive outlier periods.
#'
#' @param data Data frame containing outlier flag columns
#' @param group_by_columns Character vector of grouping columns (admin, facility)
#' @param outlier_columns Character vector of outlier flag column names
#' @param min_run Minimum consecutive outlier periods to classify as outbreak
#' @param prop_tolerance Proportion tolerance for outbreak classification
#' @param outbreak_max_gap Maximum allowed gap (non-outlier months) between outliers
#'   that can still be considered part of the same outbreak. Default 1 allows
#'   one intervening month (e.g., outlier-normal-outlier = one outbreak)
#' @return Data frame with outbreak classification columns added
#' @keywords internal
#' @noRd
.classify_outbreaks <- function(data, group_by_columns, outlier_columns,
                               min_run = 2, prop_tolerance = 0.9, outbreak_max_gap = 1, verbose = FALSE) {
  # classify sustained outbreak patterns vs isolated outliers
  # uses run-length encoding to detect consecutive outlier periods

  # validate inputs
  if (!is.data.frame(data)) {
    cli::cli_abort("data must be a data.frame")
  }

  missing_cols <- setdiff(c(group_by_columns, outlier_columns), names(data))
  if (length(missing_cols) > 0) {
    cli::cli_abort(c("Missing columns in data:", missing_cols))
  }

  # ensure data is sorted by group and date for run-length encoding
  # identify date column for temporal sorting
  date_col <- if ("yearmon" %in% names(data)) {
    "yearmon"
  } else if ("year" %in% names(data) && "month" %in% names(data)) {
    c("year", "month")
  } else if (".outlier_yearmon" %in% names(data)) {
    ".outlier_yearmon"
  } else {
    NULL
  }

  if (is.null(date_col)) {
    cli::cli_warn("No date column found for temporal sorting in outbreak classification")
    data <- data |>
      dplyr::arrange(dplyr::across(dplyr::all_of(group_by_columns)))
  } else {
    data <- data |>
      dplyr::arrange(dplyr::across(dplyr::all_of(group_by_columns)),
                     dplyr::across(dplyr::all_of(date_col)))
  }

  # classify each outlier column
  for (col in outlier_columns) {
    outbreak_col <- paste0(col, "_outbreak_classified")

    data <- data |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_by_columns))) |>
      dplyr::mutate(
        !!rlang::sym(outbreak_col) := {
          outlier_flag <- .data[[col]]

          # gap-aware clustering to find outbreak patterns
          outlier_positions <- which(outlier_flag == "outlier")
          n_positions <- length(outlier_positions)

          if (n_positions < min_run) {
            # not enough outliers to form any outbreak
            outbreak_expanded <- rep(FALSE, length(outlier_flag))
          } else if (n_positions == 1) {
            # single outlier cannot be an outbreak
            outbreak_expanded <- rep(FALSE, length(outlier_flag))
          } else {
            # calculate gaps between consecutive outlier positions
            gaps <- diff(outlier_positions) - 1  # -1 because consecutive positions have gap=0

            # identify cluster boundaries where gap exceeds max_gap
            cluster_breaks <- which(gaps > outbreak_max_gap)

            # create cluster assignments for outlier positions
            cluster_ids <- rep(1, n_positions)
            if (length(cluster_breaks) > 0) {
              # assign cluster ids based on break points
              for (i in seq_along(cluster_breaks)) {
                break_point <- cluster_breaks[i]
                cluster_ids[(break_point + 1):n_positions] <- i + 1
              }
            }

            # determine which clusters meet min_run criteria
            cluster_sizes <- table(cluster_ids)
            valid_clusters <- as.numeric(names(cluster_sizes)[cluster_sizes >= min_run])

            # mark full outbreak periods (including gaps) in valid clusters
            outbreak_expanded <- rep(FALSE, length(outlier_flag))
            if (length(valid_clusters) > 0) {
              for (cluster_id in valid_clusters) {
                # get outlier positions in this cluster
                cluster_positions <- outlier_positions[cluster_ids == cluster_id]
                # mark entire period from first to last outlier in cluster
                outbreak_start <- min(cluster_positions)
                outbreak_end <- max(cluster_positions)
                outbreak_expanded[outbreak_start:outbreak_end] <- TRUE
              }
            }
          }

          # final classification with gap handling
          dplyr::case_when(
            # positions within outbreak periods but not outliers remain as-is (gaps)
            outbreak_expanded & outlier_flag != "outlier" ~ outlier_flag,
            # outlier positions in outbreak periods become "outbreak"
            outbreak_expanded & outlier_flag == "outlier" ~ "outbreak",
            # isolated outliers remain "outlier"
            outlier_flag == "outlier" ~ "outlier",
            # everything else unchanged
            .default = outlier_flag
          )
        }
      ) |>
      dplyr::ungroup()
  }

  return(data)
}

#' @param data Data frame containing outlier flag columns
#' @param flag_columns Character vector of flag column names to simplify
#' @return Data frame with simplified flag columns
#' @keywords internal
#' @noRd
.simplify_outlier_categories <- function(data, flag_columns) {

  # Define the mapping from detailed to simplified categories
  category_mapping <- c(
    "normal" = "normal",
    "outlier" = "outlier",
    "outbreak" = "outbreak",
    "insufficient_n" = "insufficient_data",
    "insufficient_evidence" = "insufficient_data",
    "unstable_scale" = "insufficient_data",
    "insufficient_seasonal_history" = "insufficient_data",
    "low_reporting" = "insufficient_data",
    "inactive_facility" = "insufficient_data"
  )

  # Apply mapping to each flag column
  for (col in flag_columns) {
    if (col %in% names(data)) {
      data[[col]] <- dplyr::recode(data[[col]], !!!category_mapping, .default = "insufficient_data")
    }
  }

  data
}

#' Create Outlier Detection Plots
#'
#' This function creates plots to visualize outliers in data using the same
#' detection methods as `detect_outliers()`. Only values above the upper
#' thresholds are highlighted as outliers. The plotting function inherits
#' all parameters from the detection function for seamless integration.
#'
#' @inheritParams detect_outliers
#' @param methods Character vector specifying which outlier detection methods
#'   to plot: "iqr" (Interquartile Range), "median" (Median Absolute
#'   Deviation), "mean" (Mean +/- SD), and/or "consensus".
#'   Default is `c("iqr", "median", "mean", "consensus")`.
#'   For consensus, at least two other methods must be selected.
#' @param show_outbreaks Logical. When `TRUE`, displays outbreaks separately
#'   from outliers in the plot using a distinct color (#52AAC2 teal).
#'   The subtitle will show separate counts for outliers (red) and outbreaks
#'   (teal). When `FALSE` (default), outbreaks and outliers are shown
#'   together in red. Only relevant when `classify_outbreaks = TRUE`.
#' @param consensus_colors Logical. When `TRUE` (default), creates a single plot
#'   showing consensus strength across all methods using graduated colors. Point
#'   colors indicate how many methods flagged each observation: grey (normal),
#'   light red (1 method, weak signal), medium red (2 methods, moderate signal),
#'   dark red (3 methods, strong signal). Requires at least 2 detection methods
#'   (excluding "consensus"). When enabled, `show_outbreaks` is automatically set
#'   to `FALSE` as the two visualization modes are mutually exclusive. A legend
#'   is displayed showing the color-to-strength mapping. Set to `FALSE` for
#'   standard multi-method plot behavior (separate plot per method).
#' @param return_plots Character vector specifying which plots to return.
#'   Can be any subset of the methods being computed: "iqr", "median",
#'   "mean", "consensus", or "all" (default). For example, if
#'   `methods = c("iqr", "median", "consensus")` and
#'   `return_plots = c("iqr", "consensus")`, only the IQR and consensus
#'   plots will be returned. All computed plots are still created and
#'   saved (if `plot_path` is specified), but only the requested ones
#'   are returned.
#' @param year_breaks Numeric value specifying the interval for x-axis breaks.
#'   Default `2`. For example, `2` shows every second tick and `3` every third.
#' @param verbose Logical. When `TRUE`, prints an informative summary describing
#'   methods, pooling, strictness, guardrails, and consensus rule.
#'   Default is `FALSE`.
#' @param target_language Character string specifying the language for plot
#'   labels. Defaults to `"en"` (English). Use ISO 639-1 language codes.
#' @param source_language Source language code. If NULL, auto-detection is used.
#'   Defaults to "en".
#' @param lang_cache_path Path to directory for storing translation cache.
#'   Defaults to tempdir().
#' @param plot_path Character string specifying the path where plots should
#'   be saved. If NULL (default), plots are not saved.
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
#'   width is calculated based on number of facets.
#' @param plot_height Numeric. Height of saved plot in inches.
#'   If `NULL` (default), height is calculated from facet count.
#' @param plot_dpi Numeric. Resolution of saved plot in dots per inch.
#'   Default is 300.
#' @param show_plot Logical. If `FALSE`, the plot is returned invisibly
#'   (not displayed). Useful when only saving plots. Default is `TRUE`.
#'
#' @return If a single method is specified, returns a ggplot object. If multiple
#'   methods are specified, returns a named list of ggplot objects
#'   (one per method). When `show_plot` is `FALSE`, returns invisibly.
#'
#' @details
#' The function creates scatter plots showing outliers detected using the
#' specified statistical methods. Each method produces a separate plot with
#' points colored by outlier status. By default, both outliers and outbreaks
#' are shown in red, with normal values in grey. When `show_outbreaks = TRUE`,
#' outbreaks are displayed in teal (#52AAC2) to distinguish them from isolated
#' outliers (red). For consensus plots, the caption shows the consensus rule
#' used. The plots are faceted by administrative levels and include summary
#' statistics in the subtitle. Facet labels longer than 15 characters are
#' automatically truncated with "..." to prevent overcrowding.
#'
#' @examples
#' \dontrun{
#' # Create plots for all methods (default) - returns named list
#' all_plots <- outlier_plot(
#'   data = malaria_data,
#'   column = "confirmed_cases",
#'   date = "date",
#'   record_id = "facility_id"
#' )
#' # Access individual plots: all_plots$iqr, all_plots$median, etc.
#'
#' # Single method - returns single ggplot object
#' iqr_plot <- outlier_plot(
#'   data = malaria_data,
#'   column = "confirmed_cases",
#'   date = "date",
#'   record_id = "facility_id",
#'   methods = "iqr"
#' )
#'
#' # Multiple specific methods
#' selected_plots <- outlier_plot(
#'   data = malaria_data,
#'   column = "confirmed_cases",
#'   date = "date",
#'   record_id = "facility_id",
#'   methods = c("iqr", "median"),
#'   time_mode = "by_month",
#'   strictness = "strict",
#'   year_breaks = 6
#' )
#'
#' # Include consensus (requires 2+ other methods)
#' with_consensus <- outlier_plot(
#'   data = malaria_data,
#'   column = "confirmed_cases",
#'   date = "date",
#'   record_id = "facility_id",
#'   methods = c("iqr", "median", "mean", "consensus"),
#'   consensus_rule = 2,
#'   plot_path = "outliers.png"  # Saves as outliers_iqr.png, etc.
#' )
#'
#' # Detect at facility level but visualize at district level
#' district_plots <- outlier_plot(
#'   data = malaria_data,  # Contains facility-level outlier detection results
#'   column = "confirmed_cases",
#'   date = "date",
#'   record_id = "facility_id",
#'   spatial_level = "facility_id",  # Detection at facility level
#'   admin_level = c("adm1", "adm2"),  # Plot/facet at district level
#'   methods = c("consensus")
#' )
#'
#' # Show outbreaks separately in teal color
#' outbreak_plot <- outlier_plot(
#'   data = malaria_data,
#'   column = "confirmed_cases",
#'   date = "date",
#'   record_id = "facility_id",
#'   methods = "consensus",
#'   classify_outbreaks = TRUE,
#'   show_outbreaks = TRUE  # Outbreaks shown in teal, outliers in red
#' )
#'
#' # Compute multiple methods but return only specific plots
#' selected <- outlier_plot(
#'   data = malaria_data,
#'   column = "confirmed_cases",
#'   date = "date",
#'   record_id = "facility_id",
#'   methods = c("iqr", "median", "mean", "consensus"),
#'   return_plots = c("iqr", "consensus"),  # Only return these two
#'   plot_path = "outliers.png"  # All methods still saved to disk
#' )
#' # Returns list with only $iqr and $consensus plots
#'
#' # Consensus color visualization showing agreement strength
#' consensus_plot <- outlier_plot(
#'   data = malaria_data,
#'   column = "confirmed_cases",
#'   date = "date",
#'   record_id = "facility_id",
#'   methods = c("iqr", "median", "mean"),
#'   consensus_colors = TRUE  # Single plot with color-coded consensus strength
#' )
#' # Returns single ggplot with graduated colors:
#' # grey = normal, light red = 1 method, medium red = 2 methods, dark red = 3 methods
#' }
#' @export
outlier_plot <- function(
    data,
    column,
    record_id = "record_id",
    admin_level = c("adm1", "adm2"),
    spatial_level = "hf_uid",
    date = "date",
    time_mode = c("across_time", "within_year"),
    value_type = c("count", "rate"),
    strictness = c("balanced", "lenient", "strict", "advanced"),
    methods = c("iqr", "median", "mean", "consensus"),
    sd_multiplier = 3,
    mad_constant = 1.4826,
    mad_multiplier = 9,
    iqr_multiplier = 2,
    consensus_rule = 3,
    min_n = 8,
    reporting_rate_col = NULL,
    reporting_rate_min = 0.5,
    key_indicators_hf = NULL,
    classify_outbreaks = FALSE,
    outbreak_min_run = 2,
    outbreak_prop_tolerance = 0.9,
    outbreak_max_gap = 12,
    show_outbreaks = FALSE,
    consensus_colors = TRUE,
    return_plots = "all",
    year_breaks = 2,
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
    verbose = TRUE
) {
  # match choice arguments
  time_mode <- match.arg(time_mode)
  value_type <- match.arg(value_type)
  strictness <- match.arg(strictness)

  # Validate methods selection
  valid_methods <- c("iqr", "median", "mean", "consensus")
  methods <- match.arg(methods, valid_methods, several.ok = TRUE)

  # Validate return_plots parameter
  if (length(return_plots) == 1 && return_plots == "all") {
    return_plots <- methods
  } else {
    invalid_plots <- setdiff(return_plots, valid_methods)
    if (length(invalid_plots) > 0) {
      cli::cli_abort(c(
        "Invalid plot names in return_plots: {.val {invalid_plots}}",
        "i" = "Valid options: {.val {valid_methods}} or 'all'"
      ))
    }
    requested_not_computed <- setdiff(return_plots, methods)
    if (length(requested_not_computed) > 0) {
      cli::cli_abort(c(
        "Requested plots not in methods: {.val {requested_not_computed}}",
        "i" = "Can only return plots that are computed",
        "i" = "Current methods: {.val {methods}}"
      ))
    }
  }

  # Handle backward compatibility and new parameter structure
  # spatial_level replaces old admin_level parameter for detection granularity
  # admin_level replaces old plot_admin_level parameter for plotting/grouping

  # Set up detection admin levels (for calling detect_outliers)
  if (is.null(spatial_level)) {
    # Default to finest granularity available
    detection_admin_level <- admin_level
  } else {
    detection_admin_level <- c(admin_level, spatial_level)
  }

  # Set up plotting admin levels (for aggregation/faceting)
  plot_admin_level <- admin_level

  # Check if consensus is requested with sufficient other methods
  if ("consensus" %in% methods) {
    other_methods <- setdiff(methods, "consensus")
    if (length(other_methods) < 2) {
      cli::cli_abort(c(
        "Consensus requires at least 2 other methods.",
        "i" = "Include two of: 'iqr', 'median', 'mean'."
      ))
    }
  }

  # Resolve strictness preset to get actual multiplier values
  strictness_info <- .resolve_strictness(
    strictness = strictness,
    sd_multiplier = sd_multiplier,
    mad_constant = mad_constant,
    mad_multiplier = mad_multiplier,
    iqr_multiplier = iqr_multiplier
  )

  # Extract resolved values
  sd_multiplier <- strictness_info$sd_multiplier
  mad_constant <- strictness_info$mad_constant
  mad_multiplier <- strictness_info$mad_multiplier
  iqr_multiplier <- strictness_info$iqr_multiplier

  # Validate consensus_colors requirements
  if (consensus_colors) {
    # Check 1: Multiple methods required
    non_consensus_methods <- setdiff(methods, "consensus")
    if (length(non_consensus_methods) < 2) {
      cli::cli_abort(c(
        "consensus_colors requires at least 2 detection methods",
        "i" = "Current methods: {.val {methods}}",
        "i" = "Include at least two of: 'iqr', 'median', 'mean'"
      ))
    }

    # Check 2: Force show_outbreaks = FALSE
    if (show_outbreaks) {
      cli::cli_warn(c(
        "!" = "consensus_colors and show_outbreaks are mutually exclusive",
        "i" = "Setting show_outbreaks = FALSE"
      ))
      show_outbreaks <- FALSE
    }

    # Check 3: Warn about return_plots being ignored
    if (!identical(return_plots, methods) && length(return_plots) > 1) {
      cli::cli_inform(c(
        "i" = "consensus_colors creates a single plot",
        "i" = "return_plots parameter will be ignored"
      ))
    }
  }

  # If data is already outlier detection results, use it directly
  # Otherwise, run outlier detection
  if (all(c("yearmon", "outlier_flag_consensus") %in% names(data))) {
    # Data appears to be outlier detection results already
    outlier_results <- data
  } else {
    # Run outlier detection on raw data using new parameter structure
    outlier_results <- detect_outliers(
      data = data,
      column = column,
      record_id = record_id,
      admin_level = admin_level,
      spatial_level = spatial_level,
      date = date,
      time_mode = time_mode,
      value_type = value_type,
      strictness = strictness,
      sd_multiplier = sd_multiplier,
      mad_constant = mad_constant,
      mad_multiplier = mad_multiplier,
      iqr_multiplier = iqr_multiplier,
      min_n = min_n,
      reporting_rate_col = reporting_rate_col,
      reporting_rate_min = reporting_rate_min,
      key_indicators_hf = key_indicators_hf,
        methods = methods,  # Pass methods to calculate only requested flags
      consensus_rule = consensus_rule,
      output_profile = "audit",  # Get all flags for individual methods
      verbose = FALSE,  # plot helper prints summary itself
      classify_outbreaks = classify_outbreaks,
      outbreak_min_run = outbreak_min_run,
      outbreak_prop_tolerance = outbreak_prop_tolerance,
      outbreak_max_gap = outbreak_max_gap
      )
  }

  # Show summary box once before creating plots
  if (isTRUE(verbose)) {
    .display_outlier_summary_box(
      detection = outlier_results,
      methods = methods,
      column = column,
      time_mode = time_mode,
      admin_level = admin_level,
      strictness = strictness,
      strictness_info = list(
        sd_multiplier = sd_multiplier,
        mad_multiplier = mad_multiplier,
        iqr_multiplier = iqr_multiplier,
        mad_constant = mad_constant
      ),
      min_n = min_n,
      reporting_rate_min = reporting_rate_min,
      consensus_rule = consensus_rule
    )
  }

  # Prepare base data for plotting
  plot_data_base <- outlier_results |>
    dplyr::filter(value > 0) |>
    dplyr::mutate(
      # Convert yearmon to actual Date for proper x-axis scaling
      date_for_plot = lubridate::ymd(paste0(yearmon, "-01"))
    )

  # Determine admin level for faceting (visualization only - no data aggregation)
  # Use the most granular level from admin_level for faceting
  facet_column <- admin_level[length(admin_level)]

  # Note: We do NOT aggregate data when admin_level is specified
  # The admin_level parameter only changes faceting for visualization
  # This preserves the full facility-level dataset and sample size

  # Create list to store plots
  plot_list <- list()

  # Consensus colors mode: create single plot with color-coded consensus strength
  if (consensus_colors) {
    # calculate consensus strength for each point
    # extract flag columns, using NA if they don't exist
    iqr_flags <- if ("outlier_flag_iqr" %in% names(plot_data_base)) {
      plot_data_base$outlier_flag_iqr
    } else {
      rep(NA_character_, nrow(plot_data_base))
    }
    median_flags <- if ("outlier_flag_median" %in% names(plot_data_base)) {
      plot_data_base$outlier_flag_median
    } else {
      rep(NA_character_, nrow(plot_data_base))
    }
    mean_flags <- if ("outlier_flag_mean" %in% names(plot_data_base)) {
      plot_data_base$outlier_flag_mean
    } else {
      rep(NA_character_, nrow(plot_data_base))
    }

    plot_data_consensus <- plot_data_base |>
      dplyr::mutate(
        consensus_strength = .calculate_consensus_strength(
          outlier_flag_iqr = iqr_flags,
          outlier_flag_median = median_flags,
          outlier_flag_mean = mean_flags,
          methods = methods
        )
      ) |>
      # sort so normal plots first, strong plots last (visibility)
      dplyr::arrange(consensus_strength)

    # define color palette: blue -> yellow -> red for maximum distinction
    strength_colors <- c(
      "normal"   = "grey70",      # unchanged
      "weak"     = "#3288BD",     # medium blue (1 method)
      "moderate" = "#FDAE61",     # bright yellow-orange (2 methods)
      "strong"   = "#A50026"      # dark red (3 methods)
    )

    # calculate summary statistics for subtitle
    summary_stats <- plot_data_consensus |>
      dplyr::count(consensus_strength) |>
      dplyr::mutate(
        pct = round(100 * n / sum(n), 1),
        n_fmt = sntutils::big_mark(n)
      )

    # extract counts for each level
    strong_n <- summary_stats$n_fmt[summary_stats$consensus_strength == "strong"]
    strong_pct <- summary_stats$pct[summary_stats$consensus_strength == "strong"]
    moderate_n <- summary_stats$n_fmt[summary_stats$consensus_strength == "moderate"]
    moderate_pct <- summary_stats$pct[summary_stats$consensus_strength == "moderate"]
    weak_n <- summary_stats$n_fmt[summary_stats$consensus_strength == "weak"]
    weak_pct <- summary_stats$pct[summary_stats$consensus_strength == "weak"]

    # handle missing levels (set to 0 if not present)
    if (length(strong_n) == 0) {
      strong_n <- "0"
      strong_pct <- 0
    }
    if (length(moderate_n) == 0) {
      moderate_n <- "0"
      moderate_pct <- 0
    }
    if (length(weak_n) == 0) {
      weak_n <- "0"
      weak_pct <- 0
    }

    # build subtitle
    if (target_language != "en") {
      detected_word <- translate_text(
        "Detected",
        target_language = target_language,
        source_language = source_language,
        cache_path = lang_cache_path
      )
      strong_word <- translate_text(
        "strong",
        target_language = target_language,
        source_language = source_language,
        cache_path = lang_cache_path
      )
      moderate_word <- translate_text(
        "moderate",
        target_language = target_language,
        source_language = source_language,
        cache_path = lang_cache_path
      )
      weak_word <- translate_text(
        "weak",
        target_language = target_language,
        source_language = source_language,
        cache_path = lang_cache_path
      )
      outliers_word <- translate_text(
        "outliers",
        target_language = target_language,
        source_language = source_language,
        cache_path = lang_cache_path
      )
      methods_word <- translate_text(
        "methods",
        target_language = target_language,
        source_language = source_language,
        cache_path = lang_cache_path
      )
      method_word <- translate_text(
        "method",
        target_language = target_language,
        source_language = source_language,
        cache_path = lang_cache_path
      )

      subtitle_text <- glue::glue(
        "{detected_word}: {strong_n} ",
        "<b style='color:#A50026'>{strong_word} (3 {methods_word})</b> ({strong_pct}%), ",
        "{moderate_n} ",
        "<b style='color:#FDAE61'>{moderate_word} (2 {methods_word})</b> ({moderate_pct}%), ",
        "{weak_n} ",
        "<b style='color:#3288BD'>{weak_word} (1 {method_word})</b> ({weak_pct}%) ",
        "{outliers_word}"
      )
    } else {
      subtitle_text <- glue::glue(
        "Detected: {strong_n} ",
        "<b style='color:#A50026'>strong (3 methods)</b> ({strong_pct}%), ",
        "{moderate_n} ",
        "<b style='color:#FDAE61'>moderate (2 methods)</b> ({moderate_pct}%), ",
        "{weak_n} ",
        "<b style='color:#3288BD'>weak (1 method)</b> ({weak_pct}%) outliers"
      )
    }

    # build title
    if (target_language != "en") {
      multi_method_text <- translate_text(
        "Multi-Method Consensus",
        target_language = target_language,
        source_language = source_language,
        cache_path = lang_cache_path
      )
      outlier_detection_text <- translate_text(
        "Outlier Detection for",
        target_language = target_language,
        source_language = source_language,
        cache_path = lang_cache_path
      )
      title_text <- glue::glue(
        "{multi_method_text}: {outlier_detection_text} <b>{column}</b>"
      )
    } else {
      title_text <- glue::glue(
        "Multi-Method Consensus: Outlier Detection for <b>{column}</b>"
      )
    }

    # translate axis labels
    if (target_language != "en") {
      x_label <- translate_text(
        "Date",
        target_language = target_language,
        source_language = source_language,
        cache_path = lang_cache_path
      )
      y_label <- translate_text(
        "Value",
        target_language = target_language,
        source_language = source_language,
        cache_path = lang_cache_path
      )
    } else {
      x_label <- "Date"
      y_label <- "Value"
    }

    # create summary for facet labels
    percent_summary <- plot_data_consensus |>
      dplyr::group_by(dplyr::across(dplyr::all_of(facet_column))) |>
      dplyr::summarise(
        n_outlier = sum(consensus_strength != "normal", na.rm = TRUE),
        n_total = dplyr::n(),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        pct_outlier = round(100 * n_outlier / n_total, 1),
        n_outlier_fmt = sntutils::big_mark(n_outlier),
        n_total_fmt = sntutils::big_mark(n_total),
        facet_value = .data[[facet_column]],
        # truncate facet value to 15 characters
        facet_value_display = ifelse(
          nchar(as.character(facet_value)) > 15,
          paste0(substr(as.character(facet_value), 1, 15), "..."),
          as.character(facet_value)
        ),
        label = glue::glue(
          "{facet_value_display}\n {pct_outlier}% ({n_outlier_fmt}/{n_total_fmt})"
        )
      )

    facet_labels <- setNames(
      percent_summary$label,
      percent_summary[[facet_column]]
    )

    # build caption showing methods and parameters
    non_consensus_methods <- setdiff(methods, "consensus")
    method_details <- c()

    if ("iqr" %in% non_consensus_methods) {
      method_details <- c(method_details,
        glue::glue("IQR (Q3 + {iqr_multiplier}×IQR)"))
    }
    if ("median" %in% non_consensus_methods) {
      method_details <- c(method_details,
        glue::glue("Median (median + {mad_multiplier}×MAD[k={mad_constant}])"))
    }
    if ("mean" %in% non_consensus_methods) {
      method_details <- c(method_details,
        glue::glue("Mean (mean + {sd_multiplier}×SD)"))
    }

    # add detection level clarification if levels differ
    detection_level_text <- ""
    if (!is.null(spatial_level) && spatial_level != facet_column) {
      if (target_language != "en") {
        det_word <- translate_text(
          "Detection",
          target_language = target_language,
          source_language = source_language,
          cache_path = lang_cache_path
        )
        disp_word <- translate_text(
          "Display",
          target_language = target_language,
          source_language = source_language,
          cache_path = lang_cache_path
        )
        level_word <- translate_text(
          "level",
          target_language = target_language,
          source_language = source_language,
          cache_path = lang_cache_path
        )
        detection_level_text <- glue::glue(
          "{det_word}: {spatial_level} {level_word} | ",
          "{disp_word}: {facet_column} {level_word}"
        )
      } else {
        detection_level_text <- glue::glue(
          "Detection: {spatial_level} level | Display: {facet_column} level"
        )
      }
    }

    # build caption with optional detection level note + methods
    if (detection_level_text != "") {
      caption_text <- paste0(
        detection_level_text,
        "\n",
        paste(method_details, collapse = "; ")
      )
    } else {
      caption_text <- paste(method_details, collapse = "; ")
    }

    # create the consensus plot
    p <- ggplot2::ggplot(plot_data_consensus) +
      ggplot2::geom_point(
        ggplot2::aes(
          x = date_for_plot,
          y = value,
          color = consensus_strength
        ),
        size = 2,
        alpha = 0.7
      ) +
      ggplot2::scale_color_manual(
        values = strength_colors,
        guide = "none"
      ) +
      ggplot2::labs(
        title = title_text,
        subtitle = subtitle_text,
        x = NULL,
        y = paste0(y_label, "\n"),
        caption = caption_text
      ) +
      ggplot2::facet_wrap(
        stats::as.formula(paste("~", facet_column)),
        scales = "free_y",
        labeller = ggplot2::labeller(!!facet_column := facet_labels)
      ) +
      ggplot2::scale_x_date(
        date_breaks = paste(year_breaks, "months"),
        labels = function(x) {
          sntutils::translate_yearmon(
            x,
            language = target_language,
            format = "%Y-%m"
          )
        },
        expand = ggplot2::expansion(mult = 0.02)
      ) +
      ggplot2::scale_y_continuous(
        labels = scales::label_number(
          scale_cut = scales::cut_short_scale()
        ),
        expand = ggplot2::expansion(mult = c(0.02, 0.1))
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        legend.position = "none",
        axis.text.x = ggplot2::element_text(
          size = 8,
          angle = 70,
          hjust = 1
        ),
        axis.text.y = ggplot2::element_text(size = 8),
        strip.text = ggplot2::element_text(size = 9),
        plot.title = ggtext::element_markdown(size = 14),
        plot.subtitle = ggtext::element_markdown(size = 11),
        panel.grid.minor = ggplot2::element_blank(),
        panel.grid.major.x = ggplot2::element_line(
          color = "grey90",
          linewidth = 0.5
        ),
        panel.grid.major.y = ggplot2::element_line(
          color = "grey90",
          linewidth = 0.5
        ),
        panel.border = ggplot2::element_rect(fill = NA, linewidth = 0.4)
      )

    # store plot
    plot_list[["consensus"]] <- p

    # save plot if path is provided
    if (!is.null(plot_path)) {
      .save_outlier_plot(
        plot = p,
        plot_path = tools::file_path_sans_ext(plot_path),
        method_name = "consensus_colors",
        column = column,
        yearmon = "yearmon",
        adm_level = facet_column,
        data = plot_data_consensus,
        target_language = target_language,
        source_language = source_language,
        lang_cache_path = lang_cache_path,
        compress_image = compress_image,
        image_overwrite = image_overwrite,
        compression_speed = compression_speed,
        compression_verbose = compression_verbose,
        plot_scale = plot_scale,
        plot_width = plot_width,
        plot_height = plot_height,
        plot_dpi = plot_dpi
      )
    }

  } else {
    # Standard multi-method mode

  # Create plots for each requested method
  for (method in methods) {
    # Determine which flag column to use
    flag_column <- switch(method,
      "iqr" = "outlier_flag_iqr",
      "median" = "outlier_flag_median",
      "mean" = "outlier_flag_mean",
      "consensus" = "outlier_flag_consensus"
    )

    # Filter data for this method and add flag
    plot_data <- plot_data_base |>
      dplyr::filter(
        !is.na(.data[[flag_column]]),
        .data[[flag_column]] != "insufficient_evidence"
      ) |>
      dplyr::mutate(
        # Create flag that distinguishes outbreaks when show_outbreaks = TRUE
        .plot_flag = if (show_outbreaks) {
          dplyr::case_when(
            .data[[flag_column]] == "outbreak" ~ "outbreak",
            .data[[flag_column]] == "outlier" ~ "outlier",
            TRUE ~ "normal"
          )
        } else {
          dplyr::case_when(
            .data[[flag_column]] %in% c("outlier", "outbreak") ~ "outlier",
            TRUE ~ "normal"
          )
        },
        # Different alpha values: flagged points more opaque
        .alpha_val = dplyr::case_when(
          .data[[flag_column]] %in% c("outlier", "outbreak") ~ 0.9,
          TRUE ~ 0.5
        )
      ) |>
      # Sort so flagged points are plotted on top
      dplyr::arrange(.plot_flag)


    # Create summary for facet labels
    percent_summary <- plot_data |>
      dplyr::group_by(dplyr::across(dplyr::all_of(facet_column))) |>
      dplyr::summarise(
        n_outlier = sum(.data[[flag_column]] %in% c("outlier", "outbreak"), na.rm = TRUE),
        n_total = dplyr::n(),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        pct_outlier = round(100 * n_outlier / n_total, 1),
        n_outlier_fmt = sntutils::big_mark(n_outlier),
        n_total_fmt = sntutils::big_mark(n_total),
        facet_value = .data[[facet_column]]
      )

    # Translate "Outliers" if needed
    outliers_label <- if (target_language != "en") {
      translate_text(
        "Outliers",
        target_language = target_language,
        source_language = source_language,
        cache_path = lang_cache_path
      )
    } else {
      "Outliers"
    }

    # Create labels without bold formatting
    percent_summary <- percent_summary |>
      dplyr::mutate(
        # Truncate facet value to 15 characters
        facet_value_display = ifelse(
          nchar(as.character(facet_value)) > 15,
          paste0(substr(as.character(facet_value), 1, 15), "..."),
          as.character(facet_value)
        ),
        label = glue::glue(
          "{facet_value_display}\n {pct_outlier}% ({n_outlier_fmt}/{n_total_fmt})"
        )
      )

    facet_labels <- setNames(
      percent_summary$label,
      percent_summary[[facet_column]]
    )

    # Calculate counts
    outliers_n <- sntutils::big_mark(
      sum(plot_data[[flag_column]] == "outlier", na.rm = TRUE)
    )
    outbreaks_n <- sntutils::big_mark(
      sum(plot_data[[flag_column]] == "outbreak", na.rm = TRUE)
    )
    total_records <- sntutils::big_mark(nrow(plot_data))

    # Method display names
    method_display <- switch(method,
      "iqr" = glue::glue("IQR (multiplier = {iqr_multiplier})"),
      "median" = glue::glue("Median (MAD x {mad_multiplier})"),
      "mean" = glue::glue("Mean (+/- {sd_multiplier} SD)"),
      "consensus" = glue::glue("Consensus (>={consensus_rule} methods agree)")
    )

    # Note: verbose output is now shown once before the plotting loop

    # Prepare labels for translation
    if (target_language != "en") {
      # Translate method names and components
      method_name_translated <- switch(
        method,
        "iqr" = "IQR",
        "median" = translate_text(
          "Median",
          target_language = target_language,
          source_language = source_language,
          cache_path = lang_cache_path
        ),
        "mean" = translate_text(
          "Mean",
          target_language = target_language,
          source_language = source_language,
          cache_path = lang_cache_path
        ),
        "consensus" = translate_text(
          "Consensus",
          target_language = target_language,
          source_language = source_language,
          cache_path = lang_cache_path
        )
      )

      multiplier_text <- translate_text(
        "multiplier",
        target_language = target_language,
        source_language = source_language,
        cache_path = lang_cache_path
      )
      methods_agree_text <- translate_text(
        "methods agree",
        target_language = target_language,
        source_language = source_language,
        cache_path = lang_cache_path
      )

      # Build translated method display
      method_display_translated <- switch(
        method,
        "iqr" = glue::glue(
          "{method_name_translated} ({multiplier_text} = ",
          "{iqr_multiplier})"
        ),
        "median" = glue::glue(
          "{method_name_translated} (MAD x {mad_multiplier})"
        ),
        "mean" = glue::glue(
          "{method_name_translated} (+/- {sd_multiplier} SD)"
        ),
        "consensus" = glue::glue(
          "{method_name_translated} (>={consensus_rule} ",
          "{methods_agree_text})"
        )
      )

      # Translate "Outlier Detection for"
      outlier_detection_text <- translate_text(
        "Outlier Detection for",
        target_language = target_language,
        source_language = source_language,
        cache_path = lang_cache_path
      )
      title_text <- glue::glue(
        "{method_display_translated}: {outlier_detection_text} <b>{column}</b>"
      )

      # Translate words for subtitle
      outliers_word <- translate_text(
        "outliers",
        target_language = target_language,
        source_language = source_language,
        cache_path = lang_cache_path
      )
      outbreaks_word <- translate_text(
        "outbreaks",
        target_language = target_language,
        source_language = source_language,
        cache_path = lang_cache_path
      )
      detected_word <- translate_text(
        "detected",
        target_language = target_language,
        source_language = source_language,
        cache_path = lang_cache_path
      )
      there_were <- translate_text(
        "There were",
        target_language = target_language,
        source_language = source_language,
        cache_path = lang_cache_path
      )

      # Build subtitle based on show_outbreaks parameter
      if (show_outbreaks) {
        subtitle_text <- glue::glue(
          "{there_were} {outliers_n}/{total_records}",
          " <b style='color:red;font-weight:bold'>{outliers_word}</b>",
          " and {outbreaks_n}/{total_records}",
          " <b style='color:#52AAC2;font-weight:bold'>{outbreaks_word}</b>",
          " {detected_word}"
        )
      } else {
        subtitle_text <- glue::glue(
          "{there_were} {outliers_n}/{total_records}",
          " <b style='color:red;font-weight:bold'>",
          "{outliers_word}</b> {detected_word}"
        )
      }

      x_label <- translate_text(
        "Date",
        target_language = target_language,
        source_language = source_language,
        cache_path = lang_cache_path
      )
      y_label <- translate_text(
        "Value",
        target_language = target_language,
        source_language = source_language,
        cache_path = lang_cache_path
      )
    } else {
      # English version
      title_text <- glue::glue(
        "{method_display}: Outlier Detection for <b>{column}</b>"
      )

      # Build subtitle based on show_outbreaks parameter
      if (show_outbreaks) {
        subtitle_text <- glue::glue(
          "There were {outliers_n}/{total_records}",
          " <b style='color:red;font-weight:bold'>outliers</b>",
          " and {outbreaks_n}/{total_records}",
          " <b style='color:#52AAC2;font-weight:bold'>outbreaks</b> detected"
        )
      } else {
        subtitle_text <- glue::glue(
          "There were {outliers_n}/{total_records}",
          " <b style='color:red;font-weight:bold'>outliers</b> detected"
        )
      }

      x_label <- "Date"
      y_label <- "Value"
    }

    # Add detection level clarification if levels differ
    detection_level_caption <- ""
    if (!is.null(spatial_level) && spatial_level != facet_column) {
      if (target_language != "en") {
        det_word <- translate_text(
          "Detection",
          target_language = target_language,
          source_language = source_language,
          cache_path = lang_cache_path
        )
        disp_word <- translate_text(
          "Display",
          target_language = target_language,
          source_language = source_language,
          cache_path = lang_cache_path
        )
        level_word <- translate_text(
          "level",
          target_language = target_language,
          source_language = source_language,
          cache_path = lang_cache_path
        )
        detection_level_caption <- glue::glue(
          "{det_word}: {spatial_level} {level_word} | ",
          "{disp_word}: {facet_column} {level_word}"
        )
      } else {
        detection_level_caption <- glue::glue(
          "Detection: {spatial_level} level | Display: {facet_column} level"
        )
      }
    }

    # Add caption for consensus explaining the rule
    consensus_caption <- if (method == "consensus") {
      if (target_language != "en") {
        consensus_rule_text <- translate_text(
          "Consensus rule",
          target_language = target_language,
          source_language = source_language,
          cache_path = lang_cache_path
        )
        at_least_text <- translate_text(
          "At least",
          target_language = target_language,
          source_language = source_language,
          cache_path = lang_cache_path
        )
        of_text <- translate_text(
          "of",
          target_language = target_language,
          source_language = source_language,
          cache_path = lang_cache_path
        )
        must_agree_text <- translate_text(
          "methods must agree to flag an outlier",
          target_language = target_language,
          source_language = source_language,
          cache_path = lang_cache_path
        )
        glue::glue(
          "{consensus_rule_text}: {at_least_text} {consensus_rule} ",
          "{of_text} {length(setdiff(methods, 'consensus'))} ",
          "{must_agree_text}"
        )
      } else {
        glue::glue(
          "Consensus rule: At least {consensus_rule} of ",
          "{length(setdiff(methods, 'consensus'))} methods must agree ",
          "to flag an outlier"
        )
      }
    } else {
      ""
    }

    # combine detection level note and method-specific caption
    plot_caption <- if (detection_level_caption != "" && consensus_caption != "") {
      paste0(detection_level_caption, "\n ", consensus_caption)
    } else if (detection_level_caption != "") {
      detection_level_caption
    } else if (consensus_caption != "") {
      consensus_caption
    } else {
      NULL
    }

    # Create the plot for this method
    p <- ggplot2::ggplot(plot_data) +
      ggplot2::geom_point(
        ggplot2::aes(
          x = date_for_plot,
          y = value,
          color = .plot_flag,
          alpha = .alpha_val
        ),
        size = 2
      ) +
      ggplot2::scale_color_manual(
        values = if (show_outbreaks) {
          c("outbreak" = "#52AAC2", "outlier" = "red", "normal" = "grey")
        } else {
          c("outlier" = "red", "normal" = "grey")
        }
      ) +
      ggplot2::scale_alpha_identity() +  # Use alpha values directly
      ggplot2::labs(
        title = title_text,
        subtitle = subtitle_text,
        x = paste0("\n", x_label),
        y = paste0(y_label, "\n"),
        caption = plot_caption
      ) +
      ggplot2::facet_wrap(
        stats::as.formula(paste("~", facet_column)),
        scales = "free_y",
        labeller = ggplot2::labeller(!!facet_column := facet_labels)
      ) +
      ggplot2::scale_x_date(
        date_breaks = paste(year_breaks, "months"),
        labels = function(x) {
          sntutils::translate_yearmon(
            x,
            language = target_language,
            format = "%Y-%m"
          )
        },
        expand = ggplot2::expansion(mult = 0.02)
      ) +
      ggplot2::scale_y_continuous(
        labels = scales::label_number(
          scale_cut = scales::cut_short_scale()
        ),
        expand = ggplot2::expansion(mult = c(0.02, 0.1))
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        legend.position = "none",  # No legends, only coloring in subtitles
        axis.text.x = ggplot2::element_text(
          size = 8,
          angle = 70,  # 70 degree rotation as requested
          hjust = 1
        ),
        axis.text.y = ggplot2::element_text(size = 8),
        strip.text = ggplot2::element_text(size = 9),
        plot.title = ggtext::element_markdown(size = 14),
        plot.subtitle = ggtext::element_markdown(size = 11),
        plot.caption = ggplot2::element_text(
          size = 9,
          hjust = 0,
          face = "italic"
        ),
        # Only major grids
        panel.grid.minor = ggplot2::element_blank(),
        panel.grid.major.x = ggplot2::element_line(
          color = "grey90",
          linewidth = 0.5
        ),
        panel.grid.major.y = ggplot2::element_line(
          color = "grey90",
          linewidth = 0.5
        ),
        panel.border = ggplot2::element_rect(fill = NA, linewidth = 0.4)
      )

    # Store plot in list
    plot_list[[method]] <- p

    # Save plot if path is provided
    if (!is.null(plot_path)) {

      .save_outlier_plot(
        plot = p,
        plot_path = tools::file_path_sans_ext(plot_path),
        method_name = method,
        column = column,
        yearmon = "yearmon",
        adm_level = facet_column,
        data = plot_data,
        target_language = target_language,
        source_language = source_language,
        lang_cache_path = lang_cache_path,
        compress_image = compress_image,
        image_overwrite = image_overwrite,
        compression_speed = compression_speed,
        compression_verbose = compression_verbose,
        plot_scale = plot_scale,
        plot_width = plot_width,
        plot_height = plot_height,
        plot_dpi = plot_dpi
      )
    }

  }  # End of methods loop
  }  # End of else (standard multi-method mode)

  # Filter plots based on return_plots parameter
  filtered_plot_list <- if (consensus_colors) {
    plot_list  # return full list (single consensus plot)
  } else {
    plot_list[return_plots]  # filter by return_plots
  }

  # Return single plot or list of plots
  if (length(filtered_plot_list) == 1) {
    if (!show_plot) {
      return(invisible(filtered_plot_list[[1]]))
    }
    return(filtered_plot_list[[1]])
  } else {
    if (!show_plot) {
      return(invisible(filtered_plot_list))
    }
    return(filtered_plot_list)
  }
}

#' Save outlier plot with appropriate naming
#'
#' @noRd
.save_outlier_plot <- function(plot, plot_path, method_name, column, yearmon,
                              adm_level, data, target_language = "en",
                              source_language = "en",
                              lang_cache_path = tempdir(),
                              compress_image = FALSE,
                              image_overwrite = TRUE,
                              compression_speed = 1,
                              compression_verbose = TRUE,
                              plot_scale = 1,
                              plot_width = NULL,
                              plot_height = NULL,
                              plot_dpi = 300) {
  # Helper function to handle translation without messages
  .translate_quiet <- function(text) {
    if (target_language != source_language) {
      suppressMessages(
        translate_text(
          text,
          target_language = target_language,
          source_language = source_language,
          cache_path = lang_cache_path
        )
      )
    } else {
      text
    }
  }

  # Create directory if it doesn't exist
  if (!dir.exists(plot_path)) {
    dir_created <- dir.create(plot_path, recursive = TRUE, showWarnings = FALSE)
    if (!dir_created) {
      cli::cli_warn("Could not create directory: {plot_path}")
      return(invisible(NULL))
    }
  }

  # Translate terms for filename
  # Translate "outlier plot" as two separate words for better translation
  outlier_tr <- .translate_quiet("outlier") |>
    tolower()
  plot_tr <- .translate_quiet("plot") |>
    tolower()
  outlier_plot_tr <- paste(gsub(" ", "_", outlier_tr), plot_tr, sep = "_")

  method_tr <- .translate_quiet(method_name) |>
    tolower()

  for_tr <- .translate_quiet("for")

  by_tr <- .translate_quiet("by")

  # Translate yearmon label
  yearmon_label <- if (yearmon == "yearmon") {
    .translate_quiet("year_month") |>
      tolower() |>
      gsub(" ", "_", x = _)
  } else {
    yearmon
  }

  adm_level_tr <- .translate_quiet(adm_level) |>
    tolower() |>
    gsub(" ", "_", x = _)

  # Get year range
  year_range <- if (!is.null(data$year) && length(unique(data$year)) > 1) {
    glue::glue("{min(data$year, na.rm = TRUE)}-{max(data$year, na.rm = TRUE)}")
  } else if (!is.null(data$year)) {
    as.character(min(data$year, na.rm = TRUE))
  } else {
    format(Sys.Date(), "%Y")
  }

  # Construct filename
  save_path <- glue::glue(
    "{outlier_plot_tr}_{method_tr}_{for_tr}_{column}_{by_tr}_",
    "{yearmon_label}_&_{adm_level_tr}_{year_range}_",
    "v{format(Sys.Date(), '%Y-%m-%d')}.png"
  )

  full_path <- file.path(plot_path, save_path)

  # Calculate dimensions based on facets or use provided values
  n_facets <- length(unique(data[[adm_level]]))
  if (is.null(plot_width)) {
    width <- max(10, min(20, n_facets * 3))
  } else {
    width <- plot_width
  }

  if (is.null(plot_height)) {
    height <- max(8, min(15, ceiling(n_facets / 3) * 4))
  } else {
    height <- plot_height
  }

  # Try to save the plot
  tryCatch({
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
    if (compress_image && file.exists(full_path)) {
      compress_png(
        full_path,
        verbosity = compression_verbose,
        speed = compression_speed,
        png_overwrite = image_overwrite
      )
    }

    success_msg <- .translate_quiet("Plot saved to:")
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

  invisible(full_path)
}

#' validate detect_outliers inputs and derive core columns
#'
#' @param data input data frame
#' @param column target column name
#' @param record_id record id column name
#' @param admin_level admin columns vector
#' @param year year column name
#' @param yearmon year-month column name
#' @param month optional month column name
#' @param value_type value type string
#' @param time_mode time grouping mode
#' @param output_profile output profile string
#' @param strictness strictness label
#' @param consensus_rule integer consensus rule
#' @param min_n minimum observations per bucket
#' @param reporting_rate_col optional reporting rate column
#' @param reporting_rate_min minimum reporting threshold
#' @noRd
.validate_outlier_inputs <- function(
    data,
    column,
    record_id,
    admin_level,
    year,
    yearmon,
    month,
    value_type,
    time_mode,
    output_profile,
    strictness,
    consensus_rule,
    min_n,
    reporting_rate_col,
    reporting_rate_min,
    spatial_level = NULL) {
  if (!inherits(data, "data.frame")) {
    cli::cli_abort("{.arg data} must be a data frame.")
  }

  cleaned <- dplyr::as_tibble(data)

  required_cols <- c(column, record_id, year, yearmon)
  missing_cols <- required_cols[!required_cols %in% names(cleaned)]
  if (base::length(missing_cols) > 0) {
    cli::cli_abort(
      "Missing required columns: {.val {missing_cols}}"
    )
  }

  if (!is.numeric(cleaned[[column]])) {
    cli::cli_abort("{.val {column}} must be numeric.")
  }

  if (!is.null(reporting_rate_col)) {
    if (!reporting_rate_col %in% names(cleaned)) {
      cli::cli_abort(
        "Reporting column {.val {reporting_rate_col}} not found."
      )
    }
    if (!is.numeric(cleaned[[reporting_rate_col]])) {
      cli::cli_abort(
        "Reporting column {.val {reporting_rate_col}} must be numeric."
      )
    }
  }

  allowed_value_type <- c("count", "rate")
  if (!value_type %in% allowed_value_type) {
    cli::cli_abort(
      "{.arg value_type} must be one of {.val {allowed_value_type}}."
    )
  }

  allowed_time_mode <- c("within_year", "across_time", "by_month")
  if (!time_mode %in% allowed_time_mode) {
    cli::cli_abort(
      "{.arg time_mode} must be one of {.val {allowed_time_mode}}."
    )
  }

  allowed_profiles <- c("lean", "standard", "audit")
  if (!output_profile %in% allowed_profiles) {
    cli::cli_abort(
      "{.arg output_profile} must be one of {.val {allowed_profiles}}."
    )
  }

  allowed_strictness <- c("lenient", "balanced", "strict", "advanced")
  if (!strictness %in% allowed_strictness) {
    cli::cli_abort(
      "{.arg strictness} must be one of {.val {allowed_strictness}}."
    )
  }

  if (!base::is.numeric(consensus_rule) ||
      base::length(consensus_rule) != 1 ||
      consensus_rule < 1 || consensus_rule > 3) {
    cli::cli_abort(
      "{.arg consensus_rule} must be 1, 2, or 3."
    )
  }

  if (!base::is.numeric(min_n) || min_n < 1) {
    cli::cli_abort("{.arg min_n} must be >= 1.")
  }

  parsed_year <- .coerce_year_column(cleaned[[year]], year)
  parsed_month <- .derive_month_column(cleaned, yearmon, month)

  cleaned <- cleaned |>
    dplyr::mutate(
      .outlier_value = .data[[column]],
      .outlier_year = parsed_year,
      .outlier_yearmon = .data[[yearmon]],
      .outlier_month = parsed_month,
      .outlier_reporting = if (!is.null(reporting_rate_col)) {
        .data[[reporting_rate_col]]
      } else {
        NA_real_
      }
    )

  list(
    data = cleaned,
    column = column,
    record_id = record_id,
    admin_level = admin_level,
    year = year,
    yearmon = yearmon,
    month = ".outlier_month",
    value_type = value_type,
    time_mode = time_mode,
    output_profile = output_profile,
    strictness = strictness,
    consensus_rule = consensus_rule,
    min_n = min_n,
    reporting_rate_col = reporting_rate_col,
    reporting_rate_min = reporting_rate_min,
    spatial_level = spatial_level
  )
}

#' derive month column from provided month or yearmon
#'
#' @param data data frame
#' @param yearmon year-month column name
#' @param month optional month column name
#' @noRd
.derive_month_column <- function(data, yearmon, month) {
  if (!is.null(month)) {
    if (!month %in% names(data)) {
      cli::cli_abort("Month column {.val {month}} not found.")
    }
    return(.coerce_month_vector(data[[month]], month))
  }

  parsed <- .coerce_yearmon_to_month(data[[yearmon]])
  if (any(is.na(parsed))) {
    cli::cli_abort(
      "Could not derive month from {.val {yearmon}}; provide {.arg month}."
    )
  }
  parsed
}

#' coerce a supplied month column to integers 1-12
#'
#' @param values input vector
#' @param column_name name of source column
#' @noRd
.coerce_month_vector <- function(values, column_name) {
  if (is.numeric(values)) {
    if (!all(values >= 1 & values <= 12, na.rm = TRUE)) {
      cli::cli_abort(
        "Month column {.val {column_name}} must contain values 1-12."
      )
    }
    return(as.integer(values))
  }

  if (inherits(values, "Date")) {
    return(lubridate::month(values))
  }

  if (inherits(values, "POSIXt")) {
    return(lubridate::month(values))
  }

  if (is.character(values)) {
    lowered <- tolower(values)
    month_match <- match(lowered, tolower(base::month.name))
    if (!any(!is.na(month_match))) {
      # try numerical strings or YYYY-MM
      suppressWarnings({
        numeric_guess <- as.numeric(values)
      })
      if (all(!is.na(numeric_guess))) {
        return(.coerce_month_vector(numeric_guess, column_name))
      }
      parsed <- .coerce_yearmon_to_month(values)
      if (all(!is.na(parsed))) {
        return(parsed)
      }
      cli::cli_abort(
        "Cannot parse month column {.val {column_name}}."
      )
    }
    return(as.integer(month_match))
  }

  cli::cli_abort(
    "Month column {.val {column_name}} must be numeric, Date, or character."
  )
}

#' parse yearmon values into month integers
#'
#' @param values vector containing year-month values
#' @noRd
.coerce_yearmon_to_month <- function(values) {
  if (inherits(values, "Date")) {
    return(lubridate::month(values))
  }

  if (inherits(values, "yearmon")) {
    return(as.integer(lubridate::month(zoo::as.Date(values))))
  }

  if (is.numeric(values)) {
    # expect format YYYYMM or fractional year
    if (all(values > 100, na.rm = TRUE)) {
      return(as.integer(values %% 100))
    }
    # fallback: treat numeric as fraction of year
    floored <- floor((values %% 1) * 12 + 1)
    return(as.integer(floored))
  }

  if (is.character(values)) {
    parsed <- suppressWarnings(lubridate::ym(values))
    if (all(!is.na(parsed))) {
      return(lubridate::month(parsed))
    }
    parsed_dash <- suppressWarnings(lubridate::parse_date_time(
      values,
      orders = c("Y-m", "Y/m", "Ym")
    ))
    if (all(!is.na(parsed_dash))) {
      return(lubridate::month(parsed_dash))
    }
  }

  rep(NA_integer_, length(values))
}

#' coerce year column into numeric vector
#'
#' @param values year column values
#' @param column_name column name
#' @noRd
.coerce_year_column <- function(values, column_name) {
  if (is.numeric(values)) {
    return(as.integer(values))
  }

  if (is.character(values)) {
    suppressWarnings({
      numeric_year <- as.numeric(values)
    })
    if (all(!is.na(numeric_year))) {
      return(as.integer(numeric_year))
    }
  }

  if (inherits(values, "Date")) {
    return(as.integer(lubridate::year(values)))
  }

  cli::cli_abort("{.val {column_name}} must be convertible to numeric year.")
}

#' resolve admin levels and facet fallback
#'
#' @param data data frame
#' @param admin_level candidate admin columns
#' @param record_id record id column name
#' @noRd
.resolve_admin_level <- function(data, admin_level, record_id) {
  if (is.null(admin_level) || length(admin_level) == 0) {
    cli::cli_abort("{.arg admin_level} must supply at least one column name.")
  }

  missing_admin <- admin_level[!admin_level %in% names(data)]
  if (length(missing_admin) == length(admin_level)) {
    cli::cli_abort(
      "None of the supplied admin columns {.val {admin_level}} exist."
    )
  }

  available <- admin_level[admin_level %in% names(data)]
  if (length(available) == 0) {
    cli::cli_abort("No valid admin columns provided.")
  }

  facet_column <- if (length(available) >= 2) {
    available[2]
  } else {
    available[1]
  }

  list(
    admin_level = available,
    facet_column = facet_column,
    grouping_cols = available,
    id_cols = unique(c(record_id, available))
  )
}

#' resolve strictness multipliers
#'
#' @param strictness strictness label
#' @param sd_multiplier numeric multiplier (advanced)
#' @param mad_constant MAD constant
#' @param mad_multiplier MAD multiplier
#' @param iqr_multiplier IQR multiplier
#' @noRd
.resolve_strictness <- function(
    strictness,
    sd_multiplier,
    mad_constant,
    mad_multiplier,
    iqr_multiplier) {
  if (strictness == "advanced") {
    return(list(
      strictness = strictness,
      sd_multiplier = sd_multiplier,
      mad_constant = mad_constant,
      mad_multiplier = mad_multiplier,
      iqr_multiplier = iqr_multiplier,
      strictness_shifted = FALSE
    ))
  }

  presets <- list(
    lenient = list(sd = 4, mad_const = 1.4826, mad_mult = 12, iqr = 3),
    balanced = list(sd = 3, mad_const = 1.4826, mad_mult = 9, iqr = 2),
    strict = list(sd = 2.5, mad_const = 1.4826, mad_mult = 6, iqr = 1.5)
  )

  preset <- presets[[strictness]]
  list(
    strictness = strictness,
    sd_multiplier = preset$sd,
    mad_constant = preset$mad_const,
    mad_multiplier = preset$mad_mult,
    iqr_multiplier = preset$iqr,
    strictness_shifted = FALSE
  )
}

#' build grouping keys based on time_mode
#'
#' @param data data frame
#' @param admin_level admin columns
#' @param time_mode time mode string
#' @param year year column name
#' @param month month column name
#' @noRd
.build_group_keys <- function(data, admin_level, time_mode, year, month) {
  base_cols <- admin_level
  time_cols <- switch(
    time_mode,
    within_year = c(year),
    across_time = character(),
    by_month = c(month),
    character()
  )
  grouping <- unique(c(base_cols, time_cols))
  missing_group <- grouping[!grouping %in% names(data)]
  if (length(missing_group) > 0) {
    cli::cli_abort(
      "Grouping columns missing: {.val {missing_group}}"
    )
  }
  grouping
}

#' guardrail reason helper
#'
#' @param n number of observations in bucket
#' @param reporting reporting rate value
#' @param min_n minimum observations threshold
#' @param reporting_threshold minimum reporting rate
#' @noRd
.guardrail_reason <- function(n, reporting, min_n, reporting_threshold) {
  n_vec <- rep(min_n, length.out = length(n))
  reporting_vec <- rep(reporting_threshold, length.out = length(n))
  reporting_flag <- !is.na(reporting_vec) & !is.na(reporting) &
    reporting < reporting_vec
  insufficient_flag <- !is.na(n_vec) & n < n_vec

  dplyr::case_when(
    reporting_flag ~ "low_reporting",
    insufficient_flag ~ "insufficient_n",
    TRUE ~ NA_character_
  )
}

#' floor lower bound for counts only
#'
#' @param lower numeric vector
#' @param value_type value type string
#' @noRd
.apply_lower_bound_floor <- function(lower, value_type) {
  if (value_type == "count") {
    return(as.numeric(pmax(0, lower)))
  }
  as.numeric(lower)
}

#' run non-seasonal detection path
#'
#' @param data prepared data with derived columns
#' @param grouping_cols character vector of grouping columns
#' @param strictness list with multipliers
#' @param min_n minimum observations
#' @param reporting_rate_min minimum reporting rate
#' @param methods character vector of methods to calculate
#' @param consensus_rule consensus rule
#' @param value_type value type string
#' @param metadata list containing mode labels
#' @noRd
.run_non_seasonal_detection <- function(
    data,
    grouping_cols,
    strictness,
    min_n,
    reporting_rate_min,
    methods,
    consensus_rule,
    value_type,
    metadata) {
  available <- dplyr::filter(data, !is.na(.outlier_value))
  if (nrow(available) == 0) {
    cli::cli_abort("No non-missing values found for detection.")
  }

  stats_by_group <- available |>
    dplyr::group_by(dplyr::across(dplyr::all_of(grouping_cols))) |>
    dplyr::summarise(
      # compute all statistics directly without list columns
      n_in_group = base::length(.outlier_value[!is.na(.outlier_value)]),
      mean = base::mean(.outlier_value, na.rm = TRUE),
      sd = stats::sd(.outlier_value, na.rm = TRUE),
      median = stats::median(.outlier_value, na.rm = TRUE),
      mad = stats::mad(
        .outlier_value,
        constant = strictness$mad_constant,
        na.rm = TRUE
      ),
      q1 = stats::quantile(
        .outlier_value,
        probs = 0.25,
        na.rm = TRUE,
        names = FALSE
      ),
      q3 = stats::quantile(
        .outlier_value,
        probs = 0.75,
        na.rm = TRUE,
        names = FALSE
      ),
      iqr = q3 - q1,
      .groups = "drop"
    ) |>
    dplyr::mutate(
      # compute bounds directly
      mean_lower = .apply_lower_bound_floor(
        mean - strictness$sd_multiplier * sd,
        value_type
      ),
      mean_upper = mean + strictness$sd_multiplier * sd,
      median_lower = .apply_lower_bound_floor(
        median - strictness$mad_multiplier * mad,
        value_type
      ),
      median_upper = median + strictness$mad_multiplier * mad,
      iqr_lower = .apply_lower_bound_floor(
        q1 - strictness$iqr_multiplier * iqr,
        value_type
      ),
      iqr_upper = q3 + strictness$iqr_multiplier * iqr,
      # check for unstable scales
      unstable_mean = is.na(sd) | sd == 0,
      unstable_median = is.na(mad) | mad == 0,
      unstable_iqr = is.na(iqr) | iqr == 0,
      n_in_group = as.integer(n_in_group)
    )

  combined <- dplyr::left_join(
    data,
    stats_by_group,
    by = grouping_cols
  ) |>
    dplyr::mutate(
      n_in_group = dplyr::coalesce(n_in_group, 0L),
      reason = .guardrail_reason(
        n_in_group,
        .outlier_reporting,
        min_n,
        reporting_rate_min
      )
    )

  # calculate only requested method flags
  if ("mean" %in% methods) {
    combined <- combined |>
      dplyr::mutate(
        mean_flag = dplyr::case_when(
          !is.na(reason) & reason != "" ~ reason,
          unstable_mean ~ "unstable_scale",
          is.na(.outlier_value) |
            is.na(mean_lower) |
            is.na(mean_upper) ~ "insufficient_evidence",
          .outlier_value > mean_upper ~ "outlier",
          TRUE ~ "normal"
        )
      )
  }

  if ("median" %in% methods) {
    combined <- combined |>
      dplyr::mutate(
        median_flag = dplyr::case_when(
          !is.na(reason) & reason != "" ~ reason,
          unstable_median ~ "unstable_scale",
          is.na(.outlier_value) |
            is.na(median_lower) |
            is.na(median_upper) ~ "insufficient_evidence",
          .outlier_value > median_upper ~ "outlier",
          TRUE ~ "normal"
        )
      )
  }

  if ("iqr" %in% methods) {
    combined <- combined |>
      dplyr::mutate(
        iqr_flag = dplyr::case_when(
          !is.na(reason) & reason != "" ~ reason,
          unstable_iqr ~ "unstable_scale",
          is.na(.outlier_value) |
            is.na(iqr_lower) |
            is.na(iqr_upper) ~ "insufficient_evidence",
          .outlier_value > iqr_upper ~ "outlier",
          TRUE ~ "normal"
        )
      )
  }

  # calculate consensus if requested
  if ("consensus" %in% methods) {
    combined <- combined |>
      dplyr::mutate(
        # vectorized consensus calculation
        outlier_flag_consensus = .vectorized_consensus_flag(
          if ("mean" %in% methods) mean_flag else NULL,
          if ("median" %in% methods) median_flag else NULL,
          if ("iqr" %in% methods) iqr_flag else NULL,
          consensus_rule
        ),
        reason = {
          unstable_check <- rep(TRUE, dplyr::n())
          if ("mean" %in% methods) {
            unstable_check <- unstable_check &
              (mean_flag == "unstable_scale")
          }
          if ("median" %in% methods) {
            unstable_check <- unstable_check &
              (median_flag == "unstable_scale")
          }
          if ("iqr" %in% methods) {
            unstable_check <- unstable_check &
              (iqr_flag == "unstable_scale")
          }
          dplyr::case_when(
            !is.na(reason) ~ reason,
            unstable_check ~ "unstable_scale",
            TRUE ~ NA_character_
          )
        }
      )
  }

  # add metadata
  combined <- combined |>
    dplyr::mutate(
      seasonality_mode_requested = metadata$seasonality_mode_requested,
      seasonality_mode_used = metadata$seasonality_mode_used,
      seasonal_window_desc = metadata$seasonal_window_desc,
      fallback_applied = FALSE,
      fallback_reason = NA_character_,
      strictness_label = strictness$strictness,
      strictness_shifted = strictness$strictness_shifted,
      sd_multiplier = strictness$sd_multiplier,
      mad_constant = strictness$mad_constant,
      mad_multiplier = strictness$mad_multiplier,
      iqr_multiplier = strictness$iqr_multiplier
    )

  combined
}

#' parse date column into year, month, and yearmon components
#'
#' @param date_values vector of date values
#' @param column_name name of the source column for error messages
#' @noRd
.parse_date_column <- function(date_values, column_name) {
  if (inherits(date_values, "Date")) {
    dates <- date_values
  } else if (inherits(date_values, "POSIXt")) {
    dates <- as.Date(date_values)
  } else if (is.character(date_values)) {
    # try common date formats
    dates <- suppressWarnings({
      ymd_result <- lubridate::ymd(date_values)
      if (!all(is.na(ymd_result))) {
        ymd_result
      } else {
        dmy_result <- lubridate::dmy(date_values)
        if (!all(is.na(dmy_result))) {
          dmy_result
        } else {
          mdy_result <- lubridate::mdy(date_values)
          if (!all(is.na(mdy_result))) {
            mdy_result
          } else {
            lubridate::parse_date_time(
              date_values,
              orders = c("Y-m-d", "d/m/Y", "m/d/Y", "Y/m/d")
            )
          }
        }
      }
    })
    dates <- as.Date(dates)
  } else {
    cli::cli_abort(
      "Date column {.val {column_name}} must be Date/POSIXt/character."
    )
  }

  if (any(is.na(dates))) {
    na_count <- sum(is.na(dates))
    cli::cli_abort(
      "Failed to parse {na_count} date value{?s} in column ",
      "{.val {column_name}}."
    )
  }

  year_vals <- lubridate::year(dates)
  month_vals <- lubridate::month(dates)
  yearmon_vals <- format(dates, "%Y-%m")

  list(
    year = as.integer(year_vals),
    month = as.integer(month_vals),
    yearmon = yearmon_vals
  )
}

#' filter detection results based on output profile
#'
#' @param data detection results tibble
#' @param output_profile profile string (lean/standard/audit)
#' @param record_id record id column name
#' @param admin_level admin column names
#' @param yearmon yearmon column name
#' @param year year column name
#' @noRd
.filter_by_output_profile <- function(
    data,
    output_profile,
    record_id,
    admin_level,
    yearmon,
    year) {
  # core columns always included
  core_cols <- c(record_id, admin_level, yearmon, year, "month")

  if (output_profile[1] == "lean") {
    # minimal: record id, admin levels, yearmon, derived year/month,
    # column name, value, value type, consensus flag, reason,
    # activeness metadata
    lean_cols <- c(
      core_cols,
      "column_name", "value", "value_type", "outlier_flag_consensus", "reason",
      "activeness_applied", "key_indicators_used"
    )
    data |> dplyr::select(dplyr::all_of(lean_cols[lean_cols %in% names(data)]))

  } else if (output_profile[1] == "standard") {
    # lean + method flags (mean/median/iqr), bounds (*_lower, *_upper),
    # n, seasonality mode, window description, and activeness metadata
    standard_cols <- c(
      core_cols,
      "column_name", "value", "value_type", "outlier_flag_consensus", "reason",
      "outlier_flag_mean", "outlier_flag_median", "outlier_flag_iqr",
      "mean_lower", "mean_upper", "median_lower", "median_upper",
      "iqr_lower", "iqr_upper", "n_in_group",
      "seasonality_mode_used", "seasonal_window_desc",
      "activeness_applied", "key_indicators_used"
    )
    data |>
      dplyr::select(
        dplyr::all_of(standard_cols[standard_cols %in% names(data)])
      )

  } else {
    # audit: full table including scale stats, multipliers,
    # strictness label/shift, ladder metadata, activeness metadata
    data
  }
}

#' core outlier detection logic for caching
#'
#' @param ... all parameters from detect_outliers
#' @return detection results tibble
#' @keywords internal
#' @noRd
.core_outlier_detection <- function(
    data,
    column,
    record_id,
    admin_level,
    date,
    time_mode,
    value_type,
    strictness,
    sd_multiplier,
    mad_constant,
    mad_multiplier,
    iqr_multiplier,
    min_n,
    reporting_rate_col,
    reporting_rate_min,
    key_indicators_hf,
    methods,
    consensus_rule,
    output_profile,
    seasonal_pool_years,
    min_years_per_month,
    verbose,
    spatial_level,
    activeness_applied,
    n_inactive_skipped,
    key_indicators_used,
    classify_outbreaks = TRUE,
    outbreak_min_run = 2,
    outbreak_prop_tolerance = 0.9,
    outbreak_max_gap = 1) {

  # detection orchestrator ---------------------------------------------------

  options <- .normalize_outlier_options(
    time_mode = time_mode,
    value_type = value_type,
    strictness = strictness,
    methods = methods,
    output_profile = output_profile
  )
  methods <- options$methods

  # Handle spatial_level parameter for proper detection vs grouping
  if (!is.null(spatial_level)) {
    # Detection happens at spatial_level, grouping for efficiency at admin_level
    detection_admin_level <- c(admin_level, spatial_level)
    parallel_grouping_levels <- admin_level  # For data.table parallel efficiency
  } else {
    # Original behavior - use all admin_level for both detection and grouping
    detection_admin_level <- admin_level
    parallel_grouping_levels <- admin_level
  }

  context <- .prepare_outlier_context(
    data = data,
    column = column,
    record_id = record_id,
    admin_level = admin_level,  # Pass original admin_level, not detection_admin_level
    date_column = date,
    options = options,
    consensus_rule = consensus_rule,
    sd_multiplier = sd_multiplier,
    mad_constant = mad_constant,
    mad_multiplier = mad_multiplier,
    iqr_multiplier = iqr_multiplier,
    min_n = min_n,
    reporting_rate_col = reporting_rate_col,
    reporting_rate_min = reporting_rate_min,
    spatial_level = spatial_level
  )

  # Use execute_outlier_detection to handle data.table decision
  detection <- .execute_outlier_detection(
    context = context,
    methods = methods,
    consensus_rule = consensus_rule
  )

  result <- .finalize_outlier_detection(
    detection = detection,
    context = context,
    methods = methods,
    record_id = record_id,
    output_profile = output_profile,
    column = column,
    verbose = verbose,
    consensus_rule = consensus_rule,
    activeness_applied = activeness_applied,
    n_inactive_skipped = n_inactive_skipped,
    key_indicators_used = key_indicators_used,
    classify_outbreaks = classify_outbreaks,
    outbreak_min_run = outbreak_min_run,
    outbreak_prop_tolerance = outbreak_prop_tolerance,
    outbreak_max_gap = outbreak_max_gap
  )

  .filter_by_output_profile(
    result,
    output_profile,
    record_id,
    context$admin_info$admin_level,
    context$yearmon_col,
    context$year_col
  )
}

#' vectorized consensus flag calculation
#'
#' @param mean_flag vector of mean method flags
#'   (can be NULL if not calculated)
#' @param median_flag vector of median method flags
#'   (can be NULL if not calculated)
#' @param iqr_flag vector of iqr method flags
#'   (can be NULL if not calculated)
#' @param consensus_rule integer rule for consensus
#' @noRd
.vectorized_consensus_flag <- function(
    mean_flag,
    median_flag,
    iqr_flag,
    consensus_rule) {
  # define guard reasons that override consensus
  guard_reasons <- c(
    "low_reporting",
    "insufficient_n",
    "insufficient_seasonal_history"
  )

  # check if any method has a guard reason (vectorized)
  has_guard <- FALSE
  result_length <- if (!is.null(mean_flag)) {
    length(mean_flag)
  } else if (!is.null(median_flag)) {
    length(median_flag)
  } else {
    length(iqr_flag)
  }
  guard_result <- rep(NA_character_, result_length)

  if (!is.null(mean_flag)) {
    has_guard <- has_guard | (mean_flag %in% guard_reasons)
    guard_result <- ifelse(
      mean_flag %in% guard_reasons,
      mean_flag,
      guard_result
    )
  }
  if (!is.null(median_flag)) {
    has_guard <- has_guard | (median_flag %in% guard_reasons)
    guard_result <- ifelse(
      is.na(guard_result) & median_flag %in% guard_reasons,
      median_flag,
      guard_result
    )
  }
  if (!is.null(iqr_flag)) {
    has_guard <- has_guard | (iqr_flag %in% guard_reasons)
    guard_result <- ifelse(
      is.na(guard_result) & iqr_flag %in% guard_reasons,
      iqr_flag,
      guard_result
    )
  }

  # for non-guard rows, count outlier votes among usable methods
  usable_methods <- 0
  outlier_votes <- 0

  if (!is.null(mean_flag)) {
    usable_methods <- usable_methods + (
      !mean_flag %in% c("unstable_scale", guard_reasons)
    )
    outlier_votes <- outlier_votes + (
      mean_flag == "outlier" &
        !mean_flag %in% c("unstable_scale", guard_reasons)
    )
  }
  if (!is.null(median_flag)) {
    usable_methods <- usable_methods + (
      !median_flag %in% c("unstable_scale", guard_reasons)
    )
    outlier_votes <- outlier_votes + (
      median_flag == "outlier" &
        !median_flag %in% c("unstable_scale", guard_reasons)
    )
  }
  if (!is.null(iqr_flag)) {
    usable_methods <- usable_methods + (
      !iqr_flag %in% c("unstable_scale", guard_reasons)
    )
    outlier_votes <- outlier_votes + (
      iqr_flag == "outlier" &
        !iqr_flag %in% c("unstable_scale", guard_reasons)
    )
  }

  # final consensus decision
  dplyr::case_when(
    has_guard ~ guard_result,
    usable_methods == 0 ~ "insufficient_evidence",
    outlier_votes >= consensus_rule ~ "outlier",
    TRUE ~ "normal"
  )
}

# Calculate consensus strength across methods for consensus color visualization
# @param data tibble with outlier_flag_* columns
# @param methods character vector of detection methods used
# @return ordered factor with levels: normal, weak, moderate, strong
# @noRd
.calculate_consensus_strength <- function(outlier_flag_iqr = NA_character_,
                                          outlier_flag_median = NA_character_,
                                          outlier_flag_mean = NA_character_,
                                          methods) {
  # extract only non-consensus methods
  detection_methods <- setdiff(methods, "consensus")

  # count outlier + outbreak flags across methods (vectorized)
  count_col <- 0L

  if ("iqr" %in% detection_methods && !all(is.na(outlier_flag_iqr))) {
    count_col <- count_col +
      as.integer(outlier_flag_iqr %in% c("outlier", "outbreak"))
  }
  if ("median" %in% detection_methods && !all(is.na(outlier_flag_median))) {
    count_col <- count_col +
      as.integer(outlier_flag_median %in% c("outlier", "outbreak"))
  }
  if ("mean" %in% detection_methods && !all(is.na(outlier_flag_mean))) {
    count_col <- count_col +
      as.integer(outlier_flag_mean %in% c("outlier", "outbreak"))
  }

  # map count to strength factor
  strength <- dplyr::case_when(
    count_col == 0L ~ "normal",
    count_col == 1L ~ "weak",
    count_col == 2L ~ "moderate",
    count_col >= 3L ~ "strong",
    TRUE ~ "normal"
  )

  # return as ordered factor for proper legend ordering
  factor(
    strength,
    levels = c("normal", "weak", "moderate", "strong"),
    ordered = TRUE
  )
}
