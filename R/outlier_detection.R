# sntutils/R/outlier_detection.R
# orchestrates outlier detection and reporting utilities.
# anchors the public API for identifying anomalous malaria indicators.
# RELEVANT FILES:R/write_snt_data.R,R/process_rasters.R
#' Detect outliers with guardrails, consensus, and seasonal fallbacks
#'
#' @description
#' `detect_outliers()` evaluates a numeric indicator by administrative unit and
#' time using three methods (mean +/- SD, median +/- MAD, Tukey IQR).
#' It enforces reporting and sample-size guardrails, supports strictness
#' presets, and when requested applies a seasonality ladder
#' (same-month -> neighbours -> residual -> across-time) to select stable
#' comparison pools. It returns per-method flags (optional), a consensus
#' classification, scale statistics, and metadata recording any fallback used.
#'
#' @param data Data frame containing the indicator to analyse.
#' @param column Name of the numeric column to evaluate.
#' @param record_id Unique record identifier column.
#' @param admin_level Character vector of administrative level columns for
#'   parallel grouping, ordered from higher to lower resolution.
#'   Defaults to `c("adm1", "adm2")`. Used for efficient data.table grouping.
#' @param date Date column (Date, POSIXt, or parseable character string).
#'   Year, month, and yearmon are automatically derived from this column.
#' @param time_mode Pooling strategy: `"across_time"`, `"within_year"`, or
#'   `"by_month"`. `"by_month"` activates the seasonal fallback ladder.
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
#'   determine facility activeness. If supplied, the function calls
#'   `classify_facility_activity()` internally to exclude inactive health
#'   facilities from outlier detection. Inactive facility-months are tagged with
#'   `reason = "inactive_facility"` and excluded from detection. If `NULL`
#'   (default), activeness filtering is skipped. Typical indicators include
#'   `"allout"`, `"test"`, or `"conf"`. This adjustment prevents false positives
#'   caused by facilities that start or stop reporting mid-period.
#'
#' @param binary_classification Logical. When `key_indicators_hf` is provided,
#'   determines the classification method passed to `classify_facility_activity()`.
#'   If TRUE, uses binary classification ("Active", "Non-Active"). If FALSE
#'   (default), uses three-level classification ("Active Reporting",
#'   "Active Facility - Not Reporting", "Inactive Facility").
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
#' @param seasonal_pool_years Rolling history window (in years) for same-month
#'   pooling when `time_mode = "by_month"`. Default `5`.
#' @param min_years_per_month Minimum distinct years required in the same-month
#'   pool before moving to a fallback. Default `3`.
#'
#' @param verbose Logical. When `TRUE`, prints an informative summary showing
#'   which methods are being applied, the pooling strategy, strictness settings,
#'   guardrails, and consensus rule. Default is `FALSE`.
#' @param cache_stats Logical. Cache intermediate statistical calculations
#'   to improve performance across seasonal pools. Default is `TRUE`.
#' @param cache_dir Character string specifying directory for file-based cache.
#'   If NULL (default), uses project cache directory or tempdir(). Cache persists
#'   across R sessions for reproducible performance gains.
#' @param spatial_level Character string specifying the finest spatial unit
#'   for analysis (e.g., "hf_uid" for facility-level). When specified,
#'   `admin_levels` defines grouping boundaries while `spatial_level` defines
#'   the unit of analysis. This prevents excessive grouping while maintaining
#'   spatial granularity. Default is `NULL` (uses most granular admin level).
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
#'    `insufficient_seasonal_history`).
#' 5) Seasonality (if `time_mode = "by_month"`): ladder applied in order:
#'    same-month (rolling `seasonal_pool_years`), neighbours (m +/- 1 with
#'    weights 2:1), residual (remove month fixed effects), across-time (with
#'    strictness shift), then exclusion if still inadequate. Each row records
#'    `seasonality_mode_used`, window text, and any fallback reason.
#' 6) Flagging: each method classifies rows respecting guardrails and unstable
#'    scales (when `sd`, `mad`, or `iqr` equals zero the method is suppressed
#'    and marked `unstable_scale`).
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
#' `detect_outliers()` can optionally apply activeness filtering using
#' `classify_facility_activity()`. Only active facilities (those that reported
#' at least one of the specified key indicators in a given month) contribute to
#' the comparison pool for outlier detection. This step improves
#' interpretability of results and reduces false positives in areas with
#' variable reporting completeness or frequent facility additions.
#'
#' **Presets**
#' - lenient: SD 4.0; MAD constant 1.4826, MAD mult 12; IQR 3.0
#' - balanced: SD 3.0; MAD constant 1.4826, MAD mult 9; IQR 2.0
#' - strict: SD 2.5; MAD constant 1.4826, MAD mult 6; IQR 1.5
#' - advanced: use user-supplied multipliers
#'
#' **Seasonality ladder thresholds**
#' - same-month requires `min_years_per_month` and `min_n` within the
#'   `seasonal_pool_years` window; otherwise fall back as above.
#'
#' The returned tibble always contains identifiers, scale statistics, bounds,
#' strictness and seasonality metadata, and the guardrail reason. When
#' `output_profile = "standard"` or `"audit"`, method-specific flags are
#' included alongside the consensus.
#'
#' @return Tibble with outlier classifications and metadata. Columns include:
#'   identifiers (`record_id`, admin levels, `yearmon`, `year`, derived
#'   `month`), `column_name`, `value`, `value_type`, scale stats (`mean`,
#'   `sd`, `median`, `mad`, `q1`, `q3`, `iqr`), method bounds, residual stats
#'   (if residual fallback), `n_in_group`, guardrail `reason`, method flags
#'   (optional), `outlier_flag_consensus`, seasonality descriptors, strictness
#'   multipliers, fallback information, and (if activeness filtering was
#'   applied) `activeness_applied` and `key_indicators_used`.
#'
#' @examples
#' \dontrun{
#' # 1) Minimal consensus output at adm1-only level
#' detect_outliers(
#'   data = malaria_data,
#'   column = "confirmed_cases",
#'   date = "date",
#'   record_id = "facility_id",
#'   admin_levels = c("adm1"),        # ignore adm2 if not present
#'   time_mode = "across_time"
#' )
#'
#' # 2) Seasonality with fallbacks
#' #    (same-month -> neighbours -> residual -> across-time)
#' detect_outliers(
#'   data = malaria_data,
#'   column = "confirmed_cases",
#'   date = "date",
#'   admin_levels = c("adm1"),
#'   time_mode = "by_month",
#'   seasonal_pool_years = 5,
#'   min_years_per_month = 3,
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
#'   admin_levels = c("adm1", "adm2"),
#'   time_mode = "by_month",
#'   key_indicators_hf = c("allout", "test", "conf")
#' )
#'
#' # 5) With binary activeness classification
#' detect_outliers(
#'   data = malaria_data,
#'   column = "conf",
#'   date = "date",
#'   admin_levels = c("adm1", "adm2"),
#'   time_mode = "by_month",
#'   key_indicators_hf = c("allout", "test", "conf"),
#'   binary_classification = TRUE
#' )
#' }
#' @export
detect_outliers <- function(
    data,
    column,
    record_id = "record_id",
    admin_level = c("adm1", "adm2"),
    date = "date",
    time_mode = c("by_month", "across_time", "within_year"),
    value_type = c("count", "rate"),
    strictness = c("balanced", "lenient", "strict", "advanced"),
    sd_multiplier = 3,
    mad_constant = 1.4826,
    mad_multiplier = 9,
    iqr_multiplier = 2,
    min_n = 8,
    reporting_rate_col = NULL,
    reporting_rate_min = 0.5,
    key_indicators_hf = NULL,
    binary_classification = FALSE,
    methods = c("iqr", "median", "mean", "consensus"),
    consensus_rule = 2,
    output_profile = c("standard", "lean", "audit"),
    seasonal_pool_years = 5,
    min_years_per_month = 3,
    verbose = TRUE,
    cache_stats = TRUE,
    cache_dir = NULL,
    spatial_level = NULL) {
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
    if (!record_id %in% names(data)) {
      cli::cli_abort(
        "record_id {.val {record_id}} not found in data."
      )
    }
    activeness_data <- classify_facility_activity(
      data = data,
      hf_col = record_id,
      date_col = date,
      key_indicators = key_indicators_hf,
      binary_classification = binary_classification
    )
    data <- data |>
      dplyr::left_join(
        activeness_data |>
          dplyr::select(
            dplyr::all_of(c(record_id, date)),
            activity_status
          ),
        by = c(record_id, date)
      ) |>
      dplyr::mutate(
        .is_active = activity_status == "Active Reporting"
      )
    n_inactive_skipped <- sum(!data$.is_active, na.rm = TRUE)
    data <- data |>
      dplyr::filter(.is_active) |>
      dplyr::select(-.is_active, -activity_status)
    activeness_applied <- TRUE
    key_indicators_used <- paste(key_indicators_hf, collapse = ", ")
  }

  # setup caching if enabled - cache the entire detection process
  if (cache_stats && !is.null(cache_dir)) {
    cache_config <- .setup_outlier_cache(cache_stats = cache_stats, cache_dir = cache_dir)

    # Create a cached version of the core detection logic
    if (!is.null(cache_config) && cache_config$enabled) {
      cached_detection <- memoise::memoise(.core_outlier_detection, cache = cache_config$backend)

      # Call the cached detection with all parameters as cache key
      return(cached_detection(
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
        seasonal_pool_years = seasonal_pool_years,
        min_years_per_month = min_years_per_month,
        verbose = verbose,
        spatial_level = spatial_level,
        activeness_applied = activeness_applied,
        n_inactive_skipped = n_inactive_skipped,
        key_indicators_used = key_indicators_used
      ))
    }
  }

  # fallback to non-cached execution
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
    seasonal_pool_years = seasonal_pool_years,
    min_years_per_month = min_years_per_month,
    verbose = verbose,
    spatial_level = spatial_level,
    activeness_applied = activeness_applied,
    n_inactive_skipped = n_inactive_skipped,
    key_indicators_used = key_indicators_used
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
    c("by_month", "across_time", "within_year")
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
#' @param admin_levels admin hierarchy columns
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
#' @param seasonal_pool_years seasonal history window
#' @param min_years_per_month minimum seasonal years per month
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
    seasonal_pool_years,
    min_years_per_month,
    cache_stats = TRUE,
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
  if (!is.null(spatial_level)) {
    # Detection happens at spatial_level, grouping for efficiency at admin_level
    detection_admin_levels <- c(admin_level, spatial_level)
    parallel_grouping_levels <- admin_level  # For data.table parallel efficiency
  } else {
    # Original behavior - use all admin_level for both detection and grouping
    detection_admin_levels <- admin_level
    parallel_grouping_levels <- admin_level
  }

  admin_info <- .resolve_admin_levels(
    prepared_data,
    detection_admin_levels,
    record_id
  )

  # Store both levels for different purposes
  admin_info$detection_levels <- detection_admin_levels      # What we detect outliers on
  admin_info$parallel_grouping_levels <- parallel_grouping_levels  # How we group for efficiency

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

  seasonality_params <- list(
    seasonal_pool_years = seasonal_pool_years,
    min_years_per_month = min_years_per_month,
    min_obs_per_seasonal_bucket = min_n,
    mad_constant = strictness_info$mad_constant
  )

  list(
    prepared_data = prepared_data,
    admin_info = admin_info,
    strictness = strictness_info,
    grouping_cols = grouping_cols,
    seasonality_params = seasonality_params,
    options = options,
    constraints = list(
      min_n = min_n,
      reporting_rate_min = reporting_rate_min
    ),
    cache_stats = cache_stats,
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

  if (time_mode == "by_month") {
    return(
      .run_seasonal_detection(
        data = context$prepared_data,
        detection_admin_levels = context$admin_info$detection_levels,
        parallel_grouping_levels = context$admin_info$parallel_grouping_levels,
        strictness = context$strictness,
        params = context$seasonality_params,
        methods = methods,
        consensus_rule = consensus_rule,
        min_n = context$constraints$min_n,
        reporting_rate_min = context$constraints$reporting_rate_min,
        value_type = context$options$value_type
      )
    )
  }

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
    key_indicators_used = NA_character_) {
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
      admin_level_used = paste(admin_info$admin_levels, collapse = ", "),
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

  if (isTRUE(verbose)) {
    .display_outlier_summary_box(
      detection = detection,
      methods = methods,
      column = column,
      time_mode = time_mode,
      admin_levels = admin_info$admin_levels,
      strictness = strictness_label,
      strictness_info = strictness_info,
      min_n = min_n,
      reporting_rate_min = reporting_rate_min,
      consensus_rule = consensus_rule,
      activeness_applied = activeness_applied,
      n_inactive_skipped = n_inactive_skipped,
      key_indicators_used = key_indicators_used
    )
  }

  select_cols <- c(
    admin_info$admin_levels,
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
      admin_info$admin_levels,
      "yearmon",
      "year"
    )

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
#' @param admin_levels character vector of admin level columns
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
  admin_levels,
  strictness_info,
  strictness,
  min_n,
  reporting_rate_min,
  methods,
  consensus_rule,
  activeness_applied = FALSE,
  n_inactive_skipped = 0L,
  key_indicators_used = NA_character_
) {
  time_desc <- switch(
    time_mode,
    "by_month" = "months",
    "within_year" = "within each year",
    "across_time" = "across all time",
    "unknown"
  )
  level_used <- tail(admin_levels, 1)

  fmt_num <- function(x) {
    cli::col_blue(cli::style_bold(formatC(x, big.mark = ",", digits = 0)))
  }
  fmt_pct <- function(x) {
    cli::col_blue(cli::style_bold(formatC(x * 100, format = "f", digits = 0)))
  }

  # guardrail diagnostics
  n_skipped_n <- detection |>
    dplyr::filter(reason == "insufficient_n") |>
    dplyr::distinct(
      dplyr::across(all_of(admin_levels)),
      .data$.outlier_yearmon
    ) |>
    nrow()
  n_skipped_rep <- detection |>
    dplyr::filter(reason == "low_reporting") |>
    dplyr::distinct(
      dplyr::across(all_of(admin_levels)),
      .data$.outlier_yearmon
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

  if (time_mode == "by_month") {
    lines <- c(
      lines,
      glue::glue(
        "{cli::col_magenta(cli::style_bold('Seasonality ladder'))}: ",
        "same-month -> neighbours -> residual -> across-time"
      )
    )
  }

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
      "{fmt_num(out_n)}/{fmt_num(eligible_n)} outliers among eligible ",
      "(total = {fmt_num(total_n)})"
    )
  }

  method_lines <- unlist(lapply(methods, .append_method_line))
  lines <- c(lines, method_lines)

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

#' Create Outlier Detection Plots
#'
#' This function creates plots to visualize outliers in data using the same
#' detection methods as `detect_outliers()`. The plotting function inherits
#' all parameters from the detection function for seamless integration.
#'
#' @inheritParams detect_outliers
#' @param cache_stats Logical. If TRUE, caches computed statistics to improve
#'   performance on repeated calls with the same data. Default is TRUE.
#' @param cache_dir Character string specifying directory for file-based cache.
#'   If NULL (default), uses project cache directory or tempdir(). Cache persists
#'   across R sessions for reproducible performance gains.
#' @param methods Character vector specifying which outlier detection methods
#'   to plot: "iqr" (Interquartile Range), "median" (Median Absolute
#'   Deviation), "mean" (Mean +/- SD), and/or "consensus".
#'   Default is `c("iqr", "median", "mean", "consensus")`.
#'   For consensus, at least two other methods must be selected.
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
#' points colored by outlier status (red for outliers, grey for normal values).
#' For consensus plots, the caption shows the consensus rule used.
#' The plots are faceted by administrative levels and include summary
#' statistics in the subtitle.
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
#' }
#' @export
outlier_plot <- function(
    data,
    column,
    record_id = "record_id",
    spatial_level = NULL,
    admin_level = c("adm1", "adm2"),
    date = "date",
    time_mode = c("by_month", "across_time", "within_year"),
    value_type = c("count", "rate"),
    strictness = c("balanced", "lenient", "strict", "advanced"),
    sd_multiplier = 3,
    mad_constant = 1.4826,
    mad_multiplier = 9,
    iqr_multiplier = 2,
    min_n = 8,
    reporting_rate_col = NULL,
    reporting_rate_min = 0.5,
    key_indicators_hf = NULL,
    binary_classification = FALSE,
    consensus_rule = 2,
    seasonal_pool_years = 5,
    min_years_per_month = 3,
    methods = c("iqr", "median", "mean", "consensus"),
    year_breaks = 2,
    cache_stats = TRUE,
    cache_dir = NULL,
    verbose = TRUE,
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
    show_plot = TRUE
) {
  # match choice arguments
  time_mode <- match.arg(time_mode)
  value_type <- match.arg(value_type)
  strictness <- match.arg(strictness)

  # Validate methods selection
  valid_methods <- c("iqr", "median", "mean", "consensus")
  methods <- match.arg(methods, valid_methods, several.ok = TRUE)

  # Handle backward compatibility and new parameter structure
  # spatial_level replaces old admin_levels parameter for detection granularity
  # admin_level replaces old plot_admin_level parameter for plotting/grouping

  # Set up detection admin levels (for calling detect_outliers)
  if (is.null(spatial_level)) {
    # Default to finest granularity available
    detection_admin_levels <- admin_level
  } else {
    detection_admin_levels <- c(admin_level, spatial_level)
  }

  # Set up plotting admin levels (for aggregation/faceting)
  plot_admin_levels <- admin_level

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
      binary_classification = binary_classification,
      methods = methods,  # Pass methods to calculate only requested flags
      consensus_rule = consensus_rule,
      output_profile = "audit",  # Get all flags for individual methods
      seasonal_pool_years = seasonal_pool_years,
      min_years_per_month = min_years_per_month,
      cache_stats = cache_stats,
      cache_dir = cache_dir,
      verbose = FALSE  # plot helper prints summary itself
    )
  }

  # Show summary box once before creating plots
  if (isTRUE(verbose)) {
    .display_outlier_summary_box(
      detection = outlier_results,
      methods = methods,
      column = column,
      time_mode = time_mode,
      admin_levels = admin_level,
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

  # Create plots for each requested method
  for (method in methods) {
    # Determine which flag column to use
    flag_column <- switch(method,
      "iqr" = "outlier_flag_iqr",
      "median" = "outlier_flag_median",
      "mean" = "outlier_flag_mean",
      "consensus" = "outlier_flag_consensus"
    )

    # Filter data for this method and add binary flag
    plot_data <- plot_data_base |>
      dplyr::filter(
        !is.na(.data[[flag_column]]),
        .data[[flag_column]] != "insufficient_evidence"
      ) |>
      dplyr::mutate(
        # Convert to binary TRUE/FALSE for outlier status
        .binary_flag = dplyr::case_when(
          .data[[flag_column]] == "outlier" ~ "TRUE",
          TRUE ~ "FALSE"
        ),
        # Different alpha values: normal points more transparent
        .alpha_val = dplyr::case_when(
          .data[[flag_column]] == "outlier" ~ 0.9,  # Outliers more opaque
          TRUE ~ 0.5  # Normal points more transparent
        )
      ) |>
      # Sort so outliers (TRUE) are plotted on top of normal points (FALSE)
      dplyr::arrange(.binary_flag)


    # Create summary for facet labels
    percent_summary <- plot_data |>
      dplyr::group_by(dplyr::across(dplyr::all_of(facet_column))) |>
      dplyr::summarise(
        n_outlier = sum(.data[[flag_column]] == "outlier", na.rm = TRUE),
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
        label = glue::glue(
          "{facet_value}\n {pct_outlier}% ({n_outlier_fmt}/{n_total_fmt})"
        )
      )

    facet_labels <- setNames(
      percent_summary$label,
      percent_summary[[facet_column]]
    )

    outliers_n <- sntutils::big_mark(
      sum(plot_data[[flag_column]] == "outlier", na.rm = TRUE)
    )
    total_outliers <- sntutils::big_mark(nrow(plot_data))

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

      # Translate only the word "outliers" for subtitle
      outliers_word <- translate_text(
        "outliers",
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
      subtitle_text <- glue::glue(
        "{there_were} {outliers_n}/{total_outliers}",
        " <b style='color:red;font-weight:bold'>",
        "{outliers_word}</b> {detected_word}"
      )

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
      subtitle_text <- glue::glue(
        "There were {outliers_n}/{total_outliers}",
        " <b style='color:red;font-weight:bold'>outliers</b> detected"
      )
      x_label <- "Date"
      y_label <- "Value"
    }

    # Add caption for consensus explaining the rule
    plot_caption <- if (method == "consensus") {
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
      NULL
    }

    # Create the plot for this method
    p <- ggplot2::ggplot(plot_data) +
      ggplot2::geom_point(
        ggplot2::aes(
          x = date_for_plot,
          y = value,
          color = .binary_flag,
          alpha = .alpha_val
        ),
        size = 2
      ) +
      ggplot2::scale_color_manual(
        values = c("TRUE" = "red", "FALSE" = "grey")
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
        )
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
        )
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

    # Show plot if requested
    if (show_plot) {
      print(p)
    }
  }  # End of methods loop

  # Return single plot or list of plots
  if (length(methods) == 1) {
    if (!show_plot) {
      return(invisible(plot_list[[1]]))
    }
    return(plot_list[[1]])
  } else {
    if (!show_plot) {
      return(invisible(plot_list))
    }
    return(plot_list)
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
#' @param admin_levels admin columns vector
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
#' @param admin_levels candidate admin columns
#' @param record_id record id column name
#' @noRd
.resolve_admin_levels <- function(data, admin_levels, record_id) {
  if (is.null(admin_levels) || length(admin_levels) == 0) {
    cli::cli_abort("{.arg admin_levels} must supply at least one column name.")
  }

  missing_admin <- admin_levels[!admin_levels %in% names(data)]
  if (length(missing_admin) == length(admin_levels)) {
    cli::cli_abort(
      "None of the supplied admin columns {.val {admin_levels}} exist."
    )
  }

  available <- admin_levels[admin_levels %in% names(data)]
  if (length(available) == 0) {
    cli::cli_abort("No valid admin columns provided.")
  }

  facet_column <- if (length(available) >= 2) {
    available[2]
  } else {
    available[1]
  }

  list(
    admin_levels = available,
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

#' shift strictness toward lenient by one level
#'
#' @param strictness strictness label
#' @param sd_multiplier numeric multiplier
#' @param mad_constant numeric constant
#' @param mad_multiplier numeric multiplier
#' @param iqr_multiplier numeric multiplier
#' @noRd
.shift_strictness_one_level <- function(
    strictness,
    sd_multiplier,
    mad_constant,
    mad_multiplier,
    iqr_multiplier) {
  levels <- c("strict", "balanced", "lenient")
  if (strictness == "advanced") {
    # shift multipliers directly toward lenient defaults
    return(list(
      strictness = strictness,
      sd_multiplier = max(sd_multiplier, 4),
      mad_constant = mad_constant,
      mad_multiplier = max(mad_multiplier, 12),
      iqr_multiplier = max(iqr_multiplier, 3),
      strictness_shifted = TRUE
    ))
  }

  position <- match(strictness, levels, nomatch = length(levels))
  new_position <- min(position + 1, length(levels))
  new_strictness <- levels[[new_position]]
  resolved <- .resolve_strictness(
    new_strictness,
    sd_multiplier,
    mad_constant,
    mad_multiplier,
    iqr_multiplier
  )
  resolved$strictness_shifted <- TRUE
  resolved
}

#' build grouping keys based on time_mode
#'
#' @param data data frame
#' @param admin_levels admin columns
#' @param time_mode time mode string
#' @param year year column name
#' @param month month column name
#' @noRd
.build_group_keys <- function(data, admin_levels, time_mode, year, month) {
  base_cols <- admin_levels
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
    return(pmax(0, lower))
  }
  lower
}

#' compute method bounds for a numeric vector
#'
#' @param values numeric vector of pool values
#' @param strictness list with multipliers
#' @param value_type type of metric (count/rate)
#' @noRd
.calculate_method_bounds <- function(values, strictness, value_type) {
  cleaned <- values[!is.na(values)]
  n <- length(cleaned)
  if (n == 0) {
    return(list(
      n = 0,
      mean = NA_real_,
      sd = NA_real_,
      mean_lower = NA_real_,
      mean_upper = NA_real_,
      median = NA_real_,
      mad = NA_real_,
      median_lower = NA_real_,
      median_upper = NA_real_,
      q1 = NA_real_,
      q3 = NA_real_,
      iqr = NA_real_,
      iqr_lower = NA_real_,
      iqr_upper = NA_real_,
      unstable_mean = TRUE,
      unstable_median = TRUE,
      unstable_iqr = TRUE
    ))
  }

  mean_value <- mean(cleaned)
  sd_value <- stats::sd(cleaned)
  mean_lower <- mean_value - strictness$sd_multiplier * sd_value
  mean_upper <- mean_value + strictness$sd_multiplier * sd_value
  mean_lower <- .apply_lower_bound_floor(mean_lower, value_type)

  median_value <- stats::median(cleaned)
  mad_value <- stats::mad(
    cleaned,
    constant = strictness$mad_constant,
    na.rm = TRUE
  )
  median_lower <- median_value - strictness$mad_multiplier * mad_value
  median_upper <- median_value + strictness$mad_multiplier * mad_value
  median_lower <- .apply_lower_bound_floor(median_lower, value_type)

  quantiles <- stats::quantile(cleaned, probs = c(0.25, 0.75), names = FALSE)
  q1 <- quantiles[1]
  q3 <- quantiles[2]
  iqr_value <- q3 - q1
  iqr_lower <- q1 - strictness$iqr_multiplier * iqr_value
  iqr_upper <- q3 + strictness$iqr_multiplier * iqr_value
  iqr_lower <- .apply_lower_bound_floor(iqr_lower, value_type)

  list(
    n = n,
    mean = mean_value,
    sd = sd_value,
    mean_lower = mean_lower,
    mean_upper = mean_upper,
    median = median_value,
    mad = mad_value,
    median_lower = median_lower,
    median_upper = median_upper,
    q1 = q1,
    q3 = q3,
    iqr = iqr_value,
    iqr_lower = iqr_lower,
    iqr_upper = iqr_upper,
    unstable_mean = is.na(sd_value) || sd_value == 0,
    unstable_median = is.na(mad_value) || mad_value == 0,
    unstable_iqr = is.na(iqr_value) || iqr_value == 0
  )
}

#' apply method flags based on bounds and guardrails
#'
#' @param value numeric value for the row
#' @param bounds list from .calculate_method_bounds
#' @param guard_reason character reason if guardrail triggered
#' @param methods character vector of methods to calculate
#' @noRd
.flag_outliers_by_method <- function(value, bounds, guard_reason, methods) {
  result <- list()

  if (!is.na(guard_reason) && guard_reason != "") {
    if ("mean" %in% methods) result$mean_flag <- guard_reason
    if ("median" %in% methods) result$median_flag <- guard_reason
    if ("iqr" %in% methods) result$iqr_flag <- guard_reason
    return(result)
  }

  if ("mean" %in% methods) {
    result$mean_flag <- if (bounds$unstable_mean) {
      "unstable_scale"
    } else if (
        is.na(value) ||
        is.na(bounds$mean_lower) ||
        is.na(bounds$mean_upper)
      ) {
      "insufficient_evidence"
    } else if (value < bounds$mean_lower || value > bounds$mean_upper) {
      "outlier"
    } else {
      "normal"
    }
  }

  if ("median" %in% methods) {
    result$median_flag <- if (bounds$unstable_median) {
      "unstable_scale"
    } else if (
        is.na(value) ||
        is.na(bounds$median_lower) ||
        is.na(bounds$median_upper)
      ) {
      "insufficient_evidence"
    } else if (value < bounds$median_lower || value > bounds$median_upper) {
      "outlier"
    } else {
      "normal"
    }
  }

  if ("iqr" %in% methods) {
    result$iqr_flag <- if (bounds$unstable_iqr) {
      "unstable_scale"
    } else if (
        is.na(value) ||
        is.na(bounds$iqr_lower) ||
        is.na(bounds$iqr_upper)
      ) {
      "insufficient_evidence"
    } else if (value < bounds$iqr_lower || value > bounds$iqr_upper) {
      "outlier"
    } else {
      "normal"
    }
  }

  result
}

#' compute consensus flag given method flags and rule
#'
#' @param mean_flag mean method flag (can be NULL if not calculated)
#' @param median_flag median method flag (can be NULL if not calculated)
#' @param iqr_flag iqr method flag (can be NULL if not calculated)
#' @param consensus_rule integer rule
#' @noRd
.compute_consensus_flag <- function(
    mean_flag,
    median_flag,
    iqr_flag,
    consensus_rule) {
  # only include non-NULL flags
  flags <- c()
  if (!is.null(mean_flag)) flags <- c(flags, mean_flag)
  if (!is.null(median_flag)) flags <- c(flags, median_flag)
  if (!is.null(iqr_flag)) flags <- c(flags, iqr_flag)

  if (length(flags) == 0) {
    return("insufficient_evidence")
  }

  guard_reasons <- c(
    "low_reporting",
    "insufficient_n",
    "insufficient_seasonal_history"
  )
  guard_flag <- flags[flags %in% guard_reasons]
  if (length(guard_flag) > 0) {
    return(guard_flag[[1]])
  }

  usable <- flags[!flags %in% c("unstable_scale", guard_reasons)]
  outlier_votes <- sum(usable == "outlier", na.rm = TRUE)
  total_votes <- sum(usable %in% c("outlier", "normal"), na.rm = TRUE)
  if (total_votes == 0) {
    return("insufficient_evidence")
  }
  if (outlier_votes >= consensus_rule) {
    "outlier"
  } else {
    "normal"
  }
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
          .outlier_value < mean_lower |
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
          .outlier_value < median_lower |
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
          .outlier_value < iqr_lower |
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

#' build subtitle summarising seasonality and guardrails
#'
#' @param data plotted data
#' @param min_n minimum n
#' @param reporting_rate_min reporting threshold
#' @noRd
.format_seasonality_caption <- function(data, min_n, reporting_rate_min) {
  if (nrow(data) == 0) {
    return("No data available")
  }
  first_row <- data[1, , drop = FALSE]
  strictness_shift <- if (isTRUE(first_row$strictness_shifted)) {
    " (shifted)"
  } else {
    ""
  }
  glue::glue(
    "Mode: {first_row$seasonality_mode_used} | ",
    "Window: {first_row$seasonal_window_desc} | ",
    "Strictness: {first_row$strictness_label}{strictness_shift} | ",
    "min_n = {min_n} | reporting >= {reporting_rate_min}"
  )
}

#' ensure detection output has required columns for plotting
#'
#' @param data detection output
#' @param admin_levels admin columns expected
#' @noRd
.validate_plot_inputs <- function(data, admin_levels) {
  required <- c(
    "value",
    "month",
    "outlier_flag_consensus",
    "seasonality_mode_used",
    "seasonal_window_desc"
  )
  missing <- required[!required %in% names(data)]
  if (length(missing) > 0) {
    cli::cli_abort(
      "Detection output missing columns required for plotting: {.val {missing}}"
    )
  }
  facet <- if (length(admin_levels) >= 2 && admin_levels[2] %in% names(data)) {
    admin_levels[2]
  } else if (admin_levels[1] %in% names(data)) {
    admin_levels[1]
  } else {
    cli::cli_abort("Could not determine facet column from admin levels.")
  }
  list(facet_column = facet)
}

#' adjust french labels for plots
#'
#' @param label text to adjust
#' @param language target language
#' @noRd
.translation_qc_labels <- function(label, language) {
  if (language != "fr") {
    return(label)
  }
  fixer <- base::get0(".enforce_fr_reporting_terms", mode = "function")
  if (base::is.function(fixer)) {
    return(fixer(label))
  }
  label
}

#' build ggplot object with colour scheme
#'
#' @param data plotting data
#' @param facet_column column to facet by
#' @param column target column name
#' @param yearmon year-month column name
#' @param year year column name
#' @param outlier_column column containing flag to colour
#' @param caption subtitle string
#' @noRd
.build_outlier_plot <- function(
    data,
    facet_column,
    column,
    yearmon,
    year,
    outlier_column,
    caption,
    target_language,
    date_breaks = "3 months") {
  # Binary outlier classification with your specified colors
  color_values <- c(
    "TRUE" = "red",      # Outliers
    "FALSE" = "grey"  # Normal points
  )

  data <- data |>
    dplyr::mutate(
      # Binary classification: TRUE = outlier, FALSE = normal
      .plot_flag = dplyr::case_when(
        outlier_flag_consensus == "outlier" ~ "TRUE",
        TRUE ~ "FALSE"
      ),
      .plot_date = lubridate::ymd(paste0(.data[[yearmon]], "-01"))
    ) |>
    # Sort so outliers (TRUE) are plotted on top
    dplyr::arrange(.plot_flag)

  title_text <- glue::glue(
    "Outlier Detection for <b>{column}</b>"
  )

  subtitle_text <- .translation_qc_labels(caption, target_language)

  ggplot2::ggplot(data) +
    ggplot2::geom_point(
      ggplot2::aes(
        x = .plot_date,
        y = value,
        color = .plot_flag
      ),
      alpha = 0.7,  # Add transparency to show density patterns
      size = 2
    ) +
    ggplot2::scale_color_manual(
      name = .translation_qc_labels(
        "Outlier classification",
        target_language
      ),
      values = color_values,
      breaks = names(color_values)
    ) +
    ggplot2::scale_x_date(
      date_breaks = date_breaks,
      labels = function(x) {
        sntutils::translate_yearmon(
          x,
          language = target_language,
          format = "%Y-%m"
        )
      }
    ) +
    ggplot2::labs(
      title = title_text,
      subtitle = subtitle_text,
      x = paste0("\n", yearmon),
      y = paste0(column, "\n")
    ) +
    ggplot2::facet_wrap(
      stats::as.formula(paste("~", facet_column)),
      scales = "free_y"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "bottom",
      plot.title = ggtext::element_markdown(),
      plot.subtitle = ggplot2::element_text(size = 9)
    )
}


# seasonality helpers extracted from outlier_detection_seasonality.R
# implements seasonal pooling ladder with audit metadata
# RELEVANT FILES:R/outlier_detection.R,R/outlier_detection_helpers.R

#' select seasonal pool using fallback ladder
#'
#' @param group_data data for a single admin group
#' @param row_index row index within group_data
#' @param params list of tuning parameters
#' @param strictness list with multiplier settings
#' @noRd
.select_seasonal_pool <- function(group_data, row_index, params, strictness) {
  target <- group_data[row_index, , drop = FALSE]
  pool_primary <- .seasonality_primary_pool(group_data, target, params)
  if (pool_primary$eligible) {
    return(.seasonality_result(
      mode_used = "same_month",
      pool = pool_primary$pool,
      desc = pool_primary$desc,
      strictness = strictness,
      fallback = FALSE
    ))
  }

  pool_neighbors <- .seasonality_neighbors_pool(group_data, target, params)
  if (pool_neighbors$eligible) {
    return(.seasonality_result(
      mode_used = "neighbors",
      pool = pool_neighbors$pool,
      desc = pool_neighbors$desc,
      strictness = strictness,
      fallback = TRUE,
      fallback_reason = "same_month_insufficient"
    ))
  }

  pool_residual <- .seasonality_residual_pool(group_data, target, params)
  if (pool_residual$eligible) {
    return(.seasonality_result(
      mode_used = "residual",
      pool = pool_residual$pool,
      desc = pool_residual$desc,
      strictness = strictness,
      fallback = TRUE,
      fallback_reason = "neighbors_insufficient",
      residual_stats = pool_residual$residual_stats
    ))
  }

  pool_across <- .seasonality_across_time_pool(
    group_data,
    target,
    params,
    strictness
  )
  if (pool_across$eligible) {
    return(pool_across$result)
  }

  list(
    pool = NULL,
    mode_used = "none",
    desc = pool_primary$desc,
    strictness = strictness,
    strictness_shifted = pool_across$strictness_shifted,
    fallback_applied = TRUE,
    fallback_reason = "insufficient_seasonal_history",
    residual_stats = NULL
  )
}

#' default seasonality result structure
#'
#' @param mode_used character
#' @param pool data frame or NULL
#' @param desc description text
#' @param strictness list with multipliers
#' @param fallback logical
#' @param fallback_reason character
#' @param residual_stats optional list
#' @noRd
.seasonality_result <- function(
    mode_used,
    pool,
    desc,
    strictness,
    fallback,
    fallback_reason = NA_character_,
    residual_stats = NULL) {
  list(
    pool = pool,
    mode_used = mode_used,
    desc = desc,
    strictness = strictness,
    strictness_shifted = isTRUE(strictness$strictness_shifted),
    fallback_applied = fallback,
    fallback_reason = fallback_reason,
    residual_stats = residual_stats
  )
}

#' same-month seasonal pool
#'
#' @param group_data data frame
#' @param target row data
#' @param params list
#' @noRd
.seasonality_primary_pool <- function(group_data, target, params) {
  window_start <- target$.outlier_year - params$seasonal_pool_years + 1
  pool <- dplyr::filter(
    group_data,
    .outlier_month == target$.outlier_month,
    .outlier_year >= window_start,
    .outlier_year <= target$.outlier_year
  )
  if (nrow(pool) == 0) {
    return(list(
      pool = pool,
      eligible = FALSE,
      desc = glue::glue(
        "Month {target$.outlier_month}, no same-month history"
      )
    ))
  }
  years_available <- unique(pool$.outlier_year)
  eligible <- nrow(pool) >= params$min_obs_per_seasonal_bucket &&
    length(years_available) >= params$min_years_per_month
  desc <- glue::glue(
    "Month {target$.outlier_month}, {min(pool$.outlier_year, na.rm = TRUE)}-",
    "{max(pool$.outlier_year, na.rm = TRUE)}; same-month window"
  )
  list(pool = pool, eligible = eligible, desc = desc)
}

#' neighbor seasonal pool with weights
#'
#' @param group_data data frame
#' @param target row data
#' @param params list
#' @noRd
.seasonality_neighbors_pool <- function(group_data, target, params) {
  window_start <- target$.outlier_year - params$seasonal_pool_years + 1
  neighbor_months <- ((target$.outlier_month + c(-1, 0, 1) - 1) %% 12) + 1
  pool <- dplyr::filter(
    group_data,
    .outlier_month %in% neighbor_months,
    .outlier_year >= window_start,
    .outlier_year <= target$.outlier_year
  )
  if (nrow(pool) == 0) {
    return(list(pool = pool, eligible = FALSE, desc = "No neighbor data"))
  }
  pool <- pool |>
    dplyr::mutate(
      .season_weight = dplyr::if_else(
        .outlier_month == target$.outlier_month,
        2L,
        1L
      )
    )
  obs <- sum(pool$.season_weight)
  eligible <- obs >= params$min_obs_per_seasonal_bucket
  month_names <- paste(sort(unique(neighbor_months)), collapse = "/")
  desc <- glue::glue(
    "Months {month_names}, {min(pool$.outlier_year, na.rm = TRUE)}-",
    "{max(pool$.outlier_year, na.rm = TRUE)}; neighbors weights 2/1"
  )
  list(pool = pool, eligible = eligible, desc = desc)
}

#' residual seasonal pool
#'
#' @param group_data data frame
#' @param target row data
#' @param params list
#' @noRd
.seasonality_residual_pool <- function(group_data, target, params) {
  if (nrow(group_data) < params$min_obs_per_seasonal_bucket) {
    return(list(
      pool = NULL,
      eligible = FALSE,
      desc = "Insufficient residual data"
    ))
  }

  month_means <- dplyr::summarise(
    dplyr::group_by(group_data, .outlier_month),
    month_mean = mean(.outlier_value, na.rm = TRUE),
    .groups = "drop"
  )
  residuals <- dplyr::left_join(
    group_data,
    month_means,
    by = ".outlier_month"
  ) |>
    dplyr::mutate(
      .residual_value = .outlier_value - month_mean
    )
  row_month_mean <- dplyr::filter(
    month_means,
    .outlier_month == target$.outlier_month
  )$month_mean
  if (length(row_month_mean) == 0 || is.na(row_month_mean)) {
    row_residual <- NA_real_
  } else {
    row_residual <- target$.outlier_value - row_month_mean
  }
  # Safe min/max calculation for year range
  years <- group_data$.outlier_year[!is.na(group_data$.outlier_year)]
  if (length(years) > 0) {
    year_range <- paste0(min(years), "-", max(years))
  } else {
    year_range <- "unknown"
  }

  desc <- glue::glue(
    "Residual pooled, months fixed; {year_range}"
  )
  residual_stats <- list(
    residual_sd = stats::sd(residuals$.residual_value, na.rm = TRUE),
    residual_mad = stats::mad(
      residuals$.residual_value,
      constant = params$mad_constant,
      na.rm = TRUE
    ),
    residual_iqr = stats::IQR(residuals$.residual_value, na.rm = TRUE)
  )
  list(
    pool = residuals,
    eligible = nrow(residuals) >= params$min_obs_per_seasonal_bucket,
    desc = desc,
    residual_stats = c(residual_stats, list(row_residual = row_residual))
  )
}

#' across-time fallback pool
#'
#' @param group_data data frame
#' @param target row data
#' @param params list
#' @param strictness list
#' @noRd
.seasonality_across_time_pool <- function(
    group_data,
    target,
    params,
    strictness) {
  if (nrow(group_data) < params$min_obs_per_seasonal_bucket) {
    return(list(
      eligible = FALSE,
      result = list(
        pool = NULL,
        mode_used = "none",
        desc = "Across-time insufficient",
        strictness_shifted = FALSE
      )
    ))
  }
  shifted <- .shift_strictness_one_level(
    strictness$strictness,
    strictness$sd_multiplier,
    strictness$mad_constant,
    strictness$mad_multiplier,
    strictness$iqr_multiplier
  )
  # Safe min/max calculation for year range
  years <- group_data$.outlier_year[!is.na(group_data$.outlier_year)]
  if (length(years) > 0) {
    year_range <- paste0(min(years), "-", max(years))
  } else {
    year_range <- "unknown"
  }

  desc <- glue::glue(
    "Across-time fallback, {year_range}"
  )
  list(
    eligible = TRUE,
    strictness_shifted = TRUE,
    result = .seasonality_result(
      mode_used = "across_time_fallback",
      pool = group_data,
      desc = desc,
      strictness = shifted,
      fallback = TRUE,
      fallback_reason = "residual_insufficient"
    )
  )
}

#' run seasonal detection with fallback ladder
#'
#' @param data prepared data with derived columns
#' @param admin_levels character vector of admin columns
#' @param strictness list with multiplier settings
#' @param params list of seasonality parameters
#' @param methods character vector of methods to calculate
#' @param consensus_rule integer consensus rule
#' @param min_n minimum observations per bucket
#' @param reporting_rate_min minimum reporting rate
#' @param value_type value type string
#' @noRd
.run_seasonal_detection <- function(
    data,
    detection_admin_levels,
    parallel_grouping_levels = detection_admin_levels,
    strictness,
    params,
    methods,
    consensus_rule,
    min_n,
    reporting_rate_min,
    value_type) {
  # Setup data.table threading for improved performance
  if (requireNamespace("data.table", quietly = TRUE)) {
    .setup_dt_threads()
    on.exit(.reset_dt_threads())
  }

  # Use parallel_grouping_levels for efficient grouping (e.g., by district)
  # This avoids creating 188k+ facility-level groups for better performance
  grouped <- dplyr::group_by(
    data,
    dplyr::across(dplyr::all_of(parallel_grouping_levels))
  )

  # Debug: Show how many groups we're processing
  n_groups <- dplyr::n_groups(grouped)
  level_desc <- if (length(parallel_grouping_levels) > 0) {
    paste(parallel_grouping_levels, collapse="/")
  } else {
    "admin"
  }
  cli::cli_inform("Processing {n_groups} groups at {level_desc} level")

  dplyr::group_modify(grouped, function(group_data, key) {
    # Sort data within this geographic group
    group_data <- dplyr::arrange(
      group_data,
      .outlier_year,
      .outlier_month,
      .outlier_yearmon
    )

    # Within this geographic group, we need to detect outliers at the spatial level
    # Get the spatial level column (e.g., facility ID)
    spatial_col <- setdiff(detection_admin_levels, parallel_grouping_levels)

    if (length(spatial_col) > 0) {
      # Group by spatial level within this geographic group
      spatial_grouped <- dplyr::group_by(
        group_data,
        dplyr::across(dplyr::all_of(spatial_col))
      )

      # Process each spatial unit (e.g., facility) within this geographic group
      dplyr::group_modify(spatial_grouped, function(spatial_data, spatial_key) {
        # For each facility, compute seasonal caches using the full group context
        same_month_cache <- .cache_same_month_pools(
          spatial_data,
          params,
          strictness,
          value_type
        )
        neighbors_cache <- .cache_neighbors_pools(
          same_month_cache,
          params,
          strictness,
          value_type
        )
        residual_cache <- .cache_residual_pools(
          spatial_data,
          params,
          strictness,
          value_type
        )
        across_time_cache <- .cache_across_time_pools(
          spatial_data,
          strictness,
          value_type
        )

        # Apply outlier detection to this spatial unit
        .select_pools_vectorized(
          spatial_data,
          same_month_cache,
          neighbors_cache,
          residual_cache,
          across_time_cache,
          params,
          methods,
          reporting_rate_min,
          min_n,
          consensus_rule
        )
      }) |> dplyr::ungroup()
    } else {
      # No spatial level specified, use original logic
      same_month_cache <- .cache_same_month_pools(
        group_data,
        params,
        strictness,
        value_type
      )
      neighbors_cache <- .cache_neighbors_pools(
        same_month_cache,
        params,
        strictness,
        value_type
      )
      residual_cache <- .cache_residual_pools(
        group_data,
        params,
        strictness,
        value_type
      )
      across_time_cache <- .cache_across_time_pools(
        group_data,
        strictness,
        value_type
      )

      .select_pools_vectorized(
        group_data,
        same_month_cache,
        neighbors_cache,
        residual_cache,
        across_time_cache,
        params,
        methods,
        reporting_rate_min,
        min_n,
        consensus_rule
      )
    }
  }) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      seasonality_mode_requested = "by_month"
    )
}


#' setup data.table threads for optimal performance
#'
#' @keywords internal
#' @noRd
.setup_dt_threads <- function() {
  if (!requireNamespace("data.table", quietly = TRUE)) {
    cli::cli_inform("data.table not available - parallel processing disabled")
    return(invisible(NULL))
  }

  # Get number of cores safely
  n_cores <- .get_cores_safe()

  # Use n-1 cores to leave one for system
  threads_to_use <- max(1, n_cores - 1)

  # Set data.table threads
  data.table::setDTthreads(threads_to_use)

  # Verify and report
  actual_threads <- data.table::getDTthreads()
  cli::cli_inform("data.table threading: {actual_threads}/{n_cores} threads (detected {n_cores} cores)")
}

#' safely detect number of cores across platforms
#'
#' @return integer number of cores
#' @keywords internal
#' @noRd
.get_cores_safe <- function() {
  tryCatch({
    # Try parallel package first
    if (requireNamespace("parallel", quietly = TRUE)) {
      cores <- parallel::detectCores(logical = FALSE)
      if (!is.na(cores) && cores > 0) return(cores)
    }

    # Platform-specific fallbacks
    if (.Platform$OS.type == "windows") {
      cores <- as.integer(Sys.getenv("NUMBER_OF_PROCESSORS"))
      if (!is.na(cores) && cores > 0) return(cores)
    } else {
      # Unix-like (Linux/macOS)
      cores <- system("nproc 2>/dev/null || sysctl -n hw.ncpu 2>/dev/null || echo 1", intern = TRUE)
      cores <- as.integer(cores[1])
      if (!is.na(cores) && cores > 0) return(cores)
    }

    return(1L)
  }, error = function(e) 1L)
}

#' reset data.table threads to default
#'
#' @keywords internal
#' @noRd
.reset_dt_threads <- function() {
  if (requireNamespace("data.table", quietly = TRUE)) {
    data.table::setDTthreads(0)  # 0 = reset to default
  }
}

#' setup file-based caching for outlier detection functions
#'
#' @param cache_stats logical flag to enable caching
#' @param cache_dir path to cache directory (if NULL, uses default)
#' @return list with cache objects or NULL if caching disabled
#' @keywords internal
#' @noRd
.setup_outlier_cache <- function(cache_stats = TRUE, cache_dir = NULL) {
  if (!cache_stats) {
    return(NULL)
  }

  if (!requireNamespace("memoise", quietly = TRUE)) {
    cli::cli_warn("memoise package not available - caching disabled")
    return(NULL)
  }

  # Set default cache directory if not provided
  if (is.null(cache_dir)) {
    if (requireNamespace("here", quietly = TRUE)) {
      cache_dir <- here::here("cache", "outlier_cache")
    } else {
      cache_dir <- file.path(tempdir(), "outlier_cache")
    }
  }

  # Create cache directory if it doesn't exist
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  }

  # Create filesystem cache backend
  cache_backend <- memoise::cache_filesystem(cache_dir)

  # Create list of cached function names to return
  list(
    backend = cache_backend,
    dir = cache_dir,
    enabled = TRUE
  )
}

#' apply caching to key outlier detection functions
#'
#' @param cache_config cache configuration from .setup_outlier_cache
#' @return invisible NULL (functions are memoised in parent environment)
#' @keywords internal
#' @noRd
.apply_outlier_caching <- function(cache_config) {
  if (is.null(cache_config) || !cache_config$enabled) {
    return(invisible(NULL))
  }

  # Apply caching to expensive functions that exist
  cache_backend <- cache_config$backend
  parent_env <- parent.frame()

  # Cache the main expensive detection functions
  if (exists(".run_seasonal_detection", envir = parent_env, inherits = FALSE)) {
    assign(".run_seasonal_detection",
           memoise::memoise(.run_seasonal_detection, cache = cache_backend),
           envir = parent_env)
  }

  if (exists(".run_non_seasonal_detection", envir = parent_env, inherits = FALSE)) {
    assign(".run_non_seasonal_detection",
           memoise::memoise(.run_non_seasonal_detection, cache = cache_backend),
           envir = parent_env)
  }

  # Cache utility functions
  if (exists(".parse_date_column", envir = parent_env, inherits = FALSE)) {
    assign(".parse_date_column",
           memoise::memoise(.parse_date_column, cache = cache_backend),
           envir = parent_env)
  }

  invisible(NULL)
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
#' @param admin_levels admin column names
#' @param yearmon yearmon column name
#' @param year year column name
#' @noRd
.filter_by_output_profile <- function(
    data,
    output_profile,
    record_id,
    admin_levels,
    yearmon,
    year) {
  # core columns always included
  core_cols <- c(record_id, admin_levels, yearmon, year, "month")

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
    key_indicators_used) {

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
    detection_admin_levels <- c(admin_level, spatial_level)
    parallel_grouping_levels <- admin_level  # For data.table parallel efficiency
  } else {
    # Original behavior - use all admin_level for both detection and grouping
    detection_admin_levels <- admin_level
    parallel_grouping_levels <- admin_level
  }

  context <- .prepare_outlier_context(
    data = data,
    column = column,
    record_id = record_id,
    admin_level = detection_admin_levels,
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
    seasonal_pool_years = seasonal_pool_years,
    min_years_per_month = min_years_per_month,
    cache_stats = FALSE,  # Don't cache within cached function
    spatial_level = spatial_level
  )

  if (options$time_mode == "by_month") {
    detection <- .run_seasonal_detection(
      data = context$prepared_data,
      detection_admin_levels = detection_admin_levels,
      parallel_grouping_levels = parallel_grouping_levels,
      strictness = context$strictness,
      params = context$seasonality_params,
      methods = methods,
      consensus_rule = consensus_rule,
      min_n = context$constraints$min_n,
      reporting_rate_min = context$constraints$reporting_rate_min,
      value_type = options$value_type
    )
  } else {
    detection <- .run_non_seasonal_detection(
      data = context$prepared_data,
      grouping_cols = context$grouping_cols,
      strictness = context$strictness,
      min_n = context$constraints$min_n,
      reporting_rate_min = context$constraints$reporting_rate_min,
      methods = methods,
      consensus_rule = consensus_rule,
      value_type = options$value_type,
      metadata = list(
        seasonality_mode_used = options$time_mode,
        seasonal_window_desc = NA_character_
      )
    )
  }

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
    key_indicators_used = key_indicators_used
  )

  .filter_by_output_profile(
    result,
    output_profile,
    record_id,
    context$admin_info$admin_levels,
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

# optimized seasonal detection cache functions ============================

#' cache same-month pools for all (month, year) combinations
#'
#' @param group_data data for one admin group
#' @param params seasonality parameters
#' @param strictness strictness settings
#' @param value_type value type string
#' @noRd
.cache_same_month_pools <- function(
    group_data,
    params,
    strictness,
    value_type) {
  group_data |>
    dplyr::group_by(.outlier_month, .outlier_year) |>
    dplyr::group_modify(function(year_month_data, key) {
      target_year <- key$.outlier_year
      window_start <- target_year - params$seasonal_pool_years + 1

      # get pool data for this month within the rolling window
      pool_data <- group_data |>
        dplyr::filter(
          .outlier_month == key$.outlier_month,
          .outlier_year >= window_start,
          .outlier_year <= target_year
        )

      if (nrow(pool_data) == 0) {
        return(dplyr::tibble(
          eligible = FALSE,
          n_in_group = 0L,
          desc = paste0(
            "Month ",
            key$.outlier_month,
            ", no same-month history"
          ),
          mean = NA_real_,
          sd = NA_real_,
          mean_lower = NA_real_,
          mean_upper = NA_real_,
          median = NA_real_,
          mad = NA_real_,
          median_lower = NA_real_,
          median_upper = NA_real_,
          q1 = NA_real_,
          q3 = NA_real_,
          iqr = NA_real_,
          iqr_lower = NA_real_,
          iqr_upper = NA_real_,
          unstable_mean = TRUE,
          unstable_median = TRUE,
          unstable_iqr = TRUE
        ))
      }

      years_available <- length(unique(pool_data$.outlier_year))
      eligible <- nrow(pool_data) >= params$min_obs_per_seasonal_bucket &
        years_available >= params$min_years_per_month

      # compute statistics directly
      n_vals <- nrow(pool_data)
      mean_val <- mean(pool_data$.outlier_value, na.rm = TRUE)
      sd_val <- stats::sd(pool_data$.outlier_value, na.rm = TRUE)
      median_val <- stats::median(pool_data$.outlier_value, na.rm = TRUE)
      mad_val <- stats::mad(
        pool_data$.outlier_value,
        constant = strictness$mad_constant,
        na.rm = TRUE
      )
      q1_val <- stats::quantile(
        pool_data$.outlier_value,
        probs = 0.25,
        na.rm = TRUE,
        names = FALSE
      )
      q3_val <- stats::quantile(
        pool_data$.outlier_value,
        probs = 0.75,
        na.rm = TRUE,
        names = FALSE
      )
      iqr_val <- q3_val - q1_val

      dplyr::tibble(
        eligible = eligible,
        n_in_group = as.integer(n_vals),
        desc = paste0(
          "Month ",
          key$.outlier_month,
          ", ",
          min(pool_data$.outlier_year, na.rm = TRUE),
          "-",
          max(pool_data$.outlier_year, na.rm = TRUE),
          "; same-month window"
        ),
        mean = mean_val,
        sd = sd_val,
        mean_lower = .apply_lower_bound_floor(
          mean_val - strictness$sd_multiplier * sd_val,
          value_type
        ),
        mean_upper = mean_val + strictness$sd_multiplier * sd_val,
        median = median_val,
        mad = mad_val,
        median_lower = .apply_lower_bound_floor(
          median_val - strictness$mad_multiplier * mad_val,
          value_type
        ),
        median_upper = median_val + strictness$mad_multiplier * mad_val,
        q1 = q1_val,
        q3 = q3_val,
        iqr = iqr_val,
        iqr_lower = .apply_lower_bound_floor(
          q1_val - strictness$iqr_multiplier * iqr_val,
          value_type
        ),
        iqr_upper = q3_val + strictness$iqr_multiplier * iqr_val,
        unstable_mean = is.na(sd_val) | sd_val == 0,
        unstable_median = is.na(mad_val) | mad_val == 0,
        unstable_iqr = is.na(iqr_val) | iqr_val == 0
      )
    }) |>
    dplyr::ungroup()
}

#' cache neighbor pools using weighted formulas (no vector replication)
#'
#' @param same_month_cache output from .cache_same_month_pools
#' @param params seasonality parameters
#' @param strictness strictness settings
#' @param value_type value type string
#' @noRd
.cache_neighbors_pools <- function(
    same_month_cache,
    params,
    strictness,
    value_type) {
  same_month_cache |>
    dplyr::group_by(.outlier_year) |>
    dplyr::group_modify(function(year_data, key) {
      target_year <- key$.outlier_year

      # get neighbor months for each target month
      year_data |>
        dplyr::rowwise() |>
        dplyr::mutate(
          neighbor_months = list(
            ((.outlier_month + c(-1, 0, 1) - 1) %% 12) + 1
          ),
          # get stats from same-month cache for neighbors
          neighbor_stats = list({
            neighbor_data <- same_month_cache |>
              dplyr::filter(
                .outlier_month %in% neighbor_months,
                .outlier_year == target_year,
                eligible
              )

            if (nrow(neighbor_data) == 0) {
              list(
                eligible = FALSE,
                n_in_group = 0L,
                desc = "No neighbor data",
                mean = NA_real_,
                median = NA_real_,
                mean_lower = NA_real_,
                mean_upper = NA_real_,
                median_lower = NA_real_,
                median_upper = NA_real_,
                q1 = NA_real_,
                q3 = NA_real_,
                iqr_lower = NA_real_,
                iqr_upper = NA_real_,
                unstable_mean = TRUE,
                unstable_median = TRUE,
                unstable_iqr = TRUE
              )
            } else {
              # weighted combination: same month weight 2, others weight 1
              weights <- ifelse(
                neighbor_data$.outlier_month == .outlier_month,
                2,
                1
              )
              total_weight <- sum(weights)

              if (total_weight >= params$min_obs_per_seasonal_bucket) {
                # weighted means for bounds calculation
                weighted_mean <- sum(
                  neighbor_data$mean * weights,
                  na.rm = TRUE
                ) / total_weight
                weighted_median <- sum(
                  neighbor_data$median * weights,
                  na.rm = TRUE
                ) / total_weight

                month_names <- paste(
                  sort(unique(neighbor_data$.outlier_month)),
                  collapse = "/"
                )

                list(
                  eligible = TRUE,
                  n_in_group = as.integer(total_weight),
                  desc = paste0(
                    "Months ",
                    month_names,
                    "; neighbors weights 2/1"
                  ),
                  mean = weighted_mean,
                  median = weighted_median,
                  # use approximate bounds from weighted centers
                  mean_lower = .apply_lower_bound_floor(
                    weighted_mean -
                      strictness$sd_multiplier *
                        mean(neighbor_data$sd, na.rm = TRUE),
                    value_type
                  ),
                  mean_upper = weighted_mean +
                    strictness$sd_multiplier *
                      mean(neighbor_data$sd, na.rm = TRUE),
                  median_lower = .apply_lower_bound_floor(
                    weighted_median -
                      strictness$mad_multiplier *
                        mean(neighbor_data$mad, na.rm = TRUE),
                    value_type
                  ),
                  median_upper = weighted_median +
                    strictness$mad_multiplier *
                      mean(neighbor_data$mad, na.rm = TRUE),
                  q1 = mean(neighbor_data$q1, na.rm = TRUE),
                  q3 = mean(neighbor_data$q3, na.rm = TRUE),
                  iqr_lower = .apply_lower_bound_floor(
                    mean(neighbor_data$q1, na.rm = TRUE) -
                      strictness$iqr_multiplier *
                        mean(neighbor_data$iqr, na.rm = TRUE),
                    value_type
                  ),
                  iqr_upper = mean(neighbor_data$q3, na.rm = TRUE) +
                    strictness$iqr_multiplier *
                      mean(neighbor_data$iqr, na.rm = TRUE),
                  unstable_mean = any(neighbor_data$unstable_mean),
                  unstable_median = any(neighbor_data$unstable_median),
                  unstable_iqr = any(neighbor_data$unstable_iqr)
                )
              } else {
                list(
                  eligible = FALSE,
                  n_in_group = as.integer(total_weight),
                  desc = "Insufficient neighbor data",
                  mean = NA_real_,
                  median = NA_real_,
                  mean_lower = NA_real_,
                  mean_upper = NA_real_,
                  median_lower = NA_real_,
                  median_upper = NA_real_,
                  q1 = NA_real_,
                  q3 = NA_real_,
                  iqr_lower = NA_real_,
                  iqr_upper = NA_real_,
                  unstable_mean = TRUE,
                  unstable_median = TRUE,
                  unstable_iqr = TRUE
                )
              }
            }
          })
        ) |>
        dplyr::ungroup() |>
        tidyr::unnest_wider(neighbor_stats, names_sep = "_") |>
        dplyr::select(-neighbor_months)
    }) |>
    dplyr::ungroup()
}

#' cache residual pools by year (computed once per group)
#'
#' @param group_data data for one admin group
#' @param params seasonality parameters
#' @param strictness strictness settings
#' @param value_type value type string
#' @noRd
.cache_residual_pools <- function(group_data, params, strictness, value_type) {
  # compute month means once for the whole group
  month_means <- group_data |>
    dplyr::group_by(.outlier_month) |>
    dplyr::summarise(
      month_mean = mean(.outlier_value, na.rm = TRUE),
      .groups = "drop"
    )

  # add residuals to group_data
  group_with_residuals <- group_data |>
    dplyr::left_join(month_means, by = ".outlier_month") |>
    dplyr::mutate(.residual_value = .outlier_value - month_mean)

  # compute residual stats by year
  group_with_residuals |>
    dplyr::group_by(.outlier_year) |>
    dplyr::summarise(
      eligible = dplyr::n() >= params$min_obs_per_seasonal_bucket,
      n_in_group = as.integer(dplyr::n()),
      desc = paste0("Residual pooled, months fixed; ",
                   min(.outlier_year, na.rm = TRUE), "-",
                   max(.outlier_year, na.rm = TRUE)),
      residual_sd = stats::sd(.residual_value, na.rm = TRUE),
      residual_mad = stats::mad(
        .residual_value,
        constant = strictness$mad_constant,
        na.rm = TRUE
      ),
      residual_iqr = stats::IQR(.residual_value, na.rm = TRUE),
      residual_mean = mean(.residual_value, na.rm = TRUE),
      residual_median = stats::median(.residual_value, na.rm = TRUE),
      residual_q1 = stats::quantile(
        .residual_value,
        probs = 0.25,
        na.rm = TRUE,
        names = FALSE
      ),
      residual_q3 = stats::quantile(
        .residual_value,
        probs = 0.75,
        na.rm = TRUE,
        names = FALSE
      ),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      mean_lower = residual_mean - strictness$sd_multiplier * residual_sd,
      mean_upper = residual_mean + strictness$sd_multiplier * residual_sd,
      median_lower = residual_median - strictness$mad_multiplier * residual_mad,
      median_upper = residual_median + strictness$mad_multiplier * residual_mad,
      iqr_lower = residual_q1 - strictness$iqr_multiplier * residual_iqr,
      iqr_upper = residual_q3 + strictness$iqr_multiplier * residual_iqr,
      unstable_mean = is.na(residual_sd) | residual_sd == 0,
      unstable_median = is.na(residual_mad) | residual_mad == 0,
      unstable_iqr = is.na(residual_iqr) | residual_iqr == 0
    )
}

#' cache across-time pools (one per group)
#'
#' @param group_data data for one admin group
#' @param strictness strictness settings
#' @param value_type value type string
#' @noRd
.cache_across_time_pools <- function(group_data, strictness, value_type) {
  # shift strictness toward lenient
  shifted_strictness <- .shift_strictness_one_level(
    strictness$strictness,
    strictness$sd_multiplier,
    strictness$mad_constant,
    strictness$mad_multiplier,
    strictness$iqr_multiplier
  )

  # compute across-time stats for the whole group
  n_vals <- nrow(group_data)
  mean_val <- mean(group_data$.outlier_value, na.rm = TRUE)
  sd_val <- stats::sd(group_data$.outlier_value, na.rm = TRUE)
  median_val <- stats::median(group_data$.outlier_value, na.rm = TRUE)
  mad_val <- stats::mad(
    group_data$.outlier_value,
    constant = shifted_strictness$mad_constant,
    na.rm = TRUE
  )
  q1_val <- stats::quantile(
    group_data$.outlier_value,
    probs = 0.25,
    na.rm = TRUE,
    names = FALSE
  )
  q3_val <- stats::quantile(
    group_data$.outlier_value,
    probs = 0.75,
    na.rm = TRUE,
    names = FALSE
  )
  iqr_val <- q3_val - q1_val

  dplyr::tibble(
    eligible = TRUE,
    n_in_group = as.integer(n_vals),
    desc = paste0(
      "Across-time fallback, ",
      if (length(group_data$.outlier_year[!is.na(group_data$.outlier_year)]) > 0) {
        paste0(
          min(group_data$.outlier_year, na.rm = TRUE),
          "-",
          max(group_data$.outlier_year, na.rm = TRUE)
        )
      } else {
        "unknown-years"
      }
    ),
    mean = mean_val,
    sd = sd_val,
    mean_lower = .apply_lower_bound_floor(
      mean_val - shifted_strictness$sd_multiplier * sd_val,
      value_type
    ),
    mean_upper = mean_val + shifted_strictness$sd_multiplier * sd_val,
    median = median_val,
    mad = mad_val,
    median_lower = .apply_lower_bound_floor(
      median_val - shifted_strictness$mad_multiplier * mad_val,
      value_type
    ),
    median_upper = median_val + shifted_strictness$mad_multiplier * mad_val,
    q1 = q1_val,
    q3 = q3_val,
    iqr = iqr_val,
    iqr_lower = .apply_lower_bound_floor(
      q1_val - shifted_strictness$iqr_multiplier * iqr_val,
      value_type
    ),
    iqr_upper = q3_val + shifted_strictness$iqr_multiplier * iqr_val,
    unstable_mean = is.na(sd_val) | sd_val == 0,
    unstable_median = is.na(mad_val) | mad_val == 0,
    unstable_iqr = is.na(iqr_val) | iqr_val == 0,
    strictness_label = shifted_strictness$strictness,
    strictness_shifted = TRUE,
    sd_multiplier = shifted_strictness$sd_multiplier,
    mad_constant = shifted_strictness$mad_constant,
    mad_multiplier = shifted_strictness$mad_multiplier,
    iqr_multiplier = shifted_strictness$iqr_multiplier
  )
}

#' vectorized pool selection using precomputed caches
#'
#' @param group_data original group data
#' @param same_month_cache same-month pool cache
#' @param neighbors_cache neighbors pool cache
#' @param residual_cache residual pool cache
#' @param across_time_cache across-time pool cache
#' @param params seasonality parameters
#' @param methods character vector of methods to calculate
#' @param reporting_rate_min minimum reporting rate
#' @param min_n minimum observations
#' @param consensus_rule consensus rule
#' @noRd
.select_pools_vectorized <- function(
  group_data,
  same_month_cache,
  neighbors_cache,
  residual_cache,
  across_time_cache,
  params,
  methods,
  reporting_rate_min,
  min_n,
  consensus_rule
) {
  # join all pool caches to group_data
  combined <- group_data |>
    # same-month pool
    dplyr::left_join(
      same_month_cache |>
        dplyr::select(
          .outlier_month,
          .outlier_year,
          same_eligible = eligible,
          same_n = n_in_group,
          same_desc = desc,
          same_mean_lower = mean_lower,
          same_mean_upper = mean_upper,
          same_median_lower = median_lower,
          same_median_upper = median_upper,
          same_iqr_lower = iqr_lower,
          same_iqr_upper = iqr_upper,
          same_unstable_mean = unstable_mean,
          same_unstable_median = unstable_median,
          same_unstable_iqr = unstable_iqr
        ),
      by = c(".outlier_month", ".outlier_year")
    ) |>
    # neighbors pool
    dplyr::left_join(
      neighbors_cache |>
        dplyr::select(
          .outlier_month,
          .outlier_year,
          neighbor_eligible = neighbor_stats_eligible,
          neighbor_n = neighbor_stats_n_in_group,
          neighbor_desc = neighbor_stats_desc,
          neighbor_mean_lower = neighbor_stats_mean_lower,
          neighbor_mean_upper = neighbor_stats_mean_upper,
          neighbor_median_lower = neighbor_stats_median_lower,
          neighbor_median_upper = neighbor_stats_median_upper,
          neighbor_iqr_lower = neighbor_stats_iqr_lower,
          neighbor_iqr_upper = neighbor_stats_iqr_upper,
          neighbor_unstable_mean = neighbor_stats_unstable_mean,
          neighbor_unstable_median = neighbor_stats_unstable_median,
          neighbor_unstable_iqr = neighbor_stats_unstable_iqr
        ),
      by = c(".outlier_month", ".outlier_year")
    ) |>
    # residual pool
    dplyr::left_join(
      residual_cache |>
        dplyr::select(
          .outlier_year,
          residual_eligible = eligible,
          residual_n = n_in_group,
          residual_desc = desc,
          residual_mean_lower = mean_lower,
          residual_mean_upper = mean_upper,
          residual_median_lower = median_lower,
          residual_median_upper = median_upper,
          residual_iqr_lower = iqr_lower,
          residual_iqr_upper = iqr_upper,
          residual_unstable_mean = unstable_mean,
          residual_unstable_median = unstable_median,
          residual_unstable_iqr = unstable_iqr,
          residual_sd,
          residual_mad,
          residual_iqr
        ),
      by = ".outlier_year"
    ) |>
    # across-time pool (same for all rows)
    dplyr::mutate(
      across_eligible = across_time_cache$eligible,
      across_n = across_time_cache$n_in_group,
      across_desc = across_time_cache$desc,
      across_mean_lower = across_time_cache$mean_lower,
      across_mean_upper = across_time_cache$mean_upper,
      across_median_lower = across_time_cache$median_lower,
      across_median_upper = across_time_cache$median_upper,
      across_iqr_lower = across_time_cache$iqr_lower,
      across_iqr_upper = across_time_cache$iqr_upper,
      across_unstable_mean = across_time_cache$unstable_mean,
      across_unstable_median = across_time_cache$unstable_median,
      across_unstable_iqr = across_time_cache$unstable_iqr,
      across_strictness_label = across_time_cache$strictness_label,
      across_strictness_shifted = across_time_cache$strictness_shifted,
      across_sd_multiplier = across_time_cache$sd_multiplier,
      across_mad_constant = across_time_cache$mad_constant,
      across_mad_multiplier = across_time_cache$mad_multiplier,
      across_iqr_multiplier = across_time_cache$iqr_multiplier
    ) |>
    # vectorized pool selection using ladder logic
    dplyr::mutate(
      # reporting guardrail first
      reporting_reason = .guardrail_reason(
        n = rep(Inf, dplyr::n()),
        reporting = .outlier_reporting,
        min_n = NA_real_,
        reporting_threshold = reporting_rate_min
      ),

      # select first eligible pool in ladder order
      pool_mode = dplyr::case_when(
        !is.na(reporting_reason) ~ "guard",
        same_eligible & same_n >= min_n ~ "same_month",
        neighbor_eligible & neighbor_n >= min_n ~ "neighbors",
        residual_eligible & residual_n >= min_n ~ "residual",
        across_eligible & across_n >= min_n ~ "across_time",
        TRUE ~ "insufficient_seasonal_history"
      ),

      # pick bounds based on selected pool
      mean_lower = dplyr::case_when(
        pool_mode == "same_month" ~ same_mean_lower,
        pool_mode == "neighbors" ~ neighbor_mean_lower,
        pool_mode == "residual" ~ residual_mean_lower,
        pool_mode == "across_time" ~ across_mean_lower,
        TRUE ~ NA_real_
      ),
      mean_upper = dplyr::case_when(
        pool_mode == "same_month" ~ same_mean_upper,
        pool_mode == "neighbors" ~ neighbor_mean_upper,
        pool_mode == "residual" ~ residual_mean_upper,
        pool_mode == "across_time" ~ across_mean_upper,
        TRUE ~ NA_real_
      ),
      median_lower = dplyr::case_when(
        pool_mode == "same_month" ~ same_median_lower,
        pool_mode == "neighbors" ~ neighbor_median_lower,
        pool_mode == "residual" ~ residual_median_lower,
        pool_mode == "across_time" ~ across_median_lower,
        TRUE ~ NA_real_
      ),
      median_upper = dplyr::case_when(
        pool_mode == "same_month" ~ same_median_upper,
        pool_mode == "neighbors" ~ neighbor_median_upper,
        pool_mode == "residual" ~ residual_median_upper,
        pool_mode == "across_time" ~ across_median_upper,
        TRUE ~ NA_real_
      ),
      iqr_lower = dplyr::case_when(
        pool_mode == "same_month" ~ same_iqr_lower,
        pool_mode == "neighbors" ~ neighbor_iqr_lower,
        pool_mode == "residual" ~ residual_iqr_lower,
        pool_mode == "across_time" ~ across_iqr_lower,
        TRUE ~ NA_real_
      ),
      iqr_upper = dplyr::case_when(
        pool_mode == "same_month" ~ same_iqr_upper,
        pool_mode == "neighbors" ~ neighbor_iqr_upper,
        pool_mode == "residual" ~ residual_iqr_upper,
        pool_mode == "across_time" ~ across_iqr_upper,
        TRUE ~ NA_real_
      ),

      # pick unstable flags
      unstable_mean = dplyr::case_when(
        pool_mode == "same_month" ~ same_unstable_mean,
        pool_mode == "neighbors" ~ neighbor_unstable_mean,
        pool_mode == "residual" ~ residual_unstable_mean,
        pool_mode == "across_time" ~ across_unstable_mean,
        TRUE ~ TRUE
      ),
      unstable_median = dplyr::case_when(
        pool_mode == "same_month" ~ same_unstable_median,
        pool_mode == "neighbors" ~ neighbor_unstable_median,
        pool_mode == "residual" ~ residual_unstable_median,
        pool_mode == "across_time" ~ across_unstable_median,
        TRUE ~ TRUE
      ),
      unstable_iqr = dplyr::case_when(
        pool_mode == "same_month" ~ same_unstable_iqr,
        pool_mode == "neighbors" ~ neighbor_unstable_iqr,
        pool_mode == "residual" ~ residual_unstable_iqr,
        pool_mode == "across_time" ~ across_unstable_iqr,
        TRUE ~ TRUE
      ),

      # pick n_in_group
      n_in_group = dplyr::case_when(
        pool_mode == "same_month" ~ same_n,
        pool_mode == "neighbors" ~ neighbor_n,
        pool_mode == "residual" ~ residual_n,
        pool_mode == "across_time" ~ across_n,
        TRUE ~ 0L
      ),

      # pick descriptions and metadata
      seasonal_window_desc = dplyr::case_when(
        pool_mode == "same_month" ~ same_desc,
        pool_mode == "neighbors" ~ neighbor_desc,
        pool_mode == "residual" ~ residual_desc,
        pool_mode == "across_time" ~ across_desc,
        TRUE ~ "Insufficient seasonal history"
      ),

      seasonality_mode_used = pool_mode,

      fallback_applied = pool_mode != "same_month" & pool_mode != "guard",
      fallback_reason = dplyr::case_when(
        pool_mode == "neighbors" ~ "same_month_insufficient",
        pool_mode == "residual" ~ "neighbors_insufficient",
        pool_mode == "across_time" ~ "residual_insufficient",
        pool_mode == "insufficient_seasonal_history" ~
          "insufficient_seasonal_history",
        TRUE ~ NA_character_
      ),

      strictness_label = dplyr::case_when(
        pool_mode == "across_time" ~ across_strictness_label,
        TRUE ~ "balanced" # default for other modes
      ),
      strictness_shifted = pool_mode == "across_time",

      sd_multiplier = dplyr::case_when(
        pool_mode == "across_time" ~ across_sd_multiplier,
        TRUE ~ 3.0 # default
      ),
      mad_constant = dplyr::case_when(
        pool_mode == "across_time" ~ across_mad_constant,
        TRUE ~ 1.4826
      ),
      mad_multiplier = dplyr::case_when(
        pool_mode == "across_time" ~ across_mad_multiplier,
        TRUE ~ 9.0
      ),
      iqr_multiplier = dplyr::case_when(
        pool_mode == "across_time" ~ across_iqr_multiplier,
        TRUE ~ 2.0
      ),

      # guardrail reason
      reason = dplyr::case_when(
        !is.na(reporting_reason) ~ reporting_reason,
        pool_mode == "insufficient_seasonal_history" ~
          "insufficient_seasonal_history",
        n_in_group < min_n ~ "insufficient_n",
        TRUE ~ NA_character_
      )
    ) |>
    # vectorized flag calculations using the selected bounds
    dplyr::mutate(
      # adjust comparison value for residual mode
      comparison_value = dplyr::case_when(
        pool_mode == "residual" ~
          {
            # compute residual for each row
            month_means_lookup <- group_data |>
              dplyr::group_by(.outlier_month) |>
              dplyr::summarise(
                month_mean = mean(.outlier_value, na.rm = TRUE),
                .groups = "drop"
              )

            row_month_mean <- month_means_lookup$month_mean[match(
              .outlier_month,
              month_means_lookup$.outlier_month
            )]
            .outlier_value - row_month_mean
          },
        TRUE ~ .outlier_value
      ),

      # vectorized flag calculations
      mean_flag = dplyr::case_when(
        !is.na(reason) & reason != "" ~ reason,
        unstable_mean ~ "unstable_scale",
        is.na(comparison_value) |
          is.na(mean_lower) |
          is.na(mean_upper) ~
          "insufficient_evidence",
        comparison_value < mean_lower |
          comparison_value > mean_upper ~
          "outlier",
        TRUE ~ "normal"
      ),
      median_flag = dplyr::case_when(
        !is.na(reason) & reason != "" ~ reason,
        unstable_median ~ "unstable_scale",
        is.na(comparison_value) | is.na(median_lower) | is.na(median_upper) ~
          "insufficient_evidence",
        comparison_value < median_lower |
          comparison_value > median_upper ~
          "outlier",
        TRUE ~ "normal"
      ),
      iqr_flag = dplyr::case_when(
        !is.na(reason) & reason != "" ~ reason,
        unstable_iqr ~ "unstable_scale",
        is.na(comparison_value) | is.na(iqr_lower) | is.na(iqr_upper) ~
          "insufficient_evidence",
        comparison_value < iqr_lower |
          comparison_value > iqr_upper ~
          "outlier",
        TRUE ~ "normal"
      )
    ) |>
    # vectorized consensus calculation
    dplyr::mutate(
      outlier_flag_consensus = .vectorized_consensus_flag(
        mean_flag,
        median_flag,
        iqr_flag,
        consensus_rule
      ),

      # add missing columns with defaults
      mean = NA_real_,
      sd = NA_real_,
      median = NA_real_,
      mad = NA_real_,
      q1 = NA_real_,
      q3 = NA_real_,
      iqr = NA_real_,
      mean_upper = dplyr::coalesce(mean_upper, NA_real_),
      median_upper = dplyr::coalesce(median_upper, NA_real_),
      iqr_upper = dplyr::coalesce(iqr_upper, NA_real_),
      residual_sd = dplyr::coalesce(residual_sd, NA_real_),
      residual_mad = dplyr::coalesce(residual_mad, NA_real_),
      residual_iqr = dplyr::coalesce(residual_iqr, NA_real_),

      # finalize reason
      reason = dplyr::case_when(
        !is.na(reason) ~ reason,
        mean_flag == "unstable_scale" &
          median_flag == "unstable_scale" &
          iqr_flag == "unstable_scale" ~
          "unstable_scale",
        TRUE ~ NA_character_
      )
    ) |>
    # keep all columns from original data plus outlier detection results
    dplyr::select(
      .outlier_value,
      .outlier_year,
      .outlier_month,
      .outlier_yearmon,
      .outlier_reporting,
      dplyr::everything()
    )

  combined
}
