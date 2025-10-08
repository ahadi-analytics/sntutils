#' Validate health facility data comprehensively
#' Global variables for R CMD check
#'
#' @name global_vars
#' @keywords internal
#' @noRd
utils::globalVariables(c(
  "has_activeness_data", "periods_with_data", "total_periods",
  "activeness_category", "reporting_rate", "has_data"
))

#'
#' Orchestrates a suite of validation checks on routine HF data. It standardizes
#' column resolution, selects indicators, runs missing/duplicate/future/logic/
#' outlier checks, compiles a summary, and optionally translates and saves.
#'
#' @param data data.frame. Routine facility dataset.
#' @param id_col character. Unique record id. Default "record_id".
#' @param facility_col character. Facility id. Default "hf_id".
#' @param date_col character. Date column. Default "date".
#' @param yearmon_col character. Year-month col. Default "yearmon".
#' @param year_col character. Year col. Default "year".
#' @param month_col character. Month col. Default "month".
#' @param admin_cols character. Admin columns (adm0..adm3). Default
#'   c("adm0","adm1","adm2","adm3").
#' @param admin_guid_cols character. Admin GUID columns. Default
#'   c("adm0_guid","adm1_guid","adm2_guid","adm3_guid").
#' @param core_id_cols character|NULL. If NULL, uses the set above.
#' @param indicators character|NULL. Numeric indicators to validate. If NULL,
#'   auto-detect from numeric columns after excluding core ids.
#' @param consistency_pairs list|NULL. Each element is list(input=, output=).
#'   If NULL, defaults generated from common malaria cascade rules.
#' @param check_future_dates logical. Default TRUE.
#' @param check_duplicates logical. Default TRUE.
#' @param check_outliers logical. Default TRUE.
#' @param check_facility_activeness logical. Check facility activeness. Default TRUE.
#' @param hf_name_col character. Facility name column. Default "hf".
#' @param hf_uid_col character. Facility UID column. Default "hf_uid".
#' @param key_indicators character|NULL. Key indicators for activeness check.
#'   If NULL, uses all indicators. Default NULL.
#' @param min_reporting_rate numeric. Minimum reporting rate threshold. Default 0.5.
#' @param outlier_methods character. Any of c("iqr","median","mean").
#' @param iqr_multiplier numeric. Default 1.5.
#' @param save_results logical. Save outputs. Default FALSE.
#' @param output_path character|NULL. Required if save_results.
#' @param output_name character. Base output name. Default
#'   "validation_routine_data_results".
#' @param output_formats character. Any of c("xlsx","rds"). Default
#'   c("xlsx","rds").
#' @param verbose logical. CLI messages. Default TRUE.
#' @param target_language character. ISO-639-1. Default "en".
#' @param source_language character. ISO-639-1. Default "en".
#' @param lang_cache_path character. Path for translation cache. Default
#'   base::tempdir().
#'
#' @return invisible named list with elements:
#'   Summary, Missing values, Duplicate records, Future dates,
#'   Consistency failures, Consistency details, Outliers,
#'   Facility activeness over time, Facility activeness summary
#'
#' @examples
#' \dontrun{
#' validate_routine_hf_data(
#'   data = dhis2_data,
#'   indicators = c("conf","test","susp","maltreat")
#' )
#' }
#' @export
validate_routine_hf_data <- function(
  data,
  id_col = "record_id",
  facility_col = "hf_id",
  date_col = "date",
  yearmon_col = "yearmon",
  year_col = "year",
  month_col = "month",
  admin_cols = c("adm0", "adm1", "adm2", "adm3"),
  admin_guid_cols = c("adm0_guid", "adm1_guid", "adm2_guid", "adm3_guid"),
  core_id_cols = NULL,
  indicators = NULL,
  consistency_pairs = NULL,
  check_future_dates = TRUE,
  check_duplicates = TRUE,
  check_outliers = TRUE,
  check_facility_activeness = TRUE,
  hf_name_col = "hf",
  hf_uid_col = "hf_uid",
  key_indicators = NULL,
  min_reporting_rate = 0.5,
  outlier_methods = c("iqr", "median", "mean"),
  iqr_multiplier = 1.5,
  save_results = FALSE,
  output_path = NULL,
  output_name = "validation_of_hf_routine_data",
  output_formats = c("xlsx", "rds"),
  verbose = TRUE,
  target_language = "en",
  source_language = "en",
  lang_cache_path = base::tempdir()
) {
  # validate args early and fail fast
  .validate_args(
    data = data,
    save_results = save_results,
    output_path = output_path,
    outlier_methods = outlier_methods
  )

  # print quick dataset header
  if (verbose) {
    cli::cli_h1("Comprehensive Facility Data Validation")
    n_rows <- sntutils::big_mark(base::nrow(data))
    n_cols <- sntutils::big_mark(base::ncol(data))
    cli::cli_alert_info("Dataset: {n_rows} rows, {n_cols} columns")
  }

  # resolve column sets consistently
  cols <- .resolve_columns(
    data = data,
    id_col = id_col,
    facility_col = facility_col,
    date_col = date_col,
    yearmon_col = yearmon_col,
    year_col = year_col,
    month_col = month_col,
    admin_cols = admin_cols,
    admin_guid_cols = admin_guid_cols,
    core_id_cols = core_id_cols
  )

  # choose indicators (explicit or auto)
  indicators_use <- .pick_indicators(
    data = data,
    indicators = indicators,
    exclude = cols$core_id_cols,
    verbose = verbose
  )

  # build consistency pairs
  pairs_use <- .build_consistency_pairs(
    data = data,
    consistency_pairs = consistency_pairs
  )

  # prepare empty results container
  results <- base::list(
    Summary = tibble::tibble(
      check = character(),
      issues_found = character(),
      total_records = character(),
      percent = numeric()
    ),
    "Missing values" = NULL,
    "Duplicate records" = NULL,
    "Future dates" = NULL,
    "Consistency failures" = base::list(),
    "Consistency details" = NULL,
    "Outliers" = NULL,
    "HF activeness detail" = NULL,
    "HF activeness summary" = NULL
  )

  # check 1: missing values
  mv <- .check_missing(
    data = data,
    core_id_cols = cols$core_id_cols,
    indicators = indicators_use,
    verbose = verbose
  )
  results[["Missing values"]] <- mv$missing_table
  results$Summary <- dplyr::bind_rows(results$Summary, mv$summary_rows)

  # check 2: duplicates
  if (check_duplicates && cols$id_col %in% base::names(data)) {
    dup <- .check_duplicates(
      data = data,
      id_col = cols$id_col,
      verbose = verbose
    )
    results[["Duplicate records"]] <- dup$duplicates
    results$Summary <- dplyr::bind_rows(results$Summary, dup$summary_row)
  }

  # check 3: future dates
  if (check_future_dates && cols$date_col %in% base::names(data)) {
    fut <- .check_future_dates(
      data = data,
      date_col = cols$date_col,
      verbose = verbose
    )
    results[["Future dates"]] <- fut$future_rows
    results$Summary <- dplyr::bind_rows(results$Summary, fut$summary_row)
  }

  # check 4: logical consistency
  if (base::length(pairs_use) > 0) {
    cons <- .check_consistency(
      data = data,
      core_id_cols = cols$core_id_cols,
      pairs = pairs_use,
      verbose = verbose
    )
    results[["Consistency failures"]] <- cons$failures_by_pair
    results[["Consistency details"]] <- cons$details
    results$Summary <- dplyr::bind_rows(results$Summary, cons$summary_row)
  }

  # check 5: outliers
  if (check_outliers && base::length(indicators_use) > 0) {
    out <- .detect_outliers(
      data = data,
      id_col = cols$id_col,
      date_col = cols$date_col,
      year_col = cols$year_col,
      adm_cols = cols$admin_cols,
      yearmon_col = cols$yearmon_col,
      month_col = cols$month_col,
      indicators = indicators_use,
      methods = outlier_methods,
      iqr_multiplier = iqr_multiplier,
      verbose = verbose
    )
    results[["Outliers"]] <- out$combined
    results$Summary <- dplyr::bind_rows(results$Summary, out$summary_row)
  }

  # check 6: facility activeness
  if (check_facility_activeness) {
    act <- .check_facility_activeness(
      data = data,
      id_col = cols$id_col,
      facility_col = cols$facility_col,
      date_col = cols$date_col,
      hf_name_col = hf_name_col,
      hf_uid_col = hf_uid_col,
      yearmon_col = cols$yearmon_col,
      admin_cols = cols$admin_cols,
      indicators = indicators_use,
      key_indicators = key_indicators,
      min_reporting_rate = min_reporting_rate,
      verbose = verbose
    )
    results[["HF activeness detail"]] <- act$activeness_over_time
    results[["HF activeness summary"]] <- act$activeness_summary
    results$Summary <- dplyr::bind_rows(results$Summary, act$summary_row)
  }

  # final summary print
  if (verbose) {
    cli::cli_h2("Validation Summary")
    base::print(results$Summary)
    n_issue_checks <- base::sum(results$Summary$percent > 0, na.rm = TRUE)
    if (n_issue_checks == 0) {
      cli::cli_alert_success("All validation checks passed!")
    } else {
      n_checks <- sntutils::big_mark(n_issue_checks)
      n_total <- sntutils::big_mark(base::nrow(results$Summary))
      cat("\n")
      cli::cli_alert_warning("{n_checks} of {n_total} check(s) found issues")
    }
  }

  # optional translation + save
  if (save_results) {
    # translate only sheet/tab names and selected headers
    res_translated <- .translate_results(
      results = results,
      target_language = target_language,
      source_language = source_language,
      cache_path = lang_cache_path
    )

    # ensure utf8 and write files
    .save_outputs(
      results = res_translated,
      output_path = output_path,
      output_name = output_name,
      output_formats = output_formats,
      verbose = verbose
    )
  }

  # return as invisible
  base::invisible(results)
}

#' Validate top-level inputs and simple constraints
#'
#' @param data data.frame.
#' @param save_results logical.
#' @param output_path character|NULL.
#' @param outlier_methods character.
#' @keywords internal
#' @noRd
.validate_args <- function(data,
                           save_results,
                           output_path,
                           outlier_methods) {
  # check data frame
  if (!base::is.data.frame(data)) {
    cli::cli_abort("`data` must be a data frame.")
  }

  # ensure non-empty
  if (base::nrow(data) == 0L) {
    cli::cli_abort("`data` has zero rows.")
  }

  # validate saving path if needed
  if (save_results && base::is.null(output_path)) {
    cli::cli_abort(
      "`output_path` must be provided when `save_results = TRUE`."
    )
  }

  # validate methods content
  allowed <- c("iqr", "median", "mean")
  bad <- setdiff(outlier_methods, allowed)
  if (length(bad) > 0) {
    cli::cli_abort("Unsupported `outlier_methods`: {bad}.")
  }
}

#' Resolve column sets consistently
#'
#' @return named list of resolved columns
#' @keywords internal
#' @noRd
.resolve_columns <- function(data,
                             id_col,
                             facility_col,
                             date_col,
                             yearmon_col,
                             year_col,
                             month_col,
                             admin_cols,
                             admin_guid_cols,
                             core_id_cols) {
  # build full core id set if not supplied
  if (base::is.null(core_id_cols)) {
    core_id_cols <- base::c(
      id_col,
      facility_col,
      date_col,
      yearmon_col,
      year_col,
      month_col,
      admin_cols,
      admin_guid_cols
    )
  }

  # keep only present columns
  core_id_cols <- base::unique(core_id_cols[core_id_cols %in% names(data)])

  # construct return list
  base::list(
    id_col = id_col,
    facility_col = facility_col,
    date_col = date_col,
    yearmon_col = yearmon_col,
    year_col = year_col,
    month_col = month_col,
    admin_cols = admin_cols[admin_cols %in% names(data)],
    admin_guid_cols = admin_guid_cols[admin_guid_cols %in% names(data)],
    core_id_cols = core_id_cols
  )
}

#' Pick indicators to validate (explicit or auto)
#'
#' @keywords internal
#' @noRd
.pick_indicators <- function(data, indicators, exclude, verbose) {
  # if provided, filter to present & numeric
  if (!base::is.null(indicators)) {
    indicators <- indicators[
      indicators %in% names(data) &
        base::sapply(data[indicators], base::is.numeric)
    ]
  } else {
    # comprehensive list of malaria indicators including all disaggregations
    common_indicators <- base::c(
      # core indicators
      "conf",
      "test",
      "susp",
      "maltreat",
      "pres",
      "allout",
      "alladm",
      "maladm",
      "alldth",
      "maldth",
      # test disaggregations
      "test_mic",
      "test_rdt",
      "test_u5",
      "test_ov5",
      "test_preg",
      "test_mic_u5",
      "test_mic_ov5",
      "test_mic_preg",
      "test_rdt_u5",
      "test_rdt_ov5",
      "test_rdt_preg",
      # confirmed cases disaggregations
      "conf_mic",
      "conf_rdt",
      "conf_u5",
      "conf_ov5",
      "conf_preg",
      "conf_mic_u5",
      "conf_mic_ov5",
      "conf_mic_preg",
      "conf_rdt_u5",
      "conf_rdt_ov5",
      "conf_rdt_preg",
      "conf_preg_u3m",
      "conf_preg_ov3m",
      # suspected disaggregations
      "susp_u5",
      "susp_ov5",
      "susp_preg",
      # presumed disaggregations
      "pres_u5",
      "pres_ov5",
      "pres_preg",
      # treatment disaggregations
      "maltreat_u5",
      "maltreat_ov5",
      "maltreat_preg",
      # severe/admission disaggregations
      "maladm_u5",
      "maladm_ov5",
      "maladm_preg",
      # death disaggregations
      "maldth_u5",
      "maldth_ov5",
      "maldth_preg",
      "maldth_sev"
    )
    indicators <- common_indicators[common_indicators %in% names(data)]

    # fallback to all numeric columns excluding core ids
    if (base::length(indicators) == 0L) {
      indicators <- names(data)[
        base::sapply(data, base::is.numeric) & !names(data) %in% exclude
      ]
    }
  }

  # print if requested
  if (verbose && length(indicators) > 0) {
    n_ind <- sntutils::big_mark(length(indicators))
    ind_list <- base::paste(indicators, collapse = ", ")
    cli::cli_alert_info("Validating {n_ind} indicator(s): {ind_list}")
  }

  indicators
}

#' Build default or user-specified consistency pairs
#'
#' @keywords internal
#' @noRd
.build_consistency_pairs <- function(data, consistency_pairs) {
  # if user supplied, filter to present columns
  if (!base::is.null(consistency_pairs)) {
    keep <- base::Filter(
      function(p) p$input %in% names(data) && p$output %in% names(data),
      consistency_pairs
    )
    return(keep)
  }

  # base cascade rules
  base_rules <- list(
    list(input = "allout", output = "test"),
    list(input = "allout", output = "susp"),
    list(input = "susp", output = "test"),
    list(input = "test", output = "conf"),
    list(input = "conf", output = "pres"),
    list(input = "conf", output = "maltreat"),
    list(input = "alladm", output = "maladm"),
    list(input = "alldth", output = "maldth"),
    list(input = "maladm", output = "maldth"),
    list(input = "test_mic", output = "conf_mic"),
    list(input = "test_rdt", output = "conf_rdt")
  )

  # disaggregation suffixes
  suf <- c(
    "_u5", "_ov5", "_preg",
    "_mic_u5", "_mic_ov5", "_mic_preg",
    "_rdt_u5", "_rdt_ov5", "_rdt_preg",
    "_preg_u3m", "_preg_ov3m"
  )

  # expand disaggregations if both sides exist
  pairs <- base_rules
  for (r in base_rules) {
    for (s in suf) {
      i <- base::paste0(r$input, s)
      o <- base::paste0(r$output, s)
      if (i %in% names(data) && o %in% names(data)) {
        pairs[[length(pairs) + 1]] <- list(input = i, output = o)
      }
    }
  }

  # filter to existing columns
  base::Filter(
    function(p) p$input %in% names(data) && p$output %in% names(data),
    pairs
  )
}

#' Check missing values on core ids and indicators
#'
#' @keywords internal
#' @noRd
.check_missing <- function(data, core_id_cols, indicators, verbose) {
  # union of columns to check
  cols <- base::unique(base::c(core_id_cols, indicators))
  cols <- cols[cols %in% names(data)]

  # build counts only if any columns are present
  if (length(cols) == 0L) {
    empty <- tibble::tibble(
      variable = character(),
      n_missing = integer(),
      total = integer(),
      percent_missing = numeric(),
      column_type = character()
    )
    return(
      base::list(
        missing_table = empty,
        summary_rows = tibble::tibble(
          check = character(),
          issues_found = character(),
          total_records = character(),
          percent = numeric()
        )
      )
    )
  }

  # compute missing counts
  missing_counts <- data |>
    dplyr::select(dplyr::all_of(cols)) |>
    purrr::map_int(~ base::sum(base::is.na(.x)))

  # assemble table
  tab <- tibble::tibble(
    variable = base::names(missing_counts),
    n_missing = base::unname(missing_counts),
    total = base::nrow(data),
    percent_missing = base::round(n_missing / total * 100, 2),
    column_type = dplyr::if_else(
      variable %in% core_id_cols, "Core ID", "Indicator"
    )
  ) |>
    dplyr::arrange(column_type, dplyr::desc(n_missing)) |>
    sntutils::auto_parse_types()

  # split core vs indicator rows for summary
  n_core_total <- base::sum(tab$column_type == "Core ID")
  n_core_miss <- base::sum(tab$n_missing > 0 & tab$column_type == "Core ID")

  n_ind_total <- base::sum(tab$column_type == "Indicator")
  n_ind_miss <- base::sum(tab$n_missing > 0 & tab$column_type == "Indicator")

  # build summary rows
  rows <- tibble::tibble(
    check = c("Missing values (Core IDs)", "Missing values (Indicators)"),
    issues_found = c(
      base::paste(sntutils::big_mark(n_core_miss), "column(s)"),
      base::paste(sntutils::big_mark(n_ind_miss), "column(s)")
    ),
    total_records = c(
      base::paste(sntutils::big_mark(n_core_total), "column(s)"),
      base::paste(sntutils::big_mark(n_ind_total), "column(s)")
    ),
    percent = c(
      base::round(n_core_miss / base::max(n_core_total, 1) * 100, 2),
      base::round(n_ind_miss / base::max(n_ind_total, 1) * 100, 2)
    )
  )

  # minimal CLI
  if (verbose) {
    cli::cli_h2("Check 1: Missing Values")
    if (base::any(tab$n_missing > 0)) {
      if (n_core_miss > 0) {
        cli::cli_alert_warning(
          "{sntutils::big_mark(n_core_miss)} core column(s) have missing values"
        )
      }
      if (n_ind_miss > 0) {
        cli::cli_alert_warning(
          "{sntutils::big_mark(n_ind_miss)} indicator(s) have missing values"
        )
      }
      base::print(tab |> dplyr::filter(n_missing > 0))
    } else {
      cli::cli_alert_success("No missing values in core ids or indicators")
    }
  }

  base::list(missing_table = tab, summary_rows = rows)
}

#' Check duplicate records by id
#'
#' @keywords internal
#' @noRd
.check_duplicates <- function(data, id_col, verbose) {
  # group and keep ids with n > 1
  dup <- data |>
    dplyr::group_by(.data[[id_col]]) |>
    dplyr::filter(dplyr::n() > 1) |>
    dplyr::ungroup() |>
    dplyr::arrange(.data[[id_col]]) |>
    sntutils::auto_parse_types()

  # counts
  n_dup_rows <- base::nrow(dup)
  n_dup_ids <- dplyr::n_distinct(dup[[id_col]])

  # CLI
  if (verbose) {
    cli::cli_h2("Check 2: Duplicate Records")
    if (n_dup_rows > 0) {
      cli::cli_alert_warning(
        "{sntutils::big_mark(n_dup_rows)} duplicate record(s) found across {sntutils::big_mark(n_dup_ids)} unique ID(s)"
      )
    } else {
      cli::cli_alert_success("No duplicate records found")
    }
  }

  # summary row
  row <- tibble::tibble(
    check = "Duplicate records",
    issues_found = base::paste(sntutils::big_mark(n_dup_ids), "set(s)"),
    total_records = base::as.character(sntutils::big_mark(base::nrow(data))),
    percent = base::round(n_dup_ids / base::nrow(data) * 100, 2)
  )

  base::list(duplicates = dup, summary_row = row)
}

#' Check future dates
#'
#' @keywords internal
#' @noRd
.check_future_dates <- function(data, date_col, verbose) {
  # attempt to coerce to Date
  date_vec <- tryCatch(
    {
      if (base::inherits(data[[date_col]], "Date")) data[[date_col]] else
        base::as.Date(data[[date_col]])
    },
    error = function(e) NULL
  )

  # bail safely
  if (base::is.null(date_vec)) {
    if (verbose) cli::cli_warn("Could not parse date column for future check")
    row <- tibble::tibble(
      check = "Future dates",
      issues_found = "0",
      total_records = base::as.character(sntutils::big_mark(base::nrow(data))),
      percent = 0
    )
    return(base::list(future_rows = NULL, summary_row = row))
  }

  # floor to month
  current_month <- lubridate::floor_date(base::Sys.Date(), "month")

  # filter to rows after current month
  fut <- data[date_vec > current_month, ] |> sntutils::auto_parse_types()
  n_fut <- base::nrow(fut)

  # CLI
  if (verbose) {
    cli::cli_h2("Check 3: Future Dates")
    if (n_fut > 0) {
      cli::cli_alert_warning("{sntutils::big_mark(n_fut)} record(s) with future dates")
    } else {
      cli::cli_alert_success("No future dates found")
    }
  }

  # summary row
  row <- tibble::tibble(
    check = "Future dates",
    issues_found = base::as.character(sntutils::big_mark(n_fut)),
    total_records = base::as.character(sntutils::big_mark(base::nrow(data))),
    percent = base::round(n_fut / base::nrow(data) * 100, 2)
  )

  base::list(future_rows = fut, summary_row = row)
}

#' Logical consistency checks for input >= output pairs
#'
#' @keywords internal
#' @noRd
.check_consistency <- function(data, core_id_cols, pairs, verbose) {
  # print header first
  if (verbose) cli::cli_h2("Check 4: Logical Consistency")

  # counters
  pairs_with_fail <- 0L
  total_pair_types <- 0L
  failures_map <- base::list()
  all_details <- base::list()

  # iterate rules
  for (p in pairs) {
    i <- p$input
    o <- p$output
    if (!i %in% names(data) || !o %in% names(data)) next

    # restrict to complete cases
    pd <- data |>
      dplyr::filter(!base::is.na(.data[[i]]), !base::is.na(.data[[o]])) |>
      dplyr::mutate(
        input_indicator = i,
        output_indicator = o,
        input_value = .data[[i]],
        output_value = .data[[o]],
        difference = output_value - input_value,
        is_inconsistent = difference > 0
      )

    if (base::nrow(pd) == 0L) next
    total_pair_types <- total_pair_types + 1L

    # z-score over differences
    d_mean <- base::mean(pd$difference, na.rm = TRUE)
    d_sd <- stats::sd(pd$difference, na.rm = TRUE)
    pd <- pd |>
      dplyr::mutate(
        difference_sd = dplyr::if_else(
          !base::is.na(difference) & d_sd > 0,
          (difference - d_mean) / d_sd,
          NA_real_
        ) |> round(digits = 2)
      )

    # details subset with available ids
    ids_keep <- core_id_cols[core_id_cols %in% names(data)]
    detail <- pd |>
      dplyr::select(
        dplyr::all_of(ids_keep),
        input_indicator, output_indicator,
        input_value, output_value, difference, difference_sd, is_inconsistent
      ) |>
      sntutils::auto_parse_types()
    all_details[[length(all_details) + 1]] <- detail

    # failures
    fails <- pd |> dplyr::filter(is_inconsistent) |> sntutils::auto_parse_types()
    failures_map[[base::paste(i, "vs", o)]] <- fails
    if (base::nrow(fails) > 0) pairs_with_fail <- pairs_with_fail + 1L

    # per-pair CLI
    if (verbose) {
      if (base::nrow(fails) > 0) {
        cli::cli_alert_warning(
          "{sntutils::big_mark(base::nrow(fails))} row(s) where {o} > {i}"
        )
      } else {
        cli::cli_alert_success("{i} vs {o}: All values logically consistent")
      }
    }
  }

  # aggregate details: keep inconsistent rows with stable columns
  details <- NULL
  if (length(all_details) > 0) {
    keep_cols <- c(
      "record_id", "date", "adm0", "adm1", "adm2", "adm3",
      "input_indicator", "output_indicator",
      "input_value", "output_value", "difference", "difference_sd"
    )
    details <- dplyr::bind_rows(all_details) |>
      dplyr::filter(is_inconsistent) |>
      dplyr::select(dplyr::any_of(keep_cols)) |>
      dplyr::arrange(dplyr::desc(difference)) |>
      sntutils::auto_parse_types()
  }

  # summary row
  row <- tibble::tibble(
    check = "Logical consistency",
    issues_found = base::paste(sntutils::big_mark(pairs_with_fail), "pair(s)"),
    total_records = base::paste(sntutils::big_mark(total_pair_types), "pair(s)"),
    percent = base::round(
      pairs_with_fail / base::max(total_pair_types, 1) * 100, 2
    )
  )

  base::list(
    failures_by_pair = failures_map,
    details = details,
    summary_row = row
  )
}

#' Outlier detection wrapper
#'
#' @keywords internal
#' @noRd
.detect_outliers <- function(data,
                             id_col,
                             date_col,
                             year_col,
                             adm_cols,
                             yearmon_col,
                             month_col,
                             indicators,
                             methods,
                             iqr_multiplier,
                             verbose) {
  admin_present <- adm_cols[adm_cols %in% names(data)]
  req <- c(
    id_col,
    year_col,
    yearmon_col,
    admin_present[seq_len(base::min(2, length(admin_present)))]
  )
  req <- req[req %in% names(data)]
  if (length(req) < 4) {
    if (verbose) {
      cli::cli_h2("Check 5: Outlier Detection")
      cli::cli_warn("Insufficient columns for outlier detection. Skipping.")
    }
    row <- tibble::tibble(
      check = "Outliers",
      issues_found = "0",
      total_records = base::as.character(sntutils::big_mark(base::nrow(data))),
      percent = 0
    )
    return(
      base::list(
        combined = tibble::tibble(),
        by_indicator = base::list(),
        summary_row = row
      )
    )
  }

  admin_use <- admin_present
  if (base::length(admin_use) > 2) {
    admin_use <- admin_use[seq_len(2)]
  }
  month_use <- NULL
  if (!base::is.null(month_col) && month_col %in% names(data)) {
    month_use <- month_col
  }

  if (verbose) cli::cli_h2("Check 5: Outlier Detection")

  by_ind <- base::list()
  for (ind in indicators) {
    value_type <- if (base::grepl("rate", ind, ignore.case = TRUE)) {
      "rate"
    } else if (base::grepl("ratio", ind, ignore.case = TRUE)) {
      "rate"
    } else {
      "count"
    }

    res <- tryCatch(
      {
        sntutils::detect_outliers(
          data = data,
          column = ind,
          record_id = id_col,
          admin_levels = admin_use,
          date = date_col,
          time_mode = "across_time",
          value_type = value_type,
          strictness = "advanced",
          sd_multiplier = 2,
          mad_constant = 1.4826,
          mad_multiplier = 6,
          iqr_multiplier = iqr_multiplier,
          min_n = 3,
          methods = methods,
          consensus_rule = 1,
          output_profile = "audit",
          verbose = FALSE
        )
      },
      error = function(e) {
        if (verbose) {
          cli::cli_warn("Outlier detection failed for {ind}: {e$message}")
        }
        NULL
      }
    )

    if (!base::is.null(res) && base::nrow(res) > 0) {
      flags <- base::paste0("outlier_flag_", methods)
      flags <- flags[flags %in% names(res)]
      if (base::length(flags) > 0) {
        keep <- res |>
          dplyr::filter(
            dplyr::if_any(dplyr::all_of(flags), ~ .x == "outlier")
          ) |>
          dplyr::mutate(indicator_source = ind) |>
          sntutils::auto_parse_types()

        if (verbose && base::nrow(keep) > 0) {
          pct <- base::round(base::nrow(keep) / base::nrow(data) * 100, 2)
          cli::cli_alert_info(
            "{ind}: {sntutils::big_mark(base::nrow(keep))} outlier(s) ({pct}%)"
          )
        }

        if (base::nrow(keep) > 0) {
          by_ind[[length(by_ind) + 1]] <- keep
        }
      }
    }
  }

  if (base::length(by_ind) > 0) {
    combined <- dplyr::bind_rows(by_ind)
  } else {
    combined <- tibble::tibble()
  }

  if (base::ncol(combined) > 0) {
    combined <- combined |> sntutils::auto_parse_types()
  }

  id_col_present <- id_col %in% names(combined)
  n_unique <- if (id_col_present) {
    dplyr::n_distinct(combined[[id_col]])
  } else if ("record_id" %in% names(combined)) {
    dplyr::n_distinct(combined[["record_id"]])
  } else {
    0L
  }

  if (verbose) {
    if (base::nrow(combined) > 0) {
      pct_recs <- base::round(n_unique / base::nrow(data) * 100, 2)
      cli::cli_alert_success(
        base::paste(
          "Outlier detection complete: {sntutils::big_mark(n_unique)} unique",
          "record(s) ({pct_recs}%) across",
          "{sntutils::big_mark(base::length(by_ind))} indicator(s)"
        )
      )
    } else {
      cli::cli_alert_success("No outliers detected")
    }
  }

  row <- tibble::tibble(
    check = "Outliers",
    issues_found = base::as.character(sntutils::big_mark(n_unique)),
    total_records = base::as.character(sntutils::big_mark(base::nrow(data))),
    percent = base::round(n_unique / base::nrow(data) * 100, 2)
  )

  base::list(
    combined = combined,
    by_indicator = by_ind,
    summary_row = row
  )
}

#' Translate select components for Excel sheet names and headers
#'
#' @keywords internal
#' @noRd
.translate_results <- function(results,
                               target_language,
                               source_language,
                               cache_path) {
  # no-op if same language
  if (target_language == source_language) {
    return(.ensure_utf8(results))
  }

  # small helper
  tr <- function(x) {
    translated <- tryCatch(
      {
        sntutils::translate_text(
          text = x,
          target_language = target_language,
          source_language = source_language,
          cache_path = cache_path
        )
      },
      error = function(e) x
    )

    if (base::length(translated) == 0L) translated <- x

    translated <- base::as.character(translated)
    translated <- base::trimws(translated)

    fallback <- base::rep(x, length.out = base::length(translated))
    missing_mask <- base::is.na(translated) | translated == ""
    if (base::any(missing_mask)) {
      translated[missing_mask] <- fallback[missing_mask]
    }

    translated <- base::enc2utf8(translated)
    base::Encoding(translated) <- "UTF-8"
    translated
  }

  # helper to convert snake_case to Sentence case
  snake_to_sentence <- function(x) {
    # Replace underscores with spaces
    x <- base::gsub("_", " ", x)
    # Replace dots with spaces
    x <- base::gsub("\\.", " ", x)
    # Capitalize first letter
    x <- base::paste0(base::toupper(base::substr(x, 1, 1)),
                      base::substr(x, 2, base::nchar(x)))
    x
  }

  # copy to modify
  out <- results

  # translate top-level names (sheet names)
  base::names(out) <- base::sapply(base::names(out), tr)

  # translate Summary headers and "check" values
  sum_name <- tr("Summary")
  if (!base::is.null(out[[sum_name]])) {
    # Convert column names to sentence case before translation
    col_names <- base::names(out[[sum_name]])
    col_names <- base::sapply(col_names, snake_to_sentence)
    # Translate column names and replace dots with spaces
    translated_names <- base::sapply(col_names, tr)
    translated_names <- base::gsub("\\.", " ", translated_names)
    base::names(out[[sum_name]]) <- translated_names

    # Translate check values
    out[[sum_name]][[1]] <- base::sapply(out[[sum_name]][[1]], tr)

    # Translate specific terms in issues_found and total_records columns
    # Look for column by position (2nd column is issues_found, 3rd is total_records)
    if (base::ncol(out[[sum_name]]) >= 2) {
      # Translate column(s), set(s), pair(s) in issues_found column
      out[[sum_name]][[2]] <- base::sapply(out[[sum_name]][[2]], function(x) {
        x <- base::gsub("column\\(s\\)", tr("column(s)"), x)
        x <- base::gsub("set\\(s\\)", tr("set(s)"), x)
        x <- base::gsub("pair\\(s\\)", tr("pair(s)"), x)
        x
      })
    }

    if (base::ncol(out[[sum_name]]) >= 3) {
      # Translate column(s), set(s), pair(s) in total_records column
      out[[sum_name]][[3]] <- base::sapply(out[[sum_name]][[3]], function(x) {
        x <- base::gsub("column\\(s\\)", tr("column(s)"), x)
        x <- base::gsub("set\\(s\\)", tr("set(s)"), x)
        x <- base::gsub("pair\\(s\\)", tr("pair(s)"), x)
        x
      })
    }
  }

  # translate Missing values headers
  mv_name <- tr("Missing values")
  if (!base::is.null(out[[mv_name]])) {
    # Convert column names to sentence case before translation
    col_names <- base::names(out[[mv_name]])
    col_names <- base::sapply(col_names, snake_to_sentence)
    # Translate column names and replace dots with spaces
    translated_names <- base::sapply(col_names, tr)
    translated_names <- base::gsub("\\.", " ", translated_names)
    base::names(out[[mv_name]]) <- translated_names

    # Translate Core ID and Indicator in column_type
    type_col <- base::which(base::grepl("type", base::tolower(base::names(out[[mv_name]]))))
    if (base::length(type_col) > 0) {
      out[[mv_name]][[type_col]] <- base::sapply(out[[mv_name]][[type_col]], function(x) {
        if (x == "Core ID") return(tr("Core ID"))
        if (x == "Indicator") return(tr("Indicator"))
        return(x)
      })
    }
  }

  # Filter out empty tabs (except Summary)
  filtered_out <- base::list()

  for (nm in base::names(out)) {
    # Always keep Summary
    if (nm == tr("Summary") || nm == "Summary") {
      filtered_out[[nm]] <- out[[nm]]
      next
    }

    # Check if tab has content
    content <- out[[nm]]

    if (!base::is.null(content)) {
      # For data frames/tibbles, check if they have rows
      if (base::is.data.frame(content) && base::nrow(content) > 0) {
        filtered_out[[nm]] <- content
      }
      # For lists (like Consistency failures), check if any element has content
      else if (base::is.list(content) && !base::is.data.frame(content)) {
        has_content <- FALSE
        for (item in content) {
          if (!base::is.null(item) &&
              ((base::is.data.frame(item) && base::nrow(item) > 0) ||
               (base::is.list(item) && base::length(item) > 0))) {
            has_content <- TRUE
            break
          }
        }
        if (has_content) {
          filtered_out[[nm]] <- content
        }
      }
    }
  }

  # utf8 enforce
  .ensure_utf8(filtered_out)
}

#' Ensure UTF-8 in character data recursively
#'
#' @keywords internal
#' @noRd
.ensure_utf8 <- function(x) {
  # character vector
  if (base::is.character(x)) {
    y <- base::enc2utf8(x)
    base::Encoding(y) <- "UTF-8"
    return(y)
  }

  # data frame
  if (base::is.data.frame(x)) {
    for (nm in base::names(x)) {
      if (base::is.character(x[[nm]])) x[[nm]] <- .ensure_utf8(x[[nm]])
    }
    base::names(x) <- .ensure_utf8(base::names(x))
    return(x)
  }

  # list
  if (base::is.list(x) && !base::is.data.frame(x)) {
    x <- base::lapply(x, .ensure_utf8)
    if (!base::is.null(base::names(x))) {
      base::names(x) <- .ensure_utf8(base::names(x))
    }
    return(x)
  }

  x
}

#' Save outputs to disk using sntutils writer
#'
#' @keywords internal
#' @noRd
.save_outputs <- function(results,
                          output_path,
                          output_name,
                          output_formats,
                          verbose) {
  if (verbose) cli::cli_h2("Saving Validation Results")

  tryCatch(
    {
      sntutils::write_snt_data(
        obj = results,
        path = output_path,
        data_name = output_name,
        file_formats = output_formats,
        include_date = TRUE,
        quiet = !verbose
      )
    },
    error = function(e) {
      cli::cli_alert_danger("Failed to save validation results: {e$message}")
    }
  )
}

#' Check facility activeness and reporting patterns (tabular version of facility_reporting_plot)
#'
#' @keywords internal
#' @noRd
.check_facility_activeness <- function(data,
                                       id_col,
                                       facility_col,
                                       date_col,
                                       hf_name_col,
                                       hf_uid_col,
                                       yearmon_col,
                                       admin_cols,
                                       indicators,
                                       key_indicators,
                                       min_reporting_rate,
                                       verbose) {
  # check if required columns exist
  required_cols <- c(hf_name_col, hf_uid_col)
  present_cols <- required_cols[required_cols %in% base::names(data)]

  if (base::length(present_cols) == 0) {
    if (verbose) {
      cli::cli_h2("Check 6: Facility Activeness")
      cli::cli_warn("Required columns ({hf_name_col}, {hf_uid_col}) not found. Skipping facility activeness check.")
    }
    row <- tibble::tibble(
      check = "HF Activeness",
      issues_found = "No data (0% active, 0% inactive)",
      total_records = "0",
      percent = 0
    )
    return(base::list(
      activeness_over_time = NULL,
      activeness_summary = NULL,
      summary_row = row
    ))
  }

  if (verbose) cli::cli_h2("Check 6: Facility Activeness")

  # prepare admin columns that exist
  admin_present <- admin_cols[admin_cols %in% base::names(data)]

  # determine which indicators to use for activeness
  activeness_indicators <- if (!base::is.null(key_indicators)) {
    key_indicators[key_indicators %in% base::names(data)]
  } else {
    indicators
  }

  if (base::length(activeness_indicators) == 0) {
    if (verbose) {
      cli::cli_warn("No valid indicators found for activeness check. Skipping.")
    }
    row <- tibble::tibble(
      check = "HF Activeness",
      issues_found = "No indicators (0% active, 0% inactive)",
      total_records = "0",
      percent = 0
    )
    return(base::list(
      activeness_over_time = NULL,
      activeness_summary = NULL,
      summary_row = row
    ))
  }

  # build overall facility activeness summary first (more efficient)
  facility_summary <- data |>
    dplyr::mutate(
      has_activeness_data = dplyr::if_any(dplyr::all_of(activeness_indicators), ~ !base::is.na(.x))
    ) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(present_cols, admin_present)))) |>
    dplyr::summarise(
      total_periods = dplyr::n(),
      periods_with_data = base::sum(has_activeness_data),
      reporting_rate = periods_with_data / total_periods,
      first_reported = if (date_col %in% base::names(data)) {
        if (base::any(has_activeness_data)) {
          base::min(.data[[date_col]][has_activeness_data], na.rm = TRUE)
        } else {
          as.Date(NA)
        }
      } else {
        if (base::any(has_activeness_data)) {
          base::min(base::as.character(.data[[yearmon_col]])[has_activeness_data], na.rm = TRUE)
        } else {
          NA_character_
        }
      },
      last_reported = if (date_col %in% base::names(data)) {
        if (base::any(has_activeness_data)) {
          base::max(.data[[date_col]][has_activeness_data], na.rm = TRUE)
        } else {
          as.Date(NA)
        }
      } else {
        if (base::any(has_activeness_data)) {
          base::max(base::as.character(.data[[yearmon_col]])[has_activeness_data], na.rm = TRUE)
        } else {
          NA_character_
        }
      },
      .groups = "drop"
    ) |>
    dplyr::mutate(
      activeness_category = dplyr::case_when(
        reporting_rate == 0 ~ "never_reported",
        reporting_rate < min_reporting_rate ~ "low_reporting",
        reporting_rate < 0.8 ~ "moderate_reporting",
        TRUE ~ "high_reporting"
      )
    ) |>
    dplyr::arrange(activeness_category, dplyr::desc(reporting_rate)) |>
    sntutils::auto_parse_types()

  # identify problematic facilities (never reported or low reporting) for focused over-time table
  problematic_facilities <- facility_summary |>
    dplyr::filter(activeness_category %in% c("never_reported", "low_reporting")) |>
    dplyr::select(dplyr::all_of(present_cols))

  # build focused facility activeness over time table (only problematic facilities)
  if (base::nrow(problematic_facilities) > 0) {
    activeness_over_time <- data |>
      dplyr::semi_join(problematic_facilities, by = present_cols) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(c(present_cols, admin_present, yearmon_col)))) |>
      dplyr::summarise(
        has_data = base::any(dplyr::if_any(dplyr::all_of(activeness_indicators), ~ !base::is.na(.x))),
        reporting_status = dplyr::if_else(has_data, "reported", "not_reported"),
        .groups = "drop"
      ) |>
      dplyr::arrange(dplyr::across(dplyr::all_of(c(present_cols, yearmon_col)))) |>
      sntutils::auto_parse_types()
  } else {
    activeness_over_time <- NULL
  }

  # calculate summary statistics
  n_total <- base::nrow(facility_summary)
  n_never_reported <- base::sum(facility_summary$activeness_category == "never_reported")
  n_low_reporting <- base::sum(facility_summary$activeness_category == "low_reporting")
  n_moderate_reporting <- base::sum(facility_summary$activeness_category == "moderate_reporting")
  n_high_reporting <- base::sum(facility_summary$activeness_category == "high_reporting")

  # calculate percentages
  pct_never <- base::round(n_never_reported / n_total * 100, 1)
  pct_low <- base::round(n_low_reporting / n_total * 100, 1)
  pct_moderate <- base::round(n_moderate_reporting / n_total * 100, 1)
  pct_high <- base::round(n_high_reporting / n_total * 100, 1)
  pct_active <- base::round((n_total - n_never_reported) / n_total * 100, 1)

  # CLI messages
  if (verbose) {
    if (n_never_reported > 0) {
      cli::cli_alert_warning("{sntutils::big_mark(n_never_reported)} facility(ies) never reported ({pct_never}%)")
    }
    if (n_low_reporting > 0) {
      cli::cli_alert_info("{sntutils::big_mark(n_low_reporting)} facility(ies) with low reporting ({pct_low}%)")
    }
    if (n_moderate_reporting > 0) {
      cli::cli_alert_info("{sntutils::big_mark(n_moderate_reporting)} facility(ies) with moderate reporting ({pct_moderate}%)")
    }
    if (n_high_reporting > 0) {
      cli::cli_alert_success("{sntutils::big_mark(n_high_reporting)} facility(ies) with high reporting ({pct_high}%)")
    }

    # summary message
    active_count <- n_total - n_never_reported
    cli::cli_alert_info("Overall: {sntutils::big_mark(active_count)} active ({pct_active}%) vs {sntutils::big_mark(n_never_reported)} inactive ({pct_never}%)")
  }

  # summary row with proportions
  row <- tibble::tibble(
    check = "HF Activeness",
    issues_found = base::paste0(pct_active, "% active, ", pct_never, "% inactive"),
    total_records = base::as.character(n_total),
    percent = pct_never
  )

  base::list(
    activeness_over_time = activeness_over_time,
    activeness_summary = facility_summary,
    summary_row = row
  )
}
