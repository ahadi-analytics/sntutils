#'
#' Orchestrates a suite of validation checks on routine HF data. It standardizes
#' column resolution, selects indicators, runs missing/duplicate/future/logic/
#' outlier checks, compiles a summary, and optionally translates and saves.
#'
#' @param data data.frame. Routine facility dataset.
#' @param id_col character. Unique record id. Default "record_id".
#' @param facility_col character. Facility id. Default "hf_uid".
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
#' @param missing_vars character|NULL. Additional variables to check for missing data
#'   beyond core_id_cols. Core ID columns are always checked. If NULL, checks both
#'   core_id_cols and indicators (default behavior). Use this to limit missing data
#'   checks to specific indicators while always including core IDs.
#' @param consistency_pairs list|NULL. Each element is list(input=, output=).
#'   If NULL, defaults generated from common malaria cascade rules.
#' @param outlier_pairs list|NULL. Pairs for outlier detection and correction.
#'   Structure: list(input = c("test"), output = c("conf")).
#'   Uses same format as consistency_pairs. The cascade rule is: input >= output.
#'   Both directions are checked automatically:
#'   (1) output outliers validated as output <= input,
#'   (2) input outliers validated as input >= output.
#'   Corrections use neighbor median. If NULL, defaults from malaria cascade.
#' @param check_future_dates logical. Default TRUE.
#' @param check_duplicates logical. Default TRUE.
#' @param check_outliers logical. Default TRUE.
#' @param check_facility_activeness logical. Check facility activeness. Default TRUE.
#' @param hf_name_col character. Facility name column. Default "hf".
#' @param key_indicators character|NULL. Key indicators for activeness check.
#'   If NULL, uses all indicators. Default NULL.
#' @param nonreport_window integer. Minimum number of consecutive non-reporting
#'   months to classify a facility as inactive in method 3. Defaults to 6.
#' @param reporting_rule character. Defines what counts as reporting for activeness:
#'   "any_non_na" (default, counts NA as non-reporting, 0 counts as reported)
#'   or "positive_only" (requires >0 value to count as reported).
#' @param min_reporting_rate numeric. Minimum reporting rate threshold. Default 0.5.
#' @param outlier_methods character. Any of c("iqr","median","mean").
#' @param time_mode character. Time mode for outlier detection: "across_time" or "within_year". Default "across_time".
#' @param outlier_strictness character. Outlier detection strictness: "balanced", "lenient", "strict", "advanced". Default "balanced".
#' @param sd_multiplier numeric. Standard deviation multiplier for outlier detection. Default 3.
#' @param mad_constant numeric. MAD constant for outlier detection. Default 1.4826.
#' @param mad_multiplier numeric. MAD multiplier for outlier detection. Default 9.
#' @param iqr_multiplier numeric. IQR multiplier for outlier detection. Default 2.
#' @param min_n numeric. Minimum sample size for outlier detection. Default 8.
#' @param consensus_rule numeric. Number of methods that must agree for consensus outlier flag. Default 1.
#' @param n_neighbour_impute integer. Number of neighboring time periods (before and after) to use for computing the imputation median. Default 3 (uses 3 before + 3 after = 6 values).
#' @param output_path character|NULL. If provided, results are saved to this path.
#' @param output_name character. Base output name. Default
#'   "validation_routine_data_results".
#' @param output_formats character. Any of c("xlsx","rds"). Default
#'   c("xlsx","rds").
#' @param build_dictionary logical. Build data dictionary. Default TRUE.
#' @param verbose logical. CLI messages. Default TRUE.
#' @param language character. ISO-639-1 language code for output labels ("en", "fr", "pt"). Default "en".
#'
#' @return invisible named list with elements:
#'   Summary, Missing values, Missing values detail, Duplicate records, Future dates,
#'   Consistency summary, Consistency details,
#'   Outlier summary, Outlier detailed,
#'   HF activeness summary, HF activeness episodes, Data dictionary
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
  facility_col = "hf_uid",
  date_col = "date",
  yearmon_col = "yearmon",
  year_col = "year",
  month_col = "month",
  admin_cols = c("adm0", "adm1", "adm2", "adm3"),
  admin_guid_cols = c("adm0_guid", "adm1_guid", "adm2_guid", "adm3_guid"),
  core_id_cols = NULL,
  indicators = NULL,
  missing_vars = NULL,
  consistency_pairs = NULL,
  outlier_pairs = NULL,
  check_future_dates = TRUE,
  check_duplicates = TRUE,
  check_outliers = TRUE,
  check_facility_activeness = TRUE,
  hf_name_col = "hf",
  key_indicators = NULL,
  nonreport_window = 6,
  reporting_rule = "any_non_na",
  min_reporting_rate = 0.5,
  outlier_methods = c("iqr", "median", "mean", "consensus"),
  time_mode = "across_time",
  outlier_strictness = "balanced",
  sd_multiplier = 3,
  mad_constant = 1.4826,
  mad_multiplier = 9,
  iqr_multiplier = 2,
  min_n = 8,
  consensus_rule = 1,
  n_neighbour_impute = 3,
  output_path = NULL,
  output_name = "validation_of_hf_routine_data",
  output_formats = c("xlsx", "rds"),
  build_dictionary = FALSE,
  verbose = TRUE,
  language = "en"
) {
  # validate args early and fail fast
  .validate_args(
    data = data,
    output_path = output_path,
    outlier_methods = outlier_methods,
    time_mode = time_mode,
    outlier_strictness = outlier_strictness
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

  # build outlier pairs for correction
  outlier_pairs_use <- .build_outlier_pairs(
    data = data,
    outlier_pairs = outlier_pairs
  )

  # prepare empty results container
  results <- base::list(
    Summary = tibble::tibble(
      check = character(),
      issues_found = character(),
      total_records = character(),
      percent = numeric()
    ),
    "HF activeness summary" = NULL,
    "Missing values" = NULL,
    "Missing values detail" = NULL,
    "Duplicate records" = NULL,
    "Future dates" = NULL,
    "Consistency summary" = NULL,
    "Consistency details" = NULL,
    "Outlier summary" = NULL,
    "Outlier detailed" = NULL,
    "Data dictionary" = NULL
  )

  # check 1: missing values
  mv <- .check_missing(
    data = data,
    id_col = cols$id_col,
    facility_col = cols$facility_col,
    hf_name_col = hf_name_col,
    date_col = cols$date_col,
    year_col = cols$year_col,
    admin_cols = cols$admin_cols,
    core_id_cols = cols$core_id_cols,
    indicators = indicators_use,
    missing_vars = missing_vars,
    verbose = verbose
  )
  results[["Missing values"]] <- mv$missing_table
  results[["Missing values detail"]] <- mv$missing_detail
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
      language = language,
      verbose = verbose,
      hf_name_col = hf_name_col
    )
    results[["Consistency summary"]] <- cons$summary
    results[["Consistency details"]] <- cons$details
    results$Summary <- dplyr::bind_rows(results$Summary, cons$summary_row)
  }

  # check 5: outliers with correction
  if (check_outliers && base::length(outlier_pairs_use) > 0) {
    out <- .check_outliers_with_correction(
      data = data,
      id_col = cols$id_col,
      facility_col = cols$facility_col,
      date_col = cols$date_col,
      hf_name_col = hf_name_col,
      adm_cols = cols$admin_cols,
      outlier_pairs = outlier_pairs_use,
      methods = outlier_methods,
      time_mode = time_mode,
      strictness = outlier_strictness,
      sd_multiplier = sd_multiplier,
      mad_constant = mad_constant,
      mad_multiplier = mad_multiplier,
      iqr_multiplier = iqr_multiplier,
      min_n = min_n,
      consensus_rule = consensus_rule,
      n_neighbour_impute = n_neighbour_impute,
      verbose = verbose
    )
    # always include outlier summary, only include detail if outliers were found
    results[["Outlier summary"]] <- out$summary
    if (base::nrow(out$detail) > 0) {
      results[["Outlier detailed"]] <- out$detail
    }
    results$Summary <- dplyr::bind_rows(results$Summary, out$summary_row)
  }

  # check 6: facility activeness
  if (check_facility_activeness) {
    act <- .check_facility_activeness(
      data = data,
      facility_col = cols$facility_col,
      date_col = cols$date_col,
      hf_name_col = hf_name_col,
      admin_cols = cols$admin_cols,
      indicators = indicators_use,
      key_indicators = key_indicators,
      nonreport_window = nonreport_window,
      reporting_rule = reporting_rule,
      min_reporting_rate = min_reporting_rate,
      language = language,
      verbose = verbose
    )
    results[["HF activeness summary"]] <- act$activeness_summary
    results$Summary <- dplyr::bind_rows(results$Summary, act$summary_row)
  }

  # build data dictionary (optional)
  if (build_dictionary) {
    if (verbose) {
      cli::cli_h2("Building Data Dictionary")
    }

    dict <- .build_validation_dictionary(
      results = results,
      language = language
    )

    results[["Data dictionary"]] <- dict

    if (verbose) {
      n_vars <- sntutils::big_mark(base::nrow(dict))
      n_tabs <- base::length(base::unique(base::unlist(
        base::strsplit(base::as.character(dict$appears_in_tabs), ", ")
      )))
      cli::cli_alert_success(
        "Dictionary created: {n_vars} variable(s) across {n_tabs} tab(s)"
      )
    }
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
  if (!base::is.null(output_path)) {
    # translate only sheet/tab names and selected headers
    res_translated <- .translate_results(
      results = results,
      language = language
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
#' @param output_path character|NULL.
#' @param outlier_methods character.
#' @param time_mode character.
#' @param outlier_strictness character.
#' @keywords internal
#' @noRd
.validate_args <- function(data,
                           output_path,
                           outlier_methods,
                           time_mode,
                           outlier_strictness) {
  # check data frame
  if (!base::is.data.frame(data)) {
    cli::cli_abort("`data` must be a data frame.")
  }

  # ensure non-empty
  if (base::nrow(data) == 0L) {
    cli::cli_abort("`data` has zero rows.")
  }


  # validate methods content
  allowed_methods <- c("iqr", "median", "mean", "consensus")
  bad_methods <- setdiff(outlier_methods, allowed_methods)
  if (length(bad_methods) > 0) {
    cli::cli_abort("Unsupported `outlier_methods`: {bad_methods}.")
  }

  # validate time_mode
  allowed_time_modes <- c("across_time", "within_year")
  if (!time_mode %in% allowed_time_modes) {
    cli::cli_abort("`time_mode` must be one of: {allowed_time_modes}.")
  }

  # validate outlier_strictness
  allowed_strictness <- c("balanced", "lenient", "strict", "advanced")
  if (!outlier_strictness %in% allowed_strictness) {
    cli::cli_abort("`outlier_strictness` must be one of: {allowed_strictness}.")
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

  indicators
}

#' Build default or user-specified consistency pairs
#'
#' @keywords internal
#' @noRd
.build_consistency_pairs <- function(data, consistency_pairs) {
  # if user supplied, normalise then filter to present columns
  if (!base::is.null(consistency_pairs)) {
    # case 1: user passed list(input = <vec>, output = <vec>)
    # and not already a list-of-lists
    if (
      !base::is.null(consistency_pairs$input) &&
        !base::is.null(consistency_pairs$output) &&
        !base::is.list(consistency_pairs$input) &&
        !base::is.list(consistency_pairs$output)
    ) {
      n_in <- base::length(consistency_pairs$input)
      n_out <- base::length(consistency_pairs$output)

      if (n_in != n_out) {
        cli::cli_abort(
          "`consistency_pairs$input` and `consistency_pairs$output` " |>
            base::paste("must have the same length.")
        )
      }

      # convert to internal list-of-lists format
      cp <- base::Map(
        f = function(i, o) base::list(input = i, output = o),
        consistency_pairs$input,
        consistency_pairs$output
      )
    }
    # case 2: list(c(...), c(...)) - unnamed list of 2 character vectors
    else if (
      base::length(consistency_pairs) == 2 &&
        base::is.character(consistency_pairs[[1]]) &&
        base::is.character(consistency_pairs[[2]])
    ) {
      n1 <- base::length(consistency_pairs[[1]])
      n2 <- base::length(consistency_pairs[[2]])

      if (n1 != n2) {
        cli::cli_abort(
          "consistency_pairs vectors must have the same length."
        )
      }

      # treat first vector as input, second as output
      cp <- base::Map(
        f = function(i, o) base::list(input = i, output = o),
        consistency_pairs[[1]],
        consistency_pairs[[2]]
      )
    } else {
      # case 3: already a list of list(input=, output=)
      cp <- consistency_pairs
    }

    keep <- base::Filter(
      f = function(p) {
        # be defensive: skip anything that is not a list with both fields
        if (!base::is.list(p)) {
          return(FALSE)
        }
        if (base::is.null(p$input) || base::is.null(p$output)) {
          return(FALSE)
        }
        p$input %in% base::names(data) && p$output %in% base::names(data)
      },
      x = cp
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

  suf <- c(
    "_u5",
    "_ov5",
    "_preg",
    "_mic_u5",
    "_mic_ov5",
    "_mic_preg",
    "_rdt_u5",
    "_rdt_ov5",
    "_rdt_preg",
    "_preg_u3m",
    "_preg_ov3m"
  )

  pairs <- base_rules

  for (r in base_rules) {
    for (s in suf) {
      i <- base::paste0(r$input, s)
      o <- base::paste0(r$output, s)
      if (i %in% base::names(data) && o %in% base::names(data)) {
        pairs[[base::length(pairs) + 1L]] <- base::list(
          input = i,
          output = o
        )
      }
    }
  }

  base::Filter(
    f = function(p) {
      p$input %in% base::names(data) && p$output %in% base::names(data)
    },
    x = pairs
  )
}

#' Check missing values on core ids and indicators
#'
#' @keywords internal
#' @noRd
.check_missing <- function(data,
                           id_col,
                           facility_col,
                           hf_name_col,
                           date_col,
                           year_col,
                           admin_cols,
                           core_id_cols,
                           indicators,
                           missing_vars,
                           verbose) {
  # determine which columns to check
  # always include core_id_cols but exclude admin_guid_cols
  core_cols_for_missing <- core_id_cols[!grepl("_guid$", core_id_cols)]

  if (!base::is.null(missing_vars)) {
    # user-specified columns PLUS core ids (excluding guid cols)
    cols <- base::unique(base::c(core_cols_for_missing, missing_vars))
    cols <- cols[cols %in% names(data)]
  } else {
    # default: union of core ids (excluding guid cols) and indicators
    cols <- base::unique(base::c(core_cols_for_missing, indicators))
    cols <- cols[cols %in% names(data)]
  }

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
        missing_detail = NULL,
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

  # build missing values summary by admin-year-variable (INDICATORS ONLY, NO HF)
  # determine period column - use year, not yearmon
  if ("year" %in% base::names(data)) {
    period_col <- "year"
  } else if (year_col %in% base::names(data)) {
    period_col <- year_col
  } else {
    period_col <- NULL
  }

  # determine which variables to check in detail (only missing_vars or indicators, NOT core IDs)
  vars_for_detail <- if (!base::is.null(missing_vars)) {
    missing_vars[missing_vars %in% base::names(data)]
  } else {
    indicators[indicators %in% base::names(data)]
  }

  # only create detail table if we have variables to check and a period column
  if (base::length(vars_for_detail) > 0 && !base::is.null(period_col)) {
    # define grouping columns (admin hierarchy + year, NO facility)
    group_cols <- c("adm0", "adm1", "adm2", "adm3", period_col)
    group_cols <- group_cols[group_cols %in% base::names(data)]

    # calculate missing proportions for each variable
    missing_summary_list <- base::list()

    for (var in vars_for_detail) {
      if (var %in% base::names(data)) {
        var_summary <- data |>
          dplyr::select(dplyr::all_of(c(group_cols, var))) |>
          dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
          dplyr::summarise(
            variable = var,
            n_missing = base::sum(base::is.na(.data[[var]])),
            n_total = dplyr::n(),
            prop_missing = n_missing / n_total,
            .groups = "drop"
          ) |>
          dplyr::filter(n_missing > 0)  # only keep rows with missing data

        if (base::nrow(var_summary) > 0) {
          missing_summary_list[[base::length(missing_summary_list) + 1]] <- var_summary
        }
      }
    }

    # combine all summaries
    if (base::length(missing_summary_list) > 0) {
      missing_detail <- dplyr::bind_rows(missing_summary_list) |>
        dplyr::arrange(dplyr::desc(prop_missing), variable) |>
        sntutils::auto_parse_types()

      # convert character to factor to save space
      char_cols <- base::names(missing_detail)[
        base::sapply(missing_detail, base::is.character)
      ]
      for (col in char_cols) {
        missing_detail[[col]] <- base::as.factor(missing_detail[[col]])
      }
    } else {
      missing_detail <- NULL
    }
  } else {
    missing_detail <- NULL
  }

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

      # print detail summary
      if (!base::is.null(missing_detail)) {
        n_detail_rows <- sntutils::big_mark(base::nrow(missing_detail))
        cli::cli_alert_info(
          "Detailed missing data: {n_detail_rows} row(s) with missing values"
        )
      }
    } else {
      cli::cli_alert_success("No missing values in core ids or indicators")
    }
  }

  base::list(
    missing_table = tab,
    missing_detail = missing_detail,
    summary_rows = rows
  )
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
.check_consistency <- function(data, core_id_cols, pairs, language, verbose, hf_name_col = "hf") {
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
        ) |> round(digits = 2),
        difference_prop = dplyr::if_else(
          input_value > 0,
          base::round((difference / input_value) * 100, 2),
          NA_real_
        )
      )

    # details subset with available ids
    ids_keep <- core_id_cols[core_id_cols %in% names(data)]
    # explicitly include hf_name_col if it exists
    if (hf_name_col %in% names(data) && !hf_name_col %in% ids_keep) {
      ids_keep <- c(ids_keep, hf_name_col)
    }
    detail <- pd |>
      dplyr::select(
        dplyr::all_of(ids_keep),
        input_indicator, output_indicator,
        input_value, output_value, difference, difference_prop, difference_sd, is_inconsistent
      )
    all_details[[length(all_details) + 1]] <- detail

    # failures
    fails <- pd |> dplyr::filter(is_inconsistent)
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

  # build pair-level summary
  summary_table <- NULL
  if (length(all_details) > 0) {
    summary_list <- base::list()

    for (detail_df in all_details) {
      pair_summary <- detail_df |>
        dplyr::filter(is_inconsistent) |>
        dplyr::summarise(
          input_indicator = base::unique(input_indicator)[1],
          output_indicator = base::unique(output_indicator)[1],
          input_value = base::sum(input_value, na.rm = TRUE),
          output_value = base::sum(output_value, na.rm = TRUE),
          difference = base::sum(difference, na.rm = TRUE),
          difference_prop = dplyr::if_else(
            input_value > 0,
            base::round((difference / input_value) * 100, 2),
            NA_real_
          ),
          difference_sd = base::mean(difference_sd, na.rm = TRUE) |>
            base::round(digits = 2)
        )

      # only include if we have actual data (not just empty aggregation)
      if (base::nrow(pair_summary) > 0 &&
          !base::is.na(pair_summary$input_indicator[1]) &&
          !base::is.na(pair_summary$output_indicator[1])) {
        summary_list[[length(summary_list) + 1]] <- pair_summary
      }
    }

    if (length(summary_list) > 0) {
      summary_table <- dplyr::bind_rows(summary_list) |>
        dplyr::arrange(dplyr::desc(difference)) |>
        sntutils::auto_parse_types()

      # apply dictionary labels to summary table
      label_col <- base::paste0("label_", language)

      # extract unique column names from pairs
      pair_cols <- base::unique(base::as.vector(base::unlist(
        base::lapply(pairs, function(p) base::c(p$input, p$output))
      )))

      data_dict <- sntutils::build_dictionary(
        data = data |> dplyr::select(dplyr::any_of(pair_cols)),
        language = language, verbose = FALSE
      )

      # join input variable labels
      if (label_col %in% base::names(data_dict)) {
        input_labels <- data_dict |>
          dplyr::select(variable, input_variable = dplyr::all_of(label_col))
        output_labels <- data_dict |>
          dplyr::select(variable, output_variable = dplyr::all_of(label_col))

        summary_table <- summary_table |>
          dplyr::left_join(input_labels, by = c("input_indicator" = "variable")) |>
          dplyr::left_join(output_labels, by = c("output_indicator" = "variable")) |>
          dplyr::select(
            input_indicator,
            input_variable,
            output_indicator,
            output_variable,
            input_value,
            output_value,
            difference,
            difference_prop,
            difference_sd
          )
      }
    }
  }

  # aggregate details: keep inconsistent rows with stable columns
  details <- NULL
  if (length(all_details) > 0) {
    keep_cols <- c(
      "record_id", "date", "adm0", "adm1", "adm2", "adm3", "hf", "hf_uid",
      "input_indicator", "output_indicator",
      "input_value", "output_value", "difference", "difference_prop", "difference_sd"
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
    summary = summary_table,
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
                             time_mode,
                             strictness,
                             sd_multiplier,
                             mad_constant,
                             mad_multiplier,
                             iqr_multiplier,
                             min_n,
                             consensus_rule,
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
          admin_level = admin_use,
          date = date_col,
          time_mode = time_mode,
          value_type = value_type,
          strictness = strictness,
          sd_multiplier = sd_multiplier,
          mad_constant = mad_constant,
          mad_multiplier = mad_multiplier,
          iqr_multiplier = iqr_multiplier,
          min_n = min_n,
          methods = methods,
          consensus_rule = consensus_rule,
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
          # slim down to essential columns
          dplyr::select(dplyr::any_of(c(
            id_col, "record_id", date_col, "date",
            "adm0", "adm1", "adm2", "adm3",
            "value", "indicator_source",
            flags
          ))) |>
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

#' Translate indicator names to labels using dictionaries
#'
#' @keywords internal
#' @noRd
.translate_indicators <- function(indicators, language = "en") {
  if (base::length(indicators) == 0) {
    return(character(0))
  }

  # load validation_terms dictionary
  data("validation_terms", package = "sntutils", envir = environment())

  # batch lookup ALL indicators in snt_var_tree
  snt_results <- .match_snt_labels(indicators, target_lang = language)

  # build labels
  labels <- base::character(base::length(indicators))

  for (i in base::seq_along(indicators)) {
    var_name <- indicators[i]

    # check validation_terms first
    if (var_name %in% base::names(validation_terms)) {
      lang_label <- validation_terms[[var_name]][[language]]
      if (!base::is.null(lang_label)) {
        labels[i] <- lang_label
      } else {
        labels[i] <- validation_terms[[var_name]]$en  # fallback to English
      }
    } else {
      # lookup in batched snt results
      snt_match <- snt_results[snt_results$variable == var_name, ]
      if (base::nrow(snt_match) == 1 && !base::is.na(snt_match$label) && snt_match$label != var_name) {
        labels[i] <- snt_match$label
      } else {
        # fallback to title case
        labels[i] <- tools::toTitleCase(base::gsub("_", " ", var_name))
      }
    }
  }

  labels
}

#' Translate select components for Excel sheet names and headers
#'
#' @keywords internal
#' @noRd
.translate_results <- function(results,
                               language = "en") {
  # helper to filter empty tabs and flatten nested lists
  filter_and_flatten <- function(res, tr_func = base::identity) {
    filtered_out <- base::list()

    for (nm in base::names(res)) {
      # Always keep Summary
      if (nm == tr_func("Summary") || nm == "Summary") {
        filtered_out[[nm]] <- res[[nm]]
        next
      }

      content <- res[[nm]]

      if (!base::is.null(content)) {
        # For data frames/tibbles, check if they have rows
        if (base::is.data.frame(content) && base::nrow(content) > 0) {
          filtered_out[[nm]] <- content
        }
        # For lists (like Consistency failures), flatten into a single tibble
        else if (base::is.list(content) && !base::is.data.frame(content)) {
          tibbles_to_bind <- base::list()
          for (item in content) {
            if (!base::is.null(item) && base::is.data.frame(item) &&
                base::nrow(item) > 0) {
              tibbles_to_bind[[base::length(tibbles_to_bind) + 1L]] <- item
            }
          }
          if (base::length(tibbles_to_bind) > 0) {
            filtered_out[[nm]] <- dplyr::bind_rows(tibbles_to_bind)
          }
        }
      }
    }
    filtered_out
  }

  # no-op if English - still filter/flatten but don't translate

  if (language == "en") {
    filtered <- filter_and_flatten(results)
    return(.ensure_utf8(filtered))
  }

  # load validation_terms dictionary

  data("validation_terms", package = "sntutils", envir = environment())

  # small helper to translate using dictionaries only (no API)
  tr <- function(x) {
    key <- base::tolower(base::gsub(" ", "_", x))

    if (key %in% base::names(validation_terms)) {
      lang_label <- validation_terms[[key]][[language]]
      if (!base::is.null(lang_label)) {
        return(lang_label)
      }
    }

    snt_result <- .match_snt_labels(key, target_lang = language)
    if (base::nrow(snt_result) == 1 && !base::is.na(snt_result$label) &&
        snt_result$label != key) {
      return(snt_result$label)
    }

    x
  }

  # copy and translate names
  out <- results
  base::names(out) <- base::sapply(base::names(out), tr)

  # filter/flatten with translation function
  filtered_out <- filter_and_flatten(out, tr)

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
                                       facility_col,
                                       date_col,
                                       hf_name_col,
                                       admin_cols,
                                       indicators,
                                       key_indicators,
                                       nonreport_window,
                                       reporting_rule,
                                       min_reporting_rate,
                                       language,
                                       verbose) {
  # check if required columns exist
  required_cols <- c(hf_name_col, facility_col)
  present_cols <- required_cols[required_cols %in% base::names(data)]

  if (base::length(present_cols) == 0) {
    if (verbose) {
      cli::cli_h2("Check 6: Facility Activeness")
      cli::cli_warn("Required columns ({hf_name_col}, {facility_col}) not found. Skipping facility activeness check.")
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

  # run all 3 classification methods
  classification_m1 <- sntutils::classify_facility_activity(
    data = data,
    hf_col = facility_col,
    date_col = date_col,
    key_indicators = activeness_indicators,
    method = 1,
    nonreport_window = nonreport_window,
    reporting_rule = reporting_rule,
    binary_classification = FALSE
  )

  classification_m2 <- sntutils::classify_facility_activity(
    data = data,
    hf_col = facility_col,
    date_col = date_col,
    key_indicators = activeness_indicators,
    method = 2,
    nonreport_window = nonreport_window,
    reporting_rule = reporting_rule,
    binary_classification = FALSE
  )

  classification_m3 <- sntutils::classify_facility_activity(
    data = data,
    hf_col = facility_col,
    date_col = date_col,
    key_indicators = activeness_indicators,
    method = 3,
    nonreport_window = nonreport_window,
    reporting_rule = reporting_rule,
    binary_classification = FALSE
  )

  # build activity episodes for each method and stack
  episodes_m1 <- .build_activity_episodes(
    data = classification_m1,
    hf_name_col = hf_name_col,
    facility_col = facility_col,
    date_col = date_col,
    admin_cols = c("adm0", "adm1", "adm2", "adm3"),
    method_label = 1L,
    language = language
  )

  episodes_m2 <- .build_activity_episodes(
    data = classification_m2,
    hf_name_col = hf_name_col,
    facility_col = facility_col,
    date_col = date_col,
    admin_cols = c("adm0", "adm1", "adm2", "adm3"),
    method_label = 2L,
    language = language
  )

  episodes_m3 <- .build_activity_episodes(
    data = classification_m3,
    hf_name_col = hf_name_col,
    facility_col = facility_col,
    date_col = date_col,
    admin_cols = c("adm0", "adm1", "adm2", "adm3"),
    method_label = 3L,
    language = language
  )

  # stack all methods vertically
  activeness_episodes <- dplyr::bind_rows(episodes_m1, episodes_m2, episodes_m3)

  # reorder columns: admin cols, hf, hf_uid, method, activity_status, dates
  col_order <- c(
    admin_present, hf_name_col, facility_col, "method",
    "activity_status", "start_date", "end_date"
  )
  col_order <- col_order[col_order %in% base::names(activeness_episodes)]
  activeness_episodes <- activeness_episodes |>
    dplyr::select(dplyr::all_of(col_order)) |>
    dplyr::arrange(
      method,
      dplyr::across(dplyr::all_of(admin_present)),
      .data[[hf_name_col]],
      .data[[facility_col]],
      start_date
    )

  # calculate summary statistics from method 3 for CLI output
  facility_stats <- classification_m3 |>
    dplyr::group_by(.data[[facility_col]]) |>
    dplyr::summarise(
      n_active = base::sum(
        activity_status == "Active Reporting", na.rm = TRUE
      ),
      total_periods = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      reporting_rate = n_active / total_periods,
      activeness_category = dplyr::case_when(
        reporting_rate == 0 ~ "never_reported",
        reporting_rate < min_reporting_rate ~ "low_reporting",
        reporting_rate < 0.8 ~ "moderate_reporting",
        TRUE ~ "high_reporting"
      )
    )

  n_total <- base::nrow(facility_stats)
  n_never_reported <- base::sum(
    facility_stats$activeness_category == "never_reported"
  )
  n_low_reporting <- base::sum(
    facility_stats$activeness_category == "low_reporting"
  )
  n_moderate_reporting <- base::sum(
    facility_stats$activeness_category == "moderate_reporting"
  )
  n_high_reporting <- base::sum(
    facility_stats$activeness_category == "high_reporting"
  )

  # calculate percentages
  pct_never <- base::round(n_never_reported / n_total * 100, 1)
  pct_low <- base::round(n_low_reporting / n_total * 100, 1)
  pct_moderate <- base::round(n_moderate_reporting / n_total * 100, 1)
  pct_high <- base::round(n_high_reporting / n_total * 100, 1)
  pct_active <- base::round((n_total - n_never_reported) / n_total * 100, 1)

  # CLI messages
  if (verbose) {
    if (n_never_reported > 0) {
      cli::cli_alert_warning(
        "{sntutils::big_mark(n_never_reported)} facility(ies) never reported ({pct_never}%)"
      )
    }
    if (n_low_reporting > 0) {
      cli::cli_alert_info(
        "{sntutils::big_mark(n_low_reporting)} facility(ies) with low reporting ({pct_low}%)"
      )
    }
    if (n_moderate_reporting > 0) {
      cli::cli_alert_info(
        "{sntutils::big_mark(n_moderate_reporting)} facility(ies) with moderate reporting ({pct_moderate}%)"
      )
    }
    if (n_high_reporting > 0) {
      cli::cli_alert_success(
        "{sntutils::big_mark(n_high_reporting)} facility(ies) with high reporting ({pct_high}%)"
      )
    }

    active_count <- n_total - n_never_reported
    cli::cli_alert_info(
      "Overall: {sntutils::big_mark(active_count)} active ({pct_active}%) vs {sntutils::big_mark(n_never_reported)} inactive ({pct_never}%)"
    )
  }

  # summary row with proportions
  row <- tibble::tibble(
    check = "HF Activeness",
    issues_found = base::paste0(pct_active, "% active, ", pct_never, "% inactive"),
    total_records = sntutils::big_mark(n_total),
    percent = pct_never
  )

  base::list(
    activeness_episodes = activeness_episodes,
    activeness_summary = activeness_episodes,
    summary_row = row
  )
}

#' Build activity episodes from classified facility data
#'
#' @description
#' Groups consecutive months with the same activity status into episodes.
#' Detects episode transitions when status changes or date gap > 1 month.
#'
#' @keywords internal
#' @noRd
.build_activity_episodes <- function(data,
                                     hf_name_col = "hf",
                                     facility_col = "hf_uid",
                                     date_col = "date",
                                     admin_cols = c("adm0", "adm1", "adm2", "adm3"),
                                     method_label = 1L,
                                     language = "en") {
  # columns to use - include full admin hierarchy

  admin_present <- admin_cols[admin_cols %in% base::names(data)]
  group_cols <- base::c(admin_present, hf_name_col, facility_col)
  group_cols <- group_cols[group_cols %in% base::names(data)]

  if (base::length(group_cols) == 0 || !date_col %in% base::names(data)) {
    return(NULL)
  }

  # build episodes using lag to detect transitions
  episodes <- data |>
    dplyr::select(dplyr::all_of(c(group_cols, "activity_status", date_col))) |>
    dplyr::arrange(dplyr::across(dplyr::all_of(c(group_cols, date_col)))) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
    dplyr::mutate(
      status_lag = dplyr::lag(activity_status),
      date_lag = dplyr::lag(.data[[date_col]]),
      new_episode = dplyr::case_when(
        is.na(status_lag) ~ 1L,
        activity_status != status_lag ~ 1L,
        .data[[date_col]] != date_lag + lubridate::dmonths(1) ~ 1L,
        TRUE ~ 0L
      ),
      episode_id = cumsum(new_episode)
    ) |>
    dplyr::group_by(
      dplyr::across(dplyr::all_of(group_cols)),
      episode_id,
      activity_status
    ) |>
    dplyr::summarise(
      start_date = min(.data[[date_col]]),
      end_date = max(.data[[date_col]]),
      .groups = "drop"
    ) |>
    dplyr::select(-episode_id) |>
    dplyr::mutate(method = method_label)

  # translate status if French
  if (language == "fr") {
    episodes <- episodes |>
      dplyr::mutate(
        activity_status = dplyr::case_when(
          activity_status == "Active Reporting" ~
            "FOSA actif - Rapports transmis",
          activity_status == "Active - Not Reporting" ~
            "FOSA actif - Pas de rapport",
          activity_status == "Inactive" ~
            "FOSA inactif",
          TRUE ~ activity_status
        )
      )
  }

  episodes
}

#' Build data dictionary for validation results
#'
#' @description
#' Uses validation_terms and snt_var_tree dictionaries for translations.
#' No API calls - all translations are predefined.
#'
#' @keywords internal
#' @noRd
.build_validation_dictionary <- function(results,
                                        language = "en") {
  # collect all unique column names with their source tabs
  col_info <- base::list()

  for (tab_name in base::names(results)) {
    tab_data <- results[[tab_name]]

    # skip if not a data.frame
    if (!base::is.data.frame(tab_data)) next

    # extract column names
    cols <- base::names(tab_data)

    for (col in cols) {
      if (base::is.null(col_info[[col]])) {
        col_info[[col]] <- base::list(
          variable = col,
          appears_in = base::character()
        )
      }
      col_info[[col]]$appears_in <- base::c(
        col_info[[col]]$appears_in,
        tab_name
      )
    }
  }

  # build base dictionary tibble
  dict <- tibble::tibble(
    variable = base::as.character(base::names(col_info)),
    appears_in_tabs = base::as.character(base::sapply(col_info, function(x) {
      base::paste(x$appears_in, collapse = ", ")
    }))
  )

  # load dictionaries
  data("validation_terms", package = "sntutils", envir = environment())

  # FILTER: exclude columns that appear ONLY in "Duplicate records" tab
  # this includes input columns when they appear in other validation tabs
  dict <- dict |>
    dplyr::filter(appears_in_tabs != "Duplicate records")

  # if dictionary is empty after filtering, return early
  if (base::nrow(dict) == 0) {
    return(tibble::tibble(
      variable = character(),
      appears_in_tabs = character(),
      label_en = character()
    ))
  }

  # batch lookup ALL variables in snt_var_tree (single call, much faster)
  snt_results_en <- .match_snt_labels(dict$variable, target_lang = "en")

  # build label lookup using pooled dictionaries
  # priority: validation_terms > snt_var_tree > fallback
  labels_en <- base::character(base::nrow(dict))
  for (i in base::seq_len(base::nrow(dict))) {
    var_name <- dict$variable[i]

    # check validation_terms first
    if (var_name %in% base::names(validation_terms)) {
      labels_en[i] <- validation_terms[[var_name]]$en
    } else {
      # lookup in batched snt results
      snt_match <- snt_results_en[snt_results_en$variable == var_name, ]
      if (base::nrow(snt_match) == 1 && !base::is.na(snt_match$label) && snt_match$label != var_name) {
        labels_en[i] <- snt_match$label
      } else {
        # fallback to title case
        labels_en[i] <- tools::toTitleCase(base::gsub("_", " ", var_name))
      }
    }
  }

  dict$label_en <- labels_en

  # add translation if needed (from dictionaries only, no API)
  if (language != "en") {
    # batch lookup ALL variables for target language (single call, much faster)
    snt_results_lang <- .match_snt_labels(dict$variable, target_lang = language)

    labels_target <- base::character(base::nrow(dict))
    for (i in base::seq_len(base::nrow(dict))) {
      var_name <- dict$variable[i]

      # check validation_terms first
      if (var_name %in% base::names(validation_terms)) {
        lang_label <- validation_terms[[var_name]][[language]]
        if (!base::is.null(lang_label)) {
          labels_target[i] <- lang_label
        } else {
          labels_target[i] <- dict$label_en[i]  # fallback to English
        }
      } else {
        # lookup in batched snt results
        snt_match <- snt_results_lang[snt_results_lang$variable == var_name, ]
        if (base::nrow(snt_match) == 1 && !base::is.na(snt_match$label) && snt_match$label != var_name) {
          labels_target[i] <- snt_match$label
        } else {
          labels_target[i] <- dict$label_en[i]  # fallback to English
        }
      }
    }

    col_name <- base::paste0("label_", language)
    dict[[col_name]] <- labels_target
  }

  dict |> sntutils::auto_parse_types()
}

#' Build default or user-specified outlier pairs
#'
#' @description
#' Builds outlier pairs for correction validation.
#' Each pair has: target (variable to check for outliers),
#' consistency (variable to validate correction against),
#' direction ("lte" means target <= consistency).
#'
#' @keywords internal
#' @noRd
.build_outlier_pairs <- function(data, outlier_pairs) {
  # user supplied pairs
  if (!base::is.null(outlier_pairs)) {
    if (base::is.null(outlier_pairs$input) ||
        base::is.null(outlier_pairs$output)) {
      cli::cli_abort(
        "`outlier_pairs` must have `input` and `output` elements."
      )
    }

    n_in <- base::length(outlier_pairs$input)
    n_out <- base::length(outlier_pairs$output)

    if (n_in != n_out) {
      cli::cli_abort(
        "`outlier_pairs$input` and `outlier_pairs$output` must have same length."
      )
    }

    # convert to internal format: generate BOTH directions
    pairs <- base::list()
    for (i in base::seq_len(n_in)) {
      inp <- outlier_pairs$input[i]
      out <- outlier_pairs$output[i]
      if (inp %in% base::names(data) && out %in% base::names(data)) {
        # lte direction: output <= input
        pairs[[base::length(pairs) + 1L]] <- base::list(
          target = out,
          consistency = inp,
          direction = "lte"
        )
        # gte direction: input >= output
        pairs[[base::length(pairs) + 1L]] <- base::list(
          target = inp,
          consistency = out,
          direction = "gte"
        )
      }
    }
    return(pairs)
  }

  # default cascade rules: input >= output
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

  suf <- c(
    "_u5", "_ov5", "_preg",
    "_mic_u5", "_mic_ov5", "_mic_preg",
    "_rdt_u5", "_rdt_ov5", "_rdt_preg",
    "_preg_u3m", "_preg_ov3m"
  )

  # expand with suffixes
  all_rules <- base_rules
  for (r in base_rules) {
    for (s in suf) {
      i <- base::paste0(r$input, s)
      o <- base::paste0(r$output, s)
      if (i %in% base::names(data) && o %in% base::names(data)) {
        all_rules[[base::length(all_rules) + 1L]] <- base::list(
          input = i, output = o
        )
      }
    }
  }

  # convert to internal format: generate BOTH directions for each rule
  # lte: output <= input (check output outliers)
  # gte: input >= output (check input outliers)
  pairs <- base::list()
  for (r in all_rules) {
    if (r$input %in% base::names(data) && r$output %in% base::names(data)) {
      # lte direction: output <= input
      pairs[[base::length(pairs) + 1L]] <- base::list(
        target = r$output,
        consistency = r$input,
        direction = "lte"
      )
      # gte direction: input >= output
      pairs[[base::length(pairs) + 1L]] <- base::list(
        target = r$input,
        consistency = r$output,
        direction = "gte"
      )
    }
  }

  pairs
}

#' Outlier detection with neighbor-median correction
#'
#' @description
#' For each (target, consistency) pair:
#' 1. Detect outliers in target variable
#' 2. Compute neighbor median (lag/lead) as potential correction
#' 3. Validate correction against cascade rule (corrected <= consistency)
#' 4. Only apply correction if it doesn't violate the cascade
#'
#' @keywords internal
#' @noRd
.check_outliers_with_correction <- function(
    data,
    id_col,
    facility_col,
    date_col,
    hf_name_col,
    adm_cols,
    outlier_pairs,
    methods,
    time_mode,
    strictness,
    sd_multiplier,
    mad_constant,
    mad_multiplier,
    iqr_multiplier,
    min_n,
    consensus_rule,
    n_neighbour_impute,
    verbose
) {
  if (verbose) cli::cli_h2("Check 5: Outlier Detection & Correction")

  # metadata columns for detailed output
  meta_cols <- c(
    id_col, "record_id",
    adm_cols,
    hf_name_col, facility_col,
    date_col, "date", "year", "month", "yearmon"
  )
  meta_cols <- base::unique(meta_cols[meta_cols %in% base::names(data)])

  # admin columns for outlier detection grouping
  admin_use <- adm_cols[adm_cols %in% base::names(data)]
  if (base::length(admin_use) > 2) admin_use <- admin_use[seq_len(2)]

  # pre-sort data once
  data_sorted <- data |>
    dplyr::arrange(.data[[facility_col]], .data[[date_col]])

  # collect outlier details
  detail_list <- base::list()
  total_outliers <- 0L
  total_corrected <- 0L
  total_no_neighbours <- 0L
  total_correction_inconsistent <- 0L

  # track processed record IDs per target variable to avoid duplicates
  processed_ids <- base::list()

  # track corrections: corrections[[record_id]][[var]] = corrected_value
  # used to look up corrected consistency values for lte checks

  corrections <- base::list()

  # sort pairs: gte first (input/test), then lte (output/conf)
  # this ensures input corrections are available when checking output
  outlier_pairs <- outlier_pairs[base::order(
    base::sapply(outlier_pairs, function(p) p$direction != "gte")
  )]

  # pre-detect outliers for ALL unique target variables
  # used to skip cascade check when consistency var is also an outlier
  all_target_vars <- base::unique(
    base::sapply(outlier_pairs, function(p) p$target)
  )
  outlier_record_ids <- base::list()
  for (var in all_target_vars) {
    if (!var %in% base::names(data)) next
    detected_pre <- tryCatch(
      {
        sntutils::detect_outliers(
          data = data,
          column = var,
          record_id = id_col,
          admin_level = admin_use,
          date = date_col,
          time_mode = time_mode,
          value_type = "count",
          strictness = strictness,
          sd_multiplier = sd_multiplier,
          mad_constant = mad_constant,
          mad_multiplier = mad_multiplier,
          iqr_multiplier = iqr_multiplier,
          min_n = min_n,
          methods = methods,
          consensus_rule = consensus_rule,
          output_profile = "lean",
          verbose = FALSE
        )
      },
      error = function(e) NULL
    )
    if (!base::is.null(detected_pre) && base::nrow(detected_pre) > 0) {
      flag_col <- "outlier_flag_consensus"
      if (flag_col %in% base::names(detected_pre)) {
        outlier_record_ids[[var]] <- detected_pre |>
          dplyr::filter(.data[[flag_col]] == "outlier") |>
          dplyr::pull(dplyr::all_of(id_col))
      }
    }
  }

  for (p in outlier_pairs) {
    target_var <- p$target
    consistency_var <- p$consistency

    if (!target_var %in% base::names(data)) next
    if (!consistency_var %in% base::names(data)) next

    # detect outliers for target
    detected <- tryCatch(
      {
        sntutils::detect_outliers(
          data = data,
          column = target_var,
          record_id = id_col,
          admin_level = admin_use,
          date = date_col,
          time_mode = time_mode,
          value_type = "count",
          strictness = strictness,
          sd_multiplier = sd_multiplier,
          mad_constant = mad_constant,
          mad_multiplier = mad_multiplier,
          iqr_multiplier = iqr_multiplier,
          min_n = min_n,
          methods = methods,
          consensus_rule = consensus_rule,
          output_profile = "lean",
          verbose = FALSE
        )
      },
      error = function(e) {
        if (verbose) {
          cli::cli_warn("Outlier detection failed for {target_var}: {e$message}")
        }
        NULL
      }
    )

    if (base::is.null(detected) || base::nrow(detected) == 0) next

    flag_col <- "outlier_flag_consensus"
    if (!flag_col %in% base::names(detected)) next

    # join flags and compute corrections
    merged <- data_sorted |>
      dplyr::select(dplyr::all_of(c(meta_cols, target_var, consistency_var))) |>
      dplyr::left_join(
        detected |> dplyr::select(dplyr::all_of(c(id_col, flag_col))),
        by = id_col
      )

    # compute n lag/lead values within facility, filter to outliers only
    # build lag and lead columns dynamically
    merged_with_neighbours <- merged |>
      dplyr::group_by(.data[[facility_col]])

    # add lag columns
    for (i in base::seq_len(n_neighbour_impute)) {
      lag_col <- base::paste0(".lag_", i)
      merged_with_neighbours <- merged_with_neighbours |>
        dplyr::mutate(!!lag_col := dplyr::lag(.data[[target_var]], n = i))
    }

    # add lead columns for target
    for (i in base::seq_len(n_neighbour_impute)) {
      lead_col <- base::paste0(".lead_", i)
      merged_with_neighbours <- merged_with_neighbours |>
        dplyr::mutate(!!lead_col := dplyr::lead(.data[[target_var]], n = i))
    }

    # add lag columns for consistency variable
    for (i in base::seq_len(n_neighbour_impute)) {
      lag_col <- base::paste0(".cons_lag_", i)
      merged_with_neighbours <- merged_with_neighbours |>
        dplyr::mutate(!!lag_col := dplyr::lag(.data[[consistency_var]], n = i))
    }

    # add lead columns for consistency variable
    for (i in base::seq_len(n_neighbour_impute)) {
      lead_col <- base::paste0(".cons_lead_", i)
      merged_with_neighbours <- merged_with_neighbours |>
        dplyr::mutate(!!lead_col := dplyr::lead(.data[[consistency_var]], n = i))
    }

    merged_with_neighbours <- merged_with_neighbours |>
      dplyr::ungroup()

    # filter to outliers and compute neighbor median
    lag_cols <- base::paste0(".lag_", base::seq_len(n_neighbour_impute))
    lead_cols <- base::paste0(".lead_", base::seq_len(n_neighbour_impute))
    cons_lag_cols <- base::paste0(".cons_lag_", base::seq_len(n_neighbour_impute))
    cons_lead_cols <- base::paste0(".cons_lead_", base::seq_len(n_neighbour_impute))

    outlier_rows <- merged_with_neighbours |>
      dplyr::filter(.data[[flag_col]] == "outlier")

    # compute neighbor median row by row (for both target and consistency vars)
    if (base::nrow(outlier_rows) > 0) {
      results <- base::lapply(
        base::seq_len(base::nrow(outlier_rows)),
        function(i) {
          row <- outlier_rows[i, ]

          # target variable neighbors
          lag_vals <- base::as.numeric(base::unlist(row[, lag_cols]))
          lead_vals <- base::as.numeric(base::unlist(row[, lead_cols]))
          all_lags_na <- base::all(base::is.na(lag_vals))
          all_leads_na <- base::all(base::is.na(lead_vals))
          all_vals <- base::c(lag_vals, lead_vals)

          # consistency variable neighbors
          cons_lag_vals <- base::as.numeric(base::unlist(row[, cons_lag_cols]))
          cons_lead_vals <- base::as.numeric(base::unlist(row[, cons_lead_cols]))
          cons_all_vals <- base::c(cons_lag_vals, cons_lead_vals)
          cons_median <- stats::median(cons_all_vals, na.rm = TRUE)
          if (!base::is.na(cons_median)) {
            cons_median <- base::round(cons_median)
          }

          if (all_lags_na || all_leads_na) {
            return(base::list(
              median = NA_real_,
              no_neighbours = TRUE,
              cons_median = cons_median
            ))
          }

          base::list(
            median = base::round(stats::median(all_vals, na.rm = TRUE)),
            no_neighbours = FALSE,
            cons_median = cons_median
          )
        }
      )

      outlier_rows <- outlier_rows |>
        dplyr::mutate(
          neighbor_median = base::sapply(results, function(x) x$median),
          .no_neighbours = base::sapply(results, function(x) x$no_neighbours),
          consistency_median = base::sapply(results, function(x) x$cons_median)
        )
    } else {
      outlier_rows <- outlier_rows |>
        dplyr::mutate(
          neighbor_median = base::numeric(0),
          .no_neighbours = base::logical(0),
          consistency_median = base::numeric(0)
        )
    }

    outlier_rows <- outlier_rows |>
      dplyr::mutate(
        outlier_var = target_var,
        outlier_value = .data[[target_var]],
        consistency_var = consistency_var,
        consistency_value = .data[[consistency_var]]
      ) |>
      dplyr::select(-dplyr::all_of(c(
        lag_cols, lead_cols, cons_lag_cols, cons_lead_cols
      )))

    # check if consistency var is also an outlier for each record
    # if so, skip cascade check (both values unreliable, just apply correction)
    consistency_outlier_ids <- outlier_record_ids[[consistency_var]]
    if (base::is.null(consistency_outlier_ids)) {
      consistency_outlier_ids <- base::character(0)
    }

    # cascade check: compare corrected_target vs corrected_consistency
    # use consistency_median (neighbor median of consistency var)
    outlier_rows <- outlier_rows |>
      dplyr::mutate(
        # skip cascade check if consistency var is also an outlier
        consistency_is_outlier = .data[[id_col]] %in% consistency_outlier_ids,
        # check if correction would violate cascade
        # lte: corrected must be <= consistency_median
        # gte: corrected must be >= consistency_median
        cascade_violated = dplyr::case_when(
          consistency_is_outlier ~ FALSE,
          base::is.na(neighbor_median) ~ NA,
          base::is.na(consistency_median) ~ NA,
          p$direction == "lte" ~ neighbor_median > consistency_median,
          p$direction == "gte" ~ neighbor_median < consistency_median,
          TRUE ~ FALSE
        ),
        # corrected value: use median only if cascade is not violated
        corrected_value = dplyr::case_when(
          .no_neighbours ~ NA_real_,
          base::is.na(consistency_median) ~ neighbor_median,
          cascade_violated ~ NA_real_,
          TRUE ~ neighbor_median
        ),
        correction_flag = dplyr::case_when(
          .no_neighbours ~ FALSE,
          base::is.na(consistency_median) ~ TRUE,
          cascade_violated ~ FALSE,
          TRUE ~ TRUE
        ),
        failed_reason = dplyr::case_when(
          .no_neighbours ~ "no_neighbours",
          !base::is.na(cascade_violated) & cascade_violated ~ "correction_inconsistent",
          TRUE ~ NA_character_
        )
      ) |>
      dplyr::select(-".no_neighbours") |>
      dplyr::select(
        dplyr::all_of(meta_cols),
        outlier_var,
        outlier_value,
        consistency_var,
        consistency_value,
        neighbor_median,
        consistency_median,
        corrected_value,
        correction_flag,
        failed_reason
      )

    # store corrections for use in subsequent lte checks
    if (base::nrow(outlier_rows) > 0) {
      corrected_rows <- outlier_rows |>
        dplyr::filter(correction_flag == TRUE)
      for (i in base::seq_len(base::nrow(corrected_rows))) {
        rec_id <- corrected_rows[[id_col]][i]
        var_name <- corrected_rows$outlier_var[i]
        corr_val <- corrected_rows$corrected_value[i]
        if (base::is.null(corrections[[rec_id]])) {
          corrections[[rec_id]] <- base::list()
        }
        corrections[[rec_id]][[var_name]] <- corr_val
      }
    }

    # filter out records already processed for this target variable
    already_processed <- processed_ids[[target_var]]
    if (!base::is.null(already_processed) && base::length(already_processed) > 0) {
      outlier_rows <- outlier_rows |>
        dplyr::filter(!.data[[id_col]] %in% already_processed)
    }

    # track these record IDs as processed
    if (base::nrow(outlier_rows) > 0) {
      new_ids <- outlier_rows[[id_col]]
      processed_ids[[target_var]] <- base::c(already_processed, new_ids)
      detail_list[[base::length(detail_list) + 1L]] <- outlier_rows
    }

    # count stats
    n_outliers <- base::nrow(outlier_rows)
    n_corrected <- base::sum(outlier_rows$correction_flag, na.rm = TRUE)
    n_no_neighbours <- base::sum(
      outlier_rows$failed_reason == "no_neighbours", na.rm = TRUE
    )
    n_correction_inconsistent <- base::sum(
      outlier_rows$failed_reason == "correction_inconsistent", na.rm = TRUE
    )

    total_outliers <- total_outliers + n_outliers
    total_corrected <- total_corrected + n_corrected
    total_no_neighbours <- total_no_neighbours + n_no_neighbours
    total_correction_inconsistent <- total_correction_inconsistent + n_correction_inconsistent

    if (verbose && n_outliers > 0) {
      cli::cli_alert_info(
        "{target_var}: {sntutils::big_mark(n_outliers)} outlier(s), {sntutils::big_mark(n_corrected)} corrected"
      )
    }
  }

  # build detailed table
  detail_table <- if (base::length(detail_list) > 0) {
    dplyr::bind_rows(detail_list)
  } else {
    tibble::tibble(
      outlier_var = base::character(0),
      outlier_value = base::numeric(0),
      consistency_var = base::character(0),
      consistency_value = base::numeric(0),
      neighbor_median = base::numeric(0),
      consistency_median = base::numeric(0),
      corrected_value = base::numeric(0),
      correction_flag = base::logical(0),
      failed_reason = base::character(0)
    )
  }

  # build summary table (aggregated by admin + year + variable)
  admin_summary_cols <- c("adm0", "adm1", "adm2", "adm3")
  admin_summary_cols <- admin_summary_cols[
    admin_summary_cols %in% base::names(detail_table)
  ]
  summary_group_cols <- c(admin_summary_cols, "year")
  summary_group_cols <- summary_group_cols[
    summary_group_cols %in% base::names(detail_table)
  ]

  if (base::nrow(detail_table) > 0 && base::length(summary_group_cols) > 0) {
    # get total records per group from original data
    totals_by_group <- data |>
      dplyr::group_by(dplyr::across(dplyr::all_of(summary_group_cols))) |>
      dplyr::summarise(n_total = dplyr::n(), .groups = "drop")

    summary_table <- detail_table |>
      dplyr::group_by(
        dplyr::across(dplyr::all_of(c(summary_group_cols, "outlier_var")))
      ) |>
      dplyr::summarise(
        n_outliers = dplyr::n(),
        n_corrected = base::sum(correction_flag, na.rm = TRUE),
        n_no_neighbours = base::sum(
          failed_reason == "no_neighbours", na.rm = TRUE
        ),
        n_correction_inconsistent = base::sum(
          failed_reason == "correction_inconsistent", na.rm = TRUE
        ),
        .groups = "drop"
      ) |>
      dplyr::left_join(totals_by_group, by = summary_group_cols) |>
      dplyr::mutate(
        prop_outliers = base::round(n_outliers / n_total, 4)
      ) |>
      dplyr::rename(variable = outlier_var) |>
      dplyr::select(
        dplyr::all_of(summary_group_cols),
        variable,
        n_total,
        n_outliers,
        prop_outliers,
        n_corrected,
        n_no_neighbours,
        n_correction_inconsistent
      ) |>
      dplyr::arrange(dplyr::desc(prop_outliers), variable)
  } else {
    summary_table <- tibble::tibble(
      variable = base::character(0),
      n_total = base::integer(0),
      n_outliers = base::integer(0),
      prop_outliers = base::numeric(0),
      n_corrected = base::integer(0),
      n_no_neighbours = base::integer(0),
      n_correction_inconsistent = base::integer(0)
    )
  }

  # summary row for main validation summary
  pct_outliers <- base::round(total_outliers / base::nrow(data) * 100, 2)
  summary_row <- tibble::tibble(
    check = "Outliers & Corrections",
    issues_found = base::paste0(
      sntutils::big_mark(total_outliers), " outliers (",
      sntutils::big_mark(total_corrected), " corrected)"
    ),
    total_records = base::as.character(sntutils::big_mark(base::nrow(data))),
    percent = pct_outliers
  )

  if (verbose) {
    total_failed <- total_no_neighbours + total_correction_inconsistent
    cli::cli_alert_success(
      "Outlier check complete: {sntutils::big_mark(total_outliers)} outlier(s), {sntutils::big_mark(total_corrected)} corrected, {sntutils::big_mark(total_failed)} failed"
    )
    if (total_failed > 0) {
      cli::cli_alert_info(
        "Failures: {sntutils::big_mark(total_no_neighbours)} no neighbours, {sntutils::big_mark(total_correction_inconsistent)} cascade violation"
      )
    }
  }

  base::list(
    detail = detail_table,
    summary = summary_table,
    summary_row = summary_row
  )
}
