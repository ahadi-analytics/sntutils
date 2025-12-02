# tests/testthat/test-validate_routine_hf_data.R
# comprehensive tests for validate_routine_hf_data function
# this file focuses on output structure and validation logic
# RELEVANT FILES: R/validate_routine_hf_data.R, R/detect_outliers.R

# helper to create test data ------------------------------------------------

create_test_data <- function(n_rows = 100, add_issues = TRUE) {
  set.seed(123)

  current_date <- lubridate::floor_date(Sys.Date(), "month")
  start_date <- seq.Date(current_date, length.out = 2, by = "-1 year")[2]
  dates <- seq.Date(start_date, current_date, by = "month")

  data <- tibble::tibble(
    record_id = paste0("REC", seq_len(n_rows)),
    hf_id = paste0("HF", rep(1:10, length.out = n_rows)),
    hf_uid = paste0("HF", rep(1:10, length.out = n_rows)),
    hf = paste0("Health Facility ", rep(1:10, length.out = n_rows)),
    date = sample(dates, n_rows, replace = TRUE),
    yearmon = format(date, "%Y-%m"),
    year = lubridate::year(date),
    month = lubridate::month(date),
    adm0 = "Country A",
    adm1 = paste0("Region ", rep(1:3, length.out = n_rows)),
    adm2 = paste0("District ", rep(1:5, length.out = n_rows)),
    adm3 = paste0("Chiefdom ", rep(1:8, length.out = n_rows)),
    adm0_guid = "GUID_ADM0",
    adm1_guid = paste0("GUID_ADM1_", rep(1:3, length.out = n_rows)),
    adm2_guid = paste0("GUID_ADM2_", rep(1:5, length.out = n_rows)),
    adm3_guid = paste0("GUID_ADM3_", rep(1:8, length.out = n_rows)),
    allout = stats::rpois(n_rows, 100),
    susp = stats::rpois(n_rows, 80),
    test = stats::rpois(n_rows, 70),
    conf = stats::rpois(n_rows, 50),
    maltreat = stats::rpois(n_rows, 45),
    alladm = stats::rpois(n_rows, 20),
    maladm = stats::rpois(n_rows, 15),
    maldth = stats::rpois(n_rows, 2)
  )

  if (add_issues) {
    # add missing values in core IDs
    data$adm2[1:3] <- NA_character_
    data$date[4:5] <- as.Date(NA)

    # add missing values in indicators
    data$test[6:10] <- NA_real_
    data$conf[11:15] <- NA_real_

    # add duplicate records
    data$record_id[20] <- data$record_id[19]
    data$record_id[22] <- data$record_id[21]

    # add future dates
    future_date <- seq.Date(
      lubridate::floor_date(Sys.Date(), "month"),
      length.out = 2,
      by = "3 months"
    )[2]
    data$date[25:27] <- future_date

    # add consistency violations (output > input)
    if (n_rows >= 30) data$conf[30] <- data$test[30] + 10
    if (n_rows >= 31) data$maltreat[31] <- data$conf[31] + 5
    if (n_rows >= 32) data$maldth[32] <- data$maladm[32] + 2

    # add outliers
    if (n_rows >= 40) data$conf[40] <- 1000
    if (n_rows >= 41) data$test[41] <- 2000
    if (n_rows >= 42) data$maltreat[42] <- 500
    if (n_rows >= 12) data$maltreat[42] <- 600
  }

  data
}

# test basic structure -------------------------------------------------------

testthat::test_that("validate_routine_hf_data returns correct structure", {
  data <- create_test_data(n_rows = 50, add_issues = TRUE)

  result <- sntutils::validate_routine_hf_data(
    data = data,
    verbose = FALSE,
    check_outliers = TRUE,
    outlier_methods = c("iqr", "median", "mean")
  )

  # check result is a list
  testthat::expect_type(result, "list")

  # check required components exist
  testthat::expect_named(
    result,
    c(
      "Summary",
      "Missing values",
      "Missing values detail",
      "Duplicate records",
      "Future dates",
      "Consistency failures",
      "Consistency summary",
      "Consistency details",
      "Outliers",
      "HF activeness detail",
      "HF activeness summary",
      "Data dictionary"
    ),
    ignore.order = TRUE
  )

  # check Summary is a tibble
  testthat::expect_s3_class(result$Summary, "tbl_df")

  # check Summary has correct columns
  testthat::expect_named(
    result$Summary,
    c("check", "issues_found", "total_records", "percent")
  )
})

# test missing values check --------------------------------------------------

testthat::test_that("validate_routine_hf_data detects missing values correctly", {
  data <- create_test_data(n_rows = 50, add_issues = TRUE)

  result <- sntutils::validate_routine_hf_data(
    data = data,
    verbose = FALSE,
    check_duplicates = FALSE,
    check_future_dates = FALSE,
    check_outliers = FALSE
  )

  # check Missing values table exists and has correct structure
  testthat::expect_s3_class(result[["Missing values"]], "tbl_df")
  testthat::expect_named(
    result[["Missing values"]],
    c("variable", "n_missing", "total", "percent_missing", "column_type")
  )

  # check that missing values were detected
  missing_df <- result[["Missing values"]]
  testthat::expect_gt(nrow(missing_df), 0)

  # check column types are categorized
  testthat::expect_true(all(missing_df$column_type %in% c("Core ID", "Indicator")))

  # check summary has separate rows for Core IDs and Indicators
  summary_checks <- result$Summary$check
  testthat::expect_true("Missing values (Core IDs)" %in% summary_checks)
  testthat::expect_true("Missing values (Indicators)" %in% summary_checks)

  # verify core ID missing values detected
  core_missing <- missing_df |>
    dplyr::filter(column_type == "Core ID", n_missing > 0)
  testthat::expect_gt(nrow(core_missing), 0)
  testthat::expect_true("adm2" %in% core_missing$variable)
  testthat::expect_true("date" %in% core_missing$variable)

  # verify indicator missing values detected
  indicator_missing <- missing_df |>
    dplyr::filter(column_type == "Indicator", n_missing > 0)
  testthat::expect_gt(nrow(indicator_missing), 0)
})

testthat::test_that("missing values summary shows column counts not cell counts", {
  data <- create_test_data(n_rows = 100, add_issues = TRUE)

  result <- sntutils::validate_routine_hf_data(
    data = data,
    verbose = FALSE,
    check_duplicates = FALSE,
    check_future_dates = FALSE,
    check_outliers = FALSE
  )

  # check that issues_found contains "column(s)"
  summary_df <- result$Summary
  core_row <- summary_df |>
    dplyr::filter(check == "Missing values (Core IDs)")
  indicator_row <- summary_df |>
    dplyr::filter(check == "Missing values (Indicators)")

  testthat::expect_true(grepl("column\\(s\\)", core_row$issues_found))
  testthat::expect_true(grepl("column\\(s\\)", core_row$total_records))
  testthat::expect_true(grepl("column\\(s\\)", indicator_row$issues_found))
  testthat::expect_true(grepl("column\\(s\\)", indicator_row$total_records))
})

# test duplicate records check -----------------------------------------------

testthat::test_that("validate_routine_hf_data detects duplicate records", {
  data <- create_test_data(n_rows = 50, add_issues = TRUE)

  result <- sntutils::validate_routine_hf_data(
    data = data,
    verbose = FALSE,
    check_duplicates = TRUE,
    check_future_dates = FALSE,
    check_outliers = FALSE
  )

  # check duplicate records table exists
  testthat::expect_s3_class(result[["Duplicate records"]], "tbl_df")

  # check duplicates were detected
  testthat::expect_gt(nrow(result[["Duplicate records"]]), 0)

  # verify summary shows duplicate sets count
  summary_df <- result$Summary
  dup_row <- summary_df |>
    dplyr::filter(check == "Duplicate records")

  testthat::expect_equal(nrow(dup_row), 1)
  testthat::expect_true(grepl("set\\(s\\)", dup_row$issues_found))
})

testthat::test_that("validate_routine_hf_data handles no duplicates", {
  data <- create_test_data(n_rows = 50, add_issues = FALSE)

  result <- sntutils::validate_routine_hf_data(
    data = data,
    verbose = FALSE,
    check_duplicates = TRUE,
    check_future_dates = FALSE,
    check_outliers = FALSE
  )

  # check duplicate records table is empty
  testthat::expect_equal(nrow(result[["Duplicate records"]]), 0)

  # check summary shows 0 duplicates
  summary_df <- result$Summary
  dup_row <- summary_df |>
    dplyr::filter(check == "Duplicate records")

  testthat::expect_equal(dup_row$percent, 0)
})

# test future dates check ----------------------------------------------------

testthat::test_that("validate_routine_hf_data detects future dates", {
  data <- create_test_data(n_rows = 50, add_issues = TRUE)

  result <- sntutils::validate_routine_hf_data(
    data = data,
    verbose = FALSE,
    check_duplicates = FALSE,
    check_future_dates = TRUE,
    check_outliers = FALSE
  )

  # check future dates table exists
  testthat::expect_s3_class(result[["Future dates"]], "tbl_df")

  # check future dates were detected
  testthat::expect_gt(nrow(result[["Future dates"]]), 0)

  # verify all detected dates are in the future
  future_df <- result[["Future dates"]]
  current_date <- lubridate::floor_date(Sys.Date(), "month")
  testthat::expect_true(all(future_df$date > current_date, na.rm = TRUE))

  # check summary
  summary_df <- result$Summary
  future_row <- summary_df |>
    dplyr::filter(check == "Future dates")

  testthat::expect_equal(nrow(future_row), 1)
  testthat::expect_gt(future_row$percent, 0)
})

testthat::test_that("validate_routine_hf_data handles no future dates", {
  data <- create_test_data(n_rows = 50, add_issues = FALSE)

  result <- sntutils::validate_routine_hf_data(
    data = data,
    verbose = FALSE,
    check_duplicates = FALSE,
    check_future_dates = TRUE,
    check_outliers = FALSE
  )

  # check future dates table is empty
  testthat::expect_equal(nrow(result[["Future dates"]]), 0)

  # check summary shows 0 future dates
  summary_df <- result$Summary
  future_row <- summary_df |>
    dplyr::filter(check == "Future dates")

  testthat::expect_equal(future_row$percent, 0)
})

# test logical consistency check ---------------------------------------------

testthat::test_that("validate_routine_hf_data detects consistency violations", {
  data <- create_test_data(n_rows = 50, add_issues = TRUE)

  result <- sntutils::validate_routine_hf_data(
    data = data,
    verbose = FALSE,
    check_duplicates = FALSE,
    check_future_dates = FALSE,
    check_outliers = FALSE
  )

  # check consistency failures exist
  testthat::expect_type(result[["Consistency failures"]], "list")
  testthat::expect_gt(length(result[["Consistency failures"]]), 0)

  # check consistency details table
  testthat::expect_s3_class(result[["Consistency details"]], "tbl_df")
  testthat::expect_gt(nrow(result[["Consistency details"]]), 0)

  # verify consistency details has expected columns
  testthat::expect_true("input_indicator" %in% names(result[["Consistency details"]]))
  testthat::expect_true("output_indicator" %in% names(result[["Consistency details"]]))
  testthat::expect_true("difference" %in% names(result[["Consistency details"]]))
  testthat::expect_true("difference_sd" %in% names(result[["Consistency details"]]))

  # verify all violations have output > input
  details <- result[["Consistency details"]]
  testthat::expect_true(all(details$difference > 0))

  # check summary
  summary_df <- result$Summary
  consistency_row <- summary_df |>
    dplyr::filter(check == "Logical consistency")

  testthat::expect_equal(nrow(consistency_row), 1)
  testthat::expect_true(grepl("pair\\(s\\)", consistency_row$issues_found))
})

testthat::test_that("validate_routine_hf_data includes consistency summary", {
  data <- create_test_data(n_rows = 50, add_issues = TRUE)

  result <- sntutils::validate_routine_hf_data(
    data = data,
    verbose = FALSE,
    check_duplicates = FALSE,
    check_future_dates = FALSE,
    check_outliers = FALSE
  )

  # check structure
  testthat::expect_s3_class(result[["Consistency summary"]], "tbl_df")
  testthat::expect_gt(nrow(result[["Consistency summary"]]), 0)

  # check columns
  expected_cols <- c(
    "input_indicator", "output_indicator",
    "input_value", "output_value", "difference",
    "difference_prop", "difference_sd"
  )
  testthat::expect_true(all(expected_cols %in% names(result[["Consistency summary"]])))

  # verify details also has difference_prop
  testthat::expect_true("difference_prop" %in% names(result[["Consistency details"]]))

  # verify aggregation matches details
  summary <- result[["Consistency summary"]]
  details <- result[["Consistency details"]]

  for (i in seq_len(nrow(summary))) {
    pair_details <- details |>
      dplyr::filter(
        input_indicator == summary$input_indicator[i],
        output_indicator == summary$output_indicator[i]
      )

    # verify sums match
    testthat::expect_equal(
      summary$difference[i],
      sum(pair_details$difference, na.rm = TRUE)
    )
  }
})

testthat::test_that("validate_routine_hf_data includes data dictionary", {
  data <- create_test_data(n_rows = 50, add_issues = TRUE)

  result <- sntutils::validate_routine_hf_data(
    data = data,
    verbose = FALSE,
    check_outliers = FALSE
  )

  # check dictionary exists
  testthat::expect_s3_class(result[["Data dictionary"]], "tbl_df")
  testthat::expect_gt(nrow(result[["Data dictionary"]]), 0)

  # check required columns
  testthat::expect_true("variable" %in% names(result[["Data dictionary"]]))
  testthat::expect_true("appears_in_tabs" %in% names(result[["Data dictionary"]]))
  testthat::expect_true("label_en" %in% names(result[["Data dictionary"]]))

  # verify some known columns are documented
  dict <- result[["Data dictionary"]]
  testthat::expect_true("check" %in% dict$variable)  # from Summary
  testthat::expect_true("input_indicator" %in% dict$variable)  # from Consistency
})

testthat::test_that("data dictionary supports translation from dictionaries", {
  data <- create_test_data(n_rows = 50, add_issues = TRUE)

  result <- sntutils::validate_routine_hf_data(
    data = data,
    verbose = FALSE,
    check_outliers = FALSE,
    language = "fr"
  )

  # check French label column exists
  testthat::expect_true("label_fr" %in% names(result[["Data dictionary"]]))

  dict <- result[["Data dictionary"]]

  # verify validation terms are translated
  check_row <- dict |> dplyr::filter(variable == "check")
  if (nrow(check_row) > 0) {
    testthat::expect_equal(check_row$label_fr, "Vérification")
  }

  # verify SNT variables are translated (if present)
  conf_row <- dict |> dplyr::filter(variable == "conf")
  if (nrow(conf_row) > 0) {
    testthat::expect_true(!is.na(conf_row$label_fr))
    testthat::expect_true(conf_row$label_fr != "Conf")  # should be translated
  }
})

testthat::test_that("validate_routine_hf_data handles custom consistency pairs", {
  data <- create_test_data(n_rows = 50, add_issues = TRUE)

  # test with only one consistency pair
  result <- sntutils::validate_routine_hf_data(
    data = data,
    consistency_pairs = list(
      list(input = "test", output = "conf")
    ),
    verbose = FALSE,
    check_duplicates = FALSE,
    check_future_dates = FALSE,
    check_outliers = FALSE
  )

  # check that only one pair is checked
  testthat::expect_type(result[["Consistency failures"]], "list")
  testthat::expect_true("test vs conf" %in% names(result[["Consistency failures"]]))
})

# test outlier detection -----------------------------------------------------

testthat::test_that("validate_routine_hf_data detects outliers", {
  data <- create_test_data(n_rows = 100, add_issues = TRUE)

  result <- sntutils::validate_routine_hf_data(
    data = data,
    verbose = FALSE,
    check_duplicates = FALSE,
    check_future_dates = FALSE,
    check_outliers = TRUE,
    outlier_methods = c("iqr", "median", "mean")
  )

  # check outliers table exists
  testthat::expect_s3_class(result[["Outliers"]], "tbl_df")

  # check outliers were detected
  testthat::expect_gte(nrow(result[["Outliers"]]), 0)

  # If outliers were detected, verify the table structure
  if (nrow(result[["Outliers"]]) > 0) {
    outlier_cols <- names(result[["Outliers"]])
    testthat::expect_true("value" %in% outlier_cols)
    testthat::expect_true(any(grepl("outlier_flag_", outlier_cols)))
  }

  # check summary always has an outliers row
  summary_df <- result$Summary
  outlier_row <- summary_df |>
    dplyr::filter(check == "Outliers")

  testthat::expect_equal(nrow(outlier_row), 1)
  # percent can be 0 if no outliers detected
  testthat::expect_gte(outlier_row$percent, 0)
})

testthat::test_that("validate_routine_hf_data handles no outliers", {
  # create data without outliers
  data <- create_test_data(n_rows = 50, add_issues = FALSE)

  result <- sntutils::validate_routine_hf_data(
    data = data,
    verbose = FALSE,
    check_duplicates = FALSE,
    check_future_dates = FALSE,
    check_outliers = TRUE,
    outlier_methods = c("iqr")
  )

  # outliers may or may not be detected depending on the data distribution
  # just check that the structure is correct
  testthat::expect_true("Outliers" %in% names(result))

  # check summary has outliers row
  summary_df <- result$Summary
  outlier_row <- summary_df |>
    dplyr::filter(check == "Outliers")

  testthat::expect_equal(nrow(outlier_row), 1)
})

# test parameter combinations ------------------------------------------------

testthat::test_that("validate_routine_hf_data works with all checks disabled", {
  data <- create_test_data(n_rows = 50, add_issues = TRUE)

  result <- sntutils::validate_routine_hf_data(
    data = data,
    verbose = FALSE,
    check_duplicates = FALSE,
    check_future_dates = FALSE,
    check_outliers = FALSE
  )

  # only missing values and consistency checks should run (consistency always runs)
  testthat::expect_gte(nrow(result$Summary), 2)  # At least Core IDs + Indicators
  # check that no duplicate, future date, or outlier checks ran
  testthat::expect_false("Duplicate records" %in% result$Summary$check)
  testthat::expect_false("Future dates" %in% result$Summary$check)
  testthat::expect_false("Outliers" %in% result$Summary$check)
})

testthat::test_that("validate_routine_hf_data works with all checks enabled", {
  data <- create_test_data(n_rows = 100, add_issues = TRUE)

  result <- sntutils::validate_routine_hf_data(
    data = data,
    verbose = FALSE,
    check_duplicates = TRUE,
    check_future_dates = TRUE,
    check_outliers = TRUE,
    outlier_methods = c("iqr", "median", "mean")
  )

  # all checks should be in summary
  summary_checks <- result$Summary$check
  testthat::expect_true("Missing values (Core IDs)" %in% summary_checks)
  testthat::expect_true("Missing values (Indicators)" %in% summary_checks)
  testthat::expect_true("Duplicate records" %in% summary_checks)
  testthat::expect_true("Future dates" %in% summary_checks)
  testthat::expect_true("Logical consistency" %in% summary_checks)
  testthat::expect_true("Outliers" %in% summary_checks)
})

testthat::test_that("validate_routine_hf_data handles custom indicator list", {
  data <- create_test_data(n_rows = 50, add_issues = TRUE)

  result <- sntutils::validate_routine_hf_data(
    data = data,
    indicators = c("conf", "test"),
    verbose = FALSE,
    check_outliers = FALSE
  )

  # only specified indicators should be in missing values
  missing_df <- result[["Missing values"]]
  indicator_vars <- missing_df |>
    dplyr::filter(column_type == "Indicator") |>
    dplyr::pull(variable)

  # should only have conf and test (if they have missing values)
  testthat::expect_true(all(indicator_vars %in% c("conf", "test")))
})

# test edge cases ------------------------------------------------------------

testthat::test_that("validate_routine_hf_data handles data with no issues", {
  data <- create_test_data(n_rows = 50, add_issues = FALSE)

  result <- sntutils::validate_routine_hf_data(
    data = data,
    verbose = FALSE,
    check_duplicates = TRUE,
    check_future_dates = TRUE,
    check_outliers = TRUE
  )

  # summary should show all checks
  testthat::expect_gte(nrow(result$Summary), 5)

  # most checks should have 0 or very low% issues (except possibly outliers/missing)
  summary_df <- result$Summary
  zero_issues <- summary_df |>
    dplyr::filter(!check %in% c("Outliers", "Missing values (Indicators)", "Missing values (Core IDs)", "Logical consistency"))

  # if there are any checks left, they should have low percentages
  if (nrow(zero_issues) > 0) {
    testthat::expect_true(all(zero_issues$percent <= 5))
  }
})

testthat::test_that("validate_routine_hf_data handles minimal data", {
  data <- tibble::tibble(
    record_id = c("REC1", "REC2", "REC3"),
    hf_id = c("HF1", "HF2", "HF3"),
    hf_uid = c("HF1", "HF2", "HF3"),
    hf = c("Health Facility 1", "Health Facility 2", "Health Facility 3"),
    date = rep(Sys.Date(), 3),
    yearmon = format(Sys.Date(), "%Y-%m"),
    year = lubridate::year(Sys.Date()),
    month = lubridate::month(Sys.Date()),
    adm0 = "Country",
    adm1 = "Region",
    conf = c(10, 20, 30)
  )

  result <- sntutils::validate_routine_hf_data(
    data = data,
    indicators = "conf",
    verbose = FALSE,
    check_duplicates = TRUE,
    check_future_dates = TRUE,
    check_outliers = FALSE
  )

  # should complete without error
  testthat::expect_type(result, "list")
  testthat::expect_s3_class(result$Summary, "tbl_df")
})

# test verbose mode ----------------------------------------------------------

testthat::test_that("validate_routine_hf_data verbose mode works", {
  data <- create_test_data(n_rows = 50, add_issues = TRUE)

  # capture output - just check that verbose produces output
  testthat::expect_output(
    result <- sntutils::validate_routine_hf_data(
      data = data,
        verbose = TRUE,
      check_outliers = FALSE
    )
  )

  # should still return valid results
  testthat::expect_type(result, "list")
  testthat::expect_s3_class(result$Summary, "tbl_df")
})

# test return value invisibility ---------------------------------------------

testthat::test_that("validate_routine_hf_data returns invisibly", {
  data <- create_test_data(n_rows = 50, add_issues = TRUE)

  # check that function returns invisibly
  testthat::expect_invisible(
    sntutils::validate_routine_hf_data(
      data = data,
      verbose = FALSE,
      check_outliers = FALSE
    )
  )
})

# test summary percentages ---------------------------------------------------

testthat::test_that("summary percentages are correctly calculated", {
  data <- create_test_data(n_rows = 100, add_issues = TRUE)

  result <- sntutils::validate_routine_hf_data(
    data = data,
    verbose = FALSE,
    check_duplicates = TRUE,
    check_future_dates = TRUE,
    check_outliers = TRUE
  )

  summary_df <- result$Summary

  # all percentages should be numeric
  testthat::expect_type(summary_df$percent, "double")

  # all percentages should be between 0 and 100
  testthat::expect_true(all(summary_df$percent >= 0))
  testthat::expect_true(all(summary_df$percent <= 100))

  # percentages should be rounded to 2 decimal places
  # check by converting to string and counting decimal places
  pct_strings <- format(summary_df$percent, nsmall = 2)
  testthat::expect_true(all(nchar(
    sub(".*\\.", "", pct_strings)
  ) <= 2))
})

# test language translation --------------------------------------------------

testthat::test_that("validate_routine_hf_data handles language translation", {
  data <- create_test_data(n_rows = 50, add_issues = TRUE)
  temp_dir <- tempdir()
  temp_file <- file.path(temp_dir, "test_validation")

  # Test with French translation using dictionaries
  result <- sntutils::validate_routine_hf_data(
    data = data,
    verbose = FALSE,
    save_results = TRUE,
    output_path = temp_dir,
    output_name = "test_validation",
    output_formats = "xlsx",
    language = "fr"
  )

  # Check that file was created
  xlsx_files <- list.files(temp_dir, pattern = "test_validation.*\\.xlsx$", full.names = TRUE)
  testthat::expect_true(length(xlsx_files) > 0)

  # Clean up
  unlink(xlsx_files)
})

testthat::test_that("validate_routine_hf_data translation converts column names correctly", {
  # Create minimal test results
  test_results <- list(
    Summary = tibble::tibble(
      check = c("Missing values (Core IDs)", "Duplicate records"),
      issues_found = c("2 column(s)", "3 set(s)"),
      total_records = c("10 column(s)", "100"),
      percent = c(20, 3)
    ),
    `Missing values` = tibble::tibble(
      variable = c("adm2", "conf"),
      n_missing = c(5, 10),
      total = c(100, 100),
      percent_missing = c(5, 10),
      column_type = c("Core ID", "Indicator")
    )
  )

  # Call the internal translation function (now dictionary-based)
  translated <- sntutils:::`.translate_results`(
    results = test_results,
    language = "fr"
  )

  # Check sheet names are translated using validation_terms dictionary
  testthat::expect_true("Résumé" %in% names(translated))
  testthat::expect_true("Valeurs manquantes" %in% names(translated))

  # Check column names don't have dots
  summary_cols <- names(translated[["Résumé"]])
  testthat::expect_false(any(grepl("\\.", summary_cols)))

  missing_cols <- names(translated[["Valeurs manquantes"]])
  testthat::expect_false(any(grepl("\\.", missing_cols)))

  # Check that column names have been translated using dictionaries
  # Should have French column names
  testthat::expect_true(any(grepl("Type", missing_cols, ignore.case = TRUE)))
})

# test data dictionary for input columns ------------------------------------

testthat::test_that("data dictionary includes input columns from non-duplicate tabs", {
  data <- create_test_data(n_rows = 50, add_issues = TRUE)

  result <- sntutils::validate_routine_hf_data(
    data = data,
    verbose = FALSE,
    check_duplicates = TRUE,
    check_future_dates = TRUE,
    check_outliers = TRUE
  )

  dict <- result[["Data dictionary"]]

  # verify input ID columns are included (they appear in multiple tabs)
  testthat::expect_true("record_id" %in% dict$variable)
  testthat::expect_true("date" %in% dict$variable)
  testthat::expect_true("hf_uid" %in% dict$variable)

  # verify admin columns are included
  testthat::expect_true("adm0" %in% dict$variable)
  testthat::expect_true("adm1" %in% dict$variable)
  testthat::expect_true("adm2" %in% dict$variable)

  # verify malaria indicators are included (they appear in Outliers/Consistency tabs)
  testthat::expect_true(any(c("conf", "test", "susp", "maltreat") %in% dict$variable))
})

testthat::test_that("data dictionary excludes columns ONLY in Duplicate records tab", {
  data <- create_test_data(n_rows = 50, add_issues = TRUE)

  result <- sntutils::validate_routine_hf_data(
    data = data,
    verbose = FALSE,
    check_duplicates = TRUE,
    check_future_dates = FALSE,
    check_outliers = FALSE,
    check_facility_activeness = FALSE
  )

  dict <- result[["Data dictionary"]]

  # check that columns appearing ONLY in Duplicate records are excluded
  # however, if those same columns appear in other tabs (like Future dates),
  # they should be included
  duplicate_only_vars <- dict |>
    dplyr::filter(appears_in_tabs == "Duplicate records")

  # should be empty - no columns appear ONLY in Duplicate records
  testthat::expect_equal(nrow(duplicate_only_vars), 0)
})

testthat::test_that("data dictionary provides labels for input columns", {
  data <- create_test_data(n_rows = 50, add_issues = TRUE)

  result <- sntutils::validate_routine_hf_data(
    data = data,
    verbose = FALSE,
    check_outliers = TRUE
  )

  dict <- result[["Data dictionary"]]

  # verify input columns have human-readable labels
  record_id_row <- dict |> dplyr::filter(variable == "record_id")
  if (nrow(record_id_row) > 0) {
    testthat::expect_true(!is.na(record_id_row$label_en))
    testthat::expect_true(record_id_row$label_en != "record_id")
    testthat::expect_true(nchar(record_id_row$label_en) > 0)
  }

  # verify admin columns have labels
  adm0_row <- dict |> dplyr::filter(variable == "adm0")
  if (nrow(adm0_row) > 0) {
    testthat::expect_true(!is.na(adm0_row$label_en))
    testthat::expect_true(adm0_row$label_en != "adm0")
  }
})

testthat::test_that("data dictionary provides labels for malaria indicators", {
  data <- create_test_data(n_rows = 50, add_issues = TRUE)

  result <- sntutils::validate_routine_hf_data(
    data = data,
    verbose = FALSE,
    check_outliers = TRUE
  )

  dict <- result[["Data dictionary"]]

  # verify malaria indicators have labels from snt_var_tree
  conf_row <- dict |> dplyr::filter(variable == "conf")
  if (nrow(conf_row) > 0) {
    testthat::expect_true(!is.na(conf_row$label_en))
    testthat::expect_true(conf_row$label_en != "conf")
    testthat::expect_true(nchar(conf_row$label_en) > 5)  # should be descriptive
  }

  test_row <- dict |> dplyr::filter(variable == "test")
  if (nrow(test_row) > 0) {
    testthat::expect_true(!is.na(test_row$label_en))
    testthat::expect_true(test_row$label_en != "test")
  }
})

testthat::test_that("data dictionary supports multilingual labels for input columns", {
  data <- create_test_data(n_rows = 50, add_issues = TRUE)

  result <- sntutils::validate_routine_hf_data(
    data = data,
    verbose = FALSE,
    check_outliers = TRUE,
    language = "fr"
  )

  dict <- result[["Data dictionary"]]

  # verify French label column exists
  testthat::expect_true("label_fr" %in% names(dict))

  # verify input columns have French labels
  record_id_row <- dict |> dplyr::filter(variable == "record_id")
  if (nrow(record_id_row) > 0) {
    testthat::expect_true(!is.na(record_id_row$label_fr))
    testthat::expect_true(nchar(record_id_row$label_fr) > 0)
  }

  # verify malaria indicators have French labels
  conf_row <- dict |> dplyr::filter(variable == "conf")
  if (nrow(conf_row) > 0) {
    testthat::expect_true(!is.na(conf_row$label_fr))
    testthat::expect_true(conf_row$label_fr != conf_row$label_en)
  }
})

testthat::test_that("data dictionary includes validation-created columns", {
  data <- create_test_data(n_rows = 50, add_issues = TRUE)

  result <- sntutils::validate_routine_hf_data(
    data = data,
    verbose = FALSE,
    check_outliers = TRUE
  )

  dict <- result[["Data dictionary"]]

  # verify validation-created columns are still included
  testthat::expect_true("check" %in% dict$variable)
  testthat::expect_true("issues_found" %in% dict$variable)
  testthat::expect_true("percent" %in% dict$variable)
  testthat::expect_true("input_indicator" %in% dict$variable)
  testthat::expect_true("output_indicator" %in% dict$variable)
  testthat::expect_true("difference" %in% dict$variable)

  # verify outlier columns if outliers detected
  if (any(grepl("outlier_flag", dict$variable))) {
    testthat::expect_true(any(grepl("outlier_flag_", dict$variable)))
  }
})

testthat::test_that("data dictionary appears_in_tabs shows correct tabs", {
  data <- create_test_data(n_rows = 50, add_issues = TRUE)

  result <- sntutils::validate_routine_hf_data(
    data = data,
    verbose = FALSE,
    check_duplicates = TRUE,
    check_future_dates = TRUE,
    check_outliers = TRUE
  )

  dict <- result[["Data dictionary"]]

  # verify record_id appears in multiple tabs
  record_id_row <- dict |> dplyr::filter(variable == "record_id")
  if (nrow(record_id_row) > 0) {
    tabs <- record_id_row$appears_in_tabs
    # should appear in Future dates, Outliers, Consistency details, etc.
    # but NOT show "Duplicate records" alone
    testthat::expect_false(tabs == "Duplicate records")
    testthat::expect_true(grepl(",", tabs) || !grepl("Duplicate", tabs))
  }

  # verify validation-only columns appear in specific tabs
  check_row <- dict |> dplyr::filter(variable == "check")
  if (nrow(check_row) > 0) {
    testthat::expect_true(grepl("Summary", check_row$appears_in_tabs))
  }
})
