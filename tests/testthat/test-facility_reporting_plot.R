test_that("classify_facility_activity method1 with binary classification", {
  toy_method1 <- tibble::tibble(
    hf_uid = "hf_0001",
    date = seq(as.Date("2024-01-01"), as.Date("2024-12-01"), by = "month"),
    allout = c(NA, NA, 20, 30, 60, NA, NA, NA, 5, NA, NA, NA),
    susp = c(NA, NA, 15, 15, 15, NA, NA, NA, 5, NA, NA, NA),
    test = c(NA, NA, 10, 10, 10, NA, NA, NA, 5, NA, NA, NA),
    conf = c(NA, NA, 5, 8, 5, NA, NA, NA, 5, NA, NA, NA),
    maltreat = c(NA, NA, 5, 5, NA, NA, NA, NA, 5, NA, NA, NA)
  )

  result <- classify_facility_activity(
    data = toy_method1,
    hf_col = "hf_uid",
    key_indicators = c("allout", "susp", "test", "conf", "maltreat"),
    method = "method1",
    binary_classification = TRUE
  )

  expect_s3_class(result, "data.frame")
  expect_true("activity_status" %in% names(result))
  expect_equal(nrow(result), 12)

  # Check expected output matches your dummy data
  expected_status <- c("Inactive", "Inactive", "Active", "Active", "Active",
                      "Active", "Active", "Active", "Active", "Active",
                      "Active", "Active")

  expect_equal(result$activity_status, expected_status)
})

test_that("classify_facility_activity method2 with binary classification", {
  toy_method2 <- tibble::tibble(
    hf_uid = "hf_0001",
    date = seq(as.Date("2024-01-01"), as.Date("2024-12-01"), by = "month"),
    allout = c(NA, NA, 20, 30, 60, NA, NA, NA, 5, NA, NA, NA),
    susp = c(NA, NA, 15, 15, 15, NA, NA, NA, 5, NA, NA, NA),
    test = c(NA, NA, 10, 10, 10, NA, NA, NA, 5, NA, NA, NA),
    conf = c(NA, NA, 5, 8, 5, NA, NA, NA, 5, NA, NA, NA),
    maltreat = c(NA, NA, 5, 5, NA, NA, NA, NA, 5, NA, NA, NA)
  )

  result <- classify_facility_activity(
    data = toy_method2,
    hf_col = "hf_uid",
    key_indicators = c("allout", "susp", "test", "conf", "maltreat"),
    method = "method2",
    binary_classification = TRUE
  )

  expect_s3_class(result, "data.frame")
  expect_true("activity_status" %in% names(result))
  expect_equal(nrow(result), 12)

  # Check expected output matches your dummy data
  expected_status <- c("Inactive", "Inactive", "Active", "Active", "Active",
                      "Active", "Active", "Active", "Active", "Inactive",
                      "Inactive", "Inactive")

  expect_equal(result$activity_status, expected_status)
})

test_that("classify_facility_activity method3 with strict closure (no trailing tolerance)", {
  toy_method3_strict <- tibble::tibble(
    hf_uid = "hf_001",
    date = seq(as.Date("2024-01-01"), as.Date("2024-12-01"), by = "month"),
    test = c(5, 5, NA, 10, NA, NA, 8, NA, NA, NA, NA, NA)
  )

  result <- classify_facility_activity(
    data = toy_method3_strict,
    hf_col = "hf_uid",
    key_indicators = "test",
    method = "method3",
    binary_classification = TRUE,
    nonreport_window = 6,
    trailing_tolerance = FALSE
  )

  expected_status <- c(
    "Active",
    "Active",
    "Active",
    "Active",
    "Active",
    "Active",
    "Active",
    "Inactive",
    "Inactive",
    "Inactive",
    "Inactive",
    "Inactive"
  )

  expect_equal(result$activity_status, expected_status)
})

test_that("classify_facility_activity method3 with trailing tolerance (lenient)", {
  toy_method3_lenient <- tibble::tibble(
    hf_uid = "hf_0001",
    date = seq(as.Date("2024-01-01"), as.Date("2024-12-01"), by = "month"),
    allout = c(20, NA, NA, NA, NA, NA, NA, NA, 5, NA, NA, NA),
    susp = c(15, NA, NA, NA, NA, NA, NA, NA, 5, NA, NA, NA),
    test = c(10, NA, NA, NA, NA, NA, NA, NA, 5, NA, NA, NA),
    conf = c(5, NA, NA, NA, NA, NA, NA, NA, 5, NA, NA, NA),
    maltreat = c(5, NA, NA, NA, NA, NA, NA, NA, 5, NA, NA, NA)
  )

  result <- classify_facility_activity(
    data = toy_method3_lenient,
    hf_col = "hf_uid",
    key_indicators = c("allout", "susp", "test", "conf", "maltreat"),
    method = "method3",
    binary_classification = TRUE,
    nonreport_window = 6,
    trailing_tolerance = TRUE
  )

  expected_status <- c(
    "Active",
    "Inactive",
    "Inactive",
    "Inactive",
    "Inactive",
    "Inactive",
    "Inactive",
    "Inactive",
    "Active",
    "Active",
    "Active",
    "Active"
  )

  expect_equal(result$activity_status, expected_status)
})

test_that("classify_facility_activity method3 handles facilities that never report", {
  # Test facility that never reports during observation period
  never_reported <- tibble::tibble(
    hf_uid = "NEVER",
    date = seq(as.Date("2020-01-01"), by = "month", length.out = 12),
    test = rep(NA, 12)
  )

  result <- classify_facility_activity(
    data = never_reported,
    hf_col = "hf_uid",
    key_indicators = "test",
    method = "method3",
    nonreport_window = 6,
    reporting_rule = "any_non_na",
    binary_classification = FALSE
  )

  # All months should be "Inactive Health Facility"
  expected_status <- rep("Inactive Health Facility", 12)
  expect_equal(result$activity_status, expected_status)

  # first_reporting_date should be NA
  expect_true(all(is.na(result$first_reporting_date)))
})

test_that("classify_facility_activity method3 handles facility starting mid-period", {
  # Test facility that starts reporting in the middle of observation period
  # This was the key issue we discovered
  late_starter <- tibble::tibble(
    hf_uid = "LATE",
    date = seq(as.Date("2020-01-01"), by = "month", length.out = 10),
    test = c(NA, NA, NA, 1, 2, NA, NA, NA, NA, NA)  # starts in month 4
  )

  result <- classify_facility_activity(
    data = late_starter,
    hf_col = "hf_uid",
    key_indicators = "test",
    method = "method3",
    nonreport_window = 6,
    reporting_rule = "any_non_na",
    binary_classification = FALSE,
    trailing_tolerance = TRUE
  )

  expected_status <- c(
    "Inactive Health Facility",            # Jan (never reported yet)
    "Inactive Health Facility",            # Feb (never reported yet)
    "Inactive Health Facility",            # Mar (never reported yet)
    "Active Reporting",                     # Apr (first report)
    "Active Reporting",                     # May (reports again)
    "Active Health Facility - Not Reporting", # Jun (1st gap)
    "Active Health Facility - Not Reporting", # Jul (2nd gap)
    "Active Health Facility - Not Reporting", # Aug (3rd gap)
    "Active Health Facility - Not Reporting", # Sep (4th gap)
    "Active Health Facility - Not Reporting"  # Oct (5th gap, not yet inactive)
  )

  expect_equal(result$activity_status, expected_status)

  # first_reporting_date should be April
  expect_equal(unique(result$first_reporting_date), as.Date("2020-04-01"))
})

test_that("classify_facility_activity method3 validates nonreport_window parameter", {
  toy_data <- tibble::tibble(
    hf_uid = "HF1",
    date = as.Date("2020-01-01"),
    test = 1
  )

  # Test with different nonreport_window values
  for (window in c(1, 3, 6, 12)) {
    result <- classify_facility_activity(
      data = toy_data,
      hf_col = "hf_uid",
      key_indicators = "test",
      method = "method3",
      trailing_tolerance = TRUE,
      nonreport_window = window
    )

    expect_s3_class(result, "data.frame")
    expect_equal(nrow(result), 1)
  }

  # Test that parameter is properly used (not just validated)
  multi_month <- tibble::tibble(
    hf_uid = "HF1",
    date = seq(as.Date("2020-01-01"), by = "month", length.out = 8),
    test = c(1, rep(NA, 7))  # report in month 1, then 7 months of no reports
  )

  # With window=3, should become inactive in month 5 (after 3 months)
  result_3 <- classify_facility_activity(
    data = multi_month,
    hf_col = "hf_uid",
    key_indicators = "test",
    trailing_tolerance = TRUE,
    method = "method3",
    nonreport_window = 3
  )

  expect_equal(result_3$activity_status[5], "Inactive Health Facility")

  # With window=6, should still be active in month 4
  result_6 <- classify_facility_activity(
    data = multi_month,
    hf_col = "hf_uid",
    key_indicators = "test",
    method = "method3",
    trailing_tolerance = TRUE,
    nonreport_window = 6
  )

  expect_equal(result_6$activity_status[4], "Active Health Facility - Not Reporting")
})

test_that("classify_facility_activity method3 correct diagnostics behavior", {
  # Test that demonstrates the correct interpretation of inactive period lengths
  # This test documents why inactive periods can be shorter than nonreport_window
  complex_facility <- tibble::tibble(
    hf_uid = "COMPLEX",
    date = seq(as.Date("2020-01-01"), by = "month", length.out = 20),
    test = c(
      1, 1, NA, NA, NA, NA, NA, NA,  # Jan-Aug: reports 2 months, then 6 missing -> inactive in Aug
      2,                             # Sep: reports -> reactivates
      NA, NA,                        # Oct-Nov: 2 missing
      3,                             # Dec: reports again
      NA, NA, NA, NA, NA, NA, NA, 4  # Jan-Aug: 7 missing, reports in Aug
    )
  )

  result <- classify_facility_activity(
    data = complex_facility,
    hf_col = "hf_uid",
    key_indicators = "test",
    method = "method3",
    nonreport_window = 6,
    reporting_rule = "any_non_na",
    binary_classification = FALSE
  )

  # Find all inactive periods
  inactive_periods <- result |>
    dplyr::filter(activity_status == "Inactive Health Facility") |>
    dplyr::arrange(date) |>
    dplyr::mutate(run_id = data.table::rleid(activity_status)) |>
    dplyr::group_by(run_id) |>
    dplyr::summarise(
      start_date = min(date),
      end_date = max(date),
      run_length = dplyr::n(),
      .groups = "drop"
    )

  # With new gap-aware logic, facility becomes inactive when gap > nonreport_window
  # Gap from Feb to Sep = 7 months > 6, so inactive Mar-Aug
  # Gap from Dec to Aug (next year) = 8 months > 6, so inactive Jan-Jul
  expect_equal(nrow(inactive_periods), 1)  # Two periods but consecutive
  expect_equal(inactive_periods$run_length, 13)  # Mar-Aug 2020 + Jan-Jul 2021
  expect_equal(inactive_periods$start_date, as.Date("2020-03-01"))
  expect_equal(inactive_periods$end_date, as.Date("2021-07-01"))

  # Verify the transition sequence around the inactive period
  transition_period <- result |>
    dplyr::filter(date >= as.Date("2020-06-01") & date <= as.Date("2020-10-01")) |>
    dplyr::select(date, reported_any, activity_status)

  expected_transition <- c(
    "Inactive Health Facility",            # Jun (gap from Feb = 4, but gap Feb-Sep = 7 > 6)
    "Inactive Health Facility",            # Jul (gap from Feb = 5, but gap Feb-Sep = 7 > 6)
    "Inactive Health Facility",            # Aug (gap from Feb = 6, but gap Feb-Sep = 7 > 6)
    "Active Reporting",                    # Sep (reports -> reactivates)
    "Active Health Facility - Not Reporting"  # Oct (within 6 months of Sep)
  )

  expect_equal(transition_period$activity_status, expected_transition)
})

test_that("classify_facility_activity method3 handles reporting_rule parameter", {
  # Test both reporting rules with same data
  test_data <- tibble::tibble(
    hf_uid = "RULE_TEST",
    date = seq(as.Date("2020-01-01"), by = "month", length.out = 6),
    test = c(1, 0, NA, -1, 0, 2)  # mix of positive, zero, negative, and NA
  )

  # With "any_non_na": 0 and -1 count as reported
  result_any <- classify_facility_activity(
    data = test_data,
    hf_col = "hf_uid",
    key_indicators = "test",
    method = "method3",
    reporting_rule = "any_non_na",
    binary_classification = FALSE
  )

  # With "positive_only": only >0 values count as reported
  result_positive <- classify_facility_activity(
    data = test_data,
    hf_col = "hf_uid",
    key_indicators = "test",
    method = "method3",
    reporting_rule = "positive_only",
    binary_classification = FALSE
  )

  # Check reported_any flags are different
  expect_equal(result_any$reported_any, c(TRUE, TRUE, FALSE, TRUE, TRUE, TRUE))
  expect_equal(result_positive$reported_any, c(TRUE, FALSE, FALSE, FALSE, FALSE, TRUE))

  # Both should classify facility as active (has reported at least once)
  # but patterns will differ based on what counts as reporting
  expect_true(all(!is.na(result_any$first_reporting_date)))
  expect_true(all(!is.na(result_positive$first_reporting_date)))
})

test_that("classify_facility_activity returns full original data", {
  # Test that all original columns are preserved in the output
  toy_data <- tibble::tibble(
    hf_uid = "hf_0001",
    date = seq(as.Date("2024-01-01"), as.Date("2024-04-01"), by = "month"),
    allout = c(NA, 20, 30, NA),
    test = c(NA, 10, 10, NA),
    conf = c(NA, 5, 8, NA),
    extra_col1 = c("A", "B", "C", "D"),
    extra_col2 = c(100, 200, 300, 400),
    extra_col3 = c(TRUE, FALSE, TRUE, FALSE)
  )

  result <- classify_facility_activity(
    data = toy_data,
    hf_col = "hf_uid",
    key_indicators = c("allout", "test", "conf"),
    method = "method1",
    binary_classification = FALSE
  )

  # Should have all original columns plus classification columns
  expect_true(all(names(toy_data) %in% names(result)))

  # Should have classification metadata columns
  expect_true("reported_any" %in% names(result))
  expect_true("first_reporting_date" %in% names(result))
  expect_true("last_reporting_date" %in% names(result))
  expect_true("has_ever_reported" %in% names(result))
  expect_true("activity_status" %in% names(result))

  # Should preserve original data values
  expect_equal(result$extra_col1, toy_data$extra_col1)
  expect_equal(result$extra_col2, toy_data$extra_col2)
  expect_equal(result$extra_col3, toy_data$extra_col3)

  # Should have same number of rows as original data
  expect_equal(nrow(result), nrow(toy_data))
})

test_that("classify_facility_activity method=all returns all methods", {
  toy_data <- tibble::tibble(
    hf_uid = "hf_0001",
    date = seq(as.Date("2024-01-01"), as.Date("2024-04-01"), by = "month"),
    allout = c(NA, 20, 30, NA),
    test = c(NA, 10, 10, NA),
    conf = c(NA, 5, 8, NA)
  )

  result <- classify_facility_activity(
    data = toy_data,
    hf_col = "hf_uid",
    key_indicators = c("allout", "test", "conf"),
    method = "all",
    binary_classification = FALSE
  )

  expect_s3_class(result, "data.frame")
  expect_true("activity_status_method1" %in% names(result))
  expect_true("activity_status_method2" %in% names(result))
  expect_true("activity_status_method3" %in% names(result))
  expect_false("activity_status" %in% names(result))
})

test_that("classify_facility_activity validates new parameters", {
  toy_data <- tibble::tibble(
    hf_uid = "hf_0001",
    date = as.Date("2024-01-01"),
    test = 10
  )

  # Test invalid method
  expect_error(
    classify_facility_activity(
      data = toy_data,
      hf_col = "hf_uid",
      key_indicators = "test",
      method = "invalid_method"
    ),
    "method must be"
  )

  # Test invalid reporting_rule
  expect_error(
    classify_facility_activity(
      data = toy_data,
      hf_col = "hf_uid",
      key_indicators = "test",
      reporting_rule = "invalid_rule"
    ),
    "should be one of"
  )
})

test_that("classify_facility_activity builds balanced panel", {
  data <- tibble::tibble(
    hf = c("HF1", "HF2"),
    date = base::as.Date(c("2023-01-01", "2023-03-01")),
    test = c(1, 2),
    pres = c(3, 4),
    conf = c(5, 6)
  )

  result <- classify_facility_activity(
    data = data,
    hf_col = "hf",
    date_col = "date",
    key_indicators = c("test", "pres", "conf")
  )

  expect_equal(nrow(result), 6)

  hf1_months <- result[result$hf == "HF1", "date"]
  expect_equal(
    hf1_months$date,
    base::seq.Date(
      base::as.Date("2023-01-01"),
      base::as.Date("2023-03-01"),
      by = "month"
    )
  )
})



test_that("classify_facility_activity handles first_reporting_date correctly", {
  data <- tibble::tibble(
    hf = rep(c("HF1", "HF2"), each = 3),
    date = rep(
      base::seq.Date(
        base::as.Date("2023-01-01"),
        by = "month",
        length.out = 3
      ),
      times = 2
    ),
    test = c(NA, 1, 2, NA, NA, 5),
    pres = c(NA, NA, NA, NA, NA, NA),
    conf = c(NA, NA, NA, NA, NA, NA)
  )

  result <- classify_facility_activity(
    data = data,
    hf_col = "hf",
    date_col = "date",
    key_indicators = c("test", "pres", "conf")
  )

  hf1_result <- result[result$hf == "HF1", ]
  expect_equal(
    unique(hf1_result$first_reporting_date),
    base::as.Date("2023-02-01")
  )

  hf2_result <- result[result$hf == "HF2", ]
  expect_equal(
    unique(hf2_result$first_reporting_date),
    base::as.Date("2023-03-01")
  )
})

test_that("facility_reporting_plot handles new parameters and binary classification", {
  data <- tibble::tibble(
    hf = rep(c("HF1", "HF2"), each = 3),
    date = rep(
      base::seq.Date(
        base::as.Date("2023-01-01"),
        by = "month",
        length.out = 3
      ),
      times = 2
    ),
    test = c(NA, 1, 0, NA, 1, NA),
    pres = c(NA, 2, NA, 0, NA, 3),
    conf = c(0, NA, 1, NA, NA, 1)
  )

  fake_translate <- function(text, ...) paste0("TR_", text)
  fake_ensure <- function(...) invisible(NULL)

  plot_fun <- facility_reporting_plot
  mockery::stub(plot_fun, "ensure_packages", fake_ensure)
  mockery::stub(plot_fun, "translate_text", fake_translate)

  # Test with new parameters
  plot <- plot_fun(
    data = data,
    hf_col = "hf",
    date_col = "date",
    key_indicators = c("test", "pres", "conf"),
    method = "method2",
    nonreport_window = 3,
    reporting_rule = "any_non_na",
    binary_classification = TRUE,
    palette = "coral",
    target_language = "fr",
    source_language = "en",
    lang_cache_path = tempdir()
  )

  expect_s3_class(plot, "ggplot")

  title <- plot$labels$title
  expect_true(stringr::str_detect(title, stringr::fixed("(n =")))

  legend <- plot$scales$get_scales("fill")
  expect_true(stringr::str_detect(legend$name, stringr::fixed("TR_")))
  expect_true(
    stringr::str_detect(
      legend$name,
      stringr::fixed("(test, pres, conf)")
    )
  )

  # Test binary classification creates correct legend values
  legend_values <- names(legend$palette(2))
  expect_length(legend_values, 2)  # Should only have 2 values for binary

  expect_true(grepl("\n", plot$labels$subtitle, fixed = TRUE))
})

test_that("facility_reporting_plot handles translation and palettes", {
  data <- tibble::tibble(
    hf = rep(c("HF1", "HF2"), each = 3),
    date = rep(
      base::seq.Date(
        base::as.Date("2023-01-01"),
        by = "month",
        length.out = 3
      ),
      times = 2
    ),
    test = c(NA, 1, 0, NA, 1, NA),
    pres = c(NA, 2, NA, 0, NA, 3),
    conf = c(0, NA, 1, NA, NA, 1)
  )

  fake_translate <- function(text, ...) paste0("TR_", text)
  fake_ensure <- function(...) invisible(NULL)

  plot_fun <- facility_reporting_plot
  mockery::stub(plot_fun, "ensure_packages", fake_ensure)
  mockery::stub(plot_fun, "translate_text", fake_translate)

  plot <- plot_fun(
    data = data,
    hf_col = "hf",
    date_col = "date",
    key_indicators = c("test", "pres", "conf"),
    palette = "coral",
    target_language = "fr",
    source_language = "en",
    lang_cache_path = tempdir()
  )

  expect_s3_class(plot, "ggplot")

  title <- plot$labels$title
  expect_true(stringr::str_detect(title, stringr::fixed("(n =")))

  legend <- plot$scales$get_scales("fill")
  expect_true(stringr::str_detect(legend$name, stringr::fixed("TR_")))
  expect_true(
    stringr::str_detect(
      legend$name,
      stringr::fixed("(test, pres, conf)")
    )
  )

  expect_true(grepl("\n", plot$labels$subtitle, fixed = TRUE))
})

test_that("facility_reporting_plot can save and compress with method in filename", {
  data <- tibble::tibble(
    hf = rep(c("HF1", "HF2"), each = 2),
    date = rep(
      base::seq.Date(
        base::as.Date("2023-01-01"),
        by = "month",
        length.out = 2
      ),
      times = 2
    ),
    test = c(NA, 1, 0, 2),
    pres = c(NA, 2, NA, 3),
    conf = c(0, NA, 1, 1)
  )

  tmp_dir <- withr::local_tempdir()

  plot_fun <- facility_reporting_plot
  mockery::stub(plot_fun, "ensure_packages", function(...) invisible(NULL))

  plot <- plot_fun(
    data = data,
    hf_col = "hf",
    date_col = "date",
    key_indicators = c("test", "pres", "conf"),
    method = "method3",
    palette = "classic",
    plot_path = tmp_dir,
    compress_image = FALSE
  )

  expect_s3_class(plot, "ggplot")
  saved_files <- fs::dir_ls(tmp_dir, glob = "*.png")
  expect_length(saved_files, 1L)

  # Check that filename contains method
  filename <- fs::path_file(saved_files[[1]])
  expect_true(stringr::str_detect(filename, "method3"))
})

test_that("facility_reporting_plot handles facet_col parameter", {
  data <- tibble::tibble(
    hf = rep(c("HF1", "HF2", "HF3"), each = 3),
    date = rep(
      base::seq.Date(
        base::as.Date("2023-01-01"),
        by = "month",
        length.out = 3
      ),
      times = 3
    ),
    test = c(1, NA, 2, NA, NA, 3, NA, NA, NA),
    pres = c(NA, 3, NA, 5, NA, NA, 7, 8, 9),
    conf = c(NA, NA, 1, NA, 2, NA, NA, NA, NA)
  )

  plot_fun <- facility_reporting_plot
  mockery::stub(plot_fun, "ensure_packages", function(...) invisible(NULL))

  # Add a grouping column for faceting
  data$region <- c("North", "North", "North", "South", "South", "South", "East", "East", "East")

  # Test with facet_col = "region"
  plot <- plot_fun(
    data = data,
    hf_col = "hf",
    date_col = "date",
    key_indicators = c("test", "pres", "conf"),
    facet_col = "region"
  )

  expect_s3_class(plot, "ggplot")

  # Check that plot has facets
  expect_s3_class(plot$facet, "FacetWrap")

  # Check that legend title still mentions key indicators
  legend <- plot$scales$get_scales("fill")
  expect_true(stringr::str_detect(legend$name, "(test, pres, conf)"))

  # Test error when facet_col is not in data
  expect_error(
    plot_fun(
      data = data,
      hf_col = "hf",
      date_col = "date",
      key_indicators = c("test", "pres", "conf"),
      facet_col = "invalid"
    ),
    "must be a column in the data"
  )
})

test_that("facility_reporting_plot forwards ... arguments correctly", {
  data <- tibble::tibble(
    hf = rep(c("HF1", "HF2"), each = 2),
    date = rep(
      base::seq.Date(
        base::as.Date("2023-01-01"),
        by = "month",
        length.out = 2
      ),
      times = 2
    ),
    test = c(NA, 1, 0, 2),
    pres = c(NA, 2, NA, 3),
    conf = c(0, NA, 1, 1)
  )

  tmp_dir <- withr::local_tempdir()

  plot_fun <- facility_reporting_plot
  mockery::stub(plot_fun, "ensure_packages", function(...) invisible(NULL))

  # Test that ... arguments are passed through to ggsave
  plot <- plot_fun(
    data = data,
    hf_col = "hf",
    date_col = "date",
    key_indicators = c("test", "pres", "conf"),
    plot_path = tmp_dir,
    compress_image = FALSE,
    # Additional arguments that should be forwarded
    bg = "white",
    units = "in"
  )

  expect_s3_class(plot, "ggplot")
  saved_files <- fs::dir_ls(tmp_dir, glob = "*.png")
  expect_length(saved_files, 1L)
})
