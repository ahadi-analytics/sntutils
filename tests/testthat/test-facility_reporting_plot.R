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
    nonreport_window = 6
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

test_that("method2 and method3 classification align with expected example", {
  # --- create example dataset with explicit expected outputs ---
  example_data <- tibble::tibble(
    hf_uid = rep("0995b74e", 68),
    date = seq.Date(
      from = as.Date("2020-01-01"),
      to = as.Date("2025-08-01"),
      by = "1 month"
    ),
    conf = c(
      rep(NA, 35), # Jan 2020–Nov 2022
      166, # Dec 2022
      rep(NA, 17), # Jan 2023–May 2024
      rep(NA, 12), # Jun 2024–May 2025
      47, # May 2025
      rep(NA, 2) # Jun–Jul 2025
      # Aug 2025 already covered → total 68
    ),
    # expected statuses (taken directly from your snapshot table)
    method2_expected = c(
      rep("Inactive", 35),
      "Active",
      rep("Active", 6),
      rep("Active", 6), # Jul 2023 – Dec 2023
      rep("Active", 12), # Jan 2024 – Dec 2024
      rep("Active", 5), # Jan 2025 – Apr 2025
      "Active", # May 2025 (report)
      rep("Inactive", 2) # Jun – Aug 2025
    ),
    method3_expected = c(
      rep("Inactive", 35),
      "Active",
      rep("Active", 6),
      rep("Inactive", 18), # Jul 2023 – Dec 2024
      rep("Inactive", 5),
      rep("Active", 1),
      rep("Inactive", 2)
    )
  )

  # --- run your classification functions ---
  result2 <- classify_facility_activity(
    data = example_data,
    hf_col = "hf_uid",
    date_col = "date",
    key_indicators = "conf",
    method = "method2",
    nonreport_window = 6,
    binary_classification = TRUE
  ) |>
    dplyr::select(hf_uid, date, activity_status_method2 = activity_status)

  result3 <- classify_facility_activity(
    data = example_data,
    hf_col = "hf_uid",
    date_col = "date",
    key_indicators = "conf",
    method = "method3",
    nonreport_window = 6,
    binary_classification = TRUE
  ) |>
    dplyr::select(hf_uid, date, activity_status_method3 = activity_status)

  # --- join results back to expected ---
  comparison <- example_data |>
    dplyr::left_join(result2, by = c("hf_uid", "date")) |>
    dplyr::left_join(result3, by = c("hf_uid", "date"))

  # --- compare outcomes ---
  expect_equal(comparison$activity_status_method2, comparison$method2_expected)
  expect_equal(comparison$activity_status_method3, comparison$method3_expected)
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
