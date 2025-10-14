# tests/testthat/test-facility_reporting_plot.R
# basic unit tests for facility_reporting_plot: translation, palettes, saving
# RELEVANT FILES:R/facility_reporting_plot.R,tests/testthat/helper-utils.R

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
    binary_classification = TRUE,
    nonreport_window = 2
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
    binary_classification = TRUE,
    nonreport_window = 2
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

test_that("classify_facility_activity method3 with binary classification", {
  toy_method3 <- tibble::tibble(
    hf_uid = "hf_0001",
    date = seq(as.Date("2024-01-01"), as.Date("2024-12-01"), by = "month"),
    allout = c(20, NA, NA, NA, NA, NA, NA, NA, 5, NA, NA, NA),
    susp = c(15, NA, NA, NA, NA, NA, NA, NA, 5, NA, NA, NA),
    test = c(10, NA, NA, NA, NA, NA, NA, NA, 5, NA, NA, NA),
    conf = c(5, NA, NA, NA, NA, NA, NA, NA, 5, NA, NA, NA),
    maltreat = c(5, NA, NA, NA, NA, NA, NA, NA, 5, NA, NA, NA)
  )

  result <- classify_facility_activity(
    data = toy_method3,
    hf_col = "hf_uid",
    key_indicators = c("allout", "susp", "test", "conf", "maltreat"),
    method = "method3",
    binary_classification = TRUE,
    nonreport_window = 2
  )

  expect_s3_class(result, "data.frame")
  expect_true("activity_status" %in% names(result))
  expect_equal(nrow(result), 12)

  # Check expected output matches your dummy data
  expected_status <- c("Active", "Inactive", "Inactive", "Inactive", "Inactive", 
                      "Inactive", "Inactive", "Inactive", "Active", "Inactive", 
                      "Inactive", "Inactive")
  
  expect_equal(result$activity_status, expected_status)
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
    "should be one of"
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

test_that("classify_facility_activity validates inputs", {
  data <- tibble::tibble(
    hf = c("HF1", "HF2"),
    date = base::as.Date(c("2023-01-01", "2023-02-01")),
    test = c(1, 2)
  )

  expect_error(
    classify_facility_activity(
      data = list(a = 1),
      hf_col = "hf",
      key_indicators = "test"
    ),
    "must be a data.frame"
  )

  expect_error(
    classify_facility_activity(
      data = data,
      hf_col = "missing_col",
      key_indicators = "test"
    ),
    "missing required columns"
  )

  bad_dates <- data
  bad_dates$date <- NA
  expect_error(
    classify_facility_activity(
      data = bad_dates,
      hf_col = "hf",
      key_indicators = "test"
    ),
    "cannot be entirely missing"
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
