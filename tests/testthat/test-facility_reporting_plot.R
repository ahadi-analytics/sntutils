# tests/testthat/test-facility_reporting_plot.R
# basic unit tests for facility_reporting_plot: translation, palettes, saving
# RELEVANT FILES:R/facility_reporting_plot.R,tests/testthat/helper-utils.R

test_that("classify_facility_activity correctly flags activity status", {
  data <- tibble::tibble(
    hf = c("HF1", "HF1", "HF1", "HF2", "HF2", "HF2", "HF3", "HF3", "HF3"),
    date = rep(
      base::seq.Date(
        base::as.Date("2023-01-01"),
        by = "month",
        length.out = 3
      ),
      times = 3
    ),
    test = c(1, NA, 2, NA, NA, NA, NA, NA, NA),
    pres = c(NA, 3, NA, 5, NA, 6, NA, NA, NA),
    conf = c(NA, NA, 1, NA, NA, NA, NA, NA, NA)
  )

  result <- classify_facility_activity(
    data = data,
    hf_col = "hf",
    date_col = "date",
    key_indicators = c("test", "pres", "conf")
  )

  expect_s3_class(result, "data.frame")
  expect_true("activity_status" %in% names(result))
  expect_true("reported_any" %in% names(result))
  expect_true("first_reporting_date" %in% names(result))
  expect_true("has_ever_reported" %in% names(result))

  expect_s3_class(result$activity_status, "factor")
  expect_equal(
    levels(result$activity_status),
    c(
      "Active Reporting",
      "Active Facility - Not Reporting",
      "Inactive Facility"
    )
  )

  hf1_result <- result[result$hf == "HF1", ]
  expect_equal(hf1_result$reported_any, c(TRUE, TRUE, TRUE))
  expect_equal(
    as.character(hf1_result$activity_status),
    c("Active Reporting", "Active Reporting", "Active Reporting")
  )

  hf2_result <- result[result$hf == "HF2", ]
  expect_equal(hf2_result$reported_any, c(TRUE, FALSE, TRUE))
  expect_equal(
    as.character(hf2_result$activity_status),
    c("Active Reporting", "Active Facility - Not Reporting", "Active Reporting")
  )

  hf3_result <- result[result$hf == "HF3", ]
  expect_equal(hf3_result$reported_any, c(FALSE, FALSE, FALSE))
  expect_equal(
    as.character(hf3_result$activity_status),
    c("Inactive Facility", "Inactive Facility", "Inactive Facility")
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

test_that("facility_reporting_plot can save and compress", {
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
    palette = "classic",
    plot_path = tmp_dir,
    compress_image = FALSE
  )

  expect_s3_class(plot, "ggplot")
  saved_files <- fs::dir_ls(tmp_dir, glob = "*.png")
  expect_length(saved_files, 1L)
})
