  # for normalizing text like -
  normalize_text <- function(x) stringi::stri_trans_general(x, "Latin-ASCII")

testthat::test_that("classify_facility_activity behaves as expected for all methods (1/2)", {
  # read in data
  df <- file.path(
    system.file("extdata", package = "sntutils"),
    "hf_active_df.rds"
  ) |>
    readRDS()

  df1 <- df$df1
  df2 <- df$df2

  # --- run the classifier ---
  result <- classify_facility_activity(
    df1,
    hf_col = "hf",
    date_col = "date",
    key_indicators = "reported any?",
    method = "all",
    nonreport_window = 6,
    reporting_rule = "any_non_na",
    binary_classification = FALSE
  ) |>
    as.data.frame() |>
    dplyr::mutate(
      method1 = normalize_text(method1),
      method2 = normalize_text(method2),
      method3 = normalize_text(method3),
      activity_status_method1 = normalize_text(activity_status_method1),
      activity_status_method2 = normalize_text(activity_status_method2),
      activity_status_method3 = normalize_text(activity_status_method3)
    )

  # --- method 1 tests ---
  testthat::expect_equal(
    result$activity_status_method1,
    result$method1,
    label = "Method 1 should match expected reference"
  )

  # --- method 2 tests ---
  testthat::expect_equal(
    result$activity_status_method2,
    result$method2,
    label = "Method 2 should match expected reference"
  )

  # --- method 3 tests ---
  testthat::expect_equal(
    result$activity_status_method3,
    result$method3,
    label = "Method 3 should match expected reference"
  )

  # --- logical consistency checks ---
  testthat::expect_true(
    all(
      result$activity_status_method1 %in%
        c("Inactive", "Active Reporting", "Active - Not Reporting")
    ),
    label = "All statuses in method1 should be valid"
  )

  testthat::expect_true(
    all(
      result$activity_status_method2 %in%
        c("Inactive", "Active Reporting", "Active - Not Reporting")
    ),
    label = "All statuses in method2 should be valid"
  )

  testthat::expect_true(
    all(
      result$activity_status_method3 %in%
        c("Inactive", "Active Reporting", "Active - Not Reporting")
    ),
    label = "All statuses in method3 should be valid"
  )

  # --- structural ---
  testthat::expect_equal(
    nrow(result),
    nrow(df1),
    label = "Output should preserve row count"
  )
})


testthat::test_that(
  "classify_facility_activity behaves as expected for all methods (2/2)", {
  # read in data
  df <- file.path(
    system.file("extdata", package = "sntutils"),
    "hf_active_df.rds"
  ) |>
    readRDS()

  df1 <- df$df1
  df2 <- df$df2

  # --- run the classifier ---
  result <- classify_facility_activity(
    df2,
    hf_col = "hf",
    date_col = "date",
    key_indicators = "reported any?",
    method = "all",
    nonreport_window = 6,
    reporting_rule = "any_non_na",
    binary_classification = FALSE
  ) |>
    as.data.frame() |>
    dplyr::mutate(
       method1 = normalize_text(method1),
      method2 = normalize_text(method2),
      method3 = normalize_text(method3),
      activity_status_method1 = normalize_text(activity_status_method1),
      activity_status_method2 = normalize_text(activity_status_method2),
      activity_status_method3 = normalize_text(activity_status_method3)
    )

  # --- method 1 tests ---
  testthat::expect_equal(
    result$activity_status_method1,
    result$method1,
    label = "Method 1 should match expected reference"
  )

  # --- method 2 tests ---
  testthat::expect_equal(
    result$activity_status_method2,
    result$method2,
    label = "Method 2 should match expected reference"
  )

  # --- method 3 tests ---
  testthat::expect_equal(
    result$activity_status_method3,
    result$method3,
    label = "Method 3 should match expected reference"
  )

  # --- logical consistency checks ---
  testthat::expect_true(
    all(
      result$activity_status_method1 %in%
        c("Inactive", "Active Reporting", "Active - Not Reporting")
    ),
    label = "All statuses in method1 should be valid"
  )

  testthat::expect_true(
    all(
      result$activity_status_method2 %in%
        c("Inactive", "Active Reporting", "Active - Not Reporting")
    ),
    label = "All statuses in method2 should be valid"
  )

  testthat::expect_true(
    all(
      result$activity_status_method3 %in%
        c("Inactive", "Active Reporting", "Active - Not Reporting")
    ),
    label = "All statuses in method3 should be valid"
  )

  # --- structural ---
  testthat::expect_equal(
    nrow(result),
    nrow(df2),
    label = "Output should preserve row count"
  )
})


testthat::test_that("facilities that never report remain inactive in all methods", {
  df_never <- tibble::tibble(
    hf = "hf_never",
    date = seq(as.Date("2022-01-01"), by = "month", length.out = 12),
    reported = NA
  )

  res <- classify_facility_activity(
    df_never,
    hf_col = "hf",
    key_indicators = "reported",
    method = "all"
  )

  expected <- rep("Inactive", nrow(df_never))
  testthat::expect_equal(unique(res$activity_status_method1), "Inactive")
  testthat::expect_equal(unique(res$activity_status_method2), "Inactive")
  testthat::expect_equal(unique(res$activity_status_method3), "Inactive")
})

testthat::test_that("facilities reporting once handled correctly", {
  df_once <- tibble::tibble(
    hf = "hf_once",
    date = seq(as.Date("2022-01-01"), by = "month", length.out = 12),
    reported = c("yes", rep(NA, 11))
  )

  res <- classify_facility_activity(
    df_once,
    hf_col = "hf",
    key_indicators = "reported",
    method = "all"
  ) |>
    dplyr::mutate(
      activity_status_method1 = normalize_text(activity_status_method1)
    )

  # Method 1 → stays active after first report
  testthat::expect_equal(
    res$activity_status_method1[2],
    "Active - Not Reporting"
  )

  # Method 2 → only active at reporting month
  testthat::expect_equal(unique(res$activity_status_method2[-1]), "Inactive")

  # Method 3 → becomes inactive after 6 months gap
  testthat::expect_equal(res$activity_status_method3[8], "Inactive")
})

testthat::test_that("multiple facilities are classified independently", {
  df_multi <- tibble::tibble(
    hf = rep(c("A", "B"), each = 6),
    date = rep(seq(as.Date("2022-01-01"), by = "month", length.out = 6), 2),
    reported = c("yes", "yes", NA, NA, NA, NA, NA, NA, NA, "yes", "yes", NA)
  )

  res <- classify_facility_activity(
    df_multi,
    hf_col = "hf",
    key_indicators = "reported",
    method = "all"
  )

  # Each hf should have its own activation pattern
  grouped <- res |>
    dplyr::group_by(hf) |>
    dplyr::summarise(unique_status = list(unique(activity_status_method1)))
  testthat::expect_equal(length(unique(grouped$unique_status)), 2)
})

testthat::test_that("method 3 respects nonreport window boundaries", {
  df_edge <- tibble::tibble(
    hf = "hf_edge",
    date = seq(as.Date("2022-01-01"), by = "month", length.out = 12),
    reported = c("yes", rep(NA, 6), "yes", rep(NA, 4))
  )

  res <- classify_facility_activity(
    df_edge,
    hf_col = "hf",
    key_indicators = "reported",
    method = 3,
    nonreport_window = 6
  )

  # 6-month gap still active, 7th month inactive
  testthat::expect_equal(res$activity_status[7], "Inactive")
  testthat::expect_equal(res$activity_status[8], "Active Reporting")
})

testthat::test_that("missing months are filled correctly", {
  df_sparse <- tibble::tibble(
    hf = "hf_sparse",
    date = seq(as.Date("2022-01-01"), by = "2 months", length.out = 6),
    reported = c("yes", NA, "yes", NA, "yes", NA)
  )

  res <- classify_facility_activity(
    df_sparse,
    hf_col = "hf",
    key_indicators = "reported",
    method = 1
  )
  # Expect monthly panel length from Jan to Nov
  testthat::expect_equal(nrow(res), 11)
})

testthat::test_that("reporting rule positive_only excludes zeros", {
  df_zero <- tibble::tibble(
    hf = "hf_zero",
    date = seq(as.Date("2022-01-01"), by = "month", length.out = 6),
    reported = c(0, 1, 0, NA, 2, 0)
  )

  res1 <- classify_facility_activity(
    df_zero,
    hf_col = "hf",
    key_indicators = "reported",
    reporting_rule = "any_non_na"
  )
  res2 <- classify_facility_activity(
    df_zero,
    hf_col = "hf",
    key_indicators = "reported",
    reporting_rule = "positive_only"
  )

  # should differ in how zeros are treated
  testthat::expect_false(identical(res1$activity_status, res2$activity_status))
})

test_that("method1 activates permanently after first report", {
  df <- tibble::tibble(
    hf = "A",
    date = seq(as.Date("2022-01-01"), by = "month", length.out = 6),
    test = c(NA, NA, 1, NA, NA, NA)
  )

  res <- classify_facility_activity(
    df,
    hf_col = "hf",
    date_col = "date",
    key_indicators = "test",
    method = 1
  )

  # becomes active from March onwards
  expect_equal(
    unique(res$activity_status[res$date < as.Date("2022-03-01")]),
    "Inactive"
  )
  expect_true(all(
    res$activity_status[res$date >= as.Date("2022-03-01")] != "Inactive"
  ))
})

test_that("method2 restricts active period between first and last reports", {
  df <- tibble::tibble(
    hf = "A",
    date = seq(as.Date("2022-01-01"), by = "month", length.out = 8),
    test = c(NA, 1, NA, 0, NA, NA, 2, NA)
  )

  res <- classify_facility_activity(
    df,
    hf_col = "hf",
    date_col = "date",
    key_indicators = "test",
    method = 2
  )

  # inactive before first and after last
  expect_equal(res$activity_status[1], "Inactive")
  expect_equal(tail(res$activity_status, 1), "Active – Not Reporting")

  # active in between
  expect_true(any(res$activity_status == "Active Reporting"))
})

test_that("method3 deactivates after >= nonreport_window months missing", {
  df <- tibble::tibble(
    hf = "A",
    date = seq(as.Date("2022-01-01"), by = "month", length.out = 15),
    test = c(1, rep(NA, 6), 1, rep(NA, 7))
  )

  res <- classify_facility_activity(
    df,
    hf_col = "hf",
    date_col = "date",
    key_indicators = "test",
    method = 3,
    nonreport_window = 6
  )

  # first long gap causes inactivity
  expect_true(any(res$activity_status[7:8] == "Inactive"))

  # reactivation after report resumes
  expect_equal(res$activity_status[8], "Active Reporting")

  # second long gap -> inactive again
  expect_equal(tail(res$activity_status, 1), "Inactive")
})

test_that("method3 tolerant when nonreport_window larger", {
  df <- tibble::tibble(
    hf = "A",
    date = seq(as.Date("2022-01-01"), by = "month", length.out = 15),
    test = c(1, rep(NA, 6), 1, rep(NA, 7))
  )

  res <- classify_facility_activity(
    df,
    hf_col = "hf",
    date_col = "date",
    key_indicators = "test",
    method = 3,
    nonreport_window = 12
  )

  # should not go inactive until long final gap exceeds window
  expect_true(any(res$activity_status[3:7] == "Active – Not Reporting"))
  expect_equal(tail(res$activity_status, 1), "Active – Not Reporting")
})

test_that("facilities that never report remain inactive", {
  df <- tibble::tibble(
    hf = "A",
    date = seq(as.Date("2022-01-01"), by = "month", length.out = 6),
    test = NA
  )

  res <- classify_facility_activity(
    df,
    hf_col = "hf",
    date_col = "date",
    key_indicators = "test",
    method = "all"
  )

  expect_true(all(
    dplyr::select(res, dplyr::starts_with("activity_status_")) == "Inactive"
  ))
})

test_that("positive_only rule ignores zeros", {
  df <- tibble::tibble(
    hf = "A",
    date = seq(as.Date("2022-01-01"), by = "month", length.out = 3),
    test = c(0, 1, 0)
  )

  res <- classify_facility_activity(
    df,
    hf_col = "hf",
    date_col = "date",
    key_indicators = "test",
    method = 1,
    reporting_rule = "positive_only"
  )

  expect_true(any(res$activity_status == "Inactive"))
})

test_that("no NA classifications appear (robust fallback)", {
  df <- tibble::tibble(
    hf = "A",
    date = seq(as.Date("2022-01-01"), by = "month", length.out = 6),
    test = c(1, rep(NA, 5))
  )

  res <- classify_facility_activity(
    df,
    hf_col = "hf",
    date_col = "date",
    key_indicators = "test",
    method = "all"
  )

  expect_false(any(is.na(dplyr::select(
    res,
    dplyr::starts_with("activity_status_")
  ))))
})


test_that("classification works correctly even when dates are in random order", {
  # create out-of-order dataset
  df <- tibble::tibble(
    hf = rep("A", 8),
    date = sample(seq(as.Date("2022-01-01"), by = "month", length.out = 8)),
    test = c(NA, 1, NA, 0, NA, NA, 2, NA)
  )

  # method 3 is most sensitive to ordering
  res <- classify_facility_activity(
    df,
    hf_col = "hf",
    date_col = "date",
    key_indicators = "test",
    method = "all",
    nonreport_window = 3
  )

  # ensure all outputs contain valid categories (no NA)
  expect_false(any(is.na(dplyr::select(
    res,
    dplyr::starts_with("activity_status_")
  ))))


  # method consistency check
  expect_true(all(
    res$activity_status_method1 %in%
      c(
        "Inactive",
        "Active Reporting",
        "Active – Not Reporting",
        "Active - Not Reporting"
      )
  ))
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
