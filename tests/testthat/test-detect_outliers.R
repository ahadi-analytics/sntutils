# tests for the refactored outlier detection workflow

create_test_data <- function() {
  set.seed(123)

  grid <- expand.grid(
    adm1 = c("region_a", "region_b"),
    adm2 = c("district_1", "district_2"),
    year = c(2021, 2022),
    month = 1:6,
    replicate = 1:3,
    KEEP.OUT.ATTRS = FALSE
  )

  tibble::as_tibble(grid) |>
    dplyr::arrange(adm1, adm2, year, month, replicate) |>
    dplyr::mutate(
      record_id = sprintf("rec_%03d", dplyr::row_number()),
      yearmon = sprintf("%d-%02d", year, month),
      date = as.Date(paste0(year, "-", sprintf("%02d", month), "-01")),
      confirmed_cases = rpois(dplyr::n(), lambda = 40) +
        dplyr::if_else(adm2 == "district_2", 15, 0),
      reporting_rate = stats::runif(dplyr::n(), 0.7, 1)
    ) |>
    dplyr::select(record_id, adm1, adm2, yearmon, date, year, month,
                  confirmed_cases, reporting_rate)
}

seasonality_test_data <- function() {
  # limited history per month to trigger fallbacks
  tibble::tibble(
    record_id = sprintf("season_%02d", seq_len(12)),
    adm1 = "region_a",
    adm2 = "district_1",
    yearmon = rep(c("2021-01", "2021-02", "2021-03"), each = 4),
    date = as.Date(rep(c("2021-01-01", "2021-02-01", "2021-03-01"), each = 4)),
    year = rep(2021, 12),
    month = rep(1:3, each = 4),
    confirmed_cases = c(10, 12, 11, 13, 20, 18, 200, 22, 15, 13, 12, 11),
    reporting_rate = c(rep(0.9, 10), 0.6, 0.6)
  )
}

low_reporting_data <- function() {
  create_test_data() |>
    dplyr::mutate(
      reporting_rate = dplyr::if_else(
        record_id %in% c("rec_001", "rec_002"),
        0.3,
        reporting_rate
      )
    )
}

# detect_outliers -----------------------------------------------------------

testthat::test_that("detect_outliers returns consensus output by default", {
  data <- create_test_data()

  result <- sntutils::detect_outliers(
    data = data,
    column = "confirmed_cases"
  )

  testthat::expect_s3_class(result, "tbl_df")
  expected_cols <- c(
    "record_id", "adm1", "adm2", "month",
    "column_name", "value", "value_type", "outlier_flag_consensus", "reason",
    "outlier_flag_mean", "outlier_flag_median", "outlier_flag_iqr",
    "mean_lower", "mean_upper", "median_lower", "median_upper",
    "iqr_lower", "iqr_upper", "n_in_group",
    "seasonality_mode_used", "seasonal_window_desc"
  )

  testthat::expect_true(all(expected_cols %in% names(result)))
  testthat::expect_true("outlier_flag_mean" %in% names(result))
})

testthat::test_that("detect_outliers returns method flags when requested", {
  data <- create_test_data()

  result <- sntutils::detect_outliers(
    data = data,
    column = "confirmed_cases",
    output_profile = "audit",
    reporting_rate_col = "reporting_rate"
  )

  testthat::expect_true(all(
    c("outlier_flag_mean", "outlier_flag_median", "outlier_flag_iqr") %in%
      names(result)
  ))

  testthat::expect_true(
    all(result$reason[!is.na(result$reason)] %in%
      c("low_reporting", "insufficient_n", "unstable_scale"))
  )
})

testthat::test_that("guardrails flag low reporting", {
  data <- low_reporting_data()

  result <- sntutils::detect_outliers(
    data = data,
    column = "confirmed_cases",
    reporting_rate_col = "reporting_rate",
    reporting_rate_min = 0.5,
    output_profile = "audit"
  )

  flagged <- result |>
    dplyr::filter(reporting_rate < 0.5)

  testthat::expect_true(all(flagged$reason == "low_reporting"))
  testthat::expect_true(all(flagged$outlier_flag_consensus == "low_reporting"))
})

testthat::test_that("seasonality ladder records fallback metadata", {
  data <- seasonality_test_data()

  result <- sntutils::detect_outliers(
    data = data,
    column = "confirmed_cases",
    admin_level = c("adm1", "adm2"),
    reporting_rate_col = "reporting_rate",
    time_mode = "by_month",
    min_years_per_month = 3,
    output_profile = "audit"
  )

  testthat::expect_true(
    any(result$seasonality_mode_used != "same_month", na.rm = TRUE)
  )
  testthat::expect_true(all(!is.na(result$seasonal_window_desc)))
})

testthat::test_that("time_mode switches pooling", {
  data <- create_test_data()

  across <- sntutils::detect_outliers(
    data = data,
    column = "confirmed_cases",
    time_mode = "across_time"
  )

  within <- sntutils::detect_outliers(
    data = data,
    column = "confirmed_cases",
    time_mode = "within_year",
    output_profile = "audit"
  )

  testthat::expect_true(
    any(within$seasonality_mode_used == "within_year")
  )
  testthat::expect_true(
    unique(across$seasonality_mode_used) == "across_time"
  )
})

# outlier_plot -------------------------------------------------------------

testthat::test_that("outlier_plot returns ggplot object", {
  data <- create_test_data()

  plot <- sntutils::outlier_plot(
    data = data,
    column = "confirmed_cases",
    reporting_rate_col = "reporting_rate",
    reporting_rate_min = 0.6,
    methods = "iqr",
    show_plot = FALSE
  )

  testthat::expect_s3_class(plot, "ggplot")
  testthat::expect_no_error(ggplot2::ggplot_build(plot))
})

testthat::test_that("outlier_plot works with admin_level parameter", {
  data <- create_test_data()

  # First get outlier detection results
  detection_results <- sntutils::detect_outliers(
    data = data,
    column = "confirmed_cases",
    admin_level = c("adm1", "adm2"),
    methods = "iqr"
  )

  original_sample_size <- nrow(detection_results)

  # Create plot with admin_level parameter
  plot <- sntutils::outlier_plot(
    data = data,
    column = "confirmed_cases",
    admin_level = c("adm1", "adm2"),
    methods = "iqr",
    show_plot = FALSE
  )

  # Extract the underlying data from the plot
  plot_data <- ggplot2::ggplot_build(plot)$data[[1]]

  # The plot should work correctly with admin_level parameter
  testthat::expect_s3_class(plot, "ggplot")
  testthat::expect_true(nrow(plot_data) > 0)
})
