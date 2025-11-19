# tests/testthat/test-reporting_rate-weighting.R
# validates weighted reporting rate calculations

testthat::test_that(
  "weighted reporting rates rescale eligible weights",
  {
    testthat::skip_if_not_installed("slider")

    test_data <- tibble::tribble(
      ~yearmon, ~district, ~hf_id, ~allout, ~conf,
      "2023-01", "North", "HF1", 100, 10,
      "2023-01", "North", "HF2", 50, 0,
      "2023-01", "North", "HF3", 300, NA,
      "2023-02", "North", "HF1", 120, 8,
      "2023-02", "North", "HF2", 45, NA,
      "2023-02", "North", "HF3", 310, 5
    )

    weighted_result <- calculate_reporting_metrics(
      data = test_data,
      vars_of_interest = "conf",
      x_var = "yearmon",
      y_var = "district",
      hf_col = "hf_id",
      key_indicators = "conf",
      weighting = TRUE,
      weight_var = "allout",
      weight_window = 1,
      exclude_current_x = FALSE
    )

    jan_row <- weighted_result |>
      dplyr::filter(yearmon == "2023-01")

    testthat::expect_equal(
      jan_row$reprate_w,
      1,
      tolerance = 1e-6
    )

    expected_cols <- c(
      "yearmon", "district", "rep", "exp",
      "reprate", "missrate", "reprate_w", "missrate_w",
      "avg_allout", "min_allout", "max_allout"
    )

    testthat::expect_equal(names(weighted_result), expected_cols)
  }
)

testthat::test_that(
  "column order stays consistent for unweighted outputs",
  {
    test_data <- tibble::tribble(
      ~month, ~malaria,
      "Jan", NA_real_,
      "Feb", NA_real_,
      "Mar", 10
    )

    metrics <- calculate_reporting_metrics(
      data = test_data,
      vars_of_interest = "malaria",
      x_var = "month",
      hf_col = NULL
    )

    testthat::expect_equal(
      names(metrics),
      c("month", "variable", "exp", "rep", "reprate", "missrate")
    )

    testthat::expect_true(
      all(metrics$reprate >= 0 | is.na(metrics$reprate))
    )
  }
)
