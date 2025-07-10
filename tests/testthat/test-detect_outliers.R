# tests for outlier detection functions
# objective: comprehensive testing of detect_outliers and outlier_plot functions
# author: assistant
# date: 2025-07-10

# setup test data ----
create_test_data <- function(n = 100) {
  # create realistic test dataset with known outliers
  set.seed(42)

  tibble::tibble(
    record_id = paste0("rec_", seq_len(n)),
    adm1 = sample(c("region_a", "region_b"), n, replace = TRUE),
    adm2 = sample(
      c("district_1", "district_2", "district_3"),
      n,
      replace = TRUE
    ),
    yearmon = sample(c("2023-01", "2023-02", "2023-03"), n, replace = TRUE),
    year = sample(c("2023", "2024"), n, replace = TRUE),
    # normal values with some extreme outliers
    confirmed_cases = c(
      stats::rnorm(n - 10, mean = 50, sd = 10),
      c(200, 250, 300, -10, 0, 500, 600, 1000, 1500, 2000)
    ),
    # column with na values
    cases_with_na = c(
      stats::rnorm(n - 15, mean = 30, sd = 5),
      rep(NA, 10),
      c(150, 180, 200, 220, 250)
    ),
    # non-numeric column
    text_column = sample(letters[1:5], n, replace = TRUE)
  )
}

# test detect_outliers function ----
testthat::test_that("detect_outliers returns correct structure", {
  test_data <- create_test_data()

  result <- detect_outliers(
    data = test_data,
    column = "confirmed_cases"
  )

  # check return type
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_s3_class(result, "tbl_df")

  # check required columns exist
  expected_cols <- c(
    "record_id",
    "adm1",
    "adm2",
    "yearmon",
    "year",
    "column_name",
    "value",
    "outliers_moyenne",
    "outliers_halper",
    "outliers_iqr",
    "moyenne_lower_bound",
    "moyenne_upper_bound",
    "hampel_lower_bound",
    "hampel_upper_bound",
    "iqr_lower_bound",
    "iqr_upper_bound",
    "iqr"
  )

  testthat::expect_true(all(expected_cols %in% names(result)))
  testthat::expect_equal(ncol(result), length(expected_cols))
})

testthat::test_that("detect_outliers handles missing columns", {
  test_data <- create_test_data()

  # remove required column
  test_data_incomplete <- test_data |>
    dplyr::select(-adm1)

  testthat::expect_error(
    detect_outliers(
      data = test_data_incomplete,
      column = "confirmed_cases"
    ),
    regexp = "Missing columns"
  )
})

testthat::test_that("detect_outliers handles non-numeric columns", {
  test_data <- create_test_data()

  result <- detect_outliers(
    data = test_data,
    column = "text_column"
  )

  testthat::expect_null(result)
})

testthat::test_that("detect_outliers handles na values correctly", {
  test_data <- create_test_data()

  result <- detect_outliers(
    data = test_data,
    column = "cases_with_na"
  )

  # should filter out na values
  testthat::expect_true(all(!is.na(result$value)))
  testthat::expect_equal(nrow(result), 90) # 100 - 5 na values
})

testthat::test_that("detect_outliers calculates statistics correctly", {
  # create simple test case with known values
  simple_data <- tibble::tibble(
    record_id = paste0("rec_", 1:10),
    adm1 = rep("region_a", 10),
    adm2 = rep("district_1", 10),
    yearmon = rep("2023-01", 10),
    year = rep("2023", 10),
    test_values = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 100) # 100 is clear outlier
  )

  result <- detect_outliers(
    data = simple_data,
    column = "test_values"
  )

  # check that outlier methods identify the extreme value
  outlier_row <- result |>
    dplyr::filter(value == 100)

  testthat::expect_equal(nrow(outlier_row), 1)
  testthat::expect_equal(outlier_row$outliers_iqr, "outlier")
  testthat::expect_equal(outlier_row$outliers_moyenne, "normal value")

  # check bounds are calculated
  testthat::expect_true(all(!is.na(result$iqr_lower_bound)))
  testthat::expect_true(all(!is.na(result$iqr_upper_bound)))
  testthat::expect_true(all(!is.na(result$moyenne_lower_bound)))
  testthat::expect_true(all(!is.na(result$moyenne_upper_bound)))
})

testthat::test_that("detect_outliers custom iqr_multiplier works", {
  test_data <- create_test_data()

  result_default <- detect_outliers(
    data = test_data,
    column = "confirmed_cases",
    iqr_multiplier = 1.5
  )

  result_strict <- detect_outliers(
    data = test_data,
    column = "confirmed_cases",
    iqr_multiplier = 3.0
  )

  # stricter multiplier should detect fewer outliers
  outliers_default <- sum(result_default$outliers_iqr == "outlier")
  outliers_strict <- sum(result_strict$outliers_iqr == "outlier")

  testthat::expect_gte(outliers_default, outliers_strict)
})

testthat::test_that("detect_outliers custom column names work", {
  test_data <- create_test_data() |>
    dplyr::rename(
      custom_id = record_id,
      region = adm1,
      district = adm2,
      date = yearmon,
      yr = year
    )

  result <- detect_outliers(
    data = test_data,
    column = "confirmed_cases",
    record_id = "custom_id",
    adm1 = "region",
    adm2 = "district",
    yearmon = "date",
    year = "yr"
  )

  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_true("custom_id" %in% names(result))
  testthat::expect_true("region" %in% names(result))
})

testthat::test_that("detect_outliers grouping works correctly", {
  # create data with different groups
  grouped_data <- tibble::tibble(
    record_id = paste0("rec_", 1:20),
    adm1 = rep(c("region_a", "region_b"), each = 10),
    adm2 = rep(c("district_1", "district_2"), each = 10),
    yearmon = rep("2023-01", 20),
    year = rep("2023", 20),
    # group 1 has different distribution than group 2
    test_values = c(
      stats::rnorm(10, mean = 10, sd = 2), # group 1: low values
      stats::rnorm(10, mean = 100, sd = 5) # group 2: high values
    )
  )

  result <- detect_outliers(
    data = grouped_data,
    column = "test_values"
  )

  # check that statistics are calculated per group
  group_stats <- result |>
    dplyr::group_by(adm1, adm2, year) |>
    dplyr::summarise(
      mean_val = mean(value),
      .groups = "drop"
    )

  testthat::expect_equal(nrow(group_stats), 2)
  testthat::expect_true(
    abs(group_stats$mean_val[1] - group_stats$mean_val[2]) > 50
  )
})

# test outlier_plot function ----
testthat::test_that("outlier_plot returns correct structure", {
  test_data <- create_test_data()

  result <- outlier_plot(
    data = test_data,
    column = "confirmed_cases",
    methods = c("iqr", "moyenne")
  )

  testthat::expect_type(result, "list")
  testthat::expect_equal(length(result), 2)
  testthat::expect_true(all(c("iqr", "moyenne") %in% names(result)))

  # check each plot is a ggplot object
  purrr::walk(result, ~ testthat::expect_s3_class(.x, "ggplot"))
})

testthat::test_that("outlier_plot handles single method", {
  test_data <- create_test_data()

  result <- outlier_plot(
    data = test_data,
    column = "confirmed_cases",
    methods = "iqr"
  )

  testthat::expect_type(result, "list")
  testthat::expect_equal(length(result), 1)
  testthat::expect_true("iqr" %in% names(result))
  testthat::expect_s3_class(result$iqr, "ggplot")
})

testthat::test_that("outlier_plot handles all methods", {
  test_data <- create_test_data()

  result <- outlier_plot(
    data = test_data,
    column = "confirmed_cases",
    methods = c("iqr", "halper", "moyenne")
  )

  testthat::expect_equal(length(result), 3)
  testthat::expect_true(all(c("iqr", "halper", "moyenne") %in% names(result)))
})

testthat::test_that("outlier_plot handles custom parameters", {
  test_data <- create_test_data() |>
    dplyr::rename(
      custom_id = record_id,
      region = adm1,
      district = adm2
    )

  result <- outlier_plot(
    data = test_data,
    column = "confirmed_cases",
    record_id = "custom_id",
    adm1 = "region",
    adm2 = "district",
    methods = "iqr",
    iqr_multiplier = 2.0
  )

  testthat::expect_s3_class(result$iqr, "ggplot")
})

testthat::test_that("outlier_plot filters na and zero values", {
  test_data <- create_test_data()

  # add some zero and negative values
  test_data_with_zeros <- test_data |>
    dplyr::mutate(
      confirmed_cases = dplyr::if_else(
        dplyr::row_number() <= 5,
        0,
        confirmed_cases
      )
    )

  # should not error and should filter out zeros
  testthat::expect_no_error({
    result <- outlier_plot(
      data = test_data_with_zeros,
      column = "confirmed_cases",
      methods = "iqr"
    )
  })

  testthat::expect_s3_class(result$iqr, "ggplot")
})

# edge cases and error handling ----
testthat::test_that("detect_outliers handles empty data", {
  empty_data <- tibble::tibble(
    record_id = character(0),
    adm1 = character(0),
    adm2 = character(0),
    yearmon = character(0),
    year = character(0),
    test_values = numeric(0)
  )

  result <- detect_outliers(
    data = empty_data,
    column = "test_values"
  )

  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_equal(nrow(result), 0)
})

testthat::test_that("detect_outliers handles single row", {
  single_row <- tibble::tibble(
    record_id = "rec_1",
    adm1 = "region_a",
    adm2 = "district_1",
    yearmon = "2023-01",
    year = "2023",
    test_values = 100
  )

  result <- detect_outliers(
    data = single_row,
    column = "test_values"
  )

  testthat::expect_equal(nrow(result), 1)
  testthat::expect_equal(result$outliers_iqr, "normal value")
})

testthat::test_that("detect_outliers handles all same values", {
  same_values <- tibble::tibble(
    record_id = paste0("rec_", 1:10),
    adm1 = rep("region_a", 10),
    adm2 = rep("district_1", 10),
    yearmon = rep("2023-01", 10),
    year = rep("2023", 10),
    test_values = rep(50, 10)
  )

  result <- detect_outliers(
    data = same_values,
    column = "test_values"
  )

  # all values should be classified as normal
  testthat::expect_true(all(result$outliers_iqr == "normal value"))
  testthat::expect_true(all(result$outliers_moyenne == "normal value"))
  testthat::expect_true(all(result$outliers_halper == "normal value"))
})

testthat::test_that("functions handle invalid column names gracefully", {
  test_data <- create_test_data()

  testthat::expect_error(
    detect_outliers(
      data = test_data,
      column = "nonexistent_column"
    ),
    regexp = "Missing columns"
  )
})

# performance and data integrity tests ----
testthat::test_that("detect_outliers preserves data integrity", {
  test_data <- create_test_data()
  original_rows <- nrow(test_data)

  result <- detect_outliers(
    data = test_data,
    column = "confirmed_cases"
  )

  # should not lose any non-na records
  expected_rows <- test_data |>
    dplyr::filter(!is.na(confirmed_cases)) |>
    nrow()

  testthat::expect_equal(nrow(result), expected_rows)

  # check that original values are preserved
  testthat::expect_equal(
    sort(result$value),
    sort(test_data$confirmed_cases[!is.na(test_data$confirmed_cases)])
  )
})

testthat::test_that("outlier detection methods are consistent", {
  test_data <- create_test_data()

  result <- detect_outliers(
    data = test_data,
    column = "confirmed_cases"
  )

  # each outlier classification should be either "outlier" or "normal value"
  valid_values <- c("outlier", "normal value")

  testthat::expect_true(all(result$outliers_iqr %in% valid_values))
  testthat::expect_true(all(result$outliers_moyenne %in% valid_values))
  testthat::expect_true(all(result$outliers_halper %in% valid_values))

  # bounds should be properly ordered
  testthat::expect_true(all(result$iqr_lower_bound <= result$iqr_upper_bound))
  testthat::expect_true(
    all(result$moyenne_lower_bound <= result$moyenne_upper_bound)
  )
  testthat::expect_true(
    all(result$hampel_lower_bound <= result$hampel_upper_bound)
  )
})

# integration tests ----
testthat::test_that("detect_outliers and outlier_plot work together", {
  test_data <- create_test_data()

  # run detect_outliers first
  outliers <- detect_outliers(
    data = test_data,
    column = "confirmed_cases"
  )

  testthat::expect_s3_class(outliers, "data.frame")

  # then run outlier_plot
  plots <- outlier_plot(
    data = test_data,
    column = "confirmed_cases",
    methods = c("iqr", "moyenne")
  )

  testthat::expect_type(plots, "list")
  testthat::expect_equal(length(plots), 2)

  # both functions should work on the same data without errors
  testthat::expect_no_error({
    combined_workflow <- test_data |>
      {
        \(x) detect_outliers(x, "confirmed_cases")
      }() |>
      {
        \(x) outlier_plot(test_data, "confirmed_cases", methods = "iqr")
      }()
  })
})
