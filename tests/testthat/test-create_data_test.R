
testthat::test_that("create_data_test function works correctly", {
  # Create sample data
  test_data <- data.frame(
    month = rep(1:12, 5),
    year = rep(2020:2024, each = 12),
    district = rep(letters[1:5], each = 12),
    value = 1:60,
    cholera_cases = sample(0:100, 60),
    cholera_tests = sample(0:1000, 60),
    malaria_tests = sample(0:1000, 60)
  )

  # Test dimension check
  dim_test <- create_data_test(dimension_test = c(60, 7))
  testthat::expect_no_error(dim_test(test_data))

  # Test combinations check
  comb_test <- create_data_test(combinations_test = list(
    variables = c("month", "year"),
    expectation = 60
  ))
  testthat::expect_no_error(comb_test(test_data))

  # Test duplicates check
  dup_test <- create_data_test(row_duplicates = TRUE, col_duplicates = TRUE)
  testthat::expect_no_error(dup_test(test_data))

  # Test threshold checks
  thresh_test <- create_data_test(
    min_threshold_test = list(cholera_cases = 0),
    max_threshold_test = list(cholera_tests = 1000)
  )
  testthat::expect_no_error(thresh_test(test_data))


  # Test empty test function
  testthat::expect_error(create_data_test()())

  # Test with invalid column names
  invalid_thresh_test <- create_data_test(
    min_threshold_test = list(nonexistent = 0)
  )
  testthat::expect_error(invalid_thresh_test(test_data))

  # Test return value structure
  test_fn <- create_data_test(dimension_test = c(60, 7))
  testthat::expect_type(test_fn, "closure")
})

testthat::test_that("edge cases are handled correctly", {
  # Empty dataframe
  empty_df <- data.frame()
  empty_test <- create_data_test(dimension_test = c(0, 0))
  testthat::expect_no_error(empty_test(empty_df))

  # Single row/column dataframe
  single_df <- data.frame(x = 1)
  single_test <- create_data_test(dimension_test = c(1, 1))
  testthat::expect_no_error(single_test(single_df))

  # Duplicate rows/columns
  dup_df <- data.frame(
    x = c(1,1),
    y = c(1,1)
  )
  dup_test <- create_data_test(row_duplicates = TRUE, col_duplicates = TRUE)
  testthat::expect_no_error(dup_test(dup_df))
})

testthat::test_that("threshold tests work correctly", {
  test_df <- data.frame(
    x = 1:10,
    y = c(NA, 2:10)
  )

  # Test min threshold
  min_test <- create_data_test(min_threshold_test = list(x = 5))
  testthat::expect_no_error(min_test(test_df))

  # Test max threshold
  max_test <- create_data_test(max_threshold_test = list(x = 5))
  testthat::expect_no_error(max_test(test_df))

  # Test with NA values
  na_test <- create_data_test(min_threshold_test = list(y = 5))
  testthat::expect_no_error(na_test(test_df))
})
