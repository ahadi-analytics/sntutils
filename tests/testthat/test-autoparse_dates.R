testthat::test_that("autoparse_dates handles basic date formats correctly", {
  # Setup test data
  df <- data.frame(
    date1 = c("2023-10-03", "2022-09-11"),
    date2 = c("03.10.2023", "11.09.2022"),
    stringsAsFactors = FALSE
  )

  # Test basic functionality
  result <- autoparse_dates(df, date_cols = c("date1", "date2"))

  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_true(all(lubridate::is.Date(result$date1)))
  testthat::expect_true(all(lubridate::is.Date(result$date2)))
  testthat::expect_equal(
    result$date1,
    as.Date(c("2023-10-03", "2022-09-11"))
  )
})

testthat::test_that("autoparse_dates handles different output formats", {
  df <- data.frame(
    date = c("2023-10-03", "2022-09-11"),
    stringsAsFactors = FALSE
  )

  result <- autoparse_dates(
    df,
    date_cols = "date",
    output_format = "%d/%m/%Y"
  )

  testthat::expect_type(result$date, "character")
  testthat::expect_equal(
    result$date,
    c("03/10/2023", "11/09/2022")
  )
})

testthat::test_that("autoparse_dates handles invalid dates appropriately", {
  df <- data.frame(
    date = c("2023-13-45", "invalid_date"),
    stringsAsFactors = FALSE
  )

  testthat::expect_message(
    result <- autoparse_dates(df, date_cols = "date", verbose = TRUE),
    "Warning: 2 dates could not be parsed in column 'date'"
  )

  testthat::expect_true(all(is.na(result$date)))
})

testthat::test_that("autoparse_dates handles additional formats", {
  df <- data.frame(
    date = c("2023:10:03", "2022:09:11"),
    stringsAsFactors = FALSE
  )

  result <- autoparse_dates(
    df,
    date_cols = "date",
    additional_format = "%Y:%m:%d"
  )

  testthat::expect_equal(
    result$date,
    as.Date(c("2023-10-03", "2022-09-11"))
  )
})

testthat::test_that("autoparse_dates validates inputs correctly", {
  df <- data.frame(x = 1:2)

  # Test invalid date_cols
  testthat::expect_error(
    autoparse_dates(df, date_cols = "non_existent_column")
  )

  # Test invalid data input
  testthat::expect_error(
    autoparse_dates("not_a_dataframe", date_cols = "x")
  )
})

testthat::test_that("autoparse_dates handles mixed formats in same column", {
  df <- data.frame(
    dates = c(
      "2023-10-03",
      "03.10.2023",
      "2023/10/03",
      "03-10-23"
    ),
    stringsAsFactors = FALSE
  )

  result <- autoparse_dates(df, date_cols = "dates")

  testthat::expect_equal(
    result$dates,
    rep(as.Date("2023-10-03"), 4)
  )
})

testthat::test_that("autoparse_dates handles datetime formats", {
  df <- data.frame(
    datetime = c(
      "2023-10-03 14:30:00",
      "2022-09-11 05:45:12"
    ),
    stringsAsFactors = FALSE
  )

  result <- autoparse_dates(df, date_cols = "datetime")

  testthat::expect_equal(
    result$datetime,
    as.Date(c("2023-10-03", "2022-09-11"))
  )
})
