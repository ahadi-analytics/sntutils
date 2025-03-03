testthat::test_that("calculate_string_distance works correctly", {
  # get function output
  result <- calculate_string_distance(
    c("New York", "Los Angeles"),
    c("New York", "Los Angeles", "Chicago"), "lv"
  )

  # expected outputs based on function results
  expected <- data.frame(
    algorithm_name = rep("lv", 6),
    name_to_match = rep(c("New York", "Los Angeles"), each = 3),
    matched_names = c(
      "New York", "Chicago",
      "Los Angeles", "Los Angeles", "New York", "Chicago"
    ),
    distance = c(0, 8, 10, 0, 8, 10),
    match_rank = rep(1:3, 2)
  )

  # Test if the output matches expected data frame
  testthat::expect_equal(
    result$matched_names,
    c("Los Angeles", "New York", "Chicago", "New York",
      "Chicago", "Los Angeles")
  )
})

testthat::test_that("Test administrative matching stats output", {
  # setup mock data
  data <- data.frame(
    country = c("Country1", "Country2", "Country1", "Country3"),
    province = c("State1", "State2", "State1", "State3"),
    district = c("City1", "City2", "City1", "City4")
  )

  c("Los Angeles", "New York", "Chicago", "New York",
    "Chicago", "Los Angeles")

  lookup_data <- data.frame(
    country = c("Country1", "Country2", "Country1", "Country3"),
    province = c("State1", "State2", "State1", "State3"),
    district = c("City1", "City2", "City1", "City4")
  )

  suppressMessages({
    testthat::expect_invisible(
      calculate_match_stats(
        data = data, lookup_data = lookup_data, level0 = "country",
        level1 = "province", level2 = "district"
      )
    )
  })
})


testthat::test_that("harmonize_admin_names correctly processes admin names", {

  # Create a sample target data frame
  target_df <- data.frame(
    country = c("ANGOLA", "UGA", "ZAMBIA"),
    province = c("CABONDA", "TESO", "LUSAKA"),
    district = c("BALIZE", "BOKEDEA", "RAFUNSA"),
    subdistrict = c("AREA1", "AREA2", "AREA3"),
    stringsAsFactors = FALSE
  )

suppressMessages(
  # Run the function in non-interactive mode
  cleaned_df <- harmonize_admin_names(
    target_df,
    level0 = "country",
    level1 = "province",
    level2 = "district",
    interactive = FALSE
  )
)

  # Check that the function returns a data frame
  testthat::expect_s3_class(cleaned_df, "data.frame")

  # Check that the returned data frame has the same number of rows as the input
  testthat::expect_equal(nrow(cleaned_df), nrow(target_df))

  # Check that the returned data frame contains the expected columns
  testthat::expect_true(
    all(c("country", "province",
          "district", "subdistrict") %in% colnames(cleaned_df)))

})
