# Skip all tests on CRAN
if (!identical(Sys.getenv("NOT_CRAN"), "true")) {
  return()
}

# Sample data for testing
fake_data <- tidyr::expand_grid(
  state = state.name,
  month = 1:12, year = 2000:2023
) |>
  dplyr::mutate(
    measles = sample(0:1000, size = dplyr::n(), replace = TRUE),
    polio = sample(0:500, size = dplyr::n(), replace = TRUE),
    malaria = sample(0:300, size = dplyr::n(), replace = TRUE),
    cholera = sample(0:700, size = dplyr::n(), replace = TRUE)
  ) |>
  dplyr::mutate(across(
    c(measles, polio, malaria, cholera),
    ~ replace(., sample(1:dplyr::n(),
                        size = dplyr::n() * 0.4
    ), NA)
  ))

# Test 1: Check for error if 'x_var' is NULL or does not exist in the data
testthat::test_that("Error is thrown if 'x_var' is NULL or not in data", {
  testthat::expect_error(
    missing_plot(fake_data, NULL),
    "A valid 'x_var' must be provided"
  )
  testthat::expect_error(
    missing_plot(fake_data, "invalid_x_var"),
    "A valid 'x_var' must be provided"
  )
})

# Test 2: Check error if 'y_var' provided with >1 variable in 'miss_vars'
testthat::test_that("Error if 'y_var' provided with >1 vars in 'miss_vars'", {
  testthat::expect_error(
    missing_plot(
      fake_data, "year", "state",
      c("polio", "measles")
    ),
    "When 'y_var' is provided only one variable can be specified in 'miss_vars'"
  )
})

# Test 3: Check that the function returns a ggplot object
testthat::test_that("Returns ggplot object", {
  plot <- missing_plot(fake_data, "year", "state", "polio", TRUE)
  expect_s3_class(plot, "ggplot")
})

# Test 4: Check that the function works without 'y_var' and 'miss_vars'
testthat::test_that("Works without 'y_var' and 'miss_vars'", {
  plot <- missing_plot(fake_data, "year")
  expect_s3_class(plot, "ggplot")
})

# Test 5: Check that the function works with all provided parameters
testthat::test_that("Works with all provided parameters", {
  plot <- missing_plot(fake_data, "year", "state", "polio", TRUE)
  expect_s3_class(plot, "ggplot")
})

# Test 6: Check the title construction with 'use_rep_rate = TRUE'
testthat::test_that("Title construction with 'use_rep_rate = TRUE'", {
  plot <- missing_plot(fake_data, "year", "state", "polio", TRUE)
  testthat::expect_match(
    plot$labels$title, "Reporting rate of polio by year and state"
  )
})

# Test 7: Check the title construction with 'use_rep_rate = FALSE'
testthat::test_that("Title construction with 'use_rep_rate = FALSE'", {
  plot <- missing_plot(fake_data, "year", "state", "polio", FALSE)
  testthat::expect_match(
    plot$labels$title,
    "The proportion of missing data for polio by year and state"
  )
})

# Test 8: Check the title construction without 'y_var'
testthat::test_that("Title construction without 'y_var'", {
  plot <- missing_plot(fake_data, "year", NULL, "polio", TRUE)
  testthat::expect_match(plot$labels$title, "Reporting rate of polio by year")
})

# Test 9: Check the y-axis label with 'y_var'
testthat::test_that("Y-axis label with 'y_var'", {
  plot <- missing_plot(
    fake_data, "year", "state", "polio", FALSE
  )
  expect_equal(plot$labels$y, "State")
})

# Test 10: Check title construction with and without 'miss_vars'"
testthat::test_that("Title construction with and without 'miss_vars'", {
  # Case when 'miss_vars' is provided
  plot_with_vars <- missing_plot(
    fake_data, "year", "state", "polio", FALSE
  )
  expect_true(
    grepl(
      "The proportion of missing data for polio by year and state",
      plot_with_vars$labels$title
    )
  )

  # Case when 'miss_vars' is not provided
  plot_without_vars <- missing_plot(
    fake_data,
    x_var = "year", miss_vars = NULL, use_rep_rate = FALSE
  )
  expect_true(grepl("year", plot_without_vars$labels$title))
})

# Test 11: Check title construction when there are more than one var
testthat::test_that("Title is constructed correctly with remaining variables", {
  # Call your function with appropriate arguments
  plot <- missing_plot(dplyr::select(fake_data, -polio),
                       x_var = "year", miss_vars = NULL,
                       use_rep_rate = FALSE
  )

  # Construct the expected title
  expected_title <-
    paste(
      "The proportion of missing data for state,",
      "month, measles, malaria and cholera by year"
    )

  # Assert that the actual title matches the expected title
  expect_identical(plot$labels$title, expected_title)
})

# Test 12: Check title construction when there are too many variables
testthat::test_that("Title construction when there are too many variables", {
  plot <- missing_plot(fake_data, "year", NULL, NULL, FALSE)
  testthat::expect_match(
    plot$labels$title,
    "The proportion of missing data for various variables by year"
  )
})

# Test 13: Check saving functionality
testthat::test_that("Plot can be saved to a file", {
  # Create a temporary file path
  temp_dir <- tempdir()
  temp_file <- file.path(temp_dir, "missing_plot.png")

  # Test saving the plot
  plot <- missing_plot(fake_data,
                       "year",
                       "state",
                       "polio",
                       TRUE,
                       save_plot = TRUE,
                       plot_path = temp_file)

  # Check that the file was created
  testthat::expect_true(file.exists(temp_file))

  # Check file size is greater than 0 (valid image)
  testthat::expect_gt(file.size(temp_file), 0)

})

# Test 14: Test saving with custom width and height
testthat::test_that("Plot can be saved with custom dimensions", {
  # Create a temporary file path
  temp_dir <- tempdir()
  temp_file <- file.path(temp_dir, "custom_size.png")

  # Test saving the plot with custom dimensions
  plot <- missing_plot(fake_data,
                       "year",
                       "state",
                       "polio",
                       TRUE,
                       save_plot = TRUE,
                       plot_path = temp_file,
                       plot_width = 12,
                       plot_height = 8)

  # Check that the file was created
  testthat::expect_true(file.exists(temp_file))

  # Check file size is greater than 0 (valid image)
  testthat::expect_gt(file.size(temp_file), 0)

})

# Test 15: Test directory creation functionality
testthat::test_that("Plot can be saved to a newly created directory", {
  # Create a temporary directory path
  temp_dir <- file.path(tempdir(), "test_plots_dir", "nested_dir")

  # Make sure the directory doesn't exist before the test
  if (dir.exists(temp_dir)) {
    unlink(temp_dir, recursive = TRUE)
  }

  # Test creating the directory
  testthat::expect_false(dir.exists(temp_dir))
  dir.create(temp_dir, recursive = TRUE)
  testthat::expect_true(dir.exists(temp_dir))

  # Test saving to the newly created directory
  temp_file <- file.path(temp_dir, "test_plot.png")

  plot <- missing_plot(fake_data,
                       "year",
                       "state",
                       "polio",
                       TRUE,
                       save_plot = TRUE,
                       plot_path = temp_file)

  testthat::expect_true(file.exists(temp_file))

  if (dir.exists(temp_dir)) {
    unlink(temp_dir, recursive = TRUE)
  }
})

# Test 16: Test error when save_plot is TRUE but plot_path is NULL
testthat::test_that("Error when save_plot is TRUE but plot_path is NULL", {
  testthat::expect_error(
    missing_plot(fake_data,
                 "year",
                 "state",
                 "polio",
                 TRUE,
                 save_plot = TRUE,
                 plot_path = NULL),
    "When 'save_plot' is TRUE, 'plot_path' must be provided"
  )
})

# Test 17: Test with different file formats
testthat::test_that("Plot can be saved in different file formats", {
  # Create temporary file paths for different formats
  temp_dir <- tempdir()
  temp_file_pdf <- file.path(temp_dir, "test_plot.pdf")
  temp_file_jpeg <- file.path(temp_dir, "test_plot.jpeg")

  # Test saving as PDF
  plot_pdf <- missing_plot(fake_data,
                           "year",
                           "state",
                           "polio",
                           TRUE,
                           save_plot = TRUE,
                           plot_path = temp_file_pdf)

  # Test saving as JPEG
  plot_jpeg <- missing_plot(fake_data,
                            "year",
                            "state",
                            "polio",
                            TRUE,
                            save_plot = TRUE,
                            plot_path = temp_file_jpeg)

  # Check that the files were created
  testthat::expect_true(file.exists(temp_file_pdf))
  testthat::expect_true(file.exists(temp_file_jpeg))

  # Check file sizes are greater than 0 (valid images)
  testthat::expect_gt(file.size(temp_file_pdf), 0)
  testthat::expect_gt(file.size(temp_file_jpeg), 0)

})
