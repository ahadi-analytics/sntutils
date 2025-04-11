# Skip all tests on CRAN
if (!identical(Sys.getenv("NOT_CRAN"), "true")) {
  return()
}

suppressMessages(
  suppressWarnings(
    testthat::test_that("consistency_check function works correctly", {
      # Create a dummy dataset
      dummy_data <- data.frame(
        year = sample(2018:2023, 1000, replace = TRUE),
        month = sample(1:12, 1000, replace = TRUE),
        malaria_rdt_test = rnorm(1000, mean = 0, sd = 50),
        malaria_micro_test = rnorm(1000, mean = 0, sd = 10),
        malaria_rdt_cases = rnorm(1000, mean = 0, sd = 50),
        malaria_micro_cases = rnorm(1000, mean = 1, sd = 10),
        dengue_test = rnorm(1000, mean = 5, sd = 20),
        dengue_cases = rnorm(1000, mean = 2, sd = 15)
      )

      # 1. Test when the length of tests and cases is not the same
      testthat::expect_error(
        consistency_check(dummy_data,
          tests = c("malaria_rdt_test"),
          cases = c(
            "malaria_rdt_cases",
            "malaria_micro_cases"
          )
        ),
        "The length of 'tests' and 'cases' must be the same."
      )

      # 2. Test when all the tests values are greater than the cases values
      tests_pass <- c("malaria_rdt_test", "malaria_micro_test")
      cases_pass <- c("malaria_rdt_cases", "malaria_micro_cases")

      dummy_data2 <- dummy_data |>
        dplyr::mutate(
          malaria_rdt_test = abs(malaria_rdt_test) * 10000,
          malaria_micro_test = abs(malaria_micro_test) * 10000
        )

      actual <- as.character(
        testthat::capture_message(
          consistency_check(dummy_data2,
            tests = tests_pass,
            cases = cases_pass
          )
        )[1]
      )

      expected <- paste(
        "Consistency test passed for malaria_rdt_test",
        "vs malaria_rdt_cases: There are more tests than there are cases!"
      )

      testthat::expect_equal(writeLines(actual), writeLines(expected))

      # 3. Test when some tests values are less than the cases values
      dummy_data_with_inconsistency <- dummy_data
      dummy_data_with_inconsistency$malaria_micro_test[1] <- 1000

      actual_inconsistency <- as.character(
        testthat::capture_message(
          consistency_check(dummy_data_with_inconsistency,
            tests = tests_pass, cases = cases_pass
          )
        )[1]
      )

      expected <- paste(
        "Consistency test failed for malaria_micro_test",
        "vs malaria_micro_cases: There are 513 (51.3%) rows where tests",
        "are less than cases."
      )

      testthat::expect_equal(
        writeLines(actual_inconsistency),
        writeLines(expected)
      )

      # 4. Test the return type of the function
      plot_result <- consistency_check(dummy_data,
        tests = tests_pass,
        cases = cases_pass
      )
      testthat::expect_equal(class(plot_result)[2], "ggplot")

      # 5. Test with NA values
      dummy_data_na <- dummy_data
      dummy_data_na$malaria_rdt_test[1:10] <- NA
      dummy_data_na$malaria_rdt_cases[11:20] <- NA

      na_result <- consistency_check(dummy_data_na,
        tests = tests_pass[1],
        cases = cases_pass[1]
      )

      testthat::expect_s3_class(na_result, "ggplot")

      # 7. Test with multiple disease types
      multi_tests <- c("malaria_rdt_test", "dengue_test")
      multi_cases <- c("malaria_rdt_cases", "dengue_cases")

      multi_result <- consistency_check(dummy_data,
        tests = multi_tests,
        cases = multi_cases
      )

      testthat::expect_s3_class(multi_result, "ggplot")

      # 8. Test saving functionality
      # Create a temporary file path
      temp_dir <- tempdir()

      # Test saving the plot
      save_result <- consistency_check(
        dummy_data,
        tests = tests_pass,
        cases = cases_pass,
        target_language = "fr",
        save_plot = TRUE,
        plot_path = temp_dir
      )

      # Check that the file was created
      testthat::expect_true(file.exists(temp_dir))

      # Check file size is greater than 0 (valid image)
      testthat::expect_gt(file.size(temp_dir), 0)


      # 11. Test directory creation functionality
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
      temp_file3 <- file.path(temp_dir, "test_plot.png")

      save_dir_result <- consistency_check(dummy_data,
        tests = tests_pass,
        cases = cases_pass,
        save_plot = TRUE,
        plot_path = temp_file3
      )

      testthat::expect_true(file.exists(temp_file3))

      # Test that recursive = FALSE fails when parent directory doesn't exist
      non_existent_dir <- file.path(tempdir(), "non_existent", "test_dir")

      if (dir.exists(non_existent_dir)) {
        unlink(non_existent_dir, recursive = TRUE)
      }

      if (dir.exists(dirname(non_existent_dir))) {
        unlink(dirname(non_existent_dir), recursive = TRUE)
      }

      # 12. Test error when save_plot is TRUE but plot_path is NULL
      testthat::expect_error(
        consistency_check(dummy_data,
          tests = tests_pass,
          cases = cases_pass,
          save_plot = TRUE,
          plot_path = NULL
        ),
        "plot_path must be provided when save_plot is TRUE."
      )
    })
  )
)
