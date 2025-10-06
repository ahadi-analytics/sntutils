suppressMessages(
  suppressWarnings(
    testthat::test_that(
      "consistency_check function works correctly", {
      set.seed(123)
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

      # 1. Test when the length of inputs and outputs is not the same
      testthat::expect_error(
        consistency_check(dummy_data,
          inputs = "malaria_rdt_test",
          outputs = c(
            "malaria_rdt_cases",
            "malaria_micro_cases"
          )
        ),
        "The length of 'inputs' and 'outputs' must be the same."
      )

      # 2. Test when all the inputs values are greater than the outputs
      inputs_pass <- c("malaria_rdt_test", "malaria_micro_test")
      outputs_pass <- c("malaria_rdt_cases", "malaria_micro_cases")

      dummy_data2 <- dummy_data |>
        dplyr::mutate(
          malaria_rdt_test = abs(malaria_rdt_test) * 10000,
          malaria_micro_test = abs(malaria_micro_test) * 10000
        )

      actual <- testthat::capture_messages(
        consistency_check(
          dummy_data2,
          inputs = inputs_pass,
          outputs = outputs_pass
        )
      )[1]

      testthat::expect_true(
        stringr::str_detect(
          actual,
          "Consistency test passed for malaria_rdt_test vs malaria_rdt_cases"
        )
      )

      # 3. Test when some inputs values are less than the outputs values
      dummy_data_with_inconsistency <- dummy_data
      dummy_data_with_inconsistency$malaria_micro_test[1] <- 1000

      actual_inconsistency <- testthat::capture_messages(
        consistency_check(dummy_data_with_inconsistency,
          inputs = inputs_pass,
          outputs = outputs_pass
        )
      )[1]

      testthat::expect_true(
        stringr::str_detect(
          actual_inconsistency,
          "Consistency test failed for malaria_rdt_test vs malaria_rdt_cases"
        )
      )

      # 4. Test the return type of the function
      plot_result <- consistency_check(dummy_data,
        inputs = inputs_pass,
        outputs = outputs_pass
      )

      testthat::expect_equal(class(plot_result)[2], "ggplot")

      # 5. Test with NA values
      dummy_data_na <- dummy_data
      dummy_data_na$malaria_rdt_test[1:10] <- NA
      dummy_data_na$malaria_rdt_cases[11:20] <- NA

      na_result <- consistency_check(dummy_data_na,
        inputs = inputs_pass[1],
        outputs = outputs_pass[1]
      )

      testthat::expect_s3_class(na_result, "ggplot")

      # 6. Test with multiple disease types
      multi_inputs <- c("malaria_rdt_test", "dengue_test")
      multi_outputs <- c("malaria_rdt_cases", "dengue_cases")

      multi_result <- consistency_check(dummy_data,
        inputs = multi_inputs,
        outputs = multi_outputs
      )

      testthat::expect_s3_class(multi_result, "ggplot")

      # 7. Test saving functionality
      # Create a temporary file path
      temp_dir <- tempdir()

      # Test saving the plot
      save_result <- consistency_check(
        dummy_data,
        inputs = inputs_pass,
        outputs = outputs_pass,
        target_language = "fr",
        save_plot = TRUE,
        plot_path = temp_dir
      )

      # Check that the file was created
      testthat::expect_true(file.exists(temp_dir))

      # Check file size is greater than 0 (valid image)
      testthat::expect_gt(file.size(temp_dir), 0)

      # 8. Test directory creation functionality
      # Create a temporary directory path
      temp_dir2 <- file.path(tempdir(), "test_plots_dir", "nested_dir")

      # Make sure the directory doesn't exist before the test
      if (dir.exists(temp_dir2)) {
        unlink(temp_dir2, recursive = TRUE)
      }

      # Test creating the directory
      testthat::expect_false(dir.exists(temp_dir2))
      dir.create(temp_dir2, recursive = TRUE)
      testthat::expect_true(dir.exists(temp_dir2))

      # Test saving to the newly created directory
      temp_file3 <- file.path(temp_dir2, "test_plot.png")

      save_dir_result <- consistency_check(dummy_data,
        inputs = inputs_pass,
        outputs = outputs_pass,
        save_plot = TRUE,
        plot_path = temp_file3
      )

      testthat::expect_true(file.exists(temp_file3))

      # 9. Test error when save_plot is TRUE but plot_path is NULL
      testthat::expect_error(
        consistency_check(dummy_data,
          inputs = inputs_pass,
          outputs = outputs_pass,
          save_plot = TRUE,
          plot_path = NULL
        ),
        "plot_path must be provided when save_plot is TRUE."
      )

      # 10. Test backward compatibility with deprecated parameters
      # Suppress deprecation warnings for these tests
      suppressWarnings({
        # Test deprecated 'tests' and 'cases' parameters still work
        old_result <- consistency_check(dummy_data,
          tests = inputs_pass,
          cases = outputs_pass
        )

        testthat::expect_s3_class(old_result, "ggplot")

        # Test error with deprecated parameters when lengths don't match
        testthat::expect_error(
          consistency_check(dummy_data,
            tests = "malaria_rdt_test",
            cases = c("malaria_rdt_cases", "malaria_micro_cases")
          ),
          "The length of 'inputs' and 'outputs' must be the same."
        )
      })

      # 11. Test that providing neither old nor new params throws error
      testthat::expect_error(
        consistency_check(dummy_data),
        "Both 'inputs' and 'outputs' parameters must be provided."
      )
    })
  )
)
