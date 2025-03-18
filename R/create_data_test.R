#' Create Test Function
#'
#' This function creates a test function to perform various data validation
#' checks. The returned function can be applied to a dataset to perform the
#' specified tests.
#'
#' @param dimension_test A vector of two integers specifying the expected number
#' of rows and columns.
#' @param combinations_test A list with the elements `variables` (character
#' vector of variable names) and `expectation` (integer specifying the expected
#' number of unique combinations for each column).
#' @param row_duplicates Logical. If TRUE, checks for duplicate rows.
#' @param col_duplicates Logical. If TRUE, checks for duplicate columns.
#' @param min_threshold_test Named list of minimum threshold values for
#' specified columns.
#' @param max_threshold_test Named list of maximum threshold values for
#' specified columns.
#' @return A function to be applied to the dataset.
#' @examples
#'
#' # get path
#' path <- system.file(
#'         "extdata",
#'         "fake_epi_df_togo.rds",
#'          package = "epiCleanr")
#'
# # get example data
#' fake_epi_df_togo <- read(path)
#'
#' # Set up unit-test function
#' my_tests <- create_data_test(
#'   # For checking the dimension of the data
#'   dimension_test = c(900, 9),
#'   # For expected number of combinations in data
#'  combinations_test = list(
#'    variables = c("month", "year", "district"),
#'    expectation = 12 * 5 * 15),
#'   # Check repeated cols, rows and max and min thresholds
#'   row_duplicates = TRUE, col_duplicates = TRUE,
#'   max_threshold_test = list(malaria_tests = 1000, cholera_tests = 1000),
#'  min_threshold_test = list(cholera_cases = 0, cholera_cases = 0)
#' )
#'
# Apply your unit-test on your data
#' result <- my_tests(fake_epi_df_togo)
#'
#' @export
create_data_test <- function(dimension_test = NULL, combinations_test = NULL,
                        row_duplicates = FALSE, col_duplicates = FALSE,
                        min_threshold_test = NULL, max_threshold_test = NULL) {


  # praise emoji function
  praise <- function() {
    emoji <- c(
      "\U0001f600", # smile
      "\U0001f973", # party face
      "\U0001f638", # cat grin
      "\U0001f308", # rainbow
      "\U0001f947", # gold medal
      "\U0001f389", # party popper
      "\U0001f38a" # confetti ball
    )
    sample(emoji, 1)
  }

  # Function to format big numbers
  big_mark <- function(value) {
    return(formatC(value, format = "d", big.mark = ","))
  }

  return(function(data) {
    result <- list()
    total_tests <- 0
    tests_passed <- 0

    # Check Dimensions  --------------------------------------------------------
    if (!is.null(dimension_test)) {
      total_tests <- total_tests + 1
      actual_dims <- dim(data)
      expected_dims <- as.integer(dimension_test)
      if (identical(actual_dims, expected_dims)) {
        tests_passed <- tests_passed + 1
        message(
          glue::glue(
            crayon::green(
              "Test passed! You have the correct number of dimensions!"
            )
          )
        )
      } else {
        message(
          glue::glue(
            crayon::red(
              "Warning! Test failed. Expected {big_mark(expected_dims[1])}",
              "rows and {big_mark(expected_dims[2])} columns,",
              "but got {big_mark(actual_dims[1])} rows and",
              "{big_mark(actual_dims[2])} columns."
            )
          )
        )
      }
    }

    # Check Row Duplicates -----------------------------------------------------
    if (row_duplicates) {
      total_tests <- total_tests + 1
      duplicate_rows <- data[duplicated(data) | duplicated(data,
                                                           fromLast = TRUE
      ), ]
      if (nrow(duplicate_rows) > 0) {
        result$duplicate_rows <- duplicate_rows
        message(
          glue::glue(
            crayon::red(
              "Warning! Test failed. Duplicate rows found.",
              "See output$duplicate_rows."
            )
          )
        )
      } else {
        tests_passed <- tests_passed + 1
        message(
          glue::glue(crayon::green("Test passed! No duplicate rows found!"))
        )
      }
    }

    # Check Column Duplicates --------------------------------------------------
    if (col_duplicates) {
      total_tests <- total_tests + 1
      repeated_columns <- which(duplicated(t(data)))
      if (length(repeated_columns) > 0) {
        result$duplicate_columns <- repeated_columns
        message(
          glue::glue(
            crayon::red(
              "Warning! Test failed. Repeated columns found.",
              "See output$duplicate_columns."
            )
          )
        )
      } else {
        tests_passed <- tests_passed + 1
        message(
          glue::glue(
            crayon::green("Test passed! No repeated columns found!")
          )
        )
      }
    }

    # Check Combinations  ------------------------------------------------------
    if (!is.null(combinations_test)) {
      total_tests <- total_tests + 1
      variables <- combinations_test$variables

      # Check if the specified variables exist in the data
      missing_variables <- setdiff(variables, colnames(data))
      if (length(missing_variables) > 0) {
        stop(glue::glue(
          "Error! The following variables do not exist in the ",
          "dataset: {paste(missing_variables, collapse = ', ')}"
        ))
      }

      expectation <- combinations_test$expectation
      actual_combinations <- nrow(unique(data[, variables]))

      if (actual_combinations == expectation) {
        tests_passed <- tests_passed + 1
        message(
          glue::glue(
            crayon::green(
              "Test passed! You have the correct number of combinations",
              "for {paste(variables, collapse = ', ')}!"
            )
          )
        )
      } else {
        message(
          glue::glue(
            crayon::red(
              "Warning! Test failed. Expected {big_mark(expectation)}",
              "combinations but found {big_mark(actual_combinations)}",
              "for {paste(variables, collapse = ', ')}."
            )
          )
        )
      }
    }


    # Threshold Tests   --------------------------------------------------------
    for (test_type in c("min", "max")) {
      threshold_test <- if (test_type == "min") {
        min_threshold_test
      } else {
        max_threshold_test
      }
      if (!is.null(threshold_test)) {
        for (column_name in names(threshold_test)) {
          # Check if the column exists in the data
          if (!column_name %in% colnames(data)) {
            stop(
              glue::glue(
                "Error! Column {column_name} does not exist in the dataset."
              )
            )
          }

          total_tests <- total_tests + 1
          threshold <- threshold_test[[column_name]]
          failed_condition <- if (test_type == "max") {
            data[[column_name]] > threshold
          } else {
            data[[column_name]] < threshold
          }
          # Exclude NA values from the failed condition
          failed_indices <- which(
            failed_condition & !is.na(data[[column_name]])
          )

          if (length(failed_indices) > 0) {
            key <- paste0(test_type, "_thresh_", column_name)
            result[[key]] <- data.frame(
              row_number = failed_indices,
              value = data[
                failed_indices,
                column_name
              ]
            )
            message_type <- if (test_type == "max") "above" else "below"
            message(
              glue::glue(
                crayon::red(
                  "Warning! Test failed. Values in column {column_name}",
                  "are {message_type} the threshold. See output${key}."
                )
              )
            )
          } else {
            tests_passed <- tests_passed + 1
            message_type <- if (test_type == "max") "below" else "above"
            message(
              glue::glue(
                crayon::green(
                  "Test passed! Values in column {column_name}",
                  "are {message_type} the threshold."
                )
              )
            )
          }
        }
      }
    }

    # Calculate and return the percentage of tests passed
    result$percentage_passed <- if (
      total_tests > 0) {
      tests_passed / total_tests * 100
    } else {
      stop("No tests have been carried out!")
    }

    if (tests_passed == total_tests) {
      message(
        crayon::blue(
          glue::glue(
            "Congratulations! All tests passed: {tests_passed}/{total_tests}",
            " ({round(result$percentage_passed)}%) {praise()}"
          )
        )
      )
    } else {
      message(
        crayon::blue(
          glue::glue(
            "Total tests passed: {tests_passed}/{total_tests}",
            " ({round(result$percentage_passed)}%)"
          )
        )
      )
    }

  })
}
