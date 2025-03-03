# Skip all tests on CRAN
if (!identical(Sys.getenv("NOT_CRAN"), "true")) {
  return()
}

testthat::test_that("Function imports supported file formats correctly", {
  # List of supported file formats to test
  supported_formats <- c(
    "csv", "tsv", "txt", "csvy", "sas7bdat", "sav",
    "dta", "xpt", "xlsx", "rdata", "rds", "tsv", "GeoJSON", "shp"
  )

  # Loop through each supported format and test importing the file
  for (format in supported_formats) {
    file <- paste0("test_data.", format)
    file_path <- file.path("inst", "extdata", file)

    # Skip if file doesn't exist
    if (!file.exists(file_path)) {
      testthat::skip(paste("Test file for", format, "format not found"))
      next
    }

    # Import the data using the 'read' function
    testthat::expect_no_warning(
      imported_data <- testthat::expect_no_error(
        suppressWarnings(snt::read(file_path))
      )
    )

    # Only run these checks if data was successfully imported
    if (!is.null(imported_data)) {
      # Check if the imported data has the expected structure and values
      testthat::expect_true("ID" %in% colnames(imported_data),
                            info = paste("ID column not found in", file)
      )

      testthat::expect_true("Name" %in% colnames(imported_data),
                            info = paste("Name column not found in", file)
      )

      testthat::expect_true("Age" %in% colnames(imported_data),
                            info = paste("Age column not found in", file)
      )

      testthat::expect_true("Score" %in% colnames(imported_data),
                            info = paste("Score column not found in", file)
      )

      testthat::expect_equal(as.integer(imported_data$ID), 1:5,
                             info = paste("ID values mismatch in", file)
      )
      testthat::expect_equal(as.character(imported_data$Name),
                             c("Alice", "Bob", "Charlie", "David", "Eva"),
                             info = paste("Name values mismatch in", file)
      )
      testthat::expect_equal(as.integer(imported_data$Age),
                             c(25, 30, 28, 22, 27),
                             info = paste("Age values mismatch in", file)
      )
      testthat::expect_equal(as.integer(imported_data$Score),
                             c(85, 90, 78, 95, 88),
                             info = paste("Score values mismatch in", file)
      )
    }
  }
})


# # 2. Test for Unsupported Formats
testthat::test_that("Function throws error for unsupported file formats", {
  file_path <- "testdata/test_data.xyz"
  testthat::expect_error(
    snt::read(file_path),
    paste0(
      "File format 'xyz' not supported by 'rio' and 'sf'. ",
      "Please refer to the package documentation for a full list",
      "of supported formats."
    )
  )
})

# 3. Testing URL imports
testthat::test_that("Function imports data from URLs correctly", {
  # The raw URL for the mtcars.csv file from GitHub
  github_url <- paste0(
    "https://raw.githubusercontent.com/",
    "ahadi-analytics/snt/",
    "master/inst/extdata/test_data.csv"
  )

  imprt_data <- snt::read(github_url)

  path <- system.file("extdata", package = "snt")

  imprt_data_compare <- snt::read(
    file_path = file.path(path, "test_data.csv")
  )

  testthat::expect_identical(imprt_data, imprt_data_compare)
})
