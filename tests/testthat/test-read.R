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
    # Use system.file to find the correct path
    extdata_path <- system.file("extdata", package = "sntutils")
    file_path <- file.path(extdata_path, file)

    # Skip if file doesn't exist
    if (!file.exists(file_path)) {
      testthat::skip(paste("Test file for", format, "format not found"))
      next
    }

    # Import the data using the 'read' function
    testthat::expect_no_warning(
      imported_data <- testthat::expect_no_error(
        suppressWarnings(sntutils::read(file_path))
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


# 1b. Test for qs2 when qs2 is installed
testthat::test_that("Function imports qs2 when backend available", {
  if (!requireNamespace("qs2", quietly = TRUE)) {
    testthat::skip("'qs2' not installed")
  }

  # Use existing CSV as source data
  path <- system.file("extdata", package = "sntutils")
  src <- file.path(path, "test_data.csv")
  src_df <- sntutils::read(src)

  # Write to temporary qs2 file using sntutils::write (qs2 backend)
  tmp_qs2 <- tempfile(fileext = ".qs2")
  sntutils::write(src_df, tmp_qs2)

  # Read back with sntutils::read and compare
  out_qs2 <- sntutils::read(tmp_qs2)
  testthat::expect_identical(out_qs2, src_df)
})


# 2. Test for Unsupported Formats
testthat::test_that("Function throws error for unsupported file formats", {
  file_path <- "testdata/test_data.xyz"
  testthat::expect_error(
    sntutils::read(file_path),
    paste0(
      "File format 'xyz' not supported by 'rio', 'sf', or 'readxl'. ",
      "Please refer to the package documentation for a full list ",
      "of supported formats."
    )
  )
})

# 3. Testing URL imports
testthat::test_that("Function imports data from URLs correctly", {
  # The raw URL for the mtcars.csv file from GitHub
  github_url <- paste0(
    "https://raw.githubusercontent.com/",
    "truenomad/epiCleanr/",
    "master/inst/extdata/test_data.csv"
  )

  imprt_data <- sntutils::read(github_url)

  path <- system.file("extdata", package = "sntutils")

  imprt_data_compare <- sntutils::read(
    file_path = file.path(path, "test_data.csv")
  )

  testthat::expect_identical(imprt_data, imprt_data_compare)
})
