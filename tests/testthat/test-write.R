# Skip all tests on CRAN
if (!identical(Sys.getenv("NOT_CRAN"), "true")) {
  return()
}

testthat::test_that("export function works correctly", {

  tmpdir <- tempfile()
  dir.create(tmpdir)

  withr::with_dir(tmpdir, {

    # Create a dummy data frame
    dummy_df <- data.frame(
      a = 1:5,
      b = letters[1:5]
    )

    # Supported formats
    formats <- c("csv", "tsv", "xlsx", "rds", "dta")

    for (format in formats) {
      # Define file path
      file_path <- paste0(tempfile(), ".", format)

      # Test: Check if the function successfully exports without error
      testthat::expect_error(write(dummy_df, file_path), NA)

      # Read the data back
      imported_df <- read(file_path)
      attributes(imported_df$a) <- NULL
      attributes(imported_df$b) <- NULL

      # Test: Check if the data read back is the same as the original data
      testthat::expect_equal(dummy_df, imported_df, ignore_attr = TRUE)
    }

    # Test: Check if the function raises an error for unsupported formats
    unsupported_file_path <- paste0(tempfile(), ".unsupported")
    testthat::expect_error(export(dummy_df, unsupported_file_path))

    # Create a dummy shapefile
    dummy_shp <-  sf::st_sfc(
      sf::st_point(c(43, 23))) |>
      cbind(data.frame(a = 1, b = "A")) |>
      sf::st_as_sf(crs = sf::st_crs(4326))

    # Supported shapefile formats
    formats_shp <- c("shp", "json", "GeoJSON")

    for (format in formats_shp) {

      # Define file path
      file_path <- paste0(tempfile(), ".", format)

      # Test: Check if the function successfully exports without error
      testthat::expect_error(write(dummy_shp, file_path), NA)

      # Read the shapefile back
      imported_df <- sntutils::read(file_path)
      attributes(imported_df$a) <- NULL
      attributes(imported_df$b) <- NULL

      # Test: Check if the shapefile read back is the same as the
      # original shapefile
      testthat::expect_equal(dummy_shp, imported_df, ignore_attr = TRUE)
    }

    # Test: Check if the function raises an error for unsupported formats
    unsupported_file_path <- paste0(tempfile(), ".unsupported")
    testthat::expect_error(
      write(dummy_shp, unsupported_file_path))

  })

})

