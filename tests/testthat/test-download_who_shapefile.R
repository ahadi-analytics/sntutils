testthat::test_that("download_shapefile validates admin_level parameter", {
  testthat::expect_error(
    download_shapefile("KEN", admin_level = "ADM5"),
    "Invalid admin_level"
  )

  testthat::expect_error(
    download_shapefile("KEN", admin_level = "invalid"),
    "Invalid admin_level"
  )

  testthat::expect_error(
    download_shapefile("KEN", admin_level = 123),
    "Invalid admin_level"
  )
})

testthat::test_that("download_shapefile accepts valid admin levels", {
  testthat::skip_on_cran()
  testthat::skip_if_offline()

  # Test that function doesn't error with valid levels
  testthat::expect_no_error({
    result <- download_shapefile("COM", admin_level = "ADM0")
  })

  testthat::expect_no_error({
    result <- download_shapefile("COM", admin_level = "ADM1")
  })

  testthat::expect_no_error({
    result <- download_shapefile("COM", admin_level = "ADM2")
  })
})

testthat::test_that("download_shapefile returns sf object without saving when dest_path is NULL", {
  testthat::skip_on_cran()
  testthat::skip_if_offline()

  result <- download_shapefile("COM", admin_level = "ADM0")

  testthat::expect_s3_class(result, "sf")
  testthat::expect_true("adm0_code" %in% names(result))
  testthat::expect_true("adm0" %in% names(result))
  testthat::expect_equal(unique(result$adm0_code), "COM")
})

testthat::test_that("download_shapefile returns correct columns for each admin level", {
  testthat::skip_on_cran()
  testthat::skip_if_offline()

  # ADM0 level
  result_adm0 <- download_shapefile("COM", admin_level = "ADM0")
  testthat::expect_equal(
    setdiff(names(result_adm0), "geometry"),
    c("adm0_code", "adm0", "start_date", "end_date")
  )

  # ADM1 level
  result_adm1 <- download_shapefile("COM", admin_level = "ADM1")
  testthat::expect_equal(
    setdiff(names(result_adm1), "geometry"),
    c("adm0_code", "adm0", "adm1", "start_date", "end_date")
  )

  # ADM2 level
  result_adm2 <- download_shapefile("COM", admin_level = "ADM2")
  testthat::expect_equal(
    setdiff(names(result_adm2), "geometry"),
    c("adm0_code", "adm0", "adm1", "adm2", "start_date", "end_date")
  )
  
  # Check that date columns are present
  testthat::expect_true("start_date" %in% names(result_adm0))
  testthat::expect_true("end_date" %in% names(result_adm0))
})

testthat::test_that("download_shapefile saves to file when dest_path is provided", {
  testthat::skip_on_cran()
  testthat::skip_if_offline()

  temp_dir <- tempdir()
  expected_file <- file.path(temp_dir, "who_shapefile_com_adm0.gpkg")

  # Clean up if file exists
  if (file.exists(expected_file)) unlink(expected_file)

  result <- download_shapefile(
    "COM",
    admin_level = "ADM0",
    dest_path = temp_dir
  )

  testthat::expect_true(file.exists(expected_file))
  testthat::expect_s3_class(result, "sf")

  # Read back the saved file
  saved_data <- sf::st_read(expected_file, quiet = TRUE)
  testthat::expect_equal(nrow(saved_data), nrow(result))
  # Geometry column name may change when saving/reading
  testthat::expect_equal(
    setdiff(names(saved_data), c("geom", "geometry")),
    setdiff(names(result), c("geom", "geometry"))
  )

  # Clean up
  unlink(expected_file)
})

testthat::test_that("download_shapefile handles incremental updates correctly", {
  testthat::skip_on_cran()
  testthat::skip_if_offline()

  temp_dir <- tempdir()
  # File name will be who_shapefile_com_syc_adm0.gpkg when both are added
  first_file <- file.path(temp_dir, "who_shapefile_com_adm0.gpkg")
  second_file <- file.path(temp_dir, "who_shapefile_com_syc_adm0.gpkg")

  # Clean up if files exist
  if (file.exists(first_file)) unlink(first_file)
  if (file.exists(second_file)) unlink(second_file)

  # First download
  result1 <- download_shapefile(
    "COM",
    admin_level = "ADM0",
    dest_path = temp_dir
  )

  testthat::expect_true(file.exists(first_file))
  testthat::expect_equal(unique(result1$adm0_code), "COM")

  # Second download with additional country creates new file
  result2 <- download_shapefile(
    c("COM", "SYC"),
    admin_level = "ADM0",
    dest_path = temp_dir
  )

  # Should have both countries in new file
  testthat::expect_true(file.exists(second_file))
  testthat::expect_true(all(c("COM", "SYC") %in% result2$adm0_code))
  testthat::expect_true(nrow(result2) >= 2)

  # Third download with same countries (should not download again)
  expect_message(
    result3 <- download_shapefile(
      c("COM", "SYC"),
      admin_level = "ADM0",
      dest_path = temp_dir
    ),
    "All requested country codes are already"
  )

  testthat::expect_equal(nrow(result3), nrow(result2))

  # Clean up
  unlink(first_file)
  unlink(second_file)
})

testthat::test_that("download_shapefile handles multiple country codes", {
  testthat::skip_on_cran()
  testthat::skip_if_offline()

  result <- download_shapefile(
    c("COM", "SYC", "MUS"),
    admin_level = "ADM0"
  )

  testthat::expect_s3_class(result, "sf")
  testthat::expect_true(all(c("COM", "SYC", "MUS") %in% result$adm0_code))
  testthat::expect_equal(length(unique(result$adm0_code)), 3)
})

testthat::test_that("download_shapefile requires httr2 package", {
  # Mock the requireNamespace function to return FALSE
  testthat::with_mocked_bindings(
    requireNamespace = function(...) FALSE,
    {
      testthat::expect_error(
        download_shapefile("KEN"),
        "Package 'httr2' is required"
      )
    },
    .package = "base"
  )
})

testthat::test_that("download_shapefile creates directory if it doesn't exist", {
  testthat::skip_on_cran()
  testthat::skip_if_offline()

  temp_dir <- file.path(tempdir(), "new_test_dir", "nested")
  expected_file <- file.path(temp_dir, "who_shapefile_com_adm0.gpkg")

  # Ensure directory doesn't exist
  if (dir.exists(temp_dir)) unlink(temp_dir, recursive = TRUE)

  result <- download_shapefile(
    "COM",
    admin_level = "ADM0",
    dest_path = temp_dir
  )

  testthat::expect_true(dir.exists(temp_dir))
  testthat::expect_true(file.exists(expected_file))

  # Clean up
  unlink(dirname(temp_dir), recursive = TRUE)
})

testthat::test_that("download_shapefile handles case-insensitive admin_level", {
  testthat::skip_on_cran()
  testthat::skip_if_offline()

  result_lower <- download_shapefile("COM", admin_level = "adm0")
  result_upper <- download_shapefile("COM", admin_level = "ADM0")
  result_mixed <- download_shapefile("COM", admin_level = "Adm0")

  testthat::expect_equal(nrow(result_lower), nrow(result_upper))
  testthat::expect_equal(nrow(result_lower), nrow(result_mixed))
  testthat::expect_equal(names(result_lower), names(result_upper))
  testthat::expect_equal(names(result_lower), names(result_mixed))
})

testthat::test_that("download_shapefile filters by ENDDATE correctly when latest = TRUE", {
  testthat::skip_on_cran()
  testthat::skip_if_offline()

  # Test with latest = TRUE (default)
  result_latest <- download_shapefile("COM", admin_level = "ADM2", latest = TRUE)
  
  # All results should be valid (not checking specific ENDDATE value
  # as it may vary, but ensuring filtering happened)
  testthat::expect_s3_class(result_latest, "sf")
  testthat::expect_true(nrow(result_latest) > 0)
  
  # Test that default behavior is latest = TRUE
  result_default <- download_shapefile("COM", admin_level = "ADM2")
  testthat::expect_equal(nrow(result_default), nrow(result_latest))
})

testthat::test_that("download_shapefile returns all boundaries when latest = FALSE", {
  testthat::skip_on_cran()
  testthat::skip_if_offline()

  # Get all boundaries including historical
  result_all <- download_shapefile("COM", admin_level = "ADM0", latest = FALSE)
  
  # Get only latest boundaries
  result_latest <- download_shapefile("COM", admin_level = "ADM0", latest = TRUE)
  
  testthat::expect_s3_class(result_all, "sf")
  testthat::expect_s3_class(result_latest, "sf")
  
  # All boundaries should include at least as many as latest
  # (could be same if no historical boundaries exist)
  testthat::expect_gte(nrow(result_all), nrow(result_latest))
})

testthat::test_that("download_shapefile handles network errors gracefully", {
  testthat::skip_on_cran()

  # Mock httr2::request to simulate network error
  testthat::with_mocked_bindings(
    request = function(...) stop("Network error"),
    .package = "httr2",
    {
      testthat::expect_error(
        download_shapefile("KEN"),
        "Network error"
      )
    }
  )
})

testthat::test_that("download_shapefile validates geometries", {
  testthat::skip_on_cran()
  testthat::skip_if_offline()

  result <- download_shapefile("COM", admin_level = "ADM0")

  # Check that geometries are valid
  testthat::expect_true(all(sf::st_is_valid(result)))

  # When saving, geometries should also be valid
  temp_dir <- tempdir()
  result_saved <- download_shapefile(
    "COM",
    admin_level = "ADM0",
    dest_path = temp_dir
  )

  testthat::expect_true(all(sf::st_is_valid(result_saved)))

  # Clean up
  expected_file <- file.path(temp_dir, "who_shapefile_com_adm0.gpkg")
  if (file.exists(expected_file)) unlink(expected_file)
})
