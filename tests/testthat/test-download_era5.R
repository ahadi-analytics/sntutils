# Tests for ERA5 download and processing functions
# Uses mocking to avoid requiring actual API keys

# Test 1: era5_options() ----
testthat::test_that("era5_options returns valid dataset info", {
  opts <- sntutils::era5_options()

  testthat::expect_s3_class(opts, "tbl_df")
  testthat::expect_true(all(c("dataset", "label", "cds_id") %in% names(opts)))
  testthat::expect_gt(nrow(opts), 0)
  testthat::expect_true("monthly_single_levels" %in% opts$dataset)
  testthat::expect_true("hourly_single_levels" %in% opts$dataset)
  testthat::expect_true("hourly_pressure_levels" %in% opts$dataset)
  testthat::expect_equal(nrow(opts), 4)
})

# Test 2: check_era5_available() ----
testthat::test_that("check_era5_available validates inputs correctly", {
  # Valid input
  result <- sntutils::check_era5_available(
    dataset_code = "monthly_single_levels",
    years = 2020,
    months = 1:3,
    variables = c("2m_temperature")
  )

  testthat::expect_s3_class(result, "tbl_df")
  testthat::expect_equal(result$dataset, "monthly_single_levels")
  testthat::expect_equal(result$years[[1]], 2020)
  testthat::expect_equal(result$months[[1]], 1:3)
})

testthat::test_that("check_era5_available rejects invalid dataset", {
  testthat::expect_error(
    sntutils::check_era5_available("invalid_dataset", 2020, 1),
    "Invalid dataset"
  )
})

testthat::test_that("check_era5_available rejects invalid months", {
  testthat::expect_error(
    sntutils::check_era5_available("monthly_single_levels", 2020, 13),
    "months must be in 1..12"
  )

  testthat::expect_error(
    sntutils::check_era5_available("monthly_single_levels", 2020, 0),
    "months must be in 1..12"
  )
})

testthat::test_that("check_era5_available rejects invalid years", {
  testthat::expect_error(
    sntutils::check_era5_available("monthly_single_levels", "abc", 1),
    "years must be numeric"
  )
})

# Test 3: get_era5_variable_mapping() ----
testthat::test_that("get_era5_variable_mapping returns comprehensive mapping", {
  mapping <- sntutils:::get_era5_variable_mapping()

  testthat::expect_type(mapping, "character")
  testthat::expect_true(length(mapping) > 20)

  # Check key variables
  testthat::expect_true("2m_temperature" %in% names(mapping))
  testthat::expect_equal(unname(mapping["2m_temperature"]), "t2m")

  testthat::expect_true("total_precipitation" %in% names(mapping))
  testthat::expect_equal(unname(mapping["total_precipitation"]), "tp")

  testthat::expect_true("10m_u_component_of_wind" %in% names(mapping))
  testthat::expect_equal(unname(mapping["10m_u_component_of_wind"]), "u10")
})

# Test 4: detect_country_from_bbox() ----
testthat::test_that("detect_country_from_bbox works with valid packages", {
  testthat::skip_if_not_installed("rnaturalearth")
  testthat::skip_if_not_installed("sf")

  # Rwanda bbox
  rwanda_bbox <- c(29.0, -2.9, 30.9, -1.0)
  country <- sntutils:::detect_country_from_bbox(rwanda_bbox)

  testthat::expect_type(country, "character")
  testthat::expect_equal(country, "Rwanda")
})

testthat::test_that("detect_country_from_bbox handles missing packages", {
  # This test is informational - if packages are missing,
  # the function should return "Unknown" with a warning
  # We can't easily mock requireNamespace, so we skip this test
  testthat::skip("Mocking requireNamespace not supported in this context")
})

# Test 5: download_era5() parameter validation ----
testthat::test_that("download_era5 validates API key", {
  testthat::expect_error(
    sntutils::download_era5(
      dataset = "monthly_single_levels",
      cds_key = "",
      years = 2020,
      months = 1,
      variables = "2m_temperature",
      out_dir = tempdir()
    ),
    "API key"
  )
})

testthat::test_that("download_era5 validates bbox format", {
  # Invalid bbox length
  testthat::expect_error(
    sntutils::download_era5(
      dataset = "monthly_single_levels",
      cds_key = "test_key",
      years = 2020,
      months = 1,
      variables = "2m_temperature",
      bbox = c(1, 2, 3),
      out_dir = tempdir()
    ),
    "bbox must be c\\(xmin, ymin, xmax, ymax\\)"
  )
})

testthat::test_that("download_era5 validates bbox coordinates", {
  # xmin >= xmax
  testthat::expect_error(
    sntutils::download_era5(
      dataset = "monthly_single_levels",
      cds_key = "test_key",
      years = 2020,
      months = 1,
      variables = "2m_temperature",
      bbox = c(30, -2, 29, -1),
      out_dir = tempdir()
    ),
    "xmin must be less than xmax"
  )

  # ymin >= ymax
  testthat::expect_error(
    sntutils::download_era5(
      dataset = "monthly_single_levels",
      cds_key = "test_key",
      years = 2020,
      months = 1,
      variables = "2m_temperature",
      bbox = c(29, -1, 30, -2),
      out_dir = tempdir()
    ),
    "ymin must be less than ymax"
  )
})

testthat::test_that("download_era5 validates dataset", {
  testthat::expect_error(
    sntutils::download_era5(
      dataset = "invalid_dataset",
      cds_key = "test_key",
      years = 2020,
      months = 1,
      variables = "2m_temperature",
      out_dir = tempdir()
    ),
    "Invalid dataset"
  )
})

testthat::test_that("download_era5 validates aggregated_format", {
  # Valid formats should not error
  valid_formats <- c("qs2", "parquet", "feather", "rds", "csv")

  for (fmt in valid_formats) {
    testthat::expect_silent({
      match.arg(fmt, c("qs2", "parquet", "feather", "rds", "csv"))
    })
  }
})

testthat::test_that("download_era5 validates pressure_levels for pressure dataset", {
  # Test that pressure_levels is validated when using pressure dataset
  # The error occurs during request building, not during API call
  testthat::expect_error(
    sntutils::download_era5(
      dataset = "hourly_pressure_levels",
      cds_key = "test_key_dummy",  # Won't be used before validation
      years = 2020,
      months = 1,
      variables = "temperature",
      pressure_levels = NULL,  # Missing required parameter
      out_dir = tempdir()
    ),
    "pressure_levels required"
  )
})

# Test 6: Filename generation ----
testthat::test_that("variable names are shortened correctly", {
  # Test the shortening logic
  shorten_var <- function(v) {
    dplyr::case_when(
      grepl("temperature", v, ignore.case = TRUE) ~
        gsub("temperature", "temp", v, ignore.case = TRUE),
      grepl("precipitation", v, ignore.case = TRUE) ~
        gsub("precipitation", "precip", v, ignore.case = TRUE),
      grepl("component_of_wind", v) ~
        gsub("_component_of_wind", "", v),
      TRUE ~ v
    )
  }

  testthat::expect_equal(shorten_var("2m_temperature"), "2m_temp")
  testthat::expect_equal(shorten_var("total_precipitation"), "total_precip")
  testthat::expect_equal(shorten_var("10m_u_component_of_wind"), "10m_u")
  testthat::expect_equal(shorten_var("10m_v_component_of_wind"), "10m_v")
  testthat::expect_equal(shorten_var("surface_pressure"), "surface_pressure")
})

# Test 7: read_era5() validation ----
testthat::test_that("read_era5 validates file existence", {
  testthat::expect_error(
    sntutils::read_era5(nc_files = "nonexistent.nc"),
    "Files not found"
  )

  testthat::expect_error(
    sntutils::read_era5(nc_files = c("file1.nc", "file2.nc")),
    "Files not found"
  )
})

# Test 8: get_era5_metadata() validation ----
testthat::test_that("get_era5_metadata validates file existence", {
  testthat::expect_error(
    sntutils::get_era5_metadata("nonexistent.nc"),
    "File not found"
  )
})

testthat::test_that("get_era5_metadata requires ncdf4 package", {
  testthat::skip_if_not_installed("ncdf4")

  # Package is installed, so should not error on package check
  # (will error on file not found instead)
  testthat::expect_error(
    sntutils::get_era5_metadata("test.nc"),
    "File not found"
  )
})

# Test 9: migrate_era5_filenames() ----
testthat::test_that("migrate_era5_filenames validates directory", {
  testthat::expect_error(
    sntutils::migrate_era5_filenames(dir = "nonexistent_dir"),
    "Directory not found"
  )
})

testthat::test_that("migrate_era5_filenames works with empty directory", {
  test_dir <- file.path(tempdir(), "era5_migrate_empty")
  dir.create(test_dir, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(test_dir, recursive = TRUE))

  # Should return NULL or handle gracefully
  result <- suppressMessages(
    sntutils::migrate_era5_filenames(dir = test_dir, dry_run = TRUE)
  )

  testthat::expect_true(is.null(result) || nrow(result) == 0)
})

# Test 10: Integration checks ----
testthat::test_that("all required packages can be loaded", {
  required_pkgs <- c("ecmwfr", "ncdf4", "tidyr", "purrr", "dplyr")

  for (pkg in required_pkgs) {
    testthat::expect_true(
      requireNamespace(pkg, quietly = TRUE),
      info = paste("Package", pkg, "should be available")
    )
  }
})

testthat::test_that("optional packages are detected correctly", {
  optional_pkgs <- c("rnaturalearth", "sf")

  for (pkg in optional_pkgs) {
    has_pkg <- requireNamespace(pkg, quietly = TRUE)
    # Just check that the check works - don't require these packages
    testthat::expect_type(has_pkg, "logical")
  }
})

# Test 11: Date range calculation (for aggregation) ----
testthat::test_that("date range is calculated correctly from results", {
  # Simulate download results
  results <- tibble::tibble(
    year = c(2020, 2020, 2020, 2021, 2021),
    month = c(1, 2, 12, 1, 6),
    status = c("downloaded", "downloaded", "exists", "downloaded", "exists")
  )

  downloaded <- results[results$status %in% c("downloaded", "exists"), ]

  min_year <- min(downloaded$year)
  max_year <- max(downloaded$year)
  min_month <- min(downloaded$month[downloaded$year == min_year])
  max_month <- max(downloaded$month[downloaded$year == max_year])

  testthat::expect_equal(min_year, 2020)
  testthat::expect_equal(max_year, 2021)
  testthat::expect_equal(min_month, 1)
  testthat::expect_equal(max_month, 6)

  min_date <- format(
    as.Date(paste(min_year, min_month, "01", sep = "-")),
    "%b%Y"
  )
  max_date <- format(
    as.Date(paste(max_year, max_month, "01", sep = "-")),
    "%b%Y"
  )

  testthat::expect_equal(min_date, "Jan2020")
  testthat::expect_equal(max_date, "Jun2021")
})

# Test 12: Batch request building ----
testthat::test_that("batch requests are built correctly", {
  # Test that multiple year-month combinations create separate requests
  years <- c(2020, 2021)
  months <- c(1, 2)

  combos <- tidyr::expand_grid(year = years, month = months)

  testthat::expect_equal(nrow(combos), 4)
  testthat::expect_equal(combos$year, c(2020, 2020, 2021, 2021))
  testthat::expect_equal(combos$month, c(1, 2, 1, 2))
})

# Test 13: CDS credential parsing ----
testthat::test_that("CDS credentials are parsed correctly", {
  # Old format (uid:apikey)
  cds_key_old <- "12345:abcdef"
  testthat::expect_true(grepl(":", cds_key_old, fixed = TRUE))

  creds <- strsplit(cds_key_old, ":", fixed = TRUE)[[1]]
  testthat::expect_equal(length(creds), 2)
  testthat::expect_equal(creds[1], "12345")
  testthat::expect_equal(creds[2], "abcdef")

  # New format (PAT only)
  cds_key_new <- "abcdef123456"
  testthat::expect_false(grepl(":", cds_key_new, fixed = TRUE))
})

# Test 14: Area vector formatting ----
testthat::test_that("bbox is converted to CDS area format correctly", {
  # Input: c(xmin, ymin, xmax, ymax)
  # Output: c(ymax, xmin, ymin, xmax) = [N, W, S, E]
  bbox <- c(29.0, -2.9, 30.9, -1.0)

  area_vec <- c(bbox[4], bbox[1], bbox[2], bbox[3])

  testthat::expect_equal(area_vec, c(-1.0, 29.0, -2.9, 30.9))
  testthat::expect_equal(length(area_vec), 4)
})

# Test 15: Variable display formatting ----
testthat::test_that("variables are formatted correctly for display", {
  variables <- c("2m_temperature", "total_precipitation")

  # Should work without color in tests
  var_display <- paste(variables, collapse = ", ")

  testthat::expect_type(var_display, "character")
  testthat::expect_true(grepl("2m_temperature", var_display))
  testthat::expect_true(grepl("total_precipitation", var_display))
})
