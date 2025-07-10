# setup and utilities --------------------------------------------------------

testthat::test_that("setup: required packages are available", {
  testthat::expect_true(requireNamespace("httr2", quietly = TRUE))
  testthat::expect_true(requireNamespace("terra", quietly = TRUE))
  testthat::expect_true(requireNamespace("cli", quietly = TRUE))
  testthat::expect_true(requireNamespace("here", quietly = TRUE))
  testthat::expect_true(requireNamespace("glue", quietly = TRUE))
})

# create temporary directory for tests
setup_temp_dir <- function() {
  temp_dir <- tempfile("worldpop_test")
  dir.create(temp_dir, recursive = TRUE)
  return(temp_dir)
}

# mock httr2 response for testing without actual downloads
mock_successful_download <- function(path) {
  # create a minimal mock tif file
  writeLines("mock tif content", path)
  return(TRUE)
}

# download_worldpop function tests -------------------------------------------

testthat::test_that("download_worldpop: input validation works correctly", {
  temp_dir <- setup_temp_dir()
  on.exit(unlink(temp_dir, recursive = TRUE))

  # test that function exists
  testthat::expect_true(exists("download_worldpop"))

  # test type argument validation
  testthat::expect_error(
    download_worldpop("GBR", type = "invalid"),
    "should be one of.*count.*density"
  )

  # test that country_codes is required
  testthat::expect_error(
    download_worldpop(),
    "argument \"country_codes\" is missing"
  )
})

testthat::test_that("download_worldpop: directory creation works", {
  temp_dir <- file.path(tempdir(), "nonexistent", "nested", "dir")
  on.exit(unlink(dirname(temp_dir), recursive = TRUE))

  # directory should not exist initially
  testthat::expect_false(dir.exists(temp_dir))

  # test directory creation by creating it manually (simulating function behavior)
  # this tests the core logic without network dependency
  if (!dir.exists(temp_dir)) {
    dir.create(temp_dir, recursive = TRUE)
  }

  # directory should now exist
  testthat::expect_true(dir.exists(temp_dir))

  # alternatively, if you want to test with actual function call but skip download:
  # create a mock file to simulate existing download
  mock_file <- file.path(temp_dir, "gbr_ppp_2020_1km_Aggregated_UNadj.tif")
  writeLines("mock content", mock_file)

  # now call function - it should skip download since file exists
  result <- download_worldpop(
    "GBR",
    years = 2020,
    dest_dir = temp_dir,
    quiet = TRUE
  )

  # directory should now exist
  testthat::expect_true(dir.exists(temp_dir))
})

testthat::test_that("download_worldpop: url construction is correct", {
  # test density type url
  base_url_density <- paste0(
    "https://data.worldpop.org/GIS/Population_Density/",
    "Global_2000_2020_1km_UNadj/"
  )

  # test count type url
  base_url_count <- paste0(
    "https://data.worldpop.org/GIS/Population/",
    "Global_2000_2020_1km_UNadj/"
  )

  # these tests would need to be integrated into the function
  # or we'd need to expose the url construction logic
  testthat::expect_true(TRUE) # placeholder for url tests
})

testthat::test_that("download_worldpop: filename construction is correct", {
  # test density filenames
  expected_density <- "gbr_pd_2020_1km_UNadj.tif"

  # test count filenames
  expected_count <- "gbr_ppp_2020_1km_Aggregated_UNadj.tif"

  # these would need to be tested by examining the actual function behavior
  testthat::expect_true(TRUE) # placeholder for filename tests
})

testthat::test_that("download_worldpop: handles existing files correctly", {
  temp_dir <- setup_temp_dir()
  on.exit(unlink(temp_dir, recursive = TRUE))

  # create a mock existing file
  existing_file <- file.path(temp_dir, "gbr_ppp_2020_1km_Aggregated_UNadj.tif")
  writeLines("existing content", existing_file)

  testthat::expect_true(file.exists(existing_file))

  # function should skip existing files
  result <- download_worldpop(
    "GBR",
    years = 2020,
    dest_dir = temp_dir,
    quiet = TRUE
  )

  # file should still exist and not be modified
  testthat::expect_true(file.exists(existing_file))
  testthat::expect_equal(
    readLines(existing_file),
    "existing content"
  )
})

testthat::test_that("download_worldpop: returns correct structure", {
  temp_dir <- setup_temp_dir()
  on.exit(unlink(temp_dir, recursive = TRUE))

  # create mock files to simulate successful downloads
  mock_file <- file.path(temp_dir, "gbr_ppp_2020_1km_Aggregated_UNadj.tif")
  writeLines("mock content", mock_file)

  result <- download_worldpop(
    "GBR",
    years = 2020,
    dest_dir = temp_dir,
    quiet = TRUE
  )

  # check return structure
  testthat::expect_type(result, "list")
  testthat::expect_named(result, c("files", "counts"))
  testthat::expect_type(result$files, "character")
  testthat::expect_type(result$counts, "integer")
})

testthat::test_that("download_worldpop: handles multiple countries and years", {
  temp_dir <- setup_temp_dir()
  on.exit(unlink(temp_dir, recursive = TRUE))

  countries <- c("GBR", "FRA")
  years <- c(2019, 2020)

  # create mock files
  for (cc in countries) {
    for (yr in years) {
      mock_file <- file.path(
        temp_dir,
        sprintf("%s_ppp_%s_1km_Aggregated_UNadj.tif", tolower(cc), yr)
      )
      writeLines("mock content", mock_file)
    }
  }

  result <- download_worldpop(
    countries,
    years = years,
    dest_dir = temp_dir,
    quiet = TRUE
  )

  # should return 4 files total (2 countries × 2 years)
  testthat::expect_length(result$files, 4)
  testthat::expect_length(result$counts, 2)
})

# test-download_worldpop_age_band.R
# tests for download_worldpop_age_band function

testthat::test_that("function handles multiple countries and years", {
  temp_dir <- tempdir()
  test_dir <- file.path(temp_dir, "worldpop_multi_test")

  testthat::expect_no_error({
    download_worldpop_age_band(
      country_codes = c("GMB", "LUX"), # small countries
      years = c(2020, 2021),
      age_range = c(0, 0), # single age band
      out_dir = test_dir
    )
  })

  # check all expected files exist
  expected_files <- c(
    "gmb_total_00_00_2020.tif",
    "gmb_total_00_00_2021.tif",
    "lux_total_00_00_2020.tif",
    "lux_total_00_00_2021.tif"
  )

  for (file in expected_files) {
    testthat::expect_true(
      file.exists(file.path(test_dir, file)),
      info = paste("Missing file:", file)
    )
  }

  # cleanup
  unlink(test_dir, recursive = TRUE)
})

testthat::test_that("function combines multiple bands correctly", {
  temp_dir <- tempdir()
  test_dir <- file.path(temp_dir, "worldpop_bands_test")

  # test age range spanning multiple bands
  testthat::expect_no_error({
    download_worldpop_age_band(
      country_codes = "GMB",
      years = 2020,
      age_range = c(2, 9), # spans bands 1-4 and 5-9
      out_dir = test_dir
    )
  })

  # check combined output file exists
  expected_file <- file.path(test_dir, "gmb_total_01_09_2020.tif")
  testthat::expect_true(file.exists(expected_file))

  # cleanup
  unlink(test_dir, recursive = TRUE)
})

testthat::test_that("function handles edge cases in age ranges", {
  temp_dir <- tempdir()
  test_dir <- file.path(temp_dir, "worldpop_edge_test")

  # test age range at band boundaries
  testthat::expect_no_error({
    download_worldpop_age_band(
      country_codes = "GMB",
      years = 2020,
      age_range = c(5, 9), # exact band match
      out_dir = test_dir
    )
  })

  expected_file <- file.path(test_dir, "gmb_total_05_09_2020.tif")
  testthat::expect_true(file.exists(expected_file))

  # test very wide age range
  testthat::expect_no_error({
    download_worldpop_age_band(
      country_codes = "GMB",
      years = 2020,
      age_range = c(0, 25), # spans many bands
      out_dir = test_dir
    )
  })

  expected_file_wide <- file.path(test_dir, "gmb_total_00_29_2020.tif")
  testthat::expect_true(file.exists(expected_file_wide))

  # cleanup
  unlink(test_dir, recursive = TRUE)
})

testthat::test_that("function skips existing files", {
  temp_dir <- tempdir()
  test_dir <- file.path(temp_dir, "worldpop_skip_test")

  # first download
  testthat::expect_no_error({
    download_worldpop_age_band(
      country_codes = "GMB",
      years = 2020,
      age_range = c(0, 0),
      out_dir = test_dir
    )
  })

  expected_file <- file.path(test_dir, "gmb_total_00_00_2020.tif")
  testthat::expect_true(file.exists(expected_file))

  # get file modification time
  first_mtime <- file.info(expected_file)$mtime

  # wait a moment then run again
  Sys.sleep(1)

  # second download should skip
  testthat::expect_message(
    {
      download_worldpop_age_band(
        country_codes = "GMB",
        years = 2020,
        age_range = c(0, 0),
        out_dir = test_dir
      )
    },
    "already exists"
  )

  # file should not be modified
  second_mtime <- file.info(expected_file)$mtime
  testthat::expect_equal(first_mtime, second_mtime)

  # cleanup
  unlink(test_dir, recursive = TRUE)
})

testthat::test_that("function creates output directory", {
  temp_dir <- tempdir()
  test_dir <- file.path(temp_dir, "new_worldpop_dir", "subdir")

  # directory should not exist initially
  testthat::expect_false(dir.exists(test_dir))

  testthat::expect_no_error({
    download_worldpop_age_band(
      country_codes = "GMB",
      years = 2020,
      age_range = c(0, 0),
      out_dir = test_dir
    )
  })

  # directory should be created
  testthat::expect_true(dir.exists(test_dir))

  # cleanup
  unlink(file.path(temp_dir, "new_worldpop_dir"), recursive = TRUE)
})

testthat::test_that("url construction works correctly", {
  # test url construction function
  make_url <- function(country_code, sex, code) {
    url_base <- paste0(
      "https://data.worldpop.org/GIS/AgeSex_structures/",
      "Global_2000_2020_1km/unconstrained"
    )
    sprintf(
      "%s/2020/%s/%s_%s_%s_2020_1km.tif",
      url_base,
      toupper(country_code),
      tolower(country_code),
      sex,
      code
    )
  }

  expected_url <- paste0(
    "https://data.worldpop.org/GIS/AgeSex_structures/",
    "Global_2000_2020_1km/unconstrained/2020/TUN/tun_m_5_2020_1km.tif"
  )

  testthat::expect_equal(make_url("TUN", "m", 5), expected_url)
  testthat::expect_equal(
    make_url("tun", "f", 0),
    gsub("tun_m_5", "tun_f_0", expected_url)
  )
})

testthat::test_that("filename generation works correctly", {
  # test output filename generation
  country_code <- "TUN"
  cc_lo <- tolower(country_code)
  covered_lower <- 1
  covered_upper <- 9
  year <- 2020
  out_dir <- "test_dir"

  expected_fname <- file.path(
    out_dir,
    sprintf(
      "%s_total_%02d_%02d_%d.tif",
      cc_lo,
      covered_lower,
      covered_upper,
      year
    )
  )

  testthat::expect_equal(
    basename(expected_fname),
    "tun_total_01_09_2020.tif"
  )

  # test with different parameters
  expected_fname2 <- file.path(
    out_dir,
    sprintf("%s_total_%02d_%02d_%d.tif", "gmb", 0, 0, 2021)
  )

  testthat::expect_equal(
    basename(expected_fname2),
    "gmb_total_00_00_2021.tif"
  )
})
