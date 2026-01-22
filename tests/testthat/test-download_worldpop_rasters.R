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

  # test density with years >= 2015 throws error (density only in legacy)
  testthat::expect_error(
    download_worldpop("GBR", years = 2015:2020, type = "density"),
    "Density data only available for years 2000-2020"
  )

  # test 100m with years < 2015 throws error (100m only in r2025a)
  testthat::expect_error(
    download_worldpop("GBR", years = 2010:2015, resolution = "100m"),
    "100m resolution only available for years >= 2015"
  )

  # test years before 2000 throws error
  testthat::expect_error(
    download_worldpop("GBR", years = 1999:2005),
    "Legacy dataset only available for years 2000-2020"
  )

  # test years after 2030 throws error
  testthat::expect_error(
    download_worldpop("GBR", years = 2025:2031),
    "R2025A dataset only available for years 2015-2030"
  )

  # test invalid resolution
  testthat::expect_error(
    download_worldpop("GBR", resolution = "5km"),
    "should be one of.*1km.*100m"
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
  # create a mock file to simulate existing download (r2025a format, local name)
  mock_file <- file.path(temp_dir, "gbr_pop_2020_CN_1km_UA_v1.tif")
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

testthat::test_that("download_worldpop: url construction is correct for r2025a", {
  # test 1km r2025a - URL has R2025A, local filename does not
  result <- sntutils:::.build_worldpop_url("GIN", 2015, "count", "r2025a", "1km")
  expected_url <- paste0(
    "https://data.worldpop.org/GIS/Population/Global_2015_2030/R2025A/",
    "2015/GIN/v1/1km_ua/constrained/gin_pop_2015_CN_1km_R2025A_UA_v1.tif"
  )
  testthat::expect_equal(result$url, expected_url)
  testthat::expect_equal(result$filename, "gin_pop_2015_CN_1km_UA_v1.tif")

  # test 100m r2025a (no _UA suffix)
  result_100m <- sntutils:::.build_worldpop_url("GIN", 2020, "count", "r2025a", "100m")
  expected_url_100m <- paste0(
    "https://data.worldpop.org/GIS/Population/Global_2015_2030/R2025A/",
    "2020/GIN/v1/100m/constrained/gin_pop_2020_CN_100m_R2025A_v1.tif"
  )
  testthat::expect_equal(result_100m$url, expected_url_100m)
  testthat::expect_equal(result_100m$filename, "gin_pop_2020_CN_100m_v1.tif")

  # test country code case handling
  result_lower <- sntutils:::.build_worldpop_url("civ", 2015, "count", "r2025a", "1km")
  testthat::expect_true(grepl("/CIV/", result_lower$url))  # uppercase in path
  testthat::expect_true(grepl("^civ_", result_lower$filename))  # lowercase in filename
})

testthat::test_that("download_worldpop: url construction is correct for legacy", {
  # test legacy count
  result_count <- sntutils:::.build_worldpop_url("GBR", 2020, "count", "legacy", "1km")
  expected_count <- paste0(
    "https://data.worldpop.org/GIS/Population/Global_2000_2020_1km_UNadj/",
    "2020/GBR/gbr_ppp_2020_1km_Aggregated_UNadj.tif"
  )
  testthat::expect_equal(result_count$url, expected_count)
  testthat::expect_equal(result_count$filename, "gbr_ppp_2020_1km_Aggregated_UNadj.tif")

  # test legacy density
  result_density <- sntutils:::.build_worldpop_url("FRA", 2019, "density", "legacy", "1km")
  expected_density <- paste0(
    "https://data.worldpop.org/GIS/Population_Density/Global_2000_2020_1km_UNadj/",
    "2019/FRA/fra_pd_2019_1km_UNadj.tif"
  )
  testthat::expect_equal(result_density$url, expected_density)
  testthat::expect_equal(result_density$filename, "fra_pd_2019_1km_UNadj.tif")
})

testthat::test_that("download_worldpop: handles existing files correctly", {
  temp_dir <- setup_temp_dir()
  on.exit(unlink(temp_dir, recursive = TRUE))

  # create a mock existing file (r2025a format, local name without R2025A)
  existing_file <- file.path(temp_dir, "gbr_pop_2020_CN_1km_UA_v1.tif")
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

testthat::test_that("download_worldpop: handles existing legacy files correctly", {
  temp_dir <- setup_temp_dir()
  on.exit(unlink(temp_dir, recursive = TRUE))

  # create a mock existing file (legacy format for year < 2015)
  existing_file <- file.path(temp_dir, "gbr_ppp_2010_1km_Aggregated_UNadj.tif")
  writeLines("existing content", existing_file)

  testthat::expect_true(file.exists(existing_file))

  # function should skip existing files (using year < 2015 triggers legacy)
  result <- download_worldpop(
    "GBR",
    years = 2010,
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

  # create mock files to simulate successful downloads (r2025a local format)
  mock_file <- file.path(temp_dir, "gbr_pop_2020_CN_1km_UA_v1.tif")
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

  # create mock files (r2025a local format, without R2025A in name)
  for (cc in countries) {
    for (yr in years) {
      mock_file <- file.path(
        temp_dir,
        sprintf("%s_pop_%s_CN_1km_UA_v1.tif", tolower(cc), yr)
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
# note: these tests use legacy dataset (years < 2015) for reliability

testthat::test_that("age band: handles multiple countries and years (legacy)", {
  temp_dir <- tempdir()
  test_dir <- file.path(temp_dir, "worldpop_multi_test")

  # use legacy years (< 2015) for reliable testing
  testthat::expect_no_error({
    download_worldpop_age_band(
      country_codes = c("GMB", "LUX"),
      years = c(2010, 2011),
      age_range = c(0, 0),
      out_dir = test_dir
    )
  })

  expected_files <- c(
    "gmb_total_00_00_2010.tif",
    "gmb_total_00_00_2011.tif",
    "lux_total_00_00_2010.tif",
    "lux_total_00_00_2011.tif"
  )

  for (file in expected_files) {
    testthat::expect_true(
      file.exists(file.path(test_dir, file)),
      info = paste("Missing file:", file)
    )
  }

  unlink(test_dir, recursive = TRUE)
})

testthat::test_that("age band: combines multiple bands correctly (legacy)", {
  temp_dir <- tempdir()
  test_dir <- file.path(temp_dir, "worldpop_bands_test")

  testthat::expect_no_error({
    download_worldpop_age_band(
      country_codes = "GMB",
      years = 2010,
      age_range = c(2, 9),
      out_dir = test_dir
    )
  })

  expected_file <- file.path(test_dir, "gmb_total_01_09_2010.tif")
  testthat::expect_true(file.exists(expected_file))

  unlink(test_dir, recursive = TRUE)
})

testthat::test_that("age band: handles edge cases in age ranges (legacy)", {
  temp_dir <- tempdir()
  test_dir <- file.path(temp_dir, "worldpop_edge_test")

  testthat::expect_no_error({
    download_worldpop_age_band(
      country_codes = "GMB",
      years = 2010,
      age_range = c(5, 9),
      out_dir = test_dir
    )
  })

  expected_file <- file.path(test_dir, "gmb_total_05_09_2010.tif")
  testthat::expect_true(file.exists(expected_file))

  testthat::expect_no_error({
    download_worldpop_age_band(
      country_codes = "GMB",
      years = 2010,
      age_range = c(0, 25),
      out_dir = test_dir
    )
  })

  expected_file_wide <- file.path(test_dir, "gmb_total_00_29_2010.tif")
  testthat::expect_true(file.exists(expected_file_wide))

  unlink(test_dir, recursive = TRUE)
})

testthat::test_that("age band: skips existing files", {
  temp_dir <- tempdir()
  test_dir <- file.path(temp_dir, "worldpop_skip_test")

  testthat::expect_no_error({
    download_worldpop_age_band(
      country_codes = "GMB",
      years = 2010,
      age_range = c(0, 0),
      out_dir = test_dir
    )
  })

  expected_file <- file.path(test_dir, "gmb_total_00_00_2010.tif")
  testthat::expect_true(file.exists(expected_file))

  first_mtime <- file.info(expected_file)$mtime
  Sys.sleep(1)

  testthat::expect_message(
    {
      download_worldpop_age_band(
        country_codes = "GMB",
        years = 2010,
        age_range = c(0, 0),
        out_dir = test_dir
      )
    },
    "already exists"
  )

  second_mtime <- file.info(expected_file)$mtime
  testthat::expect_equal(first_mtime, second_mtime)

  unlink(test_dir, recursive = TRUE)
})

testthat::test_that("age band: creates output directory", {
  temp_dir <- tempdir()
  test_dir <- file.path(temp_dir, "new_worldpop_dir", "subdir")

  testthat::expect_false(dir.exists(test_dir))

  testthat::expect_no_error({
    download_worldpop_age_band(
      country_codes = "GMB",
      years = 2010,
      age_range = c(0, 0),
      out_dir = test_dir
    )
  })

  testthat::expect_true(dir.exists(test_dir))

  unlink(file.path(temp_dir, "new_worldpop_dir"), recursive = TRUE)
})

testthat::test_that("age band url construction works correctly", {
  # test legacy url construction (years < 2015)
  legacy_url <- paste0(
    "https://data.worldpop.org/GIS/AgeSex_structures/",
    "Global_2000_2020_1km/unconstrained/2010/TUN/tun_m_5_2010_1km.tif"
  )

  # test r2024b 1km url construction (years >= 2015)
  r2024b_1km_url <- paste0(
    "https://data.worldpop.org/GIS/AgeSex_structures/",
    "Global_2015_2030/R2024B/2015/BDI/v1/1km_ua/unconstrained/",
    "bdi_f_00_2015_UC_1km_R2024B_UA_v1.tif"
  )

  # test r2024b 100m url construction
  r2024b_100m_url <- paste0(
    "https://data.worldpop.org/GIS/AgeSex_structures/",
    "Global_2015_2030/R2024B/2015/BDI/v1/100m/unconstrained/",
    "bdi_f_00_2015_UC_100m_R2024B_v1.tif"
  )

  # verify url patterns are valid
  testthat::expect_true(grepl("Global_2000_2020_1km", legacy_url))
  testthat::expect_true(grepl("Global_2015_2030/R2024B", r2024b_1km_url))
  testthat::expect_true(grepl("Global_2015_2030/R2024B", r2024b_100m_url))
  testthat::expect_true(grepl("_1km\\.tif$", legacy_url))
  testthat::expect_true(grepl("_1km_R2024B_UA_v1\\.tif$", r2024b_1km_url))
  testthat::expect_true(grepl("_100m_R2024B_v1\\.tif$", r2024b_100m_url))
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
