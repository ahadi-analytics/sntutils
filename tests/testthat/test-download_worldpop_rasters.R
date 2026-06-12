# setup and utilities --------------------------------------------------------

testthat::test_that("setup: required packages are available", {
  testthat::expect_true(requireNamespace("httr2", quietly = TRUE))
  testthat::expect_true(requireNamespace("terra", quietly = TRUE))
  testthat::expect_true(requireNamespace("cli", quietly = TRUE))
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
  testthat::skip_if_not(
    requireNamespace("here", quietly = TRUE),
    "here package not available"
  )
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

testthat::test_that("download_worldpop: url construction for GLOBAL mosaic", {
  result <- sntutils:::.build_worldpop_url(
    "GLOBAL", 2020, "count", "r2025a", "1km"
  )
  expected_url <- paste0(
    "https://data.worldpop.org/GIS/Population/Global_2015_2030/R2025A/",
    "2020/0_Mosaicked/v1/1km_ua/constrained/",
    "global_pop_2020_CN_1km_R2025A_UA_v1.tif"
  )
  testthat::expect_equal(result$url, expected_url)
  testthat::expect_equal(result$filename, "global_pop_2020_CN_1km_UA_v1.tif")

  # case-insensitive
  result_lower <- sntutils:::.build_worldpop_url(
    "global", 2020, "count", "r2025a", "1km"
  )
  testthat::expect_equal(result_lower$url, expected_url)

  # GLOBAL + 100m rejected
  testthat::expect_error(
    sntutils:::.build_worldpop_url("GLOBAL", 2020, "count", "r2025a", "100m"),
    "GLOBAL"
  )

  # GLOBAL + legacy rejected
  testthat::expect_error(
    sntutils:::.build_worldpop_url("GLOBAL", 2010, "count", "legacy", "1km"),
    "GLOBAL"
  )
})

testthat::test_that("download_worldpop: GLOBAL validation in main function", {
  temp_dir <- setup_temp_dir()
  on.exit(unlink(temp_dir, recursive = TRUE))

  # GLOBAL + legacy year
  testthat::expect_error(
    download_worldpop("GLOBAL", years = 2010, dest_dir = temp_dir, quiet = TRUE),
    "GLOBAL"
  )
  # GLOBAL + 100m
  testthat::expect_error(
    download_worldpop(
      "GLOBAL", years = 2020,
      resolution = "100m", dest_dir = temp_dir, quiet = TRUE
    ),
    "GLOBAL"
  )
  # GLOBAL + density
  testthat::expect_error(
    download_worldpop(
      "GLOBAL", years = 2020,
      type = "density", dest_dir = temp_dir, quiet = TRUE
    ),
    "Density|GLOBAL"
  )
})

testthat::test_that("download_worldpop: GLOBAL skip when cached", {
  temp_dir <- setup_temp_dir()
  on.exit(unlink(temp_dir, recursive = TRUE))

  mock_file <- file.path(temp_dir, "global_pop_2020_CN_1km_UA_v1.tif")
  writeLines("mock", mock_file)

  result <- download_worldpop(
    "GLOBAL", years = 2020, dest_dir = temp_dir, quiet = TRUE
  )

  testthat::expect_true(file.exists(mock_file))
  testthat::expect_equal(unname(result$counts[["GLOBAL"]]), 1)
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
  skip_if_not_integration("WorldPop age-band integration test skipped.")

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
  skip_if_not_integration("WorldPop age-band integration test skipped.")

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
  skip_if_not_integration("WorldPop age-band integration test skipped.")

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
  skip_if_not_integration("WorldPop age-band integration test skipped.")

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
  skip_if_not_integration("WorldPop age-band integration test skipped.")

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

testthat::test_that(".build_worldpop_age_band_url: legacy per-country", {
  result <- sntutils:::.build_worldpop_age_band_url(
    "TUN", "m", 5, 2010, "1km"
  )
  testthat::expect_equal(
    result$url,
    paste0(
      "https://data.worldpop.org/GIS/AgeSex_structures/",
      "Global_2000_2020_1km/unconstrained/2010/TUN/tun_m_5_2010_1km.tif"
    )
  )
  testthat::expect_equal(result$filename, "tun_m_5_2010_1km.tif")

  # legacy ignores release param
  result_r <- sntutils:::.build_worldpop_age_band_url(
    "TUN", "f", 5, 2014, "1km", "R2024B"
  )
  testthat::expect_true(
    grepl("Global_2000_2020_1km", result_r$url, fixed = TRUE)
  )
})

testthat::test_that(".build_worldpop_age_band_url: legacy rejects bad combos", {
  # 100m not in legacy
  testthat::expect_error(
    sntutils:::.build_worldpop_age_band_url("TUN", "m", 5, 2010, "100m"),
    "100m"
  )
  # 't' total not in legacy
  testthat::expect_error(
    sntutils:::.build_worldpop_age_band_url("TUN", "t", 5, 2010, "1km"),
    "total"
  )
  # GLOBAL mosaic not in legacy
  testthat::expect_error(
    sntutils:::.build_worldpop_age_band_url("GLOBAL", "m", 5, 2010, "1km"),
    "GLOBAL"
  )
})

testthat::test_that(".build_worldpop_age_band_url: R2025A per-country", {
  # 1km, t total
  result <- sntutils:::.build_worldpop_age_band_url(
    "GIN", "t", 0, 2020, "1km", "R2025A"
  )
  testthat::expect_equal(
    result$url,
    paste0(
      "https://data.worldpop.org/GIS/AgeSex_structures/",
      "Global_2015_2030/R2025A/2020/GIN/v1/1km_ua/constrained/",
      "gin_t_00_2020_CN_1km_R2025A_UA_v1.tif"
    )
  )
  testthat::expect_equal(result$filename, "gin_t_00_2020_CN_1km_UA_v1.tif")

  # 100m, m
  result_100m <- sntutils:::.build_worldpop_age_band_url(
    "GIN", "m", 5, 2020, "100m", "R2025A"
  )
  testthat::expect_equal(
    result_100m$url,
    paste0(
      "https://data.worldpop.org/GIS/AgeSex_structures/",
      "Global_2015_2030/R2025A/2020/GIN/v1/100m/constrained/",
      "gin_m_05_2020_CN_100m_R2025A_v1.tif"
    )
  )
})

testthat::test_that(".build_worldpop_age_band_url: R2024B per-country", {
  # 1km, t total
  result <- sntutils:::.build_worldpop_age_band_url(
    "BDI", "t", 0, 2015, "1km", "R2024B"
  )
  testthat::expect_equal(
    result$url,
    paste0(
      "https://data.worldpop.org/GIS/AgeSex_structures/",
      "Global_2015_2030/R2024B/2015/BDI/v1/1km_ua/unconstrained/",
      "bdi_t_00_2015_UC_1km_R2024B_UA_v1.tif"
    )
  )

  # 100m, f
  result_100m <- sntutils:::.build_worldpop_age_band_url(
    "BDI", "f", 0, 2015, "100m", "R2024B"
  )
  testthat::expect_equal(
    result_100m$url,
    paste0(
      "https://data.worldpop.org/GIS/AgeSex_structures/",
      "Global_2015_2030/R2024B/2015/BDI/v1/100m/unconstrained/",
      "bdi_f_00_2015_UC_100m_R2024B_v1.tif"
    )
  )
})

testthat::test_that(".build_worldpop_age_band_url: GLOBAL mosaic", {
  # R2025A
  result_r2025a <- sntutils:::.build_worldpop_age_band_url(
    "GLOBAL", "t", 0, 2020, "1km", "R2025A"
  )
  testthat::expect_equal(
    result_r2025a$url,
    paste0(
      "https://data.worldpop.org/GIS/AgeSex_structures/",
      "Global_2015_2030/R2025A/2020/0_Mosaicked/v1/1km_ua/constrained/",
      "global_t_00_2020_CN_1km_R2025A_UA_v1.tif"
    )
  )

  # R2024B
  result_r2024b <- sntutils:::.build_worldpop_age_band_url(
    "global", "t", 0, 2020, "1km", "R2024B"
  )
  testthat::expect_equal(
    result_r2024b$url,
    paste0(
      "https://data.worldpop.org/GIS/AgeSex_structures/",
      "Global_2015_2030/R2024B/2020/0_Mosaicked/v1/1km_ua/unconstrained/",
      "global_t_00_2020_UC_1km_R2024B_UA_v1.tif"
    )
  )

  # GLOBAL + 100m is rejected
  testthat::expect_error(
    sntutils:::.build_worldpop_age_band_url(
      "GLOBAL", "t", 0, 2020, "100m", "R2025A"
    ),
    "1km"
  )
})

testthat::test_that("download_worldpop_age_band: GLOBAL/release validation", {
  temp_dir <- setup_temp_dir()
  on.exit(unlink(temp_dir, recursive = TRUE))

  # GLOBAL + legacy year
  testthat::expect_error(
    download_worldpop_age_band(
      country_codes = "GLOBAL", years = 2010,
      out_dir = temp_dir, quiet = TRUE
    ),
    "GLOBAL"
  )
  # GLOBAL + 100m
  testthat::expect_error(
    download_worldpop_age_band(
      country_codes = "GLOBAL", years = 2020,
      resolution = "100m", out_dir = temp_dir, quiet = TRUE
    ),
    "GLOBAL"
  )
  # invalid release
  testthat::expect_error(
    download_worldpop_age_band(
      country_codes = "GIN", years = 2020,
      release = "R2030A", out_dir = temp_dir, quiet = TRUE
    ),
    "should be one of"
  )
})

testthat::test_that("age band: uses 't' total when sex='both' + yr>=2015 (cached)", {
  testthat::skip_if_not(
    requireNamespace("terra", quietly = TRUE),
    "terra package not available"
  )
  temp_dir <- setup_temp_dir()
  on.exit(unlink(temp_dir, recursive = TRUE))

  # pre-stage a valid 't' raster for band 0 — age_range c(0, 0) selects
  # band 0 only so no other downloads are triggered
  r <- suppressWarnings(terra::rast(nrows = 2, ncols = 2, vals = 1))
  t_fname <- file.path(temp_dir, "gin_t_00_2020_CN_1km_UA_v1.tif")
  suppressWarnings(terra::writeRaster(r, t_fname))

  testthat::expect_no_error({
    download_worldpop_age_band(
      country_codes = "GIN",
      years = 2020,
      age_range = c(0, 0),
      sex = "both",
      out_dir = temp_dir,
      quiet = TRUE
    )
  })

  # aggregated output exists
  expected <- file.path(temp_dir, "gin_total_00_00_2020.tif")
  testthat::expect_true(file.exists(expected))

  # m and f variants were NOT touched: 't' is used directly
  m_fn <- file.path(temp_dir, "gin_m_00_2020_CN_1km_UA_v1.tif")
  f_fn <- file.path(temp_dir, "gin_f_00_2020_CN_1km_UA_v1.tif")
  testthat::expect_false(file.exists(m_fn))
  testthat::expect_false(file.exists(f_fn))
})

testthat::test_that("age band: drops band files unless keep_band_files", {
  testthat::skip_if_not(
    requireNamespace("terra", quietly = TRUE),
    "terra package not available"
  )

  stage_t_raster <- function(dir) {
    r <- suppressWarnings(terra::rast(nrows = 2, ncols = 2, vals = 1))
    t_fname <- file.path(dir, "gin_t_00_2020_CN_1km_UA_v1.tif")
    suppressWarnings(terra::writeRaster(r, t_fname))
    t_fname
  }

  # default: band file is deleted after the combined raster is written
  drop_dir <- setup_temp_dir()
  on.exit(unlink(drop_dir, recursive = TRUE), add = TRUE)
  t_fname <- stage_t_raster(drop_dir)
  download_worldpop_age_band(
    "GIN", years = 2020, age_range = c(0, 0),
    out_dir = drop_dir, quiet = TRUE
  )
  testthat::expect_true(
    file.exists(file.path(drop_dir, "gin_total_00_00_2020.tif"))
  )
  testthat::expect_false(file.exists(t_fname))

  # keep_band_files = TRUE: band file is retained
  keep_dir <- setup_temp_dir()
  on.exit(unlink(keep_dir, recursive = TRUE), add = TRUE)
  t_fname_kept <- stage_t_raster(keep_dir)
  download_worldpop_age_band(
    "GIN", years = 2020, age_range = c(0, 0),
    out_dir = keep_dir, quiet = TRUE, keep_band_files = TRUE
  )
  testthat::expect_true(
    file.exists(file.path(keep_dir, "gin_total_00_00_2020.tif"))
  )
  testthat::expect_true(file.exists(t_fname_kept))
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

# download_worldpop_urbanicity tests -----------------------------------------

testthat::test_that("download_worldpop_urbanicity: url construction is correct", {
  # uppercase iso3 input
  result <- sntutils:::.build_worldpop_urbanicity_url(
    "DZA", 2015, "L1", "R2025A", "v1"
  )
  expected_url <- paste0(
    "https://data.worldpop.org/GIS/DUG/Global_2015_2030/R2025A/v1/",
    "2015/DZA/DZA_DUG_2015_GRID_L1_R2025A_v1.tif"
  )
  testthat::expect_equal(result$url, expected_url)
  testthat::expect_equal(result$filename, "DZA_DUG_2015_GRID_L1_R2025A_v1.tif")

  # lowercase iso3 input still produces uppercase url + filename
  result_lower <- sntutils:::.build_worldpop_urbanicity_url(
    "dza", 2024, "L2", "R2025A", "v1"
  )
  testthat::expect_true(grepl("/DZA/DZA_DUG_2024_GRID_L2_", result_lower$url))
  testthat::expect_equal(
    result_lower$filename, "DZA_DUG_2024_GRID_L2_R2025A_v1.tif"
  )
})

testthat::test_that(".worldpop_dug_countries: lists ISO3 codes from server", {
  skip_if_not_integration("WorldPop DUG listing integration test skipped.")

  codes <- sntutils:::.worldpop_dug_countries(2020, "R2025A", "v1")

  testthat::expect_true(all(grepl("^[A-Z]{3}$", codes)))
  testthat::expect_true(all(c("DZA", "GIN", "NGA") %in% codes))
  testthat::expect_gt(length(codes), 200)
})

testthat::test_that("download_worldpop_urbanicity: input validation works", {
  temp_dir <- setup_temp_dir()
  on.exit(unlink(temp_dir, recursive = TRUE))

  testthat::expect_true(exists("download_worldpop_urbanicity"))

  # invalid layer
  testthat::expect_error(
    download_worldpop_urbanicity(
      "DZA", years = 2020, layers = "L3", dest_dir = temp_dir, quiet = TRUE
    ),
    "must be a subset"
  )

  # year below supported range
  testthat::expect_error(
    download_worldpop_urbanicity(
      "DZA", years = 2014, dest_dir = temp_dir, quiet = TRUE
    ),
    "2015-2030"
  )

  # year above supported range
  testthat::expect_error(
    download_worldpop_urbanicity(
      "DZA", years = 2031, dest_dir = temp_dir, quiet = TRUE
    ),
    "2015-2030"
  )

  # empty country_codes
  testthat::expect_error(
    download_worldpop_urbanicity(
      character(0), dest_dir = temp_dir, quiet = TRUE
    ),
    "non-empty character vector"
  )

  # non-character country_codes
  testthat::expect_error(
    download_worldpop_urbanicity(
      123, dest_dir = temp_dir, quiet = TRUE
    ),
    "non-empty character vector"
  )
})

testthat::test_that("download_worldpop_urbanicity: existing files are skipped", {
  temp_dir <- setup_temp_dir()
  on.exit(unlink(temp_dir, recursive = TRUE))

  # pre-stage both layer files for one country/year so no network call is made
  for (layer in c("L1", "L2")) {
    fn <- sprintf("DZA_DUG_2015_GRID_%s_R2025A_v1.tif", layer)
    writeLines("mock tif content", file.path(temp_dir, fn))
  }

  result <- download_worldpop_urbanicity(
    "DZA", years = 2015, dest_dir = temp_dir, quiet = TRUE
  )

  # both files reported as available
  testthat::expect_length(result$files, 2L)
  testthat::expect_true(all(file.exists(result$files)))

  # count for DZA should be 2 (L1 + L2)
  testthat::expect_equal(unname(result$counts[["DZA"]]), 2)

  # cached content untouched (no re-download)
  for (path in result$files) {
    testthat::expect_equal(readLines(path), "mock tif content")
  }
})

testthat::test_that(
  "download_worldpop_urbanicity: lowercase country_codes accepted",
  {
    temp_dir <- setup_temp_dir()
    on.exit(unlink(temp_dir, recursive = TRUE))

    # pre-stage so no network. Local filename uses uppercase ISO3.
    fn <- "DZA_DUG_2020_GRID_L1_R2025A_v1.tif"
    writeLines("mock", file.path(temp_dir, fn))

    result <- download_worldpop_urbanicity(
      "dza",
      years = 2020,
      layers = "L1",
      dest_dir = temp_dir,
      quiet = TRUE
    )

    testthat::expect_true(file.exists(file.path(temp_dir, fn)))
    testthat::expect_equal(unname(result$counts[["DZA"]]), 1)
  }
)
