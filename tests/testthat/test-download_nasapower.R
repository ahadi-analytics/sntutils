# test helpers ----

skip_if_no_internet <- function() {
  testthat::skip_if_not(curl::has_internet(), "No internet connection")
}

skip_if_nasapower_down <- function() {
  testthat::skip_if_not({
    tryCatch({
      # try a minimal POWER request
      nasapower::get_power(
        community = "ag",
        temporal_api = "daily",
        lonlat = c(0, 0),
        dates = c(Sys.Date() - 10, Sys.Date() - 9),
        pars = "T2M"
      )
      TRUE
    }, error = function(e) FALSE)
  }, "NASA POWER API is not accessible")
}

create_test_sf <- function() {
  # small test sf with 2 polygons
  sf::st_sf(
    adm0 = c("Country", "Country"),
    adm1 = c("Region1", "Region2"),
    adm2 = c("District1", "District2"),
    geometry = sf::st_sfc(
      sf::st_polygon(list(matrix(
        c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0),
        ncol = 2, byrow = TRUE
      ))),
      sf::st_polygon(list(matrix(
        c(1, 0, 2, 0, 2, 1, 1, 1, 1, 0),
        ncol = 2, byrow = TRUE
      ))),
      crs = 4326
    )
  )
}


# input validation tests ----

test_that("download_process_nasapower rejects non-sf input", {
  expect_error(
    download_process_nasapower(adm_sf = data.frame(x = 1)),
    "adm_sf must be an sf object"
  )
})

test_that("download_process_nasapower rejects missing admin columns", {
  test_sf <- create_test_sf()

  expect_error(
    download_process_nasapower(
      adm_sf = test_sf,
      admin_cols = c("adm0", "adm1", "adm3")
    ),
    "adm_sf is missing required columns"
  )
})

test_that("download_process_nasapower rejects sf without CRS", {
  test_sf <- create_test_sf()
  sf::st_crs(test_sf) <- NA

  expect_error(
    download_process_nasapower(adm_sf = test_sf),
    "adm_sf must have a defined CRS"
  )
})

test_that("download_process_nasapower rejects invalid dates", {
  test_sf <- create_test_sf()

  expect_error(
    download_process_nasapower(
      adm_sf = test_sf,
      start_date = "not-a-date"
    ),
    "start_date and end_date must be valid dates"
  )

  expect_error(
    download_process_nasapower(
      adm_sf = test_sf,
      end_date = "invalid"
    ),
    "start_date and end_date must be valid dates"
  )
})

test_that("download_process_nasapower rejects start_date after end_date", {
  test_sf <- create_test_sf()

  expect_error(
    download_process_nasapower(
      adm_sf = test_sf,
      start_date = "2025-12-31",
      end_date = "2025-01-01"
    ),
    "start_date must be before end_date"
  )
})


# integration tests ----

test_that("download_process_nasapower downloads and processes data", {
  skip_if_no_internet()
  skip_if_nasapower_down()

  test_sf <- create_test_sf()

  # use short date range to minimize download time
  result <- suppressMessages(
    download_process_nasapower(
      adm_sf = test_sf,
      admin_cols = c("adm0", "adm1", "adm2"),
      start_date = as.character(Sys.Date() - 10),
      end_date = as.character(Sys.Date() - 8),
      max_retries = 2
    )
  )

  # check structure

  expect_type(result, "list")
  expect_named(result, c("daily", "monthly", "dict_daily", "dict_monthly"))

  # check daily data
  expect_s3_class(result$daily, "data.frame")
  expect_true("date" %in% names(result$daily))
  expect_true("adm2" %in% names(result$daily))
  expect_true(nrow(result$daily) > 0)


  # check monthly data
  expect_s3_class(result$monthly, "data.frame")
  expect_true("location" %in% names(result$monthly))
  expect_true("total_rainfall_mm" %in% names(result$monthly))
  expect_true("mean_air_temperature_c" %in% names(result$monthly))
  expect_true("median_rainfall_mm" %in% names(result$monthly))

  # check dictionaries
  expect_s3_class(result$dict_daily, "data.frame")
  expect_s3_class(result$dict_monthly, "data.frame")
})

test_that("download_process_nasapower uses custom point_crs", {
  skip_if_no_internet()
  skip_if_nasapower_down()

  test_sf <- create_test_sf()

  # should work with different CRS
  result <- suppressMessages(
    download_process_nasapower(
      adm_sf = test_sf,
      start_date = as.character(Sys.Date() - 10),
      end_date = as.character(Sys.Date() - 9),
      point_crs = 3857,
      max_retries = 2
    )
  )

  expect_type(result, "list")
  expect_true(nrow(result$daily) > 0)
})
