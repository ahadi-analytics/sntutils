test_that(".clean_band_name extracts EVI", {
  expect_equal(
    sntutils:::.clean_band_name("1 km monthly EVI"),
    "evi"
  )
})

test_that(".clean_band_name extracts NDVI", {
  expect_equal(
    sntutils:::.clean_band_name("1 km monthly NDVI"),
    "ndvi"
  )
})

test_that(".clean_band_name handles LST subdataset name", {
  expect_equal(
    sntutils:::.clean_band_name("LST_Day_1km"),
    "lst_day"
  )
})

test_that(".clean_band_name falls back to 'band' when empty", {
  expect_equal(
    sntutils:::.clean_band_name("1 km monthly"),
    "band"
  )
})

test_that(".parse_modis_date extracts date from MODIS filename", {
  result <- sntutils:::.parse_modis_date(
    "MOD13A3.A2023001.h21v08.061.2023036090759.hdf"
  )
  expect_equal(result$year, 2023L)
  expect_equal(result$month, 1L)
  expect_equal(result$day, 1L)
  expect_equal(result$date, as.Date("2023-01-01"))
})

test_that(".parse_modis_date handles mid-year DOY", {
  result <- sntutils:::.parse_modis_date(
    "MOD13A3.A2023182.h21v08.061.hdf"
  )
  expect_equal(result$year, 2023L)
  expect_equal(result$month, 7L)
})

test_that(".parse_modis_date returns NA for bad filename", {
  result <- sntutils:::.parse_modis_date("random_file.hdf")
  expect_true(is.na(result$year))
})

test_that("download_modis errors on non-sf shapefile", {
  skip_if_not_installed("httr2")
  expect_error(
    download_modis(
      shapefile = data.frame(x = 1),
      start = "2023-01-01",
      end = "2023-12-31",
      username = "user",
      password = "pass"
    ),
    "shapefile.*must be.*sf"
  )
})

test_that("download_modis errors on missing credentials", {
  skip_if_not_installed("httr2")

  pt <- sf::st_sfc(sf::st_point(c(0, 0)), crs = 4326)
  shp <- sf::st_sf(geometry = pt)

  withr::with_envvar(
    c(EARTHDATA_USERNAME = "", EARTHDATA_PASSWORD = ""),
    expect_error(
      download_modis(
        shapefile = shp,
        start = "2023-01-01",
        end = "2023-12-31"
      ),
      "credentials.*missing"
    )
  )
})

test_that("download_modis errors on bad dates", {
  skip_if_not_installed("httr2")

  pt <- sf::st_sfc(sf::st_point(c(0, 0)), crs = 4326)
  shp <- sf::st_sf(geometry = pt)

  expect_error(
    download_modis(
      shapefile = shp,
      start = "not-a-date",
      end = "2023-12-31",
      username = "user",
      password = "pass"
    ),
    "Could not parse"
  )
})

test_that(".modis_scale_info returns correct EVI scale", {
  info <- sntutils:::.modis_scale_info("1 km monthly EVI")
  expect_equal(info$scale, 0.0001)
  expect_equal(info$nodata, -3000)
})

test_that(".modis_scale_info returns correct LST scale", {
  info <- sntutils:::.modis_scale_info("LST_Day_1km")
  expect_equal(info$scale, 0.02)
})

test_that(".modis_scale_info returns NULL for unknown band", {
  expect_null(
    sntutils:::.modis_scale_info("some_random_band")
  )
})

test_that("modis_options is a function", {
  expect_true(is.function(modis_options))
})
