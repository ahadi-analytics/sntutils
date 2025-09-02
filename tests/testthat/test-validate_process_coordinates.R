testthat::test_that("validate_process_coordinates basic df input works", {
  df <- data.frame(
    hf = paste0("HF_", 1:4),
    lon = c(-12.5, -12.6, -12.7, -12.8),
    lat = c(8.0, 8.1, 8.2, 8.3)
  )

  adm0_sf <- sf::st_as_sfc(
    sf::st_bbox(c(xmin = -14, ymin = 6.9, xmax = -10, ymax = 10.1),
                crs = sf::st_crs(4326))
  ) |>
    sf::st_sf()

  res <- validate_process_coordinates(
    data = df,
    name = "basic",
    lon_col = "lon",
    lat_col = "lat",
    adm0_sf = adm0_sf,
    fix_issues = TRUE,
    quiet = TRUE
  )

  testthat::expect_true(res$passed)
  testthat::expect_true("spat_vec" %in% names(res))
  testthat::expect_true("points" %in% names(res$spat_vec))
  pts <- res$spat_vec$points
  testthat::expect_s3_class(pts, "sf")
  # geometry last, geometry_hash before
  nm <- names(pts)
  testthat::expect_equal(nm[length(nm)], "geometry")
  testthat::expect_true(
    which(nm == "geometry_hash") == length(nm) - 1
  )
  # lon/lat exist
  testthat::expect_true(all(c("lon", "lat") %in% nm))
})

testthat::test_that("input validation errors on missing lon/lat cols", {
  df <- data.frame(hf = 1:2, x = c(0, 1), y = c(0, 1))
  testthat::expect_error(
    validate_process_coordinates(
      data = df, lon_col = "lon", lat_col = "lat", quiet = TRUE
    ),
    "Longitude/Latitude columns not found"
  )
})

testthat::test_that("missing, non-numeric, and out-of-range handled", {
  df <- data.frame(
    hf = paste0("HF_", 1:5),
    lon = c(-12.5, "", -200, -12.8, "abc"),
    lat = c(8.0, 8.1, 0.1, NA, "9.0")
  )

  res <- validate_process_coordinates(
    data = df,
    lon_col = "lon",
    lat_col = "lat",
    fix_issues = TRUE,
    quiet = TRUE
  )

  # Rows with missing / non-numeric / out-of-range dropped
  pts <- res$spat_vec$points
  testthat::expect_true(nrow(pts) < nrow(df))
  testthat::expect_true(any(grepl("missing coordinates", res$issues)))
  testthat::expect_true(any(grepl("non-numeric coordinates", res$issues)))
  testthat::expect_true(any(grepl("out of range", res$issues)))
})

testthat::test_that("precision check flags low decimal places", {
  df <- data.frame(
    hf = paste0("HF_", 1:3),
    lon = c(-12.5, -12.55, -12.555),
    lat = c(8.0, 8.05, 8.055)
  )

  res <- validate_process_coordinates(
    data = df,
    lon_col = "lon",
    lat_col = "lat",
    min_decimals = 4,
    fix_issues = TRUE,
    quiet = TRUE
  )

  testthat::expect_true(any(grepl("imprecise coordinates", res$issues)))
})

testthat::test_that("flip detection and auto-fix using adm0 bbox", {
  # A point outside bbox that becomes inside when swapped
  df <- data.frame(
    hf = "HF_1",
    lon = 8.7,  # actually latitude
    lat = -12.1 # actually longitude
  )
  adm0_sf <- sf::st_as_sfc(
    sf::st_bbox(c(xmin = -14, ymin = 6.9, xmax = -10, ymax = 10.1),
                crs = sf::st_crs(4326))
  ) |>
    sf::st_sf()

  res <- validate_process_coordinates(
    data = df,
    lon_col = "lon",
    lat_col = "lat",
    adm0_sf = adm0_sf,
    fix_issues = TRUE,
    quiet = TRUE
  )

  pts <- res$spat_vec$points
  # After auto-fix, lon should be negative (~ -12) and lat ~ 8-10
  testthat::expect_lt(pts$lon, 0)
  testthat::expect_gt(pts$lat, 6.5)
})

testthat::test_that("duplicate rows and coordinates are removed", {
  df <- data.frame(
    hf = c("A", "A", "B", "C"),
    lon = c(-12, -12, -12.1, -12.2),
    lat = c(8, 8, 8.1, 8.2)
  )

  res <- validate_process_coordinates(
    data = df,
    lon_col = "lon",
    lat_col = "lat",
    fix_issues = TRUE,
    quiet = TRUE
  )
  pts <- res$spat_vec$points
  testthat::expect_equal(nrow(pts), 3)
  testthat::expect_true(any(grepl("duplicate", res$issues, ignore.case = TRUE)))
})

testthat::test_that("adm0 containment flags issues but not included in output", {
  df <- data.frame(
    hf = c("A", "B"),
    lon = c(-50, -12),
    lat = c(0, 8)
  )
  adm0_sf <- sf::st_as_sfc(
    sf::st_bbox(c(xmin = -14, ymin = 6.9, xmax = -10, ymax = 10.1),
                crs = sf::st_crs(4326))
  ) |>
    sf::st_sf()

  res <- validate_process_coordinates(
    data = df,
    lon_col = "lon",
    lat_col = "lat",
    adm0_sf = adm0_sf,
    fix_issues = TRUE,
    quiet = TRUE
  )

  pts <- res$spat_vec$points
  # 'outside_country' should not be present in output columns
  testthat::expect_false("outside_country" %in% names(pts))
  # Issue captured
  testthat::expect_true(any(grepl("outside country", res$issues)))
})

testthat::test_that("geometry_crs controls output CRS and lon/lat are 4326", {
  df <- data.frame(
    hf = paste0("HF_", 1:2),
    lon = c(-12.5, -12.6),
    lat = c(8.0, 8.1)
  )

  res <- validate_process_coordinates(
    data = df,
    lon_col = "lon",
    lat_col = "lat",
    geometry_crs = 3857,
    fix_issues = TRUE,
    quiet = TRUE
  )
  pts <- res$spat_vec$points
  testthat::expect_equal(sf::st_crs(pts)$epsg, 3857)
  # lon/lat numeric and plausible (in degrees)
  testthat::expect_true(all(pts$lon > -180 & pts$lon < 180))
  testthat::expect_true(all(pts$lat > -90 & pts$lat < 90))
})

testthat::test_that("sf input with non-point geometry is centroided", {
  poly <- sf::st_polygon(list(rbind(
    c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0)
  )))
  shp <- sf::st_sf(data.frame(id = 1), geometry = sf::st_sfc(poly, crs = 4326))

  res <- validate_process_coordinates(
    data = shp,
    fix_issues = TRUE,
    quiet = TRUE
  )

  pts <- res$spat_vec$points
  testthat::expect_true(all(sf::st_geometry_type(pts) == "POINT"))
})

testthat::test_that("validation-only mode does not produce output spat_vec", {
  df <- data.frame(hf = 1:2, lon = c(-12.5, -12.6), lat = c(8, 8.1))
  res <- validate_process_coordinates(
    data = df,
    lon_col = "lon",
    lat_col = "lat",
    fix_issues = FALSE,
    quiet = TRUE
  )
  testthat::expect_true(length(res$spat_vec) == 0)
  testthat::expect_null(res$column_dictionary)
})

testthat::test_that("empty result handled when all rows invalid", {
  df <- data.frame(hf = 1:2, lon = c(999, 888), lat = c(999, 888))
  res <- validate_process_coordinates(
    data = df, lon_col = "lon", lat_col = "lat", fix_issues = TRUE,
    quiet = TRUE
  )
  pts <- res$spat_vec$points
  testthat::expect_s3_class(pts, "sf")
  testthat::expect_equal(nrow(pts), 0)
})

testthat::test_that("DMS parsing via parzer (if installed)", {
  testthat::skip_if_not(
    requireNamespace("parzer", quietly = TRUE),
    message = "parzer not installed"
  )
  df <- data.frame(
    hf = "HF_DMS",
    lon = "13\u00B015'38\"W",
    lat = "8\u00B027'51\"N"
  )
  res <- validate_process_coordinates(
    data = df, lon_col = "lon", lat_col = "lat", fix_issues = TRUE,
    quiet = TRUE
  )
  pts <- res$spat_vec$points
  # Expect reasonable decimal degrees after parse
  testthat::expect_true(pts$lat > 0 & pts$lat < 90)
  testthat::expect_true(pts$lon < 0 & pts$lon > -180)
})

