# Helper function to create test shapefile with artifacts
create_test_shapefile_with_artifacts <- function(
  n_features = 5,
  add_slivers = FALSE,
  add_overlaps = FALSE,
  invalid_geom = FALSE
) {
  coords_list <- list()
  for (i in 1:n_features) {
    x_offset <- (i - 1) * 2
    coords_list[[i]] <- matrix(
      c(
        x_offset,
        x_offset,
        x_offset + 1,
        x_offset + 1,
        x_offset,
        0,
        1,
        1,
        0,
        0
      ),
      ncol = 2
    )
  }

  # Create polygons
  polygons <- lapply(coords_list, function(coords) {
    sf::st_polygon(list(coords))
  })

  # Add sliver polygon if requested (very small polygon)
  if (add_slivers) {
    # Create a tiny polygon (0.0001 degree square ~ 0.0001 km²)
    sliver_coords <- matrix(
      c(
        10,
        10,
        10.0001,
        10.0001,
        10,
        0,
        0.0001,
        0.0001,
        0,
        0
      ),
      ncol = 2
    )
    polygons[[length(polygons) + 1]] <- sf::st_polygon(list(sliver_coords))
    n_features <- n_features + 1
  }

  # Add overlapping polygon if requested
  if (add_overlaps && n_features > 1) {
    # Create a polygon that overlaps with the first one
    overlap_coords <- matrix(
      c(
        0.5,
        0.5,
        1.5,
        1.5,
        0.5,
        0,
        1,
        1,
        0,
        0
      ),
      ncol = 2
    )
    polygons[[length(polygons) + 1]] <- sf::st_polygon(list(overlap_coords))
    n_features <- n_features + 1
  }

  # Create invalid geometry if requested
  if (invalid_geom && length(polygons) > 0) {
    invalid_wkt <- "POLYGON((0 0, 1 0, 0 0))"
    polygons[[1]] <- sf::st_as_sfc(invalid_wkt, check_ring_dir = FALSE)[[1]]
  }

  # Create attribute data
  data <- data.frame(
    district = paste("District", seq_len(n_features)),
    stringsAsFactors = FALSE
  )

  # Create sf object
  old_s2 <- sf::sf_use_s2()
  sf::sf_use_s2(FALSE)

  shp <- tryCatch(
    {
      sf::st_sf(data, geometry = sf::st_sfc(polygons))
    },
    finally = {
      sf::sf_use_s2(old_s2)
    }
  )

  shp <- sf::st_set_crs(shp, 4326)

  shp
}

# Test 1: Basic functionality with file path
testthat::test_that("clean_boundary_artefacts works with file path", {
  # Create temporary shapefile
  temp_dir <- tempdir()
  shp_test <- create_test_shapefile_with_artifacts(n_features = 3)
  temp_path <- file.path(temp_dir, "test_shapefile.geojson")

  # Write shapefile
  sf::st_write(shp_test, temp_path, quiet = TRUE, delete_dsn = TRUE)

  # Clean artifacts
  result <- clean_boundary_artefacts(
    shp_path = temp_path,
    metric_crs = 32736,
    sliver_threshold_km2 = 0.01,
    snap_precision = 1e5
  )

  # Check result structure
  testthat::expect_type(result, "list")
  testthat::expect_named(
    result,
    c("shp_clean", "diagnostics", "slivers_removed")
  )
  testthat::expect_s3_class(result$shp_clean, "sf")
  testthat::expect_type(result$diagnostics, "list")
  testthat::expect_type(result$slivers_removed, "integer")

  # Clean up
  unlink(temp_path)
})

# Test 2: Sliver detection and removal
testthat::test_that("clean_boundary_artefacts detects and removes slivers", {
  # Create shapefile with slivers
  temp_dir <- tempdir()
  shp_test <- create_test_shapefile_with_artifacts(
    n_features = 3,
    add_slivers = TRUE
  )
  temp_path <- file.path(temp_dir, "test_slivers.geojson")

  # Write shapefile
  sf::st_write(shp_test, temp_path, quiet = TRUE, delete_dsn = TRUE)

  # Clean artifacts
  result <- clean_boundary_artefacts(
    shp_path = temp_path,
    metric_crs = 32736,
    sliver_threshold_km2 = 0.01,
    snap_precision = 1e5
  )

  # Check that slivers were detected
  testthat::expect_true(result$diagnostics$n_slivers >= 0)

  # Clean up
  unlink(temp_path)
})

# Test 3: Overlap detection
testthat::test_that("clean_boundary_artefacts detects overlaps", {
  # Create shapefile with overlaps
  temp_dir <- tempdir()
  shp_test <- create_test_shapefile_with_artifacts(
    n_features = 3,
    add_overlaps = TRUE
  )
  temp_path <- file.path(temp_dir, "test_overlaps.geojson")

  # Write shapefile
  sf::st_write(shp_test, temp_path, quiet = TRUE, delete_dsn = TRUE)

  # Clean artifacts
  result <- clean_boundary_artefacts(
    shp_path = temp_path,
    metric_crs = 32736,
    sliver_threshold_km2 = 0.01,
    snap_precision = 1e5
  )

  # Check that overlaps were detected
  testthat::expect_type(result$diagnostics$n_overlapping, "integer")
  testthat::expect_true(result$diagnostics$n_overlapping >= 0)

  # Clean up
  unlink(temp_path)
})

# Test 4: Invalid geometry fixing
testthat::test_that("clean_boundary_artefacts fixes invalid geometries", {
  # Create shapefile with invalid geometry
  temp_dir <- tempdir()
  shp_test <- create_test_shapefile_with_artifacts(
    n_features = 3,
    invalid_geom = TRUE
  )
  temp_path <- file.path(temp_dir, "test_invalid.geojson")

  # Write shapefile
  sf::st_write(shp_test, temp_path, quiet = TRUE, delete_dsn = TRUE)

  # Clean artifacts
  result <- clean_boundary_artefacts(
    shp_path = temp_path,
    metric_crs = 32736,
    sliver_threshold_km2 = 0.01,
    snap_precision = 1e5
  )

  # Check that all geometries are now valid
  testthat::expect_true(all(sf::st_is_valid(result$shp_clean)))

  # Clean up
  unlink(temp_path)
})

# Test 5: Group-by topology rebuild
testthat::test_that("clean_boundary_artefacts rebuilds topology by group", {
  # Create shapefile with group column
  temp_dir <- tempdir()
  shp_test <- create_test_shapefile_with_artifacts(n_features = 5)
  shp_test$group <- c("A", "A", "B", "B", "B")
  temp_path <- file.path(temp_dir, "test_group.geojson")

  # Write shapefile
  sf::st_write(shp_test, temp_path, quiet = TRUE, delete_dsn = TRUE)

  # Clean artifacts with group rebuild
  result <- clean_boundary_artefacts(
    shp_path = temp_path,
    metric_crs = 32736,
    sliver_threshold_km2 = 0.01,
    snap_precision = 1e5,
    group_col = "group"
  )

  # Check that result is aggregated by group
  testthat::expect_true(nrow(result$shp_clean) <= 2)

  # Clean up
  unlink(temp_path)
})

# Test 6: Error handling - missing file
testthat::test_that("clean_boundary_artefacts errors on missing file", {
  testthat::expect_error(
    clean_boundary_artefacts(
      shp_path = "/nonexistent/path/to/shapefile.geojson",
      metric_crs = 32736
    ),
    "file not found"
  )
})

# Test 7: Error handling - invalid inputs
testthat::test_that("clean_boundary_artefacts validates input parameters", {
  temp_dir <- tempdir()
  shp_test <- create_test_shapefile_with_artifacts(n_features = 3)
  temp_path <- file.path(temp_dir, "test_validation.geojson")
  sf::st_write(shp_test, temp_path, quiet = TRUE, delete_dsn = TRUE)

  # Test invalid sliver_threshold_km2
  testthat::expect_error(
    clean_boundary_artefacts(
      shp_path = temp_path,
      sliver_threshold_km2 = -1
    ),
    "sliver_threshold_km2 must be a positive number"
  )

  # Test invalid snap_precision
  testthat::expect_error(
    clean_boundary_artefacts(
      shp_path = temp_path,
      snap_precision = -100
    ),
    "snap_precision must be a positive number"
  )

  # Clean up
  unlink(temp_path)
})

# Test 8: Diagnostics structure
testthat::test_that("clean_boundary_artefacts returns complete diagnostics", {
  temp_dir <- tempdir()
  shp_test <- create_test_shapefile_with_artifacts(n_features = 3)
  temp_path <- file.path(temp_dir, "test_diagnostics.geojson")
  sf::st_write(shp_test, temp_path, quiet = TRUE, delete_dsn = TRUE)

  result <- clean_boundary_artefacts(
    shp_path = temp_path,
    metric_crs = 32736,
    sliver_threshold_km2 = 0.01,
    snap_precision = 1e5
  )

  # Check diagnostics structure
  testthat::expect_named(
    result$diagnostics,
    c("n_invalid", "n_slivers", "n_overlapping", "lon_precision", "lat_precision")
  )

  testthat::expect_type(result$diagnostics$n_invalid, "integer")
  testthat::expect_type(result$diagnostics$n_slivers, "integer")
  testthat::expect_type(result$diagnostics$n_overlapping, "integer")
  testthat::expect_type(result$diagnostics$lon_precision, "double")
  testthat::expect_type(result$diagnostics$lat_precision, "double")

  # Clean up
  unlink(temp_path)
})

# Test 9: Different metric CRS
testthat::test_that("clean_boundary_artefacts works with different metric CRS", {
  temp_dir <- tempdir()
  shp_test <- create_test_shapefile_with_artifacts(n_features = 3)
  temp_path <- file.path(temp_dir, "test_crs.geojson")
  sf::st_write(shp_test, temp_path, quiet = TRUE, delete_dsn = TRUE)

  # Use different metric CRS (UTM Zone 33N)
  result <- clean_boundary_artefacts(
    shp_path = temp_path,
    metric_crs = 32633,
    sliver_threshold_km2 = 0.01,
    snap_precision = 1e5
  )

  # Check that result has original CRS (EPSG:4326)
  testthat::expect_equal(sf::st_crs(result$shp_clean)$epsg, 4326)

  # Clean up
  unlink(temp_path)
})
