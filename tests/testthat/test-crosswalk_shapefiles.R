# helper: create test admin sf objects with simple polygon geometries
create_test_admin_sf <- function(
  n = 1,
  xmin = 0,
  ymin = 0,
  width = 1,
  height = 1,
  adm0 = "Country",
  adm1 = "Region",
  adm2 = paste0("District_", seq_len(n)),
  crs = 4326,
  shift_x = 0,
  shift_y = 0
) {
  polys <- lapply(seq_len(n), function(i) {
    x0 <- xmin + (i - 1) * width + shift_x
    y0 <- ymin + shift_y
    coords <- matrix(c(
      x0, y0,
      x0 + width, y0,
      x0 + width, y0 + height,
      x0, y0 + height,
      x0, y0
    ), ncol = 2, byrow = TRUE)
    sf::st_polygon(list(coords))
  })

  sf_obj <- sf::st_sf(
    adm0 = rep(adm0, n),
    adm1 = rep(adm1, n),
    adm2 = adm2,
    geometry = sf::st_sfc(polys)
  )

  if (!is.null(crs)) {
    sf_obj <- sf::st_set_crs(sf_obj, crs)
  }

  sf_obj
}

# input validation tests -------------------------------------------------------

testthat::test_that("crosswalk_shapefiles_sf errors when old_sf is not sf", {
  new_sf <- create_test_admin_sf(n = 1)
  testthat::expect_error(
    crosswalk_shapefiles_sf(
      old_sf = data.frame(adm0 = "A", adm1 = "B", adm2 = "C"),
      new_sf = new_sf,
      verbose = FALSE
    ),
    "must be an sf object"
  )
})

testthat::test_that("crosswalk_shapefiles_sf errors when new_sf is not sf", {
  old_sf <- create_test_admin_sf(n = 1)
  testthat::expect_error(
    crosswalk_shapefiles_sf(
      old_sf = old_sf,
      new_sf = data.frame(adm0 = "A", adm1 = "B", adm2 = "C"),
      verbose = FALSE
    ),
    "must be an sf object"
  )
})

testthat::test_that("crosswalk_shapefiles_sf errors when required columns missing", {
  old_sf <- create_test_admin_sf(n = 1)
  new_sf <- create_test_admin_sf(n = 1)

  # remove adm2 column
  old_sf_bad <- old_sf
  old_sf_bad$adm2 <- NULL

  testthat::expect_error(
    crosswalk_shapefiles_sf(
      old_sf = old_sf_bad,
      new_sf = new_sf,
      verbose = FALSE
    ),
    "missing required columns"
  )
})

testthat::test_that("crosswalk_shapefiles_sf errors when CRS is not 4326", {
  old_sf <- create_test_admin_sf(n = 1)
  new_sf <- create_test_admin_sf(n = 1, crs = 32735)

  testthat::expect_error(
    crosswalk_shapefiles_sf(
      old_sf = old_sf,
      new_sf = new_sf,
      verbose = FALSE
    ),
    "epsg:4326"
  )
})

testthat::test_that("crosswalk_shapefiles_sf errors when min_weight out of range", {
  old_sf <- create_test_admin_sf(n = 1)
  new_sf <- create_test_admin_sf(n = 1)

  testthat::expect_error(
    crosswalk_shapefiles_sf(
      old_sf = old_sf,
      new_sf = new_sf,
      min_weight = -0.1,
      verbose = FALSE
    ),
    "min_weight"
  )

  testthat::expect_error(
    crosswalk_shapefiles_sf(
      old_sf = old_sf,
      new_sf = new_sf,
      min_weight = 1.0,
      verbose = FALSE
    ),
    "min_weight"
  )
})

testthat::test_that("crosswalk_shapefiles_sf errors when min_primary_weight invalid", {
  old_sf <- create_test_admin_sf(n = 1)
  new_sf <- create_test_admin_sf(n = 1)

  testthat::expect_error(
    crosswalk_shapefiles_sf(
      old_sf = old_sf,
      new_sf = new_sf,
      min_primary_weight = 0,
      verbose = FALSE
    ),
    "min_primary_weight"
  )

  testthat::expect_error(
    crosswalk_shapefiles_sf(
      old_sf = old_sf,
      new_sf = new_sf,
      min_primary_weight = 1.5,
      verbose = FALSE
    ),
    "min_primary_weight"
  )
})

# core functionality tests -----------------------------------------------------

testthat::test_that("crosswalk_shapefiles_sf works with valid inputs", {
  old_sf <- create_test_admin_sf(n = 1)
  new_sf <- create_test_admin_sf(n = 1)

  result <- crosswalk_shapefiles_sf(
    old_sf = old_sf,
    new_sf = new_sf,
    verbose = FALSE
  )

  testthat::expect_type(result, "list")
  testthat::expect_true("data" %in% names(result))
  testthat::expect_true("dictionary" %in% names(result))
  testthat::expect_true("metadata" %in% names(result))
})

testthat::test_that("crosswalk_shapefiles_sf returns correct structure", {
  old_sf <- create_test_admin_sf(n = 1)
  new_sf <- create_test_admin_sf(n = 1)

  result <- crosswalk_shapefiles_sf(
    old_sf = old_sf,
    new_sf = new_sf,
    level = "adm2",
    verbose = FALSE
  )

  # check data columns for adm2 level
  expected_cols <- c(
    "adm0_old", "adm1_old", "adm2_old",
    "adm0", "adm1", "adm2",
    "overlap_prop", "is_primary", "n_new_adm",
    "total_overlap_prop", "split_flag", "n_old_per_new", "merge_flag"
  )
  testthat::expect_true(all(expected_cols %in% names(result$data)))

  # check dictionary columns
  testthat::expect_true(all(c(
    "column_name", "english_description", "description_fr"
  ) %in% names(result$dictionary)))

  # check metadata columns
  testthat::expect_true(all(c(
    "created_at", "level", "area_crs", "min_weight"
  ) %in% names(result$metadata)))
})

testthat::test_that("crosswalk_shapefiles_sf calculates overlap correctly", {
  # create old sf with one unit

old_sf <- create_test_admin_sf(n = 1, xmin = 0, ymin = 0, width = 2, height = 1)

  # create new sf with identical unit (100% overlap)
  new_sf <- create_test_admin_sf(n = 1, xmin = 0, ymin = 0, width = 2, height = 1)

  result <- crosswalk_shapefiles_sf(
    old_sf = old_sf,
    new_sf = new_sf,
    verbose = FALSE
  )

  # expect ~100% overlap
  testthat::expect_equal(result$data$overlap_prop[1], 1.0)
  testthat::expect_true(result$data$is_primary[1])
})

testthat::test_that("crosswalk_shapefiles_sf detects splits correctly", {
  # one old unit spanning 2 degrees
  old_sf <- create_test_admin_sf(
    n = 1, xmin = 0, ymin = 0, width = 2, height = 1,
    adm2 = "Old_District"
  )

  # two new units each spanning 1 degree
  new_sf <- create_test_admin_sf(
    n = 2, xmin = 0, ymin = 0, width = 1, height = 1,
    adm2 = c("New_District_1", "New_District_2")
  )

  result <- crosswalk_shapefiles_sf(
    old_sf = old_sf,
    new_sf = new_sf,
    min_secondary_weight = 0.4,
    verbose = FALSE
  )

  # should have 2 mappings (split)
  testthat::expect_equal(nrow(result$data), 2)
  testthat::expect_true(all(result$data$split_flag))

  # each overlap should be ~50%
  testthat::expect_true(all(result$data$overlap_prop >= 0.49))
  testthat::expect_true(all(result$data$overlap_prop <= 0.51))
})

testthat::test_that("crosswalk_shapefiles_sf detects merges correctly", {
  # two old units each spanning 1 degree
  old_sf <- create_test_admin_sf(
    n = 2, xmin = 0, ymin = 0, width = 1, height = 1,
    adm2 = c("Old_District_1", "Old_District_2")
  )

  # one new unit spanning 2 degrees
  new_sf <- create_test_admin_sf(
    n = 1, xmin = 0, ymin = 0, width = 2, height = 1,
    adm2 = "New_District"
  )

  result <- crosswalk_shapefiles_sf(
    old_sf = old_sf,
    new_sf = new_sf,
    verbose = FALSE
  )

  # should have 2 mappings (2 old -> 1 new)
  testthat::expect_equal(nrow(result$data), 2)
  testthat::expect_true(all(result$data$merge_flag))
  testthat::expect_equal(result$data$n_old_per_new[1], 2)
})

# level parameter tests --------------------------------------------------------

testthat::test_that("crosswalk_shapefiles_sf works with level adm1", {
  old_sf <- create_test_admin_sf(n = 1)
  new_sf <- create_test_admin_sf(n = 1)

  result <- crosswalk_shapefiles_sf(
    old_sf = old_sf,
    new_sf = new_sf,
    level = "adm1",
    verbose = FALSE
  )

  testthat::expect_equal(result$metadata$level, "adm1")

  # should have adm0_old, adm1_old but not adm2_old
  testthat::expect_true("adm0_old" %in% names(result$data))
  testthat::expect_true("adm1_old" %in% names(result$data))
  testthat::expect_false("adm2_old" %in% names(result$data))
})

testthat::test_that("crosswalk_shapefiles_sf works with level adm0", {
  old_sf <- create_test_admin_sf(n = 1)
  new_sf <- create_test_admin_sf(n = 1)

  result <- crosswalk_shapefiles_sf(
    old_sf = old_sf,
    new_sf = new_sf,
    level = "adm0",
    verbose = FALSE
  )

  testthat::expect_equal(result$metadata$level, "adm0")

  # should have only adm0_old
  testthat::expect_true("adm0_old" %in% names(result$data))
  testthat::expect_false("adm1_old" %in% names(result$data))
  testthat::expect_false("adm2_old" %in% names(result$data))
})

# optional parameter tests -----------------------------------------------------

testthat::test_that("keep_overlap_geometry returns geometry", {
  old_sf <- create_test_admin_sf(n = 1)
  new_sf <- create_test_admin_sf(n = 1)

  result <- crosswalk_shapefiles_sf(
    old_sf = old_sf,
    new_sf = new_sf,
    keep_overlap_geometry = TRUE,
    verbose = FALSE
  )

  testthat::expect_true("overlap_geometry" %in% names(result))
  testthat::expect_s3_class(result$overlap_geometry, "sf")
})

testthat::test_that("include_areas adds area columns", {
  old_sf <- create_test_admin_sf(n = 1)
  new_sf <- create_test_admin_sf(n = 1)

  result <- crosswalk_shapefiles_sf(
    old_sf = old_sf,
    new_sf = new_sf,
    include_areas = TRUE,
    verbose = FALSE
  )

  testthat::expect_true("area_old_km2" %in% names(result$data))
  testthat::expect_true("area_new_km2" %in% names(result$data))
  testthat::expect_true("overlap_area_km2" %in% names(result$data))
})

testthat::test_that("include_reverse_prop adds reverse proportion", {
  old_sf <- create_test_admin_sf(n = 1)
  new_sf <- create_test_admin_sf(n = 1)

  result <- crosswalk_shapefiles_sf(
    old_sf = old_sf,
    new_sf = new_sf,
    include_reverse_prop = TRUE,
    verbose = FALSE
  )

  testthat::expect_true("overlap_prop_reverse" %in% names(result$data))
})

# output structure tests -------------------------------------------------------

testthat::test_that("dictionary has correct structure", {
  old_sf <- create_test_admin_sf(n = 1)
  new_sf <- create_test_admin_sf(n = 1)

  result <- crosswalk_shapefiles_sf(
    old_sf = old_sf,
    new_sf = new_sf,
    verbose = FALSE
  )

  dict <- result$dictionary

  testthat::expect_s3_class(dict, "data.frame")
  testthat::expect_true(nrow(dict) > 0)

  # all data columns should be documented
  data_cols <- names(result$data)
  documented_cols <- dict$column_name

  testthat::expect_true(all(data_cols %in% documented_cols))
})

testthat::test_that("metadata contains expected fields", {
  old_sf <- create_test_admin_sf(n = 1)
  new_sf <- create_test_admin_sf(n = 1)

  result <- crosswalk_shapefiles_sf(
    old_sf = old_sf,
    new_sf = new_sf,
    level = "adm2",
    min_weight = 0.01,
    area_crs = 32735,
    verbose = FALSE
  )

  meta <- result$metadata

  testthat::expect_equal(meta$level, "adm2")
  testthat::expect_equal(meta$area_crs, 32735)
  testthat::expect_equal(meta$min_weight, 0.01)
  testthat::expect_true("created_at" %in% names(meta))
  testthat::expect_true("sf_version" %in% names(meta))
  testthat::expect_true("n_mappings" %in% names(meta))
})
