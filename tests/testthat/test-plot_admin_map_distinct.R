make_admin_grid <- function(crs = 4326) {
  # 3 adm1 groups, each containing 2 adm2 polygons in a 2x3 grid
  polys <- base::list()
  adm1 <- base::character()
  adm2 <- base::character()

  idx <- 1L
  for (row in 0:1) {
    for (col in 0:2) {
      x0 <- col
      y0 <- row
      coords <- base::matrix(
        c(
          x0,     y0,
          x0 + 1, y0,
          x0 + 1, y0 + 1,
          x0,     y0 + 1,
          x0,     y0
        ),
        ncol = 2L,
        byrow = TRUE
      )
      polys[[idx]] <- sf::st_polygon(base::list(coords))
      adm1[idx] <- base::paste0("ADM1_", col)
      adm2[idx] <- base::paste0("ADM2_", row, "_", col)
      idx <- idx + 1L
    }
  }

  sf_obj <- sf::st_sf(
    adm1 = adm1,
    adm2 = adm2,
    geometry = sf::st_sfc(polys)
  )
  sf::st_set_crs(sf_obj, crs)
}

testthat::test_that("plot_admin_map_distinct() returns shp, labels, and ggplot", {
  shp <- make_admin_grid()

  result <- sntutils::plot_admin_map_distinct(
    shp = shp,
    group_col = "adm1",
    id_col = "adm2"
  )

  testthat::expect_named(result, c("shp", "labels", "gg"))
  testthat::expect_s3_class(result$shp, "sf")
  testthat::expect_s3_class(result$labels, "sf")
  testthat::expect_s3_class(result$gg, "ggplot")

  # one color per polygon, joined back to adm2
  testthat::expect_true("fill_col" %in% base::names(result$shp))
  testthat::expect_equal(base::nrow(result$shp), base::nrow(shp))

  # one label per group
  testthat::expect_equal(base::nrow(result$labels), 3L)
  testthat::expect_true(base::all(
    c("x", "y", "adm1") %in% base::names(result$labels)
  ))
})

testthat::test_that("plot_admin_map_distinct() assigns distinct colors to touching groups", {
  shp <- make_admin_grid()

  result <- sntutils::plot_admin_map_distinct(
    shp = shp,
    group_col = "adm1",
    id_col = "adm2"
  )

  # each adm1 should have a single color; touching adm1 should differ
  per_group <- result$shp |>
    sf::st_drop_geometry() |>
    dplyr::distinct(adm1, fill_col)

  testthat::expect_equal(base::nrow(per_group), 3L)
  testthat::expect_equal(
    base::length(base::unique(per_group$fill_col)),
    3L
  )
})

testthat::test_that("plot_admin_map_distinct() writes PNG when out_png is supplied", {
  shp <- make_admin_grid()
  tmp_png <- withr::local_tempfile(fileext = ".png")

  result <- sntutils::plot_admin_map_distinct(
    shp = shp,
    group_col = "adm1",
    id_col = "adm2",
    out_png = tmp_png
  )

  testthat::expect_true(base::file.exists(tmp_png))
  testthat::expect_gt(base::file.info(tmp_png)$size, 0L)
})

testthat::test_that("plot_admin_map_distinct() errors on non-sf input", {
  testthat::expect_error(
    sntutils::plot_admin_map_distinct(
      shp = base::data.frame(x = 1),
      group_col = "x",
      id_col = "x"
    ),
    regexp = "must be an sf object"
  )
})

testthat::test_that("plot_admin_map_distinct() errors on missing columns", {
  shp <- make_admin_grid()
  testthat::expect_error(
    sntutils::plot_admin_map_distinct(
      shp = shp,
      group_col = "missing_col",
      id_col = "adm2"
    ),
    regexp = "Missing columns"
  )
})

testthat::test_that("plot_admin_map_distinct() errors on empty palette", {
  shp <- make_admin_grid()
  testthat::expect_error(
    sntutils::plot_admin_map_distinct(
      shp = shp,
      group_col = "adm1",
      id_col = "adm2",
      palette = base::character()
    ),
    regexp = "palette"
  )
})

testthat::test_that("plot_admin_map_distinct() runs make_valid when requested", {
  shp <- make_admin_grid()
  testthat::expect_no_error(
    sntutils::plot_admin_map_distinct(
      shp = shp,
      group_col = "adm1",
      id_col = "adm2",
      fix_valid = TRUE
    )
  )
})
