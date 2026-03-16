testthat::test_that("facetted_map_bins returns a ggplot object", {
  skip_if_not_installed("sf")

  # create simple sf data with bins and facets
  pts <- sf::st_sfc(
    sf::st_point(c(0, 0)),
    sf::st_point(c(1, 0)),
    sf::st_point(c(0, 1)),
    sf::st_point(c(1, 1))
  )
  polys <- sf::st_buffer(pts, dist = 0.3)
  data <- sf::st_sf(
    id = 1:4,
    year = c(2020, 2020, 2021, 2021),
    bin = factor(c("low", "high", "low", "high"), levels = c("low", "high")),
    geometry = polys
  )

  colors <- c("low" = "#c6dbef", "high" = "#cb181d")


  p <- facetted_map_bins(
    data = data,
    fill_col = "bin",
    facet_col = "year",
    fill_colors = colors,
    title = "Test Title",
    subtitle = "Test Subtitle",
    fill_label = "Category"
  )

  testthat::expect_s3_class(p, "ggplot")
})

testthat::test_that("facetted_map_bins handles adm1_shp overlay", {
  skip_if_not_installed("sf")

  # main data
  pts <- sf::st_sfc(
    sf::st_point(c(0, 0)),
    sf::st_point(c(1, 1))
  )
  polys <- sf::st_buffer(pts, dist = 0.3)
  data <- sf::st_sf(
    year = c(2020, 2020),
    bin = factor(c("a", "b")),
    geometry = polys
  )

  # adm1 boundary overlay
  adm1 <- sf::st_sf(
    name = "Region",
    geometry = sf::st_sfc(sf::st_polygon(list(
      matrix(c(-1, -1, 2, -1, 2, 2, -1, 2, -1, -1), ncol = 2, byrow = TRUE)
    )))
  )

  colors <- c("a" = "#1f77b4", "b" = "#ff7f0e")

  p <- facetted_map_bins(
    data = data,
    fill_col = "bin",
    facet_col = "year",
    adm1_shp = adm1,
    fill_colors = colors,
    title = "With Overlay",
    subtitle = "",
    fill_label = "Bin"
  )

  testthat::expect_s3_class(p, "ggplot")
  # should have 2 geom_sf layers (data + adm1)
  testthat::expect_equal(sum(vapply(p$layers, function(l) {
    inherits(l$geom, "GeomSf")
  }, logical(1))), 2)
})

testthat::test_that("facetted_map_bins saves to file when output_file provided", {
  skip_if_not_installed("sf")

  tmp <- withr::local_tempdir()
  out_file <- fs::path(tmp, "test_map.png")

  pts <- sf::st_sfc(sf::st_point(c(0, 0)), sf::st_point(c(1, 1)))
  polys <- sf::st_buffer(pts, dist = 0.3)
  data <- sf::st_sf(
    year = c(2020, 2021),
    bin = factor(c("x", "x")),
    geometry = polys
  )

  colors <- c("x" = "#69b3a2")

  p <- facetted_map_bins(
    data = data,
    fill_col = "bin",
    facet_col = "year",
    fill_colors = colors,
    title = "Save Test",
    subtitle = "",
    fill_label = "Bin",
    output_file = out_file,
    width = 4,
    height = 4,
    dpi = 72
  )

  testthat::expect_true(fs::file_exists(out_file))
  testthat::expect_s3_class(p, "ggplot")
})

testthat::test_that("facetted_map_bins respects ncol parameter", {
  skip_if_not_installed("sf")

  pts <- sf::st_sfc(
    sf::st_point(c(0, 0)),
    sf::st_point(c(1, 0)),
    sf::st_point(c(2, 0)),
    sf::st_point(c(0, 1)),
    sf::st_point(c(1, 1)),
    sf::st_point(c(2, 1))
  )
  polys <- sf::st_buffer(pts, dist = 0.2)
  data <- sf::st_sf(
    year = rep(2018:2023, each = 1),
    bin = factor(rep("med", 6)),
    geometry = polys
  )

  colors <- c("med" = "#2ca02c")

  p <- facetted_map_bins(
    data = data,
    fill_col = "bin",
    facet_col = "year",
    fill_colors = colors,
    title = "Multi-year",
    subtitle = "",
    fill_label = "Level",
    ncol = 2
  )

  testthat::expect_s3_class(p, "ggplot")
  # facet_wrap with ncol = 2 should be present
  testthat::expect_true(inherits(p$facet, "FacetWrap"))
})
