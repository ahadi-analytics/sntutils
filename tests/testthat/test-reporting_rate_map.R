# tests for reporting_rate_map function

test_that("reporting_rate_map validates inputs correctly", {
  # Mock data
  test_data <- data.frame(
    adm1 = rep(c("District A", "District B"), each = 6),
    month = rep(1:3, 4),
    facility_id = rep(1:4, each = 3),
    malaria = c(10, 15, NA, 20, 25, 30, NA, NA, 35, 40, 45, 50),
    test = c(5, 10, 15, 20, NA, 30, 35, 40, 45, NA, 55, 60)
  )
  
  # Create a simple sf object for testing
  coords_a <- matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0), ncol = 2, byrow = TRUE)
  coords_b <- matrix(c(1, 0, 2, 0, 2, 1, 1, 1, 1, 0), ncol = 2, byrow = TRUE)
  
  poly_a <- sf::st_polygon(list(coords_a))
  poly_b <- sf::st_polygon(list(coords_b))
  
  test_shapefile <- sf::st_sf(
    adm1 = c("District A", "District B"),
    geometry = sf::st_sfc(poly_a, poly_b)
  )
  
  # Test that non-sf object throws error
  expect_error(
    reporting_rate_map(
      data = test_data,
      shapefile = data.frame(adm1 = c("A", "B")),  # Not an sf object
      x_var = "month",
      adm_var = "adm1",
      vars_of_interest = "malaria"
    ),
    "'shapefile' must be an sf object"
  )
  
  # Test that missing adm_var in shapefile throws error
  expect_error(
    reporting_rate_map(
      data = test_data,
      shapefile = test_shapefile,
      x_var = "month",
      adm_var = "adm2",  # Not in shapefile
      vars_of_interest = "malaria"
    ),
    "'adm2' not found in shapefile"
  )
  
  # Test that missing adm_var in data throws error
  test_data_no_adm <- test_data
  names(test_data_no_adm)[1] <- "district"
  expect_error(
    reporting_rate_map(
      data = test_data_no_adm,
      shapefile = test_shapefile,
      x_var = "month",
      adm_var = "adm1",  # Not in data anymore
      vars_of_interest = "malaria"
    ),
    "'adm1' not found in data"
  )
})

test_that("reporting_rate_map creates plot with correct structure", {
  skip_if_not_installed("wesanderson")
  
  # Create test data
  test_data <- data.frame(
    adm1 = rep(c("District A", "District B"), each = 6),
    month = factor(rep(c("Jan", "Feb", "Mar"), 4)),
    facility_id = rep(1:4, each = 3),
    malaria = c(10, 15, NA, 20, 25, 30, NA, NA, 35, 40, 45, 50)
  )
  
  # Create test shapefile
  coords_a <- matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0), ncol = 2, byrow = TRUE)
  coords_b <- matrix(c(1, 0, 2, 0, 2, 1, 1, 1, 1, 0), ncol = 2, byrow = TRUE)
  
  poly_a <- sf::st_polygon(list(coords_a))
  poly_b <- sf::st_polygon(list(coords_b))
  
  test_shapefile <- sf::st_sf(
    adm1 = c("District A", "District B"),
    geometry = sf::st_sfc(poly_a, poly_b)
  )
  
  # Mock calculate_reporting_metrics
  mock_metrics <- data.frame(
    adm1 = rep(c("District A", "District B"), each = 3),
    month = rep(c("Jan", "Feb", "Mar"), 2),
    reprate = c(0.8, 0.9, 0.7, 0.6, 0.5, 0.8),
    missrate = c(0.2, 0.1, 0.3, 0.4, 0.5, 0.2)
  )
  
  mockery::stub(
    reporting_rate_map,
    "calculate_reporting_metrics",
    mock_metrics
  )
  
  # Test basic plot creation
  p <- reporting_rate_map(
    data = test_data,
    shapefile = test_shapefile,
    x_var = "month",
    adm_var = "adm1",
    vars_of_interest = "malaria",
    show_plot = FALSE
  )
  
  expect_s3_class(p, "ggplot")
  
  # Check that plot has facets
  expect_s3_class(p$facet, "FacetWrap")
  
  # Check that plot has correct layers
  expect_true(any(sapply(p$layers, function(l) inherits(l$geom, "GeomSf"))))
  
  # Check that plot has title
  expect_true("title" %in% names(p$labels))
  
  # Check that title contains the variables of interest
  expect_true(grepl("malaria", p$labels$title, ignore.case = TRUE))
})

test_that("reporting_rate_map handles full_range parameter correctly", {
  skip_if_not_installed("wesanderson")
  
  # Create test data with limited range values
  test_data <- data.frame(
    adm1 = rep("District A", 3),
    month = factor(c("Jan", "Feb", "Mar")),
    facility_id = rep(1, 3),
    malaria = c(10, 15, 12)  # All values present
  )
  
  # Create test shapefile
  coords <- matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0), ncol = 2, byrow = TRUE)
  poly <- sf::st_polygon(list(coords))
  test_shapefile <- sf::st_sf(
    adm1 = "District A",
    geometry = sf::st_sfc(poly)
  )
  
  # Mock metrics with limited range (60-80%)
  mock_metrics <- data.frame(
    adm1 = rep("District A", 3),
    month = c("Jan", "Feb", "Mar"),
    reprate = c(0.6, 0.7, 0.8),
    missrate = c(0.4, 0.3, 0.2)
  )
  
  mockery::stub(
    reporting_rate_map,
    "calculate_reporting_metrics",
    mock_metrics
  )
  
  # Test with full_range = TRUE (default)
  p_full <- reporting_rate_map(
    data = test_data,
    shapefile = test_shapefile,
    x_var = "month",
    adm_var = "adm1",
    vars_of_interest = "malaria",
    full_range = TRUE,
    show_plot = FALSE
  )
  
  # Test with full_range = FALSE
  p_limited <- reporting_rate_map(
    data = test_data,
    shapefile = test_shapefile,
    x_var = "month",
    adm_var = "adm1",
    vars_of_interest = "malaria",
    full_range = FALSE,
    show_plot = FALSE
  )
  
  # Get the scale limits
  scale_full <- p_full$scales$scales[[1]]$limits
  scale_limited <- p_limited$scales$scales[[1]]$limits
  
  # Check full range plot has 0-100 limits
  expect_equal(scale_full, c(0, 100))
  
  # Check limited range plot has narrower limits
  expect_true(scale_limited[1] >= 50)  # Should be around 60
  expect_true(scale_limited[2] <= 90)  # Should be around 80
})

test_that("reporting_rate_map handles facet_label parameter correctly", {
  skip_if_not_installed("wesanderson")
  
  # Create minimal test data
  test_data <- data.frame(
    adm1 = "District A",
    month = "Jan",
    malaria = 10
  )
  
  # Create minimal shapefile
  coords <- matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0), ncol = 2, byrow = TRUE)
  poly <- sf::st_polygon(list(coords))
  test_shapefile <- sf::st_sf(
    adm1 = "District A",
    geometry = sf::st_sfc(poly)
  )
  
  # Mock calculate_reporting_metrics
  mock_metrics <- data.frame(
    adm1 = "District A",
    month = "Jan",
    reprate = 0.8,
    missrate = 0.2
  )
  
  mockery::stub(
    reporting_rate_map,
    "calculate_reporting_metrics",
    mock_metrics
  )
  
  # Test with custom facet_label
  p <- reporting_rate_map(
    data = test_data,
    shapefile = test_shapefile,
    x_var = "month",
    adm_var = "adm1",
    vars_of_interest = "malaria",
    facet_label = "Province",
    show_plot = FALSE
  )
  
  # Check that title contains the custom facet label (case-insensitive due to sentence case)
  expect_true(grepl("province", p$labels$title, ignore.case = TRUE))
  expect_false(grepl("adm1", p$labels$title, ignore.case = TRUE))
})

test_that("reporting_rate_map handles translation correctly", {
  skip_if_not_installed("wesanderson")
  
  # Create minimal test data
  test_data <- data.frame(
    adm1 = "District A",
    month = "Jan",
    malaria = 10
  )
  
  # Create minimal shapefile
  coords <- matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0), ncol = 2, byrow = TRUE)
  poly <- sf::st_polygon(list(coords))
  test_shapefile <- sf::st_sf(
    adm1 = "District A",
    geometry = sf::st_sfc(poly)
  )
  
  # Mock functions
  mock_metrics <- data.frame(
    adm1 = "District A",
    month = "Jan",
    reprate = 0.8,
    missrate = 0.2
  )
  
  mockery::stub(
    reporting_rate_map,
    "calculate_reporting_metrics",
    mock_metrics
  )
  
  # Mock translate_text to return predictable translations
  mockery::stub(
    reporting_rate_map,
    "translate_text",
    function(text, ...) paste0("TR_", text)
  )
  
  mockery::stub(
    reporting_rate_map,
    "ensure_packages",
    function(...) invisible(NULL)
  )
  
  # Test with translation
  p <- reporting_rate_map(
    data = test_data,
    shapefile = test_shapefile,
    x_var = "month",
    adm_var = "adm1",
    vars_of_interest = "malaria",
    target_language = "fr",
    show_plot = FALSE
  )
  
  # Check that title was translated (but note sentence case is applied after translation)
  # The title should contain translated elements but may be modified by sentence case
  expect_true(grepl("tr_", p$labels$title, ignore.case = TRUE))
  # Fill label should contain TR_ prefix in scale name
  fill_scale <- p$scales$get_scales("fill")
  expect_true(grepl("tr_", fill_scale$name, ignore.case = TRUE))
})

test_that("reporting_rate_map saves plot correctly", {
  skip_if_not_installed("wesanderson")
  
  # Create test data
  test_data <- data.frame(
    adm1 = "District A",
    month = "Jan",
    malaria = 10
  )
  
  # Create test shapefile
  coords <- matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0), ncol = 2, byrow = TRUE)
  poly <- sf::st_polygon(list(coords))
  test_shapefile <- sf::st_sf(
    adm1 = "District A",
    geometry = sf::st_sfc(poly)
  )
  
  # Mock functions
  mock_metrics <- data.frame(
    adm1 = "District A",
    month = "Jan",
    reprate = 0.8,
    missrate = 0.2
  )
  
  mockery::stub(
    reporting_rate_map,
    "calculate_reporting_metrics",
    mock_metrics
  )
  
  # Test saving plot
  tmp_file <- tempfile(fileext = ".png")
  
  p <- reporting_rate_map(
    data = test_data,
    shapefile = test_shapefile,
    x_var = "month",
    adm_var = "adm1",
    vars_of_interest = "malaria",
    plot_path = tmp_file,
    show_plot = FALSE
  )
  
  # Check that file was created
  expect_true(file.exists(tmp_file))
  
  # Clean up
  unlink(tmp_file)
})

test_that("reporting_rate_map handles weighted rates correctly", {
  skip_if_not_installed("wesanderson")
  
  # Create test data
  test_data <- data.frame(
    adm1 = "District A",
    month = "Jan",
    malaria = 10,
    allout = 100
  )
  
  # Create test shapefile
  coords <- matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0), ncol = 2, byrow = TRUE)
  poly <- sf::st_polygon(list(coords))
  test_shapefile <- sf::st_sf(
    adm1 = "District A",
    geometry = sf::st_sfc(poly)
  )
  
  # Mock metrics with weighted rates
  mock_metrics <- data.frame(
    adm1 = "District A",
    month = "Jan",
    reprate = 0.8,
    reprate_w = 0.85,
    missrate = 0.2,
    missrate_w = 0.15
  )
  
  mockery::stub(
    reporting_rate_map,
    "calculate_reporting_metrics",
    mock_metrics
  )
  
  # Test with weighting
  p <- reporting_rate_map(
    data = test_data,
    shapefile = test_shapefile,
    x_var = "month",
    adm_var = "adm1",
    vars_of_interest = "malaria",
    weighting = TRUE,
    weight_var = "allout",
    show_plot = FALSE
  )
  
  expect_s3_class(p, "ggplot")
  
  # The plot should use weighted rate (reprate_w)
  # This is harder to test directly, but at least check it doesn't error
})