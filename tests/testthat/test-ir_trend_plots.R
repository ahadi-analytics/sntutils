testthat::test_that("get_model returns a glm object for quasibinomial", {
  trend_data <- data.frame(
    x = 2010:2025,
    y = seq(0.95, 0.65, length.out = 16)
  )

  model <- get_model(trend_data, model_type = "quasibinomial")

  testthat::expect_s3_class(model, "glm")
  testthat::expect_equal(model$family$family, "quasibinomial")
})

testthat::test_that("get_model returns a lm object for linear model", {
  trend_data <- data.frame(
    x = 2010:2020,
    y = seq(0.9, 0.7, length.out = 11)
  )

  model <- get_model(trend_data, model_type = "lm")

  testthat::expect_s3_class(model, "lm")
})

testthat::test_that("get_model returns a gam object for GAM", {
  skip_if_not_installed("mgcv")

  trend_data <- data.frame(
    x = 2010:2025,
    y = c(
      0.95, 0.93, 0.91, 0.89, 0.87, 0.85, 0.83, 0.81,
      0.79, 0.77, 0.75, 0.73, 0.71, 0.69, 0.67, 0.65
    )
  )

  model <- get_model(trend_data, model_type = "gam")

  testthat::expect_s3_class(model, "gam")
})

testthat::test_that("get_model validates input data frame", {
  testthat::expect_error(
    get_model(list(x = 1:5, y = 1:5), model_type = "lm"),
    "must be a data.frame"
  )
})

testthat::test_that("get_model validates required columns", {
  trend_data <- data.frame(
    year = 2010:2020,
    value = seq(0.9, 0.7, length.out = 11)
  )

  testthat::expect_error(
    get_model(trend_data, model_type = "lm"),
    "missing required columns"
  )
})

testthat::test_that("get_model rejects unknown model type", {
  trend_data <- data.frame(
    x = 2010:2020,
    y = seq(0.9, 0.7, length.out = 11)
  )

  testthat::expect_error(
    get_model(trend_data, model_type = "random_forest"),
    "Unknown model_type"
  )
})

testthat::test_that("run_resistance_trend returns constant trend projection", {
  ir_data <- data.frame(
    adm0 = "BDI",
    adm1 = rep(c("Province A", "Province B"), each = 16),
    year = rep(2010:2025, 2),
    mean_ir = runif(32, 0.6, 0.95)
  )

  result <- run_resistance_trend(
    df_IR = ir_data,
    year_cut = 2025,
    n_years_to_add = 5,
    trendmethod = "constantTrend"
  )

  testthat::expect_true(is.data.frame(result))
  testthat::expect_true("trendmethod" %in% names(result))
  testthat::expect_true("mean_ir" %in% names(result))
  testthat::expect_equal(unique(result$trendmethod), "constantTrend")
  testthat::expect_true(max(result$year) == 2030)
})

testthat::test_that("run_resistance_trend returns continued trend projection", {
  ir_data <- data.frame(
    adm0 = "BDI",
    adm1 = "Province A",
    year = 2010:2025,
    mean_ir = seq(0.95, 0.65, length.out = 16)
  )

  result <- run_resistance_trend(
    df_IR = ir_data,
    year_cut = 2025,
    n_years_to_add = 5,
    model_type = "lm",
    trendmethod = "continuedTrend"
  )

  testthat::expect_true(is.data.frame(result))
  testthat::expect_equal(unique(result$trendmethod), "lm")
  testthat::expect_true(max(result$year) == 2030)
})

testthat::test_that("run_resistance_trend returns unchanged projection", {
  ir_data <- data.frame(
    adm0 = "BDI",
    adm1 = "Province A",
    year = 2010:2030,
    mean_ir = runif(21, 0.6, 0.95)
  )

  result <- run_resistance_trend(
    df_IR = ir_data,
    year_cut = 2025,
    n_years_to_add = 5,
    model_type = "unchanged",
    trendmethod = "continuedTrend"
  )

  testthat::expect_true(is.data.frame(result))
  testthat::expect_equal(unique(result$trendmethod), "unchanged")
})

testthat::test_that("run_resistance_trend returns hybrid trend projection", {
  ir_data <- data.frame(
    adm0 = "BDI",
    adm1 = "Province A",
    year = 2010:2025,
    mean_ir = seq(0.95, 0.65, length.out = 16)
  )

  result <- run_resistance_trend(
    df_IR = ir_data,
    year_cut = 2025,
    n_years_to_add = 5,
    trendmethod = "hybridTrend"
  )

  testthat::expect_true(is.data.frame(result))
  testthat::expect_equal(unique(result$trendmethod), "hybridTrend")
  testthat::expect_true(max(result$year) == 2030)
})

testthat::test_that("run_resistance_trend validates input data frame", {
  testthat::expect_error(
    run_resistance_trend(
      df_IR = list(adm0 = "BDI", year = 2010:2020),
      trendmethod = "constantTrend"
    ),
    "must be a data.frame"
  )
})

testthat::test_that("run_resistance_trend validates required columns", {
  ir_data <- data.frame(
    country = "BDI",
    province = "Province A",
    yr = 2010:2020,
    value = runif(11, 0.6, 0.95)
  )

  testthat::expect_error(
    run_resistance_trend(
      df_IR = ir_data,
      trendmethod = "constantTrend"
    ),
    "missing required columns"
  )
})

testthat::test_that("run_resistance_trend handles custom value column", {
  ir_data <- data.frame(
    adm0 = "BDI",
    adm1 = "Province A",
    year = 2010:2025,
    mortality_rate = runif(16, 0.6, 0.95)
  )

  result <- run_resistance_trend(
    df_IR = ir_data,
    year_cut = 2025,
    n_years_to_add = 5,
    trendmethod = "constantTrend",
    value_col = "mortality_rate"
  )

  testthat::expect_true("mortality_rate" %in% names(result))
  testthat::expect_false("mean_ir" %in% names(result))
})

testthat::test_that("generate_ir_plot returns a ggplot object", {
  skip_if_not_installed("ggplot2")

  ir_trends <- data.frame(
    adm0 = "BDI",
    adm1 = rep(c("Province A", "Province B"), each = 42),
    year = rep(2010:2030, 4),
    trendmethod = rep(c("constantTrend", "hybridTrend"), each = 42),
    mean_ir = runif(84, 0.6, 0.95)
  )

  plot_obj <- generate_ir_plot(
    data = ir_trends,
    value_col = "mean_ir",
    facet_col = "adm1",
    title = "Test IR Plot"
  )

  testthat::expect_s3_class(plot_obj, "ggplot")
})

testthat::test_that("generate_ir_plot validates input data frame", {
  testthat::expect_error(
    generate_ir_plot(
      data = list(year = 2010:2020, mean_ir = runif(11))
    ),
    "must be a data.frame"
  )
})

testthat::test_that("generate_ir_plot validates required columns", {
  ir_trends <- data.frame(
    province = "Province A",
    yr = 2010:2030,
    trend = "constantTrend",
    value = runif(21, 0.6, 0.95)
  )

  testthat::expect_error(
    generate_ir_plot(data = ir_trends),
    "missing required columns"
  )
})

testthat::test_that("generate_ir_plot saves to file when output_file provided", {
  skip_if_not_installed("ggplot2")

  tmp <- withr::local_tempdir()
  out_file <- fs::path(tmp, "test_ir_plot.png")

  ir_trends <- data.frame(
    adm1 = "Province A",
    year = 2010:2030,
    trendmethod = "constantTrend",
    mean_ir = seq(0.95, 0.65, length.out = 21)
  )

  plot_obj <- generate_ir_plot(
    data = ir_trends,
    value_col = "mean_ir",
    facet_col = "adm1",
    title = "Save Test",
    output_file = out_file,
    width = 8,
    height = 6,
    dpi = 72,
    compress_image = FALSE
  )

  testthat::expect_true(fs::file_exists(out_file))
  testthat::expect_s3_class(plot_obj, "ggplot")
})

testthat::test_that("generate_ir_plot handles custom value column", {
  skip_if_not_installed("ggplot2")

  ir_trends <- data.frame(
    adm1 = "Province A",
    year = 2010:2030,
    trendmethod = "constantTrend",
    mortality_pct = runif(21, 60, 95)
  )

  plot_obj <- generate_ir_plot(
    data = ir_trends,
    value_col = "mortality_pct",
    facet_col = "adm1",
    title = "Custom Column Test"
  )

  testthat::expect_s3_class(plot_obj, "ggplot")
})

testthat::test_that("generate_ir_plot returns invisibly when show_plot = FALSE", {
  skip_if_not_installed("ggplot2")

  ir_trends <- data.frame(
    adm1 = "Province A",
    year = 2010:2030,
    trendmethod = "constantTrend",
    mean_ir = runif(21, 0.6, 0.95)
  )

  result <- withVisible(
    generate_ir_plot(
      data = ir_trends,
      value_col = "mean_ir",
      facet_col = "adm1",
      show_plot = FALSE
    )
  )

  testthat::expect_false(result$visible)
  testthat::expect_s3_class(result$value, "ggplot")
})

testthat::test_that("generate_ir_plot respects ncol parameter", {
  skip_if_not_installed("ggplot2")

  ir_trends <- data.frame(
    adm1 = rep(paste("Province", LETTERS[1:6]), each = 21),
    year = rep(2010:2030, 6),
    trendmethod = "constantTrend",
    mean_ir = runif(126, 0.6, 0.95)
  )

  plot_obj <- generate_ir_plot(
    data = ir_trends,
    value_col = "mean_ir",
    facet_col = "adm1",
    ncol = 3,
    title = "Multi-province"
  )

  testthat::expect_s3_class(plot_obj, "ggplot")
  testthat::expect_true(inherits(plot_obj$facet, "FacetWrap"))
})
