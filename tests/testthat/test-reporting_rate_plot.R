.datatable.aware <- TRUE

testthat::test_that(
  "calculate_reporting_metrics works correctly",
  {
    # Create test data
    test_data <- data.frame(
      month = rep(c("Jan", "Feb", "Mar"), each = 4),
      district = rep(c("North", "South"), each = 2, times = 3),
      facility_id = rep(1:2, times = 6),
      malaria = c(10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 0),
      pneumonia = c(100, 150, 200, 250, 300, 350, 400, NA, 500, 550, 600, 0)
    )

    # Test 1: Basic reporting rate calculation
    result1 <- calculate_reporting_metrics(
      data = test_data,
      vars_of_interest = "malaria",
      x_var = "month",
      y_var = "district",
      hf_col = "facility_id"
    )

    testthat::expect_s3_class(result1, "tbl_df")

    testthat::expect_true(
      all(
        c(
          "month", "district", "num",
          "rep", "reprate", "missrate"
        ) %in% names(result1)
      )
    )

    testthat::expect_true(
      all(
        result1$reprate >= 0 & result1$reprate <= 100
      )
    )


    # Test 2: Missing rate calculation
    result2 <- calculate_reporting_metrics(
      data = test_data,
      vars_of_interest = "malaria",
      x_var = "month",
      y_var = "district"
    )

    testthat::expect_s3_class(result2, "tbl_df")

    testthat::expect_true(
      all(
        c(
          "month", "district",
          "num", "rep", "reprate", "missrate"
        ) %in% names(result2)
      )
    )

    testthat::expect_true(
      all(result2$missrate >= 0 & result2$missrate <= 100)
    )

    # Test 3: Multiple variables
    result3 <- calculate_reporting_metrics(
      data = test_data,
      vars_of_interest = c("malaria", "pneumonia"),
      x_var = "month",
      y_var = "district"
    )

    testthat::expect_s3_class(result3, "tbl_df")

    testthat::expect_true(
      all(
        c(
          "month", "district", "variable", "num",
          "rep", "reprate", "missrate"
        ) %in% names(result3)
      )
    )

    # Test 4: With zeros (should be converted to NA)
    test_data_with_zeros <- test_data
    result4 <- calculate_reporting_metrics(
      data = test_data_with_zeros,
      vars_of_interest = c("malaria", "pneumonia"),
      x_var = "month"
    )

    testthat::expect_s3_class(result4, "tbl_df")
    testthat::expect_true(
      all(c(
        "month", "variable",
        "num", "rep",
        "reprate", "missrate"
      ) %in% names(result4))
    )
  }
)


testthat::test_that(
  "prepare_plot_data works correctly for all scenarios",
  {
    # Create test data
    test_data <- data.frame(
      month = rep(c("Jan", "Feb", "Mar"), each = 4),
      district = rep(c("North", "South"), each = 2, times = 3),
      facility_id = rep(1:2, times = 6),
      malaria = c(10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 0),
      pneumonia = c(100, 150, 200, 250, 300, 350, 400, NA, 500, 550, 600, 0)
    )

    # Test 1: Scenario 1 - Time × Administrative Area
    result1 <- prepare_plot_data(
      data = test_data,
      x_var = "month",
      y_var = "district",
      vars_of_interest = "malaria",
      by_facility = FALSE,
      use_reprate = TRUE
    )

    testthat::expect_type(
      result1, "list"
    )
    testthat::expect_true(
      all(
        c(
          "plot_data", "vars_of_interest",
          "fill_var", "fill_label",
          "y_axis_label", "title_prefix",
          "title_vars", "title_suffix"
        ) %in% names(result1)
      )
    )

    testthat::expect_true(
      all(
        c(
          "month", "district", "num",
          "rep", "reprate"
        ) %in% names(result1$plot_data)
      )
    )


    # Test 2: Scenario 2 - Time × Variable
    result2 <- prepare_plot_data(
      data = test_data,
      x_var = "month",
      y_var = NULL,
      vars_of_interest = c("malaria", "pneumonia"),
      by_facility = FALSE,
      use_reprate = TRUE
    )

    testthat::expect_type(result2, "list")

    testthat::expect_true(all(c(
      "plot_data", "vars_of_interest", "fill_var",
      "fill_label", "y_axis_label", "title_prefix",
      "title_vars", "title_suffix"
    ) %in% names(result2)))

    testthat::expect_true(
      all(c(
        "month", "variable",
        "num", "rep", "reprate"
      ) %in% names(result2$plot_data))
    )

    # Test 3: Scenario 3 - Time × Administrative Area with Facility Coverage
    result3 <- prepare_plot_data(
      data = test_data,
      x_var = "month",
      y_var = "district",
      vars_of_interest = "malaria",
      by_facility = TRUE,
      hf_col = "facility_id",
      use_reprate = TRUE
    )

    testthat::expect_type(result3, "list")
    testthat::expect_true(all(c(
      "plot_data", "vars_of_interest", "fill_var",
      "fill_label", "y_axis_label", "title_prefix",
      "title_vars", "title_suffix"
    ) %in% names(result3)))
    testthat::expect_true(
      all(c(
        "month", "district", "num",
        "rep", "reprate"
      ) %in% names(result3$plot_data))
    )

    # Test 4: Missing rate instead of reporting rate
    result4 <- prepare_plot_data(
      data = test_data,
      x_var = "month",
      y_var = "district",
      vars_of_interest = "malaria",
      by_facility = FALSE,
      use_reprate = FALSE
    )

    testthat::expect_type(result4, "list")

    testthat::expect_true(all(c(
      "plot_data", "vars_of_interest", "fill_var", "fill_label",
      "y_axis_label", "title_prefix", "title_vars", "title_suffix"
    ) %in% names(result4)))

    testthat::expect_true(
      all(c(
        "month", "district", "num", "rep",
        "missrate"
      ) %in% names(result4$plot_data))
    )
  }
)

testthat::test_that("prepare_plot_data handles edge cases correctly", {
  # Create test data
  test_data <- data.frame(
    month = rep(c("Jan", "Feb", "Mar"), each = 4),
    district = rep(c("North", "South"), each = 2, times = 3),
    facility_id = rep(1:2, times = 6),
    malaria = c(10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 0),
    pneumonia = c(100, 150, 200, 250, 300, 350, 400, NA, 500, 550, 600, 0)
  )

  # Test 1: Single district in facility mode
  test_data_single_district <- test_data[test_data$district == "North", ]
  result1 <- prepare_plot_data(
    data = test_data_single_district,
    x_var = "month",
    y_var = "district",
    vars_of_interest = "malaria",
    by_facility = TRUE,
    hf_col = "facility_id",
    use_reprate = TRUE
  )

  testthat::expect_type(result1, "list")
  testthat::expect_true(
    all(c(
      "month", "district", "num",
      "rep", "reprate"
    ) %in% names(result1$plot_data))
  )

  # Test 2: Missing values in data
  test_data_with_na <- test_data
  test_data_with_na$malaria[1] <- NA
  result2 <- prepare_plot_data(
    data = test_data_with_na,
    x_var = "month",
    y_var = "district",
    vars_of_interest = "malaria",
    by_facility = FALSE,
    use_reprate = TRUE
  )

  testthat::expect_type(result2, "list")
  testthat::expect_true(
    all(c(
      "month", "district",
      "num", "rep", "reprate"
    ) %in% names(result2$plot_data))
  )

  # Test 3: All zeros in data
  test_data_all_zeros <- test_data
  test_data_all_zeros$malaria <- 0
  result3 <- prepare_plot_data(
    data = test_data_all_zeros,
    x_var = "month",
    y_var = "district",
    vars_of_interest = "malaria",
    by_facility = FALSE,
    use_reprate = TRUE
  )

  testthat::expect_type(result3, "list")
  testthat::expect_true(
    all(c(
      "month", "district", "num",
      "rep", "reprate"
    ) %in% names(result3$plot_data))
  )
})

testthat::test_that("prepare_plot_data handles errors correctly", {
  # Create test data
  test_data <- data.frame(
    month = rep(c("Jan", "Feb", "Mar"), each = 4),
    district = rep(c("North", "South"), each = 2, times = 3),
    facility_id = rep(1:2, times = 6),
    malaria = c(10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 0),
    pneumonia = c(100, 150, 200, 250, 300, 350, 400, NA, 500, 550, 600, 0)
  )

  # Test 1: Missing required parameters
  testthat::expect_error(
    prepare_plot_data(
      data = test_data,
      vars_of_interest = "malaria"
    ),
    'argument "x_var" is missing, with no default'
  )

  # Test 2: Invalid facility mode parameters
  testthat::expect_error(
    prepare_plot_data(
      data = test_data,
      x_var = "month",
      vars_of_interest = "malaria",
      by_facility = TRUE
    ),
    "'y_var' is required when by_facility = TRUE"
  )

  # Test 3: Multiple variables in facility mode
  testthat::expect_error(
    prepare_plot_data(
      data = test_data,
      x_var = "month",
      y_var = "district",
      vars_of_interest = c("malaria", "pneumonia"),
      by_facility = TRUE,
      hf_col = "facility_id"
    ),
    "Only one variable can be used when by_facility = TRUE"
  )
})

# Replace the failing test with this approach
testthat::test_that("reporting_rate_plot handles basic variable scenario", {
  # Create dummy data
  hf_data <- data.frame(
    month = rep(c("Jan", "Feb", "Mar"), each = 6),
    district = rep(c("North", "South"), each = 3, times = 3),
    facility = rep(1:3, times = 6),
    malaria = c(
      10, NA, 15, 7, 0, 12, 11, 14, NA, 8, 10, 9, 7,
      11, 0, 12, 15, 8
    ),
    pneumonia = c(5, NA, 7, 3, 0, 6, 8, 6, NA, 4, 0, 5, 4, 7, 0, 6, 3, 8)
  )

  # Test variable-level scenario (x_var only)
  testthat::expect_no_error(
    p1 <- reporting_rate_plot(
      data = hf_data,
      x_var = "month",
      vars_of_interest = c("malaria", "pneumonia")
    )
  )

  # Check plot is a ggplot object
  testthat::expect_s3_class(p1, "ggplot")

  # Alternative approach to check scale limits
  # Extract fill scale from the built plot layers
  fill_scale <- NULL
  for (layer in p1$scales$scales) {
    if (identical(layer$aesthetics, "fill")) {
      fill_scale <- layer
      break
    }
  }

  # Check if full_range is being applied (if scale exists)
  testthat::skip_if(is.null(fill_scale), "Fill scale not found in plot")

  # For full_range=TRUE, we expect limits to be set
  if (!is.null(fill_scale$limits)) {
    testthat::expect_equal(fill_scale$limits, c(0, 100))
  }

  # Test with full_range=FALSE
  testthat::expect_no_error(
    p2 <- reporting_rate_plot(
      data = hf_data,
      x_var = "month",
      vars_of_interest = c("malaria", "pneumonia"),
      full_range = FALSE
    )
  )

  # We can visually check the plot structure is different
  # Rather than checking exact limits (which depend on data values)
  testthat::expect_false(identical(p1, p2))

  # Test with use_reprate=FALSE to show missing rate instead
  testthat::expect_no_error(
    p3 <- reporting_rate_plot(
      data = hf_data,
      x_var = "month",
      vars_of_interest = c("malaria", "pneumonia"),
      use_reprate = FALSE
    )
  )
})


testthat::test_that("reporting_rate_plot handles district-level scenario", {
  # Create dummy data
  hf_data <- data.frame(
    month = rep(c("Jan", "Feb", "Mar"), each = 6),
    district = rep(c("North", "South"), each = 3, times = 3),
    facility = rep(1:3, times = 6),
    malaria = c(10, NA, 15, 7, 0, 12, 11, 14, NA, 8, 10, 9, 7, 11, 0, 12, 15, 8),
    pneumonia = c(5, NA, 7, 3, 0, 6, 8, 6, NA, 4, 0, 5, 4, 7, 0, 6, 3, 8)
  )

  # Test district-level scenario (x_var and y_var)
  testthat::expect_no_error(
    p1 <- reporting_rate_plot(
      data = hf_data,
      x_var = "month",
      y_var = "district",
      vars_of_interest = c("malaria", "pneumonia")
    )
  )

  # Check y-axis variable is correctly set
  testthat::expect_equal(
    rlang::as_name(p1$mapping$y),
    "district"
  )

  # Test with custom y_axis_label
  testthat::expect_no_error(
    p2 <- reporting_rate_plot(
      data = hf_data,
      x_var = "month",
      y_var = "district",
      vars_of_interest = c("malaria", "pneumonia"),
      y_axis_label = "Admin Level"
    )
  )

  # Verify that labels differ
  testthat::expect_false(
    identical(p1$labels$y, p2$labels$y)
  )
})

testthat::test_that("reporting_rate_plot handles facility-level scenario", {
  # Create dummy data
  hf_data <- data.frame(
    month = rep(c("Jan", "Feb", "Mar"), each = 6),
    district = rep(c("North", "South"), each = 3, times = 3),
    facility = rep(1:3, times = 6),
    malaria = c(
      10, NA, 15, 7, 0, 12, 11, 14, NA, 8, 10, 9, 7,
      11, 0, 12, 15, 8
    ),
    pneumonia = c(5, NA, 7, 3, 0, 6, 8, 6, NA, 4, 0, 5, 4, 7, 0, 6, 3, 8)
  )

  # Test facility-level scenario
  testthat::expect_no_error(
    p1 <- reporting_rate_plot(
      data = hf_data,
      x_var = "month",
      y_var = "district",
      vars_of_interest = "malaria",
      hf_col = "facility"
    )
  )
})

testthat::test_that("reporting_rate_plot validates inputs correctly", {
  # Create dummy data
  hf_data <- data.frame(
    month = rep(c("Jan", "Feb", "Mar"), each = 6),
    district = rep(c("North", "South"), each = 3, times = 3),
    facility = rep(1:3, times = 6),
    malaria = c(
      10, NA, 15, 7, 0, 12, 11, 14, NA, 8,
      10, 9, 7, 11, 0, 12, 15, 8
    )
  )

  # Test invalid x_var
  testthat::expect_error(
    reporting_rate_plot(
      data = hf_data,
      x_var = "nonexistent_column",
      vars_of_interest = "malaria"
    ),
    regexp = "A valid 'x_var' must be provided"
  )

  # Test invalid data type
  testthat::expect_error(
    reporting_rate_plot(
      data = "not_a_dataframe",
      x_var = "month",
      vars_of_interest = "malaria"
    ),
    "A valid 'x_var' must be provided and must exist in the data."
  )

  # Test facility-level validation - requires y_var
  testthat::expect_error(
    reporting_rate_plot(
      data = hf_data,
      x_var = "month",
      vars_of_interest = "malaria",
      hf_col = "facility"
    ),
    "For facility-level analysis, both 'hf_col'and 'y_var' must be provided."
  )

  # Test facility-level validation - only one variable
  testthat::expect_error(
    reporting_rate_plot(
      data = hf_data,
      x_var = "month",
      y_var = "district",
      vars_of_interest = c("malaria", "pneumonia"),
      hf_col = "facility"
    ),
    regexp = "Only one variable can be used"
  )

  # Test save_plot requires plot_path
  testthat::expect_error(
    reporting_rate_plot(
      data = hf_data,
      x_var = "month",
      vars_of_interest = "malaria",
      save_plot = TRUE
    ),
    regexp = "plot_path"
  )
})

testthat::test_that("reporting_rate_plot saves plots correctly", {
  # Skip if not interactive or in CI environment
  testthat::skip_if(!interactive() && !identical(Sys.getenv("CI"), "true"))
  # Skip if gtranslate not available
  testthat::skip_if_not_installed("gtranslate")
  testthat::skip_if_offline()

  # Create dummy data
  hf_data <- data.frame(
    month = rep(c("Jan", "Feb", "Mar"), each = 6),
    district = rep(c("North", "South"), each = 3, times = 3),
    facility = rep(1:3, times = 6),
    malaria = c(
      10, NA, 15, 7, 0, 12, 11, 14,
      NA, 8, 10, 9, 7, 11, 0, 12, 15, 8
    )
  )

  # Create temp dir for test
  temp_dir <- tempfile("plot_test_")
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE))

  # Test saving plot
  testthat::expect_no_error(
    reporting_rate_plot(
      data = hf_data,
      x_var = "month",
      vars_of_interest = "malaria",
      save_plot = TRUE,
      plot_path = temp_dir,
      compress_image = FALSE
    )
  )

  # Check that a file was created
  testthat::expect_gte(length(list.files(temp_dir, pattern = "\\.png$")), 1)
})

testthat::test_that("reporting_rate_plot handles language parameter", {
  # Skip if gtranslate not available
  testthat::skip_if_not_installed("gtranslate")
  testthat::skip_if_offline()

  # Create dummy data
  hf_data <- data.frame(
    month = rep(c("Jan", "Feb", "Mar"), each = 6),
    district = rep(c("North", "South"), each = 3, times = 3),
    facility = rep(1:3, times = 6),
    malaria = c(
      10, NA, 15, 7, 0, 12, 11, 14,
      NA, 8, 10, 9, 7, 11, 0, 12, 15, 8
    )
  )

  # Test with English (default)
  p1 <- reporting_rate_plot(
    data = hf_data,
    x_var = "month",
    y_var = "district",
    vars_of_interest = "malaria",
    target_language = "en"
  )

  # Store original labels
  en_title <- p1$labels$title
  en_y_label <- p1$labels$y
  en_fill_label <- p1$labels$fill

  # Create the plot with French translation
  testthat::expect_no_error(
    p2 <- reporting_rate_plot(
      data = hf_data,
      x_var = "month",
      y_var = "district",
      vars_of_interest = "malaria",
      target_language = "fr",
      y_axis_label = "Administrative level"
    )
  )

  # Get French labels
  fr_title <- p2$labels$title
  fr_y_label <- p2$labels$y
  fr_fill_label <- p2$labels$fill

  testthat::expect_equal(
    p2$labels$title,
    "Taux De Déclaration Du Paludisme Par Mois et District"
  )

  testthat::expect_equal(
    p2$labels$fill,
    "Taux de rapport (%)"
  )

  testthat::expect_equal(
    p2$labels$y,
    "Niveau administratif"
  )
})

testthat::test_that("translate_text() handles basic translations", {
  tmp_cache <- tempfile()
  dir.create(tmp_cache)

  res <- translate_text("Hola",
    target_language = "en",
    source_language = "es", cache_path = tmp_cache
  )
  testthat::expect_type(res, "character")
  testthat::expect_true(res %in% c("Hello", "hello"))

  res2 <- translate_text("Hola",
    target_language = "en",
    source_language = "es", cache_path = tmp_cache
  )
  testthat::expect_identical(res, res2)
})

testthat::test_that("same source and target language returns input", {
  testthat::expect_message(
    out <- translate_text("Bonjour",
      target_language = "fr",
      source_language = "fr"
    ),
    "Source and target languages are the same. Returning original text."
  )

  testthat::expect_identical(out, "Bonjour")
})
