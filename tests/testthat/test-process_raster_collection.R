testthat::test_that(
  "clean_filenames function works correctly with specific example", {

    # Create a vector of filenames with consistent number patterns and unique numbers
    filenames <- c(
      "image_1234_backup_2372_273626_19872.png",
      "image_1234_backup_2372_273626_19873.png",
      "image_1234_backup_2372_273626_19874.png",
      "image_1234_backup_2372_273626_19876.png"
    )

    # Define the expected result after cleaning, retaining unique numbers
    expected_filenames <- c(
      "image_backup_19872.png",
      "image_backup_19873.png",
      "image_backup_19874.png",
      "image_backup_19876.png"
    )

    # Run the cleaning function
    cleaned_filenames <- clean_filenames(filenames)

    # Test if the function output matches the expected output
    testthat::expect_equal(
      sort(cleaned_filenames),
      sort(expected_filenames)
    )
  })

testthat::test_that(
  "clean_filenames handles leading and trailing common numbers", {
    filenames <- c(
      "123_image_456_sample_001.png",
      "123_image_456_sample_002.png",
      "123_image_456_sample_003.png"
    )

    expected_filenames <- c(
      "image_sample_001.png",
      "image_sample_002.png",
      "image_sample_003.png"
    )

    cleaned_filenames <- clean_filenames(filenames)

    testthat::expect_equal(sort(cleaned_filenames), sort(expected_filenames))
  })

testthat::test_that("clean_filenames removes scattered common numbers", {
  filenames <- c(
    "file_111_part_222_section_A.png",
    "doc_111_part_222_section_B.png",
    "report_111_part_222_section_C.pdf"
  )

  expected_filenames <- c(
    "file_part_section_A.png",
    "doc_part_section_B.png",
    "report_part_section_C.pdf"
  )

  cleaned_filenames <- clean_filenames(filenames)

  testthat::expect_equal(sort(cleaned_filenames), sort(expected_filenames))
})


testthat::test_that("clean_filenames deals with inconsistent naming patterns", {
  filenames <- c(
    "backup_2020_file_01.png",
    "restore_2020_file_02.log",
    "log_2020_report_03.pdf"
  )

  expected_filenames <- c(
    "backup_file_01.png",
    "restore_file_02.log",
    "log_report_03.pdf"
  )

  cleaned_filenames <- clean_filenames(filenames)

  testthat::expect_equal(sort(cleaned_filenames), sort(expected_filenames))
})

testthat::test_that("clean_filenames processes mixed content", {
  filenames <- c(
    "data_333_entry_h1_v1.txt",
    "info_333_log_h2_v1.txt",
    "notes_333_draft_h3_v1.txt"
  )

  expected_filenames <- c(
    "data_entry_h1_v1.txt",
    "info_log_h2_v1.txt",
    "notes_draft_h3_v1.txt"
  )

  cleaned_filenames <- clean_filenames(filenames)

  testthat::expect_equal(sort(cleaned_filenames), sort(expected_filenames))
})

testthat::test_that("clean_filenames handles directories correctly", {

  files <- c(
    paste0(
      "/Users/mohamedyusuf/ahadi-analytics/GitHub/snt-code-library/english",
      "/data_r/MAPs/Malaria_Global_Pf_Parasite_Rate.273782.2021.2773.tiff"),
    paste0(
      "/Users/mohamedyusuf/ahadi-analytics/GitHub/snt-code-library/english",
      "/data_r/MAPs/Malaria_Global_Pf_Parasite_Rate.273782.2022.2773.tiff"),
    paste0(
      "/Users/mohamedyusuf/ahadi-analytics/GitHub/snt-code-library/english",
      "/data_r/MAPs/Malaria_Global_Pf_Parasite_Rate.273782.2023.2773.tiff")
  )

  expected <- c(
    paste0(
      "/Users/mohamedyusuf/ahadi-analytics/GitHub/snt-code-library/english",
      "/data_r/MAPs/Malaria_Global_Pf_Parasite_Rate.2021.tiff"),
    paste0(
      "/Users/mohamedyusuf/ahadi-analytics/GitHub/snt-code-library/english",
      "/data_r/MAPs/Malaria_Global_Pf_Parasite_Rate.2022.tiff"),
    paste0(
      "/Users/mohamedyusuf/ahadi-analytics/GitHub/snt-code-library/english",
      "/data_r/MAPs/Malaria_Global_Pf_Parasite_Rate.2023.tiff")
  )

  cleaned <- clean_filenames(files)

  testthat::expect_equal(cleaned, expected)
})


testthat::test_that("detect_time_pattern correctly identifies monthly formats", {
  # Setup various monthly format test cases
  monthly_formats <- list(
    standard = c(
      "chirps-v2.0.2020.01.tif",
      "chirps-v2.0.2020.02.tif"
    ),
    underscore = c(
      "rainfall_2020_01.tif",
      "rainfall_2020_02.tif"
    ),
    dash = c(
      "precip-2020-01.tif",
      "precip-2020-02.tif"
    ),
    mixed = c(
      "chirps-v2.0.2020.01.tif",
      "rainfall_2020_02.tif",
      "precip-2020-03.tif"
    )
  )

  # Test each monthly format
  purrr::walk(monthly_formats, function(files) {
    pattern_info <- detect_time_pattern(files)
    testthat::expect_equal(pattern_info$pattern, "monthly")

    # Test parsing works for each file
    dates <- purrr::map(files, function(f) {
      components <- extract_time_components(f, pattern_info)
      testthat::expect_false(is.na(components$year))
      testthat::expect_false(is.na(components$month))
      testthat::expect_s3_class(components$date, "Date")
    })
  })
})

testthat::test_that("detect_time_pattern correctly identifies yearly formats", {
  yearly_formats <- list(
    standard = c(
      "annual_2020.tif",
      "annual_2021.tif"
    ),
    compact = c(
      "temp2020.tif",
      "temp2021.tif"
    ),
    with_prefix = c(
      "y2020_summary.tif",
      "y2021_summary.tif"
    )
  )

  purrr::walk(yearly_formats, function(files) {
    pattern_info <- detect_time_pattern(files)
    testthat::expect_equal(pattern_info$pattern, "yearly")

    # Test parsing works for each file
    purrr::walk(files, function(f) {
      components <- extract_time_components(f, pattern_info)
      testthat::expect_false(is.na(components$year))
      testthat::expect_true(is.na(components$month))
      testthat::expect_s3_class(components$date, "Date")
      testthat::expect_equal(format(components$date, "%m-%d"), "01-01")
    })
  })
})

testthat::test_that("detect_time_pattern handles edge cases", {
  # Test single file
  single_file <- "chirps-v2.0.2020.01.tif"
  pattern_info <- detect_time_pattern(single_file)
  testthat::expect_equal(pattern_info$pattern, "monthly")

  # Test mixed yearly and monthly files - should prefer more specific pattern
  mixed_files <- c(
    "chirps-v2.0.2020.01.tif",
    "annual_2020.tif"
  )
  pattern_info <- detect_time_pattern(mixed_files)
  testthat::expect_equal(pattern_info$pattern, "monthly")
})

testthat::test_that("detect_time_pattern handles international date formats", {
  international_formats <- list(
    iso = c(
      "2020-01-data.tif",
      "2020-02-data.tif"
    ),
    european = c(
      "01-2020-data.tif",
      "02-2020-data.tif"
    )
  )

  purrr::walk(international_formats, function(files) {
    pattern_info <- detect_time_pattern(files)
    testthat::expect_equal(pattern_info$pattern, "monthly")

    purrr::walk(files, function(f) {
      components <- extract_time_components(f, pattern_info)
      testthat::expect_false(is.na(components$year))
      testthat::expect_false(is.na(components$month))
      testthat::expect_s3_class(components$date, "Date")
    })
  })
})

testthat::test_that("detect_time_pattern handles various separators", {
  separator_formats <- c(
    "data.2020.01.tif",
    "data_2020_01.tif",
    "data-2020-01.tif"
  )

  pattern_info <- detect_time_pattern(separator_formats)
  testthat::expect_equal(pattern_info$pattern, "monthly")

  purrr::walk(separator_formats, function(f) {
    components <- extract_time_components(f, pattern_info)
    testthat::expect_false(is.na(components$year))
    testthat::expect_false(is.na(components$month))
  })
})

testthat::test_that("detect_time_pattern handles various file extensions", {
  extension_formats <- c(
    "data_2020_01.tif",
    "data_2020_01.tiff",
    "data_2020_01.nc",
    "data_2020_01.grd",
    "data_2020_01.asc"
  )

  pattern_info <- detect_time_pattern(extension_formats)
  testthat::expect_equal(pattern_info$pattern, "monthly")

  purrr::walk(extension_formats, function(f) {
    components <- extract_time_components(f, pattern_info)
    testthat::expect_false(is.na(components$year))
    testthat::expect_false(is.na(components$month))
  })
})

testthat::test_that("detect_time_pattern handles daily formats", {
  daily_formats <- c(
    "data_2020-01-15.tif",
    "data_2020.01.15.tif",
    "data_2020_01_15.tif"
  )

  pattern_info <- detect_time_pattern(daily_formats)
  testthat::expect_equal(pattern_info$pattern, "daily")
})

testthat::test_that("detect_time_pattern fails gracefully with invalid input", {
  invalid_formats <- c(
    "nodate.tif",
    "invalid_format.tif"
  )

  testthat::expect_error(
    detect_time_pattern(invalid_formats),
    "No date pattern found in filenames"
  )
})

testthat::test_that("process_raster_with_boundaries works with basic inputs", {
  # create mock raster data
  temp_raster <- tempfile(fileext = ".tif")

  # create simple 10x10 raster with values 1-100
  raster_data <- terra::rast(
    nrows = 10,
    ncols = 10,
    xmin = 0,
    xmax = 10,
    ymin = 0,
    ymax = 10,
    crs = "EPSG:4326"
  )
  terra::values(raster_data) <- runif(100, min = 10, max = 100)
  terra::writeRaster(raster_data, temp_raster, overwrite = TRUE)

  # create mock shapefile with two polygons
  polygon1 <- sf::st_polygon(list(rbind(
    c(0, 0),
    c(5, 0),
    c(5, 5),
    c(0, 5),
    c(0, 0)
  )))
  polygon2 <- sf::st_polygon(list(rbind(
    c(5, 5),
    c(10, 5),
    c(10, 10),
    c(5, 10),
    c(5, 5)
  )))

  mock_shapefile <- sf::st_sf(
    adm0 = c("country_a", "country_b"),
    adm1 = c("district_1", "district_2"),
    geometry = sf::st_sfc(polygon1, polygon2, crs = 4326)
  )

  # mock helper functions
  mockery::stub(
    process_raster_with_boundaries,
    "detect_time_pattern",
    list(pattern = "monthly")
  )

  mockery::stub(
    process_raster_with_boundaries,
    "extract_time_components",
    list(date = as.Date("2022-01-01"), year = 2022, month = 1)
  )

  # test basic functionality
  result <- process_raster_with_boundaries(
    raster_file = temp_raster,
    shapefile = mock_shapefile,
    id_cols = c("adm0", "adm1"),
    aggregations = c("mean")
  )

  # check result structure
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_true(all(
    c("file_name", "adm0", "adm1", "year", "month", "mean") %in% names(result)
  ))
  testthat::expect_equal(nrow(result), 2)
  testthat::expect_true(all(is.numeric(result$mean)))
  testthat::expect_true(all(!is.na(result$mean)))

  # cleanup
  unlink(temp_raster)
})

testthat::test_that("process_raster_with_boundaries handles multiple aggregations", {
  # create mock raster data
  temp_raster <- tempfile(fileext = ".tif")

  raster_data <- terra::rast(
    nrows = 5,
    ncols = 5,
    xmin = 0,
    xmax = 5,
    ymin = 0,
    ymax = 5,
    crs = "EPSG:4326"
  )
  terra::values(raster_data) <- c(1:25)
  terra::writeRaster(raster_data, temp_raster, overwrite = TRUE)

  # create single polygon covering entire raster
  polygon <- sf::st_polygon(list(rbind(
    c(0, 0),
    c(5, 0),
    c(5, 5),
    c(0, 5),
    c(0, 0)
  )))

  mock_shapefile <- sf::st_sf(
    adm0 = "test_country",
    adm1 = "test_district",
    geometry = sf::st_sfc(polygon, crs = 4326)
  )

  # mock helper functions
  mockery::stub(
    process_raster_with_boundaries,
    "detect_time_pattern",
    list(pattern = "yearly")
  )

  mockery::stub(
    process_raster_with_boundaries,
    "extract_time_components",
    list(date = as.Date("2022-01-01"), year = 2022)
  )

  # test multiple aggregations
  result <- process_raster_with_boundaries(
    raster_file = temp_raster,
    shapefile = mock_shapefile,
    aggregations = c("mean", "sum", "median")
  )

  # check result structure
  testthat::expect_true(all(c("mean", "sum", "median") %in% names(result)))
  testthat::expect_equal(nrow(result), 1)
  testthat::expect_true(all(is.numeric(c(
    result$mean,
    result$sum,
    result$median
  ))))

  # cleanup
  unlink(temp_raster)
})

testthat::test_that("process_raster_with_boundaries handles density conversion", {
  # create mock raster data
  temp_raster <- tempfile(fileext = ".tif")

  raster_data <- terra::rast(
    nrows = 3,
    ncols = 3,
    xmin = 0,
    xmax = 3,
    ymin = 0,
    ymax = 3,
    crs = "EPSG:4326"
  )
  terra::values(raster_data) <- rep(1, 9) # density of 1 per km²
  terra::writeRaster(raster_data, temp_raster, overwrite = TRUE)

  # create polygon covering entire raster
  polygon <- sf::st_polygon(list(rbind(
    c(0, 0),
    c(3, 0),
    c(3, 3),
    c(0, 3),
    c(0, 0)
  )))

  mock_shapefile <- sf::st_sf(
    adm0 = "test_country",
    adm1 = "test_district",
    geometry = sf::st_sfc(polygon, crs = 4326)
  )

  # mock helper functions
  mockery::stub(
    process_raster_with_boundaries,
    "detect_time_pattern",
    list(pattern = "yearly")
  )

  mockery::stub(
    process_raster_with_boundaries,
    "extract_time_components",
    list(date = as.Date("2022-01-01"), year = 2022)
  )

  # test with density conversion
  result <- process_raster_with_boundaries(
    raster_file = temp_raster,
    shapefile = mock_shapefile,
    aggregations = c("sum"),
    raster_is_density = TRUE
  )

  # sum should be greater than 9 (original cell count) due to area conversion
  testthat::expect_true(result$sum > 9)

  # cleanup
  unlink(temp_raster)
})

testthat::test_that("process_raster_with_boundaries handles no-data values", {
  # create mock raster with no-data values
  temp_raster <- tempfile(fileext = ".tif")

  raster_data <- terra::rast(
    nrows = 4,
    ncols = 4,
    xmin = 0,
    xmax = 4,
    ymin = 0,
    ymax = 4,
    crs = "EPSG:4326"
  )

  # set some values to -9999 (no-data)
  raster_values <- c(
    1,
    2,
    3,
    4,
    -9999,
    -9999,
    7,
    8,
    9,
    10,
    11,
    12,
    13,
    14,
    15,
    16
  )
  terra::values(raster_data) <- raster_values
  terra::writeRaster(raster_data, temp_raster, overwrite = TRUE)

  # create polygon covering entire raster
  polygon <- sf::st_polygon(list(rbind(
    c(0, 0),
    c(4, 0),
    c(4, 4),
    c(0, 4),
    c(0, 0)
  )))

  mock_shapefile <- sf::st_sf(
    adm0 = "test_country",
    adm1 = "test_district",
    geometry = sf::st_sfc(polygon, crs = 4326)
  )

  # mock helper functions
  mockery::stub(
    process_raster_with_boundaries,
    "detect_time_pattern",
    list(pattern = "yearly")
  )

  mockery::stub(
    process_raster_with_boundaries,
    "extract_time_components",
    list(date = as.Date("2022-01-01"), year = 2022)
  )

  # test no-data handling
  result <- process_raster_with_boundaries(
    raster_file = temp_raster,
    shapefile = mock_shapefile,
    aggregations = c("mean", "sum")
  )

  # mean should exclude no-data values
  expected_mean <- mean(c(1, 2, 3, 4, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16))
  testthat::expect_equal(result$mean, expected_mean, tolerance = 0.1)

  # cleanup
  unlink(temp_raster)
})

testthat::test_that("process_raster_with_boundaries validates aggregation methods", {
  # create minimal mock data
  temp_raster <- tempfile(fileext = ".tif")

  raster_data <- terra::rast(
    nrows = 2,
    ncols = 2,
    xmin = 0,
    xmax = 2,
    ymin = 0,
    ymax = 2,
    crs = "EPSG:4326"
  )
  terra::values(raster_data) <- c(1, 2, 3, 4)
  terra::writeRaster(raster_data, temp_raster, overwrite = TRUE)

  polygon <- sf::st_polygon(list(rbind(
    c(0, 0),
    c(2, 0),
    c(2, 2),
    c(0, 2),
    c(0, 0)
  )))

  mock_shapefile <- sf::st_sf(
    adm0 = "test_country",
    geometry = sf::st_sfc(polygon, crs = 4326)
  )

  # test invalid aggregation method
  testthat::expect_error(
    process_raster_with_boundaries(
      raster_file = temp_raster,
      shapefile = mock_shapefile,
      aggregations = c("invalid_method")
    ),
    "Invalid aggregation method"
  )

  # cleanup
  unlink(temp_raster)
})

testthat::test_that("process_raster_with_boundaries handles crs transformation", {
  # create mock raster in different crs
  temp_raster <- tempfile(fileext = ".tif")

  raster_data <- terra::rast(
    nrows = 3,
    ncols = 3,
    xmin = 0,
    xmax = 3,
    ymin = 0,
    ymax = 3,
    crs = "EPSG:3857" # web mercator
  )
  terra::values(raster_data) <- c(1:9)
  terra::writeRaster(raster_data, temp_raster, overwrite = TRUE)

  # create shapefile in different crs
  polygon <- sf::st_polygon(list(rbind(
    c(0, 0),
    c(3, 0),
    c(3, 3),
    c(0, 3),
    c(0, 0)
  )))

  mock_shapefile <- sf::st_sf(
    adm0 = "test_country",
    adm1 = "test_district",
    geometry = sf::st_sfc(polygon, crs = 4326) # wgs84
  )

  # mock helper functions
  mockery::stub(
    process_raster_with_boundaries,
    "detect_time_pattern",
    list(pattern = "yearly")
  )

  mockery::stub(
    process_raster_with_boundaries,
    "extract_time_components",
    list(date = as.Date("2022-01-01"), year = 2022)
  )

  # function should handle crs transformation automatically
  testthat::expect_no_error(
    result <- process_raster_with_boundaries(
      raster_file = temp_raster,
      shapefile = mock_shapefile
    )
  )

  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_equal(nrow(result), 1)

  # cleanup
  unlink(temp_raster)
})

testthat::test_that("process_raster_with_boundaries handles custom id columns", {
  # create mock data
  temp_raster <- tempfile(fileext = ".tif")

  raster_data <- terra::rast(
    nrows = 2,
    ncols = 2,
    xmin = 0,
    xmax = 2,
    ymin = 0,
    ymax = 2,
    crs = "EPSG:4326"
  )
  terra::values(raster_data) <- c(10, 20, 30, 40)
  terra::writeRaster(raster_data, temp_raster, overwrite = TRUE)

  # create shapefile with custom column names
  polygon <- sf::st_polygon(list(rbind(
    c(0, 0),
    c(2, 0),
    c(2, 2),
    c(0, 2),
    c(0, 0)
  )))

  mock_shapefile <- sf::st_sf(
    country_name = "test_country",
    district_name = "test_district",
    population = 1000,
    geometry = sf::st_sfc(polygon, crs = 4326)
  )

  # mock helper functions
  mockery::stub(
    process_raster_with_boundaries,
    "detect_time_pattern",
    list(pattern = "yearly")
  )

  mockery::stub(
    process_raster_with_boundaries,
    "extract_time_components",
    list(date = as.Date("2022-01-01"), year = 2022)
  )

  # test custom id columns
  result <- process_raster_with_boundaries(
    raster_file = temp_raster,
    shapefile = mock_shapefile,
    id_cols = c("country_name", "district_name", "population")
  )

  # check custom columns are included
  testthat::expect_true(all(
    c("country_name", "district_name", "population") %in% names(result)
  ))
  testthat::expect_equal(result$country_name, "test_country")
  testthat::expect_equal(result$district_name, "test_district")
  testthat::expect_equal(result$population, 1000)

  # cleanup
  unlink(temp_raster)
})

testthat::test_that("process_raster_with_boundaries returns correct time columns", {
  # create mock data
  temp_raster <- tempfile(fileext = ".tif")

  raster_data <- terra::rast(
    nrows = 2,
    ncols = 2,
    xmin = 0,
    xmax = 2,
    ymin = 0,
    ymax = 2,
    crs = "EPSG:4326"
  )
  terra::values(raster_data) <- c(1, 2, 3, 4)
  terra::writeRaster(raster_data, temp_raster, overwrite = TRUE)

  polygon <- sf::st_polygon(list(rbind(
    c(0, 0),
    c(2, 0),
    c(2, 2),
    c(0, 2),
    c(0, 0)
  )))

  mock_shapefile <- sf::st_sf(
    adm0 = "test_country",
    adm1 = "test_prov",
    adm2 = "test_dist",
    geometry = sf::st_sfc(polygon, crs = 4326)
  )

  # test monthly pattern
  mockery::stub(
    process_raster_with_boundaries,
    "detect_time_pattern",
    list(pattern = "monthly")
  )

  mockery::stub(
    process_raster_with_boundaries,
    "extract_time_components",
    list(date = as.Date("2022-05-01"), year = 2022, month = 5)
  )

  result_monthly <- process_raster_with_boundaries(
    raster_file = temp_raster,
    shapefile = mock_shapefile
  )

  testthat::expect_true(all(c("year", "month") %in% names(result_monthly)))
  testthat::expect_equal(result_monthly$year, 2022)
  testthat::expect_equal(result_monthly$month, 5)

  # test yearly pattern
  mockery::stub(
    process_raster_with_boundaries,
    "detect_time_pattern",
    list(pattern = "yearly")
  )

  mockery::stub(
    process_raster_with_boundaries,
    "extract_time_components",
    list(date = as.Date("2022-01-01"), year = 2022)
  )

  result_yearly <- process_raster_with_boundaries(
    raster_file = temp_raster,
    shapefile = mock_shapefile
  )

  testthat::expect_true("year" %in% names(result_yearly))
  testthat::expect_false("month" %in% names(result_yearly))
  testthat::expect_equal(result_yearly$year, 2022)

  # cleanup
  unlink(temp_raster)
})

testthat::test_that("process_raster_collection works with multiple files", {
  # create temporary directory with mock raster files
  temp_dir <- tempdir()
  test_subdir <- file.path(temp_dir, "test_rasters")
  dir.create(test_subdir, showWarnings = FALSE)

  # create three mock raster files
  raster_files <- c(
    file.path(test_subdir, "chirps-v2.0.2022.01.tif"),
    file.path(test_subdir, "chirps-v2.0.2022.02.tif"),
    file.path(test_subdir, "chirps-v2.0.2022.03.tif")
  )

  for (i in seq_along(raster_files)) {
    raster_data <- terra::rast(
      nrows = 3,
      ncols = 3,
      xmin = 0,
      xmax = 3,
      ymin = 0,
      ymax = 3,
      crs = "EPSG:4326"
    )
    terra::values(raster_data) <- rep(i * 10, 9) # different values per file
    terra::writeRaster(raster_data, raster_files[i], overwrite = TRUE)
  }

  # create mock shapefile
  polygon <- sf::st_polygon(list(rbind(
    c(0, 0),
    c(3, 0),
    c(3, 3),
    c(0, 3),
    c(0, 0)
  )))

  mock_shapefile <- sf::st_sf(
    adm0 = "test_country",
    adm1 = "test_district",
    adm2 = "test_subdistrict",
    geometry = sf::st_sfc(polygon, crs = 4326)
  )

  # mock helper functions
  mockery::stub(
    process_raster_collection,
    "clean_filenames",
    raster_files # return original filenames
  )

  # mock process_raster_with_boundaries to return predictable results
  mock_process_function <- function(
    raster_file,
    shapefile,
    id_cols,
    aggregations,
    raster_is_density,
    layer_to_process
  ) {
    # extract month from filename for testing
    month_num <- as.numeric(gsub(
      ".*\\.([0-9]{2})\\.tif$",
      "\\1",
      basename(raster_file)
    ))

    data.frame(
      file_name = basename(raster_file),
      adm0 = shapefile$adm0[1],
      adm1 = shapefile$adm1[1],
      adm2 = shapefile$adm2[1],
      year = 2022,
      month = month_num,
      mean = month_num * 10 # predictable values
    )
  }

  mockery::stub(
    process_raster_collection,
    "process_raster_with_boundaries",
    mock_process_function
  )

  # test collection processing
  result <- process_raster_collection(
    directory = test_subdir,
    shapefile = mock_shapefile,
    id_cols = c("adm0", "adm1", "adm2"),
    pattern = "chirps.*\\.tif$",
    aggregations = c("mean")
  )

  # check result structure
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_equal(nrow(result), 3) # three files processed
  testthat::expect_true(all(
    c("file_name", "adm0", "adm1", "adm2", "year", "month", "mean") %in%
      names(result)
  ))

  # check sorting by year and month
  testthat::expect_equal(result$month, c(1, 2, 3))
  testthat::expect_equal(result$mean, c(10, 20, 30))

  # cleanup
  unlink(test_subdir, recursive = TRUE)
})

testthat::test_that("process_raster_collection handles single file", {
  # create temporary directory with single file
  temp_dir <- tempdir()
  test_subdir <- file.path(temp_dir, "single_raster")
  dir.create(test_subdir, showWarnings = FALSE)

  # create single mock raster file
  raster_file <- file.path(test_subdir, "single_raster.tif")
  raster_data <- terra::rast(
    nrows = 2,
    ncols = 2,
    xmin = 0,
    xmax = 2,
    ymin = 0,
    ymax = 2,
    crs = "EPSG:4326"
  )
  terra::values(raster_data) <- c(1, 2, 3, 4)
  terra::writeRaster(raster_data, raster_file, overwrite = TRUE)

  # create mock shapefile
  polygon <- sf::st_polygon(list(rbind(
    c(0, 0),
    c(2, 0),
    c(2, 2),
    c(0, 2),
    c(0, 0)
  )))

  mock_shapefile <- sf::st_sf(
    adm0 = "test_country",
    adm1 = "test_district",
    adm2 = "test_subdistrict",
    geometry = sf::st_sfc(polygon, crs = 4326)
  )

  # mock process_raster_with_boundaries
  mock_process_function <- function(
    raster_file,
    shapefile,
    id_cols,
    aggregations,
    raster_is_density,
    layer_to_process
  ) {
    data.frame(
      file_name = basename(raster_file),
      adm0 = shapefile$adm0[1],
      adm1 = shapefile$adm1[1],
      adm2 = shapefile$adm2[1],
      year = 2022,
      mean = 25
    )
  }

  mockery::stub(
    process_raster_collection,
    "process_raster_with_boundaries",
    mock_process_function
  )

  # test single file processing (should not call clean_filenames)
  result <- process_raster_collection(
    directory = test_subdir,
    shapefile = mock_shapefile
  )

  # check result structure
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_equal(nrow(result), 1)
  testthat::expect_equal(result$file_name, "single_raster.tif")

  # cleanup
  unlink(test_subdir, recursive = TRUE)
})

testthat::test_that("process_raster_collection handles empty directory", {
  # create empty temporary directory
  temp_dir <- tempdir()
  empty_dir <- file.path(temp_dir, "empty_rasters")
  dir.create(empty_dir, showWarnings = FALSE)

  # create mock shapefile
  polygon <- sf::st_polygon(list(rbind(
    c(0, 0),
    c(1, 0),
    c(1, 1),
    c(0, 1),
    c(0, 0)
  )))

  mock_shapefile <- sf::st_sf(
    adm0 = "test_country",
    geometry = sf::st_sfc(polygon, crs = 4326)
  )

  # test empty directory error
  testthat::expect_error(
    process_raster_collection(
      directory = empty_dir,
      shapefile = mock_shapefile
    ),
    "No files matching pattern found"
  )

  # cleanup
  unlink(empty_dir, recursive = TRUE)
})

testthat::test_that("process_raster_collection respects file pattern", {
  # create temporary directory with mixed file types
  temp_dir <- tempdir()
  test_subdir <- file.path(temp_dir, "mixed_files")
  dir.create(test_subdir, showWarnings = FALSE)

  # create files with different extensions
  tif_file <- file.path(test_subdir, "data.tif")
  txt_file <- file.path(test_subdir, "readme.txt")
  nc_file <- file.path(test_subdir, "data.nc")

  # create actual tif file
  raster_data <- terra::rast(
    nrows = 2,
    ncols = 2,
    xmin = 0,
    xmax = 2,
    ymin = 0,
    ymax = 2,
    crs = "EPSG:4326"
  )
  terra::values(raster_data) <- c(1, 2, 3, 4)
  terra::writeRaster(raster_data, tif_file, overwrite = TRUE)

  # create dummy non-raster files
  writeLines("readme content", txt_file)
  writeLines("netcdf content", nc_file)

  # create mock shapefile
  polygon <- sf::st_polygon(list(rbind(
    c(0, 0),
    c(2, 0),
    c(2, 2),
    c(0, 2),
    c(0, 0)
  )))

  mock_shapefile <- sf::st_sf(
    adm0 = "test_country",
    adm1 = "test_district",
    adm2 = "test_subdistrict",
    geometry = sf::st_sfc(polygon, crs = 4326)
  )

  # mock process_raster_with_boundaries
  mock_process_function <- function(
    raster_file,
    shapefile,
    id_cols,
    aggregations,
    raster_is_density,
    layer_to_process
  ) {
    data.frame(
      file_name = basename(raster_file),
      adm0 = shapefile$adm0[1],
      adm1 = shapefile$adm1[1],
      adm2 = shapefile$adm2[1],
      year = 2022,
      mean = 15
    )
  }

  mockery::stub(
    process_raster_collection,
    "process_raster_with_boundaries",
    mock_process_function
  )

  # test default .tif pattern
  result <- process_raster_collection(
    directory = test_subdir,
    shapefile = mock_shapefile
  )

  # should only process .tif file
  testthat::expect_equal(nrow(result), 1)
  testthat::expect_equal(result$file_name, "data.tif")

  # cleanup
  unlink(test_subdir, recursive = TRUE)
})

testthat::test_that("process_raster_collection handles multiple aggregations", {
  # create temporary directory with raster files
  temp_dir <- tempdir()
  test_subdir <- file.path(temp_dir, "multi_agg")
  dir.create(test_subdir, showWarnings = FALSE)

  # create two mock raster files
  raster_files <- c(
    file.path(test_subdir, "raster1.tif"),
    file.path(test_subdir, "raster2.tif")
  )

  for (i in seq_along(raster_files)) {
    raster_data <- terra::rast(
      nrows = 2,
      ncols = 2,
      xmin = 0,
      xmax = 2,
      ymin = 0,
      ymax = 2,
      crs = "EPSG:4326"
    )
    terra::values(raster_data) <- rep(i * 5, 4)
    terra::writeRaster(raster_data, raster_files[i], overwrite = TRUE)
  }

  # create mock shapefile
  polygon <- sf::st_polygon(list(rbind(
    c(0, 0),
    c(2, 0),
    c(2, 2),
    c(0, 2),
    c(0, 0)
  )))

  mock_shapefile <- sf::st_sf(
    adm0 = "test_country",
    adm1 = "test_district",
    adm2 = "test_subdistrict",
    geometry = sf::st_sfc(polygon, crs = 4326)
  )

  # mock helper functions
  mockery::stub(
    process_raster_collection,
    "clean_filenames",
    raster_files
  )

  # mock process_raster_with_boundaries with multiple aggregations
  mock_process_function <- function(
    raster_file,
    shapefile,
    id_cols,
    aggregations,
    raster_is_density,
    layer_to_process
  ) {
    file_num <- ifelse(grepl("raster1", raster_file), 1, 2)

    data.frame(
      file_name = basename(raster_file),
      adm0 = shapefile$adm0[1],
      adm1 = shapefile$adm1[1],
      adm2 = shapefile$adm2[1],
      year = 2022,
      mean = file_num * 5,
      sum = file_num * 20,
      median = file_num * 5
    )
  }

  mockery::stub(
    process_raster_collection,
    "process_raster_with_boundaries",
    mock_process_function
  )

  # test multiple aggregations
  result <- process_raster_collection(
    directory = test_subdir,
    shapefile = mock_shapefile,
    aggregations = c("mean", "sum", "median")
  )

  # check multiple aggregation columns
  testthat::expect_true(all(c("mean", "sum", "median") %in% names(result)))
  testthat::expect_equal(nrow(result), 2)
  testthat::expect_equal(result$mean, c(5, 10))
  testthat::expect_equal(result$sum, c(20, 40))
  testthat::expect_equal(result$median, c(5, 10))

  # cleanup
  unlink(test_subdir, recursive = TRUE)
})

testthat::test_that("process_raster_collection sorts results correctly", {
  # create temporary directory with files in non-chronological order
  temp_dir <- tempdir()
  test_subdir <- file.path(temp_dir, "sort_test")
  dir.create(test_subdir, showWarnings = FALSE)

  # create files with dates out of order
  raster_files <- c(
    file.path(test_subdir, "data_2022_03.tif"),
    file.path(test_subdir, "data_2022_01.tif"),
    file.path(test_subdir, "data_2022_02.tif")
  )

  for (i in seq_along(raster_files)) {
    raster_data <- terra::rast(
      nrows = 2,
      ncols = 2,
      xmin = 0,
      xmax = 2,
      ymin = 0,
      ymax = 2,
      crs = "EPSG:4326"
    )
    terra::values(raster_data) <- rep(i, 4)
    terra::writeRaster(raster_data, raster_files[i], overwrite = TRUE)
  }

  # create mock shapefile
  polygon <- sf::st_polygon(list(rbind(
    c(0, 0),
    c(2, 0),
    c(2, 2),
    c(0, 2),
    c(0, 0)
  )))

  mock_shapefile <- sf::st_sf(
    adm0 = "test_country",
    adm1 = "test_district",
    adm2 = "test_subdistrict",
    geometry = sf::st_sfc(polygon, crs = 4326)
  )

  # mock helper functions
  mockery::stub(
    process_raster_collection,
    "clean_filenames",
    raster_files
  )

  # mock process_raster_with_boundaries to return data with time columns
  mock_process_function <- function(
    raster_file,
    shapefile,
    id_cols,
    aggregations,
    raster_is_density,
    layer_to_process
  ) {
    # extract month from filename pattern
    month_num <- as.numeric(gsub(
      ".*_([0-9]{2})\\.tif$",
      "\\1",
      basename(raster_file)
    ))

    data.frame(
      file_name = basename(raster_file),
      adm0 = shapefile$adm0[1],
      adm1 = shapefile$adm1[1],
      adm2 = shapefile$adm2[1],
      year = 2022,
      month = month_num,
      mean = month_num
    )
  }

  mockery::stub(
    process_raster_collection,
    "process_raster_with_boundaries",
    mock_process_function
  )

  # test sorting functionality
  result <- process_raster_collection(
    directory = test_subdir,
    shapefile = mock_shapefile
  )

  # check that results are sorted by year and month
  testthat::expect_equal(result$month, c(1, 2, 3)) # chronological order
  testthat::expect_equal(result$mean, c(1, 2, 3)) # corresponding values

  # cleanup
  unlink(test_subdir, recursive = TRUE)
})

testthat::test_that("process_raster_collection handles custom parameters", {
  # create temporary directory
  temp_dir <- tempdir()
  test_subdir <- file.path(temp_dir, "custom_params")
  dir.create(test_subdir, showWarnings = FALSE)

  # create mock raster file
  raster_file <- file.path(test_subdir, "test_raster.tif")
  raster_data <- terra::rast(
    nrows = 2,
    ncols = 2,
    xmin = 0,
    xmax = 2,
    ymin = 0,
    ymax = 2,
    crs = "EPSG:4326"
  )
  terra::values(raster_data) <- c(1, 2, 3, 4)
  terra::writeRaster(raster_data, raster_file, overwrite = TRUE)

  # create mock shapefile with custom columns
  polygon <- sf::st_polygon(list(rbind(
    c(0, 0),
    c(2, 0),
    c(2, 2),
    c(0, 2),
    c(0, 0)
  )))

  mock_shapefile <- sf::st_sf(
    country_name = "test_country",
    district_name = "test_district",
    geometry = sf::st_sfc(polygon, crs = 4326)
  )

  # mock process_raster_with_boundaries
  mock_process_function <- function(
    raster_file,
    shapefile,
    id_cols,
    aggregations,
    raster_is_density,
    layer_to_process
  ) {
    # verify custom parameters are passed through
    testthat::expect_equal(id_cols, c("country_name", "district_name"))
    testthat::expect_equal(aggregations, c("sum", "median"))
    testthat::expect_true(raster_is_density)
    testthat::expect_equal(layer_to_process, 2)

    data.frame(
      file_name = basename(raster_file),
      country_name = shapefile$country_name[1],
      district_name = shapefile$district_name[1],
      year = 2022,
      sum = 100,
      median = 50
    )
  }

  mockery::stub(
    process_raster_collection,
    "process_raster_with_boundaries",
    mock_process_function
  )

  # test custom parameters
  result <- process_raster_collection(
    directory = test_subdir,
    shapefile = mock_shapefile,
    id_cols = c("country_name", "district_name"),
    aggregations = c("sum", "median"),
    raster_is_density = TRUE,
    layer_to_process = 2
  )

  # check custom columns are included
  testthat::expect_true(all(
    c("country_name", "district_name", "sum", "median") %in% names(result)
  ))
  testthat::expect_equal(result$country_name, "test_country")
  testthat::expect_equal(result$district_name, "test_district")

  # cleanup
  unlink(test_subdir, recursive = TRUE)
})

testthat::test_that("process_raster_collection handles results without time columns", {
  # create temporary directory
  temp_dir <- tempdir()
  test_subdir <- file.path(temp_dir, "no_time")
  dir.create(test_subdir, showWarnings = FALSE)

  # create mock raster file
  raster_file <- file.path(test_subdir, "no_time_raster.tif")
  raster_data <- terra::rast(
    nrows = 2,
    ncols = 2,
    xmin = 0,
    xmax = 2,
    ymin = 0,
    ymax = 2,
    crs = "EPSG:4326"
  )
  terra::values(raster_data) <- c(1, 2, 3, 4)
  terra::writeRaster(raster_data, raster_file, overwrite = TRUE)

  # create mock shapefile
  polygon <- sf::st_polygon(list(rbind(
    c(0, 0),
    c(2, 0),
    c(2, 2),
    c(0, 2),
    c(0, 0)
  )))

  mock_shapefile <- sf::st_sf(
    adm0 = "test_country",
    adm1 = "test_district",
    adm2 = "test_subdistrict",
    geometry = sf::st_sfc(polygon, crs = 4326)
  )

  # mock process_raster_with_boundaries to return data without time columns
  mock_process_function <- function(
    raster_file,
    shapefile,
    id_cols,
    aggregations,
    raster_is_density,
    layer_to_process
  ) {
    data.frame(
      file_name = basename(raster_file),
      adm0 = shapefile$adm0[1],
      adm1 = shapefile$adm1[1],
      adm2 = shapefile$adm2[1],
      mean = 25
    )
  }

  mockery::stub(
    process_raster_collection,
    "process_raster_with_boundaries",
    mock_process_function
  )

  # test without time columns (should not attempt sorting)
  testthat::expect_no_error(
    result <- process_raster_collection(
      directory = test_subdir,
      shapefile = mock_shapefile
    )
  )

  # check result structure
  testthat::expect_false("year" %in% names(result))
  testthat::expect_false("month" %in% names(result))
  testthat::expect_equal(result$mean, 25)

  # cleanup
  unlink(test_subdir, recursive = TRUE)
})








testthat::test_that("process_weighted_raster_collection handles no value files", {
  # create temporary directories
  temp_dir <- tempdir()
  value_dir <- file.path(temp_dir, "empty_values")
  pop_dir <- file.path(temp_dir, "population")
  dir.create(value_dir, showWarnings = FALSE)
  dir.create(pop_dir, showWarnings = FALSE)

  # create only population file
  pop_file <- file.path(pop_dir, "population_2020.tif")
  raster_data <- terra::rast(
    nrows = 2,
    ncols = 2,
    xmin = 0,
    xmax = 2,
    ymin = 0,
    ymax = 2,
    crs = "EPSG:4326"
  )
  terra::values(raster_data) <- c(100, 200, 300, 400)
  terra::writeRaster(raster_data, pop_file, overwrite = TRUE)

  # create mock shapefile
  polygon <- sf::st_polygon(list(rbind(
    c(0, 0),
    c(2, 0),
    c(2, 2),
    c(0, 2),
    c(0, 0)
  )))

  mock_shapefile <- sf::st_sf(
    adm0 = "test_country",
    geometry = sf::st_sfc(polygon, crs = 4326)
  )

  # mock clean_filenames to return empty
  mockery::stub(
    process_weighted_raster_collection,
    "clean_filenames",
    character(0)
  )

  # test empty value directory error
  testthat::expect_error(
    process_weighted_raster_collection(
      value_raster_dir = value_dir,
      pop_raster_dir = pop_dir,
      shapefile = mock_shapefile
    ),
    "No value raster files found"
  )

  # cleanup
  unlink(c(value_dir, pop_dir), recursive = TRUE)
})

testthat::test_that("process_weighted_raster_collection handles no population files", {
  # create temporary directories
  temp_dir <- tempdir()
  value_dir <- file.path(temp_dir, "values")
  pop_dir <- file.path(temp_dir, "empty_pop")
  dir.create(value_dir, showWarnings = FALSE)
  dir.create(pop_dir, showWarnings = FALSE)

  # create only value file
  value_file <- file.path(value_dir, "malnutrition_2020.tiff")
  raster_data <- terra::rast(
    nrows = 2,
    ncols = 2,
    xmin = 0,
    xmax = 2,
    ymin = 0,
    ymax = 2,
    crs = "EPSG:4326"
  )
  terra::values(raster_data) <- c(0.1, 0.2, 0.3, 0.4)
  terra::writeRaster(raster_data, value_file, overwrite = TRUE)

  # create mock shapefile
  polygon <- sf::st_polygon(list(rbind(
    c(0, 0),
    c(2, 0),
    c(2, 2),
    c(0, 2),
    c(0, 0)
  )))

  mock_shapefile <- sf::st_sf(
    adm0 = "test_country",
    geometry = sf::st_sfc(polygon, crs = 4326)
  )

  # mock clean_filenames - first call returns value file, second call empty
  call_count <- 0
  mock_clean_filenames <- function(files, rename_files = TRUE) {
    call_count <<- call_count + 1
    if (call_count == 1) {
      return(files) # return value files
    } else {
      return(character(0)) # return empty for population files
    }
  }

  mockery::stub(
    process_weighted_raster_collection,
    "clean_filenames",
    mock_clean_filenames
  )

  # test empty population directory error
  testthat::expect_error(
    process_weighted_raster_collection(
      value_raster_dir = value_dir,
      pop_raster_dir = pop_dir,
      shapefile = mock_shapefile
    ),
    "No population raster files found"
  )

  # cleanup
  unlink(c(value_dir, pop_dir), recursive = TRUE)
})

testthat::test_that("process_weighted_raster_collection handles unmatched years", {
  # create temporary directories
  temp_dir <- tempdir()
  value_dir <- file.path(temp_dir, "values")
  pop_dir <- file.path(temp_dir, "population")
  dir.create(value_dir, showWarnings = FALSE)
  dir.create(pop_dir, showWarnings = FALSE)

  # create value file for 2020
  value_file <- file.path(value_dir, "malnutrition_2020.tiff")
  raster_data <- terra::rast(
    nrows = 2,
    ncols = 2,
    xmin = 0,
    xmax = 2,
    ymin = 0,
    ymax = 2,
    crs = "EPSG:4326"
  )
  terra::values(raster_data) <- c(0.1, 0.2, 0.3, 0.4)
  terra::writeRaster(raster_data, value_file, overwrite = TRUE)

  # create population file for different year (2021)
  pop_file <- file.path(pop_dir, "population_2021.tif")
  raster_data <- terra::rast(
    nrows = 2,
    ncols = 2,
    xmin = 0,
    xmax = 2,
    ymin = 0,
    ymax = 2,
    crs = "EPSG:4326"
  )
  terra::values(raster_data) <- c(100, 200, 300, 400)
  terra::writeRaster(raster_data, pop_file, overwrite = TRUE)

  # create mock shapefile
  polygon <- sf::st_polygon(list(rbind(
    c(0, 0),
    c(2, 0),
    c(2, 2),
    c(0, 2),
    c(0, 0)
  )))

  mock_shapefile <- sf::st_sf(
    adm0 = "test_country",
    adm1 = "test_district",
    adm2 = "test_subdistrict",
    geometry = sf::st_sfc(polygon, crs = 4326)
  )

  # mock helper functions
  mockery::stub(
    process_weighted_raster_collection,
    "clean_filenames",
    function(files, rename_files = TRUE) files
  )

  mockery::stub(
    process_weighted_raster_collection,
    "detect_time_pattern",
    list(pattern = "yearly")
  )

  mockery::stub(
    process_weighted_raster_collection,
    "extract_time_components",
    list(date = as.Date("2020-01-01"), year = 2020)
  )

  # test unmatched year error
  testthat::expect_error(
    process_weighted_raster_collection(
      value_raster_dir = value_dir,
      pop_raster_dir = pop_dir,
      shapefile = mock_shapefile
    ),
    "No population raster matching year 2020 found"
  )

  # cleanup
  unlink(c(value_dir, pop_dir), recursive = TRUE)
})

testthat::test_that("process_weighted_raster_collection handles multiple population matches", {
  # create temporary directories
  temp_dir <- tempdir()
  value_dir <- file.path(temp_dir, "values")
  pop_dir <- file.path(temp_dir, "population")
  dir.create(value_dir, showWarnings = FALSE)
  dir.create(pop_dir, showWarnings = FALSE)

  # create value file for 2020
  value_file <- file.path(value_dir, "malnutrition_2020.tiff")
  raster_data <- terra::rast(
    nrows = 2,
    ncols = 2,
    xmin = 0,
    xmax = 2,
    ymin = 0,
    ymax = 2,
    crs = "EPSG:4326"
  )
  terra::values(raster_data) <- c(0.1, 0.2, 0.3, 0.4)
  terra::writeRaster(raster_data, value_file, overwrite = TRUE)

  # create multiple population files for 2020
  pop_files <- c(
    file.path(pop_dir, "population_2020_v1.tif"),
    file.path(pop_dir, "population_2020_v2.tif")
  )

  for (pop_file in pop_files) {
    raster_data <- terra::rast(
      nrows = 2,
      ncols = 2,
      xmin = 0,
      xmax = 2,
      ymin = 0,
      ymax = 2,
      crs = "EPSG:4326"
    )
    terra::values(raster_data) <- c(100, 200, 300, 400)
    terra::writeRaster(raster_data, pop_file, overwrite = TRUE)
  }

  # create mock shapefile
  polygon <- sf::st_polygon(list(rbind(
    c(0, 0),
    c(2, 0),
    c(2, 2),
    c(0, 2),
    c(0, 0)
  )))

  mock_shapefile <- sf::st_sf(
    adm0 = "test_country",
    adm1 = "test_district",
    adm2 = "test_subdistrict",
    geometry = sf::st_sfc(polygon, crs = 4326)
  )

  # mock helper functions
  mockery::stub(
    process_weighted_raster_collection,
    "clean_filenames",
    function(files, rename_files = TRUE) files
  )

  mockery::stub(
    process_weighted_raster_collection,
    "detect_time_pattern",
    list(pattern = "yearly")
  )

  mockery::stub(
    process_weighted_raster_collection,
    "extract_time_components",
    list(date = as.Date("2020-01-01"), year = 2020)
  )

  # test multiple matches error
  testthat::expect_error(
    process_weighted_raster_collection(
      value_raster_dir = value_dir,
      pop_raster_dir = pop_dir,
      shapefile = mock_shapefile
    ),
    "Multiple population rasters found for year 2020"
  )

  # cleanup
  unlink(c(value_dir, pop_dir), recursive = TRUE)
})

testthat::test_that("process_ihme_u5m_raster works with rates using mean", {
  # create mock raster stack with 3 years (2000-2002)
  raster_layer1 <- terra::rast(
    nrows = 4,
    ncols = 4,
    xmin = 0,
    xmax = 4,
    ymin = 0,
    ymax = 4,
    crs = "EPSG:4326"
  )
  terra::values(raster_layer1) <- runif(16, min = 10, max = 50) # u5m rates

  raster_layer2 <- terra::rast(
    nrows = 4,
    ncols = 4,
    xmin = 0,
    xmax = 4,
    ymin = 0,
    ymax = 4,
    crs = "EPSG:4326"
  )
  terra::values(raster_layer2) <- runif(16, min = 12, max = 48)

  raster_layer3 <- terra::rast(
    nrows = 4,
    ncols = 4,
    xmin = 0,
    xmax = 4,
    ymin = 0,
    ymax = 4,
    crs = "EPSG:4326"
  )
  terra::values(raster_layer3) <- runif(16, min = 8, max = 45)

  raster_stack <- c(raster_layer1, raster_layer2, raster_layer3)

  # create mock shapefile with two polygons
  polygon1 <- sf::st_polygon(list(rbind(
    c(0, 0),
    c(2, 0),
    c(2, 2),
    c(0, 2),
    c(0, 0)
  )))
  polygon2 <- sf::st_polygon(list(rbind(
    c(2, 2),
    c(4, 2),
    c(4, 4),
    c(2, 4),
    c(2, 2)
  )))

  mock_shape <- sf::st_sf(
    adm1 = c("region_a", "region_b"),
    adm2 = c("district_1", "district_2"),
    geometry = sf::st_sfc(polygon1, polygon2, crs = 4326)
  )

  # test rates with mean (default)
  result <- process_ihme_u5m_raster(
    shape = mock_shape,
    raster_stack = raster_stack,
    id_col = c("adm1", "adm2"),
    rates = TRUE,
    stat_type = "mean"
  )

  # check result structure
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_equal(nrow(result), 6) # 2 regions × 3 years
  testthat::expect_true(all(
    c("adm1", "adm2", "year", "u5m_rate") %in% names(result)
  ))

  # check years are correct (2000-2002)
  testthat::expect_equal(unique(result$year), c(2000, 2001, 2002))

  # check all values are numeric and not all NA
  testthat::expect_true(is.numeric(result$u5m_rate))
  testthat::expect_true(any(!is.na(result$u5m_rate)))

  # check regions are preserved
  testthat::expect_equal(unique(result$adm1), c("region_a", "region_b"))
  testthat::expect_equal(unique(result$adm2), c("district_1", "district_2"))
})

testthat::test_that("process_ihme_u5m_raster works with rates using median", {
  # create simple 2x2 raster with known values
  raster_layer <- terra::rast(
    nrows = 2,
    ncols = 2,
    xmin = 0,
    xmax = 2,
    ymin = 0,
    ymax = 2,
    crs = "EPSG:4326"
  )
  terra::values(raster_layer) <- c(10, 20, 30, 40)

  # create single polygon covering entire raster
  polygon <- sf::st_polygon(list(rbind(
    c(0, 0),
    c(2, 0),
    c(2, 2),
    c(0, 2),
    c(0, 0)
  )))

  mock_shape <- sf::st_sf(
    adm1 = "test_region",
    adm2 = "test_district",
    geometry = sf::st_sfc(polygon, crs = 4326)
  )

  # test rates with median
  result <- process_ihme_u5m_raster(
    shape = mock_shape,
    raster_stack = raster_layer,
    id_col = c("adm1", "adm2"),
    rates = TRUE,
    stat_type = "median"
  )

  # check result structure
  testthat::expect_equal(nrow(result), 1)
  testthat::expect_true("u5m_rate" %in% names(result))
  testthat::expect_equal(result$year, 2000)
  testthat::expect_true(is.numeric(result$u5m_rate))
})

testthat::test_that("process_ihme_u5m_raster works with rates using both stats", {
  # create simple raster
  raster_layer <- terra::rast(
    nrows = 2,
    ncols = 2,
    xmin = 0,
    xmax = 2,
    ymin = 0,
    ymax = 2,
    crs = "EPSG:4326"
  )
  terra::values(raster_layer) <- c(15, 25, 35, 45)

  # create polygon
  polygon <- sf::st_polygon(list(rbind(
    c(0, 0),
    c(2, 0),
    c(2, 2),
    c(0, 2),
    c(0, 0)
  )))

  mock_shape <- sf::st_sf(
    adm1 = "test_region",
    adm2 = "test_district",
    geometry = sf::st_sfc(polygon, crs = 4326)
  )

  # test rates with both statistics
  result <- process_ihme_u5m_raster(
    shape = mock_shape,
    raster_stack = raster_layer,
    id_col = c("adm1", "adm2"),
    rates = TRUE,
    stat_type = "both"
  )

  # check both columns are present
  testthat::expect_true(all(
    c("u5m_rate_mean", "u5m_rate_median") %in% names(result)
  ))
  testthat::expect_true(is.numeric(result$u5m_rate_mean))
  testthat::expect_true(is.numeric(result$u5m_rate_median))
})

testthat::test_that("process_ihme_u5m_raster works with counts", {
  # create raster with count data
  raster_layer <- terra::rast(
    nrows = 2,
    ncols = 2,
    xmin = 0,
    xmax = 2,
    ymin = 0,
    ymax = 2,
    crs = "EPSG:4326"
  )
  terra::values(raster_layer) <- c(5, 10, 15, 20) # death counts

  # create polygon
  polygon <- sf::st_polygon(list(rbind(
    c(0, 0),
    c(2, 0),
    c(2, 2),
    c(0, 2),
    c(0, 0)
  )))

  mock_shape <- sf::st_sf(
    adm1 = "test_region",
    adm2 = "test_district",
    geometry = sf::st_sfc(polygon, crs = 4326)
  )

  # test counts (should use sum regardless of stat_type)
  testthat::expect_warning(
    result <- process_ihme_u5m_raster(
      shape = mock_shape,
      raster_stack = raster_layer,
      id_col = c("adm1", "adm2"),
      rates = FALSE,
      stat_type = "median" # should be ignored with warning
    ),
    "stat_type is ignored when rates = FALSE"
  )

  # check result structure
  testthat::expect_true("u5m_count" %in% names(result))
  testthat::expect_true(is.numeric(result$u5m_count))
  testthat::expect_equal(result$u5m_count, 50) # sum of all values
})

testthat::test_that("process_ihme_u5m_raster validates inputs correctly", {
  # create minimal raster
  raster_layer <- terra::rast(
    nrows = 2,
    ncols = 2,
    xmin = 0,
    xmax = 2,
    ymin = 0,
    ymax = 2,
    crs = "EPSG:4326"
  )
  terra::values(raster_layer) <- c(1, 2, 3, 4)

  # create minimal shapefile
  polygon <- sf::st_polygon(list(rbind(
    c(0, 0),
    c(2, 0),
    c(2, 2),
    c(0, 2),
    c(0, 0)
  )))

  mock_shape <- sf::st_sf(
    adm1 = "test_region",
    geometry = sf::st_sfc(polygon, crs = 4326)
  )

  # test invalid stat_type
  testthat::expect_error(
    process_ihme_u5m_raster(
      shape = mock_shape,
      raster_stack = raster_layer,
      stat_type = "invalid"
    ),
    "stat_type must be 'mean', 'median', or 'both'"
  )

  # test missing id_col
  testthat::expect_error(
    process_ihme_u5m_raster(
      shape = mock_shape,
      raster_stack = raster_layer,
      id_col = c("missing_column")
    )
  )
})

testthat::test_that("process_ihme_u5m_raster handles custom id columns", {
  # create raster
  raster_layer <- terra::rast(
    nrows = 2,
    ncols = 2,
    xmin = 0,
    xmax = 2,
    ymin = 0,
    ymax = 2,
    crs = "EPSG:4326"
  )
  terra::values(raster_layer) <- c(10, 20, 30, 40)

  # create shapefile with custom columns
  polygon <- sf::st_polygon(list(rbind(
    c(0, 0),
    c(2, 0),
    c(2, 2),
    c(0, 2),
    c(0, 0)
  )))

  mock_shape <- sf::st_sf(
    country_name = "test_country",
    region_code = "TR01",
    population = 50000,
    geometry = sf::st_sfc(polygon, crs = 4326)
  )

  # test custom id columns
  result <- process_ihme_u5m_raster(
    shape = mock_shape,
    raster_stack = raster_layer,
    id_col = c("country_name", "region_code", "population")
  )

  # check custom columns are included
  testthat::expect_true(all(
    c("country_name", "region_code", "population") %in% names(result)
  ))
  testthat::expect_equal(result$country_name, "test_country")
  testthat::expect_equal(result$region_code, "TR01")
  testthat::expect_equal(result$population, 50000)
})

# ============================================================================
# tests for process_weighted_raster_stacks
# ============================================================================

testthat::test_that("process_weighted_raster_stacks works with mean", {
  # create value raster stack (3 layers)
  value_layer1 <- terra::rast(
    nrows = 3,
    ncols = 3,
    xmin = 0,
    xmax = 3,
    ymin = 0,
    ymax = 3,
    crs = "EPSG:4326"
  )
  terra::values(value_layer1) <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)

  value_layer2 <- terra::rast(
    nrows = 3,
    ncols = 3,
    xmin = 0,
    xmax = 3,
    ymin = 0,
    ymax = 3,
    crs = "EPSG:4326"
  )
  terra::values(value_layer2) <- c(
    0.15,
    0.25,
    0.35,
    0.45,
    0.55,
    0.65,
    0.75,
    0.85,
    0.95
  )

  value_layer3 <- terra::rast(
    nrows = 3,
    ncols = 3,
    xmin = 0,
    xmax = 3,
    ymin = 0,
    ymax = 3,
    crs = "EPSG:4326"
  )
  terra::values(value_layer3) <- c(0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)

  value_stack <- c(value_layer1, value_layer2, value_layer3)

  # create population weight raster stack
  pop_layer1 <- terra::rast(
    nrows = 3,
    ncols = 3,
    xmin = 0,
    xmax = 3,
    ymin = 0,
    ymax = 3,
    crs = "EPSG:4326"
  )
  terra::values(pop_layer1) <- c(100, 200, 300, 400, 500, 600, 700, 800, 900)

  pop_layer2 <- terra::rast(
    nrows = 3,
    ncols = 3,
    xmin = 0,
    xmax = 3,
    ymin = 0,
    ymax = 3,
    crs = "EPSG:4326"
  )
  terra::values(pop_layer2) <- c(110, 210, 310, 410, 510, 610, 710, 810, 910)

  pop_layer3 <- terra::rast(
    nrows = 3,
    ncols = 3,
    xmin = 0,
    xmax = 3,
    ymin = 0,
    ymax = 3,
    crs = "EPSG:4326"
  )
  terra::values(pop_layer3) <- c(120, 220, 320, 420, 520, 620, 720, 820, 920)

  pop_stack <- c(pop_layer1, pop_layer2, pop_layer3)

  # create shapefile with single polygon
  polygon <- sf::st_polygon(list(rbind(
    c(0, 0),
    c(3, 0),
    c(3, 3),
    c(0, 3),
    c(0, 0)
  )))

  mock_shape <- sf::st_sf(
    shape_id = "region_001",
    shape_name = "test_region",
    geometry = sf::st_sfc(polygon, crs = 4326)
  )

  # test weighted mean
  result <- process_weighted_raster_stacks(
    value_raster = value_stack,
    pop_raster = pop_stack,
    shape = mock_shape,
    value_var = "malnutrition_rate",
    start_year = 2000,
    stat_type = "mean"
  )

  # check result structure
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_equal(nrow(result), 3) # 3 years
  testthat::expect_true(all(
    c("shape_id", "shape_name", "year", "malnutrition_rate") %in% names(result)
  ))

  # check years are correct
  testthat::expect_equal(result$year, c(2000, 2001, 2002))

  # check all weighted values are numeric
  testthat::expect_true(is.numeric(result$malnutrition_rate))
  testthat::expect_true(all(!is.na(result$malnutrition_rate)))
})

testthat::test_that("process_weighted_raster_stacks works with median", {
  # create simple value and population rasters
  value_layer <- terra::rast(
    nrows = 2,
    ncols = 2,
    xmin = 0,
    xmax = 2,
    ymin = 0,
    ymax = 2,
    crs = "EPSG:4326"
  )
  terra::values(value_layer) <- c(0.1, 0.3, 0.5, 0.7)

  pop_layer <- terra::rast(
    nrows = 2,
    ncols = 2,
    xmin = 0,
    xmax = 2,
    ymin = 0,
    ymax = 2,
    crs = "EPSG:4326"
  )
  terra::values(pop_layer) <- c(100, 200, 300, 400)

  # create shapefile
  polygon <- sf::st_polygon(list(rbind(
    c(0, 0),
    c(2, 0),
    c(2, 2),
    c(0, 2),
    c(0, 0)
  )))

  mock_shape <- sf::st_sf(
    shape_id = "test_001",
    geometry = sf::st_sfc(polygon, crs = 4326)
  )

  # test weighted median
  result <- process_weighted_raster_stacks(
    value_raster = value_layer,
    pop_raster = pop_layer,
    shape = mock_shape,
    value_var = "indicator_weighted",
    start_year = 2020,
    stat_type = "median"
  )

  # check result structure
  testthat::expect_equal(nrow(result), 1)
  testthat::expect_true("indicator_weighted" %in% names(result))
  testthat::expect_equal(result$year, 2020)
  testthat::expect_true(is.numeric(result$indicator_weighted))
})

testthat::test_that("process_weighted_raster_stacks works with both stats", {
  # create simple rasters
  value_layer <- terra::rast(
    nrows = 2,
    ncols = 2,
    xmin = 0,
    xmax = 2,
    ymin = 0,
    ymax = 2,
    crs = "EPSG:4326"
  )
  terra::values(value_layer) <- c(0.2, 0.4, 0.6, 0.8)

  pop_layer <- terra::rast(
    nrows = 2,
    ncols = 2,
    xmin = 0,
    xmax = 2,
    ymin = 0,
    ymax = 2,
    crs = "EPSG:4326"
  )
  terra::values(pop_layer) <- c(50, 150, 250, 350)

  # create shapefile
  polygon <- sf::st_polygon(list(rbind(
    c(0, 0),
    c(2, 0),
    c(2, 2),
    c(0, 2),
    c(0, 0)
  )))

  mock_shape <- sf::st_sf(
    shape_id = "test_001",
    geometry = sf::st_sfc(polygon, crs = 4326)
  )

  # test both statistics
  result <- process_weighted_raster_stacks(
    value_raster = value_layer,
    pop_raster = pop_layer,
    shape = mock_shape,
    value_var = "test_indicator",
    start_year = 2015,
    stat_type = "both"
  )

  # check both columns are present
  testthat::expect_true(all(
    c("test_indicator_mean", "test_indicator_median") %in% names(result)
  ))
  testthat::expect_true(is.numeric(result$test_indicator_mean))
  testthat::expect_true(is.numeric(result$test_indicator_median))
  testthat::expect_equal(result$year, 2015)
})

testthat::test_that("process_weighted_raster_stacks validates inputs correctly", {
  # create minimal valid inputs
  value_layer <- terra::rast(
    nrows = 2,
    ncols = 2,
    xmin = 0,
    xmax = 2,
    ymin = 0,
    ymax = 2,
    crs = "EPSG:4326"
  )
  terra::values(value_layer) <- c(1, 2, 3, 4)

  pop_layer <- terra::rast(
    nrows = 2,
    ncols = 2,
    xmin = 0,
    xmax = 2,
    ymin = 0,
    ymax = 2,
    crs = "EPSG:4326"
  )
  terra::values(pop_layer) <- c(10, 20, 30, 40)

  polygon <- sf::st_polygon(list(rbind(
    c(0, 0),
    c(2, 0),
    c(2, 2),
    c(0, 2),
    c(0, 0)
  )))

  mock_shape <- sf::st_sf(
    shape_id = "test_001",
    geometry = sf::st_sfc(polygon, crs = 4326)
  )

  # test non-SpatRaster value_raster
  testthat::expect_error(
    process_weighted_raster_stacks(
      value_raster = "not_a_raster",
      pop_raster = pop_layer,
      shape = mock_shape
    ),
    "value_raster and pop_raster must be SpatRaster objects"
  )

  # test non-sf shape
  testthat::expect_error(
    process_weighted_raster_stacks(
      value_raster = value_layer,
      pop_raster = pop_layer,
      shape = "not_sf"
    ),
    "shape must be an sf object"
  )

  # test mismatched layer counts
  pop_stack <- c(pop_layer, pop_layer) # 2 layers vs 1 in value_layer
  testthat::expect_error(
    process_weighted_raster_stacks(
      value_raster = value_layer,
      pop_raster = pop_stack,
      shape = mock_shape
    ),
    "value_raster and pop_raster must have the same number of layers"
  )

  # test invalid stat_type
  testthat::expect_error(
    process_weighted_raster_stacks(
      value_raster = value_layer,
      pop_raster = pop_layer,
      shape = mock_shape,
      stat_type = "invalid"
    ),
    "stat_type must be 'mean', 'median', or 'both'"
  )
})

testthat::test_that("process_weighted_raster_stacks handles multiple shapes", {
  # create value and population rasters
  value_layer <- terra::rast(
    nrows = 4,
    ncols = 4,
    xmin = 0,
    xmax = 4,
    ymin = 0,
    ymax = 4,
    crs = "EPSG:4326"
  )
  terra::values(value_layer) <- seq(0.1, 1.6, length.out = 16)

  pop_layer <- terra::rast(
    nrows = 4,
    ncols = 4,
    xmin = 0,
    xmax = 4,
    ymin = 0,
    ymax = 4,
    crs = "EPSG:4326"
  )
  terra::values(pop_layer) <- seq(100, 1600, length.out = 16)

  # create shapefile with multiple polygons
  polygon1 <- sf::st_polygon(list(rbind(
    c(0, 0),
    c(2, 0),
    c(2, 2),
    c(0, 2),
    c(0, 0)
  )))
  polygon2 <- sf::st_polygon(list(rbind(
    c(2, 2),
    c(4, 2),
    c(4, 4),
    c(2, 4),
    c(2, 2)
  )))

  mock_shape <- sf::st_sf(
    shape_id = c("region_001", "region_002"),
    shape_name = c("north", "south"),
    geometry = sf::st_sfc(polygon1, polygon2, crs = 4326)
  )

  # test multiple shapes
  result <- process_weighted_raster_stacks(
    value_raster = value_layer,
    pop_raster = pop_layer,
    shape = mock_shape,
    value_var = "indicator",
    start_year = 2010
  )

  # check result structure with multiple shapes
  testthat::expect_equal(nrow(result), 2) # 2 shapes × 1 year
  testthat::expect_equal(unique(result$shape_id), c("region_001", "region_002"))
  testthat::expect_equal(unique(result$shape_name), c("north", "south"))
  testthat::expect_true(all(is.numeric(result$indicator)))
})
