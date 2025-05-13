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

