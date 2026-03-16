# tests/testthat/test-translate_yearmon.R

testthat::test_that("translate_yearmon works with default parameters", {
  # Use a fixed date to make tests reproducible
  test_date <- as.Date("2023-04-15")

  # Mock the system call to ensure test can run without locale dependencies
  mockery::stub(translate_yearmon, "system", function(...) {
    c("en_US.UTF-8", "fr_FR.UTF-8", "es_ES.UTF-8")
  })

  # Mock format with locale to return predictable values
  mockery::stub(translate_yearmon, "withr::with_locale", function(locale, expr) {
    if (locale[["LC_TIME"]] == "fr_FR.UTF-8") {
      return("avr. 2023")
    } else if (locale[["LC_TIME"]] == "en_US.UTF-8") {
      return("Apr 2023")
    } else {
      return("Apr 2023") # Default fallback
    }
  })

  # Test default (French)
  result_fr <- translate_yearmon(test_date)
  testthat::expect_equal(result_fr, "avr. 2023")

  # Test English with default format
  result_en <- translate_yearmon(test_date, "en")
  testthat::expect_equal(result_en, "Apr 2023")
})

testthat::test_that("translate_yearmon handles different formats", {
  test_date <- as.Date("2023-04-15")

  # Mock the system call
  mockery::stub(translate_yearmon, "system", function(...) {
    c("en_US.UTF-8", "fr_FR.UTF-8")
  })

  # Create mock for with_locale that allows multiple calls (cycle=TRUE)
  fr_full_mock <- mockery::mock("avril 2023", cycle=TRUE)
  fr_abbr_mock <- mockery::mock("avr. 2023", cycle=TRUE)
  en_full_mock <- mockery::mock("April 2023", cycle=TRUE)
  en_abbr_mock <- mockery::mock("Apr 2023", cycle=TRUE)

  # Setup the more complex mock using an anonymous function
  with_locale_mock <- function(locale, expr) {
    if (locale[["LC_TIME"]] == "fr_FR.UTF-8") {
      if (format == "%B %Y") {
        return(fr_full_mock())
      } else {
        return(fr_abbr_mock())
      }
    } else {
      if (format == "%B %Y") {
        return(en_full_mock())
      } else {
        return(en_abbr_mock())
      }
    }
  }

  # Access the format variable from the parent environment
  format <- "%B %Y"
  mockery::stub(translate_yearmon, "withr::with_locale", with_locale_mock)

  # Test French with full month format
  result_fr_full <- translate_yearmon(test_date, "fr", format = "%B %Y")
  testthat::expect_equal(result_fr_full, "avril 2023")

  # Update format for second test
  format <- "%b %Y"

  # Test French with abbreviated month format
  result_fr_abbr <- translate_yearmon(test_date, "fr")
  testthat::expect_equal(result_fr_abbr, "avr. 2023")

  # Update format for full English test
  format <- "%B %Y"

  # Test English with full month format
  result_en_full <- translate_yearmon(test_date, "en", format = "%B %Y")
  testthat::expect_equal(result_en_full, "April 2023")

  # Update format for abbreviated English test
  format <- "%b %Y"

  # Test English with abbreviated month format
  result_en_abbr <- translate_yearmon(test_date, "en")
  testthat::expect_equal(result_en_abbr, "Apr 2023")
})

testthat::test_that("translate_yearmon handles character date input", {
  # Mock the system call
  mockery::stub(translate_yearmon, "system", function(...) {
    c("en_US.UTF-8", "fr_FR.UTF-8")
  })

  # Mock format call
  mockery::stub(translate_yearmon, "withr::with_locale", function(locale, expr) {
    return("avr. 2023")
  })

  # Test with character date
  result <- translate_yearmon("2023-04-15")
  testthat::expect_equal(result, "avr. 2023")
})

testthat::test_that("translate_yearmon falls back when locale not available", {
  # Mock system call to return available locales without the requested one
  mockery::stub(translate_yearmon, "system", function(...) {
    c("en_US.UTF-8") # Only English available
  })

  # Mock warning function to test it's called
  mock_warning <- mockery::mock()
  mockery::stub(translate_yearmon, "warning", mock_warning)

  # Mock Sys.getlocale to return a default
  mockery::stub(translate_yearmon, "Sys.getlocale", function(...) "C")

  # Mock format call
  mockery::stub(translate_yearmon, "withr::with_locale", function(locale, expr) {
    return("Apr 2023")
  })

  # Test with unavailable locale
  result <- suppressWarnings(translate_yearmon(Sys.Date(), "ru"))

  # Expect warning was triggered
  mockery::expect_called(mock_warning, 1)

  # Expect result is still returned
  testthat::expect_type(result, "character")
})

testthat::test_that("translate_yearmon handles all supported languages", {
  # Mock the system call to return all locales
  mockery::stub(translate_yearmon, "system", function(...) {
    c("en_US.UTF-8", "fr_FR.UTF-8", "es_ES.UTF-8", "de_DE.UTF-8",
      "it_IT.UTF-8", "pt_PT.UTF-8", "ru_RU.UTF-8", "zh_CN.UTF-8",
      "ja_JP.UTF-8", "ko_KR.UTF-8", "ar_SA.UTF-8", "hi_IN.UTF-8")
  })

  # Expected month format for each language (for April 2023)
  expected_formats <- list(
    en = "Apr 2023",
    fr = "avr. 2023",
    es = "abr. 2023",
    de = "Apr. 2023",
    it = "apr. 2023",
    pt = "abr. 2023",
    ru = "апр. 2023",
    zh = "4月 2023",
    ja = "4月 2023",
    ko = "4월 2023",
    ar = "أبريل 2023",
    hi = "अप्रैल 2023"
  )

  # Mock format call with different returns based on locale
  mockery::stub(translate_yearmon, "withr::with_locale", function(locale, expr) {
    lang <- substr(locale[["LC_TIME"]], 1, 2)
    if (lang %in% names(expected_formats)) {
      return(expected_formats[[lang]])
    } else {
      return("Apr 2023") # Default fallback
    }
  })

  # Test each language
  test_date <- as.Date("2023-04-15")
  for (lang in names(expected_formats)) {
    result <- translate_yearmon(test_date, lang)
    testthat::expect_equal(result, expected_formats[[lang]])
  }
})

testthat::test_that("preprocess_en_to_fr_acronyms handles Mean to Moyenne", {
  # Test that "mean" is translated to "Moyenne"
  result <- sntutils:::.preprocess_en_to_fr_acronyms("Mean")
  testthat::expect_equal(result, "Moyenne")
  
  # Test case-insensitive
  result <- sntutils:::.preprocess_en_to_fr_acronyms("mean")
  testthat::expect_equal(result, "Moyenne")
  
  result <- sntutils:::.preprocess_en_to_fr_acronyms("MEAN")
  testthat::expect_equal(result, "Moyenne")
  
  # Test within sentences
  result <- sntutils:::.preprocess_en_to_fr_acronyms("The mean value is 5")
  testthat::expect_equal(result, "The Moyenne value is 5")
  
  # Test that it doesn't replace partial matches
  result <- sntutils:::.preprocess_en_to_fr_acronyms("meaning of life")
  testthat::expect_equal(result, "meaning of life")
  
  # Test with Mean in method context
  result <- sntutils:::.preprocess_en_to_fr_acronyms("Mean (+/- 2 SD)")
  testthat::expect_equal(result, "Moyenne (+/- 2 SD)")
})



