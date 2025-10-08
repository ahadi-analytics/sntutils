# setup test environment ----
skip_if_no_internet <- function() {
  # helper to skip tests if no internet connection
  testthat::skip_if_not(curl::has_internet(), "No internet connection")
}

skip_if_api_down <- function() {
  # helper to skip if dhs api is down
  testthat::skip_if_not(
    {
      tryCatch(
        {
          response <- httr2::request(
            "https://api.dhsprogram.com/rest/dhs/countries"
          ) |>
            httr2::req_timeout(10) |>
            httr2::req_perform()
          !httr2::resp_is_error(response)
        },
        error = function(e) FALSE
      )
    },
    "DHS API is not accessible"
  )
}

# test check_dhs_indicators function ----
testthat::test_that("check_dhs_indicators returns data frame with basic call", {
  skip_if_no_internet()
  skip_if_api_down()

  result <- check_dhs_indicators(
    countryIds = "EG",
    surveyYear = 2014,
    perPage = 10
  )

  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_gt(nrow(result), 0)

  # check expected columns exist
  expected_cols <- c("IndicatorId", "Label", "Definition", "MeasurementType")
  testthat::expect_true(all(expected_cols %in% names(result)))
})



testthat::test_that("check_dhs_indicators handles year ranges", {
  skip_if_no_internet()
  skip_if_api_down()

  result <- check_dhs_indicators(
    countryIds = "KE",
    surveyYearStart = 2010,
    surveyYearEnd = 2020,
    perPage = 10
  )

  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_gte(nrow(result), 0) # may be 0 if no surveys in range
})


testthat::test_that("check_dhs_indicators handles custom return fields", {
  skip_if_no_internet()
  skip_if_api_down()

  custom_fields <- c("IndicatorId", "Label")

  result <- check_dhs_indicators(
    countryIds = "UG",
    surveyYear = 2016,
    returnFields = custom_fields,
    perPage = 5
  )

  testthat::expect_s3_class(result, "data.frame")

  if (nrow(result) > 0) {
    # should contain requested fields
    testthat::expect_true(all(custom_fields %in% names(result)))
  }
})

testthat::test_that("check_dhs_indicators handles pagination", {
  skip_if_no_internet()
  skip_if_api_down()

  # test first page
  result_page1 <- check_dhs_indicators(
    countryIds = "NG",
    perPage = 5,
    page = 1
  )

  testthat::expect_s3_class(result_page1, "data.frame")

  if (nrow(result_page1) >= 5) {
    # test second page
    result_page2 <- check_dhs_indicators(
      countryIds = "NG",
      perPage = 5,
      page = 2
    )

    testthat::expect_s3_class(result_page2, "data.frame")

    # pages should be different if there are enough results
    if (nrow(result_page2) > 0) {
      testthat::expect_false(identical(result_page1, result_page2))
    }
  }
})

testthat::test_that("check_dhs_indicators handles survey types", {
  skip_if_no_internet()
  skip_if_api_down()

  result <- check_dhs_indicators(
    countryIds = "TZ",
    surveyType = "DHS",
    perPage = 10
  )

  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_gte(nrow(result), 0)
})

# test download_dhs_indicators function ----
testthat::test_that("download_dhs_indicators returns data with basic call", {
  skip_if_no_internet()
  skip_if_api_down()

  # test with well-known malaria indicator
  result <- download_dhs_indicators(
    countryIds = "SL",
    indicatorIds = "FE_FRTR_W_TF4",
    breakdown = "national"
  )

  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_gte(nrow(result), 0)

  if (nrow(result) > 0) {
    # check for expected data columns
    expected_data_cols <- c("Indicator", "CountryName", "SurveyYear", "Value")
    available_cols <- intersect(expected_data_cols, names(result))
    testthat::expect_gt(length(available_cols), 0)
  }
})



testthat::test_that("download_dhs_indicators handles different breakdowns", {
  skip_if_no_internet()
  skip_if_api_down()

  # test national breakdown
  result_national <- download_dhs_indicators(
    countryIds = "UG",
    indicatorIds = "FE_FRTR_W_TF4",
    breakdown = "national"
  )

  testthat::expect_s3_class(result_national, "data.frame")

  # test subnational breakdown
  result_subnational <- download_dhs_indicators(
    countryIds = "UG",
    indicatorIds = "FE_FRTR_W_TF4",
    breakdown = "subnational"
  )

  testthat::expect_s3_class(result_subnational, "data.frame")

  # subnational should typically have more rows than national
  if (nrow(result_national) > 0 && nrow(result_subnational) > 0) {
    testthat::expect_gte(nrow(result_subnational), nrow(result_national))
  }
})

testthat::test_that("download_dhs_indicators handles year filters", {
  skip_if_no_internet()
  skip_if_api_down()

  # test specific year
  result_year <- download_dhs_indicators(
    countryIds = "GH",
    indicatorIds = "FE_FRTR_W_TF4",
    surveyYear = "2014",
    breakdown = "national"
  )

  testthat::expect_s3_class(result_year, "data.frame")

  if (nrow(result_year) > 0 && "SurveyYear" %in% names(result_year)) {
    testthat::expect_true(all(result_year$SurveyYear == 2014))
  }

  # test year range
  result_range <- download_dhs_indicators(
    countryIds = "GH",
    indicatorIds = "FE_FRTR_W_TF4",
    surveyYearStart = "2010",
    surveyYearEnd = "2020",
    breakdown = "national"
  )

  testthat::expect_s3_class(result_range, "data.frame")

  if (nrow(result_range) > 0 && "SurveyYear" %in% names(result_range)) {
    testthat::expect_true(all(result_range$SurveyYear >= 2010))
    testthat::expect_true(all(result_range$SurveyYear <= 2020))
  }
})

testthat::test_that("download_dhs_indicators handles survey ids", {
  skip_if_no_internet()
  skip_if_api_down()

  result <- download_dhs_indicators(
    countryIds = "SL",
    indicatorIds = "FE_FRTR_W_TF4",
    surveyIds = "SL2016MIS",
    breakdown = "national"
  )

  testthat::expect_s3_class(result, "data.frame")

  if (nrow(result) > 0 && "SurveyId" %in% names(result)) {
    testthat::expect_true(all(stringr::str_detect(result$SurveyId, "SL2016")))
  }
})

# test error handling ----
testthat::test_that("functions handle invalid country codes", {
  skip_if_no_internet()
  skip_if_api_down()

  # invalid country code should return empty or error gracefully
    result <- check_dhs_indicators(
      countryIds = "INVALID",
      perPage = 5)

    testthat::expect_equal(nrow(result), NULL)
})

testthat::test_that("functions handle invalid indicator codes", {
  skip_if_no_internet()
  skip_if_api_down()

    result <- download_dhs_indicators(
      countryIds = "KE",
      indicatorIds = "INVALID_INDICATOR",
      breakdown = "national"
    )
    testthat::expect_equal(nrow(result), NULL)
  })



# test data quality and consistency ----
testthat::test_that("check_dhs_indicators returns consistent data types", {
  skip_if_no_internet()
  skip_if_api_down()

  result <- check_dhs_indicators(
    countryIds = "MW",
    perPage = 10
  )

  if (nrow(result) > 0) {
    # indicator id should be character
    if ("IndicatorId" %in% names(result)) {
      testthat::expect_true(is.character(result$IndicatorId))
    }

    # label should be character
    if ("Label" %in% names(result)) {
      testthat::expect_true(is.character(result$Label))
    }

    # no completely empty rows
    testthat::expect_false(any(apply(result, 1, function(x) all(is.na(x)))))
  }
})

testthat::test_that("download_dhs_indicators returns valid data values", {
  skip_if_no_internet()
  skip_if_api_down()

  result <- download_dhs_indicators(
    countryIds = "RW",
    indicatorIds = "FE_FRTR_W_TF4",
    breakdown = "national"
  )

  if (nrow(result) > 0) {
    # check that numeric values are reasonable for percentages
    if ("Value" %in% names(result)) {
      numeric_values <- result$Value[!is.na(result$Value)]
      if (length(numeric_values) > 0) {
        testthat::expect_true(all(numeric_values >= 0))
        testthat::expect_true(all(numeric_values <= 100)) # assuming percentages
      }
    }

    # survey years should be reasonable
    if ("SurveyYear" %in% names(result)) {
      years <- result$SurveyYear[!is.na(result$SurveyYear)]
      if (length(years) > 0) {
        testthat::expect_true(all(years >= 1990))
        testthat::expect_true(all(
          years <= as.numeric(format(Sys.Date(), "%Y"))
        ))
      }
    }
  }
})

# test performance and rate limiting ----
testthat::test_that("functions complete in reasonable time", {
  skip_if_no_internet()
  skip_if_api_down()

  # time a small request
  start_time <- Sys.time()

  result <- check_dhs_indicators(
    countryIds = "BF",
    perPage = 5
  )

  end_time <- Sys.time()
  execution_time <- as.numeric(difftime(end_time, start_time, units = "secs"))

  # should complete in under 30 seconds for small request
  testthat::expect_lt(execution_time, 30)
  testthat::expect_s3_class(result, "data.frame")
})

testthat::test_that("functions handle large requests appropriately", {
  skip_if_no_internet()
  skip_if_api_down()

  # test larger request but with reasonable limits
  testthat::expect_no_error({
    result <- check_dhs_indicators(
      countryIds = "NG",
      perPage = 100
    )

    testthat::expect_s3_class(result, "data.frame")
    testthat::expect_lte(nrow(result), 100) # should respect perPage limit
  })
})

# integration tests ----
testthat::test_that("functions work together in typical workflow", {
  skip_if_no_internet()
  skip_if_api_down()

  # step 1: check available indicators
  indicators <- check_dhs_indicators(
    countryIds = "ZM",
    surveyYear = 2018,
    perPage = 10
  )

  testthat::expect_s3_class(indicators, "data.frame")

  if (nrow(indicators) > 0 && "IndicatorId" %in% names(indicators)) {
    # step 2: download data for first available indicator
    first_indicator <- indicators$IndicatorId[1]

    data_result <- download_dhs_indicators(
      countryIds = "ZM",
      indicatorIds = first_indicator,
      surveyYear = "2018",
      breakdown = "national"
    )

    testthat::expect_s3_class(data_result, "data.frame")
  }
})

testthat::test_that("functions handle multiple api calls consistently", {
  skip_if_no_internet()
  skip_if_api_down()

  # make same request twice
  request_params <- list(
    countryIds = "CD",
    surveyYear = 2013,
    perPage = 5
  )

  result1 <- do.call(check_dhs_indicators, request_params)
  result2 <- do.call(check_dhs_indicators, request_params)

  testthat::expect_s3_class(result1, "data.frame")
  testthat::expect_s3_class(result2, "data.frame")

  # results should be identical for same request
  if (nrow(result1) > 0 && nrow(result2) > 0) {
    testthat::expect_equal(nrow(result1), nrow(result2))
  }
})
