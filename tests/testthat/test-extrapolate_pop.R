# tests for extrapolate_pop function
# objective: comprehensive testing of population extrapolation functionality
# author: assistant
# date: 2025-07-10

# setup test data ----
create_pop_test_data <- function(
  years = 2018:2020,
  n_regions = 2,
  n_districts = 2
) {
  # create realistic population test dataset
  set.seed(42)

  base_data <- expand.grid(
    adm0 = "country_x",
    adm1 = paste0("region_", letters[seq_len(n_regions)]),
    adm2 = paste0("district_", seq_len(n_districts)),
    year = years,
    stringsAsFactors = FALSE
  ) |>
    dplyr::mutate(
      adm3 = paste0(adm2, "_subarea"),
      total_pop = sample(1000:5000, size = dplyr::n(), replace = TRUE)
    ) |>
    dplyr::arrange(adm0, adm1, adm2, year)

  return(base_data)
}

# test basic functionality ----
testthat::test_that("extrapolate_pop returns correct structure", {
  test_data <- create_pop_test_data()

  result <- extrapolate_pop(
    data = test_data,
    year_col = "year",
    pop_col = "total_pop",
    group_cols = c("adm0", "adm1", "adm2", "adm3"),
    years_to_extrap = c(2021, 2022),
    multiplier = 1.5
  )

  # check return type
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_s3_class(result, "tbl_df")

  # check that new years are added
  testthat::expect_true(2021 %in% result$year)
  testthat::expect_true(2022 %in% result$year)

  # check that original columns are preserved
  original_cols <- names(test_data)
  testthat::expect_true(all(original_cols %in% names(result)))
})

testthat::test_that("extrapolate_pop handles unnamed years with multiplier", {
  test_data <- create_pop_test_data()

  result <- extrapolate_pop(
    data = test_data,
    year_col = "year",
    pop_col = "total_pop",
    group_cols = c("adm0", "adm1", "adm2", "adm3"),
    years_to_extrap = c(2021, 2022),
    multiplier = 1.5
  )

  # check that extrapolated years have correct population
  extrap_2021 <- result |>
    dplyr::filter(year == 2021) |>
    dplyr::slice_head(n = 1)

  base_2020 <- result |>
    dplyr::filter(
      year == 2020,
      adm0 == extrap_2021$adm0,
      adm1 == extrap_2021$adm1,
      adm2 == extrap_2021$adm2,
      adm3 == extrap_2021$adm3
    )

  expected_pop_2021 <- round(base_2020$total_pop * 1.5)
  testthat::expect_equal(extrap_2021$total_pop, expected_pop_2021)
})

testthat::test_that("extrapolate_pop handles named years with multipliers", {
  test_data <- create_pop_test_data()

  result <- extrapolate_pop(
    data = test_data,
    year_col = "year",
    pop_col = "total_pop",
    group_cols = c("adm0", "adm1", "adm2", "adm3"),
    years_to_extrap = c(`2021` = 1.5, `2022` = 1.3)
  )

  # check first location for both years
  first_location <- result |>
    dplyr::filter(
      adm0 == "country_x",
      adm1 == "region_a",
      adm2 == "district_1",
      adm3 == "district_1_subarea"
    ) |>
    dplyr::arrange(year)

  pop_2020 <- first_location |>
    dplyr::filter(year == 2020) |>
    dplyr::pull(total_pop)

  pop_2021 <- first_location |>
    dplyr::filter(year == 2021) |>
    dplyr::pull(total_pop)

  pop_2022 <- first_location |>
    dplyr::filter(year == 2022) |>
    dplyr::pull(total_pop)

  # 2021 should be 2020 * 1.5
  testthat::expect_equal(pop_2021, round(pop_2020 * 1.5))

  # 2022 should be 2021 * 1.3
  testthat::expect_equal(pop_2022, round(pop_2021 * 1.3))
})

testthat::test_that("extrapolate_pop expands year range correctly", {
  test_data <- create_pop_test_data(years = c(2018, 2020)) # missing 2019

  result <- extrapolate_pop(
    data = test_data,
    year_col = "year",
    pop_col = "total_pop",
    group_cols = c("adm0", "adm1", "adm2", "adm3"),
    years_to_extrap = c(2021),
    multiplier = 1.2
  )

  # should complete missing years in range
  expected_years <- 2018:2021
  actual_years <- sort(unique(result$year))

  testthat::expect_equal(actual_years, expected_years)

  # check that 2019 has na population (since it was missing)
  pop_2019 <- result |>
    dplyr::filter(year == 2019) |>
    dplyr::pull(total_pop)

  testthat::expect_true(all(is.na(pop_2019)))
})

testthat::test_that("extrapolate_pop preserves grouping structure", {
  test_data <- create_pop_test_data()

  result <- extrapolate_pop(
    data = test_data,
    year_col = "year",
    pop_col = "total_pop",
    group_cols = c("adm0", "adm1", "adm2", "adm3"),
    years_to_extrap = c(2021),
    multiplier = 1.1
  )

  # check that all original groups are preserved
  original_groups <- test_data |>
    dplyr::distinct(adm0, adm1, adm2, adm3) |>
    nrow()

  result_groups <- result |>
    dplyr::distinct(adm0, adm1, adm2, adm3) |>
    nrow()

  testthat::expect_equal(original_groups, result_groups)

  # each group should have complete year range
  group_year_counts <- result |>
    dplyr::group_by(adm0, adm1, adm2, adm3) |>
    dplyr::summarise(n_years = dplyr::n(), .groups = "drop")

  expected_n_years <- length(2018:2021) # original range + extrapolated
  testthat::expect_true(all(group_year_counts$n_years == expected_n_years))
})

# test parameter variations ----
testthat::test_that("extrapolate_pop handles different column names", {
  test_data <- create_pop_test_data() |>
    dplyr::rename(
      yr = year,
      population = total_pop,
      region = adm1,
      district = adm2
    )

  result <- extrapolate_pop(
    data = test_data,
    year_col = "yr",
    pop_col = "population",
    group_cols = c("adm0", "region", "district", "adm3"),
    years_to_extrap = c(2021),
    multiplier = 1.2
  )

  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_true("yr" %in% names(result))
  testthat::expect_true("population" %in% names(result))
  testthat::expect_true(2021 %in% result$yr)
})

testthat::test_that("extrapolate_pop handles single group column", {
  test_data <- create_pop_test_data() |>
    dplyr::select(adm0, adm1, adm2, year, total_pop) |>
    dplyr::distinct()

  result <- extrapolate_pop(
    data = test_data,
    year_col = "year",
    pop_col = "total_pop",
    group_cols = c("adm0"),
    years_to_extrap = c(2021),
    multiplier = 1.3
  )

  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_true(2021 %in% result$year)
  testthat::expect_equal(nrow(result), 13) # 2018:2021
})

testthat::test_that("extrapolate_pop handles multiple extrapolation years", {
  test_data <- create_pop_test_data()

  result <- extrapolate_pop(
    data = test_data,
    year_col = "year",
    pop_col = "total_pop",
    group_cols = c("adm0", "adm1", "adm2", "adm3"),
    years_to_extrap = c(2021, 2022, 2023, 2024),
    multiplier = 1.05
  )

  # all target years should be present
  expected_years <- 2018:2024
  actual_years <- sort(unique(result$year))
  testthat::expect_equal(actual_years, expected_years)

  # check sequential multiplication for one location
  location_data <- result |>
    dplyr::filter(
      adm0 == "country_x",
      adm1 == "region_a",
      adm2 == "district_1",
      adm3 == "district_1_subarea"
    ) |>
    dplyr::arrange(year)

  # each year should be previous year * 1.05 (rounded)
  for (i in 5:7) {
    # years 2021:2023 (rows 5:7)
    expected_pop <- round(location_data$total_pop[i - 1] * 1.05)
    testthat::expect_equal(location_data$total_pop[i], expected_pop)
  }
})

testthat::test_that("extrapolate_pop extrapolates backward when needed", {
  test_data <- tibble::tibble(
    adm0 = "country_x",
    adm1 = "region_a",
    adm2 = "district_1",
    adm3 = "district_1_subarea",
    year = 2024,
    total_pop = 1000
  )

  result <- extrapolate_pop(
    data = test_data,
    year_col = "year",
    pop_col = "total_pop",
    group_cols = c("adm0", "adm1", "adm2", "adm3"),
    years_to_extrap = 2020:2023,
    multiplier = 1.5
  )

  expected_years <- 2020:2024
  result_years <- sort(unique(result$year))
  testthat::expect_equal(result_years, expected_years)

  ordered <- result |>
    dplyr::arrange(year) |>
    dplyr::pull(total_pop)

  testthat::expect_equal(ordered, c(198, 297, 446, 669, 1000))
})

# test edge cases ----
testthat::test_that("extrapolate_pop handles single year input", {
  test_data <- create_pop_test_data(years = 2020)

  result <- extrapolate_pop(
    data = test_data,
    year_col = "year",
    pop_col = "total_pop",
    group_cols = c("adm0", "adm1", "adm2", "adm3"),
    years_to_extrap = c(2021),
    multiplier = 1.2
  )

  testthat::expect_true(2021 %in% result$year)

  # check that extrapolation worked
  extrap_values <- result |>
    dplyr::filter(year == 2021) |>
    dplyr::pull(total_pop)

  base_values <- result |>
    dplyr::filter(year == 2020) |>
    dplyr::pull(total_pop)

  testthat::expect_equal(extrap_values, round(base_values * 1.2))
})

testthat::test_that("extrapolate_pop handles missing population values", {
  test_data <- create_pop_test_data() |>
    dplyr::mutate(
      total_pop = dplyr::if_else(
        year == 2019 & adm2 == "district_1",
        NA_real_,
        total_pop
      )
    )

  result <- extrapolate_pop(
    data = test_data,
    year_col = "year",
    pop_col = "total_pop",
    group_cols = c("adm0", "adm1", "adm2", "adm3"),
    years_to_extrap = c(2021),
    multiplier = 1.1
  )

  # should handle na values gracefully
  testthat::expect_s3_class(result, "data.frame")

  # extrapolation from 2020 should still work for district_1
  district_1_2021 <- result |>
    dplyr::filter(year == 2021, adm2 == "district_1") |>
    dplyr::pull(total_pop)

  testthat::expect_true(all(!is.na(district_1_2021)))
})

testthat::test_that("extrapolate_pop handles zero multiplier", {
  test_data <- create_pop_test_data()

  result <- extrapolate_pop(
    data = test_data,
    year_col = "year",
    pop_col = "total_pop",
    group_cols = c("adm0", "adm1", "adm2", "adm3"),
    years_to_extrap = c(2021),
    multiplier = 0
  )

  # all extrapolated values should be 0
  extrap_values <- result |>
    dplyr::filter(year == 2021) |>
    dplyr::pull(total_pop)

  testthat::expect_true(all(extrap_values == 0))
})

testthat::test_that("extrapolate_pop handles negative multiplier", {
  test_data <- create_pop_test_data()

  testthat::expect_no_error({
    result <- extrapolate_pop(
      data = test_data,
      year_col = "year",
      pop_col = "total_pop",
      group_cols = c("adm0", "adm1", "adm2", "adm3"),
      years_to_extrap = c(2021),
      multiplier = -0.5
    )
  })

  # should produce negative populations (mathematically valid)
  extrap_values <- result |>
    dplyr::filter(year == 2021) |>
    dplyr::pull(total_pop)

  testthat::expect_true(all(extrap_values < 0))
})

# test error conditions ----
testthat::test_that("extrapolate_pop requires multiplier for unnamed years", {
  test_data <- create_pop_test_data()

  testthat::expect_error(
    extrapolate_pop(
      data = test_data,
      year_col = "year",
      pop_col = "total_pop",
      group_cols = c("adm0", "adm1", "adm2", "adm3"),
      years_to_extrap = c(2021, 2022),
      multiplier = NULL
    ),
    "`multiplier` must be provided when `years_to_extrap` is unnamed.",
    fixed = TRUE
  )
})

testthat::test_that("extrapolate_pop handles invalid column names", {
  test_data <- create_pop_test_data()

  testthat::expect_error(
    extrapolate_pop(
      data = test_data,
      year_col = "nonexistent_year",
      pop_col = "total_pop",
      group_cols = c("adm0", "adm1", "adm2", "adm3"),
      years_to_extrap = c(2021),
      multiplier = 1.1
    )
  )
})

# test data integrity ----
testthat::test_that("extrapolate_pop preserves original data", {
  test_data <- create_pop_test_data()
  original_rows <- test_data |>
    dplyr::filter(year %in% 2018:2020)

  result <- extrapolate_pop(
    data = test_data,
    year_col = "year",
    pop_col = "total_pop",
    group_cols = c("adm0", "adm1", "adm2", "adm3"),
    years_to_extrap = c(2021),
    multiplier = 1.1
  )

  result_original <- result |>
    dplyr::filter(year %in% 2018:2020) |>
    dplyr::arrange(adm0, adm1, adm2, adm3, year)

  original_sorted <- original_rows |>
    dplyr::arrange(adm0, adm1, adm2, adm3, year)

  # original data should be unchanged
  testthat::expect_equal(
    result_original$total_pop,
    original_sorted$total_pop
  )
})

testthat::test_that("extrapolate_pop rounds population values", {
  test_data <- create_pop_test_data() |>
    dplyr::mutate(total_pop = total_pop + 0.7) # add decimals

  result <- extrapolate_pop(
    data = test_data,
    year_col = "year",
    pop_col = "total_pop",
    group_cols = c("adm0", "adm1", "adm2", "adm3"),
    years_to_extrap = c(2021),
    multiplier = 1.33 # will create decimals
  )

  # extrapolated values should be integers
  new_values <- result |>
    dplyr::filter(year == 2021) |>
    dplyr::pull(total_pop)

  testthat::expect_true(all(new_values == round(new_values)))
})

testthat::test_that("extrapolate_pop maintains proper grouping", {
  test_data <- create_pop_test_data()

  result <- extrapolate_pop(
    data = test_data,
    year_col = "year",
    pop_col = "total_pop",
    group_cols = c("adm0", "adm1", "adm2", "adm3"),
    years_to_extrap = c(2021, 2022),
    multiplier = 1.1
  )

  # result should not be grouped
  testthat::expect_false(dplyr::is_grouped_df(result))

  # each administrative unit should have same number of years
  year_counts <- result |>
    dplyr::group_by(adm0, adm1, adm2, adm3) |>
    dplyr::summarise(n_years = dplyr::n(), .groups = "drop")

  expected_years <- length(2018:2022)
  testthat::expect_true(all(year_counts$n_years == expected_years))
})


# integration and workflow tests ----
testthat::test_that("extrapolate_pop works in typical workflow", {
  # simulate realistic workflow
  base_data <- create_pop_test_data(years = 2018:2019)

  # step 1: extrapolate missing 2020
  step1 <- extrapolate_pop(
    data = base_data,
    year_col = "year",
    pop_col = "total_pop",
    group_cols = c("adm0", "adm1", "adm2", "adm3"),
    years_to_extrap = c(2020),
    multiplier = 1.02
  )

  # step 2: extrapolate future years with different growth rates
  final_result <- extrapolate_pop(
    data = step1,
    year_col = "year",
    pop_col = "total_pop",
    group_cols = c("adm0", "adm1", "adm2", "adm3"),
    years_to_extrap = c(`2021` = 1.03, `2022` = 1.025),
    multiplier = NULL
  )

  testthat::expect_s3_class(final_result, "data.frame")
  testthat::expect_equal(sort(unique(final_result$year)), 2018:2022)

  # check that multi-step extrapolation works correctly
  test_location <- final_result |>
    dplyr::filter(
      adm0 == "country_x",
      adm1 == "region_a",
      adm2 == "district_1",
      adm3 == "district_1_subarea"
    ) |>
    dplyr::arrange(year)

  # verify calculation chain
  pop_2018 <- test_location$total_pop[1]
  pop_2020 <- test_location$total_pop[3]
  pop_2021 <- test_location$total_pop[4]

  # 2020 should be 2019 * 1.02, 2021 should be 2020 * 1.03
  testthat::expect_equal(pop_2021, round(pop_2020 * 1.03))
})
