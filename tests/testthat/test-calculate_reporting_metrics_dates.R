test_that("calculate_reporting_metrics_dates produces valid reporting rates", {
  # Create test data with known dates
  test_df <- tibble::tibble(
    hf_uid = rep(c("HF001", "HF002", "HF003"), each = 12),
    adm2 = rep(c("District1", "District1", "District2"), each = 12),
    date = rep(seq.Date(as.Date("2023-01-01"), by = "month", length.out = 12), 3),
    hf_opening_date = c(
      rep(as.Date("2023-01-01"), 12),  # HF001: open all year
      rep(as.Date("2023-06-01"), 12),  # HF002: opens in June
      rep(as.Date("2023-01-01"), 12)   # HF003: open all year
    ),
    hf_closing_date = c(
      rep(as.Date("2023-12-31"), 12),  # HF001: open all year
      rep(as.Date("2023-12-31"), 12),  # HF002: open from June onwards
      rep(as.Date("2023-12-31"), 12)   # HF003: open all year
    ),
    conf = sample(50:150, 36)
  )

  result <- calculate_reporting_metrics_dates(
    test_df,
    start_col = "hf_opening_date",
    end_col = "hf_closing_date"
  )

  # Reporting rate should be between 0 and 1
  expect_true(all(result$reprate >= 0 & result$reprate <= 1, na.rm = TRUE))

  # Should not depend on case counts (correlation should be low)
  expect_true(abs(cor(result$conf, result$reprate, use = "complete.obs")) < 0.5)

  # HF002 should be inactive before June 2023
  hf002_early <- result |>
    dplyr::filter(hf_uid == "HF002", date < as.Date("2023-06-01"))
  expect_true(all(hf002_early$activity_status == "Inactive"))

  # HF002 should be active from June onwards
  hf002_late <- result |>
    dplyr::filter(hf_uid == "HF002", date >= as.Date("2023-06-01"))
  expect_true(all(hf002_late$activity_status == "Active"))

  # District1 should have 50% reporting rate before June (1 out of 2 facilities active)
  district1_early <- result |>
    dplyr::filter(adm2 == "District1", date < as.Date("2023-06-01")) |>
    dplyr::distinct(date, reprate)
  expect_equal(unique(district1_early$reprate), 0.5)

  # District1 should have 100% reporting rate from June onwards (2 out of 2 facilities active)
  district1_late <- result |>
    dplyr::filter(adm2 == "District1", date >= as.Date("2023-06-01")) |>
    dplyr::distinct(date, reprate)
  expect_equal(unique(district1_late$reprate), 1.0)

  # District2 should have 100% reporting rate all year (1 out of 1 facility active)
  district2_all <- result |>
    dplyr::filter(adm2 == "District2") |>
    dplyr::distinct(date, reprate)
  expect_equal(unique(district2_all$reprate), 1.0)
})

test_that("calculate_reporting_metrics_dates handles edge cases", {
  # Test with empty data
  empty_df <- tibble::tibble(
    hf_uid = character(0),
    adm2 = character(0),
    date = as.Date(character(0)),
    hf_opening_date = as.Date(character(0)),
    hf_closing_date = as.Date(character(0))
  )

  result <- calculate_reporting_metrics_dates(
    empty_df,
    start_col = "hf_opening_date",
    end_col = "hf_closing_date"
  )

  expect_equal(nrow(result), 0)
  expect_true("reprate" %in% names(result))
  expect_true("activity_status" %in% names(result))
})

test_that("calculate_reporting_metrics_dates handles NA dates", {
  test_df <- tibble::tibble(
    hf_uid = c("HF001", "HF002", "HF003"),
    adm2 = c("District1", "District1", "District1"),
    date = rep(as.Date("2023-06-01"), 3),
    hf_opening_date = c(as.Date("2023-01-01"), NA, as.Date("2023-01-01")),
    hf_closing_date = c(as.Date("2023-12-31"), as.Date("2023-12-31"), NA),
    conf = c(100, 100, 100)
  )

  result <- calculate_reporting_metrics_dates(
    test_df,
    start_col = "hf_opening_date",
    end_col = "hf_closing_date"
  )

  # Facilities with NA dates should have NA activity_status
  expect_true(is.na(result$activity_status[result$hf_uid == "HF002"]))
  expect_true(is.na(result$activity_status[result$hf_uid == "HF003"]))

  # HF001 should be active
  expect_equal(result$activity_status[result$hf_uid == "HF001"], "Active")

  # Reporting rate should be 1/3 (only 1 out of 3 facilities is confirmed active)
  expect_equal(unique(result$reprate), 1/3)
})

test_that("calculate_reporting_metrics_dates validates missing columns", {
  # Missing start and end columns
  test_df_missing_dates <- tibble::tibble(
    hf_uid = "HF001",
    adm2 = "District1",
    date = as.Date("2023-06-01")
  )

  expect_error(
    calculate_reporting_metrics_dates(
      test_df_missing_dates,
      start_col = "missing_start",
      end_col = "missing_end"
    ),
    "not found"
  )

  # Missing record column
  test_df_no_record <- tibble::tibble(
    hf_uid = "HF001",
    adm2 = "District1",
    hf_opening_date = as.Date("2023-01-01"),
    hf_closing_date = as.Date("2023-12-31")
  )

  expect_error(
    calculate_reporting_metrics_dates(
      test_df_no_record,
      start_col = "hf_opening_date",
      end_col = "hf_closing_date",
      record_col = "missing_date"
    ),
    "not found"
  )
})

test_that("calculate_reporting_metrics_dates validates missing group columns", {
  test_df <- tibble::tibble(
    hf_uid = "HF001",
    date = as.Date("2023-06-01"),
    hf_opening_date = as.Date("2023-01-01"),
    hf_closing_date = as.Date("2023-12-31")
  )

  expect_error(
    calculate_reporting_metrics_dates(
      test_df,
      start_col = "hf_opening_date",
      end_col = "hf_closing_date",
      group_cols = "missing_adm"
    ),
    "Grouping column"
  )
})

test_that("calculate_reporting_metrics_dates works with multiple grouping columns", {
  test_df <- tibble::tibble(
    hf_uid = rep(c("HF001", "HF002", "HF003", "HF004"), each = 12),
    adm0 = "Country",
    adm1 = rep(c("Region1", "Region1", "Region2", "Region2"), each = 12),
    adm2 = rep(c("Dist1", "Dist2", "Dist3", "Dist4"), each = 12),
    date = rep(seq.Date(
      as.Date("2024-01-01"),
      by = "month",
      length.out = 12
    ), times = 4),
    hf_opening_date = c(
      rep(as.Date("2024-01-01"), 12),  # HF001: open all year
      rep(as.Date("2024-06-01"), 12),  # HF002: opens in June
      rep(as.Date("2024-01-01"), 12),  # HF003: open all year
      rep(as.Date("2024-01-01"), 12)   # HF004: open all year
    ),
    hf_closing_date = rep(as.Date("2024-12-31"), 48)
  )

  result <- calculate_reporting_metrics_dates(
    test_df,
    start_col = "hf_opening_date",
    end_col = "hf_closing_date",
    group_cols = c("adm0", "adm1", "adm2")
  )

  expect_true("reprate" %in% names(result))
  expect_true(all(c("adm0", "adm1", "adm2") %in% names(result)))

  # Check Dist2 before June (HF002 inactive, so 0 out of 1 active)
  dist2_early <- result |>
    dplyr::filter(adm2 == "Dist2", date < as.Date("2024-06-01")) |>
    dplyr::distinct(date, reprate)
  expect_equal(unique(dist2_early$reprate), 0)

  # Check Dist2 from June onwards (HF002 active, so 1 out of 1 active)
  dist2_late <- result |>
    dplyr::filter(adm2 == "Dist2", date >= as.Date("2024-06-01")) |>
    dplyr::distinct(date, reprate)
  expect_equal(unique(dist2_late$reprate), 1)
})

test_that("calculate_reporting_metrics_dates works with custom record_col", {
  test_df <- tibble::tibble(
    hf_uid = rep(c("HF001", "HF002"), each = 12),
    adm2 = "District",
    report_month = rep(seq.Date(
      as.Date("2024-01-01"),
      by = "month",
      length.out = 12
    ), times = 2),
    hf_opening_date = c(
      rep(as.Date("2024-01-01"), 12),  # HF001: open all year
      rep(as.Date("2024-06-01"), 12)   # HF002: opens in June
    ),
    hf_closing_date = rep(as.Date("2024-12-31"), 24)
  )

  result <- calculate_reporting_metrics_dates(
    test_df,
    start_col = "hf_opening_date",
    end_col = "hf_closing_date",
    record_col = "report_month"
  )

  expect_true("reprate" %in% names(result))
  expect_true("report_month" %in% names(result))

  # Before June: 1 out of 2 facilities active (50%)
  early_period <- result |>
    dplyr::filter(report_month < as.Date("2024-06-01")) |>
    dplyr::distinct(report_month, reprate)
  expect_equal(unique(early_period$reprate), 0.5)

  # From June onwards: 2 out of 2 facilities active (100%)
  late_period <- result |>
    dplyr::filter(report_month >= as.Date("2024-06-01")) |>
    dplyr::distinct(report_month, reprate)
  expect_equal(unique(late_period$reprate), 1.0)
})
