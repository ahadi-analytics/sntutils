make_hf_data <- function() {
  # 5 facilities x 12 months; HF005 never reports
  dates <- base::seq.Date(
    base::as.Date("2024-01-01"),
    by = "month",
    length.out = 12L
  )
  n_dates <- base::length(dates)
  hfs <- c("HF001", "HF002", "HF003", "HF004", "HF005")

  tibble::tibble(
    date = base::rep(dates, base::length(hfs)),
    facility_id = base::rep(hfs, each = n_dates),
    district = base::rep(c("North", "North", "South", "South", "East"), each = n_dates),
    allout = c(
      base::rep(100, n_dates),                         # HF001 always reports
      base::rep(150, n_dates),                         # HF002 always reports
      c(base::rep(0, 6L), base::rep(NA_real_, 6L)),    # HF003 active then drops
      base::rep(80, n_dates),                          # HF004 always reports
      base::rep(NA_real_, n_dates)                     # HF005 never reports
    ),
    conf = c(
      base::rep(10, n_dates),
      base::rep(15, n_dates),
      c(base::rep(0, 6L), base::rep(NA_real_, 6L)),
      base::rep(8, n_dates),
      base::rep(NA_real_, n_dates)
    )
  )
}

testthat::test_that("get_active_facilities() returns active-only data with status column", {
  hf_data <- make_hf_data()

  active <- NULL
  testthat::expect_message(
    active <- sntutils::get_active_facilities(
      data = hf_data,
      hf_col = "facility_id",
      date_col = "date",
      key_indicators = c("allout", "conf"),
      method = 3
    ),
    regexp = "active"
  )

  testthat::expect_s3_class(active, "data.frame")
  testthat::expect_true("activity_status" %in% base::names(active))
  testthat::expect_true(base::all(active$activity_status == "Active"))

  # HF005 reports nothing → never appears in the active subset
  testthat::expect_false("HF005" %in% active$facility_id)

  # attributes are attached for downstream use
  testthat::expect_false(base::is.null(base::attr(active, "n_active_facilities")))
  testthat::expect_false(base::is.null(base::attr(active, "n_total_facilities")))
})

testthat::test_that("get_active_facilities(return_summary = TRUE) returns counts tibble", {
  hf_data <- make_hf_data()

  summary <- sntutils::get_active_facilities(
    data = hf_data,
    hf_col = "facility_id",
    date_col = "date",
    key_indicators = c("allout", "conf"),
    method = 3,
    return_summary = TRUE
  )

  testthat::expect_s3_class(summary, "tbl_df")
  testthat::expect_named(
    summary,
    c("n_total", "n_active", "n_inactive", "pct_active", "pct_inactive")
  )
  testthat::expect_equal(summary$n_active + summary$n_inactive, summary$n_total)
  testthat::expect_equal(
    base::round(summary$pct_active + summary$pct_inactive),
    100
  )
})

testthat::test_that("get_active_facilities() errors on non-dataframe input", {
  testthat::expect_error(
    sntutils::get_active_facilities(
      data = list(a = 1),
      hf_col = "facility_id"
    ),
    regexp = "must be a data frame"
  )
})

testthat::test_that("get_active_facilities() errors when hf_col is not a string", {
  hf_data <- make_hf_data()
  testthat::expect_error(
    sntutils::get_active_facilities(
      data = hf_data,
      hf_col = c("a", "b")
    ),
    regexp = "single character string"
  )
})

testthat::test_that("get_active_facilities() errors when hf_col not in data", {
  hf_data <- make_hf_data()
  testthat::expect_error(
    sntutils::get_active_facilities(
      data = hf_data,
      hf_col = "nonexistent_col"
    ),
    regexp = "not found in data"
  )
})

testthat::test_that("get_active_facilities() errors when date_col not in data", {
  hf_data <- make_hf_data()
  testthat::expect_error(
    sntutils::get_active_facilities(
      data = hf_data,
      hf_col = "facility_id",
      date_col = "missing_date"
    ),
    regexp = "Date column"
  )
})

testthat::test_that("get_active_facilities() errors when no key indicators are present", {
  hf_data <- make_hf_data()
  testthat::expect_error(
    sntutils::get_active_facilities(
      data = hf_data,
      hf_col = "facility_id",
      date_col = "date",
      key_indicators = c("nope1", "nope2")
    ),
    regexp = "None of the key indicators"
  )
})

testthat::test_that("get_active_facilities() warns when some key indicators are missing", {
  hf_data <- make_hf_data()
  testthat::expect_warning(
    sntutils::get_active_facilities(
      data = hf_data,
      hf_col = "facility_id",
      date_col = "date",
      key_indicators = c("allout", "totally_not_a_column")
    ),
    regexp = "Some key indicators not found"
  )
})
