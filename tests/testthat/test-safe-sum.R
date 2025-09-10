# /tests/testthat/test-safe-sum.R
# Tests for internal helper safe_sum()
# Ensures NA-only returns NA, otherwise sums with NA treated as zero
# RELEVANT FILES:R/safe_sum.R, R/utils.R, tests/testthat/test-utils.R

testthat::test_that("safe_sum returns NA when all values are NA", {
  x <- c(NA_real_, NA_real_)
  testthat::expect_true(is.na(safe_sum(x)))
})

testthat::test_that("safe_sum sums non-missing values and ignores NAs", {
  x <- c(1, NA, 2, NA, 3)
  testthat::expect_equal(safe_sum(x), 6)
})

testthat::test_that("safe_sum handles zeros and negatives", {
  testthat::expect_equal(safe_sum(c(0, 0, NA)), 0)
  testthat::expect_equal(safe_sum(c(-2, NA, 5)), 3)
})

testthat::test_that("safe_sum works with integer input and returns numeric", {
  res <- safe_sum(c(1L, 2L, NA_integer_))
  testthat::expect_type(res, "integer")
  testthat::expect_equal(res, 3)
})
