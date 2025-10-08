testthat::test_that(
  "ensure_packages correctly handles all packages already installed",
  {
    mockery::stub(ensure_packages, "cli::cli_alert_success", function(...) NULL)

    installed_pkgs <- c("stats", "utils")
    testthat::expect_invisible(ensure_packages(installed_pkgs))
  }
)

testthat::test_that(
  "ensure_packages installs missing packages interactively when user agrees",
  {
    mockery::stub(ensure_packages, "interactive", function() TRUE)
    mockery::stub(ensure_packages, "readline", function(...) "y")
    mockery::stub(
      ensure_packages, "utils::install.packages",
      function(pkg, ...) NULL
    )
    mockery::stub(ensure_packages, "requireNamespace", function(pkg, ...) FALSE)
    mockery::stub(ensure_packages, "cli::cli_alert_success", function(...) NULL)

    missing_pkgs <- "crayon"
    suppressMessages(
      testthat::expect_invisible(ensure_packages(missing_pkgs)))
  }
)

testthat::test_that(
  "ensure_packages skips installation interactively when user declines",
  {
    mockery::stub(ensure_packages, "interactive", function() TRUE)
    mockery::stub(ensure_packages, "readline", function(...) "n")
    mockery::stub(ensure_packages, "requireNamespace", function(pkg, ...) FALSE)
    mockery::stub(ensure_packages, "cli::cli_alert_warning", function(...) NULL)

    missing_pkgs <- "pillar"
    suppressMessages(
      testthat::expect_invisible(ensure_packages(missing_pkgs)
      )
    )
  }
)

testthat::test_that(
  "ensure_packages skips installation in non-interactive sessions",
  {
    mockery::stub(ensure_packages, "interactive", function() FALSE)
    mockery::stub(ensure_packages, "requireNamespace", function(pkg, ...) FALSE)
    mockery::stub(ensure_packages, "cli::cli_alert_warning", function(...) NULL)

    missing_pkgs <- c("glue")
    suppressMessages(
    testthat::expect_invisible(ensure_packages(missing_pkgs))
    )
  }
)

testthat::test_that(
  "ensure_packages handles invalid package names gracefully",
  {
    mockery::stub(ensure_packages, "interactive", function() TRUE)
    mockery::stub(ensure_packages, "readline", function(...) "y")
    mockery::stub(
      ensure_packages, "utils::install.packages",
      function(pkg, ...) stop("package not found")
    )
    mockery::stub(ensure_packages, "requireNamespace", function(pkg, ...) FALSE)
    mockery::stub(ensure_packages, "cli::cli_alert_danger", function(...) NULL)

    invalid_pkgs <- c("thispackagedoesnotexist123")
    suppressMessages(
    testthat::expect_invisible(ensure_packages(invalid_pkgs))
    )
  }
)


# Test for vdigest function
testthat::test_that("vdigest works correctly", {
  # Test with character vector
  testthat::expect_equal(
    length(vdigest(c("a", "b", "c"))),
    3
  )

  # Test with iris Species
  testthat::expect_equal(
    length(vdigest(as.character(iris$Species))),
    length(iris$Species)
  )

  # Test that output is character
  testthat::expect_type(
    vdigest(c("test")),
    "character"
  )

  # Test that different inputs give different hashes
  testthat::expect_false(
    vdigest("a") == vdigest("b")
  )

  # Test that same inputs give same hashes
  testthat::expect_equal(
    vdigest("test"),
    vdigest("test")
  )
})

testthat::test_that("big_mark formats numbers correctly", {

  # Test default formatting
  testthat::expect_equal(big_mark(1000), "1,000")
  testthat::expect_equal(big_mark(123456789), "123,456,789")

  # Test custom big_mark (e.g., space)
  testthat::expect_equal(big_mark(1234567, big_mark = " "), "1 234 567")

  # Test decimals formatting
  testthat::expect_equal(big_mark(1234.5678, decimals = 2), "1,234.57")
  testthat::expect_equal(big_mark(1234, decimals = 3), "1,234.000")

  # Test no decimals provided, integer input
  testthat::expect_equal(big_mark(1000000), "1,000,000")

  # Test rounding with decimals
  testthat::expect_equal(big_mark(999.999, decimals = 2), "1,000.00")

  # Test negative numbers
  testthat::expect_equal(big_mark(-1234567), "-1,234,567")

  # Test large number formatting explicitly avoiding scientific notation
  testthat::expect_equal(big_mark(1e9), "1,000,000,000")

  # Edge case: zero
  testthat::expect_equal(big_mark(0), "0")
  testthat::expect_equal(big_mark(0, decimals = 3), "0.000")

})


testthat::test_that("sum2 correctly sums numeric vectors with NA values", {

  testthat::expect_equal(sum2(c(1, 2, NA, 4)), 7)
  testthat::expect_equal(sum2(c(NA, NA, 5, 10)), 15)
  testthat::expect_equal(sum2(c(NA, NA, NA)), 0)
  testthat::expect_equal(sum2(numeric(0)), 0)
  testthat::expect_equal(sum2(c(-1, NA, 1)), 0)

})

testthat::test_that("mean2 correctly computes means with NA values", {

  testthat::expect_equal(mean2(c(1, 2, NA, 4)), 7 / 3)
  testthat::expect_equal(mean2(c(NA, NA, 10, 20)), 15)
  testthat::expect_equal(mean2(c(NA, NA)), NaN)
  testthat::expect_true(is.nan(mean2(numeric(0))))
  testthat::expect_equal(mean2(c(-2, NA, 2)), 0)

})

testthat::test_that("median2 correctly computes medians with NA values", {

  testthat::expect_equal(median2(c(1, 2, NA, 4)), 2)
  testthat::expect_equal(median2(c(NA, NA, 10, 20)), 15)
  testthat::expect_true(is.na(median2(c(NA, NA))))
  testthat::expect_true(is.na(median2(numeric(0))))
  testthat::expect_equal(median2(c(-5, NA, 5)), 0)
  testthat::expect_equal(median2(c(NA, 100)), 100)

})

testthat::test_that("vdigest returns a vector of digests for character input", {

  x <- c("a", "b", "c")
  result <- vdigest(x)

  testthat::expect_type(result, "character")
  testthat::expect_length(result, 3)
  testthat::expect_true(all(nchar(result) > 0))
  testthat::expect_equal(length(unique(result)), 3)  # each digest is unique

})

testthat::test_that("vdigest works with numeric input", {

  x <- c(1, 2, 3.14)
  result <- vdigest(x)

  testthat::expect_type(result, "character")
  testthat::expect_length(result, 3)
})

testthat::test_that("vdigest gives same output as digest() element-wise", {

  x <- c("test", "123", "abc")
  manual <- vapply(x, digest::digest, character(1))
  auto <- vdigest(x)

  testthat::expect_equal(auto, manual)
})

testthat::test_that("vdigest works with factors and converts correctly", {

  f <- factor(c("low", "med", "high"))
  result <- vdigest(f)

  testthat::expect_type(result, "character")
  testthat::expect_length(result, 3)
})

testthat::test_that("fallback_row_sum computes row-wise sums correctly and preserves type", {
  # basic row-wise sum (numeric input -> numeric output)
  v1 <- c(1, 2, 3)
  v2 <- c(4, 5, 6)
  out_num <- fallback_row_sum(v1, v2)
  testthat::expect_equal(out_num, c(5, 7, 9))
  testthat::expect_true(is.double(out_num))

  # with NA values and default min_present = 1 (numeric)
  v1 <- c(1, NA, 3)
  v2 <- c(2, 4, NA)
  out_num2 <- fallback_row_sum(v1, v2)
  testthat::expect_equal(out_num2, c(3, 4, 3))
  testthat::expect_true(is.double(out_num2))

  # with min_present = 2 (requires both present) -> numeric NA_real_
  out_num3 <- fallback_row_sum(v1, v2, min_present = 2)
  testthat::expect_equal(out_num3, c(3, NA_real_, NA_real_))
  testthat::expect_true(is.double(out_num3))
  testthat::expect_true(is.na(out_num3[2]))
  testthat::expect_identical(typeof(out_num3[2]), "double")

  # all NA row check stays NA with correct type
  v1 <- c(1, NA, 3)
  v2 <- c(2, NA, 4)
  out_num4 <- fallback_row_sum(v1, v2, min_present = 2)
  testthat::expect_true(is.na(out_num4[2]))
  testthat::expect_identical(typeof(out_num4[2]), "double")

  # with three vectors (numeric)
  v3 <- c(1, 1, 1)
  out_num5 <- fallback_row_sum(v1, v2, v3)
  testthat::expect_equal(out_num5, c(4, 1, 8))
  testthat::expect_true(is.double(out_num5))

  # all NA values -> all NA_real_
  v1_all_na <- c(NA_real_, NA_real_)
  v2_all_na <- c(NA_real_, NA_real_)
  out_all_na <- fallback_row_sum(v1_all_na, v2_all_na)
  testthat::expect_true(all(is.na(out_all_na)))
  testthat::expect_true(is.double(out_all_na))

  # single vector passthrough (numeric)
  out_single <- fallback_row_sum(c(1, 2, 3))
  testthat::expect_equal(out_single, c(1, 2, 3))
  testthat::expect_true(is.double(out_single))

  # with zeros (numeric)
  v1 <- c(0, 0, 5)
  v2 <- c(0, 3, 0)
  out_zero <- fallback_row_sum(v1, v2)
  testthat::expect_equal(out_zero, c(0, 3, 5))
  testthat::expect_true(is.double(out_zero))

  # --- type preservation checks ---

  # all integer inputs -> integer output and NA_integer_
  xi <- c(1L, 2L, NA_integer_)
  yi <- c(3L, NA_integer_, 4L)
  out_int <- fallback_row_sum(xi, yi)
  testthat::expect_equal(out_int, c(4L, 2, 4))
  testthat::expect_true(is.integer(out_int))
  testthat::expect_identical(typeof(out_int[2]), "integer")

  # all integer with min_present = 2
  out_int2 <- fallback_row_sum(xi, yi, min_present = 2)
  testthat::expect_equal(out_int2, c(4L, NA_integer_, NA_integer_))
  testthat::expect_true(is.integer(out_int2))

  # mixed integer + numeric -> numeric output and NA_real_
  zi <- c(1, 2, NA_real_)
  out_mixed <- fallback_row_sum(xi, zi)
  testthat::expect_equal(out_mixed, c(2, 4, NA_real_))
  testthat::expect_true(is.double(out_mixed))
  testthat::expect_identical(typeof(out_mixed[3]), "double")
})

testthat::test_that(
  "fallback_diff computes absolute differences correctly and preserves type", {
  # basic numeric cases -> numeric output
  out_num <- fallback_diff(5, 3)
  testthat::expect_equal(out_num, 2)
  testthat::expect_true(is.double(out_num))

  testthat::expect_equal(fallback_diff(3, 5), 2)
  testthat::expect_equal(fallback_diff(10, 10), 0)

  # with one NA value -> numeric
  testthat::expect_equal(fallback_diff(NA, 4), 4)
  testthat::expect_equal(fallback_diff(7, NA), 7)

  # both NA -> NA_real_
  out_na <- fallback_diff(NA, NA)
  testthat::expect_true(is.na(out_na))
  testthat::expect_identical(typeof(out_na), "double")

  # minimum parameter respected (numeric)
  testthat::expect_equal(fallback_diff(2, 3, minimum = 5), 5)
  testthat::expect_equal(fallback_diff(NA, 2, minimum = 5), 5)
  testthat::expect_equal(fallback_diff(3, NA, minimum = 1), 3)

  # vectorised behaviour
  col1 <- c(5, NA, 7, 4, NA)
  col2 <- c(3, 4, NA, 9, NA)
  expected <- c(2, 4, 7, 5, NA_real_) # note abs + min=0 default
  out_vec <- fallback_diff(col1, col2)
  testthat::expect_equal(out_vec, expected)
  testthat::expect_true(is.double(out_vec))

  # negative differences (should be absolute)
  testthat::expect_equal(fallback_diff(-5, 3), 8)
  testthat::expect_equal(fallback_diff(3, -5), 8)

  # --- integer cases ---

  xi <- c(5L, NA_integer_, 7L, 4L, NA_integer_)
  yi <- c(3L, 4L, NA_integer_, 9L, NA_integer_)
  out_int <- fallback_diff(xi, yi)
  expected_int <- c(2L, 4L, 7L, 5L, NA_integer_)
  testthat::expect_equal(out_int, expected_int)
  testthat::expect_true(is.integer(out_int))

  # both NA -> NA_integer_
  out_int_na <- fallback_diff(NA_integer_, NA_integer_)
  testthat::expect_true(is.na(out_int_na))
  testthat::expect_identical(typeof(out_int_na), "integer")

  # integer with minimum > 0
  testthat::expect_equal(fallback_diff(2L, 3L, minimum = 5L), 5L)
  testthat::expect_true(is.integer(fallback_diff(2L, 3L, minimum = 5L)))
})

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
