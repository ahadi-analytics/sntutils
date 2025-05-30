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
