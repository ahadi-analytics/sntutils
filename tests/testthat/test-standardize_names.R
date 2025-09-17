testthat::local_edition(3)

# full pipeline ---------------------------------------------------------------
testthat::test_that("standardize_names applies full pipeline", {
  input_vec <- c("Chp Kpalime-III", "Kpalimé CHP 3", NA)
  result <- standardize_names(input_vec)

  # NA currently coerces to empty string; record behaviour explicitly
  expected <- c("CHP KPALIME 3", "CHP KPALIME 3", "")
  testthat::expect_equal(result, expected)
})

# optional steps --------------------------------------------------------------
testthat::test_that("standardize_names respects flag toggles", {
  accented <- "Hôpital d’Adéta"
  manual <- standardize_names(
    accented,
    to_upper = FALSE,
    replace_punct = TRUE,
    squish_spaces = TRUE,
    normalize_accents = FALSE,
    normalize_spaces = TRUE,
    roman_to_arabic = FALSE,
    sort_tokens = FALSE
  )
  testthat::expect_equal(manual, "Hôpital d Adéta")

  no_punct <- standardize_names(
    "Health.Center!",
    replace_punct = FALSE
  )
  testthat::expect_equal(no_punct, "HEALTH.CENTER!")

  keep_order <- standardize_names(
    "beta alpha 10",
    sort_tokens = FALSE
  )
  testthat::expect_equal(keep_order, "BETA ALPHA 10")
})

# whitespace handling --------------------------------------------------------
testthat::test_that("standardize_names normalizes whitespace variants", {
  spacey <- c("Clinic\u00A0One", "  multi\t space   entry  ")
  result <- standardize_names(spacey)
  testthat::expect_equal(result, c("CLINIC ONE", "ENTRY MULTI SPACE"))
})

# roman numerals -------------------------------------------------------------
testthat::test_that("standardize_names converts roman numerals by default", {
  roman_mix <- c("Block IX", "Block iii", "Block 4")
  result <- standardize_names(roman_mix)
  testthat::expect_equal(result, c("BLOCK 9", "BLOCK 3", "BLOCK 4"))

  untouched <- standardize_names(roman_mix, roman_to_arabic = FALSE)
  testthat::expect_equal(untouched, c("BLOCK IX", "BLOCK III", "BLOCK 4"))
})

# sorting behaviour ----------------------------------------------------------
testthat::test_that("standardize_names sorts tokens with letters then numbers", {
  mixed <- c("12 beta alpha", "alpha 10 gamma")
  result <- standardize_names(mixed)
  testthat::expect_equal(result, c("ALPHA BETA 12", "ALPHA GAMMA 10"))
})

# non-character inputs -------------------------------------------------------
testthat::test_that("standardize_names coerces factors and keeps length", {
  fac <- factor(c("Alpha Beta", "Gamma"))
  result <- standardize_names(fac)
  testthat::expect_equal(result, c("ALPHA BETA", "GAMMA"))
  testthat::expect_type(result, "character")
})

# error handling -------------------------------------------------------------
testthat::test_that("standardize_names validates inputs", {
  testthat::expect_error(
    standardize_names(list("alpha")),
    "must be an atomic vector"
  )

  testthat::expect_error(
    standardize_names("alpha", to_upper = NA),
    "missing value where TRUE/FALSE needed",
    fixed = TRUE
  )
})
