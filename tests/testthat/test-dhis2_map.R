# tests for dhis2_map ----------------------------------------------------------

testthat::test_that("dhis2_map renames with case/space/accent normalization", {
  testthat::skip_if_not_installed("stringi")

  # dictionary: harmonised (new), raw (old)
  dict <- tibble::tibble(
    harmonised = c("cases_total", "rdt_positive", "health_facility"),
    raw        = c("Total  cases", "RDT  POSITIF", "Formation sanitaire")
  )

  # dataset with messy headers expected to match raw
  data_tbl <- tibble::tibble(
    `total   cases` = 1:2,
    `rdt positif` = 3:4,
    `formation  sanitaire` = 5:6,
    extra = 7:8
  )

  out <- dhis2_map(
    data = data_tbl,
    dict = dict,
    new_col = "harmonised",
    old_col = "raw",
    verbose = FALSE
  )

  # new names must exist; extra must be preserved
  testthat::expect_true(all(c("cases_total", "rdt_positive", "health_facility") %in%
    names(out)))
  testthat::expect_true("extra" %in% names(out))
})

testthat::test_that("dhis2_map accepts column indices for new/old selectors", {
  testthat::skip_if_not_installed("stringi")

  dict <- tibble::tibble(
    harmonised = c("a", "b"),
    raw        = c("COL A", "COL B")
  )
  data_tbl <- tibble::tibble(`col a` = 1:2, `col b` = 3:4)

  # new_col = 1 (harmonised), old_col = 2 (raw)
  out <- dhis2_map(
    data = data_tbl,
    dict = dict,
    new_col = 1L,
    old_col = 2L,
    verbose = FALSE
  )
  testthat::expect_true(all(c("a", "b") %in% names(out)))
})

testthat::test_that("dhis2_map errors on invalid inputs and unknown columns", {
  # invalid data
  testthat::expect_error(
    dhis2_map(data = 1, dict = tibble::tibble(), new_col = 1L, old_col = 1L),
    "data.*data[.]frame",
    ignore.case = TRUE
  )
  # invalid dict
  testthat::expect_error(
    dhis2_map(data = tibble::tibble(), dict = 1, new_col = 1L, old_col = 1L),
    "dict.*data[.]frame",
    ignore.case = TRUE
  )
  # unknown named selector
  dict <- tibble::tibble(h = "a", r = "A")
  testthat::expect_error(
    dhis2_map(
      data = tibble::tibble(A = 1),
      dict = dict,
      new_col = "nope",
      old_col = "r",
      verbose = FALSE
    ),
    "Column 'nope' not found",
    fixed = TRUE
  )
  # out of bounds index
  testthat::expect_error(
    dhis2_map(
      data = tibble::tibble(A = 1),
      dict = dict,
      new_col = 3L,
      old_col = 2L,
      verbose = FALSE
    ),
    "out of bounds",
    ignore.case = TRUE
  )
})

