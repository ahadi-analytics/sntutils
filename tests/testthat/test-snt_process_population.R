has_pkg <- function(p) requireNamespace(p, quietly = TRUE)

# stub auto_parse_types if not present (pass-through)
.local_stub_infer <- function() {
  if (!exists("auto_parse_types", mode = "function")) {
    assign(
      "auto_parse_types",
      function(data, apply = TRUE, return = "data") data,
      envir = .GlobalEnv
    )
    withr::defer(rm(auto_parse_types, envir = .GlobalEnv))
  }
}

# core validation ---------------------------------------------------------

testthat::test_that("errors on missing required columns", {
  testthat::skip_if_not(has_pkg("dplyr"))
  testthat::skip_if_not(has_pkg("sntutils"))

  df <- data.frame(adm0 = "A", year = 2020L, stringsAsFactors = FALSE)
  testthat::expect_error(
    snt_process_population(df),
    regexp = "missing required columns|`pop`"
  )

  df2 <- data.frame(adm0 = "A", pop = 1, stringsAsFactors = FALSE)
  testthat::expect_error(
    snt_process_population(df2),
    regexp = "missing required columns|year"
  )
})

testthat::test_that("coerces year when character with 4-digit patterns", {
  testthat::skip_if_not(has_pkg("dplyr"))
  testthat::skip_if_not(has_pkg("sntutils"))
  .local_stub_infer()

  df <- data.frame(
    adm0 = c("A", "A"),
    year = c("2020", "2021-05"), # second has extra month
    pop = c(10, 20),
    stringsAsFactors = FALSE
  )

  out <- snt_process_population(
    df,
    translate = FALSE,
    infer_types = FALSE
  )

  # expect adm0 table present and year coerced to integers
  testthat::expect_true("pop_data_adm0" %in% names(out))
  yrs <- sort(unique(out$pop_data_adm0$year))
  testthat::expect_equal(yrs, c(2020L, 2021L))
})

# summaries per level -----------------------------------------------------

testthat::test_that("aggregates by available admin levels", {
  testthat::skip_if_not(has_pkg("dplyr"))
  testthat::skip_if_not(has_pkg("sntutils"))
  .local_stub_infer()

  # unique rows at adm2 level (finest level) - no duplicates
  df <- data.frame(
    adm0 = c("A", "A", "A", "B"),
    adm1 = c("X", "X", "Y", "Z"),
    adm2 = c("P", "Q", "R", "S"),
    year = c(2020L, 2020L, 2020L, 2021L),
    pop = c(5, 7, 3, 10),
    stringsAsFactors = FALSE
  )

  out <- snt_process_population(df, translate = FALSE)

  # levels detected
  testthat::expect_equal(
    sort(out$levels_present),
    c("adm0", "adm1", "adm2")
  )

  # adm0 aggregation
  testthat::expect_true("pop_data_adm0" %in% names(out))
  a0 <- out$pop_data_adm0
  testthat::expect_true(all(c("adm0", "year", "pop") %in% names(a0)))
  # totals by year
  s2020 <- sum(df$pop[df$year == 2020L])
  s2021 <- sum(df$pop[df$year == 2021L])
  testthat::expect_equal(
    sort(a0$pop[a0$year == 2020L]),
    sort(c(s2020)) # single total for adm0=A in 2020
  )
  testthat::expect_equal(
    sum(a0$pop[a0$year == 2021L]),
    s2021
  )

  # adm1 exists and aggregates from adm2
  testthat::expect_true("pop_data_adm1" %in% names(out))
  a1 <- out$pop_data_adm1
  row_x_2020 <- a1[a1$adm1 == "X" & a1$year == 2020L, , drop = FALSE]
  testthat::expect_equal(row_x_2020$pop, 12) # P(5) + Q(7)

  # adm2 data should be preserved (no aggregation needed)
  testthat::expect_true("pop_data_adm2" %in% names(out))
  a2 <- out$pop_data_adm2
  row_p_2020 <- a2[a2$adm2 == "P" & a2$year == 2020L, , drop = FALSE]
  testthat::expect_equal(row_p_2020$pop, 5)
})

testthat::test_that("only present levels are returned (adm0 only)", {
  testthat::skip_if_not(has_pkg("dplyr"))
  testthat::skip_if_not(has_pkg("sntutils"))
  .local_stub_infer()

  df <- data.frame(
    adm0 = c("A", "B"),
    year = c(2020L, 2020L),
    pop = c(10, 20),
    stringsAsFactors = FALSE
  )
  out <- snt_process_population(df, translate = FALSE)

  testthat::expect_true("pop_data_adm0" %in% names(out))
  testthat::expect_false("pop_data_adm1" %in% names(out))
  testthat::expect_false("pop_data_adm2" %in% names(out))
  testthat::expect_false("pop_data_adm3" %in% names(out))
  testthat::expect_equal(out$levels_present, "adm0")
})

# dictionary --------------------------------------------------------------

testthat::test_that("dictionary includes EN and optional FR", {
  testthat::skip_if_not(has_pkg("dplyr"))
  testthat::skip_if_not(has_pkg("sntutils"))
  .local_stub_infer()

  df <- data.frame(
    adm0 = c("A", "A"),
    adm1 = c("X", "Y"),
    year = c(2020L, 2021L),
    pop = c(1, 2),
    stringsAsFactors = FALSE
  )

  # EN only
  out1 <- snt_process_population(df, translate = FALSE)
  dict1 <- out1$data_dictionary
  testthat::expect_true(all(c("variable", "label_en") %in% names(dict1)))
  testthat::expect_false(any(grepl("^label_fr$", names(dict1))))

  # EN + FR (when translator missing, FR will mirror EN via passthrough)
  out2 <- snt_process_population(df, translate = TRUE)
  dict2 <- out2$data_dictionary
  testthat::expect_true(any(grepl("^label_fr$", names(dict2))))
  # same length, aligned variables
  testthat::expect_equal(nrow(dict2), length(unique(names(df))))
})

# infer_types path --------------------------------------------------------

testthat::test_that("respects infer_types=FALSE path", {
  testthat::skip_if_not(has_pkg("dplyr"))
  testthat::skip_if_not(has_pkg("sntutils"))

  df <- data.frame(
    adm0 = c("A", "A"),
    year = c("2020", "2021"),
    pop = c(1, 2),
    stringsAsFactors = FALSE
  )

  # do not rely on auto_parse_types; let internal year coercion run
  out <- snt_process_population(
    df,
    translate = FALSE,
    infer_types = FALSE
  )
  a0 <- out$pop_data_adm0
  testthat::expect_true(is.integer(a0$year) || is.numeric(a0$year))
  testthat::expect_equal(sort(unique(a0$year)), c(2020L, 2021L))
})

# edge cases --------------------------------------------------------------

testthat::test_that("handles empty data after validation gracefully", {
  testthat::skip_if_not(has_pkg("dplyr"))
  testthat::skip_if_not(has_pkg("sntutils"))
  .local_stub_infer()

  df <- data.frame(
    adm0 = character(0),
    year = integer(0),
    pop = numeric(0)
  )

  out <- snt_process_population(df, translate = FALSE)

  # all present level tables should be empty but exist for adm0
  testthat::expect_true("pop_data_adm0" %in% names(out))
  testthat::expect_equal(nrow(out$pop_data_adm0), 0)
  testthat::expect_true(is.data.frame(out$data_dictionary))
})

testthat::test_that(".summarise_by returns NULL when cols missing", {
  testthat::skip_if_not(has_pkg("dplyr"))
  testthat::skip_if_not(has_pkg("sntutils"))

  df <- data.frame(
    adm0 = c("A", "B"),
    year = c(2020L, 2020L),
    pop = c(1, 2)
  )
  out <- .summarise_by(df, c("adm0", "adm1"))
  testthat::expect_null(out)
})

# proportion handling -----------------------------------------------------

testthat::test_that("detects and averages proportion columns", {
  testthat::skip_if_not(has_pkg("dplyr"))
  testthat::skip_if_not(has_pkg("sntutils"))
  .local_stub_infer()

  # unique rows at adm2 level to test aggregation to adm1 and adm0
  df <- data.frame(
    adm0 = c("A", "A", "A"),
    adm1 = c("X", "X", "Y"),
    adm2 = c("P", "Q", "R"),
    year = c(2020L, 2020L, 2020L),
    pop_u5_prop = c(0.15, 0.20, 0.18),
    stringsAsFactors = FALSE
  )

  out <- snt_process_population(df, pop_cols = "pop_u5_prop", translate = FALSE)

  # adm0 aggregation should average the proportion
  a0 <- out$pop_data_adm0
  testthat::expect_true("pop_u5_prop" %in% names(a0))
  expected_mean <- mean(c(0.15, 0.20, 0.18))
  testthat::expect_equal(a0$pop_u5_prop[1], expected_mean, tolerance = 1e-6)

  # adm1 aggregation should also average
  a1 <- out$pop_data_adm1
  row_x <- a1[a1$adm1 == "X", , drop = FALSE]
  testthat::expect_equal(row_x$pop_u5_prop, mean(c(0.15, 0.20)), tolerance = 1e-6)
})

testthat::test_that("handles mixed proportion and count columns", {
  testthat::skip_if_not(has_pkg("dplyr"))
  testthat::skip_if_not(has_pkg("sntutils"))
  .local_stub_infer()

  # unique rows at adm1 level to test aggregation to adm0
  df <- data.frame(
    adm0 = c("A", "A", "B"),
    adm1 = c("X", "Y", "Z"),
    year = c(2020L, 2020L, 2020L),
    pop = c(100, 200, 150),
    pop_u5_prop = c(0.15, 0.20, 0.18),
    stringsAsFactors = FALSE
  )

  out <- snt_process_population(
    df,
    pop_cols = c("pop", "pop_u5_prop"),
    translate = FALSE
  )

  a0 <- out$pop_data_adm0
  # pop should be summed
  row_a <- a0[a0$adm0 == "A", , drop = FALSE]
  testthat::expect_equal(row_a$pop, 300) # 100 + 200
  # pop_u5_prop should be averaged
  testthat::expect_equal(row_a$pop_u5_prop, mean(c(0.15, 0.20)), tolerance = 1e-6)

  row_b <- a0[a0$adm0 == "B", , drop = FALSE]
  testthat::expect_equal(row_b$pop, 150)
  testthat::expect_equal(row_b$pop_u5_prop, 0.18, tolerance = 1e-6)
})

testthat::test_that("errors on mixed proportion/count values in single column", {
  testthat::skip_if_not(has_pkg("dplyr"))
  testthat::skip_if_not(has_pkg("sntutils"))
  .local_stub_infer()

  df <- data.frame(
    adm0 = c("A", "A", "B"),
    year = c(2020L, 2020L, 2020L),
    pop_bad = c(0.15, 0.20, 150),
    stringsAsFactors = FALSE
  )

  testthat::expect_error(
    snt_process_population(df, pop_cols = "pop_bad", translate = FALSE),
    regexp = "mixed proportion and count data"
  )
})

testthat::test_that("count columns still sum as before", {
  testthat::skip_if_not(has_pkg("dplyr"))
  testthat::skip_if_not(has_pkg("sntutils"))
  .local_stub_infer()

  # unique rows at adm1 level to test aggregation to adm0
  df <- data.frame(
    adm0 = c("A", "A", "B"),
    adm1 = c("X", "Y", "Z"),
    year = c(2020L, 2020L, 2020L),
    pop = c(100, 200, 150),
    pop_u5 = c(15, 40, 27),
    stringsAsFactors = FALSE
  )

  out <- snt_process_population(
    df,
    pop_cols = c("pop", "pop_u5"),
    translate = FALSE
  )

  a0 <- out$pop_data_adm0
  row_a <- a0[a0$adm0 == "A", , drop = FALSE]
  # both should be summed since values > 1
  testthat::expect_equal(row_a$pop, 300)
  testthat::expect_equal(row_a$pop_u5, 55)
})

# duplicate detection ------------------------------------------------------

testthat::test_that("errors on sex disaggregated data (Burundi case)", {
  testthat::skip_if_not(has_pkg("dplyr"))
  testthat::skip_if_not(has_pkg("sntutils"))
  .local_stub_infer()

  # data with M/F rows for same admin-year (common in WorldPop, DHS, etc.)
  df <- data.frame(
    adm0 = c("Burundi", "Burundi"),
    adm1 = c("Bubanza", "Bubanza"),
    adm2 = c("Gihanga", "Gihanga"),
    year = c(2023L, 2023L),
    sex = c("M", "F"),
    pop = c(120000, 125000),
    stringsAsFactors = FALSE
  )

  # should error with clear message about duplicates
  testthat::expect_error(
    snt_process_population(df, translate = FALSE, infer_types = FALSE),
    regexp = "multiple rows per admin-year"
  )
})

testthat::test_that("errors on age-disaggregated data", {
  testthat::skip_if_not(has_pkg("dplyr"))
  testthat::skip_if_not(has_pkg("sntutils"))
  .local_stub_infer()

  df <- data.frame(
    adm0 = c("A", "A", "A"),
    year = c(2020L, 2020L, 2020L),
    age_group = c("0-5", "5-15", "15+"),
    pop = c(50000, 80000, 120000),
    stringsAsFactors = FALSE
  )

  testthat::expect_error(
    snt_process_population(df, translate = FALSE, infer_types = FALSE),
    regexp = "multiple rows per admin-year"
  )
})

testthat::test_that("errors on residence strata (urban/rural)", {
  testthat::skip_if_not(has_pkg("dplyr"))
  testthat::skip_if_not(has_pkg("sntutils"))
  .local_stub_infer()

  df <- data.frame(
    adm0 = c("A", "A"),
    adm1 = c("X", "X"),
    year = c(2020L, 2020L),
    residence = c("urban", "rural"),
    pop = c(80000, 120000),
    stringsAsFactors = FALSE
  )

  testthat::expect_error(
    snt_process_population(df, translate = FALSE, infer_types = FALSE),
    regexp = "multiple rows per admin-year"
  )
})
