has_pkg <- function(p) requireNamespace(p, quietly = TRUE)

# stub infer_col_types if not present (pass-through)
.local_stub_infer <- function() {
  if (!exists("infer_col_types", mode = "function")) {
    assign(
      "infer_col_types",
      function(data, apply = TRUE, return = "data") data,
      envir = .GlobalEnv
    )
    withr::defer(rm(infer_col_types, envir = .GlobalEnv))
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

  df <- data.frame(
    adm0 = c("A", "A", "A", "B"),
    adm1 = c("X", "X", "Y", "Z"),
    adm2 = c("P", "P", "Q", "R"),
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

  # adm1 exists and aggregates
  testthat::expect_true("pop_data_adm1" %in% names(out))
  a1 <- out$pop_data_adm1
  row_x_2020 <- a1[a1$adm1 == "X" & a1$year == 2020L, , drop = FALSE]
  testthat::expect_equal(row_x_2020$pop, 12) # 5 + 7

  # adm2 exists and aggregates
  testthat::expect_true("pop_data_adm2" %in% names(out))
  a2 <- out$pop_data_adm2
  row_p_2020 <- a2[a2$adm2 == "P" & a2$year == 2020L, , drop = FALSE]
  testthat::expect_equal(row_p_2020$pop, 12)
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

  # do not rely on infer_col_types; let internal year coercion run
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
