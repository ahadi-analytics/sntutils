# tests/testthat/test-infer_col_types.R

testthat::test_that("functions exist and return invisibly", {
  testthat::skip_if_not(exists("infer_col_types", mode = "function"))
  testthat::skip_if_not(exists("detect_factors", mode = "function"))

  data_small <- tibble::tibble(a = c("x", "y", "x"))

  # return = "data"
  dat <- NULL
  testthat::expect_invisible({
    dat <- infer_col_types(
      data = data_small,
      apply = FALSE,
      return = "data"
    )
  })
  testthat::expect_s3_class(dat, "tbl_df")
  testthat::expect_named(dat, "a")

  # return = "plan"
  pl <- NULL
  testthat::expect_invisible({
    pl <- infer_col_types(
      data = data_small,
      apply = FALSE,
      return = "plan"
    )
  })
  testthat::expect_s3_class(pl, "tbl_df")
  testthat::expect_true(all(
    c(
      "name",
      "current_type",
      "proposed_type",
      "rule",
      "protected",
      "n",
      "n_non_na",
      "n_unique",
      "unique_ratio"
    ) %in%
      names(pl)
  ))

  # return = "both"
  both <- NULL
  testthat::expect_invisible({
    both <- infer_col_types(
      data = data_small,
      apply = FALSE,
      return = "both"
    )
  })
  testthat::expect_type(both, "list")
  testthat::expect_named(both, c("plan", "data"))
  testthat::expect_s3_class(both$plan, "tbl_df")
  testthat::expect_s3_class(both$data, "tbl_df")

  # detect_factors invisibility and structure
  p <- NULL
  testthat::expect_invisible({
    p <- detect_factors(data = data_small)
  })
  testthat::expect_s3_class(p, "tbl_df")
  testthat::expect_true(all(
    c(
      "name",
      "n",
      "n_non_na",
      "n_unique",
      "unique_ratio",
      "proposed",
      "reason"
    ) %in%
      names(p)
  ))
})

testthat::test_that("plan has expected columns and row count", {
  testthat::skip_if_not(exists("infer_col_types", mode = "function"))

  data_wide <- tibble::tibble(
    a = c("x", "y", "x"),
    b = c(1, 2, 3),
    c = c("2024-01-01", "2024-01-02", "2024-01-03")
  )
  res <- infer_col_types(data = data_wide, apply = FALSE, return = "both")
  plan <- res$plan

  need <- c(
    "name",
    "current_type",
    "proposed_type",
    "rule",
    "protected",
    "n",
    "n_non_na",
    "n_unique",
    "unique_ratio"
  )
  testthat::expect_true(all(need %in% names(plan)))
  testthat::expect_equal(nrow(plan), ncol(data_wide))
  testthat::expect_setequal(plan$name, names(data_wide))
})

testthat::test_that("readr parsing: numeric, integer, logical, date, datetime", {
  testthat::skip_if_not(exists("infer_col_types", mode = "function"))

  data_types <- tibble::tibble(
    num_char = c("1,200.5", "-3", "0"),
    int_char = c("1", "2", "3"),
    bool_char = c("TRUE", "FALSE", "TRUE"),
    zero_one = c("0", "1", "0"),
    date_char = c("2024-01-01", "2024-01-02", "2024-01-03"),
    dt_char = c(
      "2024-01-01 12:00:00",
      "2024-01-02 00:00:00",
      "2024-01-03 01:02:03"
    ),
    keep_chr = c("12kg", "foo", "bar")
  )

  res <- infer_col_types(data = data_types, apply = TRUE, return = "both")
  plan <- res$plan
  dc <- res$data

  ptype <- function(var) {
    plan |>
      dplyr::filter(name == var) |>
      dplyr::slice(1) |>
      dplyr::pull("proposed_type")
  }

  testthat::expect_identical(ptype("num_char"), "numeric")
  testthat::expect_identical(ptype("int_char"), "integer")
  testthat::expect_identical(ptype("bool_char"), "logical")
  testthat::expect_identical(ptype("zero_one"), "integer")
  testthat::expect_identical(ptype("date_char"), "Date")
  testthat::expect_identical(ptype("dt_char"), "POSIXct")
  testthat::expect_true(ptype("keep_chr") %in% c("character", "factor"))

  testthat::expect_true(is.numeric(dc$num_char))
  testthat::expect_true(is.integer(dc$int_char))
  testthat::expect_true(is.logical(dc$bool_char))
  testthat::expect_true(is.integer(dc$zero_one))
  testthat::expect_true(inherits(dc$date_char, "Date"))
  testthat::expect_true(inherits(dc$dt_char, "POSIXct"))
})

testthat::test_that("protected names stay character and get protected rule", {
  testthat::skip_if_not(exists("infer_col_types", mode = "function"))

  data_id <- tibble::tibble(
    health_facility_code = c("001", "002", "003"),
    other = c("A", "B", "A")
  )

  res <- infer_col_types(data = data_id, apply = TRUE, return = "both")
  plan <- res$plan
  dc <- res$data

  row <- plan |>
    dplyr::filter(name == "health_facility_code") |>
    dplyr::slice(1)

  testthat::expect_identical(row$proposed_type, "character")
  testthat::expect_identical(row$rule, "protected_by_name")
  testthat::expect_true(is.character(dc$health_facility_code))
})

testthat::test_that("leading zeros guard blocks conversion when enabled", {
  testthat::skip_if_not(exists("infer_col_types", mode = "function"))

  data_lz <- tibble::tibble(
    lot = c("01", "02", "03", "01"),
    adm = c("A", "B", "B", "A")
  )

  res <- infer_col_types(
    data = data_lz,
    protect_patterns = character(0),
    keep_leading_zero_chars = TRUE,
    max_unique_ratio = 1.0,
    apply = TRUE,
    return = "both"
  )
  plan <- res$plan
  dc <- res$data

  row_lot <- plan |> dplyr::filter(name == "lot") |> dplyr::slice(1)
  row_adm <- plan |> dplyr::filter(name == "adm") |> dplyr::slice(1)

  testthat::expect_identical(row_lot$proposed_type, "character")
  testthat::expect_identical(row_lot$rule, "leading_zeros_guard")
  testthat::expect_true(is.character(dc$lot))

  testthat::expect_identical(row_adm$proposed_type, "factor")
  testthat::expect_identical(row_adm$rule, "low_cardinality")
  testthat::expect_true(is.factor(dc$adm))
})

testthat::test_that("turning off leading zeros guard allows factor", {
  testthat::skip_if_not(exists("infer_col_types", mode = "function"))

  data_lz <- tibble::tibble(
    lot = c("01", "02", "02", "03")
  )

  res <- infer_col_types(
    data = data_lz,
    protect_patterns = character(0),
    keep_leading_zero_chars = FALSE,
    max_unique_ratio = 1.0,
    apply = TRUE,
    return = "both"
  )
  plan <- res$plan |> dplyr::filter(name == "lot") |> dplyr::slice(1)
  dc <- res$data

  testthat::expect_identical(plan$proposed_type, "factor")
  testthat::expect_identical(plan$rule, "low_cardinality")
  testthat::expect_true(is.factor(dc$lot))
})

testthat::test_that("factor thresholds work (max_levels and ratio)", {
  testthat::skip_if_not(exists("infer_col_types", mode = "function"))

  n <- 100L
  low <- rep(letters[1:5], length.out = n)
  high <- format(seq_len(n))

  data_thr <- tibble::tibble(low = low, high = high)

  res1 <- infer_col_types(
    data = data_thr,
    max_levels = 50,
    max_unique_ratio = 0.2,
    apply = TRUE,
    return = "both"
  )
  p1_low <- res1$plan |> dplyr::filter(name == "low") |> dplyr::slice(1)
  p1_high <- res1$plan |> dplyr::filter(name == "high") |> dplyr::slice(1)

  testthat::expect_identical(p1_low$proposed_type, "factor")
  testthat::expect_true(is.factor(res1$data$low))
  testthat::expect_false(p1_high$proposed_type == "factor")
  testthat::expect_false(is.factor(res1$data$high))

  res2 <- infer_col_types(
    data = data_thr,
    max_levels = 200,
    max_unique_ratio = 1.0,
    apply = TRUE,
    return = "both"
  )
  p2_high <- res2$plan |> dplyr::filter(name == "high") |> dplyr::slice(1)

  testthat::expect_identical(p2_high$proposed_type, "factor")
  testthat::expect_true(is.factor(res2$data$high))
})

testthat::test_that("no factor candidates still returns parsed data", {
  testthat::skip_if_not(exists("infer_col_types", mode = "function"))

  data_nf <- tibble::tibble(
    id = c("1", "2", "3"),
    amt = c("1,000.5", "2,000.0", "3,500.25")
  )

  res <- infer_col_types(
    data = data_nf,
    protect_patterns = c("id$"),
    max_unique_ratio = 1e-9,
    apply = TRUE,
    return = "both"
  )
  plan <- res$plan
  dc <- res$data

  row_id <- plan |> dplyr::filter(name == "id") |> dplyr::slice(1)
  testthat::expect_identical(row_id$proposed_type, "character")
  testthat::expect_identical(row_id$rule, "protected_by_name")
  testthat::expect_true(is.character(dc$id))

  row_amt <- plan |> dplyr::filter(name == "amt") |> dplyr::slice(1)
  testthat::expect_identical(row_amt$proposed_type, "numeric")
  testthat::expect_true(is.numeric(dc$amt))
})

testthat::test_that("column order is preserved after apply", {
  testthat::skip_if_not(exists("infer_col_types", mode = "function"))

  data_order <- tibble::tibble(
    b = c("z", "y", "x"),
    a = c("k", "k", "m"),
    c = c("01", "02", "03")
  )

  res <- infer_col_types(
    data = data_order,
    protect_patterns = character(0),
    keep_leading_zero_chars = TRUE,
    max_unique_ratio = 1.0,
    apply = TRUE,
    return = "both"
  )
  dc <- res$data

  testthat::expect_equal(names(dc), c("b", "a", "c"))
  testthat::expect_true(is.factor(dc$b))
  testthat::expect_true(is.factor(dc$a))
  testthat::expect_false(is.factor(dc$c))

  testthat::expect_equal(levels(dc$b), c("z", "y", "x"))
  testthat::expect_equal(levels(dc$a), c("k", "m"))
})

testthat::test_that("detect_factors respects protection and leading zeros", {
  testthat::skip_if_not(exists("detect_factors", mode = "function"))

  data_df <- tibble::tibble(
    area_id = c("A1", "A2", "A3"),
    lot = c("01", "02", "03"),
    zone = c("Z1", "Z1", "Z2")
  )

  p <- detect_factors(
    data = data_df,
    protect_patterns = c("id$"),
    keep_leading_zero_chars = TRUE,
    max_unique_ratio = 1.0
  )

  testthat::expect_false("area_id" %in% p$name)
  testthat::expect_false("lot" %in% p$name)
  testthat::expect_true("zone" %in% p$name)
})

testthat::test_that("zero-row and empty data frames handled", {
  testthat::skip_if_not(exists("infer_col_types", mode = "function"))
  testthat::skip_if_not(exists("detect_factors", mode = "function"))

  data_zero <- tibble::tibble(ch = character(), num = character())
  res <- infer_col_types(data = data_zero, apply = FALSE, return = "both")
  testthat::expect_s3_class(res$plan, "tbl_df")
  testthat::expect_equal(nrow(res$plan), 2L)

  p <- detect_factors(data = data_zero)
  testthat::expect_s3_class(p, "tbl_df")

  data_empty <- tibble::tibble()
  res2 <- infer_col_types(data = data_empty, apply = FALSE, return = "both")
  testthat::expect_equal(nrow(res2$plan), 0L)
  p2 <- detect_factors(data = data_empty)
  testthat::expect_equal(nrow(p2), 0L)
})
