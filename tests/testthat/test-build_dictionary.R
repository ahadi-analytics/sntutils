# minimal skips
has_pkg <- function(p) requireNamespace(p, quietly = TRUE)

# ---- .guess_type -------------------------------------------------------------
testthat::test_that(".guess_type classifies common types", {
  expect <- testthat::expect_equal

  expect(.guess_type(1:3), "integer")
  expect(.guess_type(c(1, 2.5)), "double")
  expect(.guess_type(factor(c("a", "b"))), "factor")
  expect(.guess_type(as.Date("2025-01-01") + 0:1), "date")
  expect(.guess_type(as.POSIXct("2025-01-01 00:00:00", tz = "UTC")), "datetime")
  expect(.guess_type(c(TRUE, FALSE)), "logical")
  expect(.guess_type(c("x", "y")), "character")
  expect(.guess_type(list(a = 1, b = "x")), "list")

  # integerish doubles should map to "integer"
  expect(.guess_type(c(1, 2, 3) + 0.0), "integer")

  # sf geometry (if sf installed)
  if (has_pkg("sf")) {
    sfc <- sf::st_sfc(sf::st_point(c(0, 0)), crs = 4326)
    expect(.guess_type(sfc), "geometry")
  }
})

# ---- .nunique ----------------------------------------------------------------
testthat::test_that(".nunique returns count and sets cap attribute when needed", {
  x <- c(1, 1, 2, 3, 3, 3)
  out <- .nunique(x, cap = 10L)

  # compare just the numeric value
  testthat::expect_equal(as.integer(out), 3L)
  # and check the attribute separately
  testthat::expect_false(isTRUE(attr(out, "cap_reached")))

  # very large vector -> sampling path
  set.seed(1)
  big <- sample(1:1e6, 20000, replace = TRUE)
  out2 <- .nunique(big, cap = 1000L)

  testthat::expect_true(isTRUE(attr(out2, "cap_reached")))
  testthat::expect_type(out2, "integer")
})

# ---- .examples ---------------------------------------------------------------
testthat::test_that(".examples shows frequent values for character/factor and truncates", {
  x <- c(rep("apple", 3), rep("banana", 2), "cherry")
  s <- .examples(x, n = 2L, width = 60L)
  # top 2 should be apple, banana (order may be "apple, banana")
  testthat::expect_true(grepl("apple", s))
  testthat::expect_true(grepl("banana", s))

  # factor behaves the same
  f <- factor(x)
  s2 <- .examples(f, n = 2L, width = 60L)
  testthat::expect_true(grepl("apple", s2))
  testthat::expect_true(grepl("banana", s2))

  # numeric: just first uniques
  y <- c(10, 20, 20, 30)
  s3 <- .examples(y, n = 2L, width = 60L)
  testthat::expect_true(grepl("10", s3) && grepl("20", s3))

  # truncation
  long <- rep(paste(rep("a", 200), collapse = ""), 2)
  s4 <- .examples(long, n = 1L, width = 20L)
  testthat::expect_true(nchar(s4) <= 20L)
  testthat::expect_true(grepl("...$", s4))
})

# ---- .range_str --------------------------------------------------------------
testthat::test_that(".range_str returns min/max as strings and handles all-NA", {
  testthat::expect_equal(.range_str(c(10, 2, 5)), c("2", "10"))
  testthat::expect_equal(
    .range_str(as.Date("2020-01-01") + c(0, 10)),
    c("2020-01-01", "2020-01-11")
  )
  testthat::expect_equal(.range_str(c(NA_real_, NA_real_)), c("", ""))
})

# ---- build_dictionary ------------------------------------------------
testthat::test_that("build_dictionary returns expected columns and profiles types", {
  df <- data.frame(
    id = 1:5,
    grp = factor(c("a", "a", "b", "b", "c")),
    when = as.Date("2024-01-01") + 0:4,
    flag = c(TRUE, NA, FALSE, TRUE, FALSE),
    txt = c("foo", "bar", "bar", "baz", NA_character_),
    stringsAsFactors = FALSE
  )

  dict <- build_dictionary(data = df, labels_path = NULL, language = NULL)

  # structure
  needed <- c(
    "variable",
    "type",
    "label_en",
    "n",
    "n_missing",
    "pct_missing",
    "n_unique",
    "example_values",
    "min",
    "max",
    "notes"
  )
  testthat::expect_true(all(needed %in% names(dict)))
  testthat::expect_equal(nrow(dict), ncol(df))

  # types guessed
  tmap <- setNames(dict$type, dict$variable)
  testthat::expect_equal(unname(tmap["id"]), "integer")
  testthat::expect_equal(unname(tmap["grp"]), "factor")
  testthat::expect_equal(unname(tmap["when"]), "date")
  testthat::expect_equal(unname(tmap["flag"]), "logical")
  testthat::expect_equal(unname(tmap["txt"]), "character")

  # missing % on 'flag' (1 NA of 5 -> 20)
  pm <- setNames(dict$pct_missing, dict$variable)
  testthat::expect_equal(unname(pm["flag"]), 20.00)

  # label_en falls back to variable name when no map
  lem <- setNames(dict$label_en, dict$variable)
  testthat::expect_equal(unname(lem["id"]), "id")
})

testthat::test_that("build_dictionary merges labels from CSV map", {
  tmp <- withr::local_tempdir()
  map_path <- fs::path(tmp, "labels_en.csv")
  lbl_df <- data.frame(
    name = c("id", "grp"),
    label = c("Identifier", "Group"),
    stringsAsFactors = FALSE
  )
  utils::write.csv(lbl_df, map_path, row.names = FALSE)

  df <- data.frame(
    id = 1:2,
    grp = factor(c("a", "b")),
    z = 3:4
  )
  dict <- build_dictionary(data = df, labels_path = map_path)

  lem <- setNames(dict$label_en, dict$variable)
  testthat::expect_equal(unname(lem["id"]), "Identifier")
  testthat::expect_equal(unname(lem["grp"]), "Group")
  # unknown column falls back to its name
  testthat::expect_equal(unname(lem["z"]), "z")
})

testthat::test_that("build_dictionary handles geometry and lists cleanly", {
  # geometry (skip if sf not available)
  if (has_pkg("sf")) {
    g <- sf::st_sfc(sf::st_point(c(0, 0)), crs = 4326)
    df_g <- data.frame(a = 1:1)
    df_g$geom <- g
    dict_g <- build_dictionary(df_g)

    row_g <- dict_g[dict_g$variable == "geom", , drop = FALSE]
    testthat::expect_equal(row_g$type[[1]], "geometry")
    testthat::expect_true(is.na(row_g$n_unique[[1]]))
    testthat::expect_equal(row_g$example_values[[1]], "")
  }

  # list column
  df_l <- data.frame(a = 1:3)
  df_l$lst <- list(1:2, letters[1:3], list(x = 1))
  dict_l <- build_dictionary(df_l)

  row_l <- dict_l[dict_l$variable == "lst", , drop = FALSE]
  testthat::expect_equal(row_l$type[[1]], "list")
  testthat::expect_true(grepl("^list of:", row_l$notes[[1]]))
})

testthat::test_that("build_dictionary adds translated label column and orders it", {
  df <- data.frame(id = 1:2, name = c("a", "b"))
  dict <- build_dictionary(df, language = "fr")

  testthat::expect_true("label_fr" %in% names(dict))
  # ensure label_fr is placed immediately after label_en
  nm <- names(dict)
  testthat::expect_equal(which(nm == "label_fr"), which(nm == "label_en") + 1L)
  # basic type/length checks; content may mirror EN if translator missing
  testthat::expect_type(dict$label_fr, "character")
  testthat::expect_equal(length(dict$label_fr), nrow(dict))
})

testthat::test_that("build_dictionary(language='fr') adds label and orders column", {
  df <- data.frame(id = 1:2, name = c("a", "b"))
  dict <- build_dictionary(data = df, labels_path = NULL, language = "fr")
  nm <- names(dict)
  testthat::expect_true("label_fr" %in% nm)
  testthat::expect_equal(which(nm == "label_fr"), which(nm == "label_en") + 1L)
  testthat::expect_type(dict$label_fr, "character")
  testthat::expect_equal(length(dict$label_fr), nrow(dict))
  testthat::expect_false(any(is.na(dict$label_fr)))
})

# ---- build_dictionary trans_cache_path passthrough ---------------------------
testthat::test_that(
  "build_dictionary forwards trans_cache_path as cache_path to translator", {
  # Prepare a small data frame
  df <- data.frame(id = 1:2, name = c("alpha", "beta"))
  tmp_cache <- withr::local_tempdir()

  # Access package namespace and swap translator with a test double
  ns <- asNamespace("sntutils")
  orig <- get("translate_text_vec", envir = ns)

  captured <- NULL
  fake_translator <- function(
    text, target_language = "fr", cache_path = NULL, ...) {
    captured <<- cache_path
    paste0(as.character(text), "_", target_language)
  }

  # Replace binding safely and restore on exit
  unlockBinding("translate_text_vec", ns)
  withr::defer({
    unlockBinding("translate_text_vec", ns)
    assign("translate_text_vec", orig, envir = ns)
    lockBinding("translate_text_vec", ns)
  })
  assign("translate_text_vec", fake_translator, envir = ns)
  lockBinding("translate_text_vec", ns)

  dict <- build_dictionary(df, language = "fr", trans_cache_path = tmp_cache)

  # Expect trans_cache_path (file path) was normalised to its directory
  testthat::expect_identical(captured, tmp_cache)

  # And translated labels reflect our fake translator output
  testthat::expect_true("label_fr" %in% names(dict))
  testthat::expect_true(all(grepl("_fr$", dict$label_fr)))
})

# ---- build_dictionary ----------------------------------------------------
testthat::test_that("build_dictionary writes CSV (utf-8, newline)", {
  tmp <- withr::local_tempdir()
  df <- data.frame(id = 1:2, name = c("école", "café"))
  dict <- build_dictionary(df, labels_path = tmp)

  p <- fs::path(tmp, "dict.csv")
  testthat::expect_invisible(build_dictionary(dict, p))
  testthat::expect_true(fs::file_exists(p))

  txt <- readChar(p, 1e6, useBytes = TRUE)
  testthat::expect_true(grepl("école|café", txt, useBytes = TRUE))
  testthat::expect_true(grepl("\n", txt, fixed = TRUE))
})

testthat::test_that("build_dictionary writes XLSX when openxlsx available", {
  testthat::skip_if_not(has_pkg("openxlsx"))

  tmp <- withr::local_tempdir()
  df <- data.frame(id = 1:2, name = c("a", "b"))
  dict <- build_dictionary(df)

  p <- fs::path(tmp, "dict.xlsx")
  testthat::expect_invisible(build_dictionary(dict, p))
  testthat::expect_true(fs::file_exists(p))

  # sanity: can read back and get header row
  wb <- openxlsx::read.xlsx(p)
  testthat::expect_true(all(
    c("variable", "type", "label_en") %in% names(wb)
  ))
})

# ---- YAML integration tests --------------------------------------------------
testthat::test_that("build_dictionary integrates YAML labels for SNT variables", {
  # create test data with known SNT variables
  df <- data.frame(
    adm1 = c("Province A", "Province B"),
    adm2 = c("District 1", "District 2"),
    conf_rdt_u5 = c(10, 20),
    test_mic_priv = c(5, 15),
    unknown_var = c("x", "y")
  )

  dict <- build_dictionary(df)
  lem <- setNames(dict$label_en, dict$variable)

  # exact matches from YAML
  testthat::expect_equal(
    unname(lem["adm1"]),
    "Administrative level 1 (province)"
  )
  testthat::expect_equal(
    unname(lem["adm2"]),
    "Administrative level 2 (district)"
  )

  # token-based inference
  testthat::expect_match(
    unname(lem["conf_rdt_u5"]),
    "Confirmed malaria cases.*RDT.*Under 5 years"
  )
  testthat::expect_match(
    unname(lem["test_mic_priv"]),
    "Tested for malaria.*[Mm]icroscopy.*Private sector"
  )

  # unknown variable gets title case from check_snt_var
  # since it has underscore it tries to parse it
  testthat::expect_equal(unname(lem["unknown_var"]), "Unknown")
})

testthat::test_that("build_dictionary uses YAML labels by default, CSV doesn't override", {
  tmp <- withr::local_tempdir()
  map_path <- fs::path(tmp, "labels_en.csv")

  # create override for a YAML variable
  lbl_df <- data.frame(
    name = c("adm1", "conf_rdt_u5"),
    label = c("Custom Province Label", "Custom Malaria Label"),
    stringsAsFactors = FALSE
  )
  utils::write.csv(lbl_df, map_path, row.names = FALSE)

  df <- data.frame(
    adm1 = c("A", "B"),
    adm2 = c("X", "Y"),
    conf_rdt_u5 = c(1, 2)
  )

  # default behavior: YAML wins
  dict <- build_dictionary(df, labels_path = map_path)
  lem <- setNames(dict$label_en, dict$variable)

  # YAML labels should win (CSV ignored)
  testthat::expect_equal(unname(lem["adm1"]), "Administrative level 1 (province)")
  testthat::expect_match(
    unname(lem["conf_rdt_u5"]),
    "Confirmed malaria cases.*RDT.*Under 5 years"
  )

  # non-overridden variable still gets YAML label
  testthat::expect_equal(
    unname(lem["adm2"]),
    "Administrative level 2 (district)"
  )
})

testthat::test_that("build_dictionary respects CSV when override_yaml = TRUE", {
  tmp <- withr::local_tempdir()
  map_path <- fs::path(tmp, "labels_en.csv")

  # create override for a YAML variable
  lbl_df <- data.frame(
    name = c("adm1", "conf_rdt_u5"),
    label = c("Custom Province Label", "Custom Malaria Label"),
    stringsAsFactors = FALSE
  )
  utils::write.csv(lbl_df, map_path, row.names = FALSE)

  df <- data.frame(
    adm1 = c("A", "B"),
    adm2 = c("X", "Y"),
    conf_rdt_u5 = c(1, 2)
  )

  # with override_yaml = TRUE, CSV wins
  dict <- build_dictionary(df, labels_path = map_path, override_yaml = TRUE)
  lem <- setNames(dict$label_en, dict$variable)

  # CSV overrides should win
  testthat::expect_equal(unname(lem["adm1"]), "Custom Province Label")
  testthat::expect_equal(unname(lem["conf_rdt_u5"]), "Custom Malaria Label")

  # non-overridden variable still gets YAML label
  testthat::expect_equal(
    unname(lem["adm2"]),
    "Administrative level 2 (district)"
  )
})

testthat::test_that("build_dictionary supports Portuguese labels", {
  # test data with SNT variables
  df <- data.frame(
    adm1 = c("A", "B"),
    conf_rdt_u5 = c(10, 20),
    test_mic_priv = c(5, 15)
  )

  # build dictionary with Portuguese
  dict <- build_dictionary(df, language = "pt")

  testthat::expect_true("label_pt" %in% names(dict))

  # check Portuguese labels were retrieved
  lpt <- setNames(dict$label_pt, dict$variable)

  # exact match from YAML
  testthat::expect_equal(
    unname(lpt["adm1"]),
    "Nível administrativo 1 (província)"
  )

  # token-based inference
  testthat::expect_match(
    unname(lpt["conf_rdt_u5"]),
    "Casos confirmados de malária.*RDT.*Menores de 5 anos"
  )
  testthat::expect_match(
    unname(lpt["test_mic_priv"]),
    "Testado para malária.*[Mm]icroscopia.*Setor privado"
  )
})

# cache tests
test_that("cache works correctly", {
  # clear cache
  clear_snt_cache()

  # build dictionary triggers cache population
  dd1 <- build_dictionary(dplyr::as_tibble(iris))

  # second call should be faster (uses cache)
  dd2 <- build_dictionary(dplyr::as_tibble(iris))

  expect_identical(dd1, dd2)

  # clear and verify cache is empty
  clear_snt_cache()
  expect_length(ls(envir = sntutils:::.snt_cache), 0)
})

test_that("cache improves performance on large var lists", {
  # create test data with 100 columns
  test_data <- as.data.frame(
    matrix(rnorm(1000), ncol = 100)
  )
  names(test_data) <- paste0("var_", 1:100)

  # clear cache and time first call
  clear_snt_cache()
  t1 <- system.time({
    dd1 <- build_dictionary(test_data)
  })

  # time second call (should use cache)
  t2 <- system.time({
    dd2 <- build_dictionary(test_data)
  })

  # second call should be faster
  expect_lt(t2[["elapsed"]], t1[["elapsed"]])
  expect_identical(dd1, dd2)
})
