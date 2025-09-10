# minimal helpers for tests ----------------------------------------------------

# return TRUE if writer exists
has_writer <- function(fmt) {
  fmt %in% names(.writer_registry())
}

# return TRUE if package exists
has_pkg <- function(p) {
  requireNamespace(p, quietly = TRUE)
}

# small df with varied types and utf-8
make_small_df <- function() {
  tibble::tibble(
    id = 1:3,
    name = c("école", "façade", "café"),
    when = as.Date("2025-01-01") + 0:2,
    flag = c(TRUE, FALSE, TRUE),
    grp = factor(c("a", "b", "a"))
  )
}

# small sf if sf present
make_small_sf <- function() {
  if (!has_pkg("sf")) {
    return(NULL)
  }
  df <- make_small_df()
  geom <- sf::st_sfc(
    sf::st_point(c(0, 0)),
    sf::st_point(c(1, 1)),
    sf::st_point(c(2, 2)),
    crs = 4326
  )
  sf::st_as_sf(df, geom = geom)
}

# core validation --------------------------------------------------------------

testthat::test_that("rejects bad inputs", {
  tmp <- withr::local_tempdir()

  err1 <- testthat::expect_error(
    write_snt_data(
      obj = NULL,
      path = tmp,
      data_name = "ok",
      file_formats = "rds",
      quiet = FALSE
    )
  )
  testthat::expect_match(err1$message, "`obj`")

  err2 <- testthat::expect_error(
    write_snt_data(
      obj = mtcars,
      path = "",
      data_name = "ok",
      file_formats = "rds",
      quiet = FALSE
    )
  )
  testthat::expect_match(err2$message, "`path`")

  err3 <- testthat::expect_error(
    write_snt_data(
      obj = mtcars,
      path = tmp,
      data_name = "a b",
      file_formats = "rds",
      quiet = FALSE
    )
  )
  testthat::expect_true(
    any(grepl("data_name|illegal", err3$message, ignore.case = TRUE))
  )

  err4 <- testthat::expect_error(
    write_snt_data(
      obj = mtcars,
      path = tmp,
      data_name = "ok",
      file_formats = "zzz",
      quiet = FALSE
    )
  )
  testthat::expect_match(err4$message, "Unsupported")
})

testthat::test_that("date vs tag conflict is rejected", {
  tmp <- withr::local_tempdir()
  testthat::expect_error(
    write_snt_data(
      obj = mtcars,
      path = tmp,
      data_name = "cars",
      file_formats = "rds",
      include_date = TRUE,
      version_tag = "tag",
      quiet = FALSE
    )
  )
})

# basic writes -----------------------------------------------------------------

testthat::test_that("writes rds and returns summary", {
  tmp <- withr::local_tempdir()
  res <- write_snt_data(
    obj = mtcars,
    path = tmp,
    data_name = "cars",
    file_formats = "rds",
    quiet = FALSE
  )

  testthat::expect_s3_class(res, "data.frame")
  testthat::expect_setequal(
    names(res),
    c("format", "path", "ok", "bytes", "hash", "message")
  )
  testthat::expect_true(res$ok[[1]])
  testthat::expect_true(fs::file_exists(res$path[[1]]))
  testthat::expect_true(res$bytes[[1]] > 0)
  testthat::expect_match(res$hash[[1]], "^[0-9a-f]+$")
})

testthat::test_that("csv is utf-8 and newline is set", {
  tmp <- withr::local_tempdir()
  df <- make_small_df()
  res <- write_snt_data(
    obj = df,
    path = tmp,
    data_name = "utf8",
    file_formats = "csv",
    quiet = FALSE
  )

  testthat::expect_true(res$ok[[1]])
  txt <- readChar(res$path[[1]], nchars = 1e6L, useBytes = TRUE)
  testthat::expect_true(grepl("école|façade|café", txt))
  # basic check that there are '\n' characters
  testthat::expect_true(grepl("\n", txt, fixed = TRUE))
})

testthat::test_that("tsv writer works", {
  tmp <- withr::local_tempdir()
  res <- write_snt_data(
    obj = head(iris),
    path = tmp,
    data_name = "iris",
    file_formats = "tsv",
    quiet = FALSE
  )
  testthat::expect_true(res$ok[[1]])
  testthat::expect_true(fs::file_exists(res$path[[1]]))
})

# excel ------------------------------------------------------------------------

testthat::test_that("xlsx writes when openxlsx is available", {
  testthat::skip_if_not(has_pkg("openxlsx"))
  testthat::skip_if_not(has_writer("xlsx"))

  tmp <- withr::local_tempdir()
  res <- write_snt_data(
    obj = list(a = head(mtcars), b = head(iris)),
    path = tmp,
    data_name = "wb",
    file_formats = "xlsx",
    quiet = FALSE
  )
  testthat::expect_true(res$ok[[1]])
  testthat::expect_true(fs::file_exists(res$path[[1]]))
})

# parquet/feather --------------------------------------------------------------

testthat::test_that("parquet writes when arrow is available", {
  testthat::skip_if_not(has_pkg("arrow"))
  testthat::skip_if_not(has_writer("parquet"))

  tmp <- withr::local_tempdir()
  res <- write_snt_data(
    obj = head(mtcars),
    path = tmp,
    data_name = "cars",
    file_formats = "parquet"
  )
  testthat::expect_true(res$ok[[1]])
})

testthat::test_that("feather writes when arrow is available", {
  testthat::skip_if_not(has_pkg("arrow"))
  testthat::skip_if_not(has_writer("feather"))

  tmp <- withr::local_tempdir()
  res <- write_snt_data(
    obj = head(mtcars),
    path = tmp,
    data_name = "cars",
    file_formats = "feather",
    quiet = FALSE
  )
  testthat::expect_true(res$ok[[1]])
})

# qs2 -------------------------------------------------------------------

testthat::test_that("qs2 writes when qs2 is available", {
  testthat::skip_if_not(has_pkg("qs2"))
  testthat::skip_if_not(has_writer("qs2"))

  tmp <- withr::local_tempdir()
  res <- write_snt_data(
    obj = head(mtcars),
    path = tmp,
    data_name = "cars",
    file_formats = "qs2",
    quiet = FALSE
  )
  testthat::expect_true(res$ok[[1]])
})

# sf handling ------------------------------------------------------------------

testthat::test_that("sf geometry is dropped for non-geo formats", {
  sfobj <- make_small_sf()
  testthat::skip_if(is.null(sfobj))

  tmp <- withr::local_tempdir()
  res <- write_snt_data(
    obj = sfobj,
    path = tmp,
    data_name = "pts",
    file_formats = "csv",
    quiet = FALSE
  )
  testthat::expect_true(res$ok[[1]])
  dat <- utils::read.csv(res$path[[1]], check.names = FALSE)
  testthat::expect_false(any(grepl("^geometry", names(dat))))
})

# overwrite and no-date behavior -----------------------------------------------

testthat::test_that("refuses overwrite when include_date = FALSE", {
  tmp <- withr::local_tempdir()

  res1 <- write_snt_data(
    obj = head(mtcars),
    path = tmp,
    data_name = "cars",
    file_formats = "rds",
    include_date = FALSE,
    quiet = FALSE
  )
  testthat::expect_true(res1$ok[[1]])

  err <- testthat::expect_error(
    write_snt_data(
      obj = head(mtcars),
      path = tmp,
      data_name = "cars",
      file_formats = "rds",
      include_date = FALSE,
      overwrite = FALSE,
      quiet = FALSE
    )
  )
  testthat::expect_true(grepl("overwrite", err$message, ignore.case = TRUE))
})

testthat::test_that(
  "allows overwrite when include_date = FALSE and overwrite = TRUE", {
  tmp <- withr::local_tempdir()

  # first write (file does not exist yet)
  res1 <- write_snt_data(
    obj = head(mtcars),
    path = tmp,
    data_name = "cars",
    file_formats = "rds",
    include_date = FALSE,
    quiet = FALSE
  )
  testthat::expect_true(res1$ok[[1]])
  testthat::expect_true(fs::file_exists(res1$path[[1]]))
  info1 <- fs::file_info(res1$path[[1]])

  # second write must set overwrite = TRUE
  res2 <- write_snt_data(
    obj = head(mtcars),
    path = tmp,
    data_name = "cars",
    file_formats = "rds",
    include_date = FALSE,
    overwrite = TRUE,
    quiet = FALSE
  )
  testthat::expect_true(res2$ok[[1]])
  testthat::expect_identical(res1$path[[1]], res2$path[[1]])
  testthat::expect_true(fs::file_exists(res2$path[[1]]))
  info2 <- fs::file_info(res2$path[[1]])

  # basic sanity: size present; mtime should be >= previous
  testthat::expect_gt(info2$size, 0)
  testthat::expect_true(info2$modification_time >= info1$modification_time)
})

# dedupe behavior --------------------------------------------------------------

testthat::test_that("dedupes identical dated versions with same-day writes", {
  tmp <- withr::local_tempdir()

  res1 <- write_snt_data(
    obj = head(mtcars),
    path = tmp,
    data_name = "cars",
    file_formats = "csv",
    include_date = TRUE,
    dedupe_identical = TRUE,
    quiet = FALSE
  )
  res2 <- write_snt_data(
    obj = head(mtcars),
    path = tmp,
    data_name = "cars",
    file_formats = "csv",
    include_date = TRUE,
    dedupe_identical = TRUE,
    quiet = FALSE
  )

  testthat::expect_true(all(c(res1$ok, res2$ok)))

  # match on basenames and only the expected pattern
  today <- format(Sys.Date(), "%Y-%m-%d")
  files <- fs::dir_ls(tmp, type = "file") |>
    fs::path_file() |>
    (\(x) x[grepl("^cars_v\\d{4}-\\d{2}-\\d{2}\\.csv$", x)])()

  testthat::expect_equal(length(files), 1L)
  testthat::expect_identical(files[[1]], paste0("cars_v", today, ".csv"))
})

# return structure and messages ------------------------------------------------

testthat::test_that("returns typed summary for multiple formats", {
  tmp <- withr::local_tempdir()

  fmts <- c("rds", "csv")
  res <- write_snt_data(
    obj = head(iris),
    path = tmp,
    data_name = "iris",
    file_formats = fmts,
    quiet = FALSE
  )

  testthat::expect_true(all(res$format %in% fmts))
  testthat::expect_true(all(res$ok))
  testthat::expect_true(all(fs::file_exists(res$path)))
  testthat::expect_true(all(res$bytes > 0))
  testthat::expect_true(all(grepl("^[0-9a-f]+$", res$hash)))
})


testthat::test_that("sidecar is created and well-formed", {
  sidecar_ready <- has_pkg("jsonlite") &&
    is.function(get0(".sidecar_path", mode = "function")) &&
    is.function(get0(".read_sidecar", mode = "function"))
  testthat::skip_if_not(sidecar_ready, "sidecar helpers missing")

  tmp <- withr::local_tempdir()
  df <- make_small_df()

  res <- write_snt_data(
    obj = df,
    path = tmp,
    data_name = "sc_basic",
    file_formats = "csv",
    quiet = TRUE
  )

  testthat::expect_s3_class(res, "data.frame")
  testthat::expect_true(res$ok[[1]])
  p <- res$path[[1]]
  sc <- .sidecar_path(p)

  testthat::expect_true(fs::file_exists(p))
  testthat::expect_true(fs::file_exists(sc))

  meta <- .read_sidecar(p)
  testthat::expect_type(meta, "list")
  testthat::expect_true(
    all(c("fmt", "data_name", "size", "obj_hash", "file_md5") %in% names(meta))
  )
  testthat::expect_identical(meta$fmt, "csv")
  testthat::expect_identical(meta$data_name, "sc_basic")
  testthat::expect_gt(meta$size, 0)
  testthat::expect_match(meta$obj_hash, "^[0-9a-f]+$")
  testthat::expect_match(meta$file_md5, "^[0-9a-f]+$")
})

testthat::test_that("cross-format dedupe removes older file + sidecar", {
  sidecar_ready <- has_pkg("jsonlite") &&
    is.function(get0(".sidecar_path", mode = "function")) &&
    is.function(get0(".read_sidecar", mode = "function"))
  testthat::skip_if_not(sidecar_ready, "sidecar helpers missing")

  tmp <- withr::local_tempdir()
  df <- make_small_df()

  # first write: csv (older)
  res1 <- write_snt_data(
    obj = df,
    path = tmp,
    data_name = "sc_cross",
    file_formats = "csv",
    quiet = TRUE
  )
  old_p <- res1$path[[1]]
  old_sc <- .sidecar_path(old_p)
  testthat::expect_true(fs::file_exists(old_p))
  testthat::expect_true(fs::file_exists(old_sc))

  # second write: rds (newer) should dedupe the csv
  res2 <- write_snt_data(
    obj = df,
    path = tmp,
    data_name = "sc_cross",
    file_formats = "rds",
    quiet = TRUE
  )
  new_p <- res2$path[[1]]
  testthat::expect_true(res2$ok[[1]])
  testthat::expect_true(fs::file_exists(new_p))

  # old csv and its sidecar removed
  testthat::expect_false(fs::file_exists(old_p))
  testthat::expect_false(fs::file_exists(old_sc))
})

testthat::test_that(
  "md5 fast path works if sidecar is missing (distinct versions)", {
  sidecar_ready <- is.function(get0(".sidecar_path", mode = "function"))
  testthat::skip_if_not(sidecar_ready, "sidecar helper missing")

  tmp <- withr::local_tempdir()
  df <- make_small_df()

  # first write: explicit tag "old" so path differs from the next write
  res1 <- write_snt_data(
    obj = df,
    path = tmp,
    data_name = "sc_md5",
    file_formats = "tsv",
    include_date = FALSE,
    version_tag = "old",
    quiet = TRUE
  )
  p_old <- res1$path[[1]]
  testthat::expect_true(res1$ok[[1]])
  testthat::expect_true(fs::file_exists(p_old))

  # remove sidecar to force md5 comparison path (no embedded obj hash)
  sc_old <- .sidecar_path(p_old)
  if (fs::file_exists(sc_old)) {
    fs::file_delete(sc_old)
  }

  # second write: same data, different tag -> a different file to compare
  res2 <- write_snt_data(
    obj = df,
    path = tmp,
    data_name = "sc_md5",
    file_formats = "tsv",
    include_date = FALSE,
    version_tag = "new",
    quiet = TRUE
  )
  p_new <- res2$path[[1]]
  testthat::expect_true(res2$ok[[1]])
  testthat::expect_true(fs::file_exists(p_new))

  # dedupe should have removed the old file (md5 fast path, same ext)
  testthat::expect_false(fs::file_exists(p_old))
})

testthat::test_that("obj_hash matches across formats for same data", {
  sidecar_ready <- has_pkg("jsonlite") &&
    is.function(get0(".sidecar_path", mode = "function")) &&
    is.function(get0(".read_sidecar", mode = "function"))
  testthat::skip_if_not(sidecar_ready, "sidecar helpers missing")

  tmp <- withr::local_tempdir()
  df <- make_small_df()

  # write both in one call so neither dedupes the other
  res <- write_snt_data(
    obj = df,
    path = tmp,
    data_name = "sc_both",
    file_formats = c("rds", "csv"),
    quiet = TRUE
  )
  testthat::expect_true(all(res$ok))

  p_rds <- res$path[res$format == "rds"][[1]]
  p_csv <- res$path[res$format == "csv"][[1]]

  sc_rds <- .read_sidecar(p_rds)
  sc_csv <- .read_sidecar(p_csv)

  testthat::expect_identical(sc_rds$obj_hash, sc_csv$obj_hash)
  testthat::expect_identical(sc_rds$data_name, "sc_both")
  testthat::expect_identical(sc_csv$data_name, "sc_both")
})

testthat::test_that("invisible return can still be captured", {
  tmp <- withr::local_tempdir()
  res <- (write_snt_data(
    obj = head(iris),
    path = tmp,
    data_name = "cap",
    file_formats = "rds",
    quiet = TRUE
  ))
  testthat::expect_s3_class(res, "data.frame")
  testthat::expect_true(res$ok[[1]])
})
