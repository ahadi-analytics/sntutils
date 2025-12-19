# test helpers ----------------------------------------------------------------

has_writer <- function(fmt) {
  tryCatch({
    reg <- sntutils:::.writer_registry()
    fmt %in% names(reg)
  }, error = function(e) FALSE)
}

has_pkg <- function(p) {
  requireNamespace(p, quietly = TRUE)
}

make_small_df <- function() {
  tibble::tibble(
    id = 1:3,
    name = c("école", "façade", "café"),
    when = as.Date("2025-01-01") + 0:2,
    flag = c(TRUE, FALSE, TRUE),
    grp = factor(c("a", "b", "a"))
  )
}

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

# input validation ------------------------------------------------------------

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
      data_name = "a/b",
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

# basic writing ---------------------------------------------------------------

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

# excel format ----------------------------------------------------------------

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

testthat::test_that(
  "longitude columns stay numeric in excel export",
  {
    testthat::skip_if_not(has_pkg("openxlsx"))
    testthat::skip_if_not(has_writer("xlsx"))

    tmp <- withr::local_tempdir()
    df <- tibble::tibble(
      lon = c(0, 1.222, 35.5),
      lat = c(-0.5, 0.2, 1.1)
    )

    res <- write_snt_data(
      obj = df,
      path = tmp,
      data_name = "coords",
      file_formats = "xlsx",
      include_date = FALSE,
      overwrite = TRUE,
      quiet = TRUE
    )

    testthat::expect_true(res$ok[[1]])
    read_back <- openxlsx::read.xlsx(res$path[[1]])
    testthat::expect_equal(read_back$lon, df$lon)
  }
)

# arrow formats ---------------------------------------------------------------

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

# qs2 format ------------------------------------------------------------------

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

# overwrite behavior ----------------------------------------------------------

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

# version management ----------------------------------------------------------

testthat::test_that("same-day versioned writes reuse the existing file", {
  tmp <- withr::local_tempdir()

  res1 <- write_snt_data(
    obj = head(mtcars),
    path = tmp,
    data_name = "cars",
    file_formats = "csv",
    include_date = TRUE,
    quiet = FALSE
  )
  res2 <- write_snt_data(
    obj = head(mtcars),
    path = tmp,
    data_name = "cars",
    file_formats = "csv",
    include_date = TRUE,
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

testthat::test_that("n_saved keeps only the newest versioned files", {
  tmp <- withr::local_tempdir()
  tags <- sprintf("2025-01-%02d", 1:4)
  old_path <- NULL

  for (i in seq_along(tags)) {
    res <- write_snt_data(
      obj = tibble::tibble(idx = i),
      path = tmp,
      data_name = "cars",
      file_formats = "csv",
      include_date = FALSE,
      version_tag = tags[[i]],
      n_saved = 3,
      quiet = TRUE
    )
    testthat::expect_true(res$ok[[1]])
    if (i == 1L) {
      old_path <- res$path[[1]]
    }
    Sys.setFileTime(
      res$path[[1]],
      as.POSIXct(sprintf("2025-01-%02d 12:00:00", i), tz = "UTC")
    )
  }

  files <- fs::dir_ls(tmp, regexp = "cars_v.*\\.csv$")
  basenames <- fs::path_file(files)

  testthat::expect_equal(length(files), 3L)
  testthat::expect_setequal(
    basenames,
    paste0("cars_v", tail(tags, 3), ".csv")
  )
  testthat::expect_false(fs::file_exists(old_path))
})

testthat::test_that("n_saved defaults to keeping all versions", {
  tmp <- withr::local_tempdir()
  tags <- c("old", "new")
  paths <- character(0)

  for (tag in tags) {
    res <- write_snt_data(
      obj = tibble::tibble(idx = tag),
      path = tmp,
      data_name = "cars",
      file_formats = "csv",
      include_date = FALSE,
      version_tag = tag,
      quiet = TRUE
    )
    testthat::expect_true(res$ok[[1]])
    paths <- c(paths, res$path[[1]])
  }

  testthat::expect_true(all(fs::file_exists(paths)))
})

# return values ---------------------------------------------------------------

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
