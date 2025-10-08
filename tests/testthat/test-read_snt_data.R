testthat::test_that("picks most recent mtime among versioned of same format", {
  tmp <- withr::local_tempdir()

  # older csv (v1) — different content to avoid dedupe
  df_old <- head(iris)
  df_new <- df_old
  df_new$Sepal.Length[1] <- df_new$Sepal.Length[1] + 0.1

  r1 <- write_snt_data(
    obj = df_old,
    path = tmp,
    data_name = "iris",
    file_formats = "csv",
    version_tag = "2025-01-01",
    include_date = FALSE,
    dedupe_identical = FALSE,
    quiet = TRUE
  )
  testthat::expect_true(r1$ok[[1]])

  r2 <- write_snt_data(
    obj = df_new,
    path = tmp,
    data_name = "iris",
    file_formats = "csv",
    version_tag = "2025-01-02",
    include_date = FALSE,
    dedupe_identical = FALSE,
    quiet = TRUE
  )
  testthat::expect_true(r2$ok[[1]])

  # Force mtimes: make v1 newer than v2 to ensure mtime wins over the tag
  Sys.setFileTime(r1$path[[1]], as.POSIXct("2025-01-03 09:00:00", tz = "UTC"))
  Sys.setFileTime(r2$path[[1]], as.POSIXct("2025-01-03 08:00:00", tz = "UTC"))

  got <- read_snt_data(path = tmp, data_name = "iris", file_formats = "csv")
  testthat::expect_true(is.data.frame(got))
  testthat::expect_true(
    abs(got$Sepal.Length[1] - df_old$Sepal.Length[1]) < 1e-9
  )
})

testthat::test_that("picks most recent mtime across formats when multiple allowed", {
  tmp <- withr::local_tempdir()

  df <- head(mtcars)

  # Write rds (older mtime)
  r_rds <- write_snt_data(
    obj = df,
    path = tmp,
    data_name = "cars",
    file_formats = "rds",
    version_tag = "v1",
    include_date = FALSE,
    dedupe_identical = FALSE,
    quiet = TRUE
  )
  testthat::expect_true(r_rds$ok[[1]])

  # Write csv (newer on tag but we'll make it older in mtime)
  r_csv <- write_snt_data(
    obj = df,
    path = tmp,
    data_name = "cars",
    file_formats = "csv",
    version_tag = "v2",
    include_date = FALSE,
    dedupe_identical = FALSE,
    quiet = TRUE
  )
  testthat::expect_true(r_csv$ok[[1]])

  # Set mtimes so RDS is the newest by time
  Sys.setFileTime(
    r_rds$path[[1]],
    as.POSIXct("2025-02-01 12:00:00", tz = "UTC")
  )
  Sys.setFileTime(
    r_csv$path[[1]],
    as.POSIXct("2025-02-01 11:59:00", tz = "UTC")
  )

  got <- read_snt_data(
    path = tmp,
    data_name = "cars",
    file_formats = c("csv", "rds"),
    quiet = TRUE
  )
  testthat::expect_true(is.data.frame(got))
  testthat::expect_equal(got, df)
})

testthat::test_that("falls back to unversioned when no versioned files exist", {
  tmp <- withr::local_tempdir()
  df <- head(iris)

  r <- write_snt_data(
    obj = df,
    path = tmp,
    data_name = "plain",
    file_formats = "tsv",
    include_date = FALSE,
    dedupe_identical = FALSE,
    overwrite = TRUE,
    quiet = TRUE
  )
  testthat::expect_true(r$ok[[1]])

  got <- read_snt_data(
    path = tmp,
    data_name = "plain",
    file_formats = "tsv",
    quiet = TRUE
  )
  testthat::expect_true(is.data.frame(got))
  # Reconstruct factor expected by the test (levels from original)
  got$Species <- factor(got$Species, levels = levels(df$Species))
  testthat::expect_equal(got, df)
})

testthat::test_that("errors on unsupported requested formats", {
  tmp <- withr::local_tempdir()
  testthat::expect_error(
    read_snt_data(path = tmp, data_name = "x", file_formats = "pdf"),
    "Unsupported format"
  )
})

testthat::test_that("errors when no matching files are present", {
  tmp <- withr::local_tempdir()
  testthat::expect_error(
    read_snt_data(path = tmp, data_name = "nothing", file_formats = "csv"),
    "No files found"
  )
})

testthat::test_that("tie on mtime resolves deterministically to one of the ties", {
  tmp <- withr::local_tempdir()
  df1 <- head(mtcars)
  df2 <- df1
  df2$mpg[1] <- df2$mpg[1] + 1

  r1 <- write_snt_data(
    obj = df1,
    path = tmp,
    data_name = "tie",
    file_formats = "rds",
    version_tag = "a",
    include_date = FALSE,
    dedupe_identical = FALSE,
    quiet = TRUE
  )
  r2 <- write_snt_data(
    obj = df2,
    path = tmp,
    data_name = "tie",
    file_formats = "csv",
    version_tag = "b",
    include_date = FALSE,
    dedupe_identical = FALSE,
    quiet = TRUE
  )
  testthat::expect_true(all(c(r1$ok[[1]], r2$ok[[1]])))

  # Same mtime
  t0 <- as.POSIXct("2025-03-01 00:00:00", tz = "UTC")
  Sys.setFileTime(r1$path[[1]], t0)
  Sys.setFileTime(r2$path[[1]], t0)

  got <- read_snt_data(
    path = tmp,
    data_name = "tie",
    file_formats = c("csv", "rds")
  )
  testthat::expect_true(is.data.frame(got))
  # Must equal one of the two objects
  ok1 <- identical(got, df1)
  ok2 <- identical(got, df2)
  testthat::expect_true(ok1 || ok2)
})

testthat::test_that("works when file_formats = NULL (any supported)", {
  tmp <- withr::local_tempdir()
  df <- head(iris)

  r_csv <- write_snt_data(
    obj = df,
    path = tmp,
    data_name = "any",
    file_formats = "csv",
    version_tag = "x1",
    include_date = FALSE,
    dedupe_identical = FALSE,
    quiet = TRUE
  )
  r_rds <- write_snt_data(
    obj = df,
    path = tmp,
    data_name = "any",
    file_formats = "rds",
    version_tag = "x2",
    include_date = FALSE,
    dedupe_identical = FALSE,
    quiet = TRUE
  )
  testthat::expect_true(all(c(r_csv$ok[[1]], r_rds$ok[[1]])))

  # Make RDS newest
  Sys.setFileTime(
    r_csv$path[[1]],
    as.POSIXct("2025-04-01 10:00:00", tz = "UTC")
  )
  Sys.setFileTime(
    r_rds$path[[1]],
    as.POSIXct("2025-04-01 10:01:00", tz = "UTC")
  )

  got <- read_snt_data(path = tmp, data_name = "any", file_formats = NULL)
  testthat::expect_true(is.data.frame(got))
  testthat::expect_equal(got, df)
})
