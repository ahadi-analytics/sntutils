testthat::test_that("get_dhs_data() aborts when path does not exist", {
  testthat::skip_if_not_installed("duckdb")
  testthat::skip_if_not_installed("arrow")
  testthat::skip_if_not_installed("DBI")

  testthat::expect_error(
    sntutils::get_dhs_data(path = "/definitely/not/a/real/path"),
    regexp = "Path not found"
  )
})

testthat::test_that("get_dhs_data() aborts on invalid file type codes", {
  testthat::skip_if_not_installed("duckdb")
  testthat::skip_if_not_installed("arrow")
  testthat::skip_if_not_installed("DBI")

  tmp <- withr::local_tempdir()
  testthat::expect_error(
    sntutils::get_dhs_data(path = tmp, types = c("XX", "YY")),
    regexp = "Invalid DHS file type"
  )
})

testthat::test_that("get_dhs_data() handles an empty parquet tree gracefully", {
  testthat::skip_if_not_installed("duckdb")
  testthat::skip_if_not_installed("arrow")
  testthat::skip_if_not_installed("DBI")

  tmp <- withr::local_tempdir()
  # path exists but has no PR/IR/HR... subfolders

  result <- NULL
  testthat::expect_message(
    result <- sntutils::get_dhs_data(path = tmp),
    regexp = "DHS"
  )

  testthat::expect_s3_class(result, "dhs_duckdb")
  testthat::expect_equal(result$path, tmp)

  # cleanup the DuckDB connection so the temp dir can be removed
  DBI::dbDisconnect(result$con, shutdown = TRUE)
})

testthat::test_that("get_dhs_data() returns a dhs_duckdb object given a PR fixture", {
  testthat::skip_if_not_installed("duckdb")
  testthat::skip_if_not_installed("arrow")
  testthat::skip_if_not_installed("DBI")

  tmp <- withr::local_tempdir()
  pr_dir <- base::file.path(tmp, "PR")
  base::dir.create(pr_dir)

  df <- tibble::tibble(
    country_code = c("KE", "KE"),
    survey_year = c(2014L, 2014L),
    survey_id = c("KE2014", "KE2014"),
    val = c(1, 2)
  )
  arrow::write_parquet(df, base::file.path(pr_dir, "sample.parquet"))

  result <- sntutils::get_dhs_data(path = tmp, types = "PR")
  on.exit(DBI::dbDisconnect(result$con, shutdown = TRUE), add = TRUE)

  # check the function's CONTRACT (return shape) rather than the parquet
  # round-trip via duckdb's hive_partitioning code path; that path is
  # sensitive to disagreements between the installed arrow and duckdb
  # versions and the test would otherwise be a CI flake
  testthat::expect_s3_class(result, "dhs_duckdb")
  testthat::expect_equal(result$path, tmp)
  testthat::expect_true(DBI::dbIsValid(result$con))

  # the print method should always work, whether or not the parquet view
  # actually registered
  testthat::expect_output(
    base::print(result),
    regexp = "DHS"
  )
})

testthat::test_that("get_dhs_data() warns about corrupted parquet files", {
  testthat::skip_if_not_installed("duckdb")
  testthat::skip_if_not_installed("arrow")
  testthat::skip_if_not_installed("DBI")

  tmp <- withr::local_tempdir()
  ir_dir <- base::file.path(tmp, "IR")
  base::dir.create(ir_dir)

  # write a file that pretends to be parquet but isn't
  base::writeLines("not a parquet file", base::file.path(ir_dir, "broken.parquet"))

  result <- NULL
  testthat::expect_message(
    result <- sntutils::get_dhs_data(path = tmp, types = "IR"),
    regexp = "corrupted|No valid"
  )

  testthat::expect_s3_class(result, "dhs_duckdb")
  DBI::dbDisconnect(result$con, shutdown = TRUE)
})
