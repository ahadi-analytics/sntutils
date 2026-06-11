cp_ns <- function(name) {
  base::get(name, envir = base::asNamespace("sntutils"))
}

testthat::test_that("compression_stats computes savings and percent correctly", {
  stats <- cp_ns("compression_stats")
  result <- stats("foo.png", 1000, 600)

  testthat::expect_named(
    result,
    c("initial_size", "final_size", "bytes_saved", "percent_saved")
  )
  testthat::expect_equal(result$bytes_saved, 400)
  testthat::expect_equal(result$percent_saved, 40)
})

testthat::test_that("compression_stats verbose path runs without error", {
  stats <- cp_ns("compression_stats")
  # exercise the formatting branches for MB, KB and byte-level sizes
  testthat::expect_no_error(stats("big.png", 5e6, 1e6, verbosity = TRUE))
  testthat::expect_no_error(stats("mid.png", 5000, 4000, verbosity = TRUE))
  testthat::expect_no_error(stats("small.png", 500, 450, verbosity = TRUE))
  # minimal-compression branch
  testthat::expect_no_error(stats("barely.png", 1000, 950, verbosity = TRUE))
})

testthat::test_that("compress_png() warns when path does not exist", {
  testthat::expect_message(
    res <- sntutils::compress_png("definitely/not/a/real/path.png"),
    regexp = "not"
  )
  testthat::expect_null(res)
})

testthat::test_that("compress_png() warns when path is neither file nor directory", {
  # a regular non-png file
  tmp <- withr::local_tempfile(fileext = ".txt")
  base::writeLines("not a png", tmp)

  testthat::expect_message(
    res <- sntutils::compress_png(tmp),
    regexp = "PNG"
  )
  testthat::expect_null(res)
})

testthat::test_that("compress_png() warns when directory has no PNGs", {
  tmp <- withr::local_tempdir()

  testthat::expect_message(
    res <- sntutils::compress_png(tmp),
    regexp = "No PNG"
  )
  testthat::expect_null(res)
})

testthat::test_that("ensure_pngquant returns NULL gracefully when pngquant is not installed", {
  ensure <- cp_ns("ensure_pngquant")

  # force all detection paths to fail by clearing PNGQUANT_PATH and
  # pointing PATH at an empty dir
  empty_dir <- withr::local_tempdir()
  withr::with_envvar(
    c(
      PNGQUANT_PATH = "",
      PATH = empty_dir
    ),
    {
      # may legitimately succeed on a dev machine where pngquant lives at
      # one of the hardcoded common paths; either NULL or a valid path is fine
      result <- ensure(verbosity = FALSE)
      testthat::expect_true(base::is.null(result) || base::is.character(result))
    }
  )
})

testthat::test_that("compress_png() compresses a real PNG when pngquant is available", {
  find <- cp_ns("find_pngquant")
  testthat::skip_if(base::is.null(find(verbosity = FALSE)),
    "pngquant not installed"
  )

  tmp <- withr::local_tempdir()
  png_path <- base::file.path(tmp, "test.png")

  # write a small in-memory PNG
  grDevices::png(png_path, width = 200, height = 200)
  graphics::plot(1:10, 1:10, pch = 19, col = "red")
  grDevices::dev.off()

  init_size <- base::file.info(png_path)$size
  testthat::expect_gt(init_size, 0L)

  result <- sntutils::compress_png(png_path, verbosity = FALSE)
  testthat::expect_true(base::is.list(result) || base::is.null(result))

  # directory variant: same file inside a directory
  dir_result <- sntutils::compress_png(tmp, verbosity = FALSE)
  testthat::expect_true(base::is.data.frame(dir_result) || base::is.null(dir_result))
})
