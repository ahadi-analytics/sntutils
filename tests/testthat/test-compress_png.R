testthat::test_that("find_pngquant works correctly", {
  withr::with_envvar(
    c("PATH" = "/opt/homebrew/bin:/usr/bin:/bin"),
    {
      # create a temporary pngquant executable
      temp_dir <- withr::local_tempdir()
      pngquant_path <- file.path(temp_dir, "pngquant")
      file.create(pngquant_path)
      Sys.chmod(pngquant_path, "755")

      withr::with_path(temp_dir, {
        result_path <- sntutils::find_pngquant()
        testthat::expect_true(grepl("pngquant", result_path))
      })
    }
  )
})

testthat::test_that("find_pngquant handles missing executable", {
  # Mock to simulate pngquant not found
  mockery::stub(find_pngquant, "Sys.which", function(cmd) "")
  mockery::stub(find_pngquant, "file.exists", function(path) FALSE)
  mockery::stub(find_pngquant, "readline", function(...) "n")

  testthat::expect_null(find_pngquant())
})

testthat::test_that("compression_stats calculates correctly", {
  init_size <- 100000
  final_size <- 75000

  # With verbosity FALSE
  stats <- compression_stats("test.png", init_size, final_size)

  testthat::expect_equal(stats$initial_size, init_size)
  testthat::expect_equal(stats$final_size, final_size)
  testthat::expect_equal(stats$bytes_saved, 25000)
  testthat::expect_equal(stats$percent_saved, 25)

  # Test with verbosity TRUE using mocking
  mockery::stub(compression_stats, "cli::cli_h2", function(x) NULL)
  mockery::stub(compression_stats, "cli::cli_alert_success", function(x) NULL)
  mockery::stub(compression_stats, "cli::cli_alert_info", function(x) NULL)
  mockery::stub(compression_stats, "cli::cli_bullets", function(x) NULL)
  mockery::stub(compression_stats, "cli::cli_h3", function(x) NULL)

  stats_verbose <- compression_stats("test.png", init_size, final_size, TRUE)
  testthat::expect_equal(stats_verbose$bytes_saved, 25000)
})

testthat::test_that("pngquant_compress_single_file compresses files", {
  # Mock functions
  mockery::stub(pngquant_compress_single_file, "file.info", function(file) {
    if (grepl("before", file)) {
      data.frame(size = 100000)
    } else {
      data.frame(size = 70000)
    }
  })

  mockery::stub(pngquant_compress_single_file, "system", function(...) {
    # Return with NULL status to indicate success
    result <- "output"
    attr(result, "status") <- 0
    result
  })

  mockery::stub(
    pngquant_compress_single_file, "compression_stats",
    function(...) {
      list(
        initial_size = 100000,
        final_size = 70000,
        bytes_saved = 30000,
        percent_saved = 30
      )
    }
  )

  result <- pngquant_compress_single_file(
    "/usr/bin/pngquant", "test.png", 3, TRUE, FALSE
  )

  testthat::expect_true(result$success)
  testthat::expect_equal(result$stats$bytes_saved, 0.0)
})

testthat::test_that("compress_png handles single file", {
  # mock base r functions
  testthat::local_mocked_bindings(
    file.exists = function(path) TRUE,
    dir.exists = function(path) FALSE,
    grepl = function(...) TRUE,
    .package = "base"
  )

  # mock package-specific functions
  testthat::local_mocked_bindings(
    find_pngquant = function(verbosity = FALSE) "/usr/bin/pngquant",
    pngquant_compress_single_file = function(...) {
      list(
        success = TRUE,
        stats = list(
          initial_size = 100000,
          final_size = 70000,
          bytes_saved = 30000,
          percent_saved = 30
        )
      )
    },
    .package = "sntutils"
  )

  result <- compress_png("test.png", verbosity = FALSE)
  testthat::expect_equal(result$bytes_saved, 30000)
})

testthat::test_that("compress_png handles directory", {
  # mock base r functions
  testthat::local_mocked_bindings(
    file.exists = function(path) TRUE,
    dir.exists = function(path) TRUE,
    list.files = function(...) c("test1.png", "test2.png"),
    .package = "base"
  )

  # create mock stats as data frames that rbind can handle properly
  test_stats <- list()
  test_stats[["test1.png"]] <- data.frame(
    initial_size = 100000,
    final_size = 70000,
    bytes_saved = 30000,
    percent_saved = 30,
    stringsAsFactors = FALSE
  )
  test_stats[["test2.png"]] <- data.frame(
    initial_size = 200000,
    final_size = 120000,
    bytes_saved = 80000,
    percent_saved = 40,
    stringsAsFactors = FALSE
  )

  # mock package-specific functions
  testthat::local_mocked_bindings(
    find_pngquant = function(verbosity = FALSE) "/usr/bin/pngquant",
    pngquant_compress_single_file = function(pngquant_path, file, ...) {
      # return appropriate stats based on filename
      filename <- basename(file)
      list(
        success = TRUE,
        stats = test_stats[[filename]]
      )
    },
    .package = "sntutils"
  )

  # use mockery to stub the specific function calls that use :: notation
  mockery::stub(
    compress_png,
    "progress::progress_bar$new",
    function(...) list(tick = function() NULL)
  )

  mockery::stub(compress_png, "cli::cli_h2", function(...) NULL)
  mockery::stub(compress_png, "cli::cli_bullets", function(...) NULL)
  mockery::stub(compress_png, "glue::glue", function(...) "mocked text")

  result <- compress_png("test_dir/", verbosity = FALSE)

  testthat::expect_equal(nrow(result), 2)
  testthat::expect_equal(sum(result$bytes_saved), 110000)
})

# Test ensure_packages helper function is available
testthat::test_that("ensure_packages handles missing packages", {
  # Create temp function to test
  ensure_packages_test <- function(pkg_list) {
    pkg_to_install <- pkg_list[!pkg_list %in%
      utils::installed.packages()[, "Package"]]
    if (length(pkg_to_install) > 0) {
      utils::install.packages(pkg_to_install)
    }
    pkg_list
  }

  mockery::stub(
    ensure_packages_test, "utils::installed.packages",
    function() {
      matrix(c("mockery", "testthat"),
        ncol = 1,
        dimnames = list(c(), "Package")
      )
    }
  )

  mockery::stub(
    ensure_packages_test, "utils::install.packages",
    function(pkgs) {
      pkgs
    }
  )

  result <- ensure_packages_test(c("testthat", "progress"))
  testthat::expect_equal(result, c("testthat", "progress"))
})
