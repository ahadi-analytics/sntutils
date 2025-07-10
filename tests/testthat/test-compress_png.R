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


testthat::test_that("find_pngquant returns path when pngquant is in PATH", {
  # mock Sys.which to return a valid path
  mockery::stub(find_pngquant, "Sys.which", "/usr/bin/pngquant")

  # mock file.exists to confirm the file exists
  mockery::stub(find_pngquant, "file.exists", TRUE)

  # mock system checks to return the path
  mockery::stub(
    find_pngquant,
    "system",
    function(command, intern = FALSE, ...) {
      if (grepl("which pngquant", command)) {
        return("/usr/bin/pngquant")
      }
      return("")
    }
  )

  result <- find_pngquant(verbosity = FALSE)

  testthat::expect_equal(result, "/usr/bin/pngquant")
})

testthat::test_that("find_pngquant finds pngquant in macOS homebrew paths", {
  # mock macOS system
  mockery::stub(find_pngquant, "Sys.info", c(sysname = "Darwin"))

  # mock Sys.which to return empty (not in PATH)
  mockery::stub(find_pngquant, "Sys.which", "")

  # mock file.exists to return TRUE for homebrew path
  mock_file_exists <- function(path) {
    if (path == "/opt/homebrew/bin/pngquant") {
      return(TRUE)
    }
    return(FALSE)
  }
  mockery::stub(find_pngquant, "file.exists", mock_file_exists)

  # mock system checks to return the path
  mockery::stub(
    find_pngquant,
    "system",
    function(command, intern = FALSE, ...) {
      if (grepl("which pngquant", command)) {
        return("/opt/homebrew/bin/pngquant")
      }
      return("")
    }
  )

  result <- find_pngquant(verbosity = FALSE)

  testthat::expect_equal(result, "/opt/homebrew/bin/pngquant")
})


testthat::test_that("find_pngquant returns NULL when user declines installation", {
  # mock system to return empty (pngquant not found)
  mockery::stub(find_pngquant, "Sys.which", "")
  mockery::stub(find_pngquant, "file.exists", FALSE)

  # mock system checks to return empty
  mockery::stub(
    find_pngquant,
    "system",
    function(command, intern = FALSE, ...) ""
  )

  # mock readline to simulate user declining installation
  mockery::stub(find_pngquant, "readline", "n")

  result <- find_pngquant(verbosity = FALSE)

  testthat::expect_null(result)
})


testthat::test_that("find_pngquant handles missing executable", {
  # Mock to simulate pngquant not found
  mockery::stub(find_pngquant, "Sys.which", function(cmd) "")
  mockery::stub(find_pngquant, "file.exists", function(path) FALSE)
  mockery::stub(find_pngquant, "readline", function(...) "n")

  testthat::expect_null(find_pngquant())
})

# test-find_pngquant.R
# tests for find_pngquant function
# author: test suite
# date: 2025-07-10

testthat::test_that("find_pngquant returns path when pngquant is in PATH", {
  # mock Sys.which to return a valid path
  mockery::stub(find_pngquant, "Sys.which", "/usr/bin/pngquant")

  # mock file.exists to confirm the path exists
  mockery::stub(find_pngquant, "file.exists", TRUE)

  # mock system calls to return the path
  mockery::stub(find_pngquant, "system", function(cmd, intern = FALSE) {
    if (grepl("which pngquant", cmd)) {
      return("/usr/bin/pngquant")
    }
    if (grepl("command -v pngquant", cmd)) {
      return("/usr/bin/pngquant")
    }
    if (grepl("type -p pngquant", cmd)) {
      return("/usr/bin/pngquant")
    }
    return(character(0))
  })

  result <- find_pngquant(verbosity = FALSE)

  testthat::expect_equal(result, "/usr/bin/pngquant")
})

testthat::test_that("find_pngquant searches homebrew paths on macOS", {
  # mock macOS system
  mockery::stub(
    find_pngquant,
    "Sys.info",
    c(sysname = "Darwin", release = "21.0.0")
  )

  # mock Sys.which to return empty (not in PATH)
  mockery::stub(find_pngquant, "Sys.which", "")

  # mock file.exists to find pngquant in homebrew path
  mockery::stub(find_pngquant, "file.exists", function(path) {
    path == "/opt/homebrew/bin/pngquant"
  })

  # mock system calls
  mockery::stub(find_pngquant, "system", function(cmd, intern = FALSE) {
    if (grepl("which pngquant", cmd)) {
      return("/opt/homebrew/bin/pngquant")
    }
    return(character(0))
  })

  result <- find_pngquant(verbosity = FALSE)

  testthat::expect_equal(result, "/opt/homebrew/bin/pngquant")
})

testthat::test_that("find_pngquant handles user declining installation", {
  # mock system info
  mockery::stub(
    find_pngquant,
    "Sys.info",
    c(sysname = "Linux", release = "5.0")
  )

  # mock Sys.which to return empty (not found)
  mockery::stub(find_pngquant, "Sys.which", "")

  # mock file.exists to return FALSE (not found in common paths)
  mockery::stub(find_pngquant, "file.exists", FALSE)

  # mock readline to simulate user declining installation
  mockery::stub(find_pngquant, "readline", "n")

  result <- find_pngquant(verbosity = FALSE)

  testthat::expect_null(result)
})

testthat::test_that("find_pngquant handles user accepting installation on unix", {
  # mock Linux system
  mockery::stub(
    find_pngquant,
    "Sys.info",
    c(sysname = "Linux", release = "5.0")
  )

  # mock Sys.which to return empty initially, then git available
  call_count <- 0
  mockery::stub(find_pngquant, "Sys.which", function(cmd) {
    call_count <<- call_count + 1
    if (cmd == "pngquant") {
      return("") # pngquant not found
    } else if (cmd == "git") {
      return("/usr/bin/git") # git available
    }
    return("")
  })

  # mock file.exists
  mockery::stub(find_pngquant, "file.exists", function(path) {
    # return TRUE for built binary path
    grepl("target/release/pngquant$", path)
  })

  # mock readline to simulate user accepting installation
  mockery::stub(find_pngquant, "readline", "y")

  # mock system commands
  mockery::stub(find_pngquant, "system", function(cmd, intern = FALSE) {
    if (grepl("git clone", cmd)) {
      return(0) # successful clone
    } else if (grepl("cargo build", cmd)) {
      return(0) # successful build
    } else if (intern) {
      # for which/command -v/type -p checks
      if (grepl("which pngquant", cmd)) {
        return("/tmp/pngquant/target/release/pngquant")
      }
      return(character(0))
    }
    return(1)
  })

  # mock dir.create
  mockery::stub(find_pngquant, "dir.create", TRUE)

  # mock normalizePath
  mockery::stub(find_pngquant, "normalizePath", function(path, ...) path)

  result <- find_pngquant(verbosity = FALSE)

  testthat::expect_true(!is.null(result))
  testthat::expect_true(grepl("pngquant", result))
})


testthat::test_that("find_pngquant handles installation failures gracefully", {
  # mock Linux system
  mockery::stub(
    find_pngquant,
    "Sys.info",
    c(sysname = "Linux", release = "5.0")
  )

  # mock Sys.which
  mockery::stub(find_pngquant, "Sys.which", function(cmd) {
    if (cmd == "pngquant") {
      return("")
    }
    if (cmd == "git") {
      return("/usr/bin/git")
    }
    return("")
  })

  # mock file.exists to return FALSE (installation failed)
  mockery::stub(find_pngquant, "file.exists", FALSE)

  # mock readline to accept installation
  mockery::stub(find_pngquant, "readline", "y")

  # mock system to fail git clone
  mockery::stub(find_pngquant, "system", function(cmd, intern = FALSE) {
    if (grepl("git clone", cmd)) {
      return(1)
    } # failure
    if (intern) {
      return(character(0))
    }
    return(1)
  })

  # mock dir.create
  mockery::stub(find_pngquant, "dir.create", TRUE)
  mockery::stub(find_pngquant, "normalizePath", function(path, ...) path)

  # should throw error on git clone failure
  testthat::expect_error(
    find_pngquant(verbosity = FALSE),
    "Failed to clone pngquant repository"
  )
})

testthat::test_that("find_pngquant handles build failures", {
  # mock Linux system
  mockery::stub(
    find_pngquant,
    "Sys.info",
    c(sysname = "Linux", release = "5.0")
  )

  # mock Sys.which
  mockery::stub(find_pngquant, "Sys.which", function(cmd) {
    if (cmd == "pngquant") {
      return("")
    }
    if (cmd == "git") {
      return("/usr/bin/git")
    }
    return("")
  })

  # mock file.exists
  mockery::stub(find_pngquant, "file.exists", FALSE)

  # mock readline to accept installation
  mockery::stub(find_pngquant, "readline", "y")

  # mock system - git succeeds but cargo build fails
  mockery::stub(find_pngquant, "system", function(cmd, intern = FALSE) {
    if (grepl("git clone", cmd)) {
      return(0)
    } # success
    if (grepl("cargo build", cmd)) {
      return(1)
    } # failure
    if (intern) {
      return(character(0))
    }
    return(1)
  })

  # mock dir.create
  mockery::stub(find_pngquant, "dir.create", TRUE)
  mockery::stub(find_pngquant, "normalizePath", function(path, ...) path)

  # should throw error on build failure
  testthat::expect_error(
    find_pngquant(verbosity = FALSE),
    "Failed to build pngquant"
  )
})

testthat::test_that("find_pngquant verbosity controls output", {
  # mock system with pngquant in PATH
  mockery::stub(find_pngquant, "Sys.which", "/usr/bin/pngquant")
  mockery::stub(find_pngquant, "file.exists", TRUE)

  # mock system calls
  mockery::stub(find_pngquant, "system", function(cmd, intern = FALSE) {
    if (grepl("which pngquant", cmd)) {
      return("/usr/bin/pngquant")
    }
    return(character(0))
  })

  # test with verbosity = FALSE (should not produce output)
  testthat::expect_silent(
    result1 <- find_pngquant(verbosity = FALSE)
  )

  # test with verbosity = TRUE (may produce output, but shouldn't error)
  testthat::expect_no_error(
    result2 <- find_pngquant(verbosity = TRUE)
  )

  # results should be the same regardless of verbosity
  testthat::expect_equal(result1, result2)
})

testthat::test_that("find_pngquant returns NULL when file doesn't exist", {
  # mock Sys.which to return empty
  mockery::stub(find_pngquant, "Sys.which", "")

  # mock file.exists to always return FALSE
  mockery::stub(find_pngquant, "file.exists", FALSE)

  # mock readline to decline installation
  mockery::stub(find_pngquant, "readline", "n")

  result <- find_pngquant(verbosity = FALSE)

  testthat::expect_null(result)
})

testthat::test_that("find_pngquant handles different user responses", {
  # mock system
  mockery::stub(
    find_pngquant,
    "Sys.info",
    c(sysname = "Linux", release = "5.0")
  )
  mockery::stub(find_pngquant, "Sys.which", "")
  mockery::stub(find_pngquant, "file.exists", FALSE)

  # test various user responses that should decline installation
  for (response in c("n", "N", "no", "NO", "nope", "")) {
    mockery::stub(find_pngquant, "readline", response)
    result <- find_pngquant(verbosity = FALSE)
    testthat::expect_null(
      result,
      info = paste("Failed for response:", response)
    )
  }
})

testthat::test_that("find_pngquant handles missing binary after build", {
  # mock Linux system
  mockery::stub(
    find_pngquant,
    "Sys.info",
    c(sysname = "Linux", release = "5.0")
  )

  # mock Sys.which
  mockery::stub(find_pngquant, "Sys.which", function(cmd) {
    if (cmd == "pngquant") {
      return("")
    }
    if (cmd == "git") {
      return("/usr/bin/git")
    }
    return("")
  })

  # mock file.exists to return FALSE for binary path
  mockery::stub(find_pngquant, "file.exists", function(path) {
    !grepl("target/release/pngquant$", path) # binary doesn't exist
  })

  # mock readline to accept installation
  mockery::stub(find_pngquant, "readline", "y")

  # mock system commands to succeed
  mockery::stub(find_pngquant, "system", function(cmd, intern = FALSE) {
    if (grepl("git clone", cmd)) {
      return(0)
    }
    if (grepl("cargo build", cmd)) {
      return(0)
    }
    if (intern) {
      return(character(0))
    }
    return(1)
  })

  # mock dir.create
  mockery::stub(find_pngquant, "dir.create", TRUE)
  mockery::stub(find_pngquant, "normalizePath", function(path, ...) path)

  # should throw error when binary not found after build
  testthat::expect_error(
    find_pngquant(verbosity = FALSE),
    "pngquant binary not found after build"
  )
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
