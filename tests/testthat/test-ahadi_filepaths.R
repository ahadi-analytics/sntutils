ahadi_ns <- function(name) {
  base::get(name, envir = base::asNamespace("sntutils"))
}

testthat::test_that("ahadi_clean_string lowercases and strips non-alphanumeric", {
  clean <- ahadi_ns("ahadi_clean_string")

  testthat::expect_equal(clean("AHADI Information - technical"), "ahadiinformationtechnical")
  testthat::expect_equal(clean("Foo (Bar) 123"), "foobar123")
  testthat::expect_equal(clean(NULL), "")
  testthat::expect_equal(clean(""), "")
  testthat::expect_equal(clean(42), "42")
})

testthat::test_that("ahadi_fuzzy_match returns exact match first", {
  fuzzy <- ahadi_ns("ahadi_fuzzy_match")
  clean <- ahadi_ns("ahadi_clean_string")

  cands <- c("AHADI Information - technical", "Other Library", "Another Lib")
  idx <- fuzzy(cands, clean("AHADI information technical"))
  testthat::expect_equal(idx, 1L)
})

testthat::test_that("ahadi_fuzzy_match falls back to substring match", {
  fuzzy <- ahadi_ns("ahadi_fuzzy_match")
  clean <- ahadi_ns("ahadi_clean_string")

  cands <- c("AHADI Information - technical", "Other Library")
  idx <- fuzzy(cands, clean("Information technical"))
  testthat::expect_equal(idx, 1L)
})

testthat::test_that("ahadi_fuzzy_match returns NA when nothing matches", {
  fuzzy <- ahadi_ns("ahadi_fuzzy_match")
  clean <- ahadi_ns("ahadi_clean_string")

  cands <- c("Foo", "Bar")
  idx <- fuzzy(cands, clean("Completely Different"))
  testthat::expect_true(base::is.na(idx))
})

testthat::test_that("ahadi_fuzzy_match returns NA on empty candidates", {
  fuzzy <- ahadi_ns("ahadi_fuzzy_match")
  testthat::expect_true(base::is.na(fuzzy(base::character(0L), "anything")))
})

testthat::test_that("ahadi_find_onedrive_roots returns existing dirs on macOS", {
  testthat::skip_if(Sys.info()[["sysname"]] != "Darwin")

  find <- ahadi_ns("ahadi_find_onedrive_roots")
  roots <- find(os_name = "Darwin")
  testthat::expect_type(roots, "character")
  # at minimum, the user's home should exist
  testthat::expect_true(base::all(base::dir.exists(roots)))
})

testthat::test_that("ahadi_find_onedrive_roots errors on Windows without USERNAME", {
  testthat::skip_if(Sys.info()[["sysname"]] == "Windows")
  find <- ahadi_ns("ahadi_find_onedrive_roots")

  withr::with_envvar(c(USERNAME = ""), {
    testthat::expect_error(
      find(os_name = "Windows"),
      regexp = "Windows username"
    )
  })
})

testthat::test_that("ahadi_path resolves a mocked OneDrive layout", {
  # build a fake "OneDrive shared library" tree
  root <- withr::local_tempdir()
  shared <- base::file.path(
    root,
    "OneDrive-SharedLibraries-Applied Health Analytics for Delivery and Innovation Inc"
  )
  library_dir <- base::file.path(shared, "AHADI Information - technical")
  base_dir <- base::file.path(library_dir, "Documentation per topic/data/dhs_data")
  base::dir.create(base_dir, recursive = TRUE)

  # write a marker file so we can verify the relative join
  marker <- base::file.path(base_dir, "marker.txt")
  base::writeLines("ok", marker)

  # mock OneDrive root discovery to point at our temp tree
  mock_find_roots <- function(os_name, verbose = FALSE) root
  mockery::stub(
    sntutils::ahadi_path,
    "ahadi_find_onedrive_roots",
    mock_find_roots
  )

  withr::with_options(list(ahadi.onedrive.cache = NULL), {
    base_returned <- sntutils::ahadi_path(refresh = TRUE)
    testthat::expect_true(base::dir.exists(base_returned))

    full <- sntutils::ahadi_path("marker.txt", refresh = TRUE)
    testthat::expect_true(base::file.exists(full))
    testthat::expect_equal(base::readLines(full), "ok")
  })
})

testthat::test_that("ahadi_path uses cached library root on subsequent calls", {
  root <- withr::local_tempdir()
  shared <- base::file.path(
    root,
    "OneDrive-SharedLibraries-AHADI"
  )
  library_dir <- base::file.path(shared, "AHADI Information - technical")
  base_dir <- base::file.path(library_dir, "Documentation per topic/data/dhs_data")
  base::dir.create(base_dir, recursive = TRUE)

  mock_find_roots <- function(os_name, verbose = FALSE) root
  mockery::stub(
    sntutils::ahadi_path,
    "ahadi_find_onedrive_roots",
    mock_find_roots
  )

  # prime cache then call again without refresh — should reuse it
  withr::with_options(list(ahadi.onedrive.cache = NULL), {
    invisible(sntutils::ahadi_path(refresh = TRUE))
    cached <- base::getOption("ahadi.onedrive.cache")

    testthat::expect_false(base::is.null(cached))
    testthat::expect_true(base::dir.exists(cached$library_root))

    # second call should hit the cache branch (verbose = TRUE for coverage)
    testthat::expect_no_error(
      sntutils::ahadi_path(verbose = TRUE)
    )
  })
})

testthat::test_that("ahadi_path errors when base folder does not exist", {
  root <- withr::local_tempdir()
  shared <- base::file.path(root, "OneDrive-SharedLibraries-AHADI")
  library_dir <- base::file.path(shared, "AHADI Information - technical")
  base::dir.create(library_dir, recursive = TRUE)
  # note: do NOT create the base sub-folder

  mockery::stub(
    sntutils::ahadi_path,
    "ahadi_find_onedrive_roots",
    function(os_name, verbose = FALSE) root
  )

  withr::with_options(list(ahadi.onedrive.cache = NULL), {
    testthat::expect_error(
      sntutils::ahadi_path(refresh = TRUE),
      regexp = "Base folder"
    )
  })
})

testthat::test_that("ahadi_path errors when no OneDrive roots are found", {
  empty_root <- withr::local_tempdir()
  # contents: nothing — no shared-libraries-style folder

  mockery::stub(
    sntutils::ahadi_path,
    "ahadi_find_onedrive_roots",
    function(os_name, verbose = FALSE) base::character(0L)
  )

  withr::with_options(list(ahadi.onedrive.cache = NULL), {
    testthat::expect_error(
      sntutils::ahadi_path(refresh = TRUE),
      regexp = "OneDrive"
    )
  })
})

testthat::test_that("ahadi_path errors on unsupported OS", {
  withr::with_options(list(ahadi.onedrive.cache = NULL), {
    # we can't easily mock Sys.info(); instead exercise the
    # downstream path-not-found error in a controlled way.
    testthat::skip_if(Sys.info()[["sysname"]] %in% c("Darwin", "Windows"),
      "Test is meaningful only on unsupported OSes."
    )
    testthat::expect_error(
      sntutils::ahadi_path(refresh = TRUE),
      regexp = "Unsupported"
    )
  })
})
