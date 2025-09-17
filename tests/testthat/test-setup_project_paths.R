# tests/testthat/test-setup_project_paths.R

has_pkg <- function(p) {
  suppressWarnings(requireNamespace(p, quietly = TRUE))
}

exp_names <- c(
  "core",
  # 1.1_foundational
  "admin_shp",
  "physical_features",
  "hf",
  "chw",
  "pop_national",
  "pop_worldpop",
  "cache",
  # 1.2_epidemiology
  "dhis2",
  "pfpr_est",
  "mortality_est",
  # 1.3_interventions
  "interventions",
  # 1.4_drug_efficacy_resistance
  "drug_eff",
  # 1.5_environment
  "climate",
  "accessibility",
  "land_use",
  # 1.6_health_systems
  "dhs",
  # 1.7_entomology
  "ento",
  # 1.8_commodities
  "commodities",
  # outputs
  "validation_plots",
  "validation_tables"
)

testthat::test_that("returns list with expected names and absolute paths", {
  tmp <- withr::local_tempdir()
  paths <- setup_project_paths(base_path = tmp, create = FALSE, quiet = TRUE)

  testthat::expect_type(paths, "list")
  testthat::expect_setequal(names(paths), exp_names)

  # all are single-length character and absolute
  lens <- vapply(paths, length, integer(1))
  testthat::expect_true(all(lens == 1L))
  all_chr <- vapply(paths, is.character, logical(1))
  testthat::expect_true(all(all_chr))
  abs_ok <- vapply(paths, fs::is_absolute_path, logical(1))
  testthat::expect_true(all(abs_ok))
})

testthat::test_that("create = TRUE makes the directories", {
  tmp <- withr::local_tempdir()
  paths <- setup_project_paths(base_path = tmp, create = TRUE, quiet = TRUE)

  # spot-check a few deep dirs plus ensure all exist
  testthat::expect_true(fs::dir_exists(paths$admin_shp))
  testthat::expect_true(fs::dir_exists(paths$chw))
  testthat::expect_true(fs::dir_exists(paths$pop_worldpop))
  testthat::expect_true(fs::dir_exists(paths$validation_plots))
  testthat::expect_true(all(fs::dir_exists(unlist(paths))))
})

testthat::test_that("create = FALSE does not create directories", {
  tmp <- withr::local_tempdir()
  paths <- setup_project_paths(base_path = tmp, create = FALSE, quiet = TRUE)

  # Should *not* exist yet (function only returns paths)
  testthat::expect_false(fs::dir_exists(paths$validation_tables))
  testthat::expect_false(fs::dir_exists(paths$admin_shp))
  testthat::expect_false(fs::dir_exists(paths$chw))

  # Creating afterwards should succeed
  fs::dir_create(paths$validation_tables, recurse = TRUE)
  testthat::expect_true(fs::dir_exists(paths$validation_tables))
})

testthat::test_that("sets useful project options", {
  tmp <- withr::local_tempdir()
  paths <- setup_project_paths(base_path = tmp, create = TRUE, quiet = TRUE)

  opt_labels <- getOption("snt.labels_en_path")
  opt_paths <- getOption("snt.paths")

  testthat::expect_true(is.character(opt_labels) && length(opt_labels) == 1L)
  testthat::expect_identical(
    opt_labels,
    fs::path(paths$cache, "labels_en.csv")
  )
  testthat::expect_true(is.list(opt_paths))
  testthat::expect_identical(opt_paths, paths)
})

testthat::test_that("idempotent and stable across repeated calls", {
  tmp <- withr::local_tempdir()

  p1 <- setup_project_paths(base_path = tmp, create = TRUE, quiet = TRUE)
  p2 <- setup_project_paths(base_path = tmp, create = TRUE, quiet = TRUE)

  testthat::expect_identical(p1, p2)
  testthat::expect_true(all(fs::dir_exists(unlist(p1))))
})

testthat::test_that("base_path normalization produces absolute core", {
  tmp <- withr::local_tempdir()
  nested <- fs::path(tmp, ".", "project_root")

  # Not created yet; function will still return absolute path
  paths <- setup_project_paths(base_path = nested, create = FALSE, quiet = TRUE)

  testthat::expect_true(fs::is_absolute_path(paths$core))
  testthat::expect_identical(paths$core, fs::path_abs(nested))
})

testthat::test_that("quiet = TRUE is silent; quiet = FALSE emits messages", {
  tmp <- withr::local_tempdir()

  # Silent creation
  testthat::expect_silent(
    setup_project_paths(base_path = tmp, create = TRUE, quiet = TRUE)
  )

  # With messages — we don't assert exact content, just that it runs
  # (cli usually writes to stdout; this ensures it doesn't error)
  setup_project_paths(base_path = tmp, create = TRUE, quiet = FALSE)

  # Still fine to call again (idempotent)
  setup_project_paths(base_path = tmp, create = FALSE, quiet = FALSE)
})

testthat::test_that(
  "does not rely on here()/rprojroot when base_path provided", {
  # even if those packages are not installed, passing base_path must work
  tmp <- withr::local_tempdir()
  paths <- setup_project_paths(base_path = tmp, create = FALSE, quiet = TRUE)
  testthat::expect_identical(paths$core, fs::path_abs(tmp))
})
