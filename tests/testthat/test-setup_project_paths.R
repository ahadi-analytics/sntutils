# tests/testthat/test-setup_project_paths.R

has_pkg <- function(p) {
  suppressWarnings(requireNamespace(p, quietly = TRUE))
}

exp_names <- c(
  "core",
  # 1.1_foundational
  "admin_shp",
  "physical_feat",
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
  # 1.9_finance
  "finance",
  # 1.10_final
  "final_data",
  # outputs - parent directories
  "val",
  "interm",
  "final",
  # outputs - figures and tables
  "val_fig",
  "val_tbl",
  "interm_fig",
  "interm_tbl",
  "final_fig",
  "final_tbl"
)

testthat::test_that("returns list with expected names and absolute paths", {
  tmp <- withr::local_tempdir()
  paths <- setup_project_paths(base_path = tmp, quiet = TRUE)

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

testthat::test_that("does not create directories automatically", {
  tmp <- withr::local_tempdir()
  paths <- setup_project_paths(base_path = tmp, quiet = TRUE)

  # should *not* exist yet (function only returns paths)
  testthat::expect_false(fs::dir_exists(paths$val_tbl))
  testthat::expect_false(fs::dir_exists(paths$admin_shp))
  testthat::expect_false(fs::dir_exists(paths$chw))

  # creating afterwards should succeed
  fs::dir_create(paths$val_tbl, recurse = TRUE)
  testthat::expect_true(fs::dir_exists(paths$val_tbl))
})

testthat::test_that("initialize_project_structure creates directories", {
  tmp <- withr::local_tempdir()
  initialize_project_structure(base_path = tmp)
  paths <- setup_project_paths(base_path = tmp, quiet = TRUE)

  # spot-check a few deep dirs plus ensure key ones exist
  testthat::expect_true(fs::dir_exists(paths$admin_shp))
  testthat::expect_true(fs::dir_exists(paths$chw))
  testthat::expect_true(fs::dir_exists(paths$pop_worldpop))
  testthat::expect_true(fs::dir_exists(paths$val_fig))
  testthat::expect_true(fs::dir_exists(paths$interm))
  testthat::expect_true(fs::dir_exists(paths$final))
})

testthat::test_that("sets useful project options", {
  tmp <- withr::local_tempdir()
  paths <- setup_project_paths(base_path = tmp, quiet = TRUE)

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

  p1 <- setup_project_paths(base_path = tmp, quiet = TRUE)
  p2 <- setup_project_paths(base_path = tmp, quiet = TRUE)

  testthat::expect_identical(p1, p2)
})

testthat::test_that("base_path normalization produces absolute core", {

  tmp <- withr::local_tempdir()
  nested <- fs::path(tmp, "project_root")

  # not created yet; function will still return absolute path
  paths <- setup_project_paths(base_path = nested, quiet = TRUE)

  testthat::expect_true(fs::is_absolute_path(paths$core))
  # compare basename to avoid symlink resolution differences on macOS
  testthat::expect_identical(basename(paths$core), "project_root")
  testthat::expect_true(grepl(basename(tmp), paths$core))
})

testthat::test_that("quiet = TRUE is silent; quiet = FALSE emits warnings", {
  tmp <- withr::local_tempdir()

  # silent when no folders exist
  testthat::expect_silent(
    setup_project_paths(base_path = tmp, quiet = TRUE)
  )

  # with messages when folders missing and quiet = FALSE
  # (cli usually writes to stdout; this ensures it doesn't error)
  testthat::expect_message(
    setup_project_paths(base_path = tmp, quiet = FALSE),
    "Missing"
  )
})

testthat::test_that(
  "does not rely on here()/rprojroot when base_path provided", {
  # even if those packages are not installed, passing base_path must work
  tmp <- withr::local_tempdir()
  paths <- setup_project_paths(base_path = tmp, quiet = TRUE)
  # compare basename to avoid symlink resolution differences on macOS
  testthat::expect_identical(basename(paths$core), basename(tmp))
  testthat::expect_true(fs::is_absolute_path(paths$core))
})
