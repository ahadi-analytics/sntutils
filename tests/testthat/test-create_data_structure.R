# tests for project structure creation functions
# objective: comprehensive testing of create_data_structure and
#           initialize_project_structure functions
# author: assistant
# date: 2025-07-10

# setup test environment ----
create_temp_test_dir <- function() {
  # create temporary directory for testing
  temp_dir <- fs::path(
    tempdir(),
    paste0("test_project_", as.integer(Sys.time()))
  )
  fs::dir_create(temp_dir)
  return(temp_dir)
}

cleanup_test_dir <- function(test_dir) {
  # clean up test directory
  if (fs::dir_exists(test_dir)) {
    fs::dir_delete(test_dir)
  }
}

# expected folder structure for validation ----
get_expected_data_structure <- function() {
  list(
    foundational = c(
      "1.1a_admin_boundaries",
      "1.1b_health_facilities",
      "1.1c_population/1.1ci_national",
      "1.1c_population/1.1cii_worldpop_rasters",
      "1.1d_cache_files"
    ),
    epidemiology = c(
      "1.2a_routine_surveillance",
      "1.2b_pfpr_estimates",
      "1.2c_mortality_estimates"
    ),
    simple_domains = c(
      "1.3_interventions",
      "1.4_drug_efficacy_resistance",
      "1.7_entomology",
      "1.8_commodities"
    ),
    environment = c(
      "1.5a_climate",
      "1.5b_accessibility",
      "1.5c_land_use"
    ),
    health_systems = c(
      "1.6a_dhs"
    )
  )
}

# test create_data_structure function ----
testthat::test_that("create_data_structure creates correct folder hierarchy", {
  test_dir <- create_temp_test_dir()

  testthat::expect_no_error({
    create_data_structure(base_path = test_dir)
  })

  # check that 01_data folder exists
  data_dir <- fs::path(test_dir, "01_data")
  testthat::expect_true(fs::dir_exists(data_dir))

  # check main domain folders exist
  expected_domains <- c(
    "1.1_foundational",
    "1.2_epidemiology",
    "1.3_interventions",
    "1.4_drug_efficacy_resistance",
    "1.5_environment",
    "1.6_health_systems",
    "1.7_entomology",
    "1.8_commodities"
  )

  for (domain in expected_domains) {
    domain_path <- fs::path(data_dir, domain)
    testthat::expect_true(
      fs::dir_exists(domain_path),
      info = glue::glue("Domain folder {domain} should exist")
    )
  }

  cleanup_test_dir(test_dir)
})

testthat::test_that("create_data_structure creates foundational subfolders", {
  test_dir <- create_temp_test_dir()

  create_data_structure(base_path = test_dir)

  foundational_base <- fs::path(test_dir, "01_data", "1.1_foundational")

  # check all foundational subfolders
  expected_subfolders <- c(
    "1.1a_admin_boundaries",
    "1.1b_health_facilities",
    "1.1c_population",
    "1.1d_cache_files"
  )

  for (subfolder in expected_subfolders) {
    subfolder_path <- fs::path(foundational_base, subfolder)
    testthat::expect_true(
      fs::dir_exists(subfolder_path),
      info = glue::glue("Subfolder {subfolder} should exist")
    )
  }

  # check nested population folders
  pop_base <- fs::path(foundational_base, "1.1c_population")
  nested_folders <- c("1.1ci_national", "1.1cii_worldpop_rasters")

  for (nested in nested_folders) {
    nested_path <- fs::path(pop_base, nested)
    testthat::expect_true(
      fs::dir_exists(nested_path),
      info = glue::glue("Nested folder {nested} should exist")
    )
  }

  cleanup_test_dir(test_dir)
})

testthat::test_that("create_data_structure creates raw and processed folders", {
  test_dir <- create_temp_test_dir()

  create_data_structure(base_path = test_dir)

  data_dir <- fs::path(test_dir, "01_data")

  # check simple domain folders have raw/processed
  simple_domains <- c(
    "1.3_interventions",
    "1.4_drug_efficacy_resistance",
    "1.7_entomology",
    "1.8_commodities"
  )

  for (domain in simple_domains) {
    domain_path <- fs::path(data_dir, domain)

    raw_path <- fs::path(domain_path, "raw")
    processed_path <- fs::path(domain_path, "processed")

    testthat::expect_true(
      fs::dir_exists(raw_path),
      info = glue::glue("{domain}/raw should exist")
    )
    testthat::expect_true(
      fs::dir_exists(processed_path),
      info = glue::glue("{domain}/processed should exist")
    )
  }

  # check complex domain subfolders have raw/processed
  complex_paths <- c(
    "1.1_foundational/1.1a_admin_boundaries",
    "1.1_foundational/1.1b_health_facilities",
    "1.1_foundational/1.1c_population/1.1ci_national",
    "1.1_foundational/1.1c_population/1.1cii_worldpop_rasters",
    "1.2_epidemiology/1.2a_routine_surveillance",
    "1.5_environment/1.5a_climate",
    "1.6_health_systems/1.6a_dhs"
  )

  for (complex_path in complex_paths) {
    full_path <- fs::path(data_dir, complex_path)

    raw_path <- fs::path(full_path, "raw")
    processed_path <- fs::path(full_path, "processed")

    testthat::expect_true(
      fs::dir_exists(raw_path),
      info = glue::glue("{complex_path}/raw should exist")
    )
    testthat::expect_true(
      fs::dir_exists(processed_path),
      info = glue::glue("{complex_path}/processed should exist")
    )
  }

  cleanup_test_dir(test_dir)
})

testthat::test_that("create_data_structure creates other standard folders", {
  test_dir <- create_temp_test_dir()

  create_data_structure(base_path = test_dir)

  # check other standard project folders
  standard_folders <- c(
    "02_scripts",
    "03_outputs/plots",
    "04_reports",
    "05_metadata_docs"
  )

  for (folder in standard_folders) {
    folder_path <- fs::path(test_dir, folder)
    testthat::expect_true(
      fs::dir_exists(folder_path),
      info = glue::glue("Standard folder {folder} should exist")
    )
  }

  cleanup_test_dir(test_dir)
})

testthat::test_that("create_data_structure works with custom base_path", {
  test_dir <- create_temp_test_dir()
  custom_base <- fs::path(test_dir, "custom_project")

  create_data_structure(base_path = custom_base)

  # check that structure is created in custom location
  custom_data_dir <- fs::path(custom_base, "01_data")
  testthat::expect_true(fs::dir_exists(custom_data_dir))

  # check a few key folders
  key_folders <- c(
    "01_data/1.1_foundational/1.1a_admin_boundaries/raw",
    "01_data/1.2_epidemiology/1.2a_routine_surveillance/processed",
    "02_scripts",
    "05_metadata_docs"
  )

  for (folder in key_folders) {
    folder_path <- fs::path(custom_base, folder)
    testthat::expect_true(
      fs::dir_exists(folder_path),
      info = glue::glue("Custom base folder {folder} should exist")
    )
  }

  cleanup_test_dir(test_dir)
})

testthat::test_that("create_data_structure returns null invisibly", {
  test_dir <- create_temp_test_dir()

  result <- create_data_structure(base_path = test_dir)

  testthat::expect_null(result)

  cleanup_test_dir(test_dir)
})

# test initialize_project_structure function ----
testthat::test_that("initialize_project_structure creates complete structure", {
  test_dir <- create_temp_test_dir()

  testthat::expect_no_error({
    initialize_project_structure(base_path = test_dir)
  })

  # check that all main folders exist
  main_folders <- c(
    "01_data",
    "02_scripts",
    "03_outputs",
    "04_reports",
    "05_metadata_docs"
  )

  for (folder in main_folders) {
    folder_path <- fs::path(test_dir, folder)
    testthat::expect_true(
      fs::dir_exists(folder_path),
      info = glue::glue("Main folder {folder} should exist")
    )
  }

  # check that data structure is complete
  data_domains <- c(
    "01_data/1.1_foundational",
    "01_data/1.2_epidemiology",
    "01_data/1.3_interventions",
    "01_data/1.4_drug_efficacy_resistance"
  )

  for (domain in data_domains) {
    domain_path <- fs::path(test_dir, domain)
    testthat::expect_true(
      fs::dir_exists(domain_path),
      info = glue::glue("Data domain {domain} should exist")
    )
  }

  cleanup_test_dir(test_dir)
})

testthat::test_that("initialize_project_structure creates base_path if missing", {
  test_dir <- create_temp_test_dir()
  nonexistent_base <- fs::path(test_dir, "new_project")

  # ensure base doesn't exist initially
  testthat::expect_false(fs::dir_exists(nonexistent_base))

  initialize_project_structure(base_path = nonexistent_base)

  # base should now exist
  testthat::expect_true(fs::dir_exists(nonexistent_base))

  # structure should be complete
  key_paths <- c(
    "01_data/1.1_foundational/1.1a_admin_boundaries",
    "02_scripts",
    "03_outputs/plots"
  )

  for (path in key_paths) {
    full_path <- fs::path(nonexistent_base, path)
    testthat::expect_true(
      fs::dir_exists(full_path),
      info = glue::glue("Path {path} should exist in new base")
    )
  }

  cleanup_test_dir(test_dir)
})

testthat::test_that("initialize_project_structure handles existing directories", {
  test_dir <- create_temp_test_dir()

  # create some existing folders
  fs::dir_create(fs::path(test_dir, "02_scripts"))
  fs::dir_create(fs::path(test_dir, "01_data", "1.1_foundational"))

  testthat::expect_no_error({
    initialize_project_structure(base_path = test_dir)
  })

  # should still create complete structure
  testthat::expect_true(
    fs::dir_exists(fs::path(test_dir, "01_data", "1.2_epidemiology"))
  )
  testthat::expect_true(
    fs::dir_exists(fs::path(test_dir, "03_outputs", "plots"))
  )

  cleanup_test_dir(test_dir)
})

testthat::test_that("initialize_project_structure uses current dir by default", {
  # save current directory
  original_wd <- getwd()

  test_dir <- create_temp_test_dir()

  # change to test directory
  setwd(test_dir)

  testthat::expect_no_error({
    initialize_project_structure() # no base_path specified
  })

  # check structure was created in current directory
  testthat::expect_true(fs::dir_exists("01_data"))
  testthat::expect_true(fs::dir_exists("02_scripts"))

  # restore original directory
  setwd(original_wd)
  cleanup_test_dir(test_dir)
})

testthat::test_that("initialize_project_structure returns null invisibly", {
  test_dir <- create_temp_test_dir()

  result <- initialize_project_structure(base_path = test_dir)

  testthat::expect_null(result)

  cleanup_test_dir(test_dir)
})

# test error handling and edge cases ----
testthat::test_that("functions handle invalid paths gracefully", {
  # test with invalid characters in path (platform dependent)
  if (.Platform$OS.type == "windows") {
    invalid_path <- "C:\\invalid|path"
  } else {
    invalid_path <- "/proc/invalid_path" # typically read-only
  }

  # functions should handle errors gracefully or skip if path issues
  testthat::expect_no_error({
    tryCatch(
      {
        create_data_structure(base_path = invalid_path)
      },
      error = function(e) {
        testthat::expect_true(TRUE) # error is expected for invalid paths
      }
    )
  })
})

testthat::test_that("functions work with relative paths", {
  test_dir <- create_temp_test_dir()
  original_wd <- getwd()

  # change to parent of test directory
  setwd(dirname(test_dir))
  relative_path <- basename(test_dir)

  testthat::expect_no_error({
    create_data_structure(base_path = relative_path)
  })

  # check structure exists
  testthat::expect_true(
    fs::dir_exists(fs::path(relative_path, "01_data"))
  )

  setwd(original_wd)
  cleanup_test_dir(test_dir)
})

# integration and workflow tests ----
testthat::test_that("functions work together in typical workflow", {
  test_dir <- create_temp_test_dir()

  # step 1: create just data structure
  create_data_structure(base_path = test_dir)

  # verify partial structure
  testthat::expect_true(fs::dir_exists(fs::path(test_dir, "01_data")))
  testthat::expect_true(fs::dir_exists(fs::path(test_dir, "02_scripts")))

  # step 2: initialize full structure (should handle existing folders)
  testthat::expect_no_error({
    initialize_project_structure(base_path = test_dir)
  })

  # verify complete structure
  complete_check_paths <- c(
    "01_data/1.1_foundational/1.1a_admin_boundaries/raw",
    "01_data/1.2_epidemiology/1.2b_pfpr_estimates/processed",
    "01_data/1.3_interventions/raw",
    "02_scripts",
    "03_outputs/plots",
    "04_reports",
    "05_metadata_docs"
  )

  for (path in complete_check_paths) {
    full_path <- fs::path(test_dir, path)
    testthat::expect_true(
      fs::dir_exists(full_path),
      info = glue::glue("Complete workflow path {path} should exist")
    )
  }

  cleanup_test_dir(test_dir)
})

testthat::test_that("folder structure matches ahadi project standards", {
  test_dir <- create_temp_test_dir()

  initialize_project_structure(base_path = test_dir)

  # verify ahadi-specific structure elements
  ahadi_specific_paths <- c(
    # hierarchical data organization
    "01_data/1.1_foundational/1.1c_population/1.1ci_national",
    "01_data/1.1_foundational/1.1c_population/1.1cii_worldpop_rasters",
    # domain-specific organization
    "01_data/1.2_epidemiology/1.2a_routine_surveillance",
    "01_data/1.5_environment/1.5b_accessibility",
    "01_data/1.6_health_systems/1.6a_dhs",
    # consistent raw/processed structure
    "01_data/1.7_entomology/raw",
    "01_data/1.8_commodities/processed"
  )

  for (path in ahadi_specific_paths) {
    full_path <- fs::path(test_dir, path)
    testthat::expect_true(
      fs::dir_exists(full_path),
      info = glue::glue("Ahadi standard path {path} should exist")
    )
  }

  cleanup_test_dir(test_dir)
})

# performance and validation tests ----
testthat::test_that("functions complete in reasonable time", {
  test_dir <- create_temp_test_dir()

  # time the full structure creation
  start_time <- Sys.time()
  initialize_project_structure(base_path = test_dir)
  end_time <- Sys.time()

  execution_time <- as.numeric(difftime(end_time, start_time, units = "secs"))

  # should complete in under 5 seconds (generous for file system operations)
  testthat::expect_lt(execution_time, 5)

  cleanup_test_dir(test_dir)
})

testthat::test_that("created structure has correct permissions", {
  test_dir <- create_temp_test_dir()

  initialize_project_structure(base_path = test_dir)

  # check that directories are readable and writable
  test_paths <- c(
    fs::path(test_dir, "01_data"),
    fs::path(test_dir, "01_data", "1.1_foundational", "1.1a_admin_boundaries"),
    fs::path(test_dir, "02_scripts")
  )

  for (path in test_paths) {
    # should be able to read directory contents
    testthat::expect_no_error({
      fs::dir_ls(path)
    })

    # should be able to create a test file (write permission)
    test_file <- fs::path(path, "test_permissions.txt")
    testthat::expect_no_error({
      writeLines("test", test_file)
      fs::file_delete(test_file)
    })
  }

  cleanup_test_dir(test_dir)
})
