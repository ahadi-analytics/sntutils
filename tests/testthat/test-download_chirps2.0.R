# setup test environment ----
skip_if_no_internet <- function() {
  # helper to skip tests if no internet connection
  testthat::skip_if_not(curl::has_internet(), "No internet connection")
}

skip_if_chirps_down <- function() {
  # helper to skip if chirps server is down
  testthat::skip_if_not({
    tryCatch({
      response <- httr2::request("https://data.chc.ucsb.edu/") |>
        httr2::req_timeout(10) |>
        httr2::req_perform()
      !httr2::resp_is_error(response)
    }, error = function(e) FALSE)
  }, "CHIRPS server is not accessible")
}

create_temp_download_dir <- function() {
  # create temporary directory for download testing
  temp_dir <- fs::path(tempdir(), paste0("chirps_test_",
                                        as.integer(Sys.time())))
  fs::dir_create(temp_dir)
  return(temp_dir)
}

cleanup_temp_dir <- function(temp_dir) {
  # clean up test directory
  if (fs::dir_exists(temp_dir)) {
    fs::dir_delete(temp_dir)
  }
}

# test chirps_options function ----
testthat::test_that("chirps_options returns correct structure", {
  result <- chirps_options()

  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_s3_class(result, "tbl_df")

  # check required columns exist
  expected_cols <- c("dataset", "frequency", "label", "subdir")
  testthat::expect_true(all(expected_cols %in% names(result)))
  testthat::expect_equal(ncol(result), length(expected_cols))
})

testthat::test_that("chirps_options contains expected datasets", {
  result <- chirps_options()

  # check that expected datasets are present
  expected_datasets <- c(
    "global_monthly", "africa_monthly",
    "camer-carib_monthly", "EAC_monthly"
  )

  testthat::expect_true(all(expected_datasets %in% result$dataset))
  testthat::expect_equal(nrow(result), length(expected_datasets))
})

testthat::test_that("chirps_options has consistent data structure", {
  result <- chirps_options()

  # all datasets should be monthly for now
  testthat::expect_true(all(result$frequency == "monthly"))

  # all subdirectories should end with "/tifs"
  testthat::expect_true(all(stringr::str_ends(result$subdir, "/tifs")))

  # dataset codes should match subdir prefixes
  for (i in seq_len(nrow(result))) {
    expected_prefix <- stringr::str_remove(result$subdir[i], "/tifs$")
    testthat::expect_equal(result$dataset[i], expected_prefix)
  }

  # labels should be descriptive and contain "Monthly"
  testthat::expect_true(all(stringr::str_detect(result$label, "Monthly")))

  # no missing values
  testthat::expect_false(any(is.na(result)))
})

# test check_chirps_available function ----
testthat::test_that("check_chirps_available returns correct structure", {
  skip_if_no_internet()
  skip_if_chirps_down()

  result <- check_chirps_available("africa_monthly")

  if (!is.null(result)) {
    testthat::expect_s3_class(result, "data.frame")
    testthat::expect_s3_class(result, "tbl_df")

    # check required columns exist
    expected_cols <- c("file_name", "year", "month", "dataset")
    testthat::expect_true(all(expected_cols %in% names(result)))

    # should have some data
    testthat::expect_gt(nrow(result), 0)
  }
})

testthat::test_that("check_chirps_available extracts years correctly", {
  skip_if_no_internet()
  skip_if_chirps_down()

  result <- check_chirps_available("africa_monthly")

  if (!is.null(result) && nrow(result) > 0) {
    # years should be 4-digit numbers
    years <- as.numeric(result$year)
    testthat::expect_true(all(!is.na(years)))
    testthat::expect_true(all(years >= 1981)) # chirps starts 1981
    testthat::expect_true(all(years <= as.numeric(format(Sys.Date(), "%Y"))))

    # should be character in the tibble
    testthat::expect_true(is.character(result$year))
  }
})

testthat::test_that("check_chirps_available extracts months correctly", {
  skip_if_no_internet()
  skip_if_chirps_down()

  result <- check_chirps_available("africa_monthly")

  if (!is.null(result) && nrow(result) > 0) {
    # months should be 2-digit strings 01-12
    months <- result$month[!is.na(result$month)]
    if (length(months) > 0) {
      month_nums <- as.numeric(months)
      testthat::expect_true(all(month_nums >= 1))
      testthat::expect_true(all(month_nums <= 12))
      testthat::expect_true(all(stringr::str_length(months) == 2))
    }
  }
})

testthat::test_that("check_chirps_available filters tif.gz files correctly", {
  skip_if_no_internet()
  skip_if_chirps_down()

  result <- check_chirps_available("africa_monthly")

  if (!is.null(result) && nrow(result) > 0) {
    # all file names should end with .tif.gz
    testthat::expect_true(all(stringr::str_ends(result$file_name, "\\.tif\\.gz")))

    # dataset column should match input
    testthat::expect_true(all(result$dataset == "africa_monthly"))
  }
})

testthat::test_that("check_chirps_available handles different datasets", {
  skip_if_no_internet()
  skip_if_chirps_down()

  # test with different valid dataset
  result_global <- check_chirps_available("global_monthly")
  result_eac <- check_chirps_available("EAC_monthly")

  if (!is.null(result_global) && nrow(result_global) > 0) {
    testthat::expect_true(all(result_global$dataset == "global_monthly"))
  }

  if (!is.null(result_eac) && nrow(result_eac) > 0) {
    testthat::expect_true(all(result_eac$dataset == "EAC_monthly"))
  }
})

testthat::test_that("check_chirps_available handles invalid dataset", {
  skip_if_no_internet()

  result <- check_chirps_available("invalid_dataset")

  # should return null for invalid dataset
  testthat::expect_null(result)
})

testthat::test_that("check_chirps_available data is properly sorted", {
  skip_if_no_internet()
  skip_if_chirps_down()

  result <- check_chirps_available("africa_monthly")

  if (!is.null(result) && nrow(result) > 1) {
    # should be sorted by year (desc) then month
    years <- as.numeric(result$year)
    testthat::expect_true(all(diff(years) <= 0)) # descending or equal

    # within same year, months should be in order
    for (year in unique(years)) {
      year_data <- result[result$year == as.character(year), ]
      if (nrow(year_data) > 1) {
        months <- as.numeric(year_data$month[!is.na(year_data$month)])
        if (length(months) > 1) {
          testthat::expect_true(all(diff(months) >= 0)) # ascending or equal
        }
      }
    }
  }
})

# test download_chirps2.0 function ----
testthat::test_that("download_chirps2.0 validates dataset parameter", {
  temp_dir <- create_temp_download_dir()

  # invalid dataset should throw error
  testthat::expect_error(
    download_chirps2.0(
      dataset = "invalid_dataset",
      start = "2020-01",
      out_dir = temp_dir
    ),
    regexp = "Invalid dataset"
  )

  cleanup_temp_dir(temp_dir)
})


testthat::test_that("download_chirps2.0 handles date range download", {
  skip_if_no_internet()
  skip_if_chirps_down()

  temp_dir <- create_temp_download_dir()

  testthat::expect_no_error({
    download_chirps2.0(
      dataset = "africa_monthly",
      start = "2020-01",
      end = "2020-02", # two months
      out_dir = temp_dir,
      unzip = FALSE
    )
  })

  # should have files for both months
  files <- fs::dir_ls(temp_dir, glob = "*.tif.gz")
  testthat::expect_gte(length(files), 2)

  # check that both months are represented
  if (length(files) >= 2) {
    filenames <- fs::path_file(files)
    has_jan <- any(stringr::str_detect(filenames, "2020\\.01"))
    has_feb <- any(stringr::str_detect(filenames, "2020\\.02"))
    testthat::expect_true(has_jan)
    testthat::expect_true(has_feb)
  }

  cleanup_temp_dir(temp_dir)
})


testthat::test_that("download_chirps2.0 skips existing files", {
  skip_if_no_internet()
  skip_if_chirps_down()

  temp_dir <- create_temp_download_dir()

  # first download
  download_chirps2.0(
    dataset = "africa_monthly",
    start = "2020-01",
    out_dir = temp_dir,
    unzip = TRUE
  )

  initial_files <- fs::dir_ls(temp_dir)
  initial_count <- length(initial_files)

  # second download should skip existing files
  testthat::expect_no_error({
    download_chirps2.0(
      dataset = "africa_monthly",
      start = "2020-01",
      out_dir = temp_dir,
      unzip = TRUE
    )
  })

  final_files <- fs::dir_ls(temp_dir)
  final_count <- length(final_files)

  # file count should be the same (no duplicates created)
  testthat::expect_equal(final_count, initial_count)

  cleanup_temp_dir(temp_dir)
})

testthat::test_that("download_chirps2.0 handles different datasets", {
  skip_if_no_internet()
  skip_if_chirps_down()

  temp_dir <- create_temp_download_dir()

  # test with global dataset
  testthat::expect_no_error({
    download_chirps2.0(
      dataset = "EAC_monthly",
      start = "2020-01",
      out_dir = temp_dir,
      unzip = FALSE
    )
  })

  files <- fs::dir_ls(temp_dir, glob = "*.tif.gz")

  if (length(files) > 0) {
    filename <- fs::path_file(files[1])
    testthat::expect_true(
      stringr::str_detect(filename, "EAC_monthly_chirps")
    )
  }

  cleanup_temp_dir(temp_dir)
})

# test error handling and edge cases ----
testthat::test_that("functions handle network errors gracefully", {
  skip_if_no_internet()

  # test with invalid url (should handle gracefully)
  result <- check_chirps_available("nonexistent_dataset")
  testthat::expect_null(result)
})

testthat::test_that("download_chirps2.0 handles invalid dates", {
  temp_dir <- create_temp_download_dir()

  # malformed date should cause error
  testthat::expect_error({
    download_chirps2.0(
      dataset = "africa_monthly",
      start = "invalid-date",
      out_dir = temp_dir
    )
  })

  cleanup_temp_dir(temp_dir)
})
