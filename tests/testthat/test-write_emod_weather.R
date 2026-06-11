make_weather_df <- function(n_nodes = 3L, n_steps = 30L) {
  tibble::tibble(
    node_id = base::rep(base::seq_len(n_nodes), each = n_steps),
    steps = base::rep(base::seq_len(n_steps), times = n_nodes),
    airtemp = base::seq(20, 35, length.out = n_nodes * n_steps),
    humidity = base::seq(40, 90, length.out = n_nodes * n_steps),
    rainfall = base::seq(0, 5, length.out = n_nodes * n_steps),
    landtemp = base::seq(18, 33, length.out = n_nodes * n_steps)
  )
}

testthat::test_that("write_emod_weather() and read_emod_weather() round-trip", {
  testthat::skip_if_not_installed("jsonlite")

  tmp <- withr::local_tempdir()
  df_in <- make_weather_df(n_nodes = 3L, n_steps = 30L)

  testthat::expect_message(
    sntutils::write_emod_weather(
      df = df_in,
      weather_dir = tmp,
      climate_profile = "seasonal"
    ),
    regexp = "EMOD weather files written"
  )

  files <- base::list.files(tmp)
  testthat::expect_true(base::any(grepl("\\.bin$", files)))
  testthat::expect_true(base::any(grepl("\\.bin\\.json$", files)))

  result <- sntutils::read_emod_weather(tmp, weather_file_prefix = "seasonal")

  testthat::expect_named(result, c("data", "attributes"))
  testthat::expect_s3_class(result$data, "tbl_df")
  testthat::expect_named(
    result$data,
    c("node_id", "steps", "airtemp", "humidity", "rainfall", "landtemp")
  )

  # round-trip preserves values within float32 precision
  testthat::expect_equal(
    result$data$airtemp,
    df_in$airtemp,
    tolerance = 1e-4
  )
  testthat::expect_equal(
    base::sort(base::unique(result$data$node_id)),
    base::sort(base::unique(df_in$node_id))
  )
  testthat::expect_equal(base::max(result$data$steps), 30L)

  # metadata is filled in
  testthat::expect_equal(result$attributes$Metadata$NodeCount, 3L)
  testthat::expect_equal(result$attributes$Metadata$DatavalueCount, 30L)
})

testthat::test_that("write_emod_weather() works with empty climate_profile", {
  testthat::skip_if_not_installed("jsonlite")

  tmp <- withr::local_tempdir()
  df_in <- make_weather_df(n_nodes = 1L, n_steps = 12L)

  sntutils::write_emod_weather(
    df = df_in,
    weather_dir = tmp,
    climate_profile = ""
  )

  # files should not have a leading underscore when profile is empty
  files <- base::list.files(tmp, pattern = "\\.bin$")
  testthat::expect_gt(base::length(files), 0L)
  testthat::expect_false(base::any(base::startsWith(files, "_")))
})

testthat::test_that("read_emod_weather() aborts when json metadata is missing", {
  empty_dir <- withr::local_tempdir()
  testthat::expect_error(
    sntutils::read_emod_weather(empty_dir),
    regexp = "JSON metadata not found"
  )
})

testthat::test_that("write_emod_weather_by_adm2() writes one subdir per district", {
  testthat::skip_if_not_installed("jsonlite")
  testthat::skip_if_not_installed("purrr")

  tmp <- withr::local_tempdir()
  df_in <- make_weather_df(n_nodes = 3L, n_steps = 12L)
  node_coord <- tibble::tibble(
    node_id = c(1L, 2L, 3L),
    adm2 = c("Northern District", "central-region", "south_zone")
  )

  sntutils::write_emod_weather_by_adm2(
    df = df_in,
    node_coord = node_coord,
    weather_dir = tmp,
    folder_case = "title"
  )

  subdirs <- base::list.dirs(tmp, recursive = FALSE)
  testthat::expect_length(subdirs, 3L)

  # title-cased folder names with non-alphanumeric chars collapsed to "_"
  basenames <- base::basename(subdirs)
  testthat::expect_true(base::any(grepl("Northern_District", basenames)))
  testthat::expect_true(base::any(grepl("Central_Region", basenames)))
  testthat::expect_true(base::any(grepl("South_Zone", basenames)))

  # each subdir contains 4 .bin + 4 .bin.json files
  bin_count <- base::length(base::list.files(
    subdirs[1], pattern = "\\.bin$", recursive = FALSE
  ))
  testthat::expect_equal(bin_count, 4L)
})

testthat::test_that("write_emod_weather_by_adm2() supports upper and lower folder_case", {
  testthat::skip_if_not_installed("jsonlite")
  testthat::skip_if_not_installed("purrr")

  tmp_upper <- withr::local_tempdir()
  tmp_lower <- withr::local_tempdir()
  df_in <- make_weather_df(n_nodes = 2L, n_steps = 5L)
  node_coord <- tibble::tibble(
    node_id = c(1L, 2L),
    adm2 = c("North Region", "South Region")
  )

  sntutils::write_emod_weather_by_adm2(
    df = df_in,
    node_coord = node_coord,
    weather_dir = tmp_upper,
    folder_case = "upper"
  )
  sntutils::write_emod_weather_by_adm2(
    df = df_in,
    node_coord = node_coord,
    weather_dir = tmp_lower,
    folder_case = "lower"
  )

  testthat::expect_true(base::any(
    grepl("^[A-Z_0-9]+$", base::basename(base::list.dirs(tmp_upper, recursive = FALSE)))
  ))
  testthat::expect_true(base::any(
    grepl("^[a-z_0-9]+$", base::basename(base::list.dirs(tmp_lower, recursive = FALSE)))
  ))
})

testthat::test_that(".parse_node_offsets and .build_node_offsets_hex round-trip", {
  build <- base::get(".build_node_offsets_hex", envir = base::asNamespace("sntutils"))
  parse <- base::get(".parse_node_offsets", envir = base::asNamespace("sntutils"))

  node_ids <- c(1L, 42L, 1000L, 65535L)
  hex <- build(node_ids, n_steps = 365L)

  testthat::expect_equal(base::nchar(hex), base::length(node_ids) * 16L)
  testthat::expect_equal(parse(hex), node_ids)
})
