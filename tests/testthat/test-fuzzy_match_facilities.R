testthat::test_that("fuzzy_match_facilities handles full pipeline options", {
  cache_dir <- withr::local_tempdir("cache")
  save_dir <- withr::local_tempdir("save")

  target_df <- tibble::tibble(
    adm0 = base::rep("cty", 5L),
    adm1 = c("prov1", "prov1", "prov1", "prov1", "prov2"),
    adm2 = c("dist1", "dist1", "dist1", "dist1", "dist2"),
    hf = c(
      "Central Hospital",
      "Saint Mary's Clinic",
      "Eastside Hlth Post",
      NA_character_,
      "Remote Outpost"
    ),
    hf_uid = base::paste0("UID", 1:5)
  )

  lookup_df <- tibble::tibble(
    adm0 = base::rep("cty", 4L),
    adm1 = c("prov1", "prov1", "prov1", "prov2"),
    adm2 = c("dist1", "dist1", "dist1", "dist2"),
    hf = c(
      "Central Hospital",
      "Saint Marys Clinic",
      "Eastside Health Post",
      "Extra Lookup Only"
    ),
    geo_code = base::paste0("G00", 1:4)
  )

  steps <- c(exact = TRUE, interactive = FALSE, standardization = TRUE, fuzzy = TRUE)

  saved <- new.env(parent = base::emptyenv())

  stubbed <- fuzzy_match_facilities
  mockery::stub(
    stubbed,
    "sntutils::write_snt_data",
    function(obj, data_name, path, file_formats) {
      saved$obj <- obj
      saved$data_name <- data_name
      saved$path <- path
      saved$file_formats <- file_formats
      invisible(NULL)
    }
  )

  result <- stubbed(
    target_df = target_df,
    lookup_df = lookup_df,
    admin_cols = c("adm0", "adm1", "adm2"),
    hf_col_name = "hf",
    uid_col = "hf_uid",
    steps = steps,
    lookup_cols = "geo_code",
    match_interactivity = FALSE,
    fuzzy_methods = c("jw", "lv"),
    fuzzy_threshold = 85L,
    score_exact = 100L,
    score_standardization = 97L,
    score_interactive = 96L,
    status_cuts = c(high = 95, medium = 85, low = 70),
    include_missing_name_rows = TRUE,
    save_path = save_dir,
    matching_cache_path = cache_dir,
    save_stem = "synthetic_bundle",
    summary_language = c("en", "fr"),
    verbose = FALSE
  )

  testthat::expect_s3_class(result$results, "tbl_df")

  expected_methods <- base::sort(c(
    "exact_admin",
    "fuzzy",
    "missing_name",
    "unmatched"
  ))
  observed_methods <- base::sort(base::unique(result$results$match_method))
  testthat::expect_identical(observed_methods, expected_methods)

  std_row <- dplyr::filter(
    result$results,
    match_method == "standardization"
  )
  testthat::expect_true(
    base::all(stringr::str_detect(std_row$hf_target_raw, "Saint"))
  )

  fuzzy_row <- dplyr::filter(
    result$results,
    match_method == "fuzzy"
  )
  testthat::expect_true(
    base::all(stringr::str_detect(fuzzy_row$hf_mfl[1], "Eastside"))
  )

  missing_row <- dplyr::filter(
    result$results,
    match_method == "missing_name"
  )
  testthat::expect_equal(base::unique(missing_row$hf_uid), "UID4")

  unmatched_row <- dplyr::filter(
    result$results,
    match_method == "unmatched"
  )
  testthat::expect_equal(base::unique(unmatched_row$hf_uid), "UID5")

  testthat::expect_true(
    base::file.exists(
      base::file.path(save_dir, "synthetic_bundle_bundle.rds")
    )
  )

  testthat::expect_equal(saved$data_name, "synthetic_bundle")
  testthat::expect_equal(saved$path, save_dir)
  testthat::expect_identical(saved$file_formats, c("qs2", "xlsx"))
  testthat::expect_true("facility_matching_results" %in% names(saved$obj))

  testthat::expect_true("geo_code" %in% names(result$target_augmented))
  uid3_row <- dplyr::filter(result$target_augmented, hf_uid == "UID3")
  testthat::expect_equal(uid3_row$geo_code, "G003")

  testthat::expect_true(
    "MFL-only facilities" %in% result$summary_table$metric
  )
  mfl_only_value <- dplyr::filter(
    result$summary_table,
    metric == "MFL-only facilities"
  )$value
  testthat::expect_true(
    stringr::str_detect(mfl_only_value, "1 \\(25%\\)")
  )

  testthat::expect_equal(
    result$coverage_summary$mfl_only$count,
    1L
  )
  testthat::expect_equal(
    result$coverage_summary$mfl_only$pct_of_lookup,
    0.25,
    tolerance = 1e-6
  )

  testthat::expect_setequal(
    base::sort(result$target_augmented$hf_uid),
    c("UID1", "UID2", "UID3", "UID4", "UID5")
  )

  testthat::expect_identical(result$params$save_path, save_dir)
  testthat::expect_identical(result$params$lookup_cols, "geo_code")
  testthat::expect_identical(result$params$summary_language, c("en", "fr"))
})
