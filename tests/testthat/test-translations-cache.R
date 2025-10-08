# translation cache integration tests -----------------------------------------

testthat::test_that(
  "build_dictionary uses pre-seeded cache for EN->FR labels", {
  data_tbl <- tibble::tibble(
    chw = 1:2,
    hf  = 3:4
  )

  # english labels map
  label_map <- tibble::tibble(
    name  = c("chw", "hf"),
    label = c("Community health worker", "Health facility")
  )

  tmp <- withr::local_tempdir()
  labels_csv <- fs::path(tmp, "labels_en.csv")
  utils::write.csv(label_map, labels_csv, row.names = FALSE, fileEncoding = "UTF-8")

  # prepare pre-seeded cache directory
  cache_dir <- fs::path(tmp, "translations")
  fs::dir_create(cache_dir, recurse = TRUE)
  cache_file <- fs::path(cache_dir, "translation_cache.rds")

  cache_df <- data.frame(
    key = c(
      "Community health worker_fr_from_en",
      "Health facility_fr_from_en"
    ),
    text = c("ASBC", "FS"),
    original_text = c("Community health worker", "Health facility"),
    to_lang = c("fr", "fr"),
    from_lang = c("en", "en"),
    translated_time = format(Sys.time(), tz = "UTC", usetz = TRUE),
    name_of_creator = Sys.getenv("RSTUDIO_USER_IDENTITY"),
    stringsAsFactors = FALSE
  )
  saveRDS(cache_df, cache_file)

  dict <- build_dictionary(
    data = data_tbl,
    labels_path = labels_csv,
    language = "fr",
    trans_cache_path = cache_dir
  )

  out <- dict |>
    dplyr::select(variable, label_fr)

  expect <- tibble::tibble(
    variable = c("chw", "hf"),
    label_fr = c("ASBC", "FS")
  )

  testthat::expect_true(all(out$label_fr[match(expect$variable, out$variable)] ==
    expect$label_fr))

  # also validate vectorized translator against the same cache
  out_vec <- sntutils::translate_text_vec(
    c("Community health worker", "Health facility"),
    target_language = "fr",
    cache_path = cache_dir
  )
  testthat::expect_identical(out_vec, c("ASBC", "FS"))
})
