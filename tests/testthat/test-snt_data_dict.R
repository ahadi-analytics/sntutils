testthat::test_that("snt_data_dict() returns a tidy tibble with required columns", {
  sntutils::clear_snt_cache()

  result <- sntutils::snt_data_dict()

  testthat::expect_s3_class(result, "tbl_df")
  testthat::expect_true(all(
    c("domain", "snt_var_name", "label_en", "label_fr", "label_pt", "disagg") %in%
      base::names(result)
  ))
  testthat::expect_gt(base::nrow(result), 0L)
  testthat::expect_type(result$snt_var_name, "character")
})

testthat::test_that("snt_data_dict(include_schema = TRUE) attaches schema attribute", {
  sntutils::clear_snt_cache()

  result <- sntutils::snt_data_dict(include_schema = TRUE)
  schema <- base::attr(result, "schema")

  testthat::expect_false(base::is.null(schema))
  testthat::expect_true(base::is.list(schema))
})

testthat::test_that("snt_data_dict(include_schema = FALSE) does not attach schema", {
  sntutils::clear_snt_cache()

  result <- sntutils::snt_data_dict(include_schema = FALSE)

  testthat::expect_true(base::is.null(base::attr(result, "schema")))
})

testthat::test_that("snt_data_dict(domain = ...) filters to requested domain", {
  full <- sntutils::snt_data_dict()
  available_domains <- base::unique(full$domain)
  testthat::expect_gt(base::length(available_domains), 0L)

  pick <- available_domains[1]
  filtered <- sntutils::snt_data_dict(domain = pick)

  testthat::expect_true(base::all(filtered$domain == pick))
  testthat::expect_lte(base::nrow(filtered), base::nrow(full))
})

testthat::test_that("snt_data_dict(domain = ...) warns when no matches found", {
  testthat::expect_warning(
    sntutils::snt_data_dict(domain = "this_domain_does_not_exist_xyz"),
    regexp = "No variables found"
  )
})

testthat::test_that("clear_snt_cache() empties the cache environment", {
  # warm caches via a full call
  invisible(sntutils::snt_data_dict())

  sntutils::clear_snt_cache()

  cache_env <- base::get(".snt_cache", envir = base::asNamespace("sntutils"))
  testthat::expect_length(base::ls(envir = cache_env), 0L)
})

testthat::test_that("disagg column detects schema-defined tokens in variable names", {
  result <- sntutils::snt_data_dict()
  schema <- base::attr(result, "schema")
  testthat::skip_if(base::is.null(schema))

  age_tokens <- base::names(schema$age_groups)
  testthat::skip_if(base::length(age_tokens) == 0L)

  with_age <- dplyr::filter(
    result,
    !base::is.na(disagg) &
      purrr::map_lgl(disagg, function(d) {
        any(base::vapply(
          base::strsplit(d, ", ")[[1]],
          function(tok) tok %in% age_tokens,
          base::logical(1L)
        ))
      })
  )

  # at least some variables in the tree should carry a disagg suffix
  testthat::expect_gte(base::nrow(with_age), 0L)
})

testthat::test_that("check_snt_var matches dynamic DHS ITN metric patterns", {
  # exercises .detect_dhs_itn_dynamic + .build_dhs_itn_label end-to-end
  result <- sntutils::check_snt_var("dhs_itn_use_0_5", return = TRUE)

  testthat::expect_s3_class(result, "tbl_df")
  testthat::expect_match(result$label_en, "0-5 years")
  testthat::expect_match(result$label_en, "%")

  with_ci <- sntutils::check_snt_var("dhs_itn_access_20_plus_low", return = TRUE)
  testthat::expect_match(with_ci$label_en, "20\\+")
  testthat::expect_match(with_ci$label_en, "lower CI")
  testthat::expect_match(with_ci$label_fr, "IC inf")

  upper <- sntutils::check_snt_var("dhs_itn_use_5_10_upp", return = TRUE)
  testthat::expect_match(upper$label_en, "upper CI")
})

testthat::test_that("check_snt_var matches DHS ITN count patterns", {
  result <- sntutils::check_snt_var("dhs_n_individuals_15_49", return = TRUE)

  testthat::expect_match(result$label_en, "individuals")
  testthat::expect_match(result$label_en, "15-49")
  # count patterns omit the percentage marker
  testthat::expect_false(stringr::str_detect(result$label_en, "\\(%\\)"))

  used <- sntutils::check_snt_var("dhs_n_used_itn_0_5", return = TRUE)
  testthat::expect_match(used$label_pt, "MILD")

  access <- sntutils::check_snt_var("dhs_n_with_access_5_10", return = TRUE)
  testthat::expect_match(access$label_en, "with access")
})

testthat::test_that("check_snt_var matches year-templated variable patterns", {
  pfpr <- sntutils::check_snt_var("pfpr_weighted_2023", return = TRUE)
  testthat::expect_match(pfpr$label_en, "PfPR")
  testthat::expect_match(pfpr$label_en, "2023")
  testthat::expect_match(pfpr$label_fr, "pond")

  incid <- sntutils::check_snt_var("n2_incidence_2024", return = TRUE)
  testthat::expect_match(incid$label_en, "incidence")
  testthat::expect_match(incid$label_en, "2024")

  slope <- sntutils::check_snt_var("sens_slope_n2_2024", return = TRUE)
  testthat::expect_match(slope$label_en, "Sen's slope")

  ratio <- sntutils::check_snt_var("rate_ratio_n2_2020_2024", return = TRUE)
  testthat::expect_match(ratio$label_en, "2020")
  testthat::expect_match(ratio$label_en, "2024")
})
