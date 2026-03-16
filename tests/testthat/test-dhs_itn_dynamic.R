testthat::test_that("check_snt_var recognizes dynamic DHS ITN age groups", {
  # test basic age range pattern
  res <- sntutils::check_snt_var("dhs_itn_use_0_5", return = TRUE)
  testthat::expect_equal(res$domain, "dhs")
  testthat::expect_match(res$label_en, "ITN use.*0-5 years")
  testthat::expect_match(res$label_fr, "Utilisation.*0-5 ans")

  # test open-ended age pattern
  res <- sntutils::check_snt_var("dhs_itn_access_20_plus", return = TRUE)
  testthat::expect_match(res$label_en, "ITN access.*20\\+ years")

  # test confidence intervals
  res <- sntutils::check_snt_var("dhs_itn_use_5_10_low", return = TRUE)
  testthat::expect_match(res$label_en, "lower CI")

  res <- sntutils::check_snt_var("dhs_itn_use_5_10_upp", return = TRUE)
  testthat::expect_match(res$label_en, "upper CI")

  # test count variables
  res <- sntutils::check_snt_var("dhs_n_individuals_15_49", return = TRUE)
  testthat::expect_match(res$label_en, "number of individuals.*15-49 years")

  # test use_if_access metric
  res <- sntutils::check_snt_var("dhs_itn_use_if_access_0_5", return = TRUE)
  testthat::expect_match(res$label_en, "ITN use.*access.*0-5 years")
})

testthat::test_that("dynamic age groups work in build_dictionary", {
  # create test data with dynamic age group variables
  test_data <- tibble::tibble(
    adm2 = c("District A", "District B"),
    dhs_itn_use_0_5 = c(0.65, 0.72),
    dhs_itn_access_20_plus = c(0.55, 0.61),
    dhs_n_individuals_5_10 = c(1250, 980)
  )

  dict <- sntutils::build_dictionary(test_data, language = "en")

  # check that dynamic variables are labeled
  testthat::expect_true("dhs_itn_use_0_5" %in% dict$variable)
  use_row <- dplyr::filter(dict, variable == "dhs_itn_use_0_5")
  testthat::expect_match(use_row$label_en, "0-5 years")

  access_row <- dplyr::filter(dict, variable == "dhs_itn_access_20_plus")
  testthat::expect_match(access_row$label_en, "20\\+ years")
})

testthat::test_that("backward compatibility with fixed age groups", {
  # existing hardcoded variables should still work
  res_u5 <- sntutils::check_snt_var("dhs_itn_use_u5", return = TRUE)
  testthat::expect_match(res_u5$label_en, "under 5|children")

  res_preg <- sntutils::check_snt_var("dhs_itn_use_preg", return = TRUE)
  testthat::expect_match(res_preg$label_en, "pregnant")

  # non-age-disaggregated metrics
  res_own <- sntutils::check_snt_var("dhs_itn_ownership", return = TRUE)
  testthat::expect_match(res_own$label_en, "ownership")
})
