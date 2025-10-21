testthat::test_that("check_snt_var correctly parses and labels SNT variables", {
  testthat::skip_if_not_installed("yaml")

  # Load schema from YAML (for reference)
  schema_path <- base::system.file(
    "inst",
    "extdata",
    "var_tree.yml",
    package = "sntutils"
  )
  testthat::skip_if_not(base::file.exists(schema_path))

  # Basic example: malaria confirmed, over-5, private sector, community level
  res <- sntutils::check_snt_var("conf_ov5_priv_chw", return = TRUE)

  testthat::expect_s3_class(res, "tbl_df")
  testthat::expect_named(
    res,
    c(
      "var_name",
      "domain",
      "test_type",
      "service_level",
      "age_group",
      "population_group",
      "sector",
      "label_en",
      "label_fr",
      "label_pt"
    )
  )
  testthat::expect_equal(res$domain, "conf")
  testthat::expect_equal(res$age_group, "ov5")
  testthat::expect_equal(res$sector, "priv")
  testthat::expect_equal(res$service_level, "chw")

  # English label
  testthat::expect_true(stringr::str_detect(
    res$label_en,
    "Confirmed malaria cases - 5 years and above \\[Private sector\\] \\(Community level \\(CHW\\)\\)"
  ))

  # French label
  testthat::expect_true(stringr::str_detect(
    res$label_fr,
    "Cas confirmés de paludisme - 5 ans et plus \\[Secteur privé\\] \\(Niveau communautaire \\(ASC\\)\\)"
  ))

  # Portuguese label (CLI output only)
  testthat::expect_visible(
    utils::capture.output(
      sntutils::check_snt_var("conf_ov5_priv_chw", return = FALSE)
    )
  )

  # Order-insensitive detection
  res2 <- sntutils::check_snt_var("conf_priv_ov5_chw", return = TRUE)
  testthat::expect_equal(res2$age_group, res$age_group)
  testthat::expect_equal(res2$sector, res$sector)
  testthat::expect_equal(res2$service_level, res$service_level)
  testthat::expect_equal(res2$label_en, res$label_en)

  # Unknown variable gracefully handled
  res_unknown <- sntutils::check_snt_var("xyz_foo_bar", return = TRUE)
  testthat::expect_true(base::is.na(res_unknown$age_group))
  testthat::expect_true(stringr::str_detect(res_unknown$label_en, "Xyz"))

  # Partial structure (no sector)
  res_partial <- sntutils::check_snt_var("conf_ov5", return = TRUE)
  testthat::expect_true(stringr::str_detect(
    res_partial$label_en,
    "5 years and above"
  ))
  testthat::expect_false(stringr::str_detect(res_partial$label_en, "\\[")) # no [sector] part
})
