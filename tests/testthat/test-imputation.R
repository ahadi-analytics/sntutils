testthat::test_that("get_pathway_vars('susp') has no upstream and full downstream", {
  result <- NULL
  testthat::expect_message(
    result <- sntutils::get_pathway_vars("susp"),
    regexp = "Upstream"
  )

  testthat::expect_equal(result$var_to_impute, "susp")
  testthat::expect_length(result$upstream_vars, 0L)
  testthat::expect_true(base::all(
    c("test", "conf", "maldth") %in% result$downstream_vars
  ))
  testthat::expect_true(result$use_structural)
})

testthat::test_that("get_pathway_vars('test') has susp upstream", {
  result <- sntutils::get_pathway_vars("test")

  testthat::expect_equal(result$upstream_vars, "susp")
  testthat::expect_true("conf" %in% result$downstream_vars)
  testthat::expect_true(result$use_structural)
})

testthat::test_that("get_pathway_vars('conf') has susp, test upstream", {
  result <- sntutils::get_pathway_vars("conf")

  testthat::expect_equal(result$upstream_vars, c("susp", "test"))
  testthat::expect_true(result$use_structural)
})

testthat::test_that("get_pathway_vars('maltreat') has susp/test/conf upstream", {
  result <- sntutils::get_pathway_vars("maltreat")

  testthat::expect_true(base::all(
    c("susp", "test", "conf") %in% result$upstream_vars
  ))
  testthat::expect_true(result$use_structural)
})

testthat::test_that("get_pathway_vars('maladm', inpatient) returns full upstream", {
  result <- sntutils::get_pathway_vars(
    "maladm",
    facility_type = "hospital"
  )

  testthat::expect_true(result$use_structural)
  testthat::expect_true("conf" %in% result$upstream_vars)
  testthat::expect_true("maldth" %in% result$downstream_vars)
})

testthat::test_that("get_pathway_vars('maladm', outpatient) blocks structural imputation", {
  result <- NULL
  testthat::expect_message(
    result <- sntutils::get_pathway_vars(
      "maladm",
      facility_type = "health_post"
    ),
    regexp = "outpatient"
  )

  testthat::expect_false(result$use_structural)
  testthat::expect_length(result$upstream_vars, 0L)
  testthat::expect_length(result$downstream_vars, 0L)
})

testthat::test_that("get_pathway_vars('maladm') without facility_type treats as outpatient", {
  result <- sntutils::get_pathway_vars("maladm")
  testthat::expect_false(result$use_structural)
})

testthat::test_that("get_pathway_vars('maladm') is case-insensitive for inpatient types", {
  result_upper <- sntutils::get_pathway_vars(
    "maladm",
    facility_type = "REFERRAL"
  )
  testthat::expect_true(result_upper$use_structural)

  result_mixed <- sntutils::get_pathway_vars(
    "maladm",
    facility_type = "Tertiary"
  )
  testthat::expect_true(result_mixed$use_structural)
})

testthat::test_that("get_pathway_vars() appends suffix when supplied", {
  result <- sntutils::get_pathway_vars("conf", suffix = "_hf")

  testthat::expect_equal(result$var_to_impute, "conf_hf")
  testthat::expect_true(base::all(base::endsWith(result$upstream_vars, "_hf")))
})

testthat::test_that("get_pathway_vars('maldth') for inpatient includes maladm upstream", {
  result <- sntutils::get_pathway_vars(
    "maldth",
    facility_type = "hospital"
  )

  testthat::expect_true("maladm" %in% result$upstream_vars)
  testthat::expect_length(result$downstream_vars, 0L)
})

testthat::test_that("get_pathway_vars('maldth') for outpatient excludes maladm from upstream", {
  result <- sntutils::get_pathway_vars("maldth")
  testthat::expect_false("maladm" %in% result$upstream_vars)
})
