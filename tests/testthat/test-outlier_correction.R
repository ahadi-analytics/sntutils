test_that("full workflow: detect_outliers then correct_outliers", {

  # create test data with one obvious outlier in row 14
  test_data <- tibble::tibble(
    record_id = paste0("rec_", 1:24),
    hf_uid = rep("facility_1", 24),
    adm1 = rep("region_a", 24),
    date = seq.Date(as.Date("2022-01-01"), by = "month", length.out = 24),
    conf = c(10, 12, 11, 13, 10, 12, 11, 14, 12, 13, 11, 10,
             12, 500, 13, 11, 12, 10, 14, 13, 11, 12, 10, 11),
    test = c(20, 25, 22, 26, 21, 24, 23, 28, 25, 27, 22, 20,
             24, 520, 26, 22, 25, 21, 28, 27, 23, 25, 20, 22)
  )

  # step 1: detect outliers
  detected <- detect_outliers(
    data = test_data,
    column = "conf",
    record_id = "record_id",
    admin_level = "adm1",
    spatial_level = "hf_uid",
    date = "date",
    time_mode = "across_time",
    strictness = "balanced",
    verbose = FALSE
  )

  # verify outlier was detected (row 14 with conf=500)
  expect_true("outlier" %in% detected$outlier_flag_consensus)

  # step 2: join detection results back to original data
  merged <- test_data |>
    dplyr::left_join(
      detected |> dplyr::select(record_id, outlier_flag_consensus),
      by = "record_id"
    )

  # step 3: correct outliers
  corrected <- correct_outliers(
    data = merged,
    target_vars = "conf",
    consistency_vars = "test",
    group_cols = "hf_uid",
    date_col = "date",
    verbose = FALSE
  )

  # the outlier should be corrected to median of neighbors (12, 13) = 12 or 13
  outlier_row <- which(corrected$outlier_flag_consensus == "outlier")
  expect_equal(corrected$conf_correction_flag[outlier_row], "corrected")
  expect_equal(corrected$conf_corrected[outlier_row], 12)  # round((12+13)/2) = 12
})


test_that("correct_outliers applies median correction for outliers", {
  test_data <- tibble::tibble(
    hf_uid = rep("facility_1", 5),
    date = as.Date(c("2023-01-01", "2023-02-01", "2023-03-01",
                     "2023-04-01", "2023-05-01")),
    conf = c(10, 12, 100, 14, 16),
    test = c(20, 25, 120, 30, 35),
    outlier_flag_consensus = c("normal", "normal", "outlier", "normal", "normal")
  )


  result <- correct_outliers(
    data = test_data,
    target_vars = "conf",
    consistency_vars = "test",
    group_cols = "hf_uid",
    date_col = "date",
    verbose = FALSE
  )

  expect_true("conf_corrected" %in% names(result))
  expect_true("conf_correction_flag" %in% names(result))

  # round((12 + 14) / 2) = 13
  expect_equal(result$conf_corrected[3], 13)
  expect_equal(result$conf_correction_flag[3], "corrected")
})


test_that("correct_outliers handles missing neighbors", {
  # first row is outlier - no lag available
  test_data <- tibble::tibble(
    hf_uid = rep("facility_1", 3),
    date = as.Date(c("2023-01-01", "2023-02-01", "2023-03-01")),
    conf = c(100, 12, 14),
    test = c(120, 25, 30),
    outlier_flag_consensus = c("outlier", "normal", "normal")
  )

  result <- correct_outliers(
    data = test_data,
    target_vars = "conf",
    consistency_vars = "test",
    group_cols = "hf_uid",
    date_col = "date",
    verbose = FALSE
  )

  expect_equal(result$conf_correction_flag[1], "missing_neighbors")
  expect_equal(result$conf_corrected[1], 100)  # unchanged
})


test_that("correct_outliers applies failed_consistency when median > consistency_var", {
  test_data <- tibble::tibble(
    hf_uid = rep("facility_1", 5),
    date = as.Date(c("2023-01-01", "2023-02-01", "2023-03-01",
                     "2023-04-01", "2023-05-01")),
    conf = c(50, 60, 200, 70, 80),
    test = c(55, 65, 50, 75, 85),  # test[3]=50, but median(60,70)=65 > 50
    outlier_flag_consensus = c("normal", "normal", "outlier", "normal", "normal")
  )

  result <- correct_outliers(
    data = test_data,
    target_vars = "conf",
    consistency_vars = "test",
    group_cols = "hf_uid",
    date_col = "date",
    verbose = FALSE
  )

  expect_equal(result$conf_correction_flag[3], "failed_consistency")
  expect_true(is.na(result$conf_corrected[3]))
})


test_that("correct_outliers leaves non-outliers unchanged", {
  test_data <- tibble::tibble(
    hf_uid = rep("facility_1", 3),
    date = as.Date(c("2023-01-01", "2023-02-01", "2023-03-01")),
    conf = c(10, 12, 14),
    test = c(20, 25, 30),
    outlier_flag_consensus = c("normal", "normal", "normal")
  )

  result <- correct_outliers(
    data = test_data,
    target_vars = "conf",
    consistency_vars = "test",
    group_cols = "hf_uid",
    date_col = "date",
    verbose = FALSE
  )

  expect_equal(result$conf_corrected, c(10, 12, 14))
  expect_equal(result$conf_correction_flag, rep("not_outlier", 3))
})


test_that("correct_outliers handles multiple variable pairs", {
  test_data <- tibble::tibble(
    hf_uid = rep("facility_1", 5),
    date = as.Date(c("2023-01-01", "2023-02-01", "2023-03-01",
                     "2023-04-01", "2023-05-01")),
    conf = c(10, 12, 100, 14, 16),
    maltreat = c(8, 10, 80, 12, 14),
    test = c(20, 25, 120, 30, 35),
    outlier_flag_consensus = c("normal", "normal", "outlier", "normal", "normal")
  )

  result <- correct_outliers(
    data = test_data,
    target_vars = c("conf", "maltreat"),
    consistency_vars = c("test", "conf"),
    group_cols = "hf_uid",
    date_col = "date",
    verbose = FALSE
  )

  # check both correction columns exist

  expect_true("conf_corrected" %in% names(result))
  expect_true("conf_correction_flag" %in% names(result))
  expect_true("maltreat_corrected" %in% names(result))
  expect_true("maltreat_correction_flag" %in% names(result))

  # conf: median(12, 14) = 13 <= test[3]=120 → corrected

expect_equal(result$conf_corrected[3], 13)
  expect_equal(result$conf_correction_flag[3], "corrected")

  # maltreat: median(10, 12) = 11 <= conf_corrected[3]=13 → corrected
  expect_equal(result$maltreat_corrected[3], 11)
  expect_equal(result$maltreat_correction_flag[3], "corrected")
})


test_that("correct_outliers respects group boundaries", {
  # two facilities - outlier in facility_1 should not use facility_2 neighbors
  test_data <- tibble::tibble(
    hf_uid = c("facility_1", "facility_1", "facility_2", "facility_2"),
    date = as.Date(c("2023-01-01", "2023-02-01", "2023-02-01", "2023-03-01")),
    conf = c(10, 100, 50, 60),
    test = c(20, 120, 80, 100),
    outlier_flag_consensus = c("normal", "outlier", "normal", "normal")
  )

  result <- correct_outliers(
    data = test_data,
    target_vars = "conf",
    consistency_vars = "test",
    group_cols = "hf_uid",
    date_col = "date",
    verbose = FALSE
  )

  # facility_1 outlier at row 2: only has lag (10), no lead within group
  expect_equal(result$conf_correction_flag[2], "missing_neighbors")
})


test_that("correct_outliers validates input lengths", {
  test_data <- tibble::tibble(
    hf_uid = "facility_1",
    date = as.Date("2023-01-01"),
    conf = 10,
    test = 20,
    outlier_flag_consensus = "normal"
  )

  expect_error(
    correct_outliers(
      data = test_data,
      target_vars = c("conf", "test"),
      consistency_vars = "test",  # length mismatch
      group_cols = "hf_uid",
      verbose = FALSE
    ),
    "same length"
  )
})


test_that("correct_outliers errors on missing columns", {
  test_data <- tibble::tibble(
    hf_uid = "facility_1",
    date = as.Date("2023-01-01"),
    conf = 10,
    outlier_flag_consensus = "normal"
  )

  expect_error(
    correct_outliers(
      data = test_data,
      target_vars = "conf",
      consistency_vars = "test",  # missing column
      group_cols = "hf_uid",
      verbose = FALSE
    ),
    "not found"
  )
})
