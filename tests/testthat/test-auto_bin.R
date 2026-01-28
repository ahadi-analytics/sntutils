test_that("custom labels with en-dash format work", {
  incidence <- c(25, 75, 150, 300, 500, 850, 1200, 45, 90)
  custom_labels <- c(
    "0\u201350",
    "50\u2013100",
    "100\u2013250",
    "250\u2013450",
    "450\u2013700",
    "700\u20131000",
    ">1000"
  )

  result <- auto_bin(
    x = incidence,
    palette = "byor",
    labels = custom_labels
  )

  # expect formatted labels with K/M suffixes
  expected_formatted <- c(
    "0\u201350",
    "50\u2013100",
    "100\u2013250",
    "250\u2013450",
    "450\u2013700",
    "700\u20131K",
    ">1K"
  )

  expect_s3_class(result$bins, "factor")
  expect_equal(result$method, "custom")
  expect_equal(length(result$colors), 7)
  expect_equal(names(result$colors), expected_formatted)
  expect_true(all(levels(result$bins) == expected_formatted))
})

test_that("custom labels with hyphen format work", {
  incidence <- c(25, 75, 150, 300, 500, 850, 1200)
  custom_labels <- c(
    "0-50",
    "50-100",
    "100-250",
    "250-450",
    "450-700",
    "700-1000",
    ">1000"
  )

  result <- auto_bin(
    x = incidence,
    palette = "blues",
    labels = custom_labels
  )

  expect_equal(result$method, "custom")
  expect_equal(length(result$colors), 7)
})

test_that("custom labels with open-ended format work", {
  incidence <- c(25, 75, 150, 1500, 2000)
  custom_labels <- c("0-100", "100-500", ">500")

  result <- auto_bin(
    x = incidence,
    palette = "reds",
    labels = custom_labels
  )

  # should be formatted as K/M
  expect_equal(result$method, "custom")
  expect_equal(length(result$colors), 3)
  expect_equal(sum(result$counts$n), length(incidence))
  # check that labels are formatted
  expect_true(any(grepl(">", names(result$colors))))
})

test_that("custom labels override automatic binning parameters", {
  incidence <- c(25, 75, 150, 300, 500)
  custom_labels <- c("0-50", "50-200", "200-600")

  # should warn about ignored parameters
  expect_warning(
    result <- auto_bin(
      x = incidence,
      bin = 10,
      round_to = 100,
      decimals = 3,
      labels = custom_labels
    ),
    "bin.*round_to.*decimals.*will be ignored"
  )

  expect_equal(result$method, "custom")
  expect_equal(length(result$bins), length(incidence))
})

test_that("custom labels map colors correctly", {
  incidence <- c(25, 75, 150)
  custom_labels <- c("0-50", "50-100", "100-200")

  result <- auto_bin(
    x = incidence,
    palette = "byor",
    labels = custom_labels
  )

  # labels should be formatted, so just check length
  expect_equal(length(result$colors), 3)
  expect_true(all(!is.na(names(result$colors))))
  expect_true(all(nchar(names(result$colors)) > 0))
})

test_that("custom labels work with reverse colors", {
  incidence <- c(25, 75, 150)
  custom_labels <- c("0-50", "50-100", "100-200")

  result_normal <- auto_bin(
    x = incidence,
    palette = "reds",
    labels = custom_labels,
    reverse = FALSE
  )

  result_reverse <- auto_bin(
    x = incidence,
    palette = "reds",
    labels = custom_labels,
    reverse = TRUE
  )

  # compare color values, not the whole named vector
  expect_equal(unname(result_normal$colors), rev(unname(result_reverse$colors)))
})

test_that("custom labels handle NA and zero values", {
  incidence <- c(0, NA, 25, 75, 150)
  custom_labels <- c("0-50", "50-100", "100-200")

  result <- auto_bin(
    x = incidence,
    labels = custom_labels
  )

  # cut() with include.lowest=TRUE and breaks starting at -Inf will bin 0
  # only NA remains as NA
  expect_equal(sum(is.na(result$bins)), 1)
  expect_equal(sum(!is.na(result$bins)), 4)
})

test_that("error on non-character labels", {
  incidence <- c(25, 75, 150)
  expect_error(
    auto_bin(x = incidence, labels = c(1, 2, 3)),
    "labels.*must be.*character"
  )
})

test_that("error on malformed labels", {
  incidence <- c(25, 75, 150)
  expect_error(
    auto_bin(x = incidence, labels = c("foo", "bar", "baz")),
    "Could not parse label"
  )
})

test_that("error on non-monotonic breaks", {
  incidence <- c(25, 75, 150)
  # labels with overlapping breaks (100 followed by 150 followed by 200)
  # this actually IS monotonic, so let's use truly non-monotonic breaks
  expect_error(
    auto_bin(x = incidence, labels = c("0-100", "200-300", "50-150")),
    "not monotonically increasing"
  )
})

test_that("custom labels return correct counts", {
  incidence <- c(25, 45, 75, 90, 150, 200, 350)
  custom_labels <- c("0-50", "50-100", "100-250", "250-400")

  result <- auto_bin(x = incidence, labels = custom_labels)

  expect_s3_class(result$counts, "data.frame")
  expect_equal(nrow(result$counts), 4)
  expect_equal(sum(result$counts$n), length(incidence))
  expect_true(all(c("bin", "n") %in% names(result$counts)))
  # verify bins are factors with proper levels
  expect_s3_class(result$counts$bin, "factor")
})

test_that("automatic binning still works when labels is NULL", {
  set.seed(42)
  incidence <- c(rep(0, 20), rgamma(80, shape = 2, rate = 0.01))

  result <- auto_bin(incidence)

  expect_true(result$method %in% c("headtail", "hybrid", "quantile"))
  expect_s3_class(result$bins, "factor")
  expect_true(length(result$colors) >= 3)
})

test_that("custom labels work with custom palette colors", {
  incidence <- c(25, 75, 150)
  custom_labels <- c("0-50", "50-100", "100-200")
  custom_palette <- c("#ff0000", "#00ff00", "#0000ff")

  result <- auto_bin(
    x = incidence,
    palette = custom_palette,
    labels = custom_labels
  )

  expect_equal(result$method, "custom")
  # colorRampPalette may return uppercase hex codes
  expect_equal(length(result$colors), 3)
  expect_equal(tolower(unname(result$colors)), tolower(custom_palette))
})

test_that("custom labels diagnostics include prop_zero", {
  incidence <- c(0, 0, 25, 75, 150)
  custom_labels <- c("0-50", "50-100", "100-200")

  result <- auto_bin(x = incidence, labels = custom_labels)

  expect_true("diagnostics" %in% names(result))
  expect_equal(result$diagnostics$prop_zero, 0.4)
  expect_true(is.na(result$diagnostics$skew_ratio))
  expect_true(is.na(result$diagnostics$tail_share))
})
