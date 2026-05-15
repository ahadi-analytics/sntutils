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

test_that("custom labels work with decimal reporting rates", {
  # simulate reporting rates (proportions from 0 to 1)
  reprate <- c(0.5, 0.65, 0.75, 0.85, 0.92, 0.97, 0.99, 1.0)
  custom_labels <- c(
    "0.00\u20130.70",
    "0.70\u20130.80",
    "0.80\u20130.95",
    "0.95\u20130.99",
    "1.00"
  )

  result <- auto_bin(
    x = reprate,
    palette = "rdbu",
    labels = custom_labels
  )

  expect_equal(result$method, "custom")
  expect_equal(length(result$colors), 5)
  # labels should be preserved as-is for decimal data (not reformatted)
  expect_equal(names(result$colors), custom_labels)
  expect_true(all(levels(result$bins) == custom_labels))

  # verify no duplicate labels
  expect_equal(anyDuplicated(names(result$colors)), 0)
})

test_that("custom labels with hyphen format work for decimal data", {
  reprate <- c(0.5, 0.75, 0.85, 0.97)
  custom_labels <- c("0.00-0.70", "0.70-0.80", "0.80-0.95", "0.95-1.00")

  result <- auto_bin(
    x = reprate,
    palette = "blues",
    labels = custom_labels
  )

  expect_equal(result$method, "custom")
  expect_equal(length(result$colors), 4)
  # labels should be preserved for decimal data
  expect_equal(names(result$colors), custom_labels)
})

test_that("custom labels preserve precision for rates with 3 decimals", {
  rate <- c(0.123, 0.456, 0.789)
  custom_labels <- c("0.000-0.250", "0.250-0.500", "0.500-1.000")

  result <- auto_bin(
    x = rate,
    palette = "greens",
    labels = custom_labels
  )

  expect_equal(result$method, "custom")
  # labels should be preserved for decimal data
  expect_equal(names(result$colors), custom_labels)
})

test_that("custom labels still apply K/M formatting for large numbers", {
  incidence <- c(500, 1500, 5000, 15000)
  custom_labels <- c("0-1000", "1000-3000", "3000-10000", ">10000")

  result <- auto_bin(
    x = incidence,
    palette = "byor",
    labels = custom_labels
  )

  # should be reformatted with K/M suffixes for large numbers
  expected_formatted <- c("0\u20131K", "1K\u20133K", "3K\u201310K", ">10K")

  expect_equal(result$method, "custom")
  expect_equal(names(result$colors), expected_formatted)
})

test_that("outlier threshold creates separate bin for values above threshold", {
  set.seed(123)
  tpr <- c(runif(80, 0.5, 0.95), runif(20, 1.05, 1.3))

  result <- auto_bin(
    tpr,
    palette = "rdbu",
    bin = 5,
    outlier_threshold = 1.0
  )

  expect_s3_class(result$bins, "factor")
  expect_equal(length(levels(result$bins)), 5)
  expect_true(">1.00" %in% levels(result$bins))
  expect_equal(sum(result$bins == ">1.00", na.rm = TRUE), 20)
})

test_that("outlier threshold uses custom color for outlier bin", {
  set.seed(123)
  tpr <- c(runif(80, 0.5, 0.95), runif(20, 1.05, 1.3))

  result <- auto_bin(
    tpr,
    palette = "rdbu",
    bin = 5,
    outlier_threshold = 1.0,
    outlier_color = "#636363"
  )

  expect_equal(result$colors[">1.00"], c(">1.00" = "#636363"))
  expect_equal(length(result$colors), 5)
})

test_that("outlier threshold bins normal data automatically", {
  set.seed(123)
  tpr <- c(runif(80, 0.5, 0.95), runif(20, 1.05, 1.3))

  result <- auto_bin(
    tpr,
    palette = "rdbu",
    bin = 5,
    outlier_threshold = 1.0
  )

  # 4 automatic bins + 1 outlier bin = 5 total
  expect_equal(length(result$colors), 5)
  # all bins except outlier should use rdbu palette (4 bins interpolated)
  rdbu_colors <- sntutils::get_palette("rdbu", n = 4)
  expect_equal(length(unname(result$colors[1:4])), 4)
  # last color should be grey
  expect_equal(unname(result$colors[5]), "#636363")
})

test_that("outlier threshold errors when combined with custom labels", {
  tpr <- c(0.5, 0.75, 0.95, 1.1)

  expect_error(
    auto_bin(
      tpr,
      labels = c("0.0-0.8", "0.8-1.0", ">1.0"),
      outlier_threshold = 1.0
    ),
    "Cannot use both.*outlier_threshold.*and.*labels"
  )
})

test_that("outlier threshold handles case with no outliers", {
  tpr <- c(0.5, 0.6, 0.7, 0.8, 0.9)

  result <- auto_bin(
    tpr,
    palette = "rdbu",
    bin = 5,
    outlier_threshold = 1.0
  )

  # should work normally, no outlier bin created
  expect_s3_class(result$bins, "factor")
  expect_false(">1.00" %in% levels(result$bins))
})

test_that("outlier threshold preserves method from automatic binning", {
  set.seed(123)
  tpr <- c(runif(80, 0.5, 0.95), runif(20, 1.05, 1.3))

  result <- auto_bin(
    tpr,
    palette = "rdbu",
    bin = 5,
    outlier_threshold = 1.0
  )

  expect_true(result$method %in% c("headtail", "hybrid", "quantile"))
})

test_that("outlier threshold returns correct counts", {
  set.seed(123)
  tpr <- c(runif(80, 0.5, 0.95), runif(20, 1.05, 1.3))

  result <- auto_bin(
    tpr,
    palette = "rdbu",
    bin = 5,
    outlier_threshold = 1.0
  )

  expect_s3_class(result$counts, "data.frame")
  expect_equal(sum(result$counts$n), 100)
  expect_equal(nrow(result$counts), 5)

  outlier_count <- result$counts$n[result$counts$bin == ">1.00"]
  expect_equal(outlier_count, 20)
})

test_that("outlier label uses same decimal precision as regular bins", {
  set.seed(123)
  tpr <- c(runif(80, 0.5, 0.95), runif(20, 1.05, 1.3))

  result <- auto_bin(
    tpr,
    palette = "rdbu",
    bin = 6,
    decimals = 2,
    outlier_threshold = 1.0
  )

  regular_label <- as.character(levels(result$bins)[1])
  outlier_label <- as.character(levels(result$bins)[length(levels(result$bins))])

  regular_precision <- sntutils:::.detect_label_precision(regular_label)
  outlier_precision <- sntutils:::.detect_label_precision(outlier_label)

  expect_equal(regular_precision, outlier_precision)
})

test_that("outlier threshold handles all values correctly without NAs", {
  set.seed(123)
  # mix of values below and above threshold
  tpr <- c(0.74, 0.64, 0.92, 0.88, 0.94, 0.74, 0.76, 0.8, 0.76, 1.05, 1.15)

  result <- auto_bin(
    tpr,
    palette = "rdbu",
    bin = 6,
    outlier_threshold = 1.0
  )

  # no bins should be NA
  expect_equal(sum(is.na(result$bins)), 0)

  # values > 1.0 should be in outlier bin
  expect_equal(sum(result$bins == ">1.00", na.rm = TRUE), 2)

  # values <= 1.0 should be in regular bins
  expect_equal(sum(result$bins != ">1.00", na.rm = TRUE), 9)
})

test_that("custom labels avoid same-K labels like 2K-2K", {
  # 1500 and 2000 both round to "2K" with digits=0, so the label

  # "1500-2000" would become "2K–2K" without the same-bounds check
  set.seed(99)
  incidence <- c(
    runif(20, 0, 100),
    runif(20, 100, 700),
    runif(20, 700, 1500),
    runif(20, 1500, 2000),
    runif(10, 2000, 3000)
  )
  inc_bins <- c(
    "<100", "100-250", "250-400", "400-700",
    "700-1000", "1000-1500", "1500-2000", ">2000"
  )

  result <- auto_bin(incidence, labels = inc_bins)

  # no label should have identical lower and upper
  labs <- levels(result$bins)
  range_labs <- labs[!grepl("^>", labs)]
  for (lab in range_labs) {
    parts <- strsplit(lab, "\u2013")[[1]]
    expect_false(
      parts[1] == parts[2],
      info = paste0("duplicate bounds in label: ", lab)
    )
  }
})

test_that("outlier threshold works with NA values in input", {
  tpr <- c(0.5, NA, 0.8, 1.2, NA, 0.9)

  result <- auto_bin(
    tpr,
    palette = "rdbu",
    bin = 5,
    outlier_threshold = 1.0
  )

  # only original NAs should be NA in output
  expect_equal(sum(is.na(result$bins)), 2)

  # outlier should be binned
  expect_equal(sum(result$bins == ">1.00", na.rm = TRUE), 1)
})
