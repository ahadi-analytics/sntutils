#' Round to nearest multiple
#'
#' @param x Numeric vector to round.
#' @param to Rounding increment.
#' @return Rounded numeric vector.
#' @noRd
.round_to_nearest <- function(x, to) {
  round(x / to) * to
}

#' Detect decimal precision from formatted label
#'
#' @param label Character. A bin label like "0.790–0.850" or ">1.000".
#' @return Integer. Number of decimal places detected.
#' @noRd
.detect_label_precision <- function(label) {
  num_str <- stringr::str_extract(label, "[0-9]+\\.[0-9]+")
  if (is.na(num_str)) {
    return(0)
  }
  decimal_part <- stringr::str_extract(num_str, "\\.[0-9]+")
  if (is.na(decimal_part)) {
    return(0)
  }
  nchar(decimal_part) - 1
}

#' Format number with K/M suffix for large values
#'
#' @param x Numeric vector.
#' @param digits Integer. Decimal places for K/M values.
#' @return Character vector with K (thousands) or M (millions) suffix.
#' @noRd
.format_number <- function(x, digits = 0) {
  dplyr::case_when(
    is.na(x) ~ NA_character_,
    abs(x) >= 1e6 ~ paste0(round(x / 1e6, digits), "M"),
    abs(x) >= 1e3 ~ paste0(round(x / 1e3, digits), "K"),
    TRUE ~ formatC(x, format = "f", digits = digits, big.mark = ",")
  )
}

#' Parse custom labels to numeric breaks
#'
#' @param labels Character vector of labels in formats like "0–50",
#'   "50-100", or ">1000".
#' @return Numeric vector of break points including Inf for open-ended ranges.
#' @noRd
.parse_labels_to_breaks <- function(labels) {
  # extract all numeric values from labels
  numbers <- lapply(labels, function(label) {
    # handle en-dash (U+2013), hyphen, and > symbols
    nums <- stringr::str_extract_all(label, "[0-9]+\\.?[0-9]*")[[1]]
    as.numeric(nums)
  })

  # build breaks from boundaries
  breaks <- numeric()
  for (i in seq_along(labels)) {
    nums <- numbers[[i]]
    if (length(nums) == 2) {
      # range format: "0–50" or "50-100"
      breaks <- c(breaks, nums[2])
    } else if (length(nums) == 1 && grepl("^>", labels[i])) {
      # open-ended format: ">1000"
      breaks <- c(breaks, Inf)
    } else if (length(nums) == 1) {
      # single number, treat as upper bound
      breaks <- c(breaks, nums[1])
    } else {
      cli::cli_abort(
        "Could not parse label {i}: {.val {labels[i]}}. Expected format like '0\u20131' or '>1000'"
      )
    }
  }

  # validate monotonically increasing
  if (any(diff(breaks[!is.infinite(breaks)]) <= 0)) {
    cli::cli_abort(
      "Parsed breaks are not monotonically increasing: {.val {breaks}}"
    )
  }

  breaks
}

#' List available palette names
#'
#' @return Character vector of available palette names.
#'
#' @examples
#' list_palettes()
#'
#' @export
list_palettes <- function() {
  c(
    # custom
    "default", "byor", "gyor",
    # sequential
    "ylord", "ylgnbu", "blues", "greens", "reds",
    "oranges", "purples", "bupu", "orrd",
    # diverging
    "rdylgn", "rdbu", "spectral", "piyg",
    # viridis
    "viridis", "magma", "plasma",
    # two-color
    "redblue", "bluered", "redgreen", "greenred",
    "yellowred", "yellowblue", "whiteblue", "whitered", "whitegreen"
  )
}

#' Get a color palette
#'
#' Returns a color palette by name or passes through custom colors. When `n`
#' is specified, interpolates to return exactly that many colors.
#'
#' @param palette Character. Either a preset name or a custom character vector
#'   of hex colors. Use `list_palettes()` to see available presets.
#' @param n Integer. Number of colors to return. If NULL (default), returns
#'   the full base palette.
#'
#' @return Character vector of hex colors.
#'
#' @details
#' Available palettes (use `list_palettes()` to see all):
#'
#' **Custom:** default, byor, gyor
#'
#' **Sequential (ColorBrewer):** ylord, ylgnbu, blues, greens, reds,
#'   oranges, purples, bupu, orrd
#'
#' **Diverging (ColorBrewer):** rdylgn, rdbu, spectral, piyg
#'
#' **Viridis-style:** viridis, magma, plasma
#'
#' **Two-color:** redblue, bluered, redgreen, greenred, yellowred,
#'   yellowblue, whiteblue, whitered, whitegreen
#'
#' @examples
#' # See available palettes
#' list_palettes()
#'
#' # Get full palette
#' get_palette("blues")
#'
#' # Get specific number of colors
#' get_palette("ylord", n = 5)
#' get_palette("viridis", n = 7)
#'
#' # Use custom colors
#' get_palette(c("#ffffcc", "#41b6c4", "#225ea8"), n = 10)
#'
#' @export
get_palette <- function(palette, n = NULL) {
  presets <- list(
    # custom palettes
    default = c(
      "#deebf7", "#c6dbef", "#9ecae1", "#6baed6",
      "#fc9272", "#fb6a4a", "#de2d26", "#a50f15"
    ),
    byor = c(
      "#084594", "#2171b5", "#6baed6", "#c6dbef",
      "#fed976", "#fd8d3c", "#e31a1c", "#800026"
    ),
    gyor = c(
      "#005a32", "#238b45", "#74c476", "#c7e9c0",
      "#fed976", "#fd8d3c", "#e31a1c", "#800026"
    ),
    # colorbrewer sequential
    ylord = c(
      "#ffffcc", "#ffeda0", "#fed976", "#feb24c",
      "#fd8d3c", "#fc4e2a", "#e31a1c", "#b10026"
    ),
    ylgnbu = c(
      "#ffffd9", "#edf8b1", "#c7e9b4", "#7fcdbb",
      "#41b6c4", "#1d91c0", "#225ea8", "#0c2c84"
    ),
    blues = c(
      "#f7fbff", "#deebf7", "#c6dbef", "#9ecae1",
      "#6baed6", "#4292c6", "#2171b5", "#084594"
    ),
    greens = c(
      "#f7fcf5", "#e5f5e0", "#c7e9c0", "#a1d99b",
      "#74c476", "#41ab5d", "#238b45", "#005a32"
    ),
    reds = c(
      "#fff5f0", "#fee0d2", "#fcbba1", "#fc9272",
      "#fb6a4a", "#ef3b2c", "#cb181d", "#99000d"
    ),
    oranges = c(
      "#fff5eb", "#fee6ce", "#fdd0a2", "#fdae6b",
      "#fd8d3c", "#f16913", "#d94801", "#8c2d04"
    ),
    purples = c(
      "#fcfbfd", "#efedf5", "#dadaeb", "#bcbddc",
      "#9e9ac8", "#807dba", "#6a51a3", "#4a1486"
    ),
    bupu = c(
      "#f7fcfd", "#e0ecf4", "#bfd3e6", "#9ebcda",
      "#8c96c6", "#8c6bb1", "#88419d", "#6e016b"
    ),
    orrd = c(
      "#fff7ec", "#fee8c8", "#fdd49e", "#fdbb84",
      "#fc8d59", "#ef6548", "#d7301f", "#990000"
    ),
    # colorbrewer diverging
    rdylgn = c(
      "#d73027", "#f46d43", "#fdae61", "#fee08b",
      "#d9ef8b", "#a6d96a", "#66bd63", "#1a9850"
    ),
    rdbu = c(
      "#b2182b", "#d6604d", "#f4a582", "#fddbc7",
      "#d1e5f0", "#92c5de", "#4393c3", "#2166ac"
    ),
    spectral = c(
      "#d53e4f", "#f46d43", "#fdae61", "#fee08b",
      "#e6f598", "#abdda4", "#66c2a5", "#3288bd"
    ),
    piyg = c(
      "#c51b7d", "#de77ae", "#f1b6da", "#fde0ef",
      "#e6f5d0", "#b8e186", "#7fbc41", "#4d9221"
    ),
    # viridis-style
    viridis = c(
      "#440154", "#482878", "#3e4a89", "#31688e",
      "#26838e", "#1f9e89", "#35b779", "#6ece58",
      "#b5de2b", "#fde725"
    ),
    magma = c(
      "#000004", "#180f3e", "#451077", "#721f81",
      "#9f2f7f", "#cd4071", "#f1605d", "#fd9668",
      "#feca8d", "#fcfdbf"
    ),
    plasma = c(
      "#0d0887", "#46039f", "#7201a8", "#9c179e",
      "#bd3786", "#d8576b", "#ed7953", "#fa9e3b",
      "#fdc328", "#f0f921"
    ),
    # diverging two-color (with white midpoint)
    redblue = c("#d73027", "#f7f7f7", "#4575b4"),
    bluered = c("#4575b4", "#f7f7f7", "#d73027"),
    redgreen = c("#d73027", "#f7f7f7", "#1a9850"),
    greenred = c("#1a9850", "#f7f7f7", "#d73027"),
    # sequential two-color
    yellowred = c("#ffffb2", "#bd0026"),
    yellowblue = c("#ffffb2", "#0c2c84"),
    whiteblue = c("#f7fbff", "#08306b"),
    whitered = c("#fff5f0", "#67000d"),
    whitegreen = c("#f7fcf5", "#00441b")
  )

  # resolve palette name to colors
  if (length(palette) == 1 && palette %in% names(presets)) {
    base_colors <- presets[[palette]]
  } else {
    base_colors <- palette
  }

  # return full palette if n not specified

if (is.null(n)) {
    return(base_colors)
  }

  # interpolate to exact number of colors
  grDevices::colorRampPalette(base_colors)(n)
}

#' Automatically bin numeric data for choropleth maps
#'
#' Selects an appropriate binning method based on data distribution and
#' returns bins with matching colors. Three methods are available: head-tail
#' breaks for highly skewed data, hybrid (quantile + tail) for moderately
#' skewed data, and pure quantile for roughly symmetric data.
#'
#' @param x Numeric vector to bin.
#' @param palette Character. Either a preset name or a custom character vector
#'   of hex colors. Use `list_palettes()` to see available presets.
#' @param bin Integer. Number of bins. Default is 6.
#' @param decimals Integer. Number of decimal places in labels. Default is 2.
#' @param round_to Numeric. Round break points to this increment. Default is 50.
#'   Set to NULL for raw values (useful for decimal data like rates).
#' @param reverse Logical. Reverse the color order? Default is FALSE.
#' @param labels Character vector. Optional custom bin labels. When provided,
#'   skips automatic binning and uses these labels with breaks parsed from
#'   label strings. Supports formats: "0–50", "50-100", ">1000". Example:
#'   c("0–50", "50–100", "100–250", "250–450", "450–700", "700–1000", ">1000")
#' @param outlier_threshold Numeric. Optional threshold to create a separate
#'   outlier bin for values above this threshold. Useful for metrics like TPR
#'   where values >1 are unusual. Default is NULL (no outlier handling).
#' @param outlier_color Character. Hex color for the outlier bin. Only used
#'   when outlier_threshold is specified. Default is "#636363" (dark grey).
#' @param outlier_label Character. Optional custom label for the outlier bin.
#'   when NULL (default), uses the auto-generated format.
#'
#' @return A list with:
#'   \item{bins}{Ordered factor of bin labels for each value in `x`}
#'   \item{colors}{Named character vector mapping labels to colors}
#'   \item{counts}{Data frame with bin labels and counts (n)}
#'   \item{method}{Character. The binning method used: "headtail", "hybrid",
#'     "quantile", or "custom"}
#'   \item{diagnostics}{List with prop_zero, skew_ratio, and tail_share}
#'
#' @details
#' Method selection logic:
#' - **headtail**: prop_zero > 0.1, skew_ratio > 4, or tail_share > 0.4
#' - **hybrid**: skew_ratio > 2
#' - **quantile**: otherwise
#' - **custom**: when labels parameter is provided
#'
#' Use `list_palettes()` to see all available palette names.
#'
#' @examples
#' # Simulated malaria incidence data
#' set.seed(42)
#' incidence <- c(rep(0, 20), rgamma(80, shape = 2, rate = 0.01))
#'
#' result <- auto_bin(incidence)
#' table(result$bins)
#' result$method
#' result$colors
#'
#' # Use named palette
#' auto_bin(incidence, palette = "byor")$colors
#'
#' # Use custom palette
#' auto_bin(incidence, palette = c("#ffffcc", "#a1dab4", "#41b6c4", "#225ea8"))
#'
#' # Reverse colors (high = light, low = dark)
#' auto_bin(incidence, reverse = TRUE)$colors
#'
#' # Use custom labels
#' custom_labels <- c("0–50", "50–100", "100–250", "250–450", "450–700", "700–1000", ">1000")
#' result <- auto_bin(incidence * 10, palette = "byor", labels = custom_labels)
#' table(result$bins)
#' result$method  # Returns "custom"
#'
#' # Handle outliers above threshold (useful for TPR)
#' set.seed(123)
#' tpr <- c(runif(80, 0.5, 0.95), runif(20, 1.05, 1.3))
#' result <- auto_bin(tpr, palette = "rdbu", bin = 5, outlier_threshold = 1.0)
#' table(result$bins)
#' # Custom outlier label for data validation
#' result <- auto_bin(tpr, outlier_threshold = 1.0, outlier_label = "Suspect Values")
#' result$colors  # Last bin (>1.00) will be grey
#'
#' @export
auto_bin <- function(
    x,
    palette = "default",
    bin = 6,
    decimals = 2,
    round_to = 50,
    reverse = FALSE,
    labels = NULL,
    outlier_threshold = NULL,
    outlier_color = "#636363",
    outlier_label = NULL) {

  if (!is.numeric(x)) {
    cli::cli_abort("{.arg x} must be numeric.")
  }

  # keep non-NA values for analysis (zeros excluded for skewness diagnostics)
  x_nonzero <- x[!is.na(x) & x != 0]
  x_finite <- x[!is.na(x)]

  if (length(x_finite) == 0) {
    cli::cli_abort("No non-NA values in {.arg x}.")
  }

  # for compatibility: x_clean used throughout
  # use x_finite as fallback if all values are zero
  x_clean <- if (length(x_nonzero) > 0) x_nonzero else x_finite

  # ---- handle outlier threshold ----
  if (!is.null(outlier_threshold)) {
    if (!is.null(labels)) {
      cli::cli_abort(
        "Cannot use both {.arg outlier_threshold} and {.arg labels}."
      )
    }

    has_outliers <- any(x > outlier_threshold, na.rm = TRUE)

    if (has_outliers) {
      # split data at threshold and track indices
      idx_normal <- which(x <= outlier_threshold | is.na(x))
      idx_outlier <- which(x > outlier_threshold)
      x_normal <- x[idx_normal]
      x_clean_normal <- x_normal[!is.na(x_normal) & x_normal > 0]

      if (length(x_clean_normal) == 0) {
        cli::cli_abort(
          "No values below {.arg outlier_threshold} = {outlier_threshold}."
        )
      }

      # recursively bin the normal data (without outlier params to avoid loop)
      # note: auto_bin typically creates bin+1 bins in quantile mode, so we
      # pass bin-2 to get bin-1 bins for normal data, plus 1 outlier = bin total
      result_normal <- auto_bin(
        x_normal,
        palette = palette,
        bin = bin - 2,
        decimals = decimals,
        round_to = round_to,
        reverse = reverse,
        labels = NULL,
        outlier_threshold = NULL
      )

      # detect actual precision from regular bins
      sample_label <- as.character(levels(result_normal$bins)[1])
      detected_precision <- .detect_label_precision(sample_label)

      # create outlier label with same precision as regular bins
      auto_outlier_label <- paste0(">", formatC(outlier_threshold,
                                          format = "f",
                                          digits = detected_precision))
      effective_outlier_label <- if (!is.null(outlier_label)) outlier_label else auto_outlier_label

      # combine normal bins with outlier bin
      all_labels <- c(levels(result_normal$bins), effective_outlier_label)

      # assign outlier bin to values above threshold
      bins_combined <- rep(NA, length(x))
      bins_combined[idx_normal] <- as.character(result_normal$bins)
      bins_combined[idx_outlier] <- effective_outlier_label
      bins_combined <- factor(bins_combined, levels = all_labels, ordered = TRUE)

      # combine colors
      colors_combined <- c(result_normal$colors, outlier_color)
      names(colors_combined) <- all_labels

      # count observations per bin
      counts <- as.data.frame(table(bins_combined, dnn = "bin"))
      names(counts) <- c("bin", "n")

      return(
        list(
          bins = bins_combined,
          colors = colors_combined,
          counts = counts,
          method = result_normal$method,
          diagnostics = result_normal$diagnostics
        )
      )
    }
  }

  # ---- handle custom labels ----
  if (!is.null(labels)) {
    if (!is.character(labels)) {
      cli::cli_abort("{.arg labels} must be a character vector.")
    }

    # warn if user provided non-default parameters that will be ignored
    if (bin != 6 || round_to != 50 || decimals != 2) {
      cli::cli_warn(
        "Custom {.arg labels} provided. Parameters {.arg bin}, {.arg round_to}, and {.arg decimals} will be ignored."
      )
    }

    # parse breaks from labels
    breaks <- .parse_labels_to_breaks(labels)
    method <- "custom"

    # build formatted labels using parsed breaks
    raw_lower <- c(0, breaks[-length(breaks)])
    raw_upper <- breaks

    # detect if data is decimal (max break < 10 suggests rates/proportions)
    # examples: reporting rates (0.0-1.0), small indices, proportions
    max_break <- max(breaks[!is.infinite(breaks)])
    is_decimal_data <- max_break < 10

    # for decimal data (max < 10), preserve original labels to avoid rounding
    # formatting with digits=0 would turn 0.70, 0.80, 0.95 into "1", causing
    # duplicate labels like "1–1"
    # for large numbers (max >= 10), apply K/M formatting for readability
    if (is_decimal_data) {
      formatted_labels <- labels
    } else {
      # try K/M formatting with increasing precision until no duplicates
      # only apply precision to K/M values, keep raw numbers clean
      k_digits <- 0
      formatted_labels <- character(length(raw_lower))

      for (attempt in 0:2) {
        for (i in seq_along(raw_lower)) {
          # determine precision: use k_digits for K/M, 0 for raw numbers
          lower_needs_k <- !is.na(raw_lower[i]) && raw_lower[i] >= 1000
          upper_needs_k <- !is.na(raw_upper[i]) && !is.infinite(raw_upper[i]) && raw_upper[i] >= 1000

          lower_digits <- if (lower_needs_k) k_digits else 0
          upper_digits <- if (upper_needs_k) k_digits else 0

          if (is.infinite(raw_upper[i])) {
            formatted_labels[i] <- paste0(">", .format_number(raw_lower[i], digits = lower_digits))
          } else {
            formatted_labels[i] <- paste0(
              .format_number(raw_lower[i], digits = lower_digits),
              "\u2013",
              .format_number(raw_upper[i], digits = upper_digits)
            )
          }
        }

        # check for same-bounds labels like "2K–2K"
        has_same_bounds <- any(vapply(
          formatted_labels, function(lab) {
            if (grepl("^>", lab)) return(FALSE)
            parts <- strsplit(lab, "\u2013")[[1]]
            length(parts) == 2 && parts[1] == parts[2]
          }, logical(1)
        ))

        # if no duplicates and no same-bounds, we're done
        if (!any(duplicated(formatted_labels)) && !has_same_bounds) {
          break
        }

        # try with higher precision for K/M values
        k_digits <- k_digits + 1
      }
    }

    # cut data using formatted labels
    bin_result <- cut(
      x,
      breaks = c(-Inf, breaks),
      labels = formatted_labels,
      include.lowest = TRUE,
      right = TRUE
    )
    bin_result <- factor(bin_result, levels = formatted_labels, ordered = TRUE)

    # build color map
    colors <- get_palette(palette, n = length(formatted_labels))
    if (reverse) colors <- rev(colors)
    names(colors) <- formatted_labels

    # count observations per bin
    counts <- as.data.frame(table(bin_result, dnn = "bin"))
    names(counts) <- c("bin", "n")

    return(
      list(
        bins = bin_result,
        colors = colors,
        counts = counts,
        method = method,
        diagnostics = list(
          prop_zero = mean(x == 0, na.rm = TRUE),
          skew_ratio = NA_real_,
          tail_share = NA_real_
        )
      )
    )
  }

  # ---- diagnostics ----
  q50 <- stats::quantile(x_clean, 0.5)
  q90 <- stats::quantile(x_clean, 0.9)
  prop_zero <- mean(x == 0, na.rm = TRUE)

  # skew_ratio only meaningful for positive data
  # for mixed/negative data, use quantile method (safest default)
  has_negative <- any(x_clean < 0)
  if (has_negative || q50 <= 0) {
    skew_ratio <- 1
    tail_share <- 0
  } else {
    skew_ratio <- q90 / q50
    total_sum <- sum(x_clean)
    tail_share <- if (total_sum > 0) {
      sum(x_clean[x_clean > q90]) / total_sum
    } else {
      0
    }
  }

  # ---- choose method ----
  method <- dplyr::case_when(
    prop_zero > 0.1 | skew_ratio > 4 | tail_share > 0.4 ~ "headtail",
    skew_ratio > 2 ~ "hybrid",
    TRUE ~ "quantile"
  )

  # ---- compute breaks ----
  base_palette <- get_palette(palette)

  if (method == "headtail") {
    breaks <- c()
    current <- x_clean

    # try to get bins breaks
    min_prop <- 0.05
    while (length(current) > min_prop * length(x_clean)) {
      m <- mean(current)
      breaks <- c(breaks, m)
      current <- current[current > m]
      if (length(breaks) >= bin) break
    }

    breaks <- sort(unique(breaks))
    breaks <- c(breaks, Inf)

    # if headtail produced fewer breaks than requested, fall back to hybrid
    if (length(breaks) < bin) {
      bulk <- x_clean[x_clean <= q90]
      bulk_breaks <- stats::quantile(
        bulk,
        probs = seq(0, 1, length.out = bin)
      )
      breaks <- unique(c(bulk_breaks[-1], Inf))
    }

  } else if (method == "hybrid") {
    bulk <- x_clean[x_clean <= q90]
    bulk_breaks <- stats::quantile(
      bulk,
      probs = seq(0, 1, length.out = bin + 1)
    )
    breaks <- c(bulk_breaks[-1], Inf)

  } else {
    breaks <- stats::quantile(
      x_clean,
      probs = seq(0, 1, length.out = bin + 2)
    )[-1]
  }

  breaks <- unique(breaks)

  # ---- build labels ----
  data_min <- min(x_finite, na.rm = TRUE)
  raw_lower <- c(data_min, breaks[-length(breaks)])
  raw_upper <- breaks

  # auto-detect decimal vs integer data based on range
  data_range <- max(x_finite, na.rm = TRUE) - data_min
  is_decimal_data <- data_range <= 2

  # auto-adjust round_to for decimal data
  if (is_decimal_data && round_to == 50) {
    round_to <- 0.05
  }

  # apply rounding
  if (!is.null(round_to)) {
    label_lower <- .round_to_nearest(raw_lower, round_to)
    label_upper <- .round_to_nearest(raw_upper, round_to)
    d <- if (is_decimal_data) decimals else 0
  } else {
    label_lower <- raw_lower
    label_upper <- raw_upper
    d <- decimals
  }

  # helper to build labels
  # use " to " separator when negative numbers are present to avoid

  # confusing labels like "-0.40–-0.20"
  make_labels <- function(lower, upper, digits) {
    fmt <- function(x) .format_number(x, digits = digits)
    has_negative <- any(lower < 0, na.rm = TRUE) | any(upper < 0, na.rm = TRUE)
    sep <- if (has_negative) " to " else "\u2013"
    ifelse(
      is.infinite(upper),
      paste0(">", fmt(lower)),
      paste0(fmt(lower), sep, fmt(upper))
    )
  }

  # check for problems and increase precision if needed
  check_labels <- function(lower, upper, digits) {
    # use .format_number (not formatC) so K/M collisions are caught
    fmt_l <- .format_number(lower, digits = digits)
    fmt_u <- .format_number(upper, digits = digits)
    labs <- make_labels(lower, upper, digits)
    has_dups <- anyDuplicated(labs) > 0
    has_same <- any(fmt_l == fmt_u & !is.infinite(upper))
    list(labels = labs, bad = has_dups || has_same)
  }

  # try with rounded values first
  result <- check_labels(label_lower, label_upper, d)

  # if problems, try finer rounding increments
  # keep decimals constant unless absolutely necessary
  if (result$bad && !is.null(round_to)) {
    original_d <- d
    for (finer in c(round_to / 2, round_to / 5, round_to / 10)) {
      label_lower <- .round_to_nearest(raw_lower, finer)
      label_upper <- .round_to_nearest(raw_upper, finer)
      # try with original decimals first
      result <- check_labels(label_lower, label_upper, original_d)
      if (!result$bad) {
        d <- original_d
        break
      }
      # only increase precision if needed and within reasonable bounds
      if (d < decimals + 3) {
        d <- d + 1
        result <- check_labels(label_lower, label_upper, d)
        if (!result$bad) break
      }
    }
  }

  # last resort: use raw values with higher precision
  # but respect user's decimals parameter and only increase minimally
  if (result$bad) {
    max_d <- decimals + 3
    if (d < decimals) {
      d <- decimals
    }
    result <- check_labels(raw_lower, raw_upper, d)
    while (result$bad && d < max_d) {
      d <- d + 1
      result <- check_labels(raw_lower, raw_upper, d)
    }
  }

  labels <- result$labels

  # ---- cut into bins ----
  bin_result <- cut(
    x,
    breaks = c(-Inf, breaks),
    labels = labels,
    include.lowest = TRUE,
    right = TRUE
  )
  bin_result <- factor(bin_result, levels = labels, ordered = TRUE)

  # ---- build color map ----
  colors <- get_palette(base_palette, n = length(labels))
  if (reverse) colors <- rev(colors)
  names(colors) <- labels

  # count observations per bin
  counts <- as.data.frame(table(bin_result, dnn = "bin"))
  names(counts) <- c("bin", "n")

  list(
    bins = bin_result,
    colors = colors,
    counts = counts,
    method = method,
    diagnostics = list(
      prop_zero = prop_zero,
      skew_ratio = as.numeric(skew_ratio),
      tail_share = tail_share
    )
  )
}
