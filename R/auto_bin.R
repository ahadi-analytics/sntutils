#' Round to nearest multiple
#'
#' @param x Numeric vector to round.
#' @param to Rounding increment.
#' @return Rounded numeric vector.
#' @noRd
.round_to_nearest <- function(x, to) {
  round(x / to) * to
}

#' Format number with commas
#'
#' @param x Numeric vector.
#' @return Character vector with comma-separated thousands.
#' @noRd
.format_number <- function(x) {
  formatC(x, format = "f", digits = 0, big.mark = ",")
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
      "#9ecae1", "#6baed6", "#4292c6", "#fecc5c",
      "#fd8d3c", "#f46d43", "#e31a1c", "#800026"
    ),
    gyor = c(
      "#31a354", "#a1d99b", "#a6cee3", "#fecc5c",
      "#fd8d3c", "#f46d43", "#e31a1c", "#800026"
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
#'
#' @return A list with:
#'   \item{bins}{Ordered factor of bin labels for each value in `x`}
#'   \item{colors}{Named character vector mapping labels to colors}
#'   \item{counts}{Data frame with bin labels and counts (n)}
#'   \item{method}{Character. The binning method used: "headtail", "hybrid",
#'     or "quantile"}
#'   \item{diagnostics}{List with prop_zero, skew_ratio, and tail_share}
#'
#' @details
#' Method selection logic:
#' - **headtail**: prop_zero > 0.1, skew_ratio > 4, or tail_share > 0.4
#' - **hybrid**: skew_ratio > 2
#' - **quantile**: otherwise
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
#' @export
auto_bin <- function(
    x,
    palette = "default",
    bin = 6,
    decimals = 2,
    round_to = 50,
    reverse = FALSE) {

  if (!is.numeric(x)) {
    cli::cli_abort("{.arg x} must be numeric.")
  }

  x_clean <- x[!is.na(x) & x > 0]

  if (length(x_clean) == 0) {
    cli::cli_abort("No positive non-NA values in {.arg x}.")
  }

  # ---- diagnostics ----
  q50 <- stats::quantile(x_clean, 0.5)
  q90 <- stats::quantile(x_clean, 0.9)

  skew_ratio <- q90 / q50
  tail_share <- sum(x_clean[x_clean > q90]) / sum(x_clean)
  prop_zero <- mean(x == 0, na.rm = TRUE)

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
  raw_lower <- c(0, breaks[-length(breaks)])
  raw_upper <- breaks

  # auto-detect decimal vs integer data
  is_decimal_data <- max(x_clean, na.rm = TRUE) < 1

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
  make_labels <- function(lower, upper, digits) {
    fmt <- function(x) formatC(x, format = "f", digits = digits, big.mark = ",")
    ifelse(
      is.infinite(upper),
      paste0(">", fmt(lower)),
      paste0(fmt(lower), "\u2013", fmt(upper))
    )
  }

  # check for problems and increase precision if needed
  check_labels <- function(lower, upper, digits) {
    fmt_l <- formatC(lower, format = "f", digits = digits)
    fmt_u <- formatC(upper, format = "f", digits = digits)
    labs <- make_labels(lower, upper, digits)
    has_dups <- anyDuplicated(labs) > 0
    has_same <- any(fmt_l == fmt_u & !is.infinite(upper))
    list(labels = labs, bad = has_dups || has_same)
  }

  # try with rounded values first
  result <- check_labels(label_lower, label_upper, d)

  # if problems, try finer rounding increments
  if (result$bad && !is.null(round_to)) {
    for (finer in c(round_to / 2, round_to / 5, round_to / 10)) {
      label_lower <- .round_to_nearest(raw_lower, finer)
      label_upper <- .round_to_nearest(raw_upper, finer)
      d <- d + 1
      result <- check_labels(label_lower, label_upper, d)
      if (!result$bad) break
    }
  }

  # last resort: use raw values with high precision
  if (result$bad) {
    d <- max(d, 3)
    result <- check_labels(raw_lower, raw_upper, d)
    while (result$bad && d < 6) {
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
