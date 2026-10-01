#' Plot Faceted Choropleth Maps from sf Data with Discrete Bins
#'
#' @description
#' Creates a faceted choropleth map from an `sf` object using a binned
#' categorical variable for fill colors. The function is designed for
#' programmatic mapping of indicators over space and time, with optional
#' administrative boundary overlays and optional file export.
#'
#' The function assumes that the fill variable is already discretised
#' (e.g. incidence bins, prevalence bins, rate categories) and mapped
#' explicitly to colors via `fill_colors`.
#'
#' Typical use cases include:
#' - Mapping crude or adjusted incidence by district over multiple years
#' - Visualising binned prevalence or coverage indicators
#' - Producing consistent, publication-ready spatial outputs
#'
#' @param data An `sf` object containing geometries and attributes to plot.
#'   Must include the columns specified in `fill_col` and `facet_col`.
#'
#' @param fill_col Character scalar. Name of the column in `data` used for
#'   polygon fill. This column should be a factor or character vector with
#'   levels matching `fill_colors`.
#'
#' @param facet_col Optional character scalar. Name of the column used for
#'   faceting columns, typically a time variable such as `year`. When `NULL`,
#'   no faceting is applied and a single map is produced. Default is `NULL`.
#'
#' @param facet_row Optional character scalar. Name of the column used for
#'   faceting rows. When provided, the function uses `facet_grid(row ~ col)`
#'   instead of `facet_wrap()`. Default is `NULL`.
#'
#' @param strip_y_position Character scalar, either `"right"` or `"left"`.
#'   Controls which side the row facet labels appear on. Only applies when
#'   both `facet_col` and `facet_row` are provided (i.e., `facet_grid()`).
#'   Default is `"right"`.
#'
#' @param strip_y_angle Optional numeric scalar, one of `-90`, `0` or `90`.
#'   Rotation of the row facet labels: `-90` reads top-to-bottom, `90` reads
#'   bottom-to-top and `0` is horizontal. When `NULL`, the angle follows
#'   `strip_y_position` (`-90` on the right, `90` on the left). Only applies
#'   when `facet_row` is provided. Default is `NULL`.
#'
#' @param adm1_shp Optional `sf` object containing higher-level administrative
#'   boundaries (e.g. ADM1). When provided, boundaries are overlaid as thin
#'   black outlines. Default is `NULL`.
#'
#' @param fill_colors Named character vector of colors. Names must correspond
#'   exactly to the levels in `fill_col`. Colors should be hex codes.
#'
#' @param title Optional character scalar. Plot title. Default is `NULL`
#'   (no title).
#'
#' @param subtitle Optional character scalar. Plot subtitle. Default is
#'   `NULL` (no subtitle).
#'
#' @param caption Optional character scalar. Plot caption (annotation at bottom
#'   right). Default is `NULL` (no caption).
#'
#' @param fill_label Optional character scalar. Legend title for the fill
#'   scale. Default is `NULL` (no legend title).
#'
#' @param ncol Integer. Number of columns in the facet layout. Only used when
#'   `facet_row` is `NULL` (i.e., when using `facet_wrap()`). Default is `3`.
#'
#' @param output_file Optional character scalar. File path where the plot
#'   should be saved. If `NULL`, the plot is not saved to disk.
#'
#' @param width Numeric. Width of the saved plot in inches. Default is `7`.
#'
#' @param height Numeric. Height of the saved plot in inches. Default is `10`.
#'
#' @param dpi Numeric. Resolution (dots per inch) for saved output.
#'   Default is `300`.
#'
#' @param scale Numeric. Scaling factor for the plot. Values greater than 1
#'   make text and elements relatively smaller; values less than 1 make them
#'   larger. Default is `1`.
#'
#' @param title_size Numeric. Font size for the plot title. Default is `14`.
#'
#' @param subtitle_size Numeric. Font size for the plot subtitle. Default
#'   is `11`.
#'
#' @param legend_title_size Numeric. Font size for the legend title.
#'   Default is `10`.
#'
#' @param compress_image Logical. Compress PNG using `compress_png()` after
#'   saving. Defaults to TRUE.
#'
#' @return
#' A `ggplot` object. If `output_file` is provided, the plot is also written
#' to disk using `ggsave()`.
#'
#' @details
#' - Uses `geom_sf()` for spatial rendering.
#' - Uses `scale_fill_manual()` with explicit legend control to ensure
#'   consistent color interpretation across plots.
#' - Faceting is implemented via `facet_wrap()` using a dynamically
#'   constructed formula, or `facet_grid()` when `facet_row` is supplied.
#' - Row strips are drawn on the right by default. Set
#'   `strip_y_position = "left"` to move them to the left of the first
#'   map column.
#' - Row strip text rotation is controlled by `strip_y_angle`. Horizontal
#'   labels (`0`) widen the strip column and are right-aligned on the left
#'   and left-aligned on the right so they sit next to the maps.
#' - The function does not perform binning internally. All binning logic
#'   should be applied upstream.
#'
#' @examples
#' \dontrun{
#' # example incidence color scale (cases per 1,000 population)
#' incidence_colors <- c(
#'   "0-5"       = "#c6dbef",
#'   "5-50"      = "#9ecae1",
#'   "50-100"    = "#4292c6",
#'   "100-250"   = "#fb6a4a",
#'   "250-500"   = "#cb181d",
#'   "500-5,000" = "#7f0000"
#' )
#'
#' facetted_map_bins(
#'   data = incidence_shp,
#'   fill_col = "n1_incidence_bin",
#'   facet_col = "year",
#'   adm1_shp = shp_file$adm1,
#'   fill_colors = incidence_colors,
#'   title = "Crude Malaria Incidence by District (ADM2), Burundi",
#'   subtitle = "All-age crude incidence aggregated annually",
#'   fill_label = "Cases per 1,000 population",
#'   ncol = 3,
#'   output_file = here::here(
#'     "outputs",
#'     "burundi_crude_incidence.png"
#'   )
#' )
#'
#' # row facets on the left-hand side with horizontal labels
#' facetted_map_bins(
#'   data = incidence_shp,
#'   fill_col = "n1_incidence_bin",
#'   facet_col = "year",
#'   facet_row = "age_group",
#'   strip_y_position = "left",
#'   strip_y_angle = 0,
#'   fill_colors = incidence_colors
#' )
#' }
#'
#' @export
facetted_map_bins <- function(
  data,
  fill_col,
  facet_col = NULL,
  facet_row = NULL,
  strip_y_position = c("right", "left"),
  strip_y_angle = NULL,
  adm1_shp = NULL,
  fill_colors,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  fill_label = NULL,
  ncol = 3,
  output_file = NULL,
  width = 7,
  height = 10,
  dpi = 300,
  scale = 1,
  title_size = 14,
  subtitle_size = 11,
  legend_title_size = 10,
  compress_image = TRUE
) {
  strip_y_position <- match.arg(strip_y_position)

  # default angle follows the strip side: -90 on the right, 90 on the left
  if (is.null(strip_y_angle)) {
    strip_y_angle <- if (strip_y_position == "left") 90 else -90
  }

  if (
    !is.numeric(strip_y_angle) ||
      length(strip_y_angle) != 1 ||
      !strip_y_angle %in% c(-90, 0, 90)
  ) {
    cli::cli_abort("{.arg strip_y_angle} must be one of -90, 0 or 90.")
  }

  # horizontal labels sit against the maps: right-aligned on the left strip,
  # left-aligned on the right strip; rotated labels stay centred
  strip_y_hjust <-
    if (strip_y_angle == 0) {
      if (strip_y_position == "left") 1 else 0
    } else {
      0.5
    }

  plot_obj <-
    ggplot2::ggplot(data) +
    ggplot2::geom_sf(
      ggplot2::aes(fill = .data[[fill_col]]),
      color = "white",
      size = 0.2,
      na.rm = TRUE
    )

  if (!is.null(adm1_shp)) {
    plot_obj <-
      plot_obj +
      ggplot2::geom_sf(
        data = adm1_shp,
        fill = NA,
        color = "black",
        linewidth = 0.2,
        inherit.aes = FALSE
      )
  }

  plot_obj <-
    plot_obj +
    ggplot2::scale_fill_manual(
      values = fill_colors,
      drop = TRUE,
      na.translate = FALSE,
      guide = ggplot2::guide_legend(
        label.position = "bottom",
        title.position = "top",
        title.hjust = 0.5,
        override.aes = list(
          colour = "black",
          size = 0.15,
          alpha = 1
        ),
        nrow = 1,
        byrow = TRUE
      )
    )

  # apply faceting only when facet_col is provided
  if (!is.null(facet_col)) {
    if (!is.null(facet_row)) {
      plot_obj <-
        plot_obj +
        ggplot2::facet_grid(
          rows = ggplot2::vars(.data[[facet_row]]),
          cols = ggplot2::vars(.data[[facet_col]]),
          drop = FALSE,
          switch = if (strip_y_position == "left") "y" else NULL
        )
    } else {
      plot_obj <-
        plot_obj +
        ggplot2::facet_wrap(
          stats::as.formula(paste0("~", facet_col)),
          ncol = ncol
        )
    }
  }

  plot_obj <-
    plot_obj +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      caption = caption,
      fill = fill_label
    ) +
    ggplot2::theme_void() +
    ggplot2::theme(
      legend.position = "bottom",
      strip.text = ggplot2::element_text(
        face = "bold",
        size = 10,
        margin = ggplot2::margin(t = 2, b = 6, l = 4, r = 4)
      ),
      # row strip text: same angle/alignment whichever side the strip is on
      strip.text.y = ggplot2::element_text(
        angle = strip_y_angle,
        hjust = strip_y_hjust
      ),
      strip.text.y.left = ggplot2::element_text(
        angle = strip_y_angle,
        hjust = strip_y_hjust
      ),
      strip.switch.pad.grid = grid::unit(4, "pt"),
      panel.spacing = grid::unit(4, "pt"),
      legend.box.margin = ggplot2::margin(t = 8),
      plot.title = ggplot2::element_text(
        size = title_size,
        margin = ggplot2::margin(b = 8)
      ),
      plot.subtitle = ggplot2::element_text(
        size = subtitle_size,
        margin = ggplot2::margin(b = 10)
      ),
      legend.title = ggplot2::element_text(
        size = legend_title_size,
        margin = ggplot2::margin(b = 6)
      ),
      plot.margin = ggplot2::margin(t = 5, r = 5, b = 5, l = 5)
    )

  if (!is.null(output_file)) {
    ggplot2::ggsave(
      plot = plot_obj,
      filename = output_file,
      width = width,
      height = height,
      dpi = dpi,
      scale = scale
    )

    if (compress_image && grepl("\\.png$", output_file, ignore.case = TRUE)) {
      compress_png(path = output_file)
    }
  }

  plot_obj
}

#' Plot Faceted Choropleth Maps from sf Data with Continuous Gradient
#'
#' @description
#' Creates a faceted choropleth map from an `sf` object using a continuous
#' numeric variable for fill colors displayed as a gradient. The function is
#' designed for programmatic mapping of continuous indicators over space and
#' time, with optional administrative boundary overlays and optional file
#' export.
#'
#' The function expects the fill variable to be continuous numeric data
#' (e.g. incidence rates, prevalence percentages, continuous coverage metrics)
#' and maps it to a color gradient.
#'
#' Typical use cases include:
#' - Mapping continuous incidence rates by district over multiple years
#' - Visualising continuous prevalence or coverage indicators
#' - Producing consistent, publication-ready spatial outputs with gradients
#'
#' @param data An `sf` object containing geometries and attributes to plot.
#'   Must include the columns specified in `fill_col` and `facet_col`.
#'
#' @param fill_col Character scalar. Name of the column in `data` used for
#'   polygon fill. This column should be numeric and contain continuous values.
#'
#' @param colors Character vector of hex color codes for the gradient. The
#'   gradient will interpolate smoothly between these colors. Default is a
#'   7-color diverging palette from dark red through yellow to dark blue.
#'
#' @param limits Optional numeric vector of length 2 specifying the minimum
#'   and maximum values for the color scale (e.g., `c(0, 100)` for percentages).
#'   When `NULL`, limits are determined from the data range. Default is `NULL`.
#'
#' @param facet_col Optional character scalar. Name of the column used for
#'   faceting columns, typically a time variable such as `year`. When `NULL`,
#'   no faceting is applied and a single map is produced. Default is `NULL`.
#'
#' @param facet_row Optional character scalar. Name of the column used for
#'   faceting rows. When provided, the function uses `facet_grid(row ~ col)`
#'   instead of `facet_wrap()`. Default is `NULL`.
#'
#' @param strip_y_position Character scalar, either `"right"` or `"left"`.
#'   Controls which side the row facet labels appear on. Only applies when
#'   both `facet_col` and `facet_row` are provided (i.e., `facet_grid()`).
#'   Default is `"right"`.
#'
#' @param strip_y_angle Optional numeric scalar, one of `-90`, `0` or `90`.
#'   Rotation of the row facet labels: `-90` reads top-to-bottom, `90` reads
#'   bottom-to-top and `0` is horizontal. When `NULL`, the angle follows
#'   `strip_y_position` (`-90` on the right, `90` on the left). Only applies
#'   when `facet_row` is provided. Default is `NULL`.
#'
#' @param adm1_shp Optional `sf` object containing higher-level administrative
#'   boundaries (e.g. ADM1). When provided, boundaries are overlaid as thin
#'   black outlines. Default is `NULL`.
#'
#' @param title Optional character scalar. Plot title. Default is `NULL`
#'   (no title).
#'
#' @param subtitle Optional character scalar. Plot subtitle. Default is
#'   `NULL` (no subtitle).
#'
#' @param caption Optional character scalar. Plot caption (annotation at bottom
#'   right). Default is `NULL` (no caption).
#'
#' @param fill_label Optional character scalar. Legend title for the fill
#'   scale. Default is `NULL` (no legend title).
#'
#' @param ncol Integer. Number of columns in the facet layout. Only used when
#'   `facet_row` is `NULL` (i.e., when using `facet_wrap()`). Default is `3`.
#'
#' @param output_file Optional character scalar. File path where the plot
#'   should be saved. If `NULL`, the plot is not saved to disk.
#'
#' @param width Numeric. Width of the saved plot in inches. Default is `7`.
#'
#' @param height Numeric. Height of the saved plot in inches. Default is `10`.
#'
#' @param dpi Numeric. Resolution (dots per inch) for saved output.
#'   Default is `300`.
#'
#' @param scale Numeric. Scaling factor for the plot. Values greater than 1
#'   make text and elements relatively smaller; values less than 1 make them
#'   larger. Default is `1`.
#'
#' @param title_size Numeric. Font size for the plot title. Default is `14`.
#'
#' @param subtitle_size Numeric. Font size for the plot subtitle. Default
#'   is `11`.
#'
#' @param legend_title_size Numeric. Font size for the legend title.
#'   Default is `10`.
#'
#' @param compress_image Logical. Compress PNG using `compress_png()` after
#'   saving. Defaults to TRUE.
#'
#' @return
#' A `ggplot` object. If `output_file` is provided, the plot is also written
#' to disk using `ggsave()`.
#'
#' @details
#' - Uses `geom_sf()` for spatial rendering.
#' - Uses `scale_fill_gradientn()` to create a smooth color gradient.
#' - Faceting is implemented via `facet_wrap()` or `facet_grid()` using a
#'   dynamically constructed formula.
#' - Row strips are drawn on the right by default. Set
#'   `strip_y_position = "left"` to move them to the left of the first
#'   map column.
#' - Row strip text rotation is controlled by `strip_y_angle`. Horizontal
#'   labels (`0`) widen the strip column and are right-aligned on the left
#'   and left-aligned on the right so they sit next to the maps.
#' - The function works directly with continuous numeric data without binning.
#'
#' @examples
#' \dontrun{
#' # continuous incidence rates
#' facetted_map_gradient(
#'   data = incidence_shp,
#'   fill_col = "incidence_rate",
#'   facet_col = "year",
#'   adm1_shp = shp_file$adm1,
#'   title = "Continuous Malaria Incidence by District (ADM2), Burundi",
#'   subtitle = "All-age crude incidence aggregated annually",
#'   fill_label = "Cases per 1,000 population",
#'   ncol = 3,
#'   output_file = here::here(
#'     "outputs",
#'     "burundi_continuous_incidence.png"
#'   )
#' )
#'
#' # percentage coverage with fixed 0-100 scale
#' facetted_map_gradient(
#'   data = coverage_shp,
#'   fill_col = "coverage_pct",
#'   limits = c(0, 100),
#'   facet_col = "year",
#'   title = "ITN Coverage (%)",
#'   fill_label = "Coverage (%)"
#' )
#'
#' # gene (rows) by year (columns), left-hand labels, horizontal text
#' facetted_map_gradient(
#'   data = res_marker_sf,
#'   fill_col = "mean_freq",
#'   limits = c(0, 1),
#'   facet_col = "year",
#'   facet_row = "gene",
#'   strip_y_position = "left",
#'   strip_y_angle = 0,
#'   adm1_shp = shp_file$adm1,
#'   fill_label = "Mean mutation frequency"
#' )
#' }
#'
#' @export
facetted_map_gradient <- function(
  data,
  fill_col,
  colors = c(
    "#7b0d0d", "#c0392b", "#e67e22", "#f7dc6f",
    "#d6eaf8", "#5dade2", "#1a5276"
  ),
  limits = NULL,
  facet_col = NULL,
  facet_row = NULL,
  strip_y_position = c("right", "left"),
  strip_y_angle = NULL,
  adm1_shp = NULL,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  fill_label = NULL,
  ncol = 3,
  output_file = NULL,
  width = 7,
  height = 10,
  dpi = 300,
  scale = 1,
  title_size = 14,
  subtitle_size = 11,
  legend_title_size = 10,
  compress_image = TRUE
) {
  strip_y_position <- match.arg(strip_y_position)

  # default angle follows the strip side: -90 on the right, 90 on the left
  if (is.null(strip_y_angle)) {
    strip_y_angle <- if (strip_y_position == "left") 90 else -90
  }

  if (
    !is.numeric(strip_y_angle) ||
      length(strip_y_angle) != 1 ||
      !strip_y_angle %in% c(-90, 0, 90)
  ) {
    cli::cli_abort("{.arg strip_y_angle} must be one of -90, 0 or 90.")
  }

  # horizontal labels sit against the maps: right-aligned on the left strip,
  # left-aligned on the right strip; rotated labels stay centred
  strip_y_hjust <-
    if (strip_y_angle == 0) {
      if (strip_y_position == "left") 1 else 0
    } else {
      0.5
    }

  plot_obj <-
    ggplot2::ggplot(data) +
    ggplot2::geom_sf(
      ggplot2::aes(fill = .data[[fill_col]]),
      color = "white",
      size = 0.2,
      na.rm = TRUE
    )

  if (!is.null(adm1_shp)) {
    plot_obj <-
      plot_obj +
      ggplot2::geom_sf(
        data = adm1_shp,
        fill = NA,
        color = "black",
        linewidth = 0.2,
        inherit.aes = FALSE
      )
  }

  plot_obj <-
    plot_obj +
    ggplot2::scale_fill_gradientn(
      colors = colors,
      limits = limits,
      na.value = "grey90",
      guide = ggplot2::guide_colorbar(
        title.position = "top",
        title.hjust = 0.5,
        barwidth = grid::unit(15, "lines"),
        barheight = grid::unit(0.5, "lines")
      )
    )

  if (!is.null(facet_col)) {
    if (!is.null(facet_row)) {
      plot_obj <-
        plot_obj +
        ggplot2::facet_grid(
          rows = ggplot2::vars(.data[[facet_row]]),
          cols = ggplot2::vars(.data[[facet_col]]),
          drop = FALSE,
          switch = if (strip_y_position == "left") "y" else NULL
        )
    } else {
      plot_obj <-
        plot_obj +
        ggplot2::facet_wrap(
          stats::as.formula(paste0("~", facet_col)),
          ncol = ncol
        )
    }
  }

  plot_obj <-
    plot_obj +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      caption = caption,
      fill = fill_label
    ) +
    ggplot2::theme_void() +
    ggplot2::theme(
      legend.position = "bottom",
      strip.text = ggplot2::element_text(
        face = "bold",
        size = 10,
        margin = ggplot2::margin(t = 2, b = 6, l = 4, r = 4)
      ),
      # row strip text: same angle/alignment whichever side the strip is on
      strip.text.y = ggplot2::element_text(
        angle = strip_y_angle,
        hjust = strip_y_hjust
      ),
      strip.text.y.left = ggplot2::element_text(
        angle = strip_y_angle,
        hjust = strip_y_hjust
      ),
      strip.switch.pad.grid = grid::unit(4, "pt"),
      panel.spacing = grid::unit(4, "pt"),
      legend.box.margin = ggplot2::margin(t = 8),
      plot.title = ggplot2::element_text(
        size = title_size,
        margin = ggplot2::margin(b = 8)
      ),
      plot.subtitle = ggplot2::element_text(
        size = subtitle_size,
        margin = ggplot2::margin(b = 10)
      ),
      legend.title = ggplot2::element_text(
        size = legend_title_size,
        margin = ggplot2::margin(b = 6)
      ),
      plot.margin = ggplot2::margin(t = 5, r = 5, b = 5, l = 5)
    )

  if (!is.null(output_file)) {
    ggplot2::ggsave(
      plot = plot_obj,
      filename = output_file,
      width = width,
      height = height,
      dpi = dpi,
      scale = scale
    )

    if (compress_image && grepl("\\.png$", output_file, ignore.case = TRUE)) {
      compress_png(path = output_file)
    }
  }

  plot_obj
}