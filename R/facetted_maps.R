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
#' @param adm1_shp Optional `sf` object containing higher-level administrative
#'   boundaries (e.g. ADM1). When provided, boundaries are overlaid as thin
#'   black outlines. Default is `NULL`.
#'
#' @param fill_colors Named character vector of colors. Names must correspond
#'   exactly to the levels in `fill_col`. Colors should be hex codes.
#'
#' @param title Character scalar. Plot title.
#'
#' @param subtitle Character scalar. Plot subtitle.
#'
#' @param fill_label Character scalar. Legend title for the fill scale.
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
#' @return
#' A `ggplot` object. If `output_file` is provided, the plot is also written
#' to disk using `ggsave()`.
#'
#' @details
#' - Uses `geom_sf()` for spatial rendering.
#' - Uses `scale_fill_manual()` with explicit legend control to ensure
#'   consistent color interpretation across plots.
#' - Faceting is implemented via `facet_wrap()` using a dynamically
#'   constructed formula.
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
#' }
#'
#' @export
facetted_map_bins <- function(
  data,
  fill_col,
  facet_col = NULL,
  facet_row = NULL,
  adm1_shp = NULL,
  fill_colors,
  title,
  subtitle,
  fill_label,
  ncol = 3,
  output_file = NULL,
  width = 7,
  height = 10,
  dpi = 300,
  scale = 1
) {
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
          drop = FALSE
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
      fill = fill_label
    ) +
    ggplot2::theme_void() +
    ggplot2::theme(
      legend.position = "bottom",
      strip.text = ggplot2::element_text(face = "bold", size = 10),
      strip.text.y = ggplot2::element_text(angle = -90),
      panel.spacing = ggplot2::unit(1, "lines"),
      plot.title = ggplot2::element_text(
        margin = ggplot2::margin(b = 8)
      ),
      plot.subtitle = ggplot2::element_text(
        margin = ggplot2::margin(b = 10)
      ),
      legend.title = ggplot2::element_text(
        margin = ggplot2::margin(b = 6)
      )
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
  }

  plot_obj
}
