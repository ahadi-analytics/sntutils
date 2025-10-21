#' Color admin groups so touching neighbors never share a color
#'
#' This function takes an adm2-level shapefile, dissolves it into a higher
#' administrative level (e.g., provinces at adm1), constructs a neighbor graph,
#' assigns colors to the aggregated units ensuring that no adjacent units share
#' the same color, and then maps those colors back to the adm2 polygons.
#' Labels are placed at the centroid of each higher-level unit, and a
#' `ggplot2` map is produced. The plot can optionally be saved as a PNG.
#'
#' @param shp sf. Polygon layer at adm2 level containing a column for the
#'   higher-level grouping (adm1) and unique adm2 identifiers.
#' @param group_col character. Column name for higher-level admin grouping
#'   (e.g., adm1).
#' @param id_col character. Column name for unique adm2 identifiers.
#' @param out_png character|NULL. File path to save PNG output. If `NULL`,
#'   no file is saved.
#' @param palette character. Vector of hex colors to use for coloring groups.
#'   Will be recycled if there are more groups than colors.
#' @param fix_valid logical. If `TRUE`, run `sf::st_make_valid()` to fix
#'   invalid geometries before processing.
#'
#' @return A list with three components:
#' \itemize{
#'   \item shp: `sf` with group colors joined back to adm2 polygons
#'   \item labels: `sf` with label coordinates (x, y) for each group
#'   \item gg: `ggplot` object of the colored map with labels
#' }
#'
#' @examples
#' # Not run:
#' # res <- plot_admin_map_distinct(
#' #   shp = shp_output_old$spat_vec$adm2,
#' #   group_col = "adm1",
#' #   id_col = "adm2",
#' #   out_png = here::here("03_outputs/plots/burundi_health_dist_old2.png")
#' # )
#' # print(res$gg)
#'
#' @export
plot_admin_map_distinct <- function(
  shp,
  group_col,
  id_col,
  out_png = NULL,
  palette = c(
    "#D95F02", # orange
    "#1B9E77", # green
    "#7570B3", # purple
    "#E7298A", # pink
    "#66A61E", # olive
    "#E6AB02"  # mustard
  ),
  fix_valid = FALSE
) {
  # ---- validate inputs ----
  if (!inherits(shp, "sf")) {
    cli::cli_abort("`shp` must be an sf object.")
  }
  miss <- setdiff(c(group_col, id_col), names(shp))
  if (length(miss) > 0) {
    cli::cli_abort("Missing columns: {miss}")
  }
  if (length(palette) < 1) {
    cli::cli_abort("`palette` must contain at least one color.")
  }

  # ---- geometry cleanup ----
  if (fix_valid) {
    shp <- sf::st_make_valid(shp)
  }
  shp <- sf::st_zm(shp, drop = TRUE, what = "ZM")

  # ---- dissolve adm2 to grouped units (adm1) ----
  groups <- shp |>
    dplyr::group_by(.data[[group_col]]) |>
    dplyr::summarise(geometry = sf::st_union(geometry), .groups = "drop")

  n_groups <- nrow(groups)
  n_pal <- length(palette)

  if (n_groups == 0) {
    cli::cli_abort("No groups found after dissolving by `{group_col}`.")
  }

  # ---- build adjacency list ----
  nb <- sf::st_touches(groups)

  # ---- initial color assignment (cycle palette) ----
  groups$color_idx <- ((seq_len(n_groups) - 1) %% n_pal) + 1

  # ---- resolve conflicts ----
  for (i in seq_len(n_groups)) {
    neigh <- nb[[i]]
    if (length(neigh) == 0) next

    used <- groups$color_idx[neigh]

    # if conflict, pick first unused color
    if (groups$color_idx[i] %in% used) {
      available <- setdiff(seq_len(n_pal), used)
      groups$color_idx[i] <- if (length(available) == 0) 1 else available[1]
    }
  }
  groups$fill_col <- palette[groups$color_idx]

  # ---- join group colors back to adm2 ----
  shp <- shp |>
    dplyr::left_join(
      groups |>
        dplyr::select(dplyr::all_of(group_col), fill_col) |>
        sf::st_drop_geometry(),
      by = group_col
    )

  # ---- compute label coordinates ----
  labels <- groups |>
    dplyr::mutate(
      pt = suppressWarnings(sf::st_point_on_surface(geometry)),
      coords = sf::st_coordinates(pt),
      x = coords[, 1],
      y = coords[, 2]
    ) |>
    dplyr::select(dplyr::all_of(group_col), x, y)

  # ---- build map ----
  gg <- ggplot2::ggplot(shp) +
    ggplot2::geom_sf(
      ggplot2::aes(fill = fill_col),
      color = "white",
      linewidth = 0.35
    ) +
    ggplot2::scale_fill_identity() +
    ggplot2::theme_void(base_size = 14) +
    ggplot2::theme(legend.position = "none")

  # Add labels with ggrepel if available, otherwise fallback to geom_text
  if (rlang::is_installed("ggrepel")) {
    gg <- gg +
      ggrepel::geom_text_repel(
        data = labels,
        ggplot2::aes(x = x, y = y, label = .data[[group_col]]),
        size = 3,
        fontface = "bold",
        box.padding = 0.3,
        segment.color = NA,
        bg.color = "white",
        bg.r = 0.5,
        padding = grid::unit(1.5, "lines")
      )
  } else {
    cli::cli_alert_info(
      "Package {\"ggrepel\"} not installed; using ggplot2::geom_text instead."
    )
    gg <- gg +
      ggplot2::geom_text(
        data = labels,
        ggplot2::aes(x = x, y = y, label = .data[[group_col]]),
        size = 4,
        fontface = "bold",
        color = "black",
        check_overlap = TRUE
      )
  }

  # ---- save to file if requested ----
  if (!is.null(out_png)) {
    # scale plot size based on bounding box footprint
    bbox <- sf::st_bbox(shp)
    bbox_width <- as.numeric(bbox[["xmax"]] - bbox[["xmin"]])
    bbox_height <- as.numeric(bbox[["ymax"]] - bbox[["ymin"]])
    max_extent <- base::max(bbox_width, bbox_height, na.rm = TRUE)

    if (!base::is.finite(max_extent) || max_extent == 0) {
      width <- 10
      height <- 10
    } else {
      width <- 10 * (bbox_width / max_extent)
      height <- 10 * (bbox_height / max_extent)
      width <- base::max(width, 6)
      height <- base::max(height, 6)
    }

    ggplot2::ggsave(
      filename = out_png,
      plot = gg,
      width = width,
      height = height,
      dpi = 300
    )
  }

  # ---- return ----
  invisible(list(shp = shp, labels = labels, gg = gg))
}
