#' Color provinces so touching neighbors never share a color
#'
#' This function takes an adm2-level shapefile, dissolves it into provinces
#' (adm1), constructs a neighbor graph, assigns colors to provinces ensuring
#' that no adjacent provinces share the same color, and then maps those
#' province colors back to the adm2 polygons. Labels are placed at the centroid
#' of each province, and a `ggplot2` map is produced. The plot can optionally
#' be saved as a PNG.
#'
#' @param shp sf. Polygon layer at adm2 level containing a column for provinces
#'   (adm1) and unique adm2 identifiers.
#' @param province_col character. Column name for province (adm1).
#' @param id_col character. Column name for unique adm2 identifiers.
#' @param out_png character|NULL. File path to save PNG output. If `NULL`,
#'   no file is saved.
#' @param palette character. Vector of hex colors to use for coloring provinces.
#'   Will be recycled if there are more provinces than colors.
#' @param fix_valid logical. If `TRUE`, run `sf::st_make_valid()` to fix
#'   invalid geometries before processing.
#'
#' @return A list with three components:
#' \itemize{
#'   \item shp: `sf` with province colors joined back to adm2 polygons
#'   \item labels: `sf` with label coordinates (x, y) for each province
#'   \item gg: `ggplot` object of the colored map with labels
#' }
#'
#' @examples
#' # Not run:
#' # res <- plot_admin_map_distinct(
#' #   shp = shp_output_old$spat_vec$adm2,
#' #   province_col = "adm1",
#' #   id_col = "adm2",
#' #   out_png = here::here("03_outputs/plots/burundi_health_dist_old2.png")
#' # )
#' # print(res$gg)
#'
#' @export
plot_admin_map_distinct <- function(
  shp,
  province_col,
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
  miss <- setdiff(c(province_col, id_col), names(shp))
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

  # ---- dissolve adm2 to provinces (adm1) ----
  prov <- shp |>
    dplyr::group_by(.data[[province_col]]) |>
    dplyr::summarise(geometry = sf::st_union(geometry), .groups = "drop")

  n_prov <- nrow(prov)
  n_pal <- length(palette)

  if (n_prov == 0) {
    cli::cli_abort("No provinces found after dissolving by `{province_col}`.")
  }

  # ---- build adjacency list ----
  nb <- sf::st_touches(prov)

  # ---- initial color assignment (cycle palette) ----
  prov$color_idx <- ((seq_len(n_prov) - 1) %% n_pal) + 1

  # ---- resolve conflicts ----
  for (i in seq_len(n_prov)) {
    neigh <- nb[[i]]
    if (length(neigh) == 0) next

    used <- prov$color_idx[neigh]

    # if conflict, pick first unused color
    if (prov$color_idx[i] %in% used) {
      available <- setdiff(seq_len(n_pal), used)
      prov$color_idx[i] <- if (length(available) == 0) 1 else available[1]
    }
  }
  prov$fill_col <- palette[prov$color_idx]

  # ---- join province colors back to adm2 ----
  shp <- shp |>
    dplyr::left_join(
      prov |>
        dplyr::select(dplyr::all_of(province_col), fill_col) |>
        sf::st_drop_geometry(),
      by = province_col
    )

  # ---- compute label coordinates ----
  labels <- prov |>
    dplyr::mutate(
      pt = suppressWarnings(sf::st_point_on_surface(geometry)),
      coords = sf::st_coordinates(pt),
      x = coords[, 1],
      y = coords[, 2]
    ) |>
    dplyr::select(dplyr::all_of(province_col), x, y)

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

  # Add labels with shadowtext if available, otherwise fallback to geom_text
  if (rlang::is_installed("shadowtext")) {
    gg <- gg +
      shadowtext::geom_shadowtext(
        data = labels,
        ggplot2::aes(x = x, y = y, label = .data[[province_col]]),
        size = 4,
        fontface = "bold",
        color = "black",
        bg.color = "white",
        bg.r = 0.35,
        check_overlap = TRUE
      )
  } else {
    cli::cli_alert_info(
      "Package {\"shadowtext\"} not installed; using ggplot2::geom_text instead."
    )
    gg <- gg +
      ggplot2::geom_text(
        data = labels,
        ggplot2::aes(x = x, y = y, label = .data[[province_col]]),
        size = 4,
        fontface = "bold",
        color = "black",
        check_overlap = TRUE
      )
  }

  # ---- save to file if requested ----
  if (!is.null(out_png)) {
    ggplot2::ggsave(out_png, gg, width = 10, height = 10, dpi = 300)
  }

  # ---- return ----
  invisible(list(shp = shp, labels = labels, gg = gg))
}
