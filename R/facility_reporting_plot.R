# R/facility_reporting_plot.R
# build monthly facility reporting activity heatmap
# helps teams quickly assess facility engagement over time
# RELEVANT FILES:R/reporting_rate.R,R/consistency_check.R,R/utils.R

#' Plot monthly reporting activity by health facility
#'
#' Builds a balanced monthly panel, flags reporting on key indicators,
#' derives first reporting date and activity status, and returns a ggplot.
#'
#' @param data Data frame containing routine health facility records.
#' @param hf_col Character. Column storing health facility identifiers.
#' @param date_col Character. Column storing observation dates. Defaults to
#'   "date".
#' @param key_indicators Character vector with columns defining reporting
#'   activity. Defaults to `c("test", "pres", "conf")`.
#' @param palette Character. Colour palette for activity statuses. One of
#'   `c("classic", "sunset", "forest", "coral", "violet", "slate",
#'   "citrus", "orchid")`. Defaults to "classic".
#' @param target_language Target language for labels (ISO 639-1). Defaults to
#'   "en".
#' @param source_language Source language for labels. Defaults to "en".
#' @param lang_cache_path Path used to cache translations. Defaults to
#'   `base::tempdir()`.
#' @param save_plot Logical. If TRUE (or `plot_path` supplied), save the plot.
#'   Defaults to FALSE.
#' @param plot_path Path to directory for saving plot output. Required when
#'   saving.
#' @param compress_image Logical. Compress PNG using `compress_png()` after
#'   saving. Defaults to FALSE.
#' @param image_overwrite Logical. Overwrite an existing file when TRUE.
#'   Defaults to TRUE.
#' @param compression_speed Integer (1-10) controlling compression effort.
#'   Defaults to 1.
#' @param compression_verbose Logical. Emit compression progress when TRUE.
#'   Defaults to TRUE.
#'
#' @return A ggplot object visualising facility reporting activity.
#' @examples
#'
#' toy_data <- tibble::tibble(
#'   hf_uid_new = rep(c("HF1", "HF2"), each = 4),
#'   date = rep(
#'     base::seq.Date(
#'       base::as.Date("2024-01-01"),
#'       by = "month",
#'       length.out = 4
#'     ),
#'     times = 2
#'   ),
#'   test = c(NA, 1, 2, NA, NA, NA, 3, 4),
#'   pres = c(0, 2, NA, 1, NA, 1, 2, 3),
#'   conf = c(0, NA, 1, 0, NA, NA, 1, 2)
#' )
#'
#' facility_reporting_plot(
#'   data = toy_data,
#'   hf_col = "hf_uid_new"
#' )
#' @export
facility_reporting_plot <- function(
  data,
  hf_col,
  date_col = "date",
  key_indicators = c("test", "pres", "conf"),
  palette = "classic",
  target_language = "en",
  source_language = "en",
  lang_cache_path = base::tempdir(),
  save_plot = FALSE,
  plot_path = NULL,
  compress_image = FALSE,
  image_overwrite = TRUE,
  compression_speed = 1,
  compression_verbose = TRUE
) {
  ensure_packages(c("ggtext", "scales"))

  if (!base::is.data.frame(data)) {
    cli::cli_abort("`data` must be a data.frame.")
  }

  required_cols <- base::c(hf_col, date_col, key_indicators)
  missing_cols <- base::setdiff(required_cols, base::names(data))
  if (base::length(missing_cols) > 0L) {
    cli::cli_abort("missing required columns: {missing_cols}")
  }

  if (base::all(base::is.na(data[[date_col]]))) {
    cli::cli_abort("`{date_col}` cannot be entirely missing.")
  }

  data <- data |>
    dplyr::mutate(
      !!rlang::sym(date_col) := lubridate::floor_date(
        base::as.Date(.data[[date_col]]),
        unit = "month"
      )
    )

  if (base::all(base::is.na(data[[date_col]]))) {
    cli::cli_abort("Unable to derive month floor from `{date_col}`.")
  }

  palette_values <- list(
    classic = c(
      "Active Reporting" = "#0072B2",
      "Active Facility - Not Reporting" = "#E69F00",
      "Inactive Facility" = "#56B4E9"
    ),
    sunset = c(
      "Active Reporting" = "#D1495B",
      "Active Facility - Not Reporting" = "#F79256",
      "Inactive Facility" = "#8E7DBE"
    ),
    forest = c(
      "Active Reporting" = "#2A9D8F",
      "Active Facility - Not Reporting" = "#E9C46A",
      "Inactive Facility" = "#264653"
    ),
    coral = c(
      "Active Reporting" = "#FF6F61",
      "Active Facility - Not Reporting" = "#FFB88C",
      "Inactive Facility" = "#6B5B95"
    ),
    violet = c(
      "Active Reporting" = "#6A4C93",
      "Active Facility - Not Reporting" = "#F0A6CA",
      "Inactive Facility" = "#80CED7"
    ),
    slate = c(
      "Active Reporting" = "#345995",
      "Active Facility - Not Reporting" = "#FB4D3D",
      "Inactive Facility" = "#98B9AB"
    ),
    citrus = c(
      "Active Reporting" = "#F4A259",
      "Active Facility - Not Reporting" = "#5B8E7D",
      "Inactive Facility" = "#BC4B51"
    ),
    orchid = c(
      "Active Reporting" = "#875C74",
      "Active Facility - Not Reporting" = "#E6C79C",
      "Inactive Facility" = "#6C7A89"
    )
  )

  if (!palette %in% names(palette_values)) {
    cli::cli_abort(c(
      "Invalid palette selection.",
      "i" = "Choose one of {toString(names(palette_values))}."
    ))
  }

  status_colours <- palette_values[[palette]]
  legend_title <- glue::glue(
    "Reported any key indicator ({base::toString(key_indicators)})"
  )
  legend_title_prefix <- "Reported any key indicator"
  legend_labels <- names(status_colours)
  subtitle_lines <- c(
    "Active Reporting -> reported at least one key indicator",
    paste(
      "Active Facility - Not Reporting -> skipped that month but",
      "had reported earlier"
    ),
    "Inactive Facility -> never reported any key indicator"
  )

  facility_ids <- base::unique(data[[hf_col]])
  date_min <- base::min(data[[date_col]], na.rm = TRUE)
  date_max <- base::max(data[[date_col]], na.rm = TRUE)

  month_sequence <- base::seq(date_min, date_max, by = "month")

  complete_panel <- tidyr::expand_grid(
    .facility = facility_ids,
    .month = month_sequence
  ) |>
    dplyr::rename(
      !!rlang::sym(hf_col) := .facility,
      !!rlang::sym(date_col) := .month
    )

  filtered_source <- data |>
    dplyr::select(dplyr::all_of(required_cols))

  balanced_panel <- complete_panel |>
    dplyr::left_join(filtered_source, by = c(hf_col, date_col))

  flagged_panel <- balanced_panel |>
    dplyr::mutate(
      reported_any = dplyr::if_any(
        dplyr::all_of(key_indicators),
        ~ !base::is.na(.x)
      )
    )

  first_reporting <- flagged_panel |>
    dplyr::filter(reported_any) |>
    dplyr::group_by(.data[[hf_col]]) |>
    dplyr::summarise(
      first_reporting_date = base::min(.data[[date_col]], na.rm = TRUE),
      .groups = "drop"
    )

  routine_reporting <- flagged_panel |>
    dplyr::left_join(first_reporting, by = hf_col) |>
    dplyr::group_by(.data[[hf_col]]) |>
    dplyr::mutate(
      has_ever_reported = dplyr::cumany(reported_any),
      activity_status = dplyr::case_when(
        reported_any ~ "Active Reporting",
        has_ever_reported ~ "Active Facility - Not Reporting",
        TRUE ~ "Inactive Facility"
      ),
      activity_status = base::factor(
        activity_status,
        levels = c(
          "Active Reporting",
          "Active Facility - Not Reporting",
          "Inactive Facility"
        )
      )
    ) |>
    dplyr::ungroup()

  facilities_in_plot <- dplyr::n_distinct(routine_reporting[[hf_col]])
  subtitle_text <- base::paste(subtitle_lines, collapse = "\n")
  plot_title <- glue::glue(
    "Monthly reporting activity by health facility",
    " (n = {scales::comma(facilities_in_plot)})"
  )
  should_translate <- target_language != "en"
  if (should_translate) {
    ensure_packages("gtranslate")
    legend_labels <- vapply(
      legend_labels,
      translate_text,
      character(1),
      target_language = target_language,
      source_language = source_language,
      cache_path = lang_cache_path
    )
    legend_title_prefix <- translate_text(
      legend_title_prefix,
      target_language = target_language,
      source_language = source_language,
      cache_path = lang_cache_path
    )
    legend_title <- glue::glue(
      "{legend_title_prefix} ({base::toString(key_indicators)})"
    )
    subtitle_lines <- vapply(
      subtitle_lines,
      translate_text,
      character(1),
      target_language = target_language,
      source_language = source_language,
      cache_path = lang_cache_path
    )
    subtitle_lines <- gsub("\n", " ", subtitle_lines)
    subtitle_text <- base::paste(subtitle_lines, collapse = "\n")
    plot_title <- translate_text(
      plot_title,
      target_language = target_language,
      source_language = source_language,
      cache_path = lang_cache_path
    )
  }
  legend_title <- glue::glue(
    "{legend_title_prefix} ({base::toString(key_indicators)})"
  )
  facility_order <- routine_reporting |>
    dplyr::filter(!base::is.na(first_reporting_date)) |>
    dplyr::distinct(.data[[hf_col]], first_reporting_date) |>
    dplyr::arrange(first_reporting_date) |>
    dplyr::pull(.data[[hf_col]])

  plot_object <- routine_reporting |>
    dplyr::filter(!base::is.na(first_reporting_date)) |>
    ggplot2::ggplot(
      ggplot2::aes(
        x = .data[[date_col]],
        y = forcats::fct_relevel(
          .data[[hf_col]],
          facility_order
        ),
        fill = activity_status
      )
    ) +
    ggplot2::geom_tile(width = 31, height = 1) +
    ggplot2::scale_fill_manual(
      values = status_colours,
      na.value = "#CCCCCC",
      name = legend_title,
      labels = legend_labels
    ) +
    ggplot2::scale_x_date(
      expand = c(0, 0),
      date_labels = "%b %Y",
      date_breaks = "3 months"
    ) +
    ggplot2::labs(
      x = "",
      y = NULL,
      title = plot_title,
      subtitle = subtitle_text
    ) +
    ggplot2::theme_minimal(base_family = "sans") +
    ggplot2::guides(
      fill = ggplot2::guide_legend(
        title.position = "top",
        label.position = "bottom",
        keywidth = grid::unit(4.5, "lines")
      )
    ) +
    ggplot2::theme(
      axis.text.y = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      axis.line = ggplot2::element_line(color = "black", linewidth = 0.5),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "vertical"
    )

  should_save <- isTRUE(save_plot) || !base::is.null(plot_path)
  if (should_save) {
    if (base::is.null(plot_path)) {
      cli::cli_abort("Provide `plot_path` when requesting plot saving.")
    }

    if (!fs::dir_exists(plot_path)) {
      ok <- try(fs::dir_create(plot_path, recurse = TRUE), silent = TRUE)
      if (inherits(ok, "try-error") || !fs::dir_exists(plot_path)) {
        cli::cli_abort("Failed to create directory: {plot_path}.")
      }
    }

    plotted_months <- routine_reporting |>
      dplyr::filter(!base::is.na(first_reporting_date)) |>
      dplyr::distinct(.data[[date_col]]) |>
      base::nrow()
    plotted_facilities <- facilities_in_plot

    width <- base::max(8, plotted_months * 0.35)
    height <- base::max(6, plotted_facilities * 0.15)

    file_name <- glue::glue(
      "facility_reporting_{palette}_{format(Sys.Date(), '%Y-%m-%d')}.png"
    )
    file_path <- fs::path(plot_path, file_name)

    ggplot2::ggsave(
      filename = file_path,
      plot = plot_object,
      width = width,
      height = height,
      dpi = 300,
      limitsize = FALSE
    )

    if (compress_image) {
      compress_png(
        path = file_path,
        png_overwrite = image_overwrite,
        speed = compression_speed,
        verbosity = compression_verbose
      )
    }
  }

  plot_object
}
