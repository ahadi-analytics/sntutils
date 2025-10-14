#' Classify health facility activity status by reporting behaviour
#'
#' Builds a balanced monthly panel for all facilities and months, flags
#' reporting on key indicators, and classifies each facility-month into an
#' activity status: "Active Reporting", "Active Facility - Not Reporting",
#' or "Inactive Facility".
#'
#' @param data Data frame containing routine health facility records.
#' @param hf_col Character. Column storing health facility identifiers.
#' @param date_col Character. Column storing observation dates. Defaults to
#'   "date".
#' @param key_indicators Character vector with columns defining reporting
#'   activity. Defaults to `c("test", "pres", "conf")`.
#' @param binary_classification Logical. If TRUE, returns binary classification
#'   ("Active", "Non-Active") instead of three-level classification. Defaults to
#'   FALSE.
#'
#' @return Data frame with original columns plus:
#'   \describe{
#'     \item{reported_any}{Logical. TRUE if any key indicator was reported
#'       that month.}
#'     \item{first_reporting_date}{Date. First month the facility reported
#'       any key indicator.}
#'     \item{has_ever_reported}{Logical. TRUE if facility has reported at
#'       least once up to this month.}
#'     \item{activity_status}{Factor. One of "Active Reporting",
#'       "Active Facility - Not Reporting", or "Inactive Facility" when
#'       binary_classification = FALSE. When binary_classification = TRUE,
#'       one of "Active" or "Non-Active".}
#'   }
#'
#' @details
#' Activity status logic:
#' \itemize{
#'   \item **Active Reporting**: Facility reported at least one key indicator
#'     in that month.
#'   \item **Active Facility - Not Reporting**: Facility has reported before
#'     but did not report in that month.
#'   \item **Inactive Facility**: Facility has never reported any key indicator.
#' }
#'
#' When binary_classification = TRUE:
#' \itemize{
#'   \item **Active**: Facility reported at least one key indicator in that month
#'     (equivalent to "Active Reporting").
#'   \item **Non-Active**: Facility did not report in that month, regardless of
#'     past reporting history (combines "Active Facility - Not Reporting" and
#'     "Inactive Facility").
#' }
#'
#' @examples
#' toy_data <- tibble::tibble(
#'   hf_uid = rep(c("HF1", "HF2"), each = 4),
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
#' classified <- classify_facility_activity(
#'   data = toy_data,
#'   hf_col = "hf_uid",
#'   key_indicators = c("test", "pres", "conf")
#' )
#'
#' table(classified$activity_status)
#'
#' # Binary classification example
#' classified_binary <- classify_facility_activity(
#'   data = toy_data,
#'   hf_col = "hf_uid",
#'   key_indicators = c("test", "pres", "conf"),
#'   binary_classification = TRUE
#' )
#'
#' table(classified_binary$activity_status)
#' @export
classify_facility_activity <- function(
  data,
  hf_col,
  date_col = "date",
  key_indicators = c("test", "pres", "conf"),
  binary_classification = FALSE
) {
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
      activity_status = if (binary_classification) {
        dplyr::case_when(
          reported_any ~ "Active",
          TRUE ~ "Non-Active"
        )
      } else {
        dplyr::case_when(
          reported_any ~ "Active Reporting",
          has_ever_reported ~ "Active Facility - Not Reporting",
          TRUE ~ "Inactive Facility"
        )
      },
      activity_status = base::factor(
        activity_status,
        levels = if (binary_classification) {
          c("Active", "Non-Active")
        } else {
          c(
            "Active Reporting",
            "Active Facility - Not Reporting",
            "Inactive Facility"
          )
        }
      )
    ) |>
    dplyr::ungroup()

  routine_reporting
}

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
#' @param binary_classification Logical. If TRUE, uses binary classification
#'   ("Active", "Non-Active") instead of three-level classification. Defaults to
#'   FALSE.
#' @param facet_col Character. Optional column name to use for faceting the plot.
#'   When provided, creates separate panels for each unique value in this column
#'   (e.g., one panel per province or administrative region). Can be any column
#'   type. Default is NULL (no faceting).
#' @param facet_ncol Integer. Number of columns for facet layout when
#'   `facet_col` is provided. Defaults to 2.
#' @param year_breaks Numeric value specifying the interval (in months) for
#'   x-axis date breaks. If NULL (default), uses "3 months".
#' @param palette Character. Colour palette for activity statuses. One of
#'   `c("classic", "sunset", "forest", "coral", "violet", "slate",
#'   "citrus", "orchid")`. Defaults to "classic".
#' @param include_never_reported Logical. If TRUE, includes facilities that
#'   have never reported in the plot. If TRUE (default), only shows
#'   facilities that have reported at least once.
#' @param target_language Target language for labels (ISO 639-1). Defaults to
#'   "en".
#' @param source_language Source language for labels. Defaults to "en".
#' @param lang_cache_path Path used to cache translations. Defaults to
#'   `base::tempdir()`.
#' @param plot_path Path to directory for saving plot output. If NULL (default),
#'   plot is not saved.
#' @param compress_image Logical. Compress PNG using `compress_png()` after
#'   saving. Defaults to FALSE.
#' @param image_overwrite Logical. Overwrite an existing file when TRUE.
#'   Defaults to TRUE.
#' @param compression_speed Integer (1-10) controlling compression effort.
#'   Defaults to 1.
#' @param compression_verbose Logical. Emit compression progress when TRUE.
#'   Defaults to TRUE.
#' @param plot_scale Numeric. Scaling factor for saved plots. Values > 1
#'   increase size, < 1 decrease size. Default is 0.75.
#' @param plot_width Numeric. Width of saved plot in inches. Default is 20.
#' @param plot_height Numeric. Height of saved plot in inches. Default is 15.
#' @param plot_dpi Numeric. Resolution of saved plot in dots per inch.
#'   Default is 300.
#' @param show_plot Logical. If FALSE, the plot is returned invisibly (not
#'   displayed). Useful when only saving plots. Default is TRUE.
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
#'
#' # Binary classification example
#' facility_reporting_plot(
#'   data = toy_data,
#'   hf_col = "hf_uid_new",
#'   binary_classification = TRUE
#' )
#' @export
facility_reporting_plot <- function(
  data,
  hf_col,
  date_col = "date",
  key_indicators = c("test", "pres", "conf"),
  binary_classification = FALSE,
  facet_col = NULL,
  facet_ncol = 2,
  year_breaks = NULL,
  palette = "classic",
  include_never_reported = TRUE,
  target_language = "en",
  source_language = "en",
  lang_cache_path = base::tempdir(),
  plot_path = NULL,
  compress_image = FALSE,
  image_overwrite = TRUE,
  compression_speed = 1,
  compression_verbose = TRUE,
  plot_scale = 0.75,
  plot_width = 22,
  plot_height = 15,
  plot_dpi = 300,
  show_plot = TRUE
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

  if (binary_classification) {
    palette_values <- list(
      classic = c(
        "Active" = "#0072B2",
        "Non-Active" = "#E69F00"
      ),
      sunset = c(
        "Active" = "#D1495B",
        "Non-Active" = "#F79256"
      ),
      forest = c(
        "Active" = "#2A9D8F",
        "Non-Active" = "#E9C46A"
      ),
      coral = c(
        "Active" = "#FF6F61",
        "Non-Active" = "#FFB88C"
      ),
      violet = c(
        "Active" = "#6A4C93",
        "Non-Active" = "#F0A6CA"
      ),
      slate = c(
        "Active" = "#345995",
        "Non-Active" = "#FB4D3D"
      ),
      citrus = c(
        "Active" = "#F4A259",
        "Non-Active" = "#5B8E7D"
      ),
      orchid = c(
        "Active" = "#875C74",
        "Non-Active" = "#E6C79C"
      )
    )
  } else {
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
  }

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

  if (binary_classification) {
    subtitle_lines <- c(
      "Active -> reported at least one key indicator",
      "Non-Active -> did not report any key indicator that month"
    )
  } else {
    subtitle_lines <- c(
      "Active Reporting -> reported at least one key indicator",
      paste(
        "Active Facility - Not Reporting -> did not report that month but",
        "had reported in previous months"
      ),
      "Inactive Facility -> never reported any key indicator"
    )
  }

  # Check if facet_col exists in the original data if provided
  if (!is.null(facet_col) && !facet_col %in% names(data)) {
    cli::cli_abort(
      "`facet_col` must be a column in the data. Available columns: {toString(names(data))}"
    )
  }

  routine_reporting <- classify_facility_activity(
    data = data,
    hf_col = hf_col,
    date_col = date_col,
    key_indicators = key_indicators,
    binary_classification = binary_classification
  )

  # If facet_col is provided, add it to the routine_reporting data
  if (!is.null(facet_col)) {
    # Join the facet column from original data
    facet_data <- data |>
      dplyr::mutate(
        !!rlang::sym(date_col) := lubridate::floor_date(
          base::as.Date(.data[[date_col]]),
          unit = "month"
        )
      ) |>
      dplyr::select(dplyr::all_of(c(hf_col, date_col, facet_col))) |>
      dplyr::distinct()

    routine_reporting <- routine_reporting |>
      dplyr::left_join(facet_data, by = c(hf_col, date_col))
  }

  # Count never-reported facilities after creating routine_reporting
  never_reported_count <- routine_reporting |>
    dplyr::filter(base::is.na(first_reporting_date)) |>
    dplyr::pull(.data[[hf_col]]) |>
    dplyr::n_distinct()

  # Add never reported summary if there are any
  if (never_reported_count > 0) {
    never_reported_line <- glue::glue(
      "{scales::comma(never_reported_count)} facilities never reported ",
      "at least one of the key indicators"
    )
    subtitle_lines <- c(subtitle_lines, "", never_reported_line)
  }

  # Count facilities based on include_never_reported parameter
  if (include_never_reported) {
    facilities_shown <- dplyr::n_distinct(routine_reporting[[hf_col]])
    plot_title <- glue::glue(
      "Monthly reporting activity by health facility",
      " (n = {scales::comma(facilities_shown)})"
    )
  } else {
    facilities_shown <- routine_reporting |>
      dplyr::filter(!base::is.na(first_reporting_date)) |>
      dplyr::pull(.data[[hf_col]]) |>
      dplyr::n_distinct()

    total_facilities <- dplyr::n_distinct(routine_reporting[[hf_col]])

    plot_title <- glue::glue(
      "Monthly reporting activity by health facility",
      " (n = {scales::comma(facilities_shown)} shown, ",
      "{scales::comma(total_facilities)} total)"
    )
  }

  subtitle_text <- base::paste(subtitle_lines, collapse = "\n")
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
    # Translate subtitle lines, handling the never reported line specially
    if (never_reported_count > 0) {
      # Translate all but the last line (which is the never reported summary)
      base_lines <- subtitle_lines[1:(length(subtitle_lines) - 2)]
      translated_base <- vapply(
        base_lines,
        translate_text,
        character(1),
        target_language = target_language,
        source_language = source_language,
        cache_path = lang_cache_path
      )

      # Translate the never reported text preserving number formatting
      facilities_text <- translate_text(
        "facilities never reported at least one of the key indicators",
        target_language = target_language,
        source_language = source_language,
        cache_path = lang_cache_path
      )
      never_reported_translated <- glue::glue(
        "{scales::comma(never_reported_count)} {facilities_text}"
      )

      subtitle_lines <- c(translated_base, "", never_reported_translated)
    } else {
      subtitle_lines <- vapply(
        subtitle_lines,
        translate_text,
        character(1),
        target_language = target_language,
        source_language = source_language,
        cache_path = lang_cache_path
      )
    }
    subtitle_lines <- gsub("\n", " ", subtitle_lines)
    subtitle_text <- base::paste(subtitle_lines, collapse = "\n")

    # Translate title parts separately to preserve number formatting
    title_base <- translate_text(
      "Monthly reporting activity by health facility",
      target_language = target_language,
      source_language = source_language,
      cache_path = lang_cache_path
    )

    if (include_never_reported) {
      plot_title <- glue::glue(
        "{title_base}",
        " (n = {scales::comma(facilities_shown)})"
      )
    } else {
      shown_text <- translate_text(
        "shown",
        target_language = target_language,
        source_language = source_language,
        cache_path = lang_cache_path
      )
      total_text <- translate_text(
        "total",
        target_language = target_language,
        source_language = source_language,
        cache_path = lang_cache_path
      )
      plot_title <- glue::glue(
        "{title_base}",
        " (n = {scales::comma(facilities_shown)} {shown_text}, ",
        "{scales::comma(total_facilities)} {total_text})"
      )
    }
  }

  if (include_never_reported) {
    # First get facilities that have reported, ordered by first reporting date
    facilities_reported <- routine_reporting |>
      dplyr::filter(!base::is.na(first_reporting_date)) |>
      dplyr::distinct(.data[[hf_col]], first_reporting_date) |>
      dplyr::arrange(first_reporting_date) |>
      dplyr::pull(.data[[hf_col]])

    # Then get facilities that have never reported
    facilities_never_reported <- routine_reporting |>
      dplyr::filter(base::is.na(first_reporting_date)) |>
      dplyr::distinct(.data[[hf_col]]) |>
      dplyr::pull(.data[[hf_col]])

    # Combine: reported facilities first, then never reported
    facility_order <- c(facilities_reported, facilities_never_reported)
  } else {
    # Original behavior: only facilities that have reported
    facility_order <- routine_reporting |>
      dplyr::filter(!base::is.na(first_reporting_date)) |>
      dplyr::distinct(.data[[hf_col]], first_reporting_date) |>
      dplyr::arrange(first_reporting_date) |>
      dplyr::pull(.data[[hf_col]])
  }

  # Calculate y-axis breaks
  n_facilities <- length(facility_order)
  if (n_facilities > 0) {
    y_breaks <- c(1, ceiling(n_facilities / 2), n_facilities)
    # Keep 1-based indexing to match the title count and format with big_mark
    y_labels <- vapply(y_breaks, function(x) big_mark(x), character(1))
  } else {
    y_breaks <- NULL
    y_labels <- NULL
  }

  # Filter data based on include_never_reported parameter
  plot_data <- if (include_never_reported) {
    routine_reporting
  } else {
    routine_reporting |>
      dplyr::filter(!base::is.na(first_reporting_date))
  }

  plot_object <- plot_data |>
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
      date_breaks = if (is.null(year_breaks)) "3 months" else paste(year_breaks, "months"),
      labels = function(x) {
        sntutils::translate_yearmon(x, language = target_language)
      }
    ) +
    ggplot2::labs(
      x = "",
      y = if (should_translate) {
        paste0(
          translate_text(
            "HF Number",
            target_language = target_language,
            source_language = source_language,
            cache_path = lang_cache_path
          ),
          "\n"
        )
      } else {
        "HF Number\n"
      },
      title = plot_title,
      subtitle = paste0(subtitle_text, "\n")
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
      axis.text.y = ggplot2::element_text(size = 8),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      axis.line = ggplot2::element_line(color = "black", linewidth = 0.5),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "vertical"
    )

  # Add faceting if facet_col is provided
  if (!is.null(facet_col)) {
    plot_object <- plot_object +
      ggplot2::facet_wrap(
        ~ .data[[facet_col]],
        scales = "free_y",
        ncol = facet_ncol
      )

    # When using free_y scales with faceting, use a function for y-axis labels
    plot_object <- plot_object +
      ggplot2::scale_y_discrete(
        labels = function(x) {
          # Get position of each facility within its facet
          pos <- seq_along(x)
          # Only show first, middle, and last
          ifelse(pos %in% c(1, ceiling(length(pos)/2), length(pos)),
                 pos,
                 "")
        }
      )
  } else {
    # Add y-axis scale if we have facilities (only when not faceting)
    if (!is.null(y_breaks) && n_facilities > 0) {
      plot_object <- plot_object +
        ggplot2::scale_y_discrete(
          breaks = facility_order[y_breaks],
          labels = y_labels
        )
    }
  }

  if (!base::is.null(plot_path)) {
    make_slug <- function(text) {
      cleaned <- tolower(text)
      cleaned <- gsub("[^[:alnum:]]+", "_", cleaned)
      cleaned <- gsub("_+", "_", cleaned)
      gsub("^_+|_+$", "", cleaned)
    }

    if (!fs::dir_exists(plot_path)) {
      ok <- try(fs::dir_create(plot_path, recurse = TRUE), silent = TRUE)
      if (inherits(ok, "try-error") || !fs::dir_exists(plot_path)) {
        cli::cli_abort("Failed to create directory: {plot_path}.")
      }
    }

    file_start <- format(base::min(data[[date_col]], na.rm = TRUE), "%Y-%m")
    file_end <- format(base::max(data[[date_col]], na.rm = TRUE), "%Y-%m")
    connector_word <- if (should_translate) {
      translate_text(
        "to",
        target_language = target_language,
        source_language = source_language,
        cache_path = lang_cache_path
      )
    } else {
      "to"
    }

    date_range_text <- if (identical(file_start, file_end)) {
      file_start
    } else {
      glue::glue("{file_start} {connector_word} {file_end}")
    }

    base_label <- "Health facility activeness status"
    if (should_translate) {
      base_label <- translate_text(
        base_label,
        target_language = target_language,
        source_language = source_language,
        cache_path = lang_cache_path
      )
    }

    file_base <- make_slug(base_label)
    date_range_slug <- make_slug(date_range_text)

    # Add facet column to filename if provided
    if (!is.null(facet_col)) {
      by_word <- if (should_translate) {
        translate_text(
          "by",
          target_language = target_language,
          source_language = source_language,
          cache_path = lang_cache_path
        )
      } else {
        "by"
      }
      facet_slug <- make_slug(paste(by_word, facet_col))
      file_name <- glue::glue(
        "{file_base}_{facet_slug}_{date_range_slug}_v{format(Sys.Date(), '%Y-%m-%d')}.png"
      )
    } else {
      file_name <- glue::glue(
        "{file_base}_{date_range_slug}_v{format(Sys.Date(), '%Y-%m-%d')}.png"
      )
    }
    file_path <- fs::path(plot_path, file_name)

    ggplot2::ggsave(
      filename = file_path,
      plot = plot_object,
      width = plot_width,
      height = plot_height,
      dpi = plot_dpi,
      scale = plot_scale,
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

    # Success message with shortened path
    success_msg <- if (should_translate) {
      translate_text(
        "Plot saved to:",
        target_language = target_language,
        source_language = source_language,
        cache_path = lang_cache_path
      )
    } else {
      "Plot saved to:"
    }

    # Show only relative path from current directory if it's a subdirectory
    display_path <- file_path
    if (startsWith(as.character(file_path), getwd())) {
      display_path <- sub(
        paste0("^", getwd(), "/"),
        "",
        as.character(file_path)
      )
    } else if (grepl("03_outputs", as.character(file_path))) {
      # Extract from 03_outputs onward if present
      display_path <- sub(".*/(03_outputs/.*)", "\\1", as.character(file_path))
    }
    cli::cli_alert_success(paste(success_msg, display_path))
  }

  # Return invisibly if show_plot is FALSE
  if (show_plot) {
    return(plot_object)
  } else {
    return(invisible(plot_object))
  }
}
