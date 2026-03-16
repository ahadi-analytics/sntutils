#' Classify health facility activity status by reporting behaviour
#'
#' Builds a balanced monthly panel for all facilities and months, flags
#' reporting on key indicators, and classifies each facility-month into
#' activity status according to one of three methods.
#'
#' @param data Data frame containing routine health facility records.
#' @param hf_col Character. Column storing health facility identifiers.
#' @param date_col Character. Column storing observation dates. Defaults to
#'   "date".
#' @param key_indicators Character vector with columns defining reporting
#'   activity. Defaults to `c("test", "pres", "conf")`.
#' @param method Character or numeric. Classification method - can be numeric
#'   (1, 2, 3) or character ("method1", "method2", "method3", "all").
#' @param nonreport_window Integer. Minimum number of consecutive non-reporting
#'   months to classify a facility as inactive in method 3. Defaults to 6.
#' @param reporting_rule Character. Defines what counts as reporting:
#'   `"any_non_na"` (default, counts NA as non-reporting, 0 counts as reported)
#'   or `"positive_only"` (requires >0 value to count as reported).
#' @param binary_classification Logical. If TRUE, collapses categories into
#'   "Active" vs "Inactive". Defaults to FALSE.
#'
#' @return Data frame with original columns plus reporting and activity status.
#' If `method = "all"`, includes all three activity classification columns.
#'
#' @details
#' Three activity classification methods are supported:
#' \itemize{
#'   \item **Method 1 (Permanent activation):** Facility is active from first
#'   report onwards, remains active afterwards.
#'   \item **Method 2 (First-Last activation):** Facility is active only between
#'   first and last report dates.
#'   \item **Method 3 (Dynamic):** Facility is active, but becomes inactive if
#'   it misses `nonreport_window` consecutive months. Reactivates if reporting
#'   resumes.
#' }
#'
#' The `reporting_rule` parameter controls how reporting is flagged:
#' \itemize{
#'   \item `"any_non_na"`: Any non-missing value counts as reported (including 0).
#'   \item `"positive_only"`: Only values strictly >0 count as reported.
#' }
#' @importFrom lubridate %m+%
#' @importFrom stats ave
#' @export
classify_facility_activity <- function(
  data,
  hf_col,
  date_col = "date",
  key_indicators = c("test", "pres", "conf"),
  method = 1,
  nonreport_window = 6,
  reporting_rule = "any_non_na",
  binary_classification = FALSE
) {
  # --- validation ---
  if (is.numeric(method)) {
    if (!method %in% 1:3) {
      cli::cli_abort("method must be 1, 2, or 3 when numeric")
    }
    method <- paste0("method", method)
  } else if (is.character(method)) {
    if (method %in% c("1", "2", "3")) {
      method <- paste0("method", method)
    } else if (!method %in% c("method1", "method2", "method3", "all")) {
      cli::cli_abort("method must be 'method1', 'method2', 'method3', or 'all'")
    }
  }

  reporting_rule <- match.arg(reporting_rule, c("any_non_na", "positive_only"))

  if (!is.data.frame(data)) {
    cli::cli_abort("`data` must be a data.frame.")
  }

  if (
    !is.numeric(nonreport_window) ||
      length(nonreport_window) != 1L ||
      is.na(nonreport_window) ||
      nonreport_window < 1
  ) {
    cli::cli_abort("`nonreport_window` must be a positive integer.")
  }

  nonreport_window <- as.integer(nonreport_window)

  required_cols <- c(hf_col, date_col, key_indicators)
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0L) {
    cli::cli_abort("missing required columns: {missing_cols}")
  }

  # --- prepare panel ---
  data <- data |>
    dplyr::mutate(
      !!rlang::sym(date_col) := lubridate::floor_date(
        as.Date(.data[[date_col]]),
        "month"
      )
    )

  facility_ids <- unique(data[[hf_col]])
  date_min <- min(data[[date_col]], na.rm = TRUE)
  date_max <- max(data[[date_col]], na.rm = TRUE)
  month_sequence <- seq(date_min, date_max, by = "month")

  complete_panel <- tidyr::expand_grid(
    .facility = facility_ids,
    .month = month_sequence
  ) |>
    dplyr::rename(
      !!rlang::sym(hf_col) := .facility,
      !!rlang::sym(date_col) := .month
    )

  # --- flag reporting (preserve non-key columns) ---
non_key_cols <- setdiff(names(data), c(required_cols, key_indicators))

flagged <- data |>
  # keep indicator + metadata columns
  dplyr::select(
    dplyr::all_of(c(hf_col, date_col, key_indicators, non_key_cols))
  ) |>
  # ensure one row per facility-month before expanding
  dplyr::distinct(
    !!rlang::sym(hf_col),
    !!rlang::sym(date_col),
    .keep_all = TRUE
  ) |>
  # create full panel
  dplyr::right_join(complete_panel, by = c(hf_col, date_col)) |>
  # fill only non-indicator metadata, not key indicators
  dplyr::group_by(.data[[hf_col]]) |>
  tidyr::fill(dplyr::all_of(non_key_cols), .direction = "downup") |>
  dplyr::ungroup() |>
  # flag reporting status based on chosen rule
  dplyr::mutate(
    reported_any = dplyr::if_any(
      dplyr::all_of(key_indicators),
      ~ if (reporting_rule == "any_non_na") {
        !is.na(.x)
      } else {
        !is.na(.x) & .x > 0
      }
    ),
    reported_any = dplyr::coalesce(reported_any, FALSE)
  ) |>
  # sort chronologically within facility
  dplyr::arrange(.data[[hf_col]], .data[[date_col]])

  # --- method 1 ---
  m1 <- flagged |>
    dplyr::group_by(.data[[hf_col]]) |>
    dplyr::arrange(.data[[date_col]], .by_group = TRUE) |>
    dplyr::mutate(
      ever_reported = dplyr::cumany(reported_any),
      activity_status_method1 = dplyr::case_when(
        !any(reported_any) ~ "Inactive",
        !ever_reported ~ "Inactive",
        reported_any ~ "Active Reporting",
        TRUE ~ "Active \u2013 Not Reporting"
      )
    ) |>
    dplyr::ungroup()

# --- method 2 (first-last activation) ---
m2 <- flagged |>
  dplyr::group_by(.data[[hf_col]]) |>
  dplyr::arrange(.data[[date_col]], .by_group = TRUE) |>
  dplyr::mutate(
    first_reporting_date = if (any(reported_any)) {
      min(.data[[date_col]][reported_any])
    } else {
      as.Date(NA)
    },
    last_reporting_date = if (any(reported_any)) {
      max(.data[[date_col]][reported_any])
    } else {
      as.Date(NA)
    },
    activity_status_method2 = dplyr::case_when(
      is.na(first_reporting_date) ~ "Inactive",
      .data[[date_col]] < first_reporting_date ~ "Inactive",
      reported_any ~ "Active Reporting",
      .data[[date_col]] >= first_reporting_date &
        .data[[date_col]] <= last_reporting_date ~ "Active \u2013 Not Reporting",
      .data[[date_col]] > last_reporting_date ~ "Active \u2013 Not Reporting",
      TRUE ~ "Inactive"
    )
  ) |>
  dplyr::mutate(
    last_date = max(.data[[date_col]]),
    # handle case with no reports (last_reporting_date = NA)
    months_since_last_report = dplyr::if_else(
      is.na(last_reporting_date),
      Inf,
      as.integer(
        lubridate::interval(last_reporting_date, last_date) /
          lubridate::dmonths(1)
      )
    ),
    activity_status_method2 = dplyr::if_else(
      !is.na(last_reporting_date) &
        months_since_last_report >= nonreport_window &
        .data[[date_col]] > last_reporting_date,
      "Inactive",
      activity_status_method2
    )
  ) |>
  dplyr::ungroup()

  # --- method 3 ---
m3 <- flagged |>
  dplyr::group_by(.data[[hf_col]]) |>
  dplyr::arrange(.data[[date_col]], .by_group = TRUE) |>
  dplyr::mutate(
    has_reported = any(reported_any),
    first_rep = ifelse(
      has_reported,
      min(.data[[date_col]][reported_any]),
      as.Date(NA)
    ),
    last_rep = ifelse(
      has_reported,
      max(.data[[date_col]][reported_any]),
      as.Date(NA)
    ),
    gap = dplyr::if_else(!reported_any, 1L, 0L),
    gap_run = data.table::rleid(gap),
    run_len = stats::ave(gap, gap_run, FUN = length),
    activity_status_method3 = dplyr::case_when(
      !any(reported_any) ~ "Inactive",
      is.na(first_rep) ~ "Inactive",
      .data[[date_col]] < first_rep ~ "Inactive",
      reported_any ~ "Active Reporting",
      gap == 1 & run_len < nonreport_window ~ "Active \u2013 Not Reporting",
      gap == 1 & run_len >= nonreport_window ~ "Inactive",
      TRUE ~ "Inactive"
    )
  ) |>
  dplyr::ungroup()

  m1 <- m1 |>
    dplyr::distinct(
      !!rlang::sym(hf_col),
      !!rlang::sym(date_col),
      .keep_all = TRUE
    )

  m2 <- m2 |>
    dplyr::distinct(
      !!rlang::sym(hf_col),
      !!rlang::sym(date_col),
      .keep_all = TRUE
    )

  m3 <- m3 |>
    dplyr::distinct(
      !!rlang::sym(hf_col),
      !!rlang::sym(date_col),
      .keep_all = TRUE
    )

  flagged <- flagged |>
    dplyr::distinct(
      !!rlang::sym(hf_col),
      !!rlang::sym(date_col),
      .keep_all = TRUE
    )

  # --- merge ---
  merged <- flagged |>
    dplyr::left_join(
      m1 |>
        dplyr::select(dplyr::all_of(c(
          hf_col,
          date_col,
          "activity_status_method1"
        ))),
      by = c(hf_col, date_col)
    ) |>
    dplyr::left_join(
      m2 |>
        dplyr::select(dplyr::all_of(c(
          hf_col,
          date_col,
          "first_reporting_date",
          "last_reporting_date",
          "activity_status_method2"
        ))),
      by = c(hf_col, date_col)
    ) |>
    dplyr::left_join(
      m3 |>
        dplyr::select(dplyr::all_of(c(
          hf_col,
          date_col,
          "activity_status_method3"
        ))),
      by = c(hf_col, date_col)
    )

  # --- binary collapse ---
  if (binary_classification) {
    merged <- merged |>
      dplyr::mutate(
        dplyr::across(
          dplyr::starts_with("activity_status_"),
          ~ dplyr::case_when(
            .x %in% c("Active Reporting", "Active \u2013 Not Reporting", "Active - Not Reporting") ~ "Active",
            TRUE ~ "Inactive"
          )
        )
      )
  }

  # --- output ---
  if (method != "all") {
    keep_col <- paste0("activity_status_", method)
    merged <- merged |>
      dplyr::mutate(activity_status = .data[[keep_col]])

    cols_to_keep <- c(
      names(flagged),
      "first_reporting_date",
      "last_reporting_date",
      "activity_status"
    )
    cols_to_keep <- cols_to_keep[cols_to_keep %in% names(merged)]

    merged <- merged |>
      dplyr::select(dplyr::all_of(cols_to_keep))
  }

  merged
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
#' @param method Character or numeric. Classification method - can be numeric
#'   (1, 2, 3) or character ("method1", "method2", "method3", "all").
#'   Defaults to 1.
#' @param nonreport_window Integer. Minimum number of consecutive non-reporting
#'   months to classify a facility as inactive in method 3. Defaults to 6.
#' @param reporting_rule Character. Defines what counts as reporting:
#'   `"any_non_na"` (default, counts NA as non-reporting, 0 counts as reported)
#'   or `"positive_only"` (requires >0 value to count as reported).
#' @param binary_classification Logical. If TRUE, uses binary classification
#'   ("Active", "Inactive") instead of three-level classification. Defaults to
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
#' @param ... Additional arguments passed to internal functions.
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
  method = 1,
  nonreport_window = 6,
  reporting_rule = "any_non_na",
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
  show_plot = TRUE,
  ...
) {

  ## ensure_packages(c("ggtext", "scales"))

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

  # Validate facet_col if provided
  if (!base::is.null(facet_col) && !facet_col %in% base::names(data)) {
    cli::cli_abort("`facet_col` must be a column in the data. Available columns: {base::paste(base::names(data), collapse = ', ')}")
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
        "Inactive" = "#E69F00"
      ),
      sunset = c(
        "Active" = "#D1495B",
        "Inactive" = "#F79256"
      ),
      forest = c(
        "Active" = "#2A9D8F",
        "Inactive" = "#E9C46A"
      ),
      coral = c(
        "Active" = "#FF6F61",
        "Inactive" = "#FFB88C"
      ),
      violet = c(
        "Active" = "#6A4C93",
        "Inactive" = "#F0A6CA"
      ),
      slate = c(
        "Active" = "#345995",
        "Inactive" = "#FB4D3D"
      ),
      citrus = c(
        "Active" = "#F4A259",
        "Inactive" = "#5B8E7D"
      ),
      orchid = c(
        "Active" = "#875C74",
        "Inactive" = "#E6C79C"
      )
    )
  } else {
    palette_values <- list(
      classic = c(
        "Active Reporting" = "#0072B2",
        "Active \u2013 Not Reporting" = "#E69F00",
        "Active - Not Reporting" = "#E69F00",
        "Inactive" = "#56B4E9"
      ),
      sunset = c(
        "Active Reporting" = "#D1495B",
        "Active \u2013 Not Reporting" = "#F79256",
        "Active - Not Reporting" = "#F79256",
        "Inactive" = "#8E7DBE"
      ),
      forest = c(
        "Active Reporting" = "#2A9D8F",
        "Active \u2013 Not Reporting" = "#E9C46A",
        "Active - Not Reporting" = "#E9C46A",
        "Inactive" = "#264653"
      ),
      coral = c(
        "Active Reporting" = "#FF6F61",
        "Active \u2013 Not Reporting" = "#FFB88C",
        "Active - Not Reporting" = "#FFB88C",
        "Inactive" = "#6B5B95"
      ),
      violet = c(
        "Active Reporting" = "#6A4C93",
        "Active \u2013 Not Reporting" = "#F0A6CA",
        "Active - Not Reporting" = "#F0A6CA",
        "Inactive" = "#80CED7"
      ),
      slate = c(
        "Active Reporting" = "#345995",
        "Active \u2013 Not Reporting" = "#FB4D3D",
        "Active - Not Reporting" = "#FB4D3D",
        "Inactive" = "#98B9AB"
      ),
      citrus = c(
        "Active Reporting" = "#F4A259",
        "Active \u2013 Not Reporting" = "#5B8E7D",
        "Active - Not Reporting" = "#5B8E7D",
        "Inactive" = "#BC4B51"
      ),
      orchid = c(
        "Active Reporting" = "#875C74",
        "Active \u2013 Not Reporting" = "#E6C79C",
        "Active - Not Reporting" = "#E6C79C",
        "Inactive" = "#6C7A89"
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

  # Create legend labels with friendly names
  if (binary_classification) {
    legend_labels <- c(
      "Active" = "Active",
      "Inactive" = "Inactive"
    )
  } else {
    legend_labels <- c(
      "Active Reporting" = "Active Reporting",
      "Active \u2013 Not Reporting" = "Active Health Facility - Not Reporting",
      "Active - Not Reporting" = "Active Health Facility - Not Reporting",
      "Inactive" = "Inactive Health Facility"
    )
  }

  if (binary_classification) {
    subtitle_lines <- c(
      "Active -> reported at least one key indicator",
      "Inactive -> did not report any key indicator that month"
    )
  } else {
    subtitle_lines <- c(
      "Active Reporting -> reported at least one key indicator",
      paste(
        "Active Health Facility - Not Reporting -> did not report that month but",
        "had reported in previous months"
      ),
      "Inactive Health Facility -> never reported any key indicator"
    )
  }

  # Check if facet_col exists in the original data if provided
  if (!is.null(facet_col) && !facet_col %in% names(data)) {
    cli::cli_abort(
      "`facet_col` must be a column in the data. Available columns: {toString(names(data))}"
    )
  }

  # Normalize method parameter first
  if (is.numeric(method)) {
    if (!method %in% 1:3) {
      cli::cli_abort(c(
        "!" = "method must be 1, 2, or 3 when numeric",
        "i" = "You provided: {method}"
      ))
    }
    method_normalized <- paste0("method", method)
  } else if (is.character(method)) {
    # Accept both "method1" and "1" formats
    if (method %in% c("1", "2", "3")) {
      method_normalized <- paste0("method", method)
    } else if (!method %in% c("method1", "method2", "method3")) {
      cli::cli_abort(c(
        "!" =
        "method must be 'method1', 'method2', 'method3', or numeric 1, 2, 3",
        "i" = "You provided: {method}"
      ))
    } else {
      method_normalized <- method
    }
  } else {
    cli::cli_abort("method must be numeric (1, 2, 3) or character")
  }

  # If facet_col is provided, include it in the data passed to classify_facility_activity
  data_for_classification <- data
  if (!is.null(facet_col)) {
    # Ensure facet_col is included in the classification data
    required_cols_with_facet <- base::c(hf_col, date_col, key_indicators, facet_col)
    data_for_classification <- data |>
    dplyr::select(dplyr::any_of(required_cols_with_facet))
  }

  routine_reporting <- classify_facility_activity(
    data = data_for_classification,
    hf_col = hf_col,
    date_col = date_col,
    key_indicators = key_indicators,
    method = method_normalized,
    nonreport_window = nonreport_window,
    reporting_rule = reporting_rule,
    binary_classification = binary_classification
  )

  # Verify facet column is present if requested
  if (!is.null(facet_col) && !facet_col %in% names(routine_reporting)) {
    cli::cli_abort("Facet column `{facet_col}` was lost during activity classification. This may indicate an issue with the data processing.")
  }

  # Count never-reported facilities after creating routine_reporting
  never_reported_count <- routine_reporting |>
  dplyr::filter(base::is.na(first_reporting_date)) |>
  dplyr::pull(.data[[hf_col]]) |>
  dplyr::n_distinct()

  # Add never reported summary if there are any
  if (never_reported_count > 0) {
    never_reported_line <- glue::glue(
      "{sntutils::big_mark(never_reported_count)} facilities never reported ",
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

  # Add method descriptions
  method_descriptions <- list(
    method1 = "Method 1: permanent activation since first report",
    method2 = "Method 2: active after first report, inactive after last report",
    method3 = "Method 3: dynamic activation and inactivation"
  )

  current_method_desc <- method_descriptions[[method_normalized]]
  if (is.null(current_method_desc)) {
    current_method_desc <- method_descriptions[["method1"]]
  }

  subtitle_text <- base::paste(subtitle_lines, collapse = "\n")
  should_translate <- target_language != "en"
  if (should_translate) {
    ## ensure_packages("gtranslate")
    # Translate the label values while preserving the names (keys)
    translated_values <- vapply(
      legend_labels,
      translate_text,
      character(1),
      target_language = target_language,
      source_language = source_language,
      cache_path = lang_cache_path
    )
    names(translated_values) <- names(legend_labels)
    legend_labels <- translated_values
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

    # Translate method description
    current_method_desc <- translate_text(
      current_method_desc,
      target_language = target_language,
      source_language = source_language,
      cache_path = lang_cache_path
    )

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

  # Prepare legend labels for scale_fill_manual
  scale_labels <- if (binary_classification) {
    legend_labels
  } else {
    # Get unique activity status values from the data
    unique_statuses <- unique(plot_data$activity_status)
    # Create a named vector for labels
    labels_vec <- legend_labels[names(legend_labels) %in% unique_statuses]
    labels_vec
  }

  # prepare ordered factor column outside of aes() to avoid splicing errors
  plot_data <- plot_data |>
    dplyr::mutate(
      .hf_ordered = forcats::fct_relevel(
        base::as.character(.data[[hf_col]]),
        !!!base::as.character(facility_order)
      )
    )

  plot_object <- plot_data |>
  ggplot2::ggplot(
    ggplot2::aes(
      x = .data[[date_col]],
      y = .hf_ordered,
      fill = .data$activity_status
    )
  ) +
  ggplot2::geom_tile(width = 31, height = 1) +
  ggplot2::scale_fill_manual(
    values = status_colours,
    na.value = "#CCCCCC",
    name = legend_title,
    labels = scale_labels
  ) +
  ggplot2::scale_x_date(
    expand = c(0, 0),
    date_breaks = if (is.null(year_breaks)) {
      "3 months"
    } else {
      paste(year_breaks, "months")
    },
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
    subtitle = paste0(subtitle_text, "\n"),
    caption = current_method_desc
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
    legend.box = "vertical",
    plot.caption = ggplot2::element_text(hjust = 1, size = 9, color = "grey40")
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
        ifelse(pos %in% c(1, ceiling(length(pos) / 2), length(pos)), pos, "")
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

    # compact date range (no connector word for shorter filenames)
    date_range_slug <- if (identical(file_start, file_end)) {
      file_start
    } else {
      glue::glue("{file_start}_{file_end}")
    }

    # use abbreviated base label for compact filenames
    file_base <- if (target_language == "fr") {
      "fosa_statut"
    } else {
      "hf_status"
    }

    # extract method number for compact filename
    method_slug <- make_slug(method_normalized)

    # add facet column to filename if provided (without "by" word)
    if (!is.null(facet_col)) {
      facet_slug <- make_slug(facet_col)
      file_name <- glue::glue(
        "{file_base}_{method_slug}_{facet_slug}_{date_range_slug}_",
        "v{format(Sys.Date(), '%Y-%m-%d')}.png"
      )
    } else {
      file_name <- glue::glue(
        "{file_base}_{method_slug}_{date_range_slug}_",
        "v{format(Sys.Date(), '%Y-%m-%d')}.png"
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
      limitsize = FALSE,
      ...
    )

    if (compress_image) {
      compress_png(
        path = file_path,
        png_overwrite = image_overwrite,
        speed = compression_speed,
        verbosity = compression_verbose,
        ...
      )
    }

    # Success message with shortened path
    success_msg <- if (should_translate) {
      translate_text(
        "Plot saved to:",
        target_language = target_language,
        source_language = source_language,
        cache_path = lang_cache_path,
        ...
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

#' Compare facility activity classification methods (multilingual)
#'
#' Runs three classification methods and produces pairwise comparison plots,
#' plus a time-series plot of active facilities over time. Optionally saves
#' the output to file with auto-language filenames.
#'
#' @param data Facility reporting dataset.
#' @param hf_col Health facility ID column.
#' @param date_col Date column.
#' @param key_indicators Indicators used for activity classification.
#' @param nonreport_window Window size for non-reporting definition.
#' @param language Output language: "en" (English), "fr" (French), "pt"
#'        (Portuguese).
#' @param plot_path Directory where plot should be saved (NULL = don't save).
#' @param width Plot width (default 15).
#' @param height Plot height (default 6).
#' @param units Units for width/height ("in", "cm", "mm"). Default "in".
#' @param dpi Resolution in dots per inch. Default 300.
#' @param scale Multiplicative scale factor for size. Default 1.
#' @param compress_image Logical. Compress PNG using `compress_png()` after
#'   saving. Defaults to FALSE.
#'
#' @return A patchwork object with scatterplots and a time-series plot.
#' @export
compare_methods_plot <- function(
  data,
  hf_col,
  date_col,
  key_indicators,
  nonreport_window = 6,
  language = "en",
  plot_path = NULL,
  width = 16,
  height = 12,
  units = "in",
  dpi = 300,
  scale = .85,
  compress_image = FALSE
) {
  # translations
  titles <- list(
    en = paste0(
      "Comparison of Methods for Classifying Health ",
      "Facility Reporting Activity"
    ),
    fr = paste0(
      "Comparaison des m\u00e9thodes de classification ",
      "de l'activit\u00e9 de rapportage des FOSA"
    ),
    pt = paste0(
      "Compara\u00e7\u00e3o dos m\u00e9todos de classifica\u00e7\u00e3o da ",
      "atividade de reporte das unidades de sa\u00fade"
    )
  )

  word_method <- list(en = "Method", fr = "M\u00e9thode", pt = "M\u00e9todo")

  xlabs <- list(en = " reporting", fr = " en rapportage", pt = " em reporte")
  ylabs <- xlabs

  y_active_facilities <- switch(
    language,
    en = "Active facilities",
    fr = "FOSA actives",
    pt = "Unidades de sa\u00fade ativas"
  )

  x_time_axis <- switch(
    language,
    en = "Month (over time)",
    fr = "Mois (au fil du temps)",
    pt = "M\u00eas (ao longo do tempo)"
  )

  # run all methods with binary classification
  meths <- sntutils::classify_facility_activity(
    data = data,
    method = "all",
    hf_col = hf_col,
    date_col = date_col,
    key_indicators = key_indicators,
    binary_classification = TRUE,
    nonreport_window = nonreport_window
  ) |>
    dplyr::select(
      dplyr::all_of(c(hf_col, date_col)),
      dplyr::starts_with("activity_status_method")
    ) |>
    tidyr::pivot_longer(
      cols = dplyr::starts_with("activity_status_method"),
      names_to = "method",
      values_to = "activity_status"
    ) |>
    dplyr::mutate(
      method = dplyr::case_match(
        method,
        "activity_status_method1" ~ paste(word_method[[language]], "1"),
        "activity_status_method2" ~ paste(word_method[[language]], "2"),
        "activity_status_method3" ~ paste(word_method[[language]], "3")
      ),
      # ensure legend order is 1,2,3
      method = factor(
        method,
        levels = c(
          paste(word_method[[language]], "1"),
          paste(word_method[[language]], "2"),
          paste(word_method[[language]], "3")
        )
      )
    ) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(date_col, "method")))) |>
    dplyr::summarise(
      n_active = sum(activity_status == "Active", na.rm = TRUE),
      n_total = dplyr::n_distinct(!!rlang::sym(hf_col)),
      reprate = n_active / n_total,
      .groups = "drop"
    )

  # pivot wide for scatterplots
  meths_wide <- meths |>
    dplyr::select(dplyr::all_of(c(date_col)), method, reprate) |>
    tidyr::pivot_wider(names_from = method, values_from = reprate)

  # helper to build one scatterplot (no coord_equal; match limits instead)
  make_plot <- function(xcol, ycol, title_text, color_choice) {
    ggplot2::ggplot(
      meths_wide,
      ggplot2::aes(x = .data[[xcol]], y = .data[[ycol]])
    ) +
      ggplot2::geom_point(alpha = 0.8, size = 2.2, color = color_choice) +
      ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
      ggplot2::scale_x_continuous(
        # limits = c(0, 1.0),
        expand = ggplot2::expansion(0)
      ) +
      ggplot2::scale_y_continuous(
        # limits = c(0, 1.0),
        expand = ggplot2::expansion(0)
      ) +
      ggplot2::theme_minimal() +
      ggplot2::labs(
        title = title_text,
        x = paste0("\n", xcol, xlabs[[language]]),
        y = paste0(ycol, ylabs[[language]], "\n")
      ) +
      ggplot2::theme(
        plot.margin = grid::unit(c(0.4, 0.4, 0.4, 0.4), "cm"),
        panel.border = ggplot2::element_rect(
          color = "black",
          fill = NA,
          linewidth = 1
        ),
        panel.grid.minor = ggplot2::element_blank(),
        plot.title = ggplot2::element_text(size = 11, face = "bold")
      )
  }

# ---- scatterplots (suppress harmless warnings) ----
p1 <- suppressWarnings(make_plot(
  paste(word_method[[language]], "1"),
  paste(word_method[[language]], "2"),
  paste(word_method[[language]], "1 vs", word_method[[language]], "2"),
  "#1b9e77"
))

p2 <- suppressWarnings(make_plot(
  paste(word_method[[language]], "2"),
  paste(word_method[[language]], "3"),
  paste(word_method[[language]], "2 vs", word_method[[language]], "3"),
  "#d95f02"
))

p3 <- suppressWarnings(make_plot(
  paste(word_method[[language]], "1"),
  paste(word_method[[language]], "3"),
  paste(word_method[[language]], "1 vs", word_method[[language]], "3"),
  "#7570b3"
))

  # time-series plot of active facilities
  time_plot <- ggplot2::ggplot(
    meths,
    ggplot2::aes(
      x = .data[[date_col]],
      y = n_active,
      color = method,
      linetype = method
    )
  ) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_point(size = 1.6) +
    ggplot2::scale_y_continuous(labels = scales::comma_format()) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = switch(
        language,
        en = "Number of Active Facilities Over Time by Method",
        fr = "Nombre de FOSA actives au fil du temps selon la m\u00e9thode",
        pt = "N\u00famero de unidades de sa\u00fade ativas ao longo do tempo por m\u00e9todo"
      ),
      x = x_time_axis,
      y = y_active_facilities,
      color = word_method[[language]],
      linetype = word_method[[language]]
    ) +
    ggplot2::theme(
      plot.margin = grid::unit(c(0.4, 0.2, 0.2, 0.2), "cm"),
      panel.border = ggplot2::element_rect(
        color = "black",
        fill = NA,
        linewidth = 1
      ),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "right"
    )

  # patchwork layout with balanced widths (side panels wider than before)
  top <- patchwork::wrap_plots(p1, p2, p3, nrow = 1) +
    patchwork::plot_layout(widths = c(1.2, 1.8, 1.2))

  final_plot <- top /
    time_plot +
    patchwork::plot_layout(heights = c(1, 1.1)) +
    patchwork::plot_annotation(title = titles[[language]])

  # save if requested
  if (!is.null(plot_path)) {
    plot_path <- ifelse(
      grepl("/$", plot_path),
      plot_path,
      paste0(plot_path, "/")
    )
    filename <- switch(
      language,
      en = "comparison_health_facility_reporting_rate.png",
      fr = "comparaison_taux_rapportage_fosa.png",
      pt = "comparacao_taxa_reporte_unidades_saude.png"
    )
    ggplot2::ggsave(
      filename = paste0(plot_path, filename),
      plot = final_plot,
      width = width,
      height = height,
      units = units,
      dpi = dpi,
      scale = scale
    )
    if (isTRUE(compress_image)) {
      sntutils::compress_png(path = paste0(plot_path, filename))
    }
  }

  final_plot
}
