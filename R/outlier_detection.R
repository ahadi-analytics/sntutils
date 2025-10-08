#' Detect outliers in a dataset using multiple statistical methods
#'
#' @description
#' This function identifies outliers in a specified numeric column using three
#' different statistical approaches: parametric (mean ± 3SD), Median identifier
#' (median ± 15 * MAD), and Tukey's fences (quartiles ± custom IQR).
#'
#' @param data A data frame containing the data to analyze
#' @param column Character string specifying the column name to check for
#'    outliers
#' @param iqr_multiplier Numeric value specifying the IQR multiplier for Tukey's
#'    method (default = 1.5)
#' @param record_id Character string specifying the column name for record ID
#'    (default = "record_id")
#' @param adm1 Character string specifying the column name for administrative
#'    level 1 (default = "adm1")
#' @param adm2 Character string specifying the column name for administrative
#'    level 2 (default = "adm2")
#' @param yearmon Character string specifying the column name for year-month
#'    (default = "yearmon")
#' @param year Character string specifying the column name for year
#'    (default = "year")
#'
#' @return A data frame with the record_id, specified column, and outlier
#'    classification information containing:
#'    - record_id: Identifier for each record
#'    - column_name: The name of the analyzed column
#'    - value: The value in the specified column
#'    - outlier_flag_mean: Identifies outliers using mean ± 3 standard deviations
#'    - outlier_flag_median: Identifies outliers using Median identifier
#'      (median ± 15*MAD)
#'    - outlier_flag_iqr: Identifies outliers using Tukey's fences
#'      (Q1/Q3 ± custom*IQR)
#'    - mean_lower_bound: Lower bound for mean-based detection
#'    - mean_upper_bound: Upper bound for mean-based detection
#'    - median_lower_bound: Lower bound for Median identifier
#'    - median_upper_bound: Upper bound for Median identifier
#'    - iqr_lower_bound: Lower bound for IQR-based detection
#'    - iqr_upper_bound: Upper bound for IQR-based detection
#'    - iqr_value: The calculated IQR value
#'
#' @details
#' The function groups data by administrative units (adm1, adm2), health
#' facility (hf), and year before calculating statistics. Each method has
#' different sensitivity to outliers, with the Median identifier being more
#' robust against extreme values.
#'
#' **About `iqr_multiplier`:**
#' The `iqr_multiplier` controls the sensitivity of Tukey's fences (IQR method).
#' - The standard value is **1.5** (used in boxplots) for "mild" outliers.
#' - A value of **3** is sometimes used for "extreme" outliers.
#' - Higher values make the method less sensitive (fewer outliers detected).
#' - Lower values make it more sensitive (more outliers detected).
#'
#' Returns NULL if the specified column doesn't exist or isn't numeric.
#' @examples
#' \dontrun{
#' # Detect outliers in the "confirmed_cases" column with default IQR multiplier
#' outlier_results <- detect_outliers(malaria_data, "confirmed_cases")
#'
#' # Use custom IQR multiplier
#' outlier_results <- detect_outliers(malaria_data, "confirmed_cases",
#'                                   iqr_multiplier = 2)
#' }
#' @export
detect_outliers <- function(
  data,
  column,
  iqr_multiplier = 1.5,
  record_id = "record_id",
  adm1 = "adm1",
  adm2 = "adm2",
  yearmon = "yearmon",
  year = "year"
) {
  required_cols <- c(column, record_id, adm1, adm2, yearmon, year)
  if (!all(required_cols %in% names(data))) {
    missing_cols <- required_cols[!required_cols %in% names(data)]
    cli::cli_abort("Missing columns: {.val {missing_cols}}")
  }

  if (!is.numeric(data[[column]])) {
    return(NULL)
  }

  data |>
    # Convert to tibble and filter NA values
    dplyr::as_tibble() |>
    dplyr::filter(!is.na(.data[[column]])) |>
    # Group by administrative regions and year
    dplyr::group_by(.data[[adm1]], .data[[adm2]], .data[[year]]) |>
    # Calculate statistics
    dplyr::mutate(
      mean = ceiling(mean(.data[[column]], na.rm = TRUE)),
      sd = ceiling(stats::sd(.data[[column]], na.rm = TRUE)),
      median = ceiling(stats::median(.data[[column]], na.rm = TRUE)),
      median_absolute = ceiling(stats::mad(
        .data[[column]],
        constant = 1,
        na.rm = TRUE
      )),
      q1 = as.numeric(stats::quantile(.data[[column]], 0.25, na.rm = TRUE)),
      q3 = as.numeric(stats::quantile(.data[[column]], 0.75, na.rm = TRUE)),
      iqr = base::round(q3 - q1),
      # Calculate bounds (floor lower bounds at 0 for count data)
      mean_lower_bound = base::pmax(0, mean - 3 * sd),
      mean_upper_bound = mean + 3 * sd,
      median_lower_bound = base::pmax(0, median - 15 * median_absolute),
      median_upper_bound = median + 15 * median_absolute,
      iqr_lower_bound = base::round(base::pmax(0, q1 - iqr_multiplier * iqr)),
      iqr_upper_bound = base::round(q3 + iqr_multiplier * iqr),
      # Classify outliers
      outlier_flag_mean = dplyr::if_else(
        .data[[column]] < mean_lower_bound |
          .data[[column]] > mean_upper_bound,
        "outlier",
        "normal value"
      ),
      outlier_flag_median = dplyr::if_else(
        .data[[column]] < median_lower_bound |
          .data[[column]] > median_upper_bound,
        "outlier",
        "normal value"
      ),
      outlier_flag_iqr = dplyr::if_else(
        .data[[column]] < iqr_lower_bound |
          .data[[column]] > iqr_upper_bound,
        "outlier",
        "normal value"
      ),
      column_name = column,
      value = .data[[column]]
    ) |>
    dplyr::ungroup() |>
    # Select final columns in desired order
    dplyr::select(
      dplyr::all_of(c(
        record_id,
        adm1,
        adm2,
        yearmon,
        year,
        "column_name",
        "value",
        "outlier_flag_mean",
        "outlier_flag_median",
        "outlier_flag_iqr",
        "mean_lower_bound",
        "mean_upper_bound",
        "median_lower_bound",
        "median_upper_bound",
        "iqr_lower_bound",
        "iqr_upper_bound",
        "iqr"
      ))
    )
}

#' Create Outlier Detection Plots
#'
#' This function creates plots to visualize outliers in data using multiple
#' detection methods.
#'
#' @param data A data frame containing the data to analyze
#' @param column Name of the column to check for outliers
#' @param adm0 Optional administrative level 0 filter (default: NULL)
#' @param adm1 Name of administrative level 1 column (default: "adm1")
#' @param adm2 Name of administrative level 2 column (default: "adm2")
#' @param record_id Name of unique record ID column (default: "record_id")
#' @param yearmon Name of year-month date column (default: "yearmon")
#' @param year Name of year column (default: "year")
#' @param methods Vector of outlier detection methods to use:
#'   "iqr" (Interquartile Range),
#'   "median" (Median method),
#'   "mean" (Mean method)
#'   (default: c("iqr", "median", "mean"))
#' @param iqr_multiplier Multiplier for IQR method (default: 1.5)
#' @param year_breaks Numeric value specifying the interval for x-axis breaks
#'   (default: 2). For example, 2 shows every 2nd year/month, 3 shows every 3rd.
#' @param compare_periods Logical. If TRUE, creates comparison plots between
#'   different time periods (default: FALSE)
#' @param target_language Character string specifying the language for plot
#'   labels. Defaults to "en" (English). Use ISO 639-1 language codes.
#' @param source_language Source language code. If NULL, auto-detection is used.
#'   Defaults to NULL.
#' @param lang_cache_path Path to directory for storing translation cache.
#'   Defaults to tempdir().
#' @param plot_path Character string specifying the path where plots should
#'   be saved. If NULL (default), plots are not saved.
#' @param compress_image Logical. If TRUE, will compress the saved plot.
#'   Defaults to FALSE
#' @param image_overwrite Logical. If TRUE, will overwrite existing files.
#'   Defaults to TRUE.
#' @param compression_speed Integer. Speed/quality trade-off from 1
#'   (brute-force) to 10 (fastest). Default is 1.
#' @param compression_verbose Logical. Controls output verbosity.
#'   FALSE = silent, TRUE = verbose. Defaults to TRUE.
#' @param plot_scale Numeric. Scaling factor for saved plots. Values > 1 
#'   increase size, < 1 decrease size. Default is 1.
#' @param plot_width Numeric. Width of saved plot in inches. If NULL (default),
#'   width is calculated based on number of facets.
#' @param plot_height Numeric. Height of saved plot in inches. If NULL (default),
#'   height is calculated based on number of facets.
#' @param plot_dpi Numeric. Resolution of saved plot in dots per inch. 
#'   Default is 300.
#' @param show_plot Logical. If FALSE, the plot is returned invisibly (not displayed).
#'   Useful when only saving plots. Default is TRUE.
#'
#' @return If a single method is specified, returns a ggplot object. If multiple
#'   methods are specified, returns a list of ggplot objects named by method.
#'   When show_plot is FALSE, returns invisibly.
#'
#' @details
#' The function creates scatter plots showing outliers detected by different
#' methods. Each plot shows data points colored by outlier status, faceted by
#' administrative level 2 regions. Summary statistics of outliers are shown in
#' facet labels.
#'
#' @examples
#' \dontrun{
#' plots <- outlier_plot(
#'   data = my_data,
#'   column = "price",
#'   record_id = "id",
#'   methods = c("iqr", "median")
#' )
#' }
#' @export
outlier_plot <- function(
    data,
    column,
    adm0 = NULL,
    adm1 = "adm1",
    adm2 = "adm2",
    record_id = "record_id",
    yearmon = "yearmon",
    year = "year",
    methods = c("iqr", "median", "mean"),
    iqr_multiplier = 1.5,
    year_breaks = 2,
    compare_periods = FALSE,
    target_language = "en",
    source_language = "en",
    lang_cache_path = tempdir(),
    plot_path = NULL,
    compress_image = FALSE,
    image_overwrite = TRUE,
    compression_speed = 1,
    compression_verbose = TRUE,
    plot_scale = 1,
    plot_width = NULL,
    plot_height = NULL,
    plot_dpi = 300,
    show_plot = TRUE
) {
  # Create outlier columns for each method
  outlier_cols <- paste0("outlier_flag_", methods)

  outlier_df <- detect_outliers(
    data,
    column,
    record_id,
    adm1,
    adm2,
    yearmon,
    year,
    iqr_multiplier = iqr_multiplier
  )

  data_out <- data |>
    dplyr::filter(!is.na(.data[[column]]), .data[[column]] > 0) |>
    dplyr::left_join(
      outlier_df |>
        dplyr::select(
          dplyr::all_of(record_id),
          dplyr::starts_with("outlier_flag_")
        ),
      by = record_id
    )

  # Create plot for each outlier method
  res <- list()

  for (outlier_col in outlier_cols) {
    percent_summary <- data_out |>
      dplyr::group_by(dplyr::across(dplyr::all_of(adm2))) |>
      dplyr::summarise(
        n_outlier = sum(get(outlier_col) == "outlier", na.rm = TRUE),
        n_total = dplyr::n(),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        pct_outlier = round(100 * n_outlier / n_total, 1),
        n_outlier_fmt = sntutils::big_mark(n_outlier),
        n_total_fmt = sntutils::big_mark(n_total),
        adm2_value = .data[[adm2]],
        label = glue::glue(
          "{adm2_value}\nOutliers: {pct_outlier}% ({n_outlier_fmt}/{n_total_fmt})"
        )
      )

    facet_labels <- setNames(percent_summary$label, percent_summary[[adm2]])

    outliers_n <- sntutils::big_mark(
      sum(data_out[[outlier_col]] == "outlier", na.rm = TRUE)
    )
    total_outliers <- sntutils::big_mark(nrow(data_out))

    method_name <- gsub("outlier_flag_", "", outlier_col)
    method_name2 <- switch(
      method_name,
      "iqr" = glue::glue("IQR (multiplier = {iqr_multiplier})"),
      "median" = "Median",
      "mean" = "Mean"
    )
    cli::cli_inform(
      glue::glue(
        "Method: {method_name2}\n",
        "There were {outliers_n}/",
        "{total_outliers} outliers detected for the {column} variable"
      )
    )

    # Prepare labels for translation
    # Keep column name as-is, translate other parts
    if (target_language != "en") {
      # Translate individual components
      outlier_detection_text <- translate_text(
        "Outlier Detection for",
        target_language = target_language,
        source_language = source_language,
        cache_path = lang_cache_path
      )
      title_text <- glue::glue(
        "{method_name2} {outlier_detection_text} <b>{column}</b>"
      )
      
      # Translate only the word "outliers" for subtitle
      outliers_word <- translate_text(
        "outliers",
        target_language = target_language,
        source_language = source_language,
        cache_path = lang_cache_path
      )
      detected_word <- translate_text(
        "detected",
        target_language = target_language,
        source_language = source_language,
        cache_path = lang_cache_path
      )
      there_were <- translate_text(
        "There were",
        target_language = target_language,
        source_language = source_language,
        cache_path = lang_cache_path
      )
      subtitle_text <- glue::glue(
        "{there_were} {outliers_n}/{total_outliers}",
        " <b style='color:red;font-weight:bold'>{outliers_word}</b> {detected_word}"
      )
      
      x_label <- if (yearmon == "yearmon") {
        translate_text(
          "Year-Month",
          target_language = target_language,
          source_language = source_language,
          cache_path = lang_cache_path
        )
      } else {
        yearmon
      }
      y_label <- translate_text(
        "Value",
        target_language = target_language,
        source_language = source_language,
        cache_path = lang_cache_path
      )
      color_label <- translate_text(
        "Outlier Classification",
        target_language = target_language,
        source_language = source_language,
        cache_path = lang_cache_path
      )
    } else {
      # English version
      title_text <- glue::glue(
        "{method_name2} Outlier Detection for <b>{column}</b>"
      )
      subtitle_text <- glue::glue(
        "There were {outliers_n}/{total_outliers}",
        " <b style='color:red;font-weight:bold'>outliers</b> detected"
      )
      x_label <- if (yearmon == "yearmon") "Year-Month" else yearmon
      y_label <- "Value"
      color_label <- "Outlier Classification"
    }

    p <- ggplot2::ggplot(data_out) +
      ggplot2::geom_point(
        ggplot2::aes(
          x = .data[[yearmon]],
          y = .data[[column]],
          color = .data[[outlier_col]]
        )
      ) +
      ggplot2::scale_color_manual(
        values = c("normal value" = "grey", "outlier" = "red")
      ) +
      ggplot2::labs(
        title = title_text,
        subtitle = subtitle_text,
        x = paste0("\n", x_label),
        y = paste0(y_label, "\n"),
        color = color_label
      ) +
      ggplot2::facet_wrap(
        stats::as.formula(paste("~", adm2)),
        scales = "free_y",
        labeller = ggplot2::labeller(!!adm2 := facet_labels)
      ) +
      ggplot2::scale_x_discrete(
        breaks = function(x) x[seq(1, length(x), by = year_breaks)]
      ) +
      ggplot2::scale_y_continuous(
        labels = scales::label_number(
          scale_cut = scales::cut_short_scale()
        )
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        legend.position = "bottom",
        legend.key.width = ggplot2::unit(2, "cm"),
        axis.text.x = ggplot2::element_text(
          size = 8,
          angle = 45,
          hjust = 1
        ),
        legend.text = ggplot2::element_text(size = 8),
        legend.title = ggplot2::element_text(size = 9, face = "bold"),
        plot.title = ggtext::element_markdown(),
        plot.subtitle = ggtext::element_markdown()
      ) +
      ggplot2::guides(
        color = "none"
        # color = ggplot2::guide_legend(
        # title.position = "top",
        #  title.hjust = 0.5,
        #  label.position = "bottom",
        # nrow = 1
        #)
      )

    res[[method_name]] <- p

    # Save plot if path is provided
    if (!is.null(plot_path)) {
      .save_outlier_plot(
        plot = p,
        plot_path = plot_path,
        method_name = method_name,
        column = column,
        yearmon = yearmon,
        adm_level = adm2,
        data = data,
        target_language = target_language,
        source_language = source_language,
        lang_cache_path = lang_cache_path,
        compress_image = compress_image,
        image_overwrite = image_overwrite,
        compression_speed = compression_speed,
        compression_verbose = compression_verbose,
        plot_scale = plot_scale,
        plot_width = plot_width,
        plot_height = plot_height,
        plot_dpi = plot_dpi
      )
    }
  }

  # Return single plot if only one method, otherwise return list
  result <- if (length(methods) == 1) {
    res[[1]]
  } else {
    res
  }
  
  # Return invisibly if show_plot is FALSE
  if (show_plot) {
    return(result)
  } else {
    return(invisible(result))
  }
}

#' Save outlier plot with appropriate naming
#'
#' @noRd
.save_outlier_plot <- function(plot, plot_path, method_name, column, yearmon,
                              adm_level, data, target_language = "en",
                              source_language = "en",
                              lang_cache_path = tempdir(),
                              compress_image = FALSE,
                              image_overwrite = TRUE,
                              compression_speed = 1,
                              compression_verbose = TRUE,
                              plot_scale = 1,
                              plot_width = NULL,
                              plot_height = NULL,
                              plot_dpi = 300) {
  # Helper function to handle translation without messages
  .translate_quiet <- function(text) {
    if (target_language != source_language) {
      suppressMessages(
        translate_text(
          text,
          target_language = target_language,
          source_language = source_language,
          cache_path = lang_cache_path
        )
      )
    } else {
      text
    }
  }
  
  # Create directory if it doesn't exist
  if (!dir.exists(plot_path)) {
    dir_created <- dir.create(plot_path, recursive = TRUE, showWarnings = FALSE)
    if (!dir_created) {
      cli::cli_warn("Could not create directory: {plot_path}")
      return(invisible(NULL))
    }
  }

  # Translate terms for filename
  # Translate "outlier plot" as two separate words for better translation
  outlier_tr <- .translate_quiet("outlier") |>
    tolower()
  plot_tr <- .translate_quiet("plot") |>
    tolower()
  outlier_plot_tr <- paste(outlier_tr, plot_tr, sep = "_")

  method_tr <- .translate_quiet(method_name) |>
    tolower()

  for_tr <- .translate_quiet("for")

  by_tr <- .translate_quiet("by")

  # Translate yearmon label
  yearmon_label <- if (yearmon == "yearmon") {
    .translate_quiet("year_month") |>
      tolower() |>
      gsub(" ", "_", x = _)
  } else {
    yearmon
  }

  adm_level_tr <- .translate_quiet(adm_level) |>
    tolower() |>
    gsub(" ", "_", x = _)

  # Get year range
  year_range <- if (!is.null(data$year) && length(unique(data$year)) > 1) {
    glue::glue("{min(data$year, na.rm = TRUE)}-{max(data$year, na.rm = TRUE)}")
  } else if (!is.null(data$year)) {
    as.character(min(data$year, na.rm = TRUE))
  } else {
    format(Sys.Date(), "%Y")
  }

  # Construct filename
  save_path <- glue::glue(
    "{outlier_plot_tr}_{method_tr}_{for_tr}_{column}_{by_tr}_",
    "{yearmon_label}_&_{adm_level_tr}_{year_range}_",
    "v{format(Sys.Date(), '%Y-%m-%d')}.png"
  )

  full_path <- file.path(plot_path, save_path)

  # Calculate dimensions based on facets or use provided values
  n_facets <- length(unique(data[[adm_level]]))
  if (is.null(plot_width)) {
    width <- max(10, min(20, n_facets * 3))
  } else {
    width <- plot_width
  }
  
  if (is.null(plot_height)) {
    height <- max(8, min(15, ceiling(n_facets / 3) * 4))
  } else {
    height <- plot_height
  }

  # Try to save the plot
  tryCatch({
    ggplot2::ggsave(
      filename = full_path,
      plot = plot,
      width = width,
      height = height,
      dpi = plot_dpi,
      scale = plot_scale
    )

    # Close device to prevent warnings
    if (grDevices::dev.cur() > 1) {
      grDevices::dev.off()
    }

    # Compress if requested
    if (compress_image && file.exists(full_path)) {
      compress_png(
        full_path,
        verbosity = compression_verbose,
        speed = compression_speed,
        png_overwrite = image_overwrite
      )
    }

    success_msg <- .translate_quiet("Plot saved to:")
    # Show only relative path from current directory if it's a subdirectory
    display_path <- full_path
    if (startsWith(full_path, getwd())) {
      display_path <- sub(paste0("^", getwd(), "/"), "", full_path)
    } else if (grepl("03_outputs", full_path)) {
      # Extract from 03_outputs onward if present
      display_path <- sub(".*/(03_outputs/.*)", "\\1", full_path)
    }
    cli::cli_alert_success(paste(success_msg, display_path))
  }, error = function(e) {
    # Close device on error
    if (grDevices::dev.cur() > 1) {
      grDevices::dev.off()
    }
    cli::cli_warn("Failed to save plot to {full_path}: {e$message}")
  })

  invisible(full_path)
}
