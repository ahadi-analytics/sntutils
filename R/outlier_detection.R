#' Detect outliers in a dataset using multiple statistical methods
#'
#' @description
#' This function identifies outliers in a specified numeric column using three
#' different statistical approaches: parametric (mean ± 3SD), Hampel identifier
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
#'    - outliers_moyenne: Identifies outliers using mean ± 3 standard deviations
#'    - outliers_halper: Identifies outliers using Hampel identifier
#'      (median ± 15*MAD)
#'    - outliers_iqr: Identifies outliers using Tukey's fences
#'      (Q1/Q3 ± custom*IQR)
#'    - moyenne_lower_bound: Lower bound for mean-based detection
#'    - moyenne_upper_bound: Upper bound for mean-based detection
#'    - hampel_lower_bound: Lower bound for Hampel identifier
#'    - hampel_upper_bound: Upper bound for Hampel identifier
#'    - iqr_lower_bound: Lower bound for IQR-based detection
#'    - iqr_upper_bound: Upper bound for IQR-based detection
#'    - iqr_value: The calculated IQR value
#'
#' @details
#' The function groups data by administrative units (adm1, adm2), health
#' facility (hf), and year before calculating statistics. Each method has
#' different sensitivity to outliers, with the Hampel identifier being more
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
    record_id   = "record_id",
    adm1        = "adm1",
    adm2        = "adm2",
    yearmon     = "yearmon",
    year        = "year"
) {
  required_cols <- c(column, record_id, adm1, adm2, yearmon, year)
  if (!all(required_cols %in% names(data))) {
    missing_cols <- required_cols[!required_cols %in% names(data)]
    cli::cli_abort("Missing columns: {.val {missing_cols}}")
  }
  if (!is.numeric(data[[column]])) {
    return(NULL)
  }

  dt <- data.table::as.data.table(data)
  data.table::setnames(
    dt,
    old = c(record_id, adm1, adm2, yearmon, year),
    new = c(
      "func_id", "func_region", "func_subregion",
      "func_date", "func_year"
    )
  )

  dt <- dt[!is.na(get(column))]
  dt_stats <- dt[, .(
    moyenne         = ceiling(mean(get(column), na.rm = TRUE)),
    sd               = ceiling(stats::sd(get(column), na.rm = TRUE)),
    median           = ceiling(stats::median(get(column), na.rm = TRUE)),
    median_absolute = ceiling(
      stats::mad(get(column), constant = 1, na.rm = TRUE)
    ),
    q1               = as.numeric(
      stats::quantile(get(column), 0.25, na.rm = TRUE)
    ),
    q3               = as.numeric(
      stats::quantile(get(column), 0.75, na.rm = TRUE)
    )
  ), by = c(
    "func_region",
    "func_subregion",
    "func_year"
  )]

  data.table::set(
    dt_stats, j = "iqr",
    value = dt_stats$q3 - dt_stats$q1
  )
  data.table::set(
    dt_stats, j = "moyenne_lower_bound",
    value = dt_stats$moyenne - 3 * dt_stats$sd
  )
  data.table::set(
    dt_stats, j = "moyenne_upper_bound",
    value = dt_stats$moyenne + 3 * dt_stats$sd
  )
  data.table::set(
    dt_stats, j = "hampel_lower_bound",
    value = dt_stats$median - 15 * dt_stats$median_absolute
  )
  data.table::set(
    dt_stats, j = "hampel_upper_bound",
    value = dt_stats$median + 15 * dt_stats$median_absolute
  )
  data.table::set(
    dt_stats, j = "iqr_lower_bound",
    value = dt_stats$q1 - iqr_multiplier * dt_stats$iqr
  )
  data.table::set(
    dt_stats, j = "iqr_upper_bound",
    value = dt_stats$q3 + iqr_multiplier * dt_stats$iqr
  )

  result <- data.table::merge.data.table(
    dt,
    dt_stats,
    by = c("func_region", "func_subregion", "func_year"),
    all.x = TRUE
  )

  col_val <- result[[column]]
  data.table::set(
    result, j = "outliers_moyenne",
    value = ifelse(
      col_val < result$moyenne_lower_bound |
        col_val > result$moyenne_upper_bound,
      "outlier",
      "normal value"
    )
  )
  data.table::set(
    result, j = "outliers_halper",
    value = ifelse(
      col_val < result$hampel_lower_bound |
        col_val > result$hampel_upper_bound,
      "outlier",
      "normal value"
    )
  )
  data.table::set(
    result, j = "outliers_iqr",
    value = ifelse(
      col_val < result$iqr_lower_bound |
        col_val > result$iqr_upper_bound,
      "outlier",
      "normal value"
    )
  )
  data.table::set(result, j = "column_name", value = column)
  data.table::set(result, j = "value", value = col_val)

  data.table::setnames(
    result,
    old = c(
      "func_id", "func_region", "func_subregion",
      "func_date", "func_year"
    ),
    new = c(record_id, adm1, adm2, yearmon, year)
  )

  result <- result[, c(
    record_id, adm1, adm2, "hf",
    yearmon, year, "column_name", "value",
    "outliers_moyenne", "outliers_halper",
    "outliers_iqr", "moyenne_lower_bound",
    "moyenne_upper_bound", "hampel_lower_bound",
    "hampel_upper_bound", "iqr_lower_bound",
    "iqr_upper_bound", "iqr"
  ), with = FALSE]

  dplyr::as_tibble(result)
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
#' @param yearmon Name of year-month date column (default: "yearmon_date")
#' @param year Name of year column (default: "year")
#' @param methods Vector of outlier detection methods to use:
#'   "iqr" (Interquartile Range),
#'   "halper" (Halper method),
#'   "moyenne" (Moyenne method)
#'   (default: c("iqr", "halper", "moyenne"))
#' @param iqr_multiplier Multiplier for IQR method (default: 1.5)
#'
#' @return A list of ggplot objects, one for each outlier detection method
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
#'   methods = c("iqr", "halper")
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
    methods = c("iqr", "halper", "moyenne"),
    iqr_multiplier = 1.5
) {
  # Create outlier columns for each method
  outlier_cols <- paste0("outliers_", methods)

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
          dplyr::starts_with("outliers_")
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
        n_outlier = sntutils::big_mark(n_outlier),
        n_total_fmt = sntutils::big_mark(n_total),
        pct_outlier = round(
          100 * as.numeric(n_outlier) / as.numeric(n_total),
          1
        ),
        adm2_value = .data[[adm2]],
        label = glue::glue(
          "{adm2_value}\nOutliers: {pct_outlier}% ({n_outlier}/{n_total_fmt})"
        )
      )

    facet_labels <- setNames(percent_summary$label, percent_summary[[adm2]])

    outliers_n <- sntutils::big_mark(
      sum(data_out[[outlier_col]] == "outlier", na.rm = TRUE)
    )
    total_outliers <- sntutils::big_mark(nrow(data_out))

    method_name <- gsub("outliers_", "", outlier_col)
    method_name2 <- switch(
      method_name,
      "iqr" = glue::glue("IQR (multiplier = {iqr_multiplier})"),
      "halper" = "Halper",
      "moyenne" = "Moyenne"
    )
    cli::cli_inform(
      glue::glue(
        "Method: {method_name2}\n",
        "There were {outliers_n}/",
        "{total_outliers} outliers detected for the `{column}` variable"
      )
    )

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
        title = glue::glue("{method_name2} Outlier Detection for `{column}`"),
        subtitle = glue::glue(
          "There were {outliers_n}/{total_outliers}",
          "<b style='color:red;font-weight:bold'> outliers</b> detected"
        ),
        x = "\nYear-Month",
        y = "Value\n",
        color = "Outlier Classification"
      ) +
      ggplot2::facet_wrap(
        stats::as.formula(paste("~", adm2)),
        scales = "free_y",
        labeller = ggplot2::labeller(!!adm2 := facet_labels)
      ) +
      ggplot2::scale_x_discrete(
        breaks = function(x) x[seq(1, length(x), by = 2)]
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
  }

  return(res)
}
