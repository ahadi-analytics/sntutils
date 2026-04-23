#' Fit regression model for insecticide resistance trends
#'
#' @description
#' Fits a regression model predicting mortality (y) from year (x) for a
#' single administrative unit or insecticide slice.
#'
#' Supports multiple model families including GLM (gaussian, binomial,
#' quasibinomial), linear regression, and generalized additive models (GAM).
#'
#' @param df Data frame containing columns `x` (year) and `y` (mortality
#'   proportion).
#' @param model_type Character scalar. One of `"gaussian"`, `"binomial"`,
#'   `"quasibinomial"`, `"lm"`, or `"gam"`. Default is `"quasibinomial"`.
#'
#' @return
#' A fitted model object of class `glm`, `lm`, or `gam` depending on
#' `model_type`.
#'
#' @examples
#' \dontrun{
#' trend_data <- data.frame(
#'   x = 2010:2025,
#'   y = c(0.95, 0.93, 0.91, 0.89, 0.87, 0.85, 0.83, 0.81,
#'         0.79, 0.77, 0.75, 0.73, 0.71, 0.69, 0.67, 0.65)
#' )
#'
#' model <- get_model(trend_data, model_type = "quasibinomial")
#' summary(model)
#' }
#'
#' @export
get_model <- function(df, model_type = "quasibinomial") {

  if (!base::is.data.frame(df)) {
    cli::cli_abort("`df` must be a data.frame.")
  }

  required_cols <- base::c("x", "y")
  missing_cols <- base::setdiff(required_cols, base::names(df))
  if (base::length(missing_cols) > 0L) {
    cli::cli_abort("missing required columns in df: {missing_cols}")
  }

  if (model_type == "gaussian") {
    return(stats::glm(y ~ x, data = df, family = stats::gaussian))
  }

  if (model_type == "binomial") {
    return(stats::glm(y ~ x, data = df, family = stats::binomial))
  }

  if (model_type == "quasibinomial") {
    return(stats::glm(y ~ x, data = df, family = stats::quasibinomial))
  }

  if (model_type == "lm") {
    return(stats::lm(y ~ x, data = df))
  }

  if (model_type == "gam") {
    if (!requireNamespace("mgcv", quietly = TRUE)) {
      cli::cli_abort(c(
        "Package {.pkg mgcv} is required for GAM models.",
        "i" = "Install with: {.code install.packages('mgcv')}"
      ))
    }
    return(mgcv::gam(y ~ s(x), data = df, family = stats::quasibinomial))
  }

  cli::cli_abort("Unknown model_type: {model_type}")
}


#' Generate insecticide resistance trend scenarios
#'
#' @description
#' Extends mortality time series beyond a specified year using one of three
#' projection methods: constant trend (flat-line), continued trend (model
#' extrapolation), or hybrid trend (average of both).
#'
#' Supports flexible grouping and can handle any IR metric column name.
#'
#' @param df_IR Data frame containing insecticide resistance data with
#'   grouping columns, year column, and value column.
#' @param year_cut Numeric. Last year of observed data. Projections start
#'   from `year_cut + 1`. Default is `2025`.
#' @param n_years_to_add Numeric. Number of years to project forward.
#'   Default is `5`.
#' @param continue_trend_start_year Numeric. First year to include in model
#'   fitting for continued trend. Default is `2010`.
#' @param model_type Character scalar. Regression family for continued trend.
#'   One of `"gaussian"`, `"binomial"`, `"quasibinomial"`, `"lm"`, `"gam"`,
#'   or `"unchanged"`. If `"unchanged"`, uses existing data values directly
#'   without refitting. Default is `"unchanged"`.
#' @param trendmethod Character scalar. Projection method: `"constantTrend"`
#'   (flat-line from last observed value), `"continuedTrend"` (model
#'   extrapolation), or `"hybridTrend"` (average of constant and continued).
#'   Default is `"constantTrend"`.
#' @param group_cols Character vector. Names of grouping columns (e.g.,
#'   administrative units, insecticides). Default is `c("adm0", "adm1")`.
#' @param value_col Character scalar. Name of the mortality/resistance value
#'   column. Default is `"mean_ir"`.
#'
#' @return
#' A data frame with grouping columns, `year`, `trendmethod`, and the value
#' column specified in `value_col`.
#'
#' @examples
#' \dontrun{
#' ir_data <- data.frame(
#'   adm0 = "BDI",
#'   adm1 = rep(c("Province A", "Province B"), each = 16),
#'   year = rep(2010:2025, 2),
#'   mean_ir = runif(32, 0.6, 0.95)
#' )
#'
#' constant_proj <- run_resistance_trend(
#'   df_IR = ir_data,
#'   year_cut = 2025,
#'   n_years_to_add = 5,
#'   trendmethod = "constantTrend"
#' )
#'
#' hybrid_proj <- run_resistance_trend(
#'   df_IR = ir_data,
#'   year_cut = 2025,
#'   trendmethod = "hybridTrend"
#' )
#' }
#'
#' @export
run_resistance_trend <- function(
  df_IR,
  year_cut = 2025,
  n_years_to_add = 5,
  continue_trend_start_year = 2010,
  model_type = "unchanged",
  trendmethod = c("constantTrend", "continuedTrend", "hybridTrend"),
  group_cols = c("adm0", "adm1"),
  value_col = "mean_ir"
) {

  trendmethod <- base::match.arg(trendmethod)

  if (!base::is.data.frame(df_IR)) {
    cli::cli_abort("`df_IR` must be a data.frame.")
  }

  required_cols <- base::c(group_cols, "year", value_col)
  missing_cols <- base::setdiff(required_cols, base::names(df_IR))
  if (base::length(missing_cols) > 0L) {
    cli::cli_abort("missing required columns in df_IR: {missing_cols}")
  }

  df_IR$value <- df_IR[[value_col]]
  years_to_add <- base::seq(year_cut + 1, year_cut + n_years_to_add)

  df_IR_out <- df_IR

  if (trendmethod %in% base::c("constantTrend", "hybridTrend")) {

    df_IR_out_constant <- df_IR_out |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
      dplyr::filter(year <= year_cut) |>
      dplyr::group_modify(~dplyr::add_row(.x, year = years_to_add)) |>
      tidyr::fill(value, .direction = "down")

    df_IR_out_constant$trendmethod <- "constantTrend"

    if (trendmethod == "constantTrend") {
      result <- df_IR_out_constant |>
        dplyr::select(dplyr::all_of(group_cols), year, trendmethod, value)
      base::names(result)[base::names(result) == "value"] <- value_col
      return(result)
    }
  }

  if (trendmethod %in% base::c("continuedTrend", "hybridTrend")) {

    if (model_type == "unchanged") {
      df_IR_out_continued <- df_IR_out
      df_IR_out_continued$trendmethod <- "unchanged"

    } else {
      df_IR_out_continued <- df_IR_out |>
        dplyr::filter(year >= continue_trend_start_year) |>
        dplyr::rename(x = year, y = value) |>
        dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
        dplyr::group_modify(~{
          df <- .x
          model <- get_model(df, model_type)
          pred_years <- tibble::tibble(x = years_to_add)
          preds <- pred_years |>
            dplyr::mutate(
              y = base::pmax(
                stats::predict(
                  model,
                  newdata = pred_years,
                  type = "response"
                ),
                0
              )
            )
          df_out <- dplyr::bind_rows(df, preds)
          df_out
        }) |>
        dplyr::ungroup() |>
        dplyr::rename(year = x, value = y)

      df_IR_out_continued$trendmethod <- model_type
    }

    if (trendmethod == "continuedTrend") {
      result <- df_IR_out_continued |>
        dplyr::select(dplyr::all_of(group_cols), year, trendmethod, value)
      base::names(result)[base::names(result) == "value"] <- value_col
      return(result)
    }
  }

  if (trendmethod == "hybridTrend") {

    df_IR_out_hybrid <- df_IR_out_constant |>
      dplyr::bind_rows(df_IR_out_continued) |>
      dplyr::mutate(trendmethod = "hybridTrend") |>
      dplyr::group_by(
        dplyr::across(dplyr::all_of(group_cols)),
        year,
        trendmethod
      ) |>
      dplyr::summarise(value = base::mean(value, na.rm = TRUE),
                       .groups = "drop")

    result <- df_IR_out_hybrid |>
      dplyr::select(dplyr::all_of(group_cols), year, trendmethod, value)
    base::names(result)[base::names(result) == "value"] <- value_col
    return(result)
  }
}


#' Generate insecticide resistance trend plot
#'
#' @description
#' Creates a faceted time-series plot showing insecticide resistance or
#' mortality trends with multiple projection scenarios.
#'
#' Inspired by `facetted_map_gradient()` from sntutils. Supports flexible
#' value columns, faceting, and automatic PNG compression.
#'
#' @param data Data frame containing pre-filtered IR data with year, value
#'   column, facet column, and trendmethod column.
#' @param value_col Character scalar. Name of y-axis value column.
#'   Default is `"mean_ir"`.
#' @param facet_col Character scalar. Name of facetting column.
#'   Default is `"adm1"`.
#' @param ncol Numeric. Number of facet columns. Default is `4`.
#' @param title Character scalar. Plot title. If `NULL`, no title is shown.
#' @param subtitle Character scalar. Plot subtitle. If `NULL`, no subtitle
#'   is shown.
#' @param caption Character scalar. Plot caption. If `NULL`, no caption is
#'   shown.
#' @param y_label Character scalar. Y-axis label.
#'   Default is `"Mortalit\u00e9 (mean)\n"`.
#' @param legend_title Character scalar. Legend title for trend method.
#'   If `NULL`, a default title is generated.
#' @param continue_trend_start_year Numeric. First year to show in plot.
#'   Default is `2010`.
#' @param year_cut Numeric. Year to show vertical reference line indicating
#'   projection start. Default is `2025`.
#' @param width Numeric. Plot width in inches. Default is `16`.
#' @param height Numeric. Plot height in inches. Default is `9`.
#' @param output_file Optional character scalar. File path where the plot
#'   should be saved. If `NULL`, the plot is not saved to disk.
#' @param dpi Numeric. Resolution (dots per inch) for saved output.
#'   Default is `300`.
#' @param compress_image Logical. Compress PNG using `compress_png()` after
#'   saving. Default is `TRUE`.
#' @param image_overwrite Logical. Overwrite existing PNG during compression.
#'   Default is `TRUE`.
#' @param compression_speed Numeric. Compression speed from `1` (best quality,
#'   slowest) to `10` (fastest, lower quality). Default is `1`.
#' @param compression_verbose Logical. Print compression statistics.
#'   Default is `TRUE`.
#' @param show_plot Logical. Display the plot. If `FALSE`, returns plot object
#'   invisibly. Default is `TRUE`.
#'
#' @return
#' A `ggplot` object. If `output_file` is provided, the plot is also written
#' to disk using `ggsave()` and optionally compressed.
#'
#' @examples
#' \dontrun{
#' ir_trends <- data.frame(
#'   adm0 = "BDI",
#'   adm1 = rep(c("Province A", "Province B"), each = 48),
#'   year = rep(2010:2030, 4),
#'   trendmethod = rep(c("constantTrend", "hybridTrend"), each = 42),
#'   mean_ir = runif(96, 0.6, 0.95)
#' )
#'
#' plot_obj <- generate_ir_plot(
#'   data = ir_trends,
#'   value_col = "mean_ir",
#'   facet_col = "adm1",
#'   title = "Deltamethrin Mortality Trends",
#'   output_file = here::here("outputs", "ir_trends.png")
#' )
#' }
#'
#' @export
generate_ir_plot <- function(
  data,
  value_col = "mean_ir",
  facet_col = "adm1",
  ncol = 4,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  y_label = "Mortalit\u00e9 (mean)\n",
  legend_title = NULL,
  continue_trend_start_year = 2010,
  year_cut = 2025,
  width = 16,
  height = 9,
  output_file = NULL,
  dpi = 300,
  compress_image = TRUE,
  image_overwrite = TRUE,
  compression_speed = 1,
  compression_verbose = TRUE,
  show_plot = TRUE
) {

  if (!base::is.data.frame(data)) {
    cli::cli_abort("`data` must be a data.frame.")
  }

  required_cols <- base::c("year", value_col, facet_col, "trendmethod")
  missing_cols <- base::setdiff(required_cols, base::names(data))
  if (base::length(missing_cols) > 0L) {
    cli::cli_abort("missing required columns in data: {missing_cols}")
  }

  pdat <- data |>
    dplyr::filter(year >= continue_trend_start_year)

  if (base::is.null(legend_title)) {
    legend_title <- base::paste0(
      "Options de tendance (Ann\u00e9e de d\u00e9but: ",
      year_cut,
      ")"
    )
  }

  pp <- ggplot2::ggplot(data = pdat) +
    ggplot2::geom_line(
      ggplot2::aes(
        x = year,
        y = .data[[value_col]],
        col = .data[["trendmethod"]],
        group = interaction(.data[[facet_col]], .data[["trendmethod"]])
      ),
      linewidth = 1.1
    ) +
    ggplot2::geom_vline(
      xintercept = year_cut,
      linetype = "solid",
      colour = "black",
      linewidth = 0.5
    ) +
    ggplot2::facet_wrap(
      stats::as.formula(base::paste0("~", facet_col)),
      ncol = ncol,
      scales = "free_x"
    ) +
    ggplot2::scale_x_continuous(
      limits = base::c(continue_trend_start_year - 0.5, NA),
      expand = ggplot2::expansion(c(0, 0.03))
    ) +
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(c(0, 0)),
      limits = base::c(0, 1)
    ) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      col = legend_title,
      x = NULL,
      y = y_label,
      caption = caption
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.spacing = grid::unit(2, "lines"),
      strip.background = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(size = 14),
      plot.subtitle = ggplot2::element_text(size = 12),
      strip.text.x.top = ggplot2::element_text(size = 12),
      axis.text = ggplot2::element_text(size = 12),
      axis.title = ggplot2::element_text(size = 12),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "vertical",
      legend.box.margin = ggplot2::margin(t = 10, b = 0),
      legend.title.position = "top",
      legend.title = ggplot2::element_text(size = 12, hjust = 0.5),
      legend.text = ggplot2::element_text(size = 10),
      plot.caption = ggplot2::element_text(size = 10)
    )

  if (!base::is.null(output_file)) {
    base::tryCatch(
      {
        ggplot2::ggsave(
          plot = pp,
          filename = output_file,
          dpi = dpi,
          width = width,
          height = height
        )

        if (grDevices::dev.cur() > 1) {
          grDevices::dev.off()
        }

        if (compress_image &&
            base::grepl("\\.png$", output_file, ignore.case = TRUE)) {
          compress_png(
            path = output_file,
            png_overwrite = image_overwrite,
            speed = compression_speed,
            verbosity = compression_verbose
          )
        }

        success_msg <- "Plot saved to:"
        display_path <- output_file
        if (base::startsWith(output_file, base::getwd())) {
          display_path <- base::sub(
            base::paste0("^", base::getwd(), "/"),
            "",
            output_file
          )
        } else if (base::grepl("03_outputs", output_file)) {
          display_path <- base::sub(
            ".*/(03_outputs/.*)",
            "\\1",
            output_file
          )
        }
        cli::cli_alert_success(base::paste(success_msg, display_path))
      },
      error = function(e) {
        if (grDevices::dev.cur() > 1) {
          grDevices::dev.off()
        }
        cli::cli_warn("Failed to save plot to {output_file}: {e$message}")
      }
    )
  }

  if (!show_plot) {
    return(base::invisible(pp))
  }
  pp
}
