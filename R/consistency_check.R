#' Consistency Check Function
#'
#' This function performs a consistency check to ensure that the number of tests
#' is greater than the number of cases for given columns in a dataset. It
#' returns a \code{\link[=ggplot2]{ggplot2}} object visualizing the results.
#'
#' @param data A data frame containing the test and case data.
#' @param tests A character vector specifying the column names for the test
#'   data.
#' @param cases A character vector specifying the column names for the case
#'   data. The length of 'tests' and 'cases' must be the same, and each element
#'   in 'tests' corresponds to an element in 'cases'.
#' @param save_plot Logical. If TRUE, the plot will be saved to disk. Default
#'   is FALSE.
#' @param plot_path Character. Directory path where the plot should be saved.
#'   Required if save_plot is TRUE. Default is NULL.
#' @param target_language A character string specifying the language for plot
#'   labels. Defaults to "en" (English). Use ISO 639-1 language codes.
#' @param source_language Source language code, defaults to "en"
#' @param lang_cache_path Path for translation cache, defaults to tempdir()
#' @param compress_image Logical. If TRUE, the saved image will be compressed.
#'   Default is TRUE.
#' @param image_overwrite Logical. If TRUE, existing image will be overwritten.
#'   Default is TRUE.
#' @param compression_speed Integer from 1-10. Speed of compression
#'   (1=slow/high compression). Default is 1.
#' @param compression_verbose Logical. If TRUE, compression details will be
#'   printed. Default is TRUE.
#'
#' @return A \code{\link[=ggplot2]{ggplot2::ggplot()}} object showing
#'   the consistency between the number of tests and cases. The x-axis
#'   represents the cases, and the y-axis represents the tests. Each facet
#'   represents a disease, and the diagonal line shows where the number of
#'   tests equals the number of cases.
#'
#' @examples
#' # check the consistency between malaria tests and cases
#'
#' # get path
#' path <- system.file(
#'   "extdata",
#'   "fake_epi_df_togo.rds",
#'   package = "epiCleanr"
#' )
#'
#' # get example data
#' fake_epi_df_togo <- read(path)
#'
#' consistency_check(
#'   fake_epi_df_togo,
#'   tests = c("malaria_tests", "cholera_tests"),
#'   cases = c("malaria_cases", "cholera_cases")
#' )
#'
#' # Save the plot
#' consistency_check(
#'   fake_epi_df_togo,
#'   tests = c("malaria_tests", "cholera_tests"),
#'   cases = c("malaria_cases", "cholera_cases"),
#'   save_plot = TRUE,
#'   plot_path = tempdir()
#' )
#'
#' @export
consistency_check <- function(data, tests, cases,
                              save_plot = FALSE,
                              plot_path = NULL,
                              target_language = "en",
                              source_language = "en",
                              lang_cache_path = tempdir(),
                              compress_image = TRUE,
                              image_overwrite = TRUE,
                              compression_speed = 1,
                              compression_verbose = TRUE) {
  # Ensure relevant packages are installed
  ensure_packages(c("ggtext", "scales"))

  # Check if the length of tests and cases are the same
  if (length(tests) != length(cases)) {
    cli::cli_abort(c(
      "!" = "The length of 'tests' and 'cases' must be the same.",
      "i" = "Run `rlang::last_trace()` to see where the error occurred."
    ))
  }

  # Check if plot_path is provided when save_plot is TRUE
  if (save_plot && is.null(plot_path)) {
    cli::cli_abort(c(
      "!" = "plot_path must be provided when save_plot is TRUE.",
      "i" = "Run `rlang::last_trace()` to see where the error occurred."
    ))
  }

  # Initialize a data frame to store results
  results <- data.frame(
    disease = character(),
    cases = numeric(),
    tests = numeric()
  )

  # Compute statistics and find rows where tests are less than cases
  inconsistent_rows <- list()
  consistency_stats <- list()

  for (i in seq_along(tests)) {
    test_column <- tests[i]
    case_column <- cases[i]

    # Rows where there are more cases than tests
    inconsistency <- data |>
      dplyr::filter(
        !is.na(!!rlang::sym(case_column)),
        !is.na(!!rlang::sym(test_column))
      ) |>
      dplyr::filter(
        !!rlang::sym(case_column) > !!rlang::sym(test_column)
      )

    inconsistent_count <- nrow(inconsistency)
    inconsistent_prop <- inconsistent_count / nrow(data) * 100
    inconsistent_rows[[i]] <- inconsistency
    disease_name <- paste0(
      test_column, " vs ", case_column,
      " (n=", inconsistent_count, ", ",
      sprintf("%.1f%%", inconsistent_prop), ")"
    )

    results <- rbind(
      results,
      data.frame(
        disease = disease_name,
        cases = data[[case_column]],
        tests = data[[test_column]]
      )
    )

    # Check if there are more tests than cases
    if (inconsistent_count == 0) {
      cli::cli_alert_success(
        glue::glue(
          "Consistency test passed for {disease_name}: ",
          "There are more tests than there are cases!"
        )
      )
    } else {
      cli::cli_alert_warning(paste0(
        "Consistency test failed for ", disease_name, ": There are ",
        scales::comma(inconsistent_count), " (",
        round(inconsistent_prop, 2),
        "%) rows where cases are greater than tests."
      ))
    }
  }

  # Create the plot
  plot <- ggplot2::ggplot(results, ggplot2::aes(y = cases, x = tests)) +
    ggplot2::geom_point(
      shape = 16,
      size = 4,
      show.legend = FALSE,
      alpha = .5,
      color = "#1e81b0",
      na.rm = TRUE
    ) +
    ggplot2::geom_abline(
      intercept = 0,
      linewidth = 1,
      alpha = .7,
      slope = 1,
      color = "darkred"
    ) +
    ggplot2::facet_wrap(~disease, scales = "free") +
    ggplot2::labs(
      y = "Cases",
      x = "Tests",
      title = paste0(
        "<span style = 'font-size:10pt'><b style='color:#526A83'>",
        "Consistency Check</b>: Comparing the number of tests and cases</span>"
      )
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "top",
      plot.title = ggtext::element_markdown(),
      plot.caption = ggplot2::element_text(size = 8),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(
        color = "grey90",
        linetype = 3
      ),
      panel.background = ggplot2::element_rect(
        color = "grey10",
        linewidth = 0.6
      ),
      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 10)),
      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 12)),
      axis.text.x = ggplot2::element_text(
        size = 8,
        angle = 45,
        hjust = 1,
        margin = ggplot2::margin(t = 2, b = 2)
      )
    ) +
    ggplot2::scale_x_continuous(
      labels = scales::comma_format(big.mark = ","),
      limits = c(0, NA),
      expand = c(0, 0)
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::comma_format(big.mark = ","),
      limits = c(0, NA),
      expand = c(0, 0)
    )

  # Translate labels if language is not English
  if (target_language != "en") {
    plot <- translate_plot_labels(
      plot,
      target_language = target_language,
      source_language = source_language,
      lang_cache_path = lang_cache_path
    )
  }

  # Save the plot if requested
  if (save_plot) {
    # Get common translated terms for filenames
    translated_terms <- get_translated_terms(
      target_language = target_language,
      source_language = source_language,
      lang_cache_path = lang_cache_path,
      x_var = "tests",
      vars_of_interest = paste(tests, collapse = "_"),
      data = data,
      save_title_prefix = "consistency check"
    )

    # Create directory if it doesn't exist
    if (!dir.exists(plot_path)) {
      dir_created <- dir.create(plot_path,
        recursive = TRUE, showWarnings = FALSE
      )
      if (!dir_created) {
        cli::cli_warn("Could not create directory: {plot_path}")
        return(invisible(NULL))
      }
    }

    basename <- glue::glue(
      "{translated_terms$prefix}_{translated_terms$for_word}_",
      "{paste(tests[0:3], collapse = '_')}_vs_",
      "{paste(cases[0:3], collapse = '_')}_",
      "{translated_terms$year_range}_",
      "{format(Sys.Date(), '%Y-%m-%d')}.png"
    ) |> stringr::str_remove_all("_NA")

    full_path <- file.path(plot_path, basename)

    # Calculate dimensions based on number of test variables
    n_vars <- length(tests)
    width <- min(10, max(6, n_vars * 3))
    height <- min(8, max(4, n_vars * 2))

    # Try to save the plot
    tryCatch(
      {
        ggplot2::ggsave(
          filename = full_path,
          plot = plot,
          width = width,
          height = height,
          dpi = 300
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

        success_msg <- translate_text(
          "Plot saved to:",
          target_language = target_language,
          source_language = source_language,
          cache_path = lang_cache_path
        )
        cli::cli_alert_success(paste(success_msg, full_path))
      },
      error = function(e) {
        # Close device on error
        if (grDevices::dev.cur() > 1) {
          grDevices::dev.off()
        }
        cli::cli_warn("Failed to save plot to {full_path}: {e$message}")
      }
    )
  }

  return(plot)
}
