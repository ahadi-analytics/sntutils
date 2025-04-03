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
#' @param language A character string specifying the language for plot labels.
#'   Defaults to "en" (English). Use ISO 639-1 language codes.
#' @return A ggplot2 object representing the tile plot.
#' @param compress_image Option to compress the plot
#' @param image_overwrite Logical. If TRUE, will overwrite existing files.
#'    Default is TRUE
#' @param compression_speed Integer. Speed/quality trade-off from 1
#'    (brute-force) to 10 (fastest). Default is 3. Speed 10 has 5% lower
#'    quality but is 8 times
#'    faster.
#' @param compression_verbose Logical. Controls the amount of information
#'     displayed. FALSE = minimal, TRUE = detailed. Default is TRUE.
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
                              language = "en",
                              compress_image = TRUE,
                              image_overwrite = TRUE,
                              compression_speed = 1,
                              compression_verbose = TRUE) {
  # ensure relevant packages are installed
  # ensure_packages(c("ggtext", "scales"))

  # Check if the length of tests and cases are the same
  if (length(tests) != length(cases)) {
    stop("The length of 'tests' and 'cases' must be the same.")
  }

  # Check if plot_path is provided when save_plot is TRUE
  if (save_plot && is.null(plot_path)) {
    stop("plot_path must be provided when save_plot is TRUE.")
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
    disease_name <- paste(
      test_column, case_column,
      sprintf("(n=%d, %.1f%%)", inconsistent_count, inconsistent_prop),
      sep = " vs "
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
      message(
        crayon::green(
          glue::glue(
            "Consistency test passed for {disease_name}: ",
            "There are more tests than there are cases!"
          )
        )
      )
    } else {
      message(
        crayon::red(
          glue::glue(
            "Consistency test failed for {disease_name}: ",
            "There are {scales::comma(inconsistent_count)} ",
            "({round(inconsistent_prop, 2)}%) ",
            "rows where cases are greater than tests."
          )
        )
      )
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
    ggplot2::theme_bw() +
    ggplot2::facet_wrap(~disease, scales = "free") +
    ggplot2::labs(
      y = "Cases",
      x = "Tests",
      title = paste(
        "<span style = 'font-size:10pt'><b style='color",
        ":#526A83'>Consistency Check</b>: Comparing",
        "the number of tests and cases</span>"
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
  if (language != "en") {
    plot$labels$x <- translate_text(plot$labels$x, language)
    plot$labels$y <- translate_text(plot$labels$y, language)
    plot$labels$title <- translate_text(plot$labels$title, language)
    # Remove any spaces between ** and the text, handling multiple spaces
    plot$labels$title <- gsub(
      "\\*\\*\\s+(.*?)\\s+\\*\\*", "**\\1**",
      plot$labels$title
    )
  }

  # Save the plot if requested
  if (save_plot) {
    save_title <- gsub(
      " ", "_",
      translate_text(
        paste0(
          "consistency checks comparing malaria cases against tests"
        ), language
      )
    ) |> tolower()

    year_range <- if (
      min(data$year, na.rm = T) != max(data$year, na.rm = T)) {
      glue::glue("{min(data$year, na.rm = T)}-{max(data$year, na.rm = T)}")
    } else {
      min(date$year)
    }

    basename <- glue::glue(
      "{save_title}",
      "_{year_range}_{format(Sys.Date(), '%Y-%m-%d')}.png"
    )

    # Create directory if it doesn't exist
    if (!dir.exists(plot_path)) {
      dir.create(plot_path, recursive = TRUE)
    }

    full_path <- file.path(plot_path, basename)

    # Calculate dimensions based on number of test variables
    n_vars <- length(tests)
    width <- min(10, max(6, n_vars * 3))
    height <- min(8, max(4, n_vars * 2))

    # Save the plot
    ggplot2::ggsave(
      filename = full_path,
      plot = plot,
      width = width,
      height = height,
      dpi = 300
    )

    success_msg <- translate_text("Plot saved to:", language)
    cli::cli_alert_success(
      paste(success_msg, full_path)
    )

    if (compress_image) {
      snt::compress_png(full_path,
        verbosity = compression_verbose,
        speed = compression_speed,
        png_overwrite = image_overwrite
      )
    }
  }

  return(plot)
}
