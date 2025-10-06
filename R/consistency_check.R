#' Consistency Check Function
#'
#' This function performs logical consistency checks to ensure that the
#' number of inputs is greater than or equal to the number of outputs for
#' given columns in a dataset. It returns a ggplot2 object visualizing the
#' results and alerts the user to any inconsistencies where outputs exceed
#' inputs.
#'
#' @details
#' For malaria surveillance data, this function validates the logical
#' coherence of the care cascade by ensuring upstream events (inputs) are
#' ≥ downstream events (outputs). Common malaria data validation checks
#' include:
#'
#' \strong{Common malaria variable definitions:}
#' \itemize{
#'   \item \code{allout} = all outpatients
#'   \item \code{susp} = suspected malaria cases
#'   \item \code{test} = malaria tests (or tested for malaria)
#'   \item \code{conf} = confirmed malaria cases
#'   \item \code{maltreat} = malaria cases treated
#'   \item \code{alladm} = all admissions (all hospital admissions)
#'   \item \code{maladm} = malaria admissions (hospital admissions for
#'     malaria)
#'   \item \code{alldth} = all deaths (total deaths)
#'   \item \code{maldth} = malaria deaths
#' }
#'
#' \strong{Typical logical validation checks:}
#' \itemize{
#'   \item susp vs test: suspected malaria ≥ malaria tests
#'   \item allout vs susp: all outpatients ≥ suspected malaria
#'   \item allout vs test: all outpatients ≥ malaria tests
#'   \item test vs conf: malaria tests ≥ confirmed malaria cases
#'   \item conf vs maltreat: confirmed malaria cases ≥ malaria treated
#'   \item alladm vs maladm: all admissions ≥ malaria admissions
#'   \item alldth vs maldth: all deaths ≥ malaria deaths
#'   \item maladm vs maldth: malaria admissions ≥ malaria deaths
#' }
#'
#' @param data A data frame containing the input and output data.
#' @param inputs A character vector specifying the column names for the
#'   input data (e.g., malaria tests, all outpatients, suspected cases).
#'   These represent upstream events in the care cascade.
#' @param outputs A character vector specifying the column names for the
#'   output data (e.g., confirmed cases, malaria admissions, malaria
#'   deaths). These represent downstream events in the care cascade. The
#'   length of 'inputs' and 'outputs' must be the same, and each element
#'   in 'inputs' corresponds to an element in 'outputs'.
#' @param tests \strong{[Deprecated]} Use `inputs` instead. A character
#'   vector specifying the column names for the test data.
#' @param cases \strong{[Deprecated]} Use `outputs` instead. A character
#'   vector specifying the column names for the case data.
#' @param save_plot Logical. If TRUE, the plot will be saved to disk.
#'   Default is FALSE.
#' @param plot_path Character. Directory path where the plot should be
#'   saved. Required if save_plot is TRUE. Default is NULL.
#' @param target_language A character string specifying the language for
#'   plot labels. Defaults to "en" (English). Use ISO 639-1 language
#'   codes.
#' @param source_language Source language code, defaults to "en"
#' @param lang_cache_path Path for translation cache, defaults to
#'   tempdir()
#' @param compress_image Logical. If TRUE, the saved image will be
#'   compressed. Default is FALSE
#' @param image_overwrite Logical. If TRUE, existing image will be
#'   overwritten. Default is TRUE.
#' @param compression_speed Integer from 1-10. Speed of compression
#'   (1=slow/high compression). Default is 1.
#' @param compression_verbose Logical. If TRUE, compression details will
#'   be printed. Default is TRUE.
#'
#' @return A ggplot2 object showing the consistency between the number of
#'   inputs and outputs. The x-axis represents the outputs, and the y-axis
#'   represents the inputs. Each facet represents a variable pair, and the
#'   diagonal line shows where the number of inputs equals the number of
#'   outputs.
#'
#' @examples
#' # Basic consistency checks for malaria surveillance data
#'\dontrun{
#' # get path
#' path <- system.file(
#'   "extdata",
#'   "fake_epi_df_togo.rds",
#'   package = "epiCleanr"
#' )
#'
#' # get example data
#' fake_epi_df_togo <- sntutils::read(path)
#'
#' # Example 1: Check test-to-confirmation cascade
#' # Validates: malaria tests ≥ confirmed malaria cases
#' consistency_check(
#'   fake_epi_df_togo,
#'   inputs = c("malaria_tests"),
#'   outputs = c("malaria_cases")
#' )
#'
#' # Example 2: Multiple validation checks at once
#' # Checks common malaria care cascade validations
#' consistency_check(
#'   fake_epi_df_togo,
#'   inputs = c("test", "allout", "conf", "alladm", "alldth", "maladm"),
#'   outputs = c("conf", "susp", "maltreat", "maladm", "maldth", "maldth")
#' )
#'
#' # Example 3: Save the plot to disk
#' consistency_check(
#'   fake_epi_df_togo,
#'   inputs = c("malaria_tests", "all_outpatients"),
#'   outputs = c("malaria_cases", "suspected_malaria"),
#'   save_plot = TRUE,
#'   plot_path = tempdir()
#' )
#'
#' # Example 4: Validate with multiple diseases
#' consistency_check(
#'   fake_epi_df_togo,
#'   inputs = c("malaria_tests", "cholera_tests"),
#'   outputs = c("malaria_cases", "cholera_cases")
#' )
#'
#' # Old syntax (deprecated but still supported for backward
#' # compatibility)
#' consistency_check(
#'   fake_epi_df_togo,
#'   tests = c("malaria_tests"),
#'   cases = c("malaria_cases")
#' )
#'}
#'
#' @export
consistency_check <- function(data,
                              inputs = NULL,
                              outputs = NULL,
                              tests = NULL,
                              cases = NULL,
                              save_plot = FALSE,
                              plot_path = NULL,
                              target_language = "en",
                              source_language = "en",
                              lang_cache_path = tempdir(),
                              compress_image = FALSE,
                              image_overwrite = TRUE,
                              compression_speed = 1,
                              compression_verbose = TRUE) {

  # Ensure relevant packages are installed
  ensure_packages(c("ggtext", "scales"))

  # Handle deprecated parameters with soft deprecation
  if (!is.null(tests) || !is.null(cases)) {
    lifecycle::deprecate_soft(
      when = "0.1.0",
      what = "consistency_check(tests)",
      with = "consistency_check(inputs)"
    )
    lifecycle::deprecate_soft(
      when = "0.1.0",
      what = "consistency_check(cases)",
      with = "consistency_check(outputs)"
    )

    # Use deprecated parameters if new ones not provided
    if (is.null(inputs)) inputs <- tests
    if (is.null(outputs)) outputs <- cases
  }

  # Check that inputs and outputs are provided
  if (is.null(inputs) || is.null(outputs)) {
    cli::cli_abort(c(
      "!" = "Both 'inputs' and 'outputs' parameters must be provided.",
      "i" = "Run `rlang::last_trace()` to see where the error occurred."
    ))
  }

  # Check if the length of inputs and outputs are the same
  if (length(inputs) != length(outputs)) {
    cli::cli_abort(c(
      "!" = "The length of 'inputs' and 'outputs' must be the same.",
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
    comparison = character(),
    outputs = numeric(),
    inputs = numeric()
  )

  # Compute statistics and find rows where inputs are less than outputs
  inconsistent_rows <- list()
  consistency_stats <- list()

  for (i in seq_along(inputs)) {
    input_column <- inputs[i]
    output_column <- outputs[i]

    # Rows where there are more outputs than inputs
    inconsistency <- data |>
      dplyr::filter(
        !is.na(!!rlang::sym(output_column)),
        !is.na(!!rlang::sym(input_column))
      ) |>
      dplyr::filter(
        !!rlang::sym(output_column) > !!rlang::sym(input_column)
      )

    inconsistent_count <- nrow(inconsistency)
    inconsistent_prop <- inconsistent_count / nrow(data) * 100
    inconsistent_rows[[i]] <- inconsistency
    comparison_name <- paste0(
      input_column, " vs ", output_column,
      " (n = ", inconsistent_count, ", ",
      sprintf("%.1f%%", inconsistent_prop), ")"
    )

    results <- rbind(
      results,
      data.frame(
        comparison = comparison_name,
        outputs = data[[output_column]],
        inputs = data[[input_column]]
      )
    )

    # Check if there are more inputs than outputs
    if (inconsistent_count == 0) {
      cli::cli_alert_success(
        glue::glue(
          "Consistency test passed for {comparison_name}: ",
          "There are more inputs than there are outputs!"
        )
      )
    } else {
      cli::cli_alert_warning(paste0(
        "Consistency test failed for ", comparison_name, ": There are ",
        scales::comma(inconsistent_count), " (",
        round(inconsistent_prop, 2),
        "%) rows where outputs are greater than inputs."
      ))
    }
  }

  # Create the plot
  plot <-
    results |>
    dplyr::filter(!is.na(outputs) & !is.na(inputs)) |>
    ggplot2::ggplot(ggplot2::aes(x = outputs, y = inputs)) +
    ggplot2::geom_point(
      ggplot2::aes(color = outputs > inputs),
      shape = 16,
      size = 4,
      show.legend = FALSE,
      alpha = .5,
      na.rm = TRUE
    ) +
    ggplot2::scale_color_manual(
      values = c("TRUE" = "red", "FALSE" = "#1e81b0")
    ) +
    ggplot2::geom_line(
      data = function(df) {
        bounds <- df |>
          dplyr::summarise(
            min_val = min(c(outputs, inputs), na.rm = TRUE),
            max_val = max(c(outputs, inputs), na.rm = TRUE)
          )
        data.frame(
          outputs = seq(bounds$min_val, bounds$max_val, length.out = 100),
          inputs = seq(bounds$min_val, bounds$max_val, length.out = 100)
        )
      },
      ggplot2::aes(x = outputs, y = inputs),
      color = "grey10",
      linetype = "dashed",
      linewidth = 1,
      na.rm = TRUE
    ) +
    ggplot2::facet_wrap(~comparison, scales = "free") +
    ggplot2::labs(
      x = "Reported events (input)",
      y = "Recorded outcomes (output)",
      title = paste0(
        "<span style = 'font-size:10pt'><b style='color:#526A83'>",
        "Consistency Check</b>: Comparing the number of inputs ",
        "and outputs</span>"
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
      axis.title.y = ggplot2::element_text(
        margin = ggplot2::margin(r = 10)
      ),
      axis.title.x = ggplot2::element_text(
        margin = ggplot2::margin(t = 12)
      ),
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
      x_var = "inputs",
      vars_of_interest = paste(inputs, collapse = "_"),
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
      "{paste(inputs[0:3], collapse = '_')}_vs_",
      "{paste(outputs[0:3], collapse = '_')}_",
      "{translated_terms$year_range}_",
      "{format(Sys.Date(), '%Y-%m-%d')}.png"
    ) |> stringr::str_remove_all("_NA")

    full_path <- file.path(plot_path, basename)

    # Calculate dimensions based on number of input variables
    n_vars <- length(inputs)
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

  plot
}
