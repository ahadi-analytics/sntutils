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
#' greater than or equal to downstream events (outputs). Common malaria
#' data validation checks include:
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
#'   \item susp vs test: suspected malaria >= malaria tests
#'   \item allout vs susp: all outpatients >= suspected malaria
#'   \item allout vs test: all outpatients >= malaria tests
#'   \item test vs conf: malaria tests >= confirmed malaria cases
#'   \item conf vs maltreat: confirmed malaria cases >= malaria treated
#'   \item alladm vs maladm: all admissions >= malaria admissions
#'   \item alldth vs maldth: all deaths >= malaria deaths
#'   \item maladm vs maldth: malaria admissions >= malaria deaths
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
#' @param plot_path Character. Directory path where the plot should be
#'   saved. If NULL (default), plot is not saved.
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
#' @param plot_scale Numeric. Scaling factor for saved plots. Values > 1
#'   increase size, < 1 decrease size. Default is 1.
#' @param plot_width Numeric. Width of saved plot in inches. If NULL (default),
#'   width is calculated based on number of variables.
#' @param plot_height Numeric. Height of saved plot in inches. If NULL (default),
#'   height is calculated based on number of variables.
#' @param plot_dpi Numeric. Resolution of saved plot in dots per inch.
#'   Default is 300.
#' @param show_plot Logical. If FALSE, the plot is returned invisibly (not displayed).
#'   Useful when only saving plots. Default is TRUE.
#' @param facet_by Character. Column name to facet the plot by (e.g., "year",
#'   "month", "adm1"). When provided, only the first input/output pair is
#'   processed and the plot is faceted by this variable instead of by comparison.
#'   Useful for examining consistency patterns across time or geographical units.
#'   Default is NULL (facet by comparison).
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
#' # Validates: malaria tests >= confirmed malaria cases
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
#' # Example 5: Facet by year for time-series analysis (single pair only)
#' consistency_check(
#'   fake_epi_df_togo,
#'   inputs = c("malaria_tests"),
#'   outputs = c("malaria_cases"),
#'   facet_by = "year"
#' )
#'
#' # Example 6: Facet by administrative unit (single pair only)
#' consistency_check(
#'   fake_epi_df_togo,
#'   inputs = c("test"),
#'   outputs = c("conf"),
#'   facet_by = "adm1"
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
                              plot_path = NULL,
                              target_language = "en",
                              source_language = "en",
                              lang_cache_path = tempdir(),
                              compress_image = FALSE,
                              image_overwrite = TRUE,
                              compression_speed = 1,
                              compression_verbose = TRUE,
                              plot_scale = 1,
                              plot_width = NULL,
                              plot_height = NULL,
                              plot_dpi = 300,
                              show_plot = TRUE,
                              facet_by = NULL) {

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

  # Check facet_by parameter constraints
  if (!is.null(facet_by)) {
    if (length(inputs) > 1 || length(outputs) > 1) {
      cli::cli_abort(c(
        "!" = "When 'facet_by' is provided, only one input/output pair is allowed.",
        "i" = "Please provide single values for 'inputs' and 'outputs'.",
        "i" = "Run `rlang::last_trace()` to see where the error occurred."
      ))
    }

    if (!facet_by %in% names(data)) {
      cli::cli_abort(c(
        "!" = "Column '{facet_by}' not found in the data.",
        "i" = "Available columns: {paste(names(data), collapse = ', ')}",
        "i" = "Run `rlang::last_trace()` to see where the error occurred."
      ))
    }
  }


  # Initialize a data frame to store results
  if (!is.null(facet_by)) {
    results <- data.frame(
      comparison = character(),
      outputs = numeric(),
      inputs = numeric(),
      facet_var = character(),
      stringsAsFactors = FALSE
    )
  } else {
    results <- data.frame(
      comparison = character(),
      outputs = numeric(),
      inputs = numeric(),
      stringsAsFactors = FALSE
    )
  }

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
      " (n = ", scales::comma(inconsistent_count), ", ",
      sprintf("%.1f%%", inconsistent_prop), ")"
    )

    if (!is.null(facet_by)) {
      results <- rbind(
        results,
        data.frame(
          comparison = comparison_name,
          outputs = data[[output_column]],
          inputs = data[[input_column]],
          facet_var = as.character(data[[facet_by]]),
          stringsAsFactors = FALSE
        )
      )
    } else {
      results <- rbind(
        results,
        data.frame(
          comparison = comparison_name,
          outputs = data[[output_column]],
          inputs = data[[input_column]],
          stringsAsFactors = FALSE
        )
      )
    }

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

  # Determine axis labels based on variable names
  if (length(inputs) == 1 && length(outputs) == 1) {
    x_label <- outputs[1]
    y_label <- inputs[1]
  } else {
    x_label <- "Output variables"
    y_label <- "Input variables"
  }

  # Add per-facet min/max for diagonal lines
  if (!is.null(facet_by)) {
    results <- results |>
      dplyr::group_by(facet_var) |>
      dplyr::mutate(
        diag_min = min(c(outputs, inputs), na.rm = TRUE),
        diag_max = max(c(outputs, inputs), na.rm = TRUE)
      ) |>
      dplyr::ungroup()
  } else {
    results <- results |>
      dplyr::group_by(comparison) |>
      dplyr::mutate(
        diag_min = min(c(outputs, inputs), na.rm = TRUE),
        diag_max = max(c(outputs, inputs), na.rm = TRUE)
      ) |>
      dplyr::ungroup()
  }

  # Create diagonal line data per facet
  if (!is.null(facet_by)) {
    diagonal_data <- results |>
      dplyr::distinct(facet_var, diag_min, diag_max) |>
      tidyr::expand_grid(t = seq(0, 1, length.out = 100)) |>
      dplyr::mutate(
        outputs = diag_min + t * (diag_max - diag_min),
        inputs = outputs
      )
  } else {
    diagonal_data <- results |>
      dplyr::distinct(comparison, diag_min, diag_max) |>
      tidyr::expand_grid(t = seq(0, 1, length.out = 100)) |>
      dplyr::mutate(
        outputs = diag_min + t * (diag_max - diag_min),
        inputs = outputs
      )
  }

  # Create the plot
  # Separate consistent and inconsistent points for proper layering
  plot_data <- results |>
    dplyr::filter(!is.na(outputs) & !is.na(inputs)) |>
    dplyr::mutate(is_inconsistent = outputs > inputs)

  plot <-
    ggplot2::ggplot(plot_data, ggplot2::aes(x = outputs, y = inputs)) +
    # Layer 1: Draw consistent points (blue) first
    ggplot2::geom_point(
      data = dplyr::filter(plot_data, !is_inconsistent),
      color = "#1e81b0",
      shape = 16,
      size = 4,
      alpha = .5,
      na.rm = TRUE
    ) +
    # Layer 2: Draw inconsistent points (red) on top
    ggplot2::geom_point(
      data = dplyr::filter(plot_data, is_inconsistent),
      color = "red",
      shape = 16,
      size = 4,
      alpha = .5,
      na.rm = TRUE
    ) +
    ggplot2::geom_line(
      data = diagonal_data,
      ggplot2::aes(x = outputs, y = inputs),
      color = "grey10",
      linetype = "dashed",
      linewidth = 1,
      na.rm = TRUE
    ) +
    {
      if (!is.null(facet_by)) {
        # Calculate inconsistency stats by facet group
        facet_stats <- results |>
          dplyr::filter(!is.na(outputs) & !is.na(inputs)) |>
          dplyr::group_by(facet_var) |>
          dplyr::summarise(
            total_obs = dplyr::n(),
            inconsistent_obs = sum(outputs > inputs, na.rm = TRUE),
            .groups = 'drop'
          ) |>
          dplyr::mutate(
            prop = inconsistent_obs / total_obs * 100,
            label = paste0(facet_var, " (n = ", scales::comma(inconsistent_obs),
                          ", ", sprintf("%.1f", prop), "%)")
          )

        # Create named vector for labeller
        facet_labels <- setNames(facet_stats$label, facet_stats$facet_var)

        ggplot2::facet_wrap(ggplot2::vars(facet_var), scales = "free",
                           labeller = ggplot2::labeller(
                             facet_var = facet_labels
                           ))
      } else {
        ggplot2::facet_wrap(~comparison, scales = "free")
      }
    } +
    ggplot2::labs(
      x = x_label,
      y = paste0(y_label, "\n"),
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
      strip.text = ggplot2::element_text(face = "bold"),
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
      labels = scales::label_number(scale_cut = scales::cut_short_scale()),
      expand = ggplot2::expansion(mult = c(0.05, 0.05))
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::label_number(scale_cut = scales::cut_short_scale()),
      expand = ggplot2::expansion(mult = c(0.05, 0.05))
    )

  # Translate labels if language is not English
  if (target_language != "en") {
    # Store original x and y labels when faceting to preserve them
    if (!is.null(facet_by)) {
      original_x_label <- plot$labels$x
      original_y_label <- plot$labels$y
    }

    plot <- translate_plot_labels(
      plot,
      target_language = target_language,
      source_language = source_language,
      lang_cache_path = lang_cache_path
    )

    # Restore original axis labels when faceting
    if (!is.null(facet_by)) {
      plot$labels$x <- original_x_label
      plot$labels$y <- original_y_label
    }
  }

  # Save the plot if requested
  if (!is.null(plot_path)) {
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

    # Generate filename based on single vs multiple pairs
    if (length(inputs) == 1 && length(outputs) == 1) {
      # Single pair: use actual variable names with translated filename components
      # Extract year range from data
      year_range <- tryCatch({
        if ("year" %in% names(data)) {
          years <- data$year[!is.na(data$year)]
          if (length(years) > 0) {
            min_year <- min(years)
            max_year <- max(years)
            if (min_year == max_year) {
              as.character(min_year)
            } else {
              paste0(min_year, "-", max_year)
            }
          } else {
            NA
          }
        } else {
          NA
        }
      }, error = function(e) NA)

      if (target_language != "en") {
        # Try to get translations for filename components, fall back to English if not available
        check_word <- tryCatch({
          translate_text(
            "consistency check",
            target_language = target_language,
            source_language = source_language,
            cache_path = lang_cache_path
          ) |> stringr::str_replace_all(" ", "_")
        }, error = function(e) "consistency_check")

        # Add "pour" (for) after consistency check in non-English languages
        pour_word <- tryCatch({
          translate_text(
            "for",
            target_language = target_language,
            source_language = source_language,
            cache_path = lang_cache_path
          )
        }, error = function(e) "for")

        vs_word <- "vs"  # Always keep "vs" untranslated in filenames

        by_word <- if (!is.null(facet_by)) {
          by_translated <- tryCatch({
            translate_text(
              "by",
              target_language = target_language,
              source_language = source_language,
              cache_path = lang_cache_path
            )
          }, error = function(e) "by")
          paste0("_", by_translated, "_", facet_by)
        } else {
          ""
        }
      } else {
        check_word <- "consistency_check"
        pour_word <- ""  # No "for" in English filenames
        vs_word <- "vs"
        by_word <- if (!is.null(facet_by)) paste0("_by_", facet_by) else ""
      }

      # Add year range to filename if available
      year_suffix <- if (!is.na(year_range)) paste0("_", year_range) else ""

      # Add "pour" word if not empty
      pour_part <- if (pour_word != "") paste0("_", pour_word) else ""

      basename <- glue::glue(
        "{check_word}{pour_part}_{inputs[1]}_{vs_word}_{outputs[1]}{by_word}{year_suffix}_",
        "v{format(Sys.Date(), '%Y-%m-%d')}.png"
      )
    } else {
      # Multiple pairs: use translated terms approach
      translated_terms <- get_translated_terms(
        target_language = target_language,
        source_language = source_language,
        lang_cache_path = lang_cache_path,
        x_var = "inputs",
        vars_of_interest = paste(inputs, collapse = "_"),
        data = data,
        save_title_prefix = "consistency check"
      )

      basename <- glue::glue(
        "{translated_terms$prefix}_{translated_terms$for_word}_",
        "{paste(inputs[0:3], collapse = '_')}_vs_",
        "{paste(outputs[0:3], collapse = '_')}_",
        "{translated_terms$year_range}_",
        "v{format(Sys.Date(), '%Y-%m-%d')}.png"
      ) |> stringr::str_remove_all("_NA")
    }

    full_path <- file.path(plot_path, basename)

    # Calculate dimensions or use provided values
    if (is.null(plot_width) || is.null(plot_height)) {
      n_vars <- length(inputs)
      if (is.null(plot_width)) {
        width <- min(10, max(6, n_vars * 3))
      } else {
        width <- plot_width
      }
      if (is.null(plot_height)) {
        height <- min(8, max(4, n_vars * 2))
      } else {
        height <- plot_height
      }
    } else {
      width <- plot_width
      height <- plot_height
    }

    # Try to save the plot
    tryCatch(
      {
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

        success_msg <- tryCatch({
          translate_text(
            "Plot saved to:",
            target_language = target_language,
            source_language = source_language,
            cache_path = lang_cache_path
          )
        }, error = function(e) "Plot saved to:")

        # Show only relative path from current directory if it's a subdirectory
        display_path <- full_path
        if (startsWith(full_path, getwd())) {
          display_path <- sub(paste0("^", getwd(), "/"), "", full_path)
        } else if (grepl("03_outputs", full_path)) {
          # Extract from 03_outputs onward if present
          display_path <- sub(".*/(03_outputs/.*)", "\\1", full_path)
        }
        cli::cli_alert_success(paste(success_msg, display_path))
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

  # Return invisibly if show_plot is FALSE
  if (show_plot) {
    return(plot)
  } else {
    return(invisible(plot))
  }
}

#' Consistency violation map
#'
#' @description
#' Creates a map showing the number of periods where a logical consistency
#' violation occurred. A violation is defined as rows where the output variable
#' exceeds the input variable (output > input). Results are aggregated at the
#' specified administrative level and optionally faceted by a time variable.
#'
#' @details
#' Designed to complement `consistency_check()`. This function summarises the
#' number of violating rows within each administrative unit and displays the
#' magnitude spatially. Only a single input-output pair is accepted per call.
#'
#' Typical malaria cascade checks include:
#' - test vs conf
#' - susp vs test
#' - conf vs treat
#'
#' @param data Data frame containing the input and output variables.
#' @param shapefile sf object with administrative boundaries.
#' @param input_var Character. Upstream variable (e.g. "test").
#' @param output_var Character. Downstream variable (e.g. "conf").
#' @param adm_var Character. Administrative unit in both data and shapefile.
#' @param x_var Character. Optional time variable (e.g. "year", "yearmon").
#'   When provided, the map is faceted by this variable.
#' @param facet_ncol Integer. Number of facet columns. Default is 4.
#' @param language Character. Language code: "en", "fr", or "pt". Default "en".
#' @param plot_path Character. Directory or full file path for saving output.
#'   Default NULL.
#' @param compress_image Logical. Compress final PNG. Default FALSE.
#' @param image_overwrite Logical. Overwrite existing file. Default TRUE.
#' @param compression_speed Integer 1-10. Default 1.
#' @param compression_verbose Logical. Default TRUE.
#' @param plot_scale Numeric. Default 1.
#' @param plot_width Numeric. Default NULL.
#' @param plot_height Numeric. Default NULL.
#' @param plot_dpi Numeric. Default 300.
#' @param show_plot Logical. Print plot. Default TRUE.
#'
#' @return A ggplot2 object.
#'
#' @examples
#' \dontrun{
#' consistency_map(
#'   data = hf_data,
#'   shapefile = adm2_sf,
#'   input_var = "test",
#'   output_var = "conf",
#'   adm_var = "adm2",
#'   x_var = "year"
#' )
#' }
#'
#' @export
consistency_map <- function(
  data,
  shapefile,
  input_var,
  output_var,
  adm_var,
  x_var = NULL,
  facet_ncol = 4,
  language = "en",
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
  # Validate -------------------------------------------------------------------
  if (!inherits(shapefile, "sf")) {
    cli::cli_abort("'shapefile' must be an sf object.")
  }
  if (!all(c(input_var, output_var) %in% names(data))) {
    cli::cli_abort("Input or output variable not found in data.")
  }
  if (!adm_var %in% names(data)) {
    cli::cli_abort("'adm_var' missing in data.")
  }
  if (!adm_var %in% names(shapefile)) {
    cli::cli_abort("'adm_var' missing in shapefile.")
  }
  if (!is.null(x_var) && !x_var %in% names(data)) {
    cli::cli_abort("'x_var' missing in data.")
  }

  # Compute violations ---------------------------------------------------------
  summary_data <- data |>
    dplyr::mutate(
      violation = dplyr::if_else(
        .data[[output_var]] > .data[[input_var]],
        1L,
        0L
      )
    )

  summary_data <- if (is.null(x_var)) {
    summary_data |> dplyr::group_by(.data[[adm_var]])
  } else {
    summary_data |> dplyr::group_by(.data[[adm_var]], .data[[x_var]])
  }

  summary_data <- summary_data |>
    dplyr::summarise(
      n_violations = sum(violation, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::rename(admin = dplyr::all_of(adm_var))

  if (!is.null(x_var)) {
    summary_data <- summary_data |>
      dplyr::rename(time_var = dplyr::all_of(x_var))
  }

  # Join shapefile -------------------------------------------------------------
  merged <- shapefile |>
    dplyr::left_join(
      summary_data,
      by = dplyr::join_by(!!rlang::sym(adm_var) == admin)
    )

  if (!is.null(x_var)) {
    merged <- merged |> dplyr::mutate(time_fac = forcats::as_factor(time_var))
  }

  total_violations <- sum(summary_data$n_violations, na.rm = TRUE)

  # Translations ---------------------------------------------------------------
  titles <- list(
    en = "Consistency violations at the subnational level",
    fr = "Violations de coh\u00e9rence au niveau sous-national",
    pt = "Viola\u00e7\u00f5es de consist\u00eancia no n\u00edvel subnacional"
  )
  subtitle_templates <- list(
    en = paste0(
      "There are <b>{n}</b> more <b>{input_var}</b> observations",
      " than <b>{output_var}</b>"
    ),
    fr = paste0(
      "Il y a <b>{n}</b> observations de <b>{input_var}</b> de plus que ",
      "<b>{output_var}</b>"
    ),
    pt = paste0(
      "H\u00e1 <b>{n}</b> observa\u00e7\u00f5es de ",
      "<b>{input_var}</b> a mais que <b>{output_var}</b>"
    )
  )
  legend_labels <- list(
    en = "Number of logical inconsistencies",
    fr = "Nombre d'incoh\u00e9rences logiques",
    pt = "N\u00famero de inconsist\u00eancias l\u00f3gicas"
  )

  subtitle_text <- glue::glue(
    subtitle_templates[[language]],
    n = sntutils::big_mark(total_violations),
    input_var = input_var,
    output_var = output_var
  )

  # Palettes -------------------------------------------------------------------
  color_pal <- grDevices::colorRampPalette(
    c("#FC9272", "#FB6A4A", "#EF3B2C", "#CB181D", "#99000D")
  )(9)

  merged <- merged |>
    dplyr::mutate(
      viol_cat = dplyr::case_when(
        is.na(n_violations) ~ NA_character_,
        n_violations == 0 ~ "zero",
        n_violations > 0 ~ "nonzero"
      ),
      viol_val = dplyr::if_else(
        n_violations > 0,
        as.numeric(n_violations),
        NA_real_
      )
    )

  # Breaks ---------------------------------------------------------------------
  nonzero_values <- merged$viol_val[!is.na(merged$viol_val)]
  if (length(nonzero_values) > 0) {
    min_nonzero <- floor(min(nonzero_values))
    max_nonzero <- ceiling(max(nonzero_values))
    legend_breaks <- if (max_nonzero - min_nonzero <= 10) {
      seq(min_nonzero, max_nonzero, 2)
    } else {
      pretty(c(min_nonzero, max_nonzero), n = 5)
    }
  } else {
    legend_breaks <- NULL
  }

  has_ggnewscale <- requireNamespace("ggnewscale", quietly = TRUE)

  # Plot -----------------------------------------------------------------------
  if (has_ggnewscale) {
    p <- ggplot2::ggplot() +
      # ZERO VIOLATIONS LAYER
      ggplot2::geom_sf(
        data = merged |> dplyr::filter(viol_cat == "zero"),
        ggplot2::aes(fill = "0"),
        color = "white",
        size = 0.2
      ) +
      ggplot2::scale_fill_manual(
        values = c("0" = "#52AAC2"),
        breaks = "0",
        name = "",
        guide = ggplot2::guide_legend(
          order = 1,
          title.position = "top",
          label.position = "bottom",
          direction = "horizontal",
          keywidth = ggplot2::unit(0.8, "cm"),
          keyheight = ggplot2::unit(0.4, "cm")
        )
      ) +
      ggnewscale::new_scale_fill() +
      # NONZERO VIOLATIONS LAYER
      ggplot2::geom_sf(
        data = merged |> dplyr::filter(viol_cat == "nonzero"),
        ggplot2::aes(fill = viol_val),
        color = "white",
        size = 0.2
      ) +
      ggplot2::scale_fill_gradientn(
        colours = color_pal,
        name = legend_labels[[language]],
        na.value = "grey90",
        breaks = legend_breaks,
        labels = function(x) format(round(x), big.mark = ","),
        guide = ggplot2::guide_colorbar(
          order = 2,
          title.position = "top",
          label.position = "bottom",
          direction = "horizontal",
          barheight = ggplot2::unit(0.4, "cm"),
          barwidth = ggplot2::unit(6, "cm")
        )
      )
  } else {
    # Fallback if ggnewscale missing
    p <- ggplot2::ggplot() +
      ggplot2::geom_sf(
        data = merged |> dplyr::filter(viol_cat == "zero"),
        fill = "#52AAC2",
        color = "white",
        size = 0.2
      ) +
      ggplot2::geom_sf(
        data = merged |> dplyr::filter(viol_cat == "nonzero"),
        ggplot2::aes(fill = viol_val),
        color = "white",
        size = 0.2
      ) +
      ggplot2::scale_fill_gradientn(
        colours = color_pal,
        name = legend_labels[[language]],
        na.value = "grey90",
        breaks = legend_breaks,
        labels = function(x) format(round(x), big.mark = ",")
      )
  }

  ensure_packages("ggtext")

  # Theme ----------------------------------------------------------------------
  p <- p +
    ggplot2::labs(
      title = titles[[language]],
      subtitle = subtitle_text
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 13, face = "bold"),
      plot.subtitle = ggtext::element_markdown(
        size = 10,
        margin = ggplot2::margin(b = 10)
      ),
      legend.title = ggplot2::element_text(
        size = 12,
        face = "bold",
        margin = ggplot2::margin(l = -30, b = 4)
        # hjust = -0.7,
        # vjust = 0
      ),
      legend.position = "bottom",
      legend.justification = c(0, 0.5),
      legend.direction = "horizontal",
      legend.box = "horizontal",
      legend.box.just = "center",
      legend.box.spacing = ggplot2::unit(0.5, "cm"),
      legend.spacing.x = ggplot2::unit(0, "cm"),
      legend.margin = ggplot2::margin(t = 0, l = 0.3, unit = "cm"),
      axis.text = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold")
    )

  # Facet if time variable provided --------------------------------------------
    if (!is.null(x_var)) {
    p <- p +
      ggplot2::facet_wrap(
        stats::as.formula("~ time_fac"),
        ncol = facet_ncol
      )
    }

  # Saving ----------------------------------------------------------------------
  if (!is.null(plot_path)) {
    is_directory <- !grepl("\\.png$", plot_path, ignore.case = TRUE)

    if (is_directory) {
      if (!dir.exists(plot_path)) {
        dir.create(plot_path, recursive = TRUE, showWarnings = FALSE)
      }

      filename_parts <- list(
        en = "consistency_map",
        fr = "carte_violations_coherence",
        pt = "mapa_violacoes_consistencia"
      )

      base_name <- paste0(
        filename_parts[[language]],
        "_",
        input_var,
        "_vs_",
        output_var,
        if (!is.null(x_var)) paste0("_by_", x_var) else "",
        "_v",
        format(Sys.Date(), "%Y-%m-%d"),
        ".png"
      )

      full_path <- file.path(plot_path, base_name)
    } else {
      full_path <- plot_path
      plot_dir <- dirname(full_path)
      if (!dir.exists(plot_dir)) {
        dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)
      }
    }

    if (is.null(plot_width)) {
      plot_width <- 14
    }
    if (is.null(plot_height)) {
      plot_height <- 10
    }

    ggplot2::ggsave(
      filename = full_path,
      plot = p,
      width = plot_width,
      height = plot_height,
      dpi = plot_dpi,
      scale = plot_scale
    )

    if (compress_image) {
      compress_png(
        path = full_path,
        png_overwrite = image_overwrite,
        speed = compression_speed,
        verbosity = compression_verbose
      )
    }

    cli::cli_alert_success("Plot saved to: {full_path}")
  }

  # Return ---------------------------------------------------------------------
  if (show_plot) return(p) else return(invisible(p))
}
