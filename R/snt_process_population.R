# population summaries + dictionary ------------------------------------------

#' Summarise population by available admin levels and build a dictionary
#'
#' @description
#' Produces population summaries at the admin levels present in `pop_data`
#' (any of adm0..adm3) and returns a list containing per-level tables, a
#' bilingual (EN + optional FR) data dictionary, and a record of which
#' admin levels were found. By default, column types are first inferred
#' via [auto_parse_types()] with `apply = TRUE` and `return = "data"`.
#'
#' @details
#' ## Data uniqueness requirement
#' Input data must have unique rows per admin-year at the finest level present.
#' If your data contains sex, age, or residence strata, aggregate these upstream
#' before passing to this function. The function will error if duplicates are
#' detected to prevent silent double counting.
#'
#' ## Automatic proportion handling
#' The function automatically detects which population columns contain
#' proportions (all non-NA values <= 1) versus counts. Proportion columns
#' are aggregated using the mean, while count columns are summed. If a
#' column contains mixed data (some values <= 1 and some > 1), the function
#' will abort with an error message.
#'
#' @param pop_data data.frame/tibble with `adm0`, optional `adm1`/`adm2`/`adm3`,
#'   `year`, and population column(s). `year` may be integer, numeric, Date/POSIXt, factor,
#'   or character; it is coerced to integer years when feasible.
#' @param pop_cols character vector of population column names to process.
#'   Default is "pop" for backward compatibility.
#' @param translate logical; when TRUE, add a translated label column
#'   (default FR unless `language` is provided) using the translation cache.
#' @param language Optional ISO code (e.g., "fr"). When provided, a
#'   `label_<language>` column is added to the dictionary and placed
#'   immediately after `label_en`.
#' @param trans_cache_path character path to the translation cache directory.
#'   default: `here::here("cache/translations")` if available, else
#'   `"cache/translations"`.
#' @param infer_types logical; when TRUE (default) coerce `pop_data` first using
#'   `auto_parse_types(apply = TRUE, return = "data")`.
#'
#' @returns
#' named list with elements (only levels present are included):
#' - `pop_data_adm0`, `pop_data_adm1`, `pop_data_adm2`, `pop_data_adm3`
#'   (aggregated by level and year)
#' - `data_dictionary` (from [build_dictionary()])
#' - `levels_present` (character vector of admin levels detected)
#'
#' @examples
#' \dontrun{
#' # Basic usage with default "pop" column
#' example_pop <- tibble::tibble(
#'   adm0 = c("A", "A", "B"),
#'   adm1 = c("X", "X", "Y"),
#'   adm2 = c("P", "Q", "R"),
#'   year = c(2020L, 2020L, 2021L),
#'   pop  = c(100, 50, 70)
#' )
#' out <- snt_process_population(example_pop)
#' names(out)
#' out$levels_present
#'
#' # Using custom population column names
#' example_multi_pop <- tibble::tibble(
#'   adm0 = c("A", "A", "B"),
#'   adm1 = c("X", "X", "Y"),
#'   year = c(2020L, 2020L, 2021L),
#'   total_pop = c(100, 50, 70),
#'   under5_pop = c(20, 10, 15)
#' )
#' out_multi <- snt_process_population(
#'   example_multi_pop,
#'   pop_cols = c("total_pop", "under5_pop")
#' )
#' names(out_multi)
#'}
#' @export
snt_process_population <- function(
  pop_data,
  pop_cols = "pop",
  translate = TRUE,
  language = NULL,
  trans_cache_path = if (requireNamespace("here", quietly = TRUE)) {
    here::here("cache/translations")
  } else {
    "cache/translations"
  },
  infer_types = TRUE
) {
  # validate input shape early
  .validate_pop_input(pop_data, pop_cols)

  # optionally coerce types using the shared engine (preferred)
  if (isTRUE(infer_types)) {
    pop_data <- auto_parse_types(
      data = pop_data,
      apply = TRUE,
      return = "data"
    )
  } else {
    # fallback best-effort for year only when not using the engine
    pop_data <- .coerce_year_if_needed(pop_data)
  }

  # detect which columns are proportions vs counts
  prop_cols <- .detect_proportion_cols(pop_data, pop_cols)

  # detect available admin levels after any coercions
  lv_all <- c("adm0", "adm1", "adm2", "adm3")
  levels_present <- lv_all[lv_all %in% names(pop_data)]

  # check for duplicates at the finest granularity level in input data
  .check_uniqueness(pop_data, levels_present)

  # summarise for each level that truly exists
  pop_data_adm0 <- .summarise_by(pop_data, c("adm0"), pop_cols, prop_cols)
  pop_data_adm1 <- .summarise_by(pop_data, c("adm0", "adm1"), pop_cols, prop_cols)
  pop_data_adm2 <- .summarise_by(pop_data, c("adm0", "adm1", "adm2"), pop_cols, prop_cols)
  pop_data_adm3 <- .summarise_by(pop_data, c("adm0", "adm1", "adm2", "adm3"), pop_cols, prop_cols)

  # build compact dictionary for the columns we actually expose
  dict_vars <- unique(c(levels_present, "year", pop_cols))
  keep_cols <- intersect(names(pop_data), dict_vars)

  # determine target language: explicit `language` wins; otherwise use FR when
  # translate=TRUE; NULL -> English only
  target_lang <- if (!is.null(language) && nzchar(language)) {
    language
  } else if (isTRUE(translate)) {
    "fr"
  } else {
    NULL
  }

  dict <- build_dictionary(
    data = pop_data[, keep_cols, drop = FALSE],
    labels_path = getOption("snt.labels_en_path", NULL),
    language = target_lang,
    max_levels = 50L,
    n_examples = 3L
  )

  # assemble result with only present levels
  out <- list()
  if (!is.null(pop_data_adm0)) {
    out$pop_data_adm0 <- pop_data_adm0
  }
  if (!is.null(pop_data_adm1)) {
    out$pop_data_adm1 <- pop_data_adm1
  }
  if (!is.null(pop_data_adm2)) {
    out$pop_data_adm2 <- pop_data_adm2
  }
  if (!is.null(pop_data_adm3)) {
    out$pop_data_adm3 <- pop_data_adm3
  }
  out$data_dictionary <- dict
  out$levels_present <- levels_present
  out
}

# helpers -----------------------------------------------------------------

#' Conditional message
#'
#' Prints info if `snt.verbose` option is TRUE and `cli` is available.
#'
#' @param text Message to display.
#' @noRd
.msg <- function(text) {
  if (
    isTRUE(getOption("snt.verbose", FALSE)) &&
      requireNamespace("cli", quietly = TRUE)
  ) {
    cli::cli_alert_info(text)
  }
  invisible(NULL)
}

#' Check for duplicate rows at finest admin level
#'
#' Ensures population data has unique rows per admin-year at the finest
#' granularity level present. Aborts if duplicates are found.
#'
#' @param pop_data Data frame with population.
#' @param levels_present Character vector of admin levels in data.
#' @noRd
.check_uniqueness <- function(pop_data, levels_present) {
  if (length(levels_present) == 0 || !requireNamespace("dplyr", quietly = TRUE)) {
    return(invisible(NULL))
  }

  # identify finest admin level (last in hierarchy)
  finest_level <- levels_present[length(levels_present)]
  check_cols <- c(levels_present, "year")

  # count rows per admin-year combination at finest level
  dup <- pop_data |>
    dplyr::count(
      dplyr::across(dplyr::all_of(check_cols))
    ) |>
    dplyr::filter(.data$n > 1)

  if (nrow(dup) > 0) {
    example_key <- paste(
      names(dup)[1:(ncol(dup) - 1)],
      unlist(dup[1, 1:(ncol(dup) - 1)]),
      sep = "=",
      collapse = ", "
    )
    cli::cli_abort(
      c(
        "Population data has multiple rows per admin-year combination at {finest_level} level.",
        "i" = "Example duplicate: {example_key} (n={dup$n[1]} rows)",
        "x" = "This causes populations to be summed incorrectly.",
        ">" = "Common causes: sex disaggregation, age groups, residence strata.",
        ">" = "Ensure data is unique per admin-year at {finest_level} level or aggregate upstream."
      )
    )
  }

  invisible(NULL)
}

#' Grouped population summary
#'
#' Aggregates population by grouping columns and year. Uses mean for proportion
#' columns and sum for count columns.
#'
#' @param pop_data Data frame with population.
#' @param group_cols Character vector of grouping columns.
#' @param pop_cols Character vector of population column names.
#' @param prop_cols Character vector of columns that are proportions (will be averaged).
#' @return Data frame or NULL.
#' @noRd
.summarise_by <- function(pop_data, group_cols, pop_cols, prop_cols = character(0)) {
  if (!all(group_cols %in% names(pop_data))) {
    return(NULL)
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    cli::cli_abort("Package 'dplyr' is required for summarising.")
  }

  # create summarise expressions for each population column
  # use mean for proportions, sum for counts
  summarise_exprs <- rlang::set_names(
    lapply(pop_cols, function(col) {
      if (col %in% prop_cols) {
        rlang::expr(sntutils::mean2(.data[[!!col]]))
      } else {
        rlang::expr(sntutils::sum2(.data[[!!col]]))
      }
    }),
    pop_cols
  )

  dplyr::group_by(
    pop_data,
    dplyr::across(dplyr::all_of(group_cols)),
    .data$year
  ) |>
    dplyr::summarise(!!!summarise_exprs, .groups = "drop")
}

#' Cached translator factory
#'
#' Builds a translator function that uses project-level translation
#' (if available) with caching; otherwise returns a pass-through.
#'
#' @param cache_path Path for translation cache.
#' @return Function(text, target_language) → character.
#' @noRd
.translate_cached_factory <- function(cache_path) {
  fn <- get0("sntutils::translate_text_vec", mode = "function")
  if (is.function(fn)) {
    function(text, target_language) {
      fn(
        text = text,
        target_language = target_language,
        cache_path = cache_path
      )
    }
  } else {
    function(text, target_language) {
      as.character(text)
    }
  }
}

#' Validate minimal shape of population input
#'
#' Ensures required columns exist and population columns are numeric. Leaves `year`
#' validation/coercion to later steps.
#' @param pop_data data frame to validate
#' @param pop_cols character vector of population column names
#' @noRd
.validate_pop_input <- function(pop_data, pop_cols) {
  if (!is.data.frame(pop_data)) {
    cli::cli_abort("`pop_data` must be a data.frame or tibble.")
  }
  req <- c("adm0", "year", pop_cols)
  miss <- setdiff(req, names(pop_data))
  if (length(miss)) {
    cli::cli_abort("missing required columns: {toString(miss)}")
  }
  # check that all population columns are numeric
  for (col in pop_cols) {
    if (!is.numeric(pop_data[[col]])) {
      cli::cli_abort("`{col}` must be numeric.")
    }
  }
  invisible(TRUE)
}

#' Detect which population columns contain proportions
#'
#' Checks each population column to determine if it contains proportion data
#' (all non-NA values <= 1) or count data. Aborts if a column appears to be
#' a proportion but contains values > 1.
#'
#' @param pop_data data frame with population columns
#' @param pop_cols character vector of population column names to check
#' @return character vector of column names that are proportions
#' @noRd
.detect_proportion_cols <- function(pop_data, pop_cols) {
  prop_cols <- character(0)

  for (col in pop_cols) {
    values <- pop_data[[col]]
    non_na_values <- values[!is.na(values)]

    if (length(non_na_values) == 0) {
      next
    }

    all_lte_one <- all(non_na_values <= 1)
    has_fraction <- any(non_na_values < 1 & non_na_values > 0)
    has_large <- any(non_na_values > 1)

    if (all_lte_one) {
      prop_cols <- c(prop_cols, col)
    } else if (has_fraction && has_large) {
      max_val <- max(non_na_values, na.rm = TRUE)
      min_val <- min(non_na_values, na.rm = TRUE)
      cli::cli_abort(
        c(
          "Column `{col}` appears to contain mixed proportion and count data.",
          "i" = "Values range from {min_val} to {max_val}.",
          "x" = "Proportion columns must have all values <= 1."
        )
      )
    }
  }

  # log which columns are treated as proportions
  if (length(prop_cols) > 0) {
    .msg(paste0(
      "detected proportion columns (will use mean): ",
      toString(prop_cols)
    ))
  }

  prop_cols
}

#' Best-effort coercion of `year` to integer years
#'
#' Handles character/factor (direct integer or first 4-digit pattern),
#' Date/POSIXt (uses %Y). Errors if result is not numeric/integer.
#' @noRd
.coerce_year_if_needed <- function(pop_data) {
  y <- pop_data$year

  # factor -> character
  if (is.factor(y)) {
    y <- as.character(y)
  }

  if (is.character(y)) {
    # try plain integer first
    yi <- suppressWarnings(as.integer(y))

    # fill remaining with first 4-digit match if present
    need <- is.na(yi)
    if (any(need)) {
      m <- regexpr("\\d{4}", y[need], perl = TRUE)
      hit <- regmatches(y[need], m)
      hit[lengths(hit) == 0L] <- NA_character_
      yi[need] <- suppressWarnings(as.integer(hit))
    }
    pop_data$year <- yi
  } else if (inherits(y, "Date")) {
    pop_data$year <- as.integer(format(y, "%Y"))
  } else if (inherits(y, "POSIXt")) {
    pop_data$year <- as.integer(format(y, "%Y"))
  } else {
    # leave numeric/integer as is
    pop_data$year <- y
  }

  # final check
  if (!is.integer(pop_data$year) && !is.numeric(pop_data$year)) {
    cli::cli_abort("`year` must be numeric/integer after coercion.")
  }

  pop_data
}
