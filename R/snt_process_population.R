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
#'
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

  # detect available admin levels after any coercions
  lv_all <- c("adm0", "adm1", "adm2", "adm3")
  levels_present <- lv_all[lv_all %in% names(pop_data)]

  # summarise for each level that truly exists
  pop_data_adm0 <- .summarise_by(pop_data, c("adm0"), pop_cols)
  pop_data_adm1 <- .summarise_by(pop_data, c("adm0", "adm1"), pop_cols)
  pop_data_adm2 <- .summarise_by(pop_data, c("adm0", "adm1", "adm2"), pop_cols)
  pop_data_adm3 <- .summarise_by(pop_data, c("adm0", "adm1", "adm2", "adm3"), pop_cols)

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

#' Grouped population summary
#'
#' Aggregates population by grouping columns and year.
#' Returns `NULL` if grouping columns are missing.
#'
#' @param pop_data Data frame with population.
#' @param group_cols Character vector of grouping columns.
#' @param pop_cols Character vector of population column names.
#' @return Data frame or NULL.
#' @noRd
.summarise_by <- function(pop_data, group_cols, pop_cols) {
  if (!all(group_cols %in% names(pop_data))) {
    return(NULL)
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    cli::cli_abort("Package 'dplyr' is required for summarising.")
  }
  
  # create summarise expressions for each population column
  summarise_exprs <- rlang::set_names(
    lapply(pop_cols, function(col) {
      rlang::expr(sntutils::sum2(.data[[!!col]]))
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
