# helpers ----------------------------------------------------------------------

#' Guess a compact semantic type (internal)
#'
#' @description
#' Heuristically classifies a vector into one of: `geometry`, `datetime`,
#' `date`, `factor`, `logical`, `integer`, `double`, `character`, `list`,
#' or `other`.
#'
#' @param x A vector or list-column to classify.
#' @return A single-character type label.
#' @keywords internal
#' @noRd
.guess_type <- function(x) {
  cls <- class(x)
  if ("sfc" %in% cls || "sfg" %in% cls) {
    return("geometry")
  }
  if (inherits(x, "POSIXt")) {
    return("datetime")
  }
  if (inherits(x, "Date")) {
    return("date")
  }
  if (is.factor(x)) {
    return("factor")
  }
  if (is.logical(x)) {
    return("logical")
  }
  if (is.integer(x)) {
    return("integer")
  }
  if (is.double(x)) {
    if (all(is.na(x) | is.finite(x))) {
      ix <- x[is.finite(x)]
      if (
        length(ix) &&
        all(abs(ix - round(ix)) < .Machine$double.eps^0.5)
      ) {
        return("integer")
      }
    }
    return("double")
  }
  if (is.character(x)) {
    return("character")
  }
  if (is.list(x)) {
    return("list")
  }
  "other"
}

#' Safe unique count (internal)
#'
#' @description
#' Computes the number of unique values with an optional cap for very
#' large vectors by sampling, attaching an attribute `cap_reached`.
#'
#' @param x A vector.
#' @param cap Maximum elements to inspect exactly before sampling.
#' @return Integer count; attr `cap_reached` indicates approximation.
#' @keywords internal
#' @noRd
.nunique <- function(x, cap = 10000L) {
  n <- length(x)
  if (n <= cap) {
    out <- length(unique(x))
    attr(out, "cap_reached") <- FALSE
    return(out)
  }
  set.seed(1)
  s <- sample.int(n, cap)
  ub <- length(unique(x[s]))
  attr(ub, "cap_reached") <- TRUE
  ub
}

#' Format top example values (internal)
#'
#' @description
#' Produces a concise string of representative example values. For text
#' columns, shows the most frequent `n` values; for other types, shows the
#' first distinct values. Truncates to `width` characters.
#'
#' @param x A vector.
#' @param n Number of examples to include.
#' @param width Maximum character width of the output string.
#' @return A single character string of example values.
#' @keywords internal
#' @noRd
.examples <- function(x, n = 3L, width = 60L) {
  x <- x[!is.na(x)]
  if (!length(x)) return("")

  if (is.factor(x) || is.character(x)) {
    tab <- sort(table(x), decreasing = TRUE)
    vals <- names(tab)[seq_len(min(n, length(tab)))]
  } else {
    vals <- unique(utils::head(x, n))
  }

  out <- paste(utils::head(as.character(vals), n), collapse = ", ")

  if (is.finite(width) && width > 0L && nchar(out) > width) {
    ell <- "..."
    cut <- max(0L, width - nchar(ell))
    out <- paste0(substr(out, 1L, cut), ell)
  }

  out
}

#' Numeric/date range as strings (internal)
#'
#' @description
#' Computes min and max of non-missing values and returns character
#' representations suitable for compact display.
#'
#' @param x A numeric, Date, or POSIXt vector.
#' @return Character length-2 vector: c(min, max); empty strings if none.
#' @keywords internal
#' @noRd
.range_str <- function(x) {
  x2 <- x[!is.na(x)]
  if (!length(x2)) {
    return(c("", ""))
  }
  rng <- range(x2)
  c(as.character(rng[1L]), as.character(rng[2L]))
}

#' Read a name/label mapping from CSV
#'
#' This helper loads a simple two-column CSV containing variable names and
#' their human-readable labels. It searches for flexible column names:
#' - name column: one of "name", "variable", "key", "column"
#' - label column: one of "label", "label_en", "english"
#'
#' Duplicate variable names keep the last occurrence (later rows override).
#' Blank or NA names are dropped. The returned object is a named character
#' vector mapping variable -> label.
#'
#' @param path Character scalar. Path to CSV file. If NULL, missing, or
#'   unreadable, returns NULL.
#'
#' @return Named character vector mapping variable names to labels,
#'   or NULL on error/invalid file.
#'
#' @examples
#' tmp <- tempfile(fileext = ".csv")
#' write.csv(
#'   data.frame(name = c("id","grp"), label = c("Identifier","Group")),
#'   tmp, row.names = FALSE
#' )
#' .read_label_map_csv(tmp)
#'
#' @noRd
.read_label_map_csv <- function(path) {
  if (is.null(path) || !nzchar(path) || !file.exists(path)) {
    return(NULL)
  }
  # Avoid attempting to read directories; quietly return NULL
  finfo <- tryCatch(utils::file_test("-d", path), error = function(e) NA)
  if (isTRUE(finfo)) return(NULL)
  tryCatch(
    {
      df <- utils::read.csv(
        path,
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
      name_col <- intersect(names(df), c("name", "variable", "key", "column"))
      label_col <- intersect(names(df), c("label", "label_en", "english"))
      if (length(name_col) == 0L || length(label_col) == 0L) {
        return(NULL)
      }
      nm <- trimws(df[[name_col[1L]]])
      lb <- as.character(df[[label_col[1L]]])
      if (anyNA(nm) || any(nm == "")) {
        return(NULL)
      }
      if (any(duplicated(nm))) {
        keep <- !duplicated(nm, fromLast = TRUE)
        nm <- nm[keep]
        lb <- lb[keep]
      }
      stats::setNames(lb, nm)
    },
    error = function(e) NULL
  )
}

# main -------------------------------------------------------------------------

#' Build a compact data dictionary
#'
#' @description
#' Create a tidy dictionary from a data.frame (sf ok). Infers a simple
#' type per column, adds English labels (merged with an on-disk CSV map
#' when available), and reports small stats: missing %, unique count,
#' a few example values, and min/max for numeric/date/datetime. Can add
#' one translated label column via a user function, or mirror English
#' labels when a translator is not provided.
#'
#' @param data A data.frame/tibble; sf columns are detected.
#' @param labels_path Optional CSV with columns `name,label` used to
#'   override/add English labels. If missing, only internal defaults are
#'   used.
#' @param language Optional ISO code (e.g., "fr"). If set, a
#'   `label_<language>` column is added; values are translated using
#'   an internal translator when available, otherwise mirrored from English.
#' @param max_levels Max factor levels to summarize in notes (default 50).
#' @param n_examples Number of example values to show (default 3).
#'
#' @return A tibble with at least:
#'   - variable, type, label_en
#'   - n, n_missing, pct_missing, n_unique
#'   - example_values, min, max, notes
#'   If `language` is set and not "en", also `label_<language>`.
#'
#' @details
#' English labels are formed by merging internal defaults with the CSV
#' at `labels_path`. CSV entries win on name conflicts. Unknown columns
#' fall back to their original names. When `language` is provided but
#' no translator is available, the translated label column mirrors the
#' English labels.
#'
#' @examples
#' dd <- build_dictionary(iris)
#' head(dd[c("variable", "type", "label_en")])
#' @export
build_dictionary <- function(
  data,
  labels_path = getOption("snt.labels_en_path", NULL),
  language = NULL,
  max_levels = 50L,
  n_examples = 3L
) {
  # special case: if caller passes a pre-built dictionary and a file path
  # as second argument (historical calling style), write it to disk
  if (
    is.data.frame(data) &&
    all(c("variable", "type", "label_en") %in% names(data)) &&
    is.character(labels_path) && length(labels_path) == 1L && nzchar(labels_path) &&
    grepl("\\.(csv|xlsx)$", labels_path, ignore.case = TRUE)
  ) {
    out_path <- labels_path
    ext <- tolower(tools::file_ext(out_path))
    if (identical(ext, "csv")) {
      utils::write.csv(
        data,
        file = out_path,
        row.names = FALSE,
        fileEncoding = "UTF-8"
      )
    } else if (identical(ext, "xlsx")) {
      if (!requireNamespace("openxlsx", quietly = TRUE)) {
        stop("openxlsx package is required to write .xlsx files", call. = FALSE)
      }
      openxlsx::write.xlsx(data, file = out_path)
    } else {
      stop("Unsupported file extension: ", ext, call. = FALSE)
    }
    return(invisible(out_path))
  }

  stopifnot(!is.null(data))
  vars <- names(data)
  if (is.null(vars)) {
    vars <- character(0)
  }

  # 1) get internal defaults ONLY (do not pass labels_path here)
  get_map <- get0("get_column_label_map_en", mode = "function")
  base_map <- if (is.function(get_map)) {
    tryCatch(get_map(NULL), error = function(e) character(0))
  } else {
    character(0)
  }

  # 2) overlay explicit CSV map provided by caller
  file_map <- .read_label_map_csv(labels_path)
  if (!is.null(file_map) && length(file_map)) {
    if (!length(base_map)) {
      lbl_map <- file_map
    } else {
      lbl_map <- base_map
      lbl_map[names(file_map)] <- unname(file_map)
    }
  } else {
    lbl_map <- base_map
  }

  prof_one <- function(col, nm) {
    tp <- .guess_type(col)
    n <- length(col)
    nmis <- sum(is.na(col))
    pmis <- if (n) round(100 * nmis / n, 2) else 0
    nu <- tryCatch(.nunique(col), error = function(e) NA_integer_)
    ex <- tryCatch(.examples(col, n_examples), error = function(e) "")
    rng <- c("", "")
    notes <- ""

    if (tp %in% c("integer", "double", "date", "datetime")) {
      rng <- tryCatch(.range_str(col), error = function(e) c("", ""))
    }
    if (tp == "factor") {
      lv <- levels(col)
      notes <- if (length(lv) > max_levels) {
        paste0("levels: ", max_levels, "+")
      } else {
        paste0("levels: ", length(lv))
      }
    }
    if (tp == "list") {
      subcls <- tryCatch(
        unique(vapply(col, function(z) class(z)[1L], "")),
        error = function(e) character(0)
      )
      if (length(subcls) > 3L) {
        subcls <- c(subcls[1:3], "...")
      }
      notes <- paste0("list of: ", paste(subcls, collapse = ", "))
    }
    if (tp == "geometry") {
      crs <- tryCatch(
        if (inherits(col, "sfc")) sf::st_crs(col)$input else NA_character_,
        error = function(e) NA_character_
      )
      gcl <- tryCatch(
        paste(
          unique(vapply(col, function(z) class(z)[1L], "")),
          collapse = ", "
        ),
        error = function(e) "sfc"
      )
      notes <- paste0("geom: ", gcl, if (!is.na(crs)) paste0(" | crs=", crs))
      ex <- ""
      rng <- c("", "")
      nu <- NA_integer_
    }

    list(
      variable = nm,
      type = tp,
      n = n,
      n_missing = nmis,
      pct_missing = pmis,
      n_unique = nu,
      example_values = ex,
      min = rng[1L],
      max = rng[2L],
      notes = notes
    )
  }

  rows <- lapply(seq_along(vars), function(i) prof_one(data[[i]], vars[[i]]))
  dict <- tibble::as_tibble(do.call(rbind, lapply(rows, as.data.frame)))

  # 3) attach English labels; fall back to name when missing/blank
  eng <- unname(lbl_map[dict$variable])
  miss <- is.na(eng) | !nzchar(eng)
  if (any(miss)) {
    eng[miss] <- dict$variable[miss]
  }
  dict$label_en <- eng

  # 4) optional translation
  if (!is.null(language) && nzchar(language)) {
    trg <- tolower(language)
    if (!identical(trg, "en")) {
      coln <- paste0("label_", trg)
      # Try to use package's vectorized translator if available
      tr_fn <- get0("translate_text_vec", mode = "function")
      if (is.function(tr_fn)) {
        dict[[coln]] <- tryCatch(
          tr_fn(dict$label_en, target_language = trg),
          error = function(e) dict$label_en
        )
      } else {
        dict[[coln]] <- dict$label_en
      }
    }
  }

  # Ensure translated label columns appear immediately after label_en
  all_cols <- names(dict)
  label_cols <- grep("^label_", all_cols, value = TRUE)
  extra_labels <- setdiff(label_cols, "label_en")
  rest <- setdiff(all_cols, c("variable", "type", label_cols))
  dict <- dict[, c("variable", "type", "label_en", extra_labels, rest)]

  dict
}
