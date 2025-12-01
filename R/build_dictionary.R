# helpers ----------------------------------------------------------------------

#' guess a compact semantic type (internal)
#'
#' @description
#' classify a vector into one of:
#' `geometry`, `datetime`, `date`, `factor`, `logical`, `integer`,
#' `double`, `character`, `list`, or `other`.
#'
#' @param x vector or list-column to classify.
#'
#' @return single-character type label.
#'
#' @keywords internal
#' @noRd
.guess_type <- function(x) {
  # get top-level class
  cls <- base::class(x)

  # use switch-on-TRUE to avoid nested ifs
  out <- dplyr::case_when(
    "sfc" %in% cls | "sfg" %in% cls ~ "geometry",
    base::inherits(x, "POSIXt") ~ "datetime",
    base::inherits(x, "Date") ~ "date",
    base::is.factor(x) ~ "factor",
    base::is.logical(x) ~ "logical",
    base::is.integer(x) ~ "integer",
    base::is.double(x) ~ NA_character_,  # inspect doubles below
    base::is.character(x) ~ "character",
    base::is.list(x) ~ "list",
    TRUE ~ "other"
  )

  # inspect doubles to detect integer-like values
  if (base::is.na(out) && base::is.double(x)) {
    # keep finite values only
    finite_x <- x[base::is.finite(x)]

    # check if all finite values are near integers
    is_int_like <- base::length(finite_x) > 0 &&
      base::all(base::abs(finite_x - base::round(finite_x)) <
        .Machine$double.eps^0.5)

    # return integer if int-like, else double
    return(if (is_int_like) "integer" else "double")
  }

  # return decided label
  rlang::`%||%`(out, "other")
}

#' safe unique count (internal)
#'
#' @description
#' compute number of unique values with an optional cap for very large
#' vectors by sampling. attach attribute `cap_reached` when approximated.
#'
#' @param x vector.
#' @param cap maximum elements to inspect exactly before sampling.
#'
#' @return integer count with attr `cap_reached` (logical).
#'
#' @keywords internal
#' @noRd
.nunique <- function(x, cap = 10000L) {
  # validate cap
  if (!base::is.numeric(cap) || cap < 1L) {
    cli::cli_abort("{.arg cap} must be a positive integer.")
  }

  # get length
  n <- base::length(x)

  # exact path when small
  if (n <= cap) {
    out <- base::length(base::unique(x))
    base::attr(out, "cap_reached") <- FALSE
    return(out)
  }

  # approximate path: sample indices without fixing global seed
  s <- base::sample.int(n, cap)
  ub <- base::length(base::unique(x[s]))
  base::attr(ub, "cap_reached") <- TRUE
  ub
}

#' format top example values (internal)
#'
#' @description
#' build a concise string of representative example values. for text, show
#' most frequent `n`. for other types, show first distinct values. truncate
#' to `width` characters.
#'
#' @param x vector.
#' @param n number of examples to include.
#' @param width max character width of the output string.
#'
#' @return single character string of example values.
#'
#' @keywords internal
#' @noRd
.examples <- function(x, n = 3L, width = 60L) {
  # drop missing
  x <- x[!base::is.na(x)]

  # return empty when no data
  if (!base::length(x)) {
    return("")
  }

  # select values depending on type
  vals <- if (base::is.factor(x) || base::is.character(x)) {
    # compute frequency table
    tab <- base::sort(base::table(x), decreasing = TRUE)

    # take top-n names
    base::names(tab)[base::seq_len(base::min(n, base::length(tab)))]
  } else {
    # take first distinct values
    base::unique(utils::head(x, n))
  }

  # collapse into a string
  out <- base::paste(
    utils::head(base::as.character(vals), n),
    collapse = ", "
  )

  # enforce width limit
  if (base::is.finite(width) && width > 0L && base::nchar(out) > width) {
    ell <- "..."
    cut <- base::max(0L, width - base::nchar(ell))
    out <- base::paste0(base::substr(out, 1L, cut), ell)
  }

  out
}

#' numeric/date range as strings (internal)
#'
#' @description
#' compute min and max of non-missing values and return character versions.
#'
#' @param x numeric, Date, or POSIXt vector.
#'
#' @return length-2 character vector: c(min, max); empty strings if none.
#'
#' @keywords internal
#' @noRd
.range_str <- function(x) {
  # drop missing
  x2 <- x[!base::is.na(x)]

  # handle empty
  if (!base::length(x2)) {
    return(c("", ""))
  }

  # compute range
  rng <- base::range(x2)

  # return as character
  c(base::as.character(rng[1L]), base::as.character(rng[2L]))
}

#' read a name/label mapping from csv (internal)
#'
#' @description
#' load a two-column csv of variable names and their labels. name column is
#' matched from one of {"name","variable","key","column"}. label column is
#' matched from {"label","label_en","english"}. later duplicates win.
#'
#' @param path character scalar path to csv file.
#'
#' @return named character vector (variable -> label), or NULL on error.
#'
#' @examples
#' tmp <- tempfile(fileext = ".csv")
#' utils::write.csv(
#'   data.frame(name = c("id", "grp"), label = c("Identifier", "Group")),
#'   tmp,
#'   row.names = FALSE
#' )
#' .read_label_map_csv(tmp)
#'
#' @keywords internal
#' @noRd
.read_label_map_csv <- function(path) {
  # handle null or empty path
  if (base::is.null(path) || !base::nzchar(path)) {
    return(NULL)
  }

  # path must exist and not be a directory
  if (!base::file.exists(path) ||
      isTRUE(tryCatch(utils::file_test("-d", path),
                      error = function(e) NA))) {
    return(NULL)
  }

  # try to read csv
  out <- tryCatch(
    {
      # read raw csv without altering names
      df <- utils::read.csv(
        path,
        stringsAsFactors = FALSE,
        check.names = FALSE
      )

      # detect columns
      name_col <- base::intersect(
        base::names(df),
        c("name", "variable", "key", "column")
      )
      label_col <- base::intersect(
        base::names(df),
        c("label", "label_en", "english")
      )

      # require both
      if (!base::length(name_col) || !base::length(label_col)) {
        return(NULL)
      }

      # extract and trim
      nm <- base::trimws(df[[name_col[1L]]])
      lb <- base::as.character(df[[label_col[1L]]])

      # drop blanks or NA names
      keep_name <- !base::is.na(nm) & base::nzchar(nm)
      nm <- nm[keep_name]
      lb <- lb[keep_name]

      # keep last occurrence on duplicates
      if (base::any(base::duplicated(nm))) {
        keep <- !base::duplicated(nm, fromLast = TRUE)
        nm <- nm[keep]
        lb <- lb[keep]
      }

      # return named vector
      stats::setNames(lb, nm)
    },
    error = function(e) NULL
  )

  out
}

# helpers ----------------------------------------------------------------------

#' guess a compact semantic type (internal)
#'
#' @description
#' classify a vector into one of:
#' `geometry`, `datetime`, `date`, `factor`, `logical`, `integer`,
#' `double`, `character`, `list`, or `other`.
#'
#' @param x vector or list-column to classify.
#'
#' @return single-character type label.
#'
#' @keywords internal
#' @noRd
.guess_type <- function(x) {
  # get top-level class
  cls <- base::class(x)

  # use switch-on-TRUE to avoid nested ifs
  out <- dplyr::case_when(
    "sfc" %in% cls | "sfg" %in% cls ~ "geometry",
    base::inherits(x, "POSIXt") ~ "datetime",
    base::inherits(x, "Date") ~ "date",
    base::is.factor(x) ~ "factor",
    base::is.logical(x) ~ "logical",
    base::is.integer(x) ~ "integer",
    base::is.double(x) ~ NA_character_, # inspect doubles below
    base::is.character(x) ~ "character",
    base::is.list(x) ~ "list",
    TRUE ~ "other"
  )

  # inspect doubles to detect integer-like values
  if (base::is.na(out) && base::is.double(x)) {
    # keep finite values only
    finite_x <- x[base::is.finite(x)]

    # check if all finite values are near integers
    is_int_like <- base::length(finite_x) > 0 &&
      base::all(
        base::abs(finite_x - base::round(finite_x)) < .Machine$double.eps^0.5
      )

    # return integer if int-like, else double
    return(if (is_int_like) "integer" else "double")
  }

  # return decided label
  rlang::`%||%`(out, "other")
}

#' safe unique count (internal)
#'
#' @description
#' compute number of unique values with an optional cap for very large
#' vectors by sampling. attach attribute `cap_reached` when approximated.
#'
#' @param x vector.
#' @param cap maximum elements to inspect exactly before sampling.
#'
#' @return integer count with attr `cap_reached` (logical).
#'
#' @keywords internal
#' @noRd
.nunique <- function(x, cap = 10000L) {
  # validate cap
  if (!base::is.numeric(cap) || cap < 1L) {
    cli::cli_abort("{.arg cap} must be a positive integer.")
  }

  # get length
  n <- base::length(x)

  # exact path when small
  if (n <= cap) {
    out <- base::length(base::unique(x))
    base::attr(out, "cap_reached") <- FALSE
    return(out)
  }

  # approximate path: sample indices without fixing global seed
  s <- base::sample.int(n, cap)
  ub <- base::length(base::unique(x[s]))
  base::attr(ub, "cap_reached") <- TRUE
  ub
}

#' format top example values (internal)
#'
#' @description
#' build a concise string of representative example values. for text, show
#' most frequent `n`. for other types, show first distinct values. truncate
#' to `width` characters.
#'
#' @param x vector.
#' @param n number of examples to include.
#' @param width max character width of the output string.
#'
#' @return single character string of example values.
#'
#' @keywords internal
#' @noRd
.examples <- function(x, n = 3L, width = 60L) {
  # drop missing
  x <- x[!base::is.na(x)]

  # return empty when no data
  if (!base::length(x)) {
    return("")
  }

  # select values depending on type
  vals <- if (base::is.factor(x) || base::is.character(x)) {
    # compute frequency table
    tab <- base::sort(base::table(x), decreasing = TRUE)

    # take top-n names
    base::names(tab)[base::seq_len(base::min(n, base::length(tab)))]
  } else {
    # take first distinct values
    base::unique(utils::head(x, n))
  }

  # collapse into a string
  out <- base::paste(
    utils::head(base::as.character(vals), n),
    collapse = ", "
  )

  # enforce width limit
  if (base::is.finite(width) && width > 0L && base::nchar(out) > width) {
    ell <- "..."
    cut <- base::max(0L, width - base::nchar(ell))
    out <- base::paste0(base::substr(out, 1L, cut), ell)
  }

  out
}

#' numeric/date range as strings (internal)
#'
#' @description
#' compute min and max of non-missing values and return character versions.
#'
#' @param x numeric, Date, or POSIXt vector.
#'
#' @return length-2 character vector: c(min, max); empty strings if none.
#'
#' @keywords internal
#' @noRd
.range_str <- function(x) {
  # drop missing
  x2 <- x[!base::is.na(x)]

  # handle empty
  if (!base::length(x2)) {
    return(c("", ""))
  }

  # compute range
  rng <- base::range(x2)

  # return as character
  c(base::as.character(rng[1L]), base::as.character(rng[2L]))
}

#' read a name/label mapping from csv (internal)
#'
#' @description
#' load a two-column csv of variable names and their labels. name column is
#' matched from one of {"name","variable","key","column"}. label column is
#' matched from {"label","label_en","english"}. later duplicates win.
#'
#' @param path character scalar path to csv file.
#'
#' @return named character vector (variable -> label), or NULL on error.
#'
#' @examples
#' tmp <- tempfile(fileext = ".csv")
#' utils::write.csv(
#'   data.frame(name = c("id", "grp"), label = c("Identifier", "Group")),
#'   tmp,
#'   row.names = FALSE
#' )
#' .read_label_map_csv(tmp)
#'
#' @keywords internal
#' @noRd
.read_label_map_csv <- function(path) {
  # handle null or empty path
  if (base::is.null(path) || !base::nzchar(path)) {
    return(NULL)
  }

  # path must exist and not be a directory
  if (
    !base::file.exists(path) ||
      isTRUE(tryCatch(utils::file_test("-d", path), error = function(e) NA))
  ) {
    return(NULL)
  }

  # try to read csv
  out <- tryCatch(
    {
      # read raw csv without altering names
      df <- utils::read.csv(
        path,
        stringsAsFactors = FALSE,
        check.names = FALSE
      )

      # detect columns
      name_col <- base::intersect(
        base::names(df),
        c("name", "variable", "key", "column")
      )
      label_col <- base::intersect(
        base::names(df),
        c("label", "label_en", "english")
      )

      # require both
      if (!base::length(name_col) || !base::length(label_col)) {
        return(NULL)
      }

      # extract and trim
      nm <- base::trimws(df[[name_col[1L]]])
      lb <- base::as.character(df[[label_col[1L]]])

      # drop blanks or NA names
      keep_name <- !base::is.na(nm) & base::nzchar(nm)
      nm <- nm[keep_name]
      lb <- lb[keep_name]

      # keep last occurrence on duplicates
      if (base::any(base::duplicated(nm))) {
        keep <- !base::duplicated(nm, fromLast = TRUE)
        nm <- nm[keep]
        lb <- lb[keep]
      }

      # return named vector
      stats::setNames(lb, nm)
    },
    error = function(e) NULL
  )

  out
}

#' match SNT variables with multilingual labels (internal)
#'
#' @description
#' matches variable names against the snt_var_tree dataset using vectorized
#' exact match first, then batch token structure detection for unmatched.
#'
#' @param vars character vector of variable names
#' @param target_lang target language ("en", "fr", "pt")
#' @param tree_data optional snt_var_tree data object (unused, for compatibility)
#'
#' @return tibble with variable and label columns
#' @noRd
.match_snt_labels <- function(vars,
                              target_lang = "en",
                              tree_data = NULL) {
  # get cached lookup vectors for all languages
  lookup_en <- .get_lookup_vector_cached("en")
  lookup_fr <- .get_lookup_vector_cached("fr")
  lookup_pt <- .get_lookup_vector_cached("pt")

  # vectorized exact match
  results <- tibble::tibble(
    variable = vars,
    label_en = lookup_en[vars],
    label_fr = lookup_fr[vars],
    label_pt = lookup_pt[vars]
  )

  # find unmatched variables with underscores (potential SNT vars)
  unmatched_idx <- base::which(
    base::is.na(results$label_en) & base::grepl("_", vars)
  )

  if (base::length(unmatched_idx) > 0) {
    # batch process unmatched variables
    unmatched_vars <- vars[unmatched_idx]

    # use cached flat tree and schema
    flat_tree <- .get_flat_tree_cached()
    schema <- .get_schema_cached()

    for (i in base::seq_along(unmatched_idx)) {
      idx <- unmatched_idx[i]
      nm <- unmatched_vars[i]

      # try check_snt_var with pre-loaded data
      det <- tryCatch(
        check_snt_var(
          nm,
          return = TRUE,
          schema = schema,
          var_tree = list(schema = schema, flat_tree = flat_tree)
        ),
        error = function(e) NULL
      )

      if (!base::is.null(det)) {
        results$label_en[idx] <- det$label_en
        results$label_fr[idx] <- det$label_fr
        if ("label_pt" %in% base::names(det)) {
          results$label_pt[idx] <- det$label_pt
        }
      }
    }
  }

  # select target language
  lang_col <- base::paste0("label_", target_lang)
  if (!lang_col %in% base::names(results)) {
    lang_col <- "label_en"
  }

  results$label <- results[[lang_col]]
  dplyr::select(results, variable, label)
}

# main -------------------------------------------------------------------------

#' build a compact data dictionary
#'
#' @description
#' create a tidy dictionary from a data.frame (sf supported). infer a
#' simple type per column, attach english labels (optionally overridden
#' by a csv map), and report stats: missing %, unique count, example
#' values, and min/max for numeric/date/datetime. optionally add a
#' translated label column.
#'
#' @param data data.frame or tibble; sf columns allowed.
#' @param labels_path optional csv with columns like `name,label` to
#'   override english labels.
#' @param language optional iso code (e.g., "fr") to add `label_<language>`.
#' @param max_levels max factor levels to summarize in notes. default 50.
#' @param n_examples number of example values to show. default 3.
#' @param trans_cache_path optional cache dir for translate_text_vec().
#' @param override_yaml logical; if TRUE, CSV labels override YAML labels.
#'   default FALSE (YAML takes precedence).
#'
#' @return tibble with:
#'   variable, type, label_en, n, n_missing, pct_missing, n_unique,
#'   example_values, min, max, notes, and optionally `label_<language>`.
#'
#' @details
#' english labels are merged as: internal defaults, then csv overrides.
#' unknown variables fall back to their column name.
#'
#' performance: the snt variable tree is cached in a package environment on
#' first use. subsequent calls reuse the flattened tree. the cache automatically
#' refreshes when the tree version changes (tracked via _meta$last_updated).
#'
#' @examples
#' dd <- build_dictionary(dplyr::as_tibble(iris))
#' dd |> dplyr::select(variable, type, label_en) |> utils::head()
#'
#' @export
build_dictionary <- function(
  data,
  labels_path = base::getOption("snt.labels_en_path", NULL),
  language = NULL,
  max_levels = 50L,
  n_examples = 3L,
  trans_cache_path = NULL,
  override_yaml = FALSE
) {
  # validate input (sf objects inherit from data.frame)
  if (!base::inherits(data, "data.frame") && !base::inherits(data, "sf")) {
    cli::cli_abort("`data` must be a data.frame, tibble, or sf object.")
  }
  if (!base::is.null(language) && !base::is.character(language)) {
    cli::cli_abort("`language` must be NULL or a character scalar.")
  }

  # writing mode: if `data` already looks like a dictionary and labels_path
  # is a file path ending in .csv or .xlsx, write it and return invisibly.
  if (
    base::all(c("variable", "type", "label_en") %in% base::names(data)) &&
    base::is.character(labels_path) && base::length(labels_path) == 1L &&
    base::nzchar(labels_path) &&
    base::grepl("\\.(csv|xlsx)$", labels_path, ignore.case = TRUE)
  ) {
    out_path <- labels_path
    ext <- base::tolower(tools::file_ext(out_path))
    if (base::identical(ext, "csv")) {
      utils::write.csv(
        data,
        file = out_path,
        row.names = FALSE,
        fileEncoding = "UTF-8"
      )
      return(base::invisible(out_path))
    }
    if (base::identical(ext, "xlsx")) {
      if (!requireNamespace("openxlsx", quietly = TRUE)) {
        stop("openxlsx package is required to write .xlsx files", call. = FALSE)
      }
      openxlsx::write.xlsx(data, file = out_path)
      return(base::invisible(out_path))
    }
    stop("Unsupported file extension: ", ext, call. = FALSE)
  }

  # capture names (handle NULL)
  vars <- rlang::`%||%`(base::names(data), character(0))

  # get internal defaults (function optional in calling env)
  get_map <- base::get0("get_column_label_map_en", mode = "function")
  base_map <- if (base::is.function(get_map)) {
    tryCatch(get_map(NULL), error = function(e) character(0))
  } else {
    character(0)
  }

  # load external map from csv and overlay
  file_map <- .read_label_map_csv(labels_path)
  lbl_map <- base_map
  if (!base::is.null(file_map) && base::length(file_map)) {
    lbl_map[base::names(file_map)] <- base::unname(file_map)
  }

  # profile a single column
  prof_one <- function(col, nm) {
    # infer simple type
    tp <- .guess_type(col)

    # counts and pct missing
    n <- base::length(col)
    n_missing <- base::sum(base::is.na(col))
    pct_missing <- if (n) base::round(100 * n_missing / n, 2) else 0

    # unique count (possibly capped)
    n_unique <- tryCatch(.nunique(col), error = function(e) NA_integer_)

    # example values
    example_values <- tryCatch(
      .examples(col, n_examples),
      error = function(e) ""
    )

    # ranges for numeric/date/datetime
    minmax <- if (tp %in% c("integer", "double", "date", "datetime")) {
      tryCatch(.range_str(col), error = function(e) c("", ""))
    } else {
      c("", "")
    }

    # notes for factor, list, geometry
    notes <- ""
    if (base::identical(tp, "factor")) {
      # count levels with cap reporting
      n_lv <- base::length(base::levels(col))
      notes <- if (n_lv > max_levels) {
        base::paste0("levels: ", max_levels, "+")
      } else {
        base::paste0("levels: ", n_lv)
      }
    } else if (base::identical(tp, "list")) {
      # summarize first-level classes inside list
      subcls <- tryCatch(
        base::unique(base::vapply(col, function(z) base::class(z)[1L], "")),
        error = function(e) character(0)
      )
      if (base::length(subcls) > 3L) {
        subcls <- c(subcls[1:3], "...")
      }
      notes <- base::paste0("list of: ", base::paste(subcls, collapse = ", "))
    } else if (base::identical(tp, "geometry")) {
      # summarize geometry type and crs when available
      crs <- tryCatch(
        if (base::inherits(col, "sfc")) {
          sf::st_crs(col)$input
        } else {
          NA_character_
        },
        error = function(e) NA_character_
      )
      gcl <- tryCatch(
        base::paste(
          base::unique(base::vapply(col, function(z) base::class(z)[1L], "")),
          collapse = ", "
        ),
        error = function(e) "sfc"
      )
      notes <- base::paste0(
        "geom: ",
        gcl,
        if (!base::is.na(crs)) base::paste0(" | crs=", crs) else ""
      )
      # suppress range and examples for geometry
      example_values <- ""
      minmax <- c("", "")
      n_unique <- NA_integer_
    }

    # return row list
    list(
      variable = nm,
      type = tp,
      n = n,
      n_missing = n_missing,
      pct_missing = pct_missing,
      n_unique = n_unique,
      example_values = example_values,
      min = minmax[1L],
      max = minmax[2L],
      notes = notes
    )
  }

  # iterate over columns
  rows <- base::lapply(
    base::seq_along(vars),
    function(i) prof_one(data[[i]], vars[[i]])
  )

  # bind rows into tibble
  dict <- tibble::as_tibble(
    base::do.call(base::rbind, base::lapply(rows, base::as.data.frame))
  )

  # step 1: YAML-based SNT auto-labelling
  snt_lbls <- .match_snt_labels(dict$variable, target_lang = "en")
  dict <- dplyr::left_join(dict, snt_lbls, by = "variable") |>
    dplyr::rename(label_yaml = label)

  # step 2: combine with user-provided or base map labels
  label_en <- base::unname(lbl_map[dict$variable])

  if (override_yaml) {
    # CSV takes precedence over YAML
    dict$label_en <- dplyr::coalesce(
      label_en,         # CSV first
      dict$label_yaml,  # then YAML
      dict$variable     # fallback to variable name
    )
  } else {
    # YAML takes precedence over CSV (default)
    dict$label_en <- dplyr::coalesce(
      dict$label_yaml,  # YAML first
      label_en,         # then CSV
      dict$variable     # fallback to variable name
    )
  }

  dict$label_yaml <- NULL  # remove temp column

  # optional translation
  if (!base::is.null(language) && base::nzchar(language)) {
    # normalize code
    trg <- base::tolower(language)

    # only add when not english
    if (!base::identical(trg, "en")) {
      # set column name
      coln <- base::paste0("label_", trg)

      # use package translator if present (resolve from package namespace)
      tr_fn <- tryCatch(
        utils::getFromNamespace("translate_text_vec", "sntutils"),
        error = function(e) NULL
      )

      # translate unique english labels in small chunks
      # on chunk error, fall back per‑item; keep others intact
      if (base::is.function(tr_fn)) {
        # deduplicate english labels to reduce calls
        en_labels <- base::as.character(dict$label_en)
        uniq_labels <- base::unique(en_labels)

        # storage for translated values keyed by english label
        translated_map <- stats::setNames(
          object = base::rep(NA_character_, base::length(uniq_labels)),
          nm = uniq_labels
        )

        # chunk parameters (small by default to reduce rate limits)
        chunk_size <- 25L
        starts <- base::seq.int(1L, base::length(uniq_labels), by = chunk_size)

        # helper: translate a character vector with safe fallback
        safe_translate_vec <- function(items) {
          # try vector call first
          args <- list(text = items, target_language = trg)
          if (!base::is.null(trans_cache_path)) {
            args$cache_path <- trans_cache_path
          }
          out <- tryCatch(
            base::do.call(tr_fn, args),
            error = function(e) NULL
          )
          if (!base::is.null(out)) {
            out_chr <- base::as.character(out)
            out_chr[base::is.na(out_chr) | !base::nzchar(out_chr)] <-
              items
            return(out_chr)
          }
          # per‑item fallback on vector error
          per <- base::character(base::length(items))
          for (k in base::seq_along(items)) {
            one <- tryCatch(
              {
                args1 <- list(text = items[k], target_language = trg)
                if (!base::is.null(trans_cache_path)) {
                  args1$cache_path <- trans_cache_path
                }
                base::do.call(tr_fn, args1)
              },
              error = function(e) items[k]
            )
            if (base::length(one) == 0L || base::is.na(one)) {
              one <- items[k]
            }
            per[k] <- base::as.character(one)
          }
          per
        }

        # process chunks
        for (s in starts) {
          e <- base::min(s + chunk_size - 1L, base::length(uniq_labels))
          chunk <- uniq_labels[s:e]
          tr_chunk <- safe_translate_vec(chunk)
          translated_map[chunk] <- tr_chunk
        }

        # map back to full vector; fill any NAs with english
        fr_labels <- translated_map[en_labels]
        miss <- base::is.na(fr_labels) | !base::nzchar(fr_labels)
        if (base::any(miss)) fr_labels[miss] <- en_labels[miss]
        dict[[coln]] <- base::unname(fr_labels)
      } else {
        # translator not available: mirror english labels
        dict[[coln]] <- dict$label_en
      }
    }
  }

  # reorder so label columns follow label_en
  all_cols <- base::names(dict)
  lbl_cols <- base::grep("^label_", all_cols, value = TRUE)
  extra_lbl <- base::setdiff(lbl_cols, "label_en")
  rest <- base::setdiff(all_cols, c("variable", "type", lbl_cols))

  # select final order
  dict <- dict[, c("variable", "type", "label_en", extra_lbl, rest)]

  dict
}
