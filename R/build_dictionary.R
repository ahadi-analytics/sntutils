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
#'
#' @return tibble with:
#'   variable, type, label_en, n, n_missing, pct_missing, n_unique,
#'   example_values, min, max, notes, and optionally `label_<language>`.
#'
#' @details
#' english labels are merged as: internal defaults, then csv overrides.
#' unknown variables fall back to their column name.
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
  trans_cache_path = NULL
) {
  # validate input
  if (!base::inherits(data, "data.frame")) {
    cli::cli_abort("`data` must be a data.frame or tibble.")
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

  # attach english labels; fall back to variable name
  label_en <- base::unname(lbl_map[dict$variable])
  fill_name <- base::is.na(label_en) | !base::nzchar(label_en)
  if (base::any(fill_name)) {
    label_en[fill_name] <- dict$variable[fill_name]
  }
  dict$label_en <- label_en

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
        getFromNamespace("translate_text_vec", "sntutils"),
        error = function(e) NULL
      )

      # translate or mirror
      dict[[coln]] <- if (base::is.function(tr_fn)) {
        tryCatch(
          {
            if (!base::is.null(trans_cache_path)) {
              tr_fn(
                dict$label_en,
                target_language = trg,
                trans_cache_path = trans_cache_path
              )
            } else {
              tr_fn(dict$label_en, target_language = trg)
            }
          },
          error = function(e) dict$label_en
        )
      } else {
        dict$label_en
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
