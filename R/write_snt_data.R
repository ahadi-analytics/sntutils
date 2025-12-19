#' Validate inputs for write_snt_data (internal)
#'
#' @description
#' Checks directory existence/writability, validates `data_name`, normalizes
#' requested formats, and ensures `obj` is not NULL.
#'
#' @inheritParams write_snt_data
#' @return Invisibly `TRUE` on success; otherwise aborts with a clear message.
#' @keywords internal
#' @noRd
.validate_inputs <- function(obj, path, data_name, file_formats) {
  if (is.null(path) || !nzchar(path)) {
    cli::cli_abort("`path` must be a non-empty string.")
  }

  if (!fs::dir_exists(path)) {
    ok <- try(fs::dir_create(path, recurse = TRUE), silent = TRUE)
    if (inherits(ok, "try-error") || !fs::dir_exists(path)) {
      cli::cli_abort("Failed to create directory: {path}.")
    }
  }

  if (utils::file_test("-d", path)) {
    if (file.access(path, 2) != 0) {
      cli::cli_abort("Directory not writable: {path}.")
    }
  } else {
    cli::cli_abort("`path` is not a directory: {path}.")
  }


  if (!nzchar(data_name) || grepl("[/\\\\:*?\"<>|]", data_name)) {
    cli::cli_abort("`data_name` is empty or contains illegal characters.")
  }

  file_formats <- tolower(file_formats)
  ok_fmts <- c("rds", "csv", "tsv", "xlsx",
  "parquet", "feather", "qs2", "geojson")
  bad <- setdiff(file_formats, ok_fmts)
  if (length(bad) > 0) {
    cli::cli_abort(c(
      "Unsupported format(s): {toString(bad)}.",
      "i" = "Supported: {toString(ok_fmts)}."
    ))
  }

  if (is.null(obj)) {
    cli::cli_abort("`obj` cannot be NULL.")
  }

  invisible(TRUE)
}

# path building ----------------------------------------------------------------

#' Create standardized write path (internal)
#'
#' @description
#' Builds a normalized absolute file path using the naming
#' scheme `<data_name>[_v<tag>].<file_format>`. When `version_tag` is
#' supplied it takes precedence; otherwise the date is appended when
#' `include_date = TRUE`.
#'
#' @inheritParams write_snt_data
#' @param file_format Single format string (e.g., "csv", "rds").
#' @return Absolute path to the file to be written.
#' @keywords internal
#' @noRd
.create_write_path <- function(
  path,
  data_name,
  file_format,
  include_date = TRUE,
  date_format = "%Y-%m-%d",
  version_tag = NULL
) {
  suffix <- ""
  if (!is.null(version_tag)) {
    if (!grepl("^[A-Za-z0-9._-]+$", version_tag)) {
      cli::cli_abort("`version_tag` has illegal characters.")
    }
    suffix <- paste0("_v", version_tag)
  } else if (isTRUE(include_date)) {
    suffix <- paste0("_v", format(Sys.Date(), date_format))
  }

  fname <- paste0(data_name, suffix, ".", file_format)
  fs::path_abs(fs::path(path, fname))
}

# encoding ---------------------------------------------------------------------

#' Convert character columns to UTF-8
#'
#' @param x A data.frame
#' @return Data.frame with UTF-8 characters
#' @noRd
.to_utf8 <- function(x) {
  if (!is.data.frame(x)) {
    return(x)
  }

  # convert character and factor columns
  x[] <- lapply(x, function(col) {
    # handle factors by converting to character first
    if (is.factor(col)) {
      col <- as.character(col)
    }

    if (is.character(col)) {
      # multi-stage conversion with fallbacks
      conv <- iconv(col, from = "", to = "UTF-8", sub = "")

      # fallback to latin1 if NAs produced
      bad <- is.na(conv) & !is.na(col)
      if (any(bad)) {
        conv[bad] <- iconv(col[bad], from = "latin1", to = "UTF-8", sub = "")
      }

      # last resort: ASCII transliteration
      bad <- is.na(conv) & !is.na(col)
      if (any(bad)) {
        tmp <- iconv(col[bad], from = "", to = "ASCII//TRANSLIT", sub = "")
        conv[bad] <- iconv(tmp, from = "", to = "UTF-8", sub = "")
      }

      # set encoding mark
      Encoding(conv) <- "UTF-8"
      return(conv)
    }
    col
  })

  # also convert column names
  names(x) <- iconv(names(x), from = "", to = "UTF-8", sub = "")
  Encoding(names(x)) <- "UTF-8"

  x
}

# excel prep -------------------------------------------------------------------

#' Prepare object for Excel writing (internal)
#'
#' @description
#' Drops `sf` geometry, stringifies dates and list-columns, sanitizes and
#' deduplicates sheet names, and enforces UTF-8 for stable Excel exports.
#'
#' @param x A data.frame or list of data.frames.
#' @return A cleaned data.frame or a named list of cleaned data.frames.
#' @keywords internal
#' @noRd
.prepare_for_excel <- function(x) {
  clean_df <- function(df) {
    if (inherits(df, "sf") && requireNamespace("sf", quietly = TRUE)) {
      df <- sf::st_drop_geometry(df)
    }

    df[] <- lapply(df, function(col) {
      if (inherits(col, c("POSIXct", "POSIXt", "Date"))) {
        return(as.character(col))
      }
      if (is.list(col)) {
        return(vapply(
          col,
          function(v) {
            paste0(
              utils::capture.output(utils::str(v, give.attr = FALSE)),
              collapse = " "
            )
          },
          character(1)
        ))
      }
      col
    })

    names(df) <- make.names(names(df), unique = TRUE)
    .to_utf8(df)
  }

  if (is.data.frame(x)) {
    return(clean_df(x))
  }

  if (is.list(x)) {
    dfs <- x[vapply(x, is.data.frame, logical(1))]
    dfs <- lapply(dfs, clean_df)
    nm <- names(dfs)
    if (is.null(nm)) {
      nm <- paste0("Sheet", seq_along(dfs))
    }
    nm[nm == "" | is.na(nm)] <- paste0("Sheet", which(nm == "" | is.na(nm)))

    # ensure sheet names are UTF-8 before sanitization
    nm <- iconv(nm, from = "", to = "UTF-8", sub = "")
    Encoding(nm) <- "UTF-8"

    nm <- gsub("[\\[\\]\\*\\:\\?\\/\\\\]", "_", nm, perl = TRUE)
    nm <- substr(nm, 1, 31)
    if (any(duplicated(nm))) {
      used <- character(0)
      for (i in seq_along(nm)) {
        base <- substr(nm[i], 1, 28)
        cand <- nm[i]
        j <- 1
        while (cand %in% used) {
          cand <- paste0(base, "_", j %% 100)
          j <- j + 1
        }
        nm[i] <- cand
        used <- c(used, cand)
      }
    }

    # final UTF-8 enforcement on names
    nm <- iconv(nm, from = "", to = "UTF-8", sub = "")
    Encoding(nm) <- "UTF-8"
    names(dfs) <- nm
    return(dfs)
  }

  as_df <- try(as.data.frame(x), silent = TRUE)
  if (!inherits(as_df, "try-error")) {
    return(clean_df(as_df))
  }
  x
}

#' Write Excel workbook with standardized formatting (internal)
#'
#' @description
#' Saves a workbook where each sheet has fixed column widths, wrapping
#' disabled, and navy column headers for readability.
#'
#' @param obj A data.frame or named list of data.frames ready for Excel.
#' @param path Output path for the workbook.
#' @return Invisibly returns the written path.
#' @keywords internal
#' @noRd
.write_excel_formatted <- function(obj, path) {
  wb <- openxlsx::createWorkbook()
  sheets <- obj

  if (is.data.frame(sheets)) {
    sheets <- list(Data = sheets)
  }

  if (is.null(names(sheets))) {
    names(sheets) <- base::paste0("Sheet", base::seq_along(sheets))
  }

  header_style <- openxlsx::createStyle(
    fgFill = "#003865",
    fontColour = "#FFFFFF",
    textDecoration = "bold",
    halign = "center",
    wrapText = FALSE
  )
  body_style <- openxlsx::createStyle(wrapText = FALSE)
  integer_style <- openxlsx::createStyle(
    numFmt = "#,##0",
    wrapText = FALSE
  )
  decimal_style <- openxlsx::createStyle(
    numFmt = "#,##0.############",
    wrapText = FALSE
  )
  percent_style <- openxlsx::createStyle(
    numFmt = "0.00%",
    wrapText = FALSE
  )

  # ensure all sheets share consistent layout and styling
  for (sheet_name in names(sheets)) {
    data <- sheets[[sheet_name]]
    if (!is.data.frame(data)) {
      data <- base::as.data.frame(data)
    }
    openxlsx::addWorksheet(wb, sheet_name)
    openxlsx::writeData(
      wb = wb,
      sheet = sheet_name,
      x = data,
      withFilter = FALSE
    )
    n_cols <- base::ncol(data)
    n_rows <- base::nrow(data)
    if (n_cols > 0L) {
      col_widths <- base::vapply(
        X = base::seq_len(n_cols),
        FUN = function(idx) {
          col <- data[[idx]]
          header <- base::names(data)[idx]
          if (base::is.numeric(col)) {
            cell_strings <- base::format(
              col,
              trim = TRUE,
              scientific = FALSE
            )
          } else {
            cell_strings <- base::as.character(col)
          }
          cell_strings <- cell_strings[!base::is.na(cell_strings)]
          candidates <- base::c(header, cell_strings)
          if (base::length(candidates) == 0L) {
            return(12)
          }
          width <- base::max(base::nchar(candidates)) + 2L
          base::min(60, base::max(12, width))
        },
        FUN.VALUE = base::double(1)
      )
      openxlsx::setColWidths(
        wb = wb,
        sheet = sheet_name,
        cols = base::seq_len(n_cols),
        widths = col_widths
      )
      openxlsx::addStyle(
        wb = wb,
        sheet = sheet_name,
        style = header_style,
        rows = 1,
        cols = base::seq_len(n_cols),
        gridExpand = TRUE
      )
      if (n_rows > 0L) {
        body_rows <- 1L + base::seq_len(n_rows)
        openxlsx::addStyle(
          wb = wb,
          sheet = sheet_name,
          style = body_style,
          rows = body_rows,
          cols = base::seq_len(n_cols),
          gridExpand = TRUE
        )

        numeric_mask <- base::vapply(
          X = data,
          FUN = base::is.numeric,
          FUN.VALUE = base::logical(1)
        )
        numeric_cols <- base::which(numeric_mask)
        if (base::length(numeric_cols) > 0L) {
          col_names_lower <- base::tolower(base::names(data)[numeric_cols])
          year_mask <- base::grepl("year", col_names_lower, fixed = TRUE)
          numeric_cols <- numeric_cols[!year_mask]

          if (base::length(numeric_cols) > 0L) {
            col_names_lower <- base::tolower(base::names(data)[numeric_cols])
            percent_mask <- base::grepl(
              pattern = "percent|pct|%",
              x = col_names_lower
            )
            percent_cols <- numeric_cols[percent_mask]
            if (base::length(percent_cols) > 0L) {
              for (col_idx in percent_cols) {
                column_data <- data[[col_idx]]
                clean_values <- column_data[!base::is.na(column_data)]
                scale <- 1
                if (base::length(clean_values) > 0L) {
                  max_abs <- base::max(base::abs(clean_values))
                  if (max_abs > 1) {
                    scale <- 100
                  }
                }
                if (!base::identical(scale, 1)) {
                  data[[col_idx]] <- column_data / 100
                  openxlsx::writeData(
                    wb = wb,
                    sheet = sheet_name,
                    x = data[[col_idx]],
                    startCol = col_idx,
                    startRow = 2,
                    colNames = FALSE
                  )
                }
                openxlsx::addStyle(
                  wb = wb,
                  sheet = sheet_name,
                  style = percent_style,
                  rows = body_rows,
                  cols = col_idx,
                  gridExpand = TRUE
                )
              }
            }

            remaining_cols <- numeric_cols[!percent_mask]
            if (base::length(remaining_cols) > 0L) {
              remaining_data <- data[remaining_cols]
              integer_cols <- remaining_cols[
                base::vapply(
                  X = remaining_data,
                  FUN = function(col) {
                    clean <- col[!base::is.na(col)]
                    if (base::length(clean) == 0L) {
                      return(TRUE)
                    }
                    base::all(
                      base::abs(clean - base::round(clean)) <
                        base::sqrt(base::.Machine$double.eps)
                    )
                  },
                  FUN.VALUE = base::logical(1)
                )
              ]

              decimal_cols <- base::setdiff(remaining_cols, integer_cols)

              if (base::length(integer_cols) > 0L) {
                openxlsx::addStyle(
                  wb = wb,
                  sheet = sheet_name,
                  style = integer_style,
                  rows = body_rows,
                  cols = integer_cols,
                  gridExpand = TRUE
                )
              }

              if (base::length(decimal_cols) > 0L) {
                openxlsx::addStyle(
                  wb = wb,
                  sheet = sheet_name,
                  style = decimal_style,
                  rows = body_rows,
                  cols = decimal_cols,
                  gridExpand = TRUE
                )
              }
            }
          }
        }
      }
    }
  }

  openxlsx::saveWorkbook(wb, file = path, overwrite = TRUE)
  invisible(path)
}

# atomic write -----------------------------------------------------------------

#' Atomic write helper (internal)
#'
#' @description
#' Writes to a temporary file in the same directory (preserving extension)
#' and atomically renames it into place on success.
#'
#' @param write_fun Function of one argument (`path`) that performs the write.
#' @param final_path Target path to move the temp file into.
#' @return Final path on success.
#' @keywords internal
#' @noRd
.atomic_write <- function(write_fun, final_path) {
  dir <- fs::path_dir(final_path)
  ext <- fs::path_ext(final_path)
  ext <- if (nzchar(ext)) paste0(".", ext) else ".tmp"
  tmp <- fs::file_temp(tmp_dir = dir, pattern = ".snt.", ext = ext)
  on.exit(try(fs::file_delete(tmp), silent = TRUE), add = TRUE)
  write_fun(tmp)
  ok <- file.rename(tmp, final_path)
  if (!ok) {
    cli::cli_abort("Failed to move temp file to final path: {final_path}.")
  }
  final_path
}

# writer registry --------------------------------------------------------------

#' Get writer registry (internal)
#'
#' @description
#' Returns a map of file format to writer function. Each writer accepts
#' an object, a file path, and `...`, and returns the path on success.
#'
#' @return Named list of format -> function(x, path, ...).
#' @keywords internal
#' @noRd
.writer_registry <- function() {
  rio_writer <- function(x, path, ...) {
    if (!requireNamespace("rio", quietly = TRUE)) {
      cli::cli_abort("Install 'rio' to write {fs::path_ext(path)}.")
    }
    ext <- tolower(fs::path_ext(path))
    obj <- x

    if (inherits(obj, "sf") && requireNamespace("sf", quietly = TRUE)) {
      obj <- sf::st_drop_geometry(obj)
    }

    if (is.list(obj) && !is.data.frame(obj)) {
      dfs <- obj[vapply(obj, is.data.frame, logical(1))]
      if (length(dfs) > 0L) {
        if (ext == "xlsx") {
          obj <- dfs
        } else if (ext %in% c("csv", "tsv")) {
          if (length(dfs) == 1L) {
            obj <- dfs[[1L]]
          } else if (requireNamespace("vctrs", quietly = TRUE)) {
            obj <- vctrs::vec_rbind(!!!dfs)
          } else {
            cli::cli_abort(
              "Export to {ext} needs one data.frame or install 'vctrs'."
            )
          }
        }
      } else {
        obj <- try(as.data.frame(obj), silent = TRUE)
        if (inherits(obj, "try-error")) {
          cli::cli_abort(
            "Unsupported object type for export to {ext}: {class(x)}"
          )
        }
      }
    }

    if (ext %in% c("csv", "tsv") && !is.data.frame(obj)) {
      obj <- as.data.frame(obj)
    }

    if (ext %in% c("csv", "tsv")) {
      obj <- .to_utf8(obj)
    }
    if (ext == "xlsx") {
      obj <- .prepare_for_excel(obj)
      if (requireNamespace("openxlsx", quietly = TRUE)) {
        .write_excel_formatted(obj, path)
        return(path)
      }
    }

    rio::export(obj, file = path, ...)
    path
  }

  list(
    qs2 = function(x, path, ...) {
      if (requireNamespace("qs2", quietly = TRUE)) {
        ns <- asNamespace("qs2")
        fn <- if (exists("qs_save", envir = ns, mode = "function")) {
          get("qs_save", envir = ns)
        } else if (exists("qsave", envir = ns, mode = "function")) {
          get("qsave", envir = ns)
        } else {
          NULL
        }
        if (!is.null(fn)) {
          fn(x, path, ...)
          return(path)
        }
      }
      cli::cli_abort("Writing '.qs2' requires 'qs2'.")
    },

    rds = function(x, path, ...) {
      saveRDS(object = x, file = path, version = 3)
      path
    },

    csv = rio_writer,
    tsv = rio_writer,
    xlsx = rio_writer,

    parquet = function(x, path, ...) {
      if (!requireNamespace("arrow", quietly = TRUE)) {
        cli::cli_abort("Install 'arrow' for parquet output.")
      }
      if (inherits(x, "sf")) {
        x <- sf::st_drop_geometry(x)
      }
      if (!is.data.frame(x)) {
        x <- as.data.frame(x)
      }
      arrow::write_parquet(x, path, ...)
      path
    },

    feather = function(x, path, ...) {
      if (!requireNamespace("arrow", quietly = TRUE)) {
        cli::cli_abort("Install 'arrow' for feather output.")
      }
      if (inherits(x, "sf")) {
        x <- sf::st_drop_geometry(x)
      }
      if (!is.data.frame(x)) {
        x <- as.data.frame(x)
      }
      arrow::write_feather(x, path, ...)
      path
    },

    geojson = function(x, path, ...) {
      if (!requireNamespace("sf", quietly = TRUE)) {
        cli::cli_abort("Install 'sf' for GeoJSON output.")
      }
      if (!inherits(x, "sf")) {
        cli::cli_abort("GeoJSON format requires an 'sf' object with geometry.")
      }
      sf::st_write(x, path, driver = "GeoJSON", delete_dsn = TRUE, quiet = TRUE, ...)
      path
    }
  )
}


# list versions ----------------------------------------------------------------

#' List prior versioned files (internal)
#'
#' @description
#' Lists files in `dir` whose basename starts with `<data_name>_v`.
#'
#' @param dir Directory to scan.
#' @inheritParams write_snt_data
#' @return Character vector of file paths.
#' @keywords internal
#' @noRd
.list_versions <- function(dir, data_name) {
  pref <- paste0(data_name, "_v")
  files <- fs::dir_ls(dir, type = "file", recurse = FALSE)
  bn <- fs::path_file(files)
  files[startsWith(bn, pref)]
}

#' Prune old versioned files (internal)
#'
#' @description
#' Keeps the newest `keep` files for `<data_name>` in a format and removes
#' any extra files beyond that limit. Uses birth time when available and
#' falls back to modification time.
#'
#' @param dir Directory containing the files.
#' @param data_name Target data name.
#' @param fmt Lowercase format extension.
#' @param keep Number of newest files to retain.
#' @param protect Paths that must never be deleted (e.g., current write).
#' @param verbose Emit messages for deleted files.
#' @return Character vector of removed file paths.
#' @keywords internal
#' @noRd
.prune_versions <- function(dir,
                            data_name,
                            fmt,
                            keep,
                            protect = character(0),
                            verbose = FALSE) {
  keep <- as.integer(keep)
  if (is.na(keep) || keep < 1L) {
    cli::cli_abort("`n_saved` must be a positive integer when supplied.")
  }

  files <- .list_versions(dir, data_name)
  if (length(files) == 0L) {
    return(character(0))
  }

  files <- fs::path_abs(files)
  fmt <- tolower(fmt)
  files <- files[tolower(fs::path_ext(files)) == fmt]
  if (length(files) <= keep) {
    return(character(0))
  }

  info <- fs::file_info(files)
  time_col <- info$birth_time
  if (all(is.na(time_col))) {
    time_col <- info$modification_time
  }

  ord <- order(time_col, decreasing = TRUE, na.last = TRUE)
  keep_idx <- ord[
    seq_len(base::min(keep, base::length(ord)))
  ]
  keep_paths <- files[keep_idx]
  keep_paths <- unique(c(keep_paths, fs::path_abs(protect)))

  drop <- setdiff(files, keep_paths)
  if (length(drop) == 0L) {
    return(character(0))
  }

  removed <- character(0)

  for (f in drop) {
    ok <- try(fs::file_delete(f), silent = TRUE)
    ok <- !inherits(ok, "try-error") && !fs::file_exists(f)
    if (!ok) {
      ok <- tryCatch(isTRUE(file.remove(f)), error = function(...) FALSE)
    }
    if (isTRUE(ok)) {
      removed <- c(removed, f)
      if (isTRUE(verbose)) {
        cli::cli_inform("Removed older version: {fs::path_file(f)}")
      }
    }
  }

  removed
}

# main api ---------------------------------------------------------------------

#' Write an object to standardized filenames in one or more formats
#'
#' @description
#' Writes an object (data.frame, list of data.frames, sf, etc.) to a directory
#' using standardized naming. Supports atomic writes, optional pruning of older
#' versioned files, and returns a structured summary.
#'
#' When filenames are not versioned (no date and no tag) and a file already
#' exists, set `overwrite = TRUE` to replace it. Otherwise an error is thrown.
#'
#' @param obj Object to write
#' @param path Directory to write into
#' @param data_name Base file name without extension
#' @param file_formats Character vector of formats. Default "rds".
#' @param date_format Date format for version tag. Default "%Y-%m-%d".
#' @param include_date Logical; append _v<date>. Default TRUE.
#' @param version_tag Optional explicit tag (conflicts with include_date)
#' @param n_saved Optional positive integer; keep only the newest
#'   `n_saved` versioned files per format. Default is 3.
#' @param quiet Logical; suppress info logs.
#' @param overwrite Logical; allow overwrite when not versioned.
#' @param ... Additional writer-specific arguments
#'
#' @return tibble with columns format, path, ok, bytes, message
#' @examples
#' # write_snt_data(head(mtcars), tempdir(), "cars", "rds",
#' #                include_date = FALSE, overwrite = TRUE)
#' @export
write_snt_data <- function(
  obj,
  path,
  data_name,
  file_formats = "rds",
  date_format = "%Y-%m-%d",
  include_date = TRUE,
  version_tag = NULL,
  n_saved = 3,
  quiet = TRUE,
  overwrite = TRUE,
  ...
) {
  .validate_inputs(obj, path, data_name, file_formats)

  if (isTRUE(include_date) && !is.null(version_tag)) {
    cli::cli_abort("Use either `include_date` or `version_tag`, not both.")
  }

  include_date_eff <- isTRUE(include_date) && is.null(version_tag)
  is_versioned <- include_date_eff || !is.null(version_tag)

  if (!is.null(n_saved)) {
    if (!is.numeric(n_saved) || length(n_saved) != 1L || is.na(n_saved)) {
      cli::cli_abort("`n_saved` must be a positive integer or NULL.")
    }
    if (is.infinite(n_saved)) {
      n_saved <- NULL
    } else {
      n_saved <- as.integer(n_saved)
      if (n_saved < 1L) {
        cli::cli_abort("`n_saved` must be at least 1 when supplied.")
      }
    }
  }

  reg <- .writer_registry()
  fmts <- tolower(file_formats)

  # plan all output paths first (protect siblings during pruning)
  planned_paths <- vapply(
    fmts,
    function(fmt) {
      .create_write_path(
        path,
        data_name,
        fmt,
        include_date_eff,
        date_format,
        version_tag
      )
    },
    character(1)
  )

  rows <- vector("list", length(fmts))
  for (i in seq_along(fmts)) {
    fmt <- fmts[[i]]
    write_path <- planned_paths[[i]]

    if (!is_versioned && fs::file_exists(write_path)) {
      if (!isTRUE(overwrite)) {
        cli::cli_abort(c(
          "Refusing to overwrite existing file (non-versioned).",
          "i" = "Set `overwrite = TRUE`, enable `include_date`, ",
          "i" = "or supply a `version_tag`.",
          "x" = "Path: {fs::path_abs(write_path)}"
        ))
      }
      ok_rm <- try(fs::file_delete(write_path), silent = TRUE)
      if (inherits(ok_rm, "try-error") && fs::file_exists(write_path)) {
        cli::cli_abort(c(
          "Failed to remove existing file before overwrite.",
          "x" = "Path: {fs::path_abs(write_path)}"
        ))
      }
    }

    writer <- reg[[fmt]]
    if (is.null(writer)) {
      msg <- sprintf("No writer registered for format: %s.", fmt)
      if (!quiet) {
        cli::cli_alert_danger(msg)
      }
      rows[[i]] <- list(
        format = fmt,
        path = write_path,
        ok = FALSE,
        bytes = NA_real_,
        message = cli::format_message(msg)
      )
      next
    }

    ok <- FALSE
    bytes <- NA_real_
    err_msg <- NULL

        tryCatch(
          {
            .atomic_write(function(p) writer(obj, p, ...), write_path)
            bytes <- suppressWarnings(file.size(write_path))
            ok <- TRUE

          },
          error = function(e) err_msg <<- conditionMessage(e)
        )

    if (isTRUE(ok) && !is.null(n_saved) && isTRUE(is_versioned)) {
      removed <- .prune_versions(
        dir = path,
        data_name = data_name,
        fmt = fmt,
        keep = n_saved,
        protect = write_path,
        verbose = FALSE
      )
      if (!quiet && length(removed) > 0L) {
        cli::cli_inform("Removed {length(removed)} older version(s).")
      }
    }

    if (!quiet) {
      if (ok) {
        cli::cli_alert_success(
          "Wrote {fmt} -> {fs::path_rel(write_path)}"
        )
      } else {
        cli::cli_alert_danger(
          if (is.null(err_msg)) {
            paste0("Failed {fmt} -> {fs::path_rel(write_path)}")
          } else {
            paste0(
              "Failed {fmt} -> {fs::path_rel(write_path)}\nReason: ",
              err_msg
            )
          }
        )
      }
    }

    rows[[i]] <- list(
      format = fmt,
      path = write_path,
      ok = ok,
      bytes = bytes,
      message = if (ok) "ok" else (rlang::`%||%`(err_msg, "error"))
    )
  }

  # always build the summary; return it invisibly to avoid clutter
  res_df <- if (requireNamespace("tibble", quietly = TRUE)) {
    tibble::as_tibble(do.call(rbind, lapply(rows, as.data.frame)))
  } else {
    as.data.frame(do.call(rbind, lapply(rows, as.data.frame)))
  }
  invisible(res_df)
}
