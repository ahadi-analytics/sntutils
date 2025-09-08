# validation -------------------------------------------------------------------

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

  if (!grepl("^[A-Za-z0-9._-]+$", data_name)) {
    cli::cli_abort("`data_name` contains illegal characters.")
  }

  file_formats <- tolower(file_formats)
  ok_fmts <- c("rds", "csv", "tsv", "xlsx", "parquet", "feather", "qs", "qs2")
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
#' @examples
#' @noRd
.to_utf8 <- function(x) {
  if (!is.data.frame(x)) {
    return(x)
  }
  x[] <- lapply(x, function(col) {
    if (is.character(col)) {
      return(iconv(col, to = "UTF-8"))
    }
    col
  })
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
            paste0(capture.output(str(v, give.attr = FALSE)), collapse = " ")
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
    names(dfs) <- nm
    return(dfs)
  }

  as_df <- try(as.data.frame(x), silent = TRUE)
  if (!inherits(as_df, "try-error")) {
    return(clean_df(as_df))
  }
  x
}

# hashing ----------------------------------------------------------------------

#' Normalize object for hashing (internal)
#'
#' @description
#' Produces a canonical representation by optionally sorting columns/rows,
#' dropping `sf` geometry, stringifying dates/factors/list-cols, and
#' enforcing UTF-8. For `fmt = "xlsx"`, uses Excel preparation rules.
#'
#' @param x Object to normalize (data.frame or list of them).
#' @param fmt Format hint (affects normalization, e.g., "xlsx").
#' @param ignore_row Logical; if TRUE, order rows deterministically.
#' @param ignore_col Logical; if TRUE, sort columns by name.
#' @param ignore_sheet Logical; if TRUE, sort sheet names.
#' @return A normalized object suitable for stable hashing.
#' @keywords internal
#' @noRd
.normalize_for_hash <- function(
  x,
  fmt,
  ignore_row = TRUE,
  ignore_col = TRUE,
  ignore_sheet = TRUE
) {
  if (identical(fmt, "xlsx")) {
    x <- .prepare_for_excel(x)
  }

  norm_df <- function(df) {
    if (inherits(df, "sf") && requireNamespace("sf", quietly = TRUE)) {
      df <- sf::st_drop_geometry(df)
    }

    df[] <- lapply(df, function(col) {
      if (is.factor(col)) {
        return(as.character(col))
      }
      if (inherits(col, c("POSIXct", "POSIXt", "Date"))) {
        return(as.character(col))
      }
      if (is.list(col)) {
        return(vapply(
          col,
          function(v) {
            paste0(capture.output(str(v, give.attr = FALSE)), collapse = " ")
          },
          character(1)
        ))
      }
      col
    })

    if (isTRUE(ignore_col)) {
      ord_cols <- sort(names(df), method = "radix")
      df <- df[, ord_cols, drop = FALSE]
    }

    if (isTRUE(ignore_row) && nrow(df) > 1L && ncol(df) > 0L) {
      ord <- do.call(order, c(df, list(na.last = TRUE)))
      if (length(ord) == nrow(df)) {
        df <- df[ord, , drop = FALSE]
      }
      rownames(df) <- NULL
    }

    .to_utf8(df)
  }

  if (is.data.frame(x)) {
    return(norm_df(x))
  }
  if (is.list(x)) {
    dfs <- x[vapply(x, is.data.frame, logical(1))]
    dfs <- lapply(dfs, norm_df)
    nm <- names(dfs)
    if (is.null(nm)) {
      nm <- paste0("Sheet", seq_along(dfs))
    }
    names(dfs) <- nm
    if (isTRUE(ignore_sheet)) {
      dfs <- dfs[order(names(dfs))]
    }
    return(dfs)
  }
  x
}

#' Compute a stable object hash (internal)
#'
#' @description
#' Normalizes tabular/list objects to a canonical form (sorts columns/rows,
#' stringifies dates/factors, drops `sf` geometry), then hashes with digest.
#'
#' @inheritParams write_snt_data
#' @param x Object to hash.
#' @param fmt Format hint (affects normalization, e.g. "xlsx").
#' @param algo Digest algorithm; default "xxhash64".
#' @return Character scalar hash.
#' @keywords internal
#' @noRd
.obj_hash <- function(x, fmt, algo = "xxhash64") {
  nx <- .normalize_for_hash(x, fmt, TRUE, TRUE, TRUE)

  if (!requireNamespace("digest", quietly = TRUE)) {
    p <- tempfile(fileext = ".bin")
    on.exit(unlink(p), add = TRUE)
    con <- file(p, open = "wb")
    on.exit(try(close(con), silent = TRUE), add = TRUE)
    serialize(nx, con, version = 3)
    return(as.character(tools::md5sum(p)))
  }

  if (
    !algo %in%
    c(
      "crc32",
      "md5",
      "sha1",
      "sha256",
      "sha512",
      "xxhash32",
      "xxhash64",
      "murmur32"
    )
  ) {
    algo <- "md5"
  }

  digest::digest(nx, algo = algo, serialize = TRUE)
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
    }

    rio::export(obj, file = path, ...)
    path
  }

  list(
    qs = function(x, path, ...) {
      if (requireNamespace("qs2", quietly = TRUE)) {
        ns <- asNamespace("qs2")
        fn <- if (exists("qsave", envir = ns, mode = "function")) {
          get("qsave", envir = ns)
        } else if (exists("qs_save", envir = ns, mode = "function")) {
          get("qs_save", envir = ns)
        } else {
          NULL
        }
        if (!is.null(fn)) {
          fn(x, path, ...)
          return(path)
        }
      }
      if (requireNamespace("qs", quietly = TRUE)) {
        qs::qsave(x, path, ...)
        return(path)
      }
      cli::cli_abort("Writing '.qs' requires 'qs2' or 'qs'.")
    },

    qs2 = function(x, path, ...) {
      if (requireNamespace("qs2", quietly = TRUE)) {
        ns <- asNamespace("qs2")
        fn <- if (exists("qsave", envir = ns, mode = "function")) {
          get("qsave", envir = ns)
        } else if (exists("qs_save", envir = ns, mode = "function")) {
          get("qs_save", envir = ns)
        } else {
          NULL
        }
        if (!is.null(fn)) {
          fn(x, path, ...)
          return(path)
        }
      }
      if (requireNamespace("qs", quietly = TRUE)) {
        qs::qsave(x, path, ...)
        return(path)
      }
      cli::cli_abort("Writing '.qs2' requires 'qs2' (or fallback 'qs').")
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
    }
  )
}

# sidecar metadata (.snt) ------------------------------------------------------

#' Build hidden sidecar path (internal)
#'
#' @description
#' Computes the path for the hidden `.snt/<basename>.snt.json` sidecar that
#' stores metadata for a written data file. Creates the `.snt/` directory if
#' missing.
#'
#' @param data_path Path to the primary data file.
#' @return Absolute path to the sidecar JSON file.
#' @keywords internal
#' @noRd
.sidecar_path <- function(data_path) {
  dir <- fs::path_dir(data_path)
  hid <- fs::path(dir, ".snt")
  if (!fs::dir_exists(hid)) {
    fs::dir_create(hid, recurse = TRUE)
  }
  fs::path(hid, paste0(fs::path_file(data_path), ".snt.json"))
}

#' Write sidecar JSON (internal)
#'
#' @description
#' Writes a compact JSON sidecar with metadata for a given data file. On
#' Windows, attempts to hide the sidecar file.
#'
#' @param data_path Path to the primary data file.
#' @param meta Named list of metadata to write.
#' @return Invisibly, the sidecar path.
#' @keywords internal
#' @noRd
.write_sidecar <- function(data_path, meta) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    cli::cli_abort("Install 'jsonlite' to write sidecar JSON.")
  }
  sc <- .sidecar_path(data_path)
  jsonlite::write_json(meta, path = sc, auto_unbox = TRUE, pretty = FALSE)
  if (.Platform$OS.type == "windows") {
    try(fs::file_hide(sc), silent = TRUE)
  }
  invisible(sc)
}

#' Read sidecar JSON (if present)
#'
#' @description
#' Reads the `.snt` sidecar metadata for a written data file when present.
#'
#' @param data_path Path to the primary data file.
#' @return A named list of metadata, or `NULL` if missing/unavailable.
#' @keywords internal
#' @noRd
.read_sidecar <- function(data_path) {
  sc <- .sidecar_path(data_path)
  if (!fs::file_exists(sc)) {
    return(NULL)
  }
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    return(NULL)
  }
  out <- try(jsonlite::read_json(sc, simplifyVector = TRUE), silent = TRUE)
  if (inherits(out, "try-error")) {
    return(NULL)
  }
  out
}

# read back (for hashing on files) ---------------------------------------------


#' Read a file back for file-based hashing (internal)
#'
#' @description
#' Best-effort reader used for deduplication: attempts to import a file by
#' format using lightweight dependencies (base, qs/qs2, arrow, openxlsx).
#'
#' @param path File path to read.
#' @param fmt Lowercase format (e.g., "csv", "qs2").
#' @return R object or `NULL` on failure.
#' @keywords internal
#' @noRd
.read_back <- function(path, fmt) {
  if (!fs::file_exists(path)) {
    return(NULL)
  }
  fmt <- tolower(fmt)

  try_read <- function(funs) {
    for (fn in funs) {
      out <- try(fn(), silent = TRUE)
      if (!inherits(out, "try-error")) return(out)
    }
    NULL
  }

  out <- switch(
    fmt,
    rds = try(readRDS(path), silent = TRUE),
    csv = try(
      utils::read.csv(
        path,
        stringsAsFactors = FALSE,
        check.names = FALSE,
        encoding = "UTF-8"
      ),
      silent = TRUE
    ),
    tsv = try(
      utils::read.delim(
        path,
        stringsAsFactors = FALSE,
        check.names = FALSE,
        encoding = "UTF-8"
      ),
      silent = TRUE
    ),
    qs = try_read(list(
      function() {
        if (requireNamespace("qs2", quietly = TRUE)) {
          ns <- asNamespace("qs2")
          if (exists("qread", envir = ns, mode = "function")) {
            return(get("qread", envir = ns)(path))
          }
        }
        stop("no qs2 reader")
      },
      function() {
        if (requireNamespace("qs", quietly = TRUE)) {
          return(qs::qread(path))
        }
        stop("no qs reader")
      }
    )),
    qs2 = try_read(list(
      function() {
        if (requireNamespace("qs2", quietly = TRUE)) {
          ns <- asNamespace("qs2")
          if (exists("qread", envir = ns, mode = "function")) {
            return(get("qread", envir = ns)(path))
          }
        }
        stop("no qs2 reader")
      },
      function() {
        if (requireNamespace("qs", quietly = TRUE)) {
          return(qs::qread(path))
        }
        stop("no qs reader")
      }
    )),
    parquet = if (requireNamespace("arrow", quietly = TRUE)) {
      try(arrow::read_parquet(path), silent = TRUE)
    } else {
      try("error", silent = TRUE)
    },
    feather = if (requireNamespace("arrow", quietly = TRUE)) {
      try(arrow::read_feather(path), silent = TRUE)
    } else {
      try("error", silent = TRUE)
    },
    xlsx = if (requireNamespace("openxlsx", quietly = TRUE)) {
      try(openxlsx::read.xlsx(path), silent = TRUE)
    } else {
      try("error", silent = TRUE)
    },
    try("error", silent = TRUE)
  )

  if (inherits(out, "try-error")) {
    return(NULL)
  }
  out
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

# fast file hash ---------------------------------------------------------------

#' Fast file hash (internal)
#'
#' @description
#' Computes a fast file hash using `tools::md5sum` by default, or other
#' algorithms via `digest` when available.
#'
#' @param path Path to an existing file.
#' @param algo Hash algorithm (e.g., "md5").
#' @return Lowercase hex digest or `NA_character_` on failure.
#' @keywords internal
#' @noRd
.file_hash <- function(path, algo = "md5") {
  if (!fs::file_exists(path)) {
    return(NA_character_)
  }
  if (identical(algo, "md5")) {
    h <- suppressWarnings(tools::md5sum(path))
    return(tolower(as.character(h[[1]])))
  }
  if (requireNamespace("digest", quietly = TRUE)) {
    raw <- try(
      readBin(path, what = "raw", n = file.info(path)$size),
      silent = TRUE
    )
    if (inherits(raw, "try-error")) {
      return(NA_character_)
    }
    return(digest::digest(raw, algo = algo, serialize = FALSE))
  }
  NA_character_
}

# dedupe (cross-format, tag-agnostic) ------------------------------------------

#' Dedupe identical historical versions (internal)
#'
#' @description
#' Removes older files for the same `data_name` whose content matches the
#' current write. Compares sidecar object-hash, file MD5 (same format), or
#' last-resort read+normalize object-hash. Excludes the current write and any
#' `exclude_paths` from deletion.
#'
#' @inheritParams write_snt_data
#' @param current_path Path to the newly written file to keep.
#' @param exclude_paths Paths to protect from deletion.
#' @param verbose Emit messages for each removed file.
#' @return Character vector of removed file paths.
#' @keywords internal
#' @noRd
.dedupe_versions <- function(
  obj,
  dir,
  data_name,
  current_path,
  verbose = FALSE,
  exclude_paths = character(0)
) {
  cur_ext <- tolower(fs::path_ext(current_path))
  cur_file_hash <- .file_hash(current_path, "md5")

  # try sidecar first for the new file
  cur_sc <- .read_sidecar(current_path)
  cur_obj_hash <- NULL
  if (!is.null(cur_sc) && isTRUE(nzchar(cur_sc$obj_hash))) {
    cur_obj_hash <- cur_sc$obj_hash
  } else {
    # compute once only if needed
    cur_obj_hash <- .obj_hash(obj, fmt = "csv")
  }

  all_ver <- .list_versions(dir, data_name)
  if (length(all_ver) == 0) {
    return(character(0))
  }

  keep <- unique(fs::path_abs(c(current_path, exclude_paths)))
  cand <- setdiff(fs::path_abs(all_ver), keep)
  if (length(cand) == 0) {
    return(character(0))
  }

  removed <- character(0)

  for (f in cand) {
    f_ext <- tolower(fs::path_ext(f))
    same <- FALSE

    # 1) fast path: sidecar hash match (format-agnostic)
    f_sc <- .read_sidecar(f)
    if (!is.null(f_sc) && isTRUE(nzchar(f_sc$obj_hash))) {
      same <- identical(f_sc$obj_hash, cur_obj_hash)
    }

    # 2) if same ext and no sidecar path, try file MD5
    if (!same && identical(f_ext, cur_ext) && !is.na(cur_file_hash)) {
      f_hash <- .file_hash(f, "md5")
      same <- !is.na(f_hash) && identical(f_hash, cur_file_hash)
    }

    # 3) last resort: read+normalize and compare object-hash
    if (!same) {
      f_obj <- .read_back(f, f_ext)
      if (!is.null(f_obj)) {
        f_hash_obj <- .obj_hash(f_obj, fmt = "csv")
        same <- identical(f_hash_obj, cur_obj_hash)
      }
    }

    if (isTRUE(same) && file.remove(f)) {
      removed <- c(removed, f)
      if (isTRUE(verbose)) {
        cli::cli_inform("Removed duplicate: {fs::path_file(f)}")
      }
      # best effort: remove stale sidecar, too
      sc <- .sidecar_path(f)
      if (fs::file_exists(sc)) try(fs::file_delete(sc), silent = TRUE)
    }
  }
  unique(removed)
}

# main api ---------------------------------------------------------------------

#' Write an object to standardized filenames in one or more formats
#'
#' @description
#' Writes an object (data.frame, list of data.frames, sf, etc.) to a directory
#' using standardized naming. Supports atomic writes, optional deduping across
#' prior dated versions, and returns a structured summary.
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
#' @param dedupe_identical Logical; remove older identical versions.
#' @param quiet Logical; suppress info logs.
#' @param overwrite Logical; allow overwrite when not versioned.
#' @param ... Additional writer-specific arguments
#'
#' @return tibble with columns format, path, ok, bytes, hash, message
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
  dedupe_identical = TRUE,
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

  reg <- .writer_registry()
  fmts <- tolower(file_formats)

  # plan all output paths first (protect siblings during dedupe)
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
        hash = NA_character_,
        message = cli::format_message(msg)
      )
      next
    }

    ok <- FALSE
    bytes <- NA_real_
    hash_val <- NA_character_
    err_msg <- NULL

        tryCatch(
          {
            .atomic_write(function(p) writer(obj, p, ...), write_path)
            bytes <- suppressWarnings(file.size(write_path))
            hash_val <- .obj_hash(obj, fmt)
            ok <- TRUE

            # write sidecar metadata (hidden in .snt/)
            .write_sidecar(
              write_path,
              list(
                fmt = fmt,
                data_name = data_name,
                size = as.numeric(bytes),
                algo = "xxhash64",
                obj_hash = .obj_hash(obj, fmt = "csv"),
                file_md5 = .file_hash(write_path, "md5"),
                written_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ"),
                path = fs::path_file(write_path)
              )
            )
          },
          error = function(e) err_msg <<- conditionMessage(e)
        )

    if (isTRUE(ok) && isTRUE(dedupe_identical) && isTRUE(is_versioned)) {
      removed <- .dedupe_versions(
        obj = obj,
        dir = path,
        data_name = data_name,
        current_path = write_path,
        verbose = if (quiet == FALSE) TRUE,
        exclude_paths = planned_paths
      )
      if (!quiet && length(removed) > 0) {
        cli::cli_inform("Removed {length(removed)} duplicate file(s).")
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
      hash = hash_val,
      message = if (ok) "ok" else (err_msg %||% "error")
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
