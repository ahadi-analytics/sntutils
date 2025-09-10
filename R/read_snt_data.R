#' Read the most-recently modified saved dataset
#'
#' @param path Directory to look in.
#' @param data_name Base name (without extension).
#' @param file_formats Character vector of allowed formats (e.g. "csv","rds").
#'   If NULL, consider any supported format found.
#' @param quiet Logical; when FALSE prints which file was read.
#' @return The loaded R object (data.frame, list, etc.). Aborts if not found.
#' @examples
#' \dontrun{
#' x <- read_snt_data(tmp, "my_population_data", "csv", quiet = FALSE)
#' }
#' @export
read_snt_data <- function(path, data_name, file_formats = NULL, quiet = TRUE) {
  if (is.null(path) || !nzchar(path) || !fs::dir_exists(path)) {
    cli::cli_abort("Invalid or missing directory: {path}.")
  }
  if (!grepl("^[A-Za-z0-9._-]+$", data_name)) {
    cli::cli_abort("`data_name` contains illegal characters.")
  }

  all_ok <- c("rds", "csv", "tsv", "xlsx", "parquet", "feather", "qs2")
  fmts <- if (is.null(file_formats)) all_ok else tolower(file_formats)
  bad <- setdiff(fmts, all_ok)
  if (length(bad)) {
    cli::cli_abort(c(
      "Unsupported format(s): {toString(bad)}.",
      "i" = "Supported: {toString(all_ok)}."
    ))
  }

  # Versioned candidates (<name>_v*.*) in allowed formats
  ver_files <- .list_versions(path, data_name)
  if (length(ver_files)) {
    ver_files <- ver_files[
      tolower(fs::path_ext(ver_files)) %in% fmts
    ]
  }

  # Unversioned fallbacks (<name>.<ext>)
  unv_files <- fs::path(path, paste0(data_name, ".", fmts))
  unv_files <- unv_files[fs::file_exists(unv_files)]

  cand <- unique(c(ver_files, unv_files))
  if (length(cand) == 0) {
    cli::cli_abort(c(
      "No files found for '{data_name}' in {fs::path_abs(path)}.",
      "i" = if (is.null(file_formats)) {
        "Tried any supported format."
      } else {
        "Searched formats: {toString(fmts)}."
      }
    ))
  }

  info <- fs::file_info(cand)
  chosen <- cand[which.max(info$modification_time)]
  fmt <- tolower(fs::path_ext(chosen))

  if (!quiet) {
    cli::cli_inform("Reading {fs::path_file(chosen)} (latest by mtime).")
  }

  obj <- .read_back(chosen, fmt)
  if (is.null(obj)) {
    cli::cli_abort(
      "Failed to read '{fs::path_file(chosen)}' (format: {fmt})."
    )
  }

  # ensure sf's vctrs methods are available whenever sf/sfc appears anywhere
  needs_sf <- (function(x) {
    if (inherits(x, "sf") || inherits(x, "sfc")) {
      return(TRUE)
    }
    if (is.data.frame(x)) {
      return(any(vapply(x, inherits, logical(1), "sfc")))
    }
    if (is.list(x)) {
      for (el in x) {
        if (Recall(el)) return(TRUE)
      }
    }
    FALSE
  })(obj)

  if (isTRUE(needs_sf)) {
    requireNamespace("sf", quietly = TRUE)

    # optional: clear accidental names on geometry if a single sf object
    if (inherits(obj, "sf")) {
      g <- sf::st_geometry(obj)
      if (!is.null(names(g))) {
        sf::st_geometry(obj) <- rlang::set_names(g, NULL)
      }
    }
  }

  obj
}
