#' Crosswalk DHIS2 dataset using dictionary
#'
#' @description
#' Given a dataset and its dictionary, build mappings between harmonised
#' names and raw headers, then rename dataset columns accordingly. Matching
#' is case/space/accent-insensitive. Optionally prints CLI diagnostics.
#'
#' @param data A data.frame with DHIS2 columns to be renamed.
#' @param dict A data.frame containing the dictionary.
#' @param new_col Column in `dict` with harmonised names (name or index).
#' @param old_col Column in `dict` with raw/source names (name or index).
#' @param verbose Logical; if TRUE (default) prints CLI messages.
#'
#' @return The input dataset with renamed columns.
#'
#' @export
dhis2_map <- function(data, dict, new_col, old_col, verbose = TRUE) {
  if (!inherits(data, "data.frame")) {
    cli::cli_abort("{.arg data} must be a data.frame.")
  }
  if (!inherits(dict, "data.frame")) {
    cli::cli_abort("{.arg dict} must be a data.frame.")
  }

  # helper to resolve column selectors
  to_pos <- function(x, df) {
    if (is.numeric(x)) {
      if (x > ncol(df)) {
        cli::cli_abort("Column index {x} out of bounds.")
      }
      return(x)
    }
    if (is.character(x)) {
      pos <- match(x, colnames(df))
      if (is.na(pos)) {
        cli::cli_abort(
          "Column '{x}' not found in dictionary. Available: \\
          {paste(colnames(df), collapse = ', ')}"
        )
      }
      return(pos)
    }
    cli::cli_abort("Column spec must be a name or index.")
  }

  new_col <- to_pos(new_col, dict)
  old_col <- to_pos(old_col, dict)

  # normalisation helper
  norm <- function(x) {
    out <- tolower(x)
    out <- trimws(out)
    out <- gsub("\\s+", " ", out)
    stringi::stri_trans_general(out, "Latin-ASCII")
  }

  # pull dictionary columns
  new_names <- dict[[new_col]]
  old_names <- dict[[old_col]]

  # build mapping (new = old)
  name_mapping <- stats::setNames(old_names, new_names)

  # prepare matching
  data_cols_raw <- colnames(data)
  data_cols_norm <- norm(data_cols_raw)
  dict_old_norm <- norm(old_names)

  # filter to dictionary entries present in dataset
  keep_idx <- dict_old_norm %in% data_cols_norm
  filtered_mapping <- name_mapping[keep_idx]

  # replace with exact raw names from dataset
  matched_pos <- match(dict_old_norm[keep_idx], data_cols_norm)
  filtered_mapping[] <- data_cols_raw[matched_pos]

  # diagnostics
  dict_unmatched <- old_names[!keep_idx]
  data_unmatched <- data_cols_raw[
    !(data_cols_norm %in% dict_old_norm)
  ]

  if (isTRUE(verbose)) {
    cli::cli_h2("DHIS2 dictionary crosswalk")
    cli::cli_alert_success(
      "{sum(keep_idx)} of {length(name_mapping)} dictionary entries \\
      matched dataset."
    )
    if (length(dict_unmatched)) {
      cli::cli_alert_info(
        "Dictionary entries not found in dataset: \\
        {length(dict_unmatched)}"
      )
    }
    if (length(data_unmatched)) {
      cli::cli_alert_info(
        "Dataset columns without dictionary entry: \\
        {length(data_unmatched)}"
      )
    }
  }

  dplyr::rename(data, !!!filtered_mapping)
}
