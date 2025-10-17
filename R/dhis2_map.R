#' Crosswalk DHIS2 dataset using dictionary
#'
#' @description
#' Given a dataset and its dictionary, build mappings between harmonised
#' names and raw headers, then rename dataset columns accordingly. Matching
#' is case-, space-, punctuation-, and accent-insensitive. Optionally prints
#' CLI diagnostics summarising matched and unmatched columns.
#'
#' @param data a data.frame with DHIS2 columns to be renamed.
#' @param dict a data.frame containing the dictionary.
#' @param new_col column in `dict` with harmonised names (name or index).
#' @param old_col column in `dict` with raw/source names (name or index).
#' @param verbose logical; if TRUE (default) prints CLI messages.
#' @param drop_unmatched logical; if TRUE, drops columns that are not found in the dictionary. Default is FALSE.
#'
#' @return the input dataset with renamed columns.
#'
#' @export
dhis2_map <- function(data, dict, new_col, old_col, verbose = TRUE, drop_unmatched = FALSE) {
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
          "Column '{x}' not found in dictionary. Available: {paste(colnames(df), collapse = ', ')}"
        )
      }
      return(pos)
    }
    cli::cli_abort("Column spec must be a name or index.")
  }

  new_col <- to_pos(new_col, dict)
  old_col <- to_pos(old_col, dict)

  # normalize text robustly
  norm <- function(x) {
    out <- gsub("\\u00A0", " ", x) # non-breaking spaces
    out <- stringi::stri_trans_general(out, "Latin-ASCII")
    out <- tolower(out)
    out <- gsub("<", "lt", out) # retain < and >
    out <- gsub(">", "gt", out)
    out <- gsub("[[:punct:]]", " ", out)
    out <- gsub("\\s+", " ", out)
    trimws(out)
  }

  new_names <- dict[[new_col]]
  old_names <- dict[[old_col]]

  # normalized forms
  data_cols_raw <- colnames(data)
  data_cols_norm <- norm(data_cols_raw)
  dict_old_norm <- norm(old_names)

  # dictionary match positions
  matched_pos <- match(dict_old_norm, data_cols_norm)

  # keep only true matches
  valid_idx <- which(!is.na(matched_pos))
  matched_data_cols <- data_cols_raw[matched_pos[valid_idx]]

  # construct mapping: new = matched data col
  matched_mapping <- stats::setNames(matched_data_cols, new_names[valid_idx])

  # preserve unmatched columns (unless drop_unmatched is TRUE)
  unmatched_cols <- setdiff(data_cols_raw, matched_data_cols)

  if (isTRUE(drop_unmatched)) {
    final_mapping <- matched_mapping
  } else {
    unmatched_mapping <- stats::setNames(unmatched_cols, unmatched_cols)
    # combine (but only overwrite if actually matched)
    final_mapping <- c(matched_mapping, unmatched_mapping)
  }

  if (isTRUE(verbose)) {
    cli::cli_h2("DHIS2 dictionary crosswalk")
    cli::cli_alert_success(
      "{length(valid_idx)} of {nrow(dict)} dictionary entries matched dataset."
    )
    if (length(old_names) - length(valid_idx) > 0) {
      cli::cli_alert_info(
        "Dictionary entries not found in dataset: {length(old_names) - length(valid_idx)}"
      )
      cat(
        "  *",
        paste(
          utils::head(old_names[is.na(matched_pos)], 5),
          collapse = "\n  * "
        ),
        "\n"
      )
    }
    not_in_dict <- setdiff(data_cols_raw, matched_data_cols)
    if (length(not_in_dict)) {
      if (isTRUE(drop_unmatched)) {
        cli::cli_alert_warning(
          "Dropping {length(not_in_dict)} dataset column{?s} without dictionary entry"
        )
      } else {
        cli::cli_alert_info(
          "Dataset columns without dictionary entry: {length(not_in_dict)}"
        )
      }
      cat("  *", paste(utils::head(not_in_dict, 5), collapse = "\n  * "), "\n")
    }
  }

  if (isTRUE(drop_unmatched)) {
    # Use select to keep only matched columns and rename them
    dplyr::select(data, !!!final_mapping)
  } else {
    # Use rename to preserve all columns
    dplyr::rename(data, !!!final_mapping)
  }
}
