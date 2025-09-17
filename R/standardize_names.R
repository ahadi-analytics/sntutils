# R/standardize_names.R
# helper utilities for name normalization across the package
# supports consistent cleaning of admin and facility strings
# RELEVANT FILES:R/harmonize_admin_names.R,tests/testthat/test-standardize_names.R
#' Standardize name-like strings with optional steps
#'
#' Applies a sequence of standardization steps to a vector of names. Each step
#' can be enabled or disabled. By default, all steps are enabled.
#'
#' @param name_vec Atomic vector. Values to standardize.
#' @param to_upper Logical. Convert to uppercase. Default is TRUE.
#' @param replace_punct Logical. Replace punctuation with spaces. Default is
#'       TRUE.
#' @param squish_spaces Logical. Trim/condense spaces. Default is TRUE.
#' @param normalize_accents Logical. Convert accents to ASCII. Default is TRUE.
#' @param normalize_spaces Logical. Normalize all space-like chars to " ".
#'   Default is TRUE.
#' @param roman_to_arabic Logical. Convert roman numerals I..IX to 1..9 at
#'   token boundaries. Default is TRUE.
#' @param sort_tokens Logical. Sort tokens with letters first, numbers last,
#'   both groups alphabetically. Default is TRUE.
#'
#' @return Character vector of standardized names.
#'
#' @examples
#' # basic usage
#' standardize_names(c("Chp Kpalime-III", "Kpalimé CHP 3"))
#'
#' # disable token sorting and roman conversion
#' standardize_names(
#'   c("USP Hanyigba-Todzi ii", "USP Hanyigba Todzi 2"),
#'   roman_to_arabic = FALSE, sort_tokens = FALSE
#' )
#'
#' # keep accents and case, only squish spaces and replace punctuation
#' standardize_names(
#'   c("Hôpital d’Adéta", "Hopital d'Adeta"),
#'   to_upper = FALSE, normalize_accents = FALSE
#' )
#' @export
standardize_names <- function(
  name_vec,
  to_upper = TRUE,
  replace_punct = TRUE,
  squish_spaces = TRUE,
  normalize_accents = TRUE,
  normalize_spaces = TRUE,
  roman_to_arabic = TRUE,
  sort_tokens = TRUE
) {
  # validate input type
  if (!rlang::is_atomic(name_vec)) {
    cli::cli_abort("`name_vec` must be an atomic vector.")
  }

  # validate flags
  flags <- list(
    to_upper = to_upper,
    replace_punct = replace_punct,
    squish_spaces = squish_spaces,
    normalize_accents = normalize_accents,
    normalize_spaces = normalize_spaces,
    roman_to_arabic = roman_to_arabic,
    sort_tokens = sort_tokens
  )

  bad_flag <- purrr::keep(flags, \(v) !(is.logical(v) && length(v) == 1L))
  if (length(bad_flag) > 0L) {
    bad_names <- paste(names(bad_flag), collapse = ", ")
    cli::cli_abort(
      "All flags must be single logicals. Problems: {bad_names}."
    )
  }

  # helper: apply function if condition is TRUE
  apply_if <- function(x, cond, fun) {
    if (cond) fun(x) else x
  }

  # helper: roman to arabic replacements at token boundaries
  roman_map <- c(
    "\\bIX\\b" = "9",
    "\\bVIII\\b" = "8",
    "\\bVII\\b" = "7",
    "\\bVI\\b" = "6",
    "\\bV\\b" = "5",
    "\\bIV\\b" = "4",
    "\\bIII\\b" = "3",
    "\\bII\\b" = "2",
    "\\bI\\b" = "1"
  )

  # helper: sort tokens letters first, numbers last
  sort_tokens_fun <- function(x) {
    purrr::map_chr(
      x,
      \(s) {
        # split on one-or-more spaces
        tokens <- strsplit(s, " +")[[1]]

        # detect pure numeric tokens
        is_num <- stringr::str_detect(tokens, "^[0-9]+$")

        # order alphabetic first, then numeric; sort within each group
        ordered <- c(sort(tokens[!is_num]), sort(tokens[is_num]))

        # rejoin
        paste(ordered, collapse = " ")
      }
    )
  }

  name_vec |>
    # ensure character
    as.character() |>
    # uppercase for consistent comparison
    (\(x) apply_if(x, to_upper, stringr::str_to_upper))() |>
    # replace punctuation with space
    (\(x) {
      apply_if(
        x,
        replace_punct,
        \(y) stringr::str_replace_all(y, "[[:punct:]]", " ")
      )
    })() |>
    # remove extra spaces and trim
    (\(x) apply_if(x, squish_spaces, stringr::str_squish))() |>
    # normalize accents
    (\(x) {
      apply_if(
        x,
        normalize_accents,
        \(y) stringi::stri_trans_general(y, "Latin-ASCII")
      )
    })() |>
    # normalize all space-like characters to single ASCII space
    (\(x) {
      apply_if(
        x,
        normalize_spaces,
        \(y) {
          stringi::stri_replace_all_regex(
            y,
            "\\p{Zs}+",
            " "
          )
        }
      )
    })() |>
    # convert roman numerals to arabic numerals
    (\(x) {
      apply_if(
        x,
        roman_to_arabic,
        \(y) stringr::str_replace_all(y, roman_map)
      )
    })() |>
    # sort tokens with letters first, numbers last
    (\(x) apply_if(x, sort_tokens, sort_tokens_fun))()
}
