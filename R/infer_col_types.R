#' Infer column types using readr, then layer factor detection
#'
#' use readr::type_convert() to infer non-factor types (numeric, integer, date,
#' datetime, logical), then propose factors via low-cardinality rules. protect
#' id-like names and leading-zero codes. return the dataset by default, and
#' include the metadata plan only when requested.
#'
#' @param data data.frame or tibble.
#' @param max_levels integer. max distinct values for factor. default 50.
#' @param max_unique_ratio numeric (0, 1]. max unique/n for factor. default 0.2.
#' @param protect_patterns character regexes for protected names.
#'   default c("id$", "uid$", "code$", "ref$", "key$").
#' @param keep_leading_zero_chars logical. keep character if any value has
#'   leading zeros in digit-only strings. default TRUE.
#' @param prefer_logical_for_binary logical. kept for api compatibility, not
#'   used when delegating to readr. default TRUE.
#' @param apply logical. if TRUE, apply factor conversions on top of parsed
#'   types. default FALSE.
#' @param return one of "data", "both", "plan". default "data".
#'
#' @returns
#' invisible object depending on `return`:
#' - "data": tibble of parsed data (and factors if apply = TRUE)
#' - "both": list(plan = tibble, data = tibble as above)
#' - "plan": tibble only
#'
#' @examples
#' df <- tibble::tibble(
#'   id  = c("001", "002", "003"),
#'   sex = c("M", "F", "F"),
#'   age = c("1", "2", "3"),
#'   dt  = c("2024-01-01 12:00:00", "2024-01-02 00:00:00",
#'           "2024-01-03 01:02:03")
#' )
#'
#' # just get the dataset (parsed + factors)
#' dat <- infer_col_types(df, apply = TRUE, return = "data")
#'
#' # get dataset and plan
#' both <- infer_col_types(df, apply = TRUE, return = "both")
#' both$plan |> dplyr::select(name, current_type, proposed_type, rule)
#' dplyr::glimpse(both$data)
#' @export
infer_col_types <- function(
  data,
  max_levels = 50,
  max_unique_ratio = 0.2,
  protect_patterns = c("id$", "uid$", "code$", "ref$", "key$"),
  keep_leading_zero_chars = TRUE,
  prefer_logical_for_binary = TRUE,
  apply = FALSE,
  return = c("data", "both", "plan")
) {
  # match return argument
  return <- match.arg(return)

  # validate inputs
  if (!inherits(data, "data.frame")) {
    cli::cli_abort("`data` must be a data.frame or tibble.")
  }
  if (length(max_levels) != 1 || max_levels < 2) {
    cli::cli_abort("`max_levels` must be a single integer >= 2.")
  }
  if (
    length(max_unique_ratio) != 1 ||
      max_unique_ratio <= 0 ||
      max_unique_ratio > 1
  ) {
    cli::cli_abort("`max_unique_ratio` must be in (0, 1].")
  }

  # core stats for plan and factor heuristics
  cols <- names(data)
  n <- nrow(data)
  n_non_na <- purrr::map_int(cols, function(nm) sum(!is.na(data[[nm]])))
  n_unique <- purrr::map_int(cols, function(nm) {
    dplyr::n_distinct(data[[nm]], na.rm = TRUE)
  })
  unique_ratio <- n_unique / dplyr::if_else(n_non_na > 0, n_non_na, 1L)
  orig_types <- purrr::map_chr(cols, function(nm) class(data[[nm]])[1])

  # flags for protected and leading-zero character columns
  protected <- purrr::map_lgl(cols, function(nm) {
    .is_protected(nm, protect_patterns)
  })
  lead0 <- purrr::map_lgl(cols, function(nm) {
    if (!keep_leading_zero_chars) {
      return(FALSE)
    }
    x <- data[[nm]]
    if (!is.character(x)) {
      return(FALSE)
    }
    .has_leading_zeros(x)
  })

  # build readr col_types so protected/lead0 stay character
  col_map <- stats::setNames(
    rep(list(readr::col_guess()), length(cols)),
    cols
  )
  keep_char <- cols[protected | lead0]
  if (length(keep_char) > 0) {
    for (nm in keep_char) {
      col_map[[nm]] <- readr::col_character()
    }
  }

  # parse via readr only when there are character columns
  has_char <- length(cols) > 0 &&
    any(purrr::map_lgl(cols, function(nm) is.character(data[[nm]])))

  data_parsed <- if (has_char) {
    suppressWarnings(suppressMessages(
      readr::type_convert(
        dplyr::as_tibble(data),
        col_types = do.call(readr::cols, col_map),
        guess_integer = TRUE
      )
    ))
  } else {
    dplyr::as_tibble(data)
  }

  # base proposals from readr
  base_prop <- purrr::map_chr(cols, function(nm) class(data_parsed[[nm]])[1])
  base_rule <- paste0("readr:", base_prop)

  # factor detection (character only) on original data
  fplan <- detect_factors(
    data = data,
    max_levels = max_levels,
    max_unique_ratio = max_unique_ratio,
    protect_patterns = protect_patterns,
    keep_leading_zero_chars = keep_leading_zero_chars
  )
  f_names <- if (nrow(fplan) == 0) character(0) else fplan$name

  # override proposals for protected/lead0 and factor candidates
  proposed_type <- base_prop
  rule <- base_rule

  for (i in seq_along(cols)) {
    nm <- cols[i]
    if (protected[i]) {
      proposed_type[i] <- "character"
      rule[i] <- "protected_by_name"
      next
    }
    if (lead0[i]) {
      proposed_type[i] <- "character"
      rule[i] <- "leading_zeros_guard"
      next
    }
    if (nm %in% f_names) {
      proposed_type[i] <- "factor"
      rule[i] <- "low_cardinality"
    }
  }

  # assemble plan
  plan <- tibble::tibble(
    name = cols,
    current_type = orig_types,
    proposed_type = proposed_type,
    rule = rule,
    protected = protected,
    n = n,
    n_non_na = n_non_na,
    n_unique = n_unique,
    unique_ratio = unique_ratio
  )

  # build final dataset according to apply
  data_final <- data_parsed
  if (isTRUE(apply) && length(f_names) > 0) {
    for (nm in f_names) {
      # convert using original text to preserve first-seen order
      data_final[[nm]] <- forcats::as_factor(as.character(data[[nm]]))
    }
  }

  # return based on `return` argument
  if (return == "data") {
    return(invisible(data_final))
  } else if (return == "plan") {
    return(invisible(plan))
  } else {
    return(invisible(list(plan = plan, data = data_final)))
  }
}

#' Detect factor-like character columns (low-cardinality only)
#'
#' identify character columns that look categorical using simple heuristics.
#' protects id-like names and leading zeros. returns a compact tibble invisibly.
#'
#' @inheritParams infer_col_types
#'
#' @returns
#' invisible tibble with:
#'   name, n, n_non_na, n_unique, unique_ratio, proposed, reason
#'
#' @examples
#' tibble::tibble(adm = c("A","B","A"), code = c("01","02","03")) |>
#'   detect_factors()
#' @export
detect_factors <- function(
  data,
  max_levels = 50,
  max_unique_ratio = 0.2,
  protect_patterns = c("id$", "uid$", "code$", "ref$", "key$"),
  keep_leading_zero_chars = TRUE
) {
  if (!inherits(data, "data.frame")) {
    cli::cli_abort("`data` must be a data.frame or tibble.")
  }

  # compute stats
  cols <- names(data)
  n <- nrow(data)

  plan <- tibble::tibble(
    name = cols,
    n = n,
    n_non_na = purrr::map_int(cols, function(nm) {
      sum(!is.na(data[[nm]]))
    }),
    n_unique = purrr::map_int(cols, function(nm) {
      dplyr::n_distinct(data[[nm]], na.rm = TRUE)
    }),
    unique_ratio = n_unique /
      dplyr::if_else(n_non_na > 0, n_non_na, 1L),
    is_char = purrr::map_lgl(cols, function(nm) is.character(data[[nm]])),
    protected = purrr::map_lgl(cols, function(nm) {
      .is_protected(nm, protect_patterns)
    }),
    lead0 = purrr::map_lgl(cols, function(nm) {
      if (!keep_leading_zero_chars) {
        return(FALSE)
      }
      x <- data[[nm]]
      if (!is.character(x)) {
        return(FALSE)
      }
      .has_leading_zeros(x)
    })
  ) |>
    dplyr::mutate(
      proposed = dplyr::case_when(
        !is_char ~ FALSE,
        protected ~ FALSE,
        lead0 ~ FALSE,
        n_unique <= 1 ~ FALSE,
        n_unique > max_levels ~ FALSE,
        unique_ratio > max_unique_ratio ~ FALSE,
        TRUE ~ TRUE
      ),
      reason = dplyr::case_when(
        !is_char ~ "not character",
        protected ~ "protected by name",
        lead0 ~ "leading zeros detected",
        n_unique <= 1 ~ "constant or all NA",
        n_unique > max_levels ~ "too many levels",
        unique_ratio > max_unique_ratio ~ "too unique for factor",
        TRUE ~ "low cardinality character"
      )
    ) |>
    dplyr::filter(proposed) |>
    dplyr::select(
      dplyr::all_of(c(
        "name",
        "n",
        "n_non_na",
        "n_unique",
        "unique_ratio",
        "proposed",
        "reason"
      ))
    )

  return(invisible(plan))
}

#' Check if a column name is protected by regex patterns
#'
#' prevent conversion of id-like variables by name.
#'
#' @param nm single column name.
#' @param patterns character vector of regex patterns.
#'
#' @returns
#' logical scalar; TRUE if name matches any pattern (case-insensitive).
#'
#' @examples
#' .is_protected("hf_id", c("id$", "uid$"))
#' .is_protected("area", c("id$"))
#'
#' @keywords internal
#' @noRd
.is_protected <- function(nm, patterns) {
  if (length(patterns) == 0) {
    return(FALSE)
  }
  any(vapply(
    patterns,
    function(p) grepl(p, nm, ignore.case = TRUE),
    logical(1)
  ))
}

#' Detect leading zeros in all-digit character strings
#'
#' detect if any non-missing values start with one or more zeros followed by
#' digits. used to guard against coercing codes like "00123".
#'
#' @param x_chr character vector to test.
#'
#' @returns
#' logical scalar; TRUE if any leading-zero digit strings are present.
#'
#' @examples
#' .has_leading_zeros(c("001", "12", NA))
#' .has_leading_zeros(c("A1", "B2"))
#'
#' @keywords internal
#' @noRd
.has_leading_zeros <- function(x_chr) {
  # keep only non-missing
  x_chr <- x_chr[!is.na(x_chr)]
  if (length(x_chr) == 0) {
    return(FALSE)
  }
  any(grepl("^0+\\d+$", x_chr))
}
