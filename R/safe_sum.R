# /R/safe_sum.R
# Internal helper: row-safe sum for grouped aggregation
# Provides NA-aware sum: returns NA if all values are NA; otherwise sums with NA as 0
# RELEVANT FILES:R/utils.R, R/reporting_rate.R, R/build_dictionary.R

#' Row-safe sum for grouped aggregation
#'
#' Returns `NA_real_` if all values are `NA`; otherwise returns the sum with
#' missing values treated as zero.
#'
#' This is useful in `dplyr::summarise()` calls where you want a sum that
#' respects full-missing groups by yielding `NA_real_`, and otherwise sums while
#' ignoring `NA`s.
#'
#' @param x A numeric vector.
#'
#' @return A single numeric value: `NA_real_` if all values in `x` are missing,
#'   otherwise the numeric sum with `NA`s ignored.
#'
#' @examples
#' # All missing -> NA
#' sntutils:::`safe_sum`(c(NA, NA))
#'
#' # Mixed missing -> sum of non-missing
#' sntutils:::`safe_sum`(c(1, NA, 2))
#'
#' # No missing -> normal sum
#' sntutils:::`safe_sum`(c(1, 2, 3))
#'
#' @keywords internal
#' @export
safe_sum <- function(x) {
  n_ok <- sum(!is.na(x))
  s <- sum(x, na.rm = TRUE)
  if (n_ok == 0) return(NA_real_)
  s
}
