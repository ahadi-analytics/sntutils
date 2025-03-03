#' Format Numbers with Thousand Separator
#'
#' This function formats numbers by adding a thousand separator (big mark)
#' and optionally rounding to a specified number of decimal places.
#'
#' @param x A numeric vector to be formatted.
#' @param decimals An integer specifying the number of decimal places to
#'         round to. Default is NULL, which means no rounding is performed.
#' @param big_mark A character to use as the thousand separator.
#'        Default is ",".
#' @return A character vector of formatted numbers.
#'
#' @examples
#' big_mark(1234567.89)
#' big_mark(c(1234.56, 7890123.45), decimals = 2, big_mark = ",")
#'
#' @export
big_mark <- function(x, decimals = NULL, big_mark = ",") {
  if (!is.null(decimals)) {
    x <- round(x, decimals)
  }
  base::format(x,
               big.mark = big_mark, scientific = FALSE, trim = TRUE,
               nsmall = if (is.null(decimals)) 0 else decimals
  )
}
