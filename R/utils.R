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

#' Ensure Required Packages are Installed and Loaded
#'
#' This function checks if the specified packages are installed,
#' prompts the user to install missing packages, and loads all
#' packages into the current session.
#'
#' @param pkgs A character vector of package names to check and
#'    load.
#'
#' @return Invisibly returns the result of loading the packages.
#'
#' @examples
#' \dontrun{
#' ensure_packages(c("dplyr", "ggplot2"))
#' }
#' @importFrom utils install.packages
ensure_packages <- function(pkgs) {
  for (pkg in pkgs) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      response <- readline(
        paste0(
          'Package "',
          pkg,
          '" is required but not installed. Install it now? [y/n]: '
        )
      )
      if (tolower(response) %in% c("y", "yes")) {
        install.packages(pkg)
      } else {
        stop(paste0(
          'Please install package "', pkg, '" to continue.'
        ), call. = FALSE)
      }
    }
  }

  invisible(lapply(pkgs, library, character.only = TRUE))
}
