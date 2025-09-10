#' Ensure Required Packages are Installed and Loaded
#'
#' This function checks if the specified packages are installed,
#' prompts the user to install missing packages, and loads all
#' packages into the current session. In non-interactive sessions,
#' installation is skipped with a warning.
#'
#' @param pkgs A character vector of package names to check and
#'    potentially install.
#'
#' @return Invisibly returns NULL.
#'
#' @examples
#' \dontrun{
#' ensure_packages(c("dplyr", "ggplot2"))
#' }
#' @importFrom utils install.packages
ensure_packages <- function(pkgs) {
  missing_pkgs <- pkgs[!sapply(
    pkgs, requireNamespace,
    quietly = TRUE
  )]

  if (length(missing_pkgs) > 0) {
    cli::cli_h2("Package Installation Required")
    cli::cli_text("The following packages are missing:")
    cli::cli_ol(paste0("{.pkg ", missing_pkgs, "}"))

    # Handle non-interactive sessions
    if (!interactive()) {
      cli::cli_alert_warning(
        "Non-interactive session detected. Skipping package installation."
      )
      return(invisible(NULL))
    }

    # Prompt user for installation
    user_choice <- readline(
      prompt = cli::col_blue(
        "Do you want to install all missing packages? (y/n): "
      )
    )

    if (tolower(user_choice) == "y") {
      cli::cli_alert_success("Installing missing packages...")

      # Install CRAN packages
      for (pkg in missing_pkgs) {
        tryCatch(
          {
            utils::install.packages(pkg, quiet = TRUE)
          },
          error = function(e) {
            cli::cli_alert_danger(paste0(
              "Failed to install package: ", pkg,
              ". Error: ", e$message
            ))
          }
        )
      }

      cli::cli_alert_success("Installation of all packages complete.")
    } else {
      cli::cli_alert_warning(paste0(
        "Skipping installation of packages. ",
        "Some functionality might be limited."
      ))
    }
  }

  invisible(NULL)
}

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

#' Sum values with automatic NA handling
#'
#' @description A wrapper around base::sum() that automatically removes NA
#'    values
#' @param x A numeric vector
#' @return The sum of all non-NA values in x
#' @examples
#' sum2(c(1, 2, NA, 4)) # Returns 7
#' sum2(c(NA, NA, 5, 10)) # Returns 15
#' @export
sum2 <- function(x) {
  sum(x, na.rm = TRUE)
}

#' Calculate mean with automatic NA handling
#'
#' @description A wrapper around base::mean() that automatically removes NA
#'    values
#' @param x A numeric vector
#' @return The mean of all non-NA values in x
#' @examples
#' mean2(c(1, 2, NA, 4)) # Returns 2.333333
#' mean2(c(NA, NA, 10, 20)) # Returns 15
#' @export
mean2 <- function(x) {
  mean(x, na.rm = TRUE)
}

#' Calculate median with automatic NA handling
#'
#' @description A wrapper around base::median() that automatically removes NA
#'    values
#' @param x A numeric vector
#' @return The median of all non-NA values in x
#' @examples
#' median2(c(1, 2, NA, 4)) # Returns 2.333333
#' median2(c(NA, NA, 10, 20)) # Returns 15
#' @export
median2 <- function(x) {
  stats::median(x, na.rm = TRUE)
}

#' Vectorized version of digest::digest
#'
#' This function applies the digest::digest function to each element of a
#' vector, returning a vector of hash values.
#'
#' @param object An R object to calculate a hash value for
#' @param algo The hashing algorithm to be used. See digest::digest for options
#' @param serialize Whether to serialize the object first. Default TRUE
#' @param file A filename to read from. Default FALSE
#' @param length The number of characters to read from file. Default NULL
#' @param skip Number of bytes to skip before reading file. Default 0
#' @param ascii If TRUE, return hash in ASCII format. Default FALSE
#' @param raw If TRUE, return raw vector. Default FALSE
#' @param seed Random seed for hash. Default 0
#' @param errormode Error mode for file reading. Default "stop"
#' @param serializeVersion Version used in serialization. Default NULL
#' @return A character vector of hash values
#' @examples
#' vdigest(c("a", "b", "c"))
#' vdigest(as.character(iris$Species))
#' @export
vdigest <- Vectorize(digest::digest)

#' Smart row-wise sum with missing data handling and type preservation
#'
#' Computes the row-wise sum across multiple input vectors, while allowing
#' control over how many non-missing values must be present for a valid
#' result. If the number of non-`NA` values in a row is below `min_present`,
#' the result is set to `NA` of the appropriate type.
#'
#' The return type matches the inputs:
#' - If all inputs are integer vectors, the result is an integer vector
#'   with `NA_integer_` where insufficient data are present.
#' - Otherwise, the result is a numeric (double) vector with `NA_real_`
#'   where insufficient data are present.
#'
#' @param ... Numeric or integer vectors of equal length to be summed row-wise.
#' @param min_present Integer. Minimum number of non-`NA` values required per
#'   row to return a sum. Rows with fewer than `min_present` non-`NA` values
#'   return `NA_integer_` if all inputs are integer, otherwise `NA_real_`.
#' @param .keep_zero_as_zero Logical. Currently unused, reserved for future
#'   development. Defaults to `TRUE`.
#'
#' @return An integer or numeric vector of row-wise sums, depending on the input
#'   types, with appropriate `NA` values where insufficient data are present.
#'
#' @examples
#' # all integer inputs -> integer output
#' x <- c(1L, 2L, NA_integer_)
#' y <- c(3L, NA_integer_, 4L)
#' fallback_row_sum(x, y)
#' typeof(fallback_row_sum(x, y))  # "integer"
#'
#' # mixed integer and numeric inputs -> numeric output
#' z <- c(1, 2, NA)
#' fallback_row_sum(x, z)
#' typeof(fallback_row_sum(x, z))  # "double"
#'
#' # using min_present to control NA behaviour
#' fallback_row_sum(c(1, NA), c(2, NA), min_present = 1)
#'
#' @export
fallback_row_sum <- function(..., min_present = 1, .keep_zero_as_zero = TRUE) {
  args <- list(...)
  all_int <- all(vapply(args, is.integer, logical(1)))

  vars_matrix <- cbind(...)
  valid_count <- rowSums(!is.na(vars_matrix))
  raw_sum <- rowSums(vars_matrix, na.rm = TRUE)

  if (all_int) {
    out <- ifelse(valid_count >= min_present, raw_sum, NA_integer_)
    return(as.integer(out))
  } else {
    out <- ifelse(valid_count >= min_present, raw_sum, NA_real_)
    return(out)
  }
}

#' Fallback Absolute Difference Between Two Vectors (type-preserving)
#'
#' Computes the difference between two numeric or integer vectors element-wise.
#' If both values are present, returns \code{pmax(abs(col1 - col2), minimum)}.
#' If only one value is present, returns the non-missing value or \code{minimum},
#' whichever is greater. If both are missing, returns \code{NA} of the
#' appropriate type.
#'
#' The return type matches the inputs:
#' - If both inputs are integer vectors (and `minimum` is an integer scalar),
#'   the result is integer with `NA_integer_` where both are missing.
#' - Otherwise, the result is numeric (double) with `NA_real_` where both
#'   are missing.
#'
#' @param col1 Numeric or integer vector. First input column.
#' @param col2 Numeric or integer vector. Second input column.
#' @param minimum Numeric or integer scalar. Minimum allowable value for the
#'   result (default is `0`).
#'
#' @return An integer or numeric vector with the same length as the inputs.
#'   Each element is:
#'   \itemize{
#'     \item the absolute difference between \code{col1} and \code{col2}, if
#'       both are non-missing,
#'     \item the non-missing value if only one is present,
#'     \item `NA_integer_` or `NA_real_` if both are missing.
#'   }
#'   In all cases, the result is constrained to be no less than `minimum`.
#'
#' @examples
#' fallback_diff(5, 3)        # 2
#' fallback_diff(NA, 4)       # 4
#' fallback_diff(7, NA)       # 7
#' fallback_diff(4, 9)        # 0
#' fallback_diff(NA, NA)      # NA_real_
#'
#' xi <- c(5L, NA, 7L)
#' yi <- c(3L, 4L, NA_integer_)
#' fallback_diff(xi, yi)      # integer output
#'
#' @export
fallback_diff <- function(col1, col2, minimum = 0) {
  both_int <- is.integer(col1) && is.integer(col2)

  res <- dplyr::case_when(
    is.na(col1) & is.na(col2) ~ if (both_int) NA_integer_ else NA_real_,
    is.na(col1) ~ pmax(col2, minimum, na.rm = TRUE),
    is.na(col2) ~ pmax(col1, minimum, na.rm = TRUE),
    TRUE ~ pmax(abs(col1 - col2), minimum)
  )

  if (both_int) {
    return(as.integer(res))
  }
  res
}

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
