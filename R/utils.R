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

#' Smart row-wise sum with missing data handling
#'
#' Computes the row-wise sum across multiple input vectors, while allowing
#' control over how many non-missing values must be present for a valid result.
#' If the number of non-`NA` values in a row is below `min_present`, the result
#' is set to `NA_real_`.
#'
#' @param ... Numeric vectors of equal length to be summed row-wise.
#' @param min_present Integer. Minimum number of non-`NA` values required per row
#'   to return a sum. Rows with fewer than `min_present` non-`NA` values return
#'   `NA_real_`.
#' @param .keep_zero_as_zero Logical. Currently unused, reserved for future
#'   development. Defaults to `TRUE`.
#'
#' @return A numeric vector of row-wise sums with `NA_real_` where insufficient
#'   data are present.
#'
#' @examples
#' fallback_row_sum(c(1, NA, 3), c(2, 4, NA))  # returns c(3, NA, NA)
#' fallback_row_sum(c(1, NA), c(2, NA), min_present = 1)  # returns c(3, NA)
#'
#' @export
fallback_row_sum <- function(..., min_present = 1, .keep_zero_as_zero = TRUE) {
  vars_matrix <- cbind(...)
  valid_count <- rowSums(!is.na(vars_matrix))
  raw_sum <- rowSums(vars_matrix, na.rm = TRUE)

  ifelse(valid_count >= min_present, raw_sum, NA_real_)
}

#' Fallback Absolute Difference Between Two Vectors
#'
#' Computes the difference between two numeric vectors element-wise.
#' If both values are present, returns \code{pmax(abs(col1 - col2), minimum)}.
#' If only one value is present, returns the non-missing value or \code{minimum},
#' whichever is greater. If both are missing, returns \code{NA_real_}.
#'
#' This function is useful when calculating differences that must be non-negative
#' and when missingness should fall back to a non-missing value rather than return
#' \code{NA}.
#'
#' @param col1 Numeric vector. First input column.
#' @param col2 Numeric vector. Second input column.
#' @param minimum Numeric scalar. Minimum allowable value for the result
#'   (default is \code{0}).
#'
#' @return A numeric vector with the same length as the inputs. Each element is:
#'   \itemize{
#'     \item the absolute difference between \code{col1} and \code{col2}, if both are non-missing,
#'     \item the non-missing value if only one is present,
#'     \item \code{NA_real_} if both values are missing.
#'   }
#'   In all cases, the result is constrained to be no less than \code{minimum}.
#'
#' @examples
#' fallback_diff(5, 3)        # 2
#' fallback_diff(NA, 4)       # 4
#' fallback_diff(7, NA)       # 7
#' fallback_diff(4, 9)        # 0
#' fallback_diff(NA, NA)      # NA
#'
#' @export
fallback_diff <- function(col1, col2, minimum = 0) {
  dplyr::case_when(
    is.na(col1) & is.na(col2) ~ NA_real_,
    is.na(col1) ~ pmax(col2, minimum),
    is.na(col2) ~ pmax(col1, minimum),
    TRUE ~ pmax(col1 - col2, minimum)
  )
}
