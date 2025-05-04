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
