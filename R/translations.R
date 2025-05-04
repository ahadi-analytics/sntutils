#' Convert date to yearmon format with localized month names
#'
#' This function converts a date to yearmon format (Month Year) with the month
#' name displayed in the specified language.
#'
#' @param date A date object or character string that can be converted to a
#'    date
#' @param language A two-letter language code. Available options:
#'   \itemize{
#'     \item "fr" - French
#'     \item "en" - English
#'     \item "es" - Spanish
#'     \item "de" - German
#'     \item "it" - Italian
#'     \item "pt" - Portuguese
#'     \item "ru" - Russian
#'     \item "zh" - Chinese
#'     \item "ja" - Japanese
#'     \item "ko" - Korean
#'     \item "ar" - Arabic
#'     \item "hi" - Hindi
#'   }
#' @param format Format string for the output. Default is "%b %Y" for
#'   abbreviated month name and year. Use "%B %Y" for full month name.
#' @return A character string with the month and year in the specified
#'   language
#' @examples
#' translate_yearmon(Sys.Date(), "en")
#' translate_yearmon(Sys.Date(), "fr")
#' translate_yearmon(Sys.Date(), "fr", format = "%B %Y")
#' @export
translate_yearmon <- function(date, language = "fr", format = "%b %Y") {
  # Get available locales
  available_locales <- system("locale -a", intern = TRUE)

  # Map language codes to locales, checking availability
  locale <- switch(
    language,
    "fr" = grep("^fr_.*\\.UTF-8$", available_locales, value = TRUE)[1] %||%
      "fr_FR.UTF-8",
    "en" = grep("^en_.*\\.UTF-8$", available_locales, value = TRUE)[1] %||%
      "en_US.UTF-8",
    "es" = grep("^es_.*\\.UTF-8$", available_locales, value = TRUE)[1] %||%
      "es_ES.UTF-8",
    "de" = grep("^de_.*\\.UTF-8$", available_locales, value = TRUE)[1] %||%
      "de_DE.UTF-8",
    "it" = grep("^it_.*\\.UTF-8$", available_locales, value = TRUE)[1] %||%
      "it_IT.UTF-8",
    "pt" = grep("^pt_.*\\.UTF-8$", available_locales, value = TRUE)[1] %||%
      "pt_PT.UTF-8",
    "ru" = grep("^ru_.*\\.UTF-8$", available_locales, value = TRUE)[1] %||%
      "ru_RU.UTF-8",
    "zh" = grep("^zh_.*\\.UTF-8$", available_locales, value = TRUE)[1] %||%
      "zh_CN.UTF-8",
    "ja" = grep("^ja_.*\\.UTF-8$", available_locales, value = TRUE)[1] %||%
      "ja_JP.UTF-8",
    "ko" = grep("^ko_.*\\.UTF-8$", available_locales, value = TRUE)[1] %||%
      "ko_KR.UTF-8",
    "ar" = grep("^ar_.*\\.UTF-8$", available_locales, value = TRUE)[1] %||%
      "ar_SA.UTF-8",
    "hi" = grep("^hi_.*\\.UTF-8$", available_locales, value = TRUE)[1] %||%
      "hi_IN.UTF-8"
  )

  # Check if locale is available
  if (!locale %in% available_locales) {
    warning(
      "Locale ", locale,
      " not available. Using system default."
    )
    locale <- Sys.getlocale("LC_TIME")
  }

  # Convert to date first if not already a date
  if (!inherits(date, "Date")) {
    date <- as.Date(date)
  }

  # Fix for encoding issues with month names
  result <- date |>
    zoo::as.yearmon() |>
    (\(x) withr::with_locale(
      c("LC_TIME" = locale),
      format(x, format)
    ))()

  return(result)
}

#' Translate text to target language with persistent file cache
#'
#' Uses gtranslate, and caches successful translations in a file (RDS).
#' The cache is implemented as a data frame with metadata for better tracking
#' and management of translations.
#'
#' @param text Character string to translate
#' @param target_language Target language code (default: "en")
#' @param source_language Source language code (default: "en" for English)
#' @param cache_path Path to directory for storing the cache file
#'
#' @return Translated text or original text if translation fails
#' @export
#'
#' @examples
#' \dontrun{
#' # Translate from Spanish to English
#' translate_text("Hola mundo", target_language = "en", source_language = "es")
#'
#' # Translate to French with English as source
#' translate_text("Hello world", target_language = "fr")
#'
#' # Use custom cache location
#' translate_text("Hello world",
#'   target_language = "de",
#'   cache_path = "~/translation_cache"
#' )
#' }
translate_text <- function(text,
                           target_language = "en",
                           source_language = "en",
                           cache_path = tempdir()) {
  # Check if source and target languages are the same
  if (source_language == target_language) {
    cli::cli_inform(
      c(
        "i" = paste0(
          "Source and target languages are the ",
          "same. Returning original text."
        )
      )
    )
    return(text)
  }

  # Set up cache
  if (!dir.exists(cache_path)) {
    dir.create(cache_path, recursive = TRUE, showWarnings = FALSE)
  }

  cache_file <- file.path(cache_path, "translation_cache.rds")

  # Ensure digest is available
  if (!requireNamespace("digest", quietly = TRUE)) {
    stop("Package 'digest' is required for translation caching")
  }

  # Load or initialize cache as a data frame
  if (file.exists(cache_file)) {
    cache <- readRDS(cache_file)
    # Convert legacy list cache to data frame if needed
    if (is.list(cache) && !is.data.frame(cache)) {
      cache <- data.frame(
        key = names(cache),
        text = unlist(cache, use.names = FALSE),
        original_text = text,
        to_lang = target_language,
        from_lang = source_language,
        translated_time = format(Sys.time(), tz = "UTC", usetz = TRUE),
        name_of_creator = Sys.getenv("RSTUDIO_USER_IDENTITY"),
        stringsAsFactors = FALSE
      )
    }
  } else {
    cache <- data.frame(
      key = character(),
      text = character(),
      original_text = character(),
      to_lang = character(),
      from_lang = character(),
      translated_time = character(),
      name_of_creator = character(),
      stringsAsFactors = TRUE
    )
  }

  # Create a unique key for this text and language combination
  source_suffix <- paste0("_from_", source_language)

  # Normalize text by removing accents for more consistent caching
  normalized_text <- text
  if (requireNamespace("stringi", quietly = TRUE)) {
    normalized_text <- stringi::stri_trans_general(text, "Latin-ASCII")
  }

  key <- digest::digest(paste0(
    normalized_text, "_",
    target_language, source_suffix
  ))

  # Return from cache if available
  cache_match <- cache$key == key
  if (any(cache_match)) {
    match_idx <- which(cache_match)[1]
    saveRDS(cache, cache_file)
    return(as.character(cache$text[match_idx]))
  }

  # Check for internet connection
  has_internet <- function() {
    if (requireNamespace("curl", quietly = TRUE)) {
      curl::has_internet()
    } else {
      tryCatch(
        {
          con <- url("https://www.google.com", "r")
          close(con)
          TRUE
        },
        error = function(e) FALSE
      )
    }
  }

  if (!has_internet()) {
    message("No internet connection. Skipping translation.")
    return(text)
  }

  # Try to translate
  result <- tryCatch(
    {
      if (!requireNamespace("gtranslate", quietly = TRUE)) {
        stop("Package 'gtranslate' is required for translation")
      }
      gtranslate::translate(
        text,
        to = target_language,
        from = source_language
      )
    },
    error = function(e) {
      message("Translation failed: ", e$message)
      text
    }
  )

  # Cache if successful
  if (!identical(result, text)) {
    cache <- rbind(cache, data.frame(
      key = key,
      text = result,
      original_text = text,
      to_lang = target_language,
      from_lang = source_language,
      translated_time = format(Sys.time(), tz = "UTC", usetz = TRUE),
      name_of_creator = Sys.getenv("RSTUDIO_USER_IDENTITY"),
      stringsAsFactors = TRUE
    ))
    rownames(cache) <- NULL
    saveRDS(cache, cache_file)
  }

  result
}

#' Vectorized version of translate_text function
#'
#' This function applies the translate_text function to each element of a
#' character vector, allowing for batch translation of multiple text strings.
#' It preserves the original text if translation fails or returns NA.
#'
#' @param text A character vector containing strings to translate
#' @param ... Additional arguments passed to translate_text function, such as:
#'   \itemize{
#'     \item target_language: Target language code (default: "en")
#'     \item source_language: Source language code (default: "en")
#'     \item cache_path: Path to directory for storing the cache file
#'   }
#'
#' @return A character vector of the same length as the input, containing
#'   translated strings or the original strings if translation failed
#'
#' @seealso \code{\link{translate_text}} for translating individual strings
#'
#' @examples
#' \dontrun{
#' # Translate multiple strings from English to French
#' translate_text_vec(
#'   c("Hello world", "Good morning", "Thank you"),
#'   target_language = "fr"
#' )
#'
#' # Translate with custom cache location
#' translate_text_vec(
#'   c("Data analysis", "Statistical model"),
#'   target_language = "es",
#'   cache_path = "~/translation_cache"
#' )
#' }
#'
#' @export
translate_text_vec <- function(text, ...) {
  data.frame(original = text) |>
    dplyr::mutate(
      translated = purrr::map_chr(
        original,
        function(x) {
          res <- translate_text(x, ...)
          if (length(res) == 0 || is.na(res)) {
            return(x)
          }
          res
        }
      )
    ) |>
    dplyr::pull(translated)
}
