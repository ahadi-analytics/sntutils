#' Translate text to target language with persistent file cache
#'
#' Uses gtranslate, and caches successful translations in a file (RDS).
#' The cache is implemented as a data frame with metadata for better tracking
#' and management of translations.
#'
#' @param text Character string to translate
#' @param target_language Target language code (default: "en")
#' @param source_language Source language code (default: NULL for auto-detection)
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
#' # Translate to French with auto-detection of source
#' translate_text("Hello world", target_language = "fr")
#'
#' # Use custom cache location
#' translate_text("Hello world",
#'     target_language = "de",
#'     cache_path = "~/translation_cache"
#' )
#' }
translate_text <- function(text,
                           target_language = "en",
                           source_language = NULL,
                           cache_path = tempdir()) {
    # Check if source and target languages are the same
    if (!is.null(source_language) && source_language == target_language) {
        cli::cli_inform(c(
            "i" = "Source and target languages are the same. Returning original text.",
            "i" = "Run `rlang::last_trace()` to see where this occurred."
        ))
        return(text)
    }

    # Prompt user when target is English and source is NULL
    if (target_language == "en" && is.null(source_language)) {
        cli::cli_abort(c(
            "!" = "Target language is English but source language is not specified.",
            "i" = "Consider specifying source_language for better results."
        ))
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
                usage_count = 1,
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
            usage_count = integer(),
            stringsAsFactors = FALSE
        )
    }

    # Create a unique key for this text and language combination
    source_suffix <- if (!is.null(source_language)) {
        paste0("_from_", source_language)
    } else {
        ""
    }

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
        # Update usage count
        cache$usage_count[match_idx] <- cache$usage_count[match_idx] + 1
        saveRDS(cache, cache_file)
        return(cache$text[match_idx])
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
            usage_count = 1,
            stringsAsFactors = FALSE
        ))
        saveRDS(cache, cache_file)
    }

    result
}
