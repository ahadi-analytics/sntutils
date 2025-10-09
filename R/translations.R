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
    "fr" = rlang::`%||%`(grep("^fr_.*\\.UTF-8$", available_locales, value = TRUE)[1], "fr_FR.UTF-8"),
    "en" = rlang::`%||%`(grep("^en_.*\\.UTF-8$", available_locales, value = TRUE)[1], "en_US.UTF-8"),
    "es" = rlang::`%||%`(grep("^es_.*\\.UTF-8$", available_locales, value = TRUE)[1], "es_ES.UTF-8"),
    "de" = rlang::`%||%`(grep("^de_.*\\.UTF-8$", available_locales, value = TRUE)[1], "de_DE.UTF-8"),
    "it" = rlang::`%||%`(grep("^it_.*\\.UTF-8$", available_locales, value = TRUE)[1], "it_IT.UTF-8"),
    "pt" = rlang::`%||%`(grep("^pt_.*\\.UTF-8$", available_locales, value = TRUE)[1], "pt_PT.UTF-8"),
    "ru" = rlang::`%||%`(grep("^ru_.*\\.UTF-8$", available_locales, value = TRUE)[1], "ru_RU.UTF-8"),
    "zh" = rlang::`%||%`(grep("^zh_.*\\.UTF-8$", available_locales, value = TRUE)[1], "zh_CN.UTF-8"),
    "ja" = rlang::`%||%`(grep("^ja_.*\\.UTF-8$", available_locales, value = TRUE)[1], "ja_JP.UTF-8"),
    "ko" = rlang::`%||%`(grep("^ko_.*\\.UTF-8$", available_locales, value = TRUE)[1], "ko_KR.UTF-8"),
    "ar" = rlang::`%||%`(grep("^ar_.*\\.UTF-8$", available_locales, value = TRUE)[1], "ar_SA.UTF-8"),
    "hi" = rlang::`%||%`(grep("^hi_.*\\.UTF-8$", available_locales, value = TRUE)[1], "hi_IN.UTF-8")
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
  # optionally preprocess english->french to enforce malaria acronyms
  text_for_translation <- text
  if (
    base::identical(base::tolower(source_language), "en") &&
      base::identical(base::tolower(target_language), "fr")
  ) {
    text_for_translation <- .preprocess_en_to_fr_acronyms(text_for_translation)
  }
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

  # no hashing required; keys are human-readable, no extra deps

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
      stringsAsFactors = FALSE
    )
  }

  # Create a stable cache key based on the ORIGINAL input text + languages
  source_suffix <- paste0("_from_", source_language)

  normalized_text <- text
  if (requireNamespace("stringi", quietly = TRUE)) {
    normalized_text <- stringi::stri_trans_general(text, "Latin-ASCII")
  }

  key <- paste0(normalized_text, "_", target_language, source_suffix)

  # Return from cache if available
  # compare using character keys to avoid factor pitfalls
  cache_keys_chr <- base::as.character(cache$key)
  cache_match <- cache_keys_chr == key
  if (any(cache_match)) {
    match_idx <- which(cache_match)[1]
    cached_text <- as.character(cache$text[match_idx])
    if (tolower(target_language) == "fr") {
      cached_text <- .apply_fr_translation_fixups(cached_text)
      cache$text[match_idx] <- cached_text
    }
    saveRDS(cache, cache_file)
    return(as.character(cached_text))
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
        text_for_translation,
        to = target_language,
        from = source_language
      )
    },
    error = function(e) {
      message("Translation failed: ", e$message)
      text
    }
  )

  if (length(result) == 0 || all(is.na(result))) {
    result <- text
  }

  # for EN -> FR, enforce acronyms on fresh results as well
  if (tolower(target_language) == "fr") {
    result <- .apply_fr_translation_fixups(result)
  }

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
      stringsAsFactors = FALSE
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

# french acronyms --------------------------------------------------------------

#' French malaria acronyms mapping
#'
#' returns a tibble of English<->French acronyms and their full phrases used
#' in malaria program contexts. the first French acronym listed is treated
#' as the preferred one when enforcing acronyms.
#'
#' @return tibble with columns: english_acronyms (list), english_full,
#'   french_acronyms (list), french_full, preferred_french_acronym
#'
#' @examples
#' tbl <- french_malaria_acronyms()
#' tbl$preferred_french_acronym
#'
#' @export
french_malaria_acronyms <- function() {
  # raw rows, semicolon-separated variants
  rows <- list(
    list("CHW", "Community health worker", "ASBC; ASC",
         "Agents Sant\u00e9 de Base Communautaire"),
    list("", "case management", "PEC",
         "Prise en charge des cas, gestion de cas"),
    list("", "case management at the household", "PECADOM",
         "Prise en charge des cas \u00e0 domicile"),
    list("iCCM", "Integrated community case management", "iCCM",
         "Prise en charge communautaire int\u00e9gr\u00e9e des cas"),
    list("ACT", "Artemisinin-based combination therapy", "CTA",
         "Th\u00e9rapie combin\u00e9e \u00e0 base d'art\u00e9misinine"),
    list("SMC", "Seasonal Malaria Chemoprevention", "CPS",
         "La chimiopr\u00e9vention du paludisme saisonnier"),
    list("IPTp", "Intermittent preventive treatment in pregnancy", "TPIg",
         "Le traitement pr\u00e9ventif intermittent pendant la grossesse"),
    list("IPTi", "Intermittent preventive treatment in infants", "TPIn",
         "Le traitement pr\u00e9ventif intermittent du nourrisson"),
    list("IRS", "Indoor residual spraying", "PID",
         "Pulv\u00e9risation intra-domiciliaire"),
    list("LLIN; ITN", "Long-lasting insecticidal nets",
         "MILDA; MILD; MII",
         "Moustiquaire impr\u00e9gn\u00e9e \u00e0 longue dur\u00e9e d\u2019action"),
    list("EPI", "Expanded Programme on Immunization", "PEV",
         "Programme \u00e9largi de vaccination"),
    list("WHO", "World Health Organization", "OMS",
         "Organisation mondiale de la sant\u00e9"),
    list("GF", "Global Fund", "FM", "Fonds Mondial"),
    list("DHS", "Demographic and Health Survey", "EDS",
         "Enqu\u00eate d\u00e9mographique et de sant\u00e9"),
    list("MIS", "Malaria Indicators Survey", "EPI; EIPAG",
         "Les enqu\u00eates sur les indicateurs du paludisme"),
    list("", "Health district", "DS", "District sanitaire"),
    list("BAU", "Business as usual", "CSP",
         "Continuation de la strat\u00e9gie pr\u00e9c\u00e9dente"),
    list("U5", "under five years old", "M5", "moins de cinq ans"),
    list("U1", "under one year old", "M1", "moins d'un an"),
    list("PMC", "perennial malaria chemoprevention", "CPP",
         "Chimiopr\u00e9vention du paludisme p\u00e9renne"),
    list("RDT", "rapid diagnostic test", "TDR", "test diagnostique rapide"),
    list("NSP", "national strategic plan", "PSN", "plan strat\u00e9gique national"),
    list("FR", "(GFATM) funding request", "DF", "demande de financement"),
    list("HF", "health facility", "FS", "formation sanitaire")
  )

  # convert into columns
  split_trim <- function(s) {
    if (base::is.na(s) || !base::nzchar(s)) return(character())
    parts <- base::unlist(base::strsplit(s, ";", fixed = TRUE))
    base::trimws(parts)
  }

  english_acronyms <- lapply(rows, function(r) split_trim(r[[1]]))
  english_full <- vapply(rows, function(r) r[[2]], character(1))
  french_acronyms <- lapply(rows, function(r) split_trim(r[[3]]))
  french_full <- vapply(rows, function(r) r[[4]], character(1))

  preferred <- vapply(
    french_acronyms,
    function(v) if (base::length(v)) v[[1]] else NA_character_,
    character(1)
  )

  tibble::tibble(
    english_acronyms = english_acronyms,
    english_full = english_full,
    french_acronyms = french_acronyms,
    french_full = french_full,
    preferred_french_acronym = preferred
  )
}

# internal: preprocess en->fr by enforcing acronyms
# replaces english acronyms or full phrases with preferred french acronym
# prior to translation so the acronym survives translation intact.
# @noRd
.preprocess_en_to_fr_acronyms <- function(text) {
  map_tbl <- french_malaria_acronyms()
  out <- base::as.character(text)

  # First handle reporting rate terms before malaria acronyms
  # Handle "weighted reporting rate" first (before generic "reporting rate")
  out <- base::gsub(
    "(?i)weighted\\s+reporting\\s+rate(s?)",
    "taux de rapport pond\u00e9r\u00e9",
    out,
    perl = TRUE
  )
  
  # Then handle generic "reporting rate"
  out <- base::gsub(
    "(?i)reporting\\s+rate(s?)",
    "taux de rapport",
    out,
    perl = TRUE
  )

  # iterate rows and replace malaria acronyms
  for (i in base::seq_len(nrow(map_tbl))) {
    fr_acr <- map_tbl$preferred_french_acronym[[i]]
    if (!base::nzchar(fr_acr)) next

    # english acronyms
    en_acrs <- map_tbl$english_acronyms[[i]]
    if (base::length(en_acrs)) {
      for (tok in en_acrs) {
        if (!base::nzchar(tok)) next
        # word-boundary, case-insensitive
        pattern <- base::paste0("\\b", tok, "\\b")
        out <- base::gsub(pattern, fr_acr, out, ignore.case = TRUE)
      }
    }

    # english full phrase
    en_full <- map_tbl$english_full[[i]]
    if (base::nzchar(en_full)) {
      out <- base::gsub(en_full, fr_acr, out, ignore.case = TRUE)
    }
  }

  out
}

# internal: apply french-specific translation fixups
# ensures cached and fresh translations share the same conventions
# @noRd
.apply_fr_translation_fixups <- function(text) {
  out <- text
  if (!base::is.character(out)) {
    out <- base::as.character(out)
  }
  na_mask <- base::is.na(out)
  fixers <- list(
    base::get0(".enforce_fr_acronyms_in_french", mode = "function"),
    .enforce_fr_reporting_terms
  )

  for (fn in fixers) {
    if (base::is.function(fn)) {
      out <- fn(out)
    }
  }

  out[na_mask] <- NA_character_

  out
}

# internal: enforce consistent french phrasing for reporting metrics
# @noRd
.enforce_fr_reporting_terms <- function(text) {
  out <- base::as.character(text)

  # Handle weighted reporting rate FIRST (before generic reporting rate)
  out <- base::gsub(
    "(?i)weighted\\s+reporting\\s+rate(s?)",
    "taux de rapport pond\u00e9r\u00e9",
    out,
    perl = TRUE
  )

  # Then handle generic reporting rate
  out <- base::gsub(
    "(?i)reporting\\s+rate(s?)",
    "taux de rapport",
    out,
    perl = TRUE
  )

  out <- base::gsub(
    "(?i)taux[\\s_]+(de|du|d['\u2019])?[\\s_]*d[\u00e9e]claration(s?)",
    "taux de rapport",
    out,
    perl = TRUE
  )

  out <- base::gsub(
    "(?i)taux_de_d[\u00e9e]claration(s?)",
    "taux de rapport",
    out,
    perl = TRUE
  )

  out <- base::gsub(
    "(?i)taux\\s+(de|du|d['\u2019]|the)\\s+rapportage(s?)",
    "taux de rapport",
    out,
    perl = TRUE
  )

  out <- base::gsub(
    "(?i)taux\\s+de\\s+rapportage\\s+pond\u00e9r\u00e9s?",
    "taux de rapport pond\u00e9r\u00e9",
    out,
    perl = TRUE
  )

  out <- base::gsub(
    "(?i)taux_de_rapportage_pond\u00e9r\u00e9s?",
    "taux de rapport pond\u00e9r\u00e9",
    out,
    perl = TRUE
  )

  # Also catch simple underscore patterns
  out <- base::gsub(
    "(?i)taux_de_rapportage",
    "taux de rapport",
    out,
    perl = TRUE
  )

  # Also handle "taux de rapport" with underscores that might be malformed
  out <- base::gsub(
    "(?i)taux_de_rapport(?!age)",
    "taux de rapport",
    out,
    perl = TRUE
  )



  out <- base::gsub("D['\u2019]", "d'", out, perl = TRUE)

  # Lowercase French articles (de, du) unless at start of string
  # Handle " De " -> " de " (not at start of string)
  out <- base::gsub("(?<!^)\\s+De\\s+", " de ", out, perl = TRUE)

  # Handle " Du " -> " du " (not at start of string)
  out <- base::gsub("(?<!^)\\s+Du\\s+", " du ", out, perl = TRUE)

  # Handle "_De_" and "_De " in filenames -> "_de_" and "_de "
  out <- base::gsub("_De([\\s_])", "_de\\1", out, perl = TRUE)

  # Handle "_Du_" and "_Du " in filenames -> "_du_" and "_du "
  out <- base::gsub("_Du([\\s_])", "_du\\1", out, perl = TRUE)
  out
}
