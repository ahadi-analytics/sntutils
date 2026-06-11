# Translate text to target language with persistent file cache

Uses gtranslate, and caches successful translations in a file (RDS). The
cache is implemented as a data frame with metadata for better tracking
and management of translations.

## Usage

``` r
translate_text(
  text,
  target_language = "en",
  source_language = "en",
  cache_path = tempdir()
)
```

## Arguments

- text:

  Character string to translate

- target_language:

  Target language code (default: "en")

- source_language:

  Source language code (default: "en" for English)

- cache_path:

  Path to directory for storing the cache file

## Value

Translated text or original text if translation fails

## Examples

``` r
if (FALSE) { # \dontrun{
# Translate from Spanish to English
translate_text("Hola mundo", target_language = "en", source_language = "es")

# Translate to French with English as source
translate_text("Hello world", target_language = "fr")

# Use custom cache location
translate_text("Hello world",
  target_language = "de",
  cache_path = "~/translation_cache"
)
} # }
```
