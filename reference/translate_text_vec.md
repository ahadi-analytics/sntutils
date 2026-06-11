# Vectorized version of translate_text function

This function applies the translate_text function to each element of a
character vector, allowing for batch translation of multiple text
strings. It preserves the original text if translation fails or returns
NA.

## Usage

``` r
translate_text_vec(text, ...)
```

## Arguments

- text:

  A character vector containing strings to translate

- ...:

  Additional arguments passed to translate_text function, such as:

  - target_language: Target language code (default: "en")

  - source_language: Source language code (default: "en")

  - cache_path: Path to directory for storing the cache file

## Value

A character vector of the same length as the input, containing
translated strings or the original strings if translation failed

## See also

[`translate_text`](https://ahadi-analytics.github.io/sntutils/reference/translate_text.md)
for translating individual strings

## Examples

``` r
if (FALSE) { # \dontrun{
# Translate multiple strings from English to French
translate_text_vec(
  c("Hello world", "Good morning", "Thank you"),
  target_language = "fr"
)

# Translate with custom cache location
translate_text_vec(
  c("Data analysis", "Statistical model"),
  target_language = "es",
  cache_path = "~/translation_cache"
)
} # }
```
