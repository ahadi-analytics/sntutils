# Convert date to yearmon format with localized month names

This function converts a date to yearmon format (Month Year) with the
month name displayed in the specified language.

## Usage

``` r
translate_yearmon(date, language = "fr", format = "%b %Y")
```

## Arguments

- date:

  A date object or character string that can be converted to a date

- language:

  A two-letter language code. Available options:

  - "fr" - French

  - "en" - English

  - "es" - Spanish

  - "de" - German

  - "it" - Italian

  - "pt" - Portuguese

  - "ru" - Russian

  - "zh" - Chinese

  - "ja" - Japanese

  - "ko" - Korean

  - "ar" - Arabic

  - "hi" - Hindi

- format:

  Format string for the output. Default is "%b %Y" for abbreviated month
  name and year. Use "%B %Y" for full month name.

## Value

A character string with the month and year in the specified language

## Examples

``` r
translate_yearmon(Sys.Date(), "en")
#> Warning: Locale NA not available. Using system default.
#> [1] "Jul 2026"
translate_yearmon(Sys.Date(), "fr")
#> Warning: Locale NA not available. Using system default.
#> [1] "Jul 2026"
translate_yearmon(Sys.Date(), "fr", format = "%B %Y")
#> Warning: Locale NA not available. Using system default.
#> [1] "July 2026"
```
