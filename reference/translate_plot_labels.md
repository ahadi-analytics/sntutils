# Translate plot labels to specified language

Translate plot labels to specified language

## Usage

``` r
translate_plot_labels(
  plot,
  target_language,
  source_language = "en",
  lang_cache_path = tempdir()
)
```

## Arguments

- plot:

  A ggplot2 object

- target_language:

  Target language code

- source_language:

  Source language code (default: NULL)

- lang_cache_path:

  Path for translation cache (default: tempdir())

## Value

The plot with translated labels
