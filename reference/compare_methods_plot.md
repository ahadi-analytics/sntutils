# Compare facility activity classification methods (multilingual)

Runs three classification methods and produces pairwise comparison
plots, plus a time-series plot of active facilities over time.
Optionally saves the output to file with auto-language filenames.

## Usage

``` r
compare_methods_plot(
  data,
  hf_col,
  date_col,
  key_indicators,
  nonreport_window = 6,
  language = "en",
  plot_path = NULL,
  width = 16,
  height = 12,
  units = "in",
  dpi = 300,
  scale = 0.85,
  compress_image = FALSE
)
```

## Arguments

- data:

  Facility reporting dataset.

- hf_col:

  Health facility ID column.

- date_col:

  Date column.

- key_indicators:

  Indicators used for activity classification.

- nonreport_window:

  Window size for non-reporting definition.

- language:

  Output language: "en" (English), "fr" (French), "pt" (Portuguese).

- plot_path:

  Directory where plot should be saved (NULL = don't save).

- width:

  Plot width (default 15).

- height:

  Plot height (default 6).

- units:

  Units for width/height ("in", "cm", "mm"). Default "in".

- dpi:

  Resolution in dots per inch. Default 300.

- scale:

  Multiplicative scale factor for size. Default 1.

- compress_image:

  Logical. Compress PNG using
  [`compress_png()`](https://ahadi-analytics.github.io/sntutils/reference/compress_png.md)
  after saving. Defaults to FALSE.

## Value

A patchwork object with scatterplots and a time-series plot.
