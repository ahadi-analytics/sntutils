# Get translated terms for plot filenames

Get translated terms for plot filenames

## Usage

``` r
get_translated_terms(
  target_language,
  source_language,
  lang_cache_path,
  x_var,
  vars_of_interest,
  save_title_prefix,
  data
)
```

## Arguments

- target_language:

  Target language code

- source_language:

  Source language code

- lang_cache_path:

  Path for translation cache

- x_var:

  X-axis variable

- vars_of_interest:

  Variables being visualized

- save_title_prefix:

  A string prefix for the plot title and filename. If NULL, a default
  prefix will be used based on the visualization type.

- data:

  Original data for year range

## Value

List of translated terms
