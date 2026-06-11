# Consistency violation map

Creates a map showing the number of periods where a logical consistency
violation occurred. A violation is defined as rows where the output
variable exceeds the input variable (output \> input). Results are
aggregated at the specified administrative level and optionally faceted
by a time variable.

## Usage

``` r
consistency_map(
  data,
  shapefile,
  input_var,
  output_var,
  adm_var,
  x_var = NULL,
  facet_ncol = 4,
  language = "en",
  plot_path = NULL,
  compress_image = FALSE,
  image_overwrite = TRUE,
  compression_speed = 1,
  compression_verbose = TRUE,
  plot_scale = 1,
  plot_width = NULL,
  plot_height = NULL,
  plot_dpi = 300,
  show_plot = TRUE
)
```

## Arguments

- data:

  Data frame containing the input and output variables.

- shapefile:

  sf object with administrative boundaries.

- input_var:

  Character. Upstream variable (e.g. "test").

- output_var:

  Character. Downstream variable (e.g. "conf").

- adm_var:

  Character. Administrative unit in both data and shapefile.

- x_var:

  Character. Optional time variable (e.g. "year", "yearmon"). When
  provided, the map is faceted by this variable.

- facet_ncol:

  Integer. Number of facet columns. Default is 4.

- language:

  Character. Language code: "en", "fr", or "pt". Default "en".

- plot_path:

  Character. Directory or full file path for saving output. Default
  NULL.

- compress_image:

  Logical. Compress final PNG. Default FALSE.

- image_overwrite:

  Logical. Overwrite existing file. Default TRUE.

- compression_speed:

  Integer 1-10. Default 1.

- compression_verbose:

  Logical. Default TRUE.

- plot_scale:

  Numeric. Default 1.

- plot_width:

  Numeric. Default NULL.

- plot_height:

  Numeric. Default NULL.

- plot_dpi:

  Numeric. Default 300.

- show_plot:

  Logical. Print plot. Default TRUE.

## Value

A ggplot2 object.

## Details

Designed to complement
[`consistency_check()`](https://ahadi-analytics.github.io/sntutils/reference/consistency_check.md).
This function summarises the number of violating rows within each
administrative unit and displays the magnitude spatially. Only a single
input-output pair is accepted per call.

Typical malaria cascade checks include:

- test vs conf

- susp vs test

- conf vs treat

## Examples

``` r
if (FALSE) { # \dontrun{
consistency_map(
  data = hf_data,
  shapefile = adm2_sf,
  input_var = "test",
  output_var = "conf",
  adm_var = "adm2",
  x_var = "year"
)
} # }
```
