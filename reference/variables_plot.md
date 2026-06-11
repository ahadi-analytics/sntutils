# Calculate and visualize reporting rates

Calculates and visualizes two key metrics:

1.  The reporting/missing rate of variables based on x and y dimensions

2.  The proportion of health facilities reporting a given variable

## Usage

``` r
variables_plot(
  plot_data,
  x_var,
  vars_of_interest,
  fill_var,
  fill_label,
  title_prefix,
  subtitle = NULL,
  include_plot_title = TRUE,
  plot_caption = NULL,
  common_elements,
  target_language = "en",
  source_language = "en",
  lang_cache_path = tempdir(),
  x_axis_breaks = 6
)
```

## Arguments

- plot_data:

  The data frame containing summarized health facility data

- x_var:

  The time variable for plotting (e.g., "year", "month")

- vars_of_interest:

  Variables to analyze for reporting rates

- fill_var:

  The column to use for fill values ("reprate" or "missrate")

- fill_label:

  Label for the fill scale

- title_prefix:

  Title prefix based on whether showing reporting or missing rates

- subtitle:

  Optional subtitle text to display under the title. Default NULL.

- include_plot_title:

  Logical. If TRUE, plot titles are included. Default TRUE.

- plot_caption:

  Optional caption text to display at the bottom of the plot. Default
  NULL.

- common_elements:

  Common ggplot elements to apply to all plots

- target_language:

  Language code for labels (ISO 639-1), defaults to "en"

- source_language:

  Source language code, defaults to NULL

- lang_cache_path:

  Path for translation cache, defaults to tempdir()

- x_axis_breaks:

  Numeric value specifying the interval for x-axis breaks. Default `6`.
  For example, `2` shows every second tick and `6` every sixth.

## Value

A ggplot2 object
