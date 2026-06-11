# Save a single plot to a file

Save a single plot to a file

## Usage

``` r
save_single_plot(
  plot,
  plot_data,
  plot_path,
  x_var,
  y_var,
  y_axis_label,
  vars_of_interest,
  target_language = "en",
  source_language = "en",
  lang_cache_path = tempdir(),
  data,
  compression_options,
  use_reprate,
  save_title_prefix,
  plot_scale = 1,
  plot_width = NULL,
  plot_height = NULL,
  plot_dpi = 300,
  hf_col = NULL,
  require_all = FALSE,
  weighting = FALSE,
  weight_var = NULL,
  method = "method3",
  nonreport_window = 6,
  reporting_rule = "any_non_na"
)
```

## Arguments

- plot:

  The ggplot2 object to save

- plot_data:

  Prepared data frame for dimension calculations

- plot_path:

  Directory path to save the plot

- x_var:

  The time variable name

- y_var:

  The grouping variable name (if any)

- y_axis_label:

  Label for the y-axis

- vars_of_interest:

  Variables being visualized

- target_language:

  Language code for translation (default: "en")

- source_language:

  Source language code (default: NULL for auto-detection)

- lang_cache_path:

  Path for translation cache (default: tempdir())

- data:

  Original data for extracting year range

- compression_options:

  List with compression settings

- use_reprate:

  A logical value. If TRUE, the reporting rate is visualized; otherwise,
  the proportion of missing data is visualized. Defaults to TRUE

- save_title_prefix:

  A string prefix for the plot title and filename. If NULL, a default
  prefix will be used based on the visualization type.

- plot_scale:

  Numeric. Scaling factor for saved plots. Values \> 1 increase size, \<
  1 decrease size. Default is 1.

- plot_width:

  Numeric. Width of saved plot in inches. If NULL (default), width is
  calculated based on content.

- plot_height:

  Numeric. Height of saved plot in inches. If NULL (default), height is
  calculated based on content.

- plot_dpi:

  Numeric. Resolution of saved plot in dots per inch. Default is 300.

- hf_col:

  Character. Health facility ID column (default: NULL).

- require_all:

  Logical. If TRUE, requires all variables to be reported (default:
  FALSE).

- weighting:

  Logical. If TRUE, uses weighted reporting rates (default: FALSE).

- weight_var:

  Character. Variable to use for weighting (default: NULL).

- method:

  Character. Facility activity classification method (default:
  "method3").

- nonreport_window:

  Integer. Consecutive non-reporting months for inactivity (default: 6).

- reporting_rule:

  Character. What counts as reporting: "any_non_na" or "positive_only"
  (default: "any_non_na").

## Value

Invisible path to the saved file
