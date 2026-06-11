# Plot reporting rate maps over time

Creates faceted maps of reporting rates (or missing rates) by
administrative unit and time. Designed to complement
[`reporting_rate_plot()`](https://ahadi-analytics.github.io/sntutils/reference/reporting_rate_plot.md)
with consistent styling and color schemes. The function displays the
variables of interest in both the title and subtitle for clarity.

## Usage

``` r
reporting_rate_map(
  data,
  shapefile,
  x_var,
  adm_var,
  vars_of_interest,
  hf_col = "hf_uid",
  key_indicators = c("allout", "conf", "test", "treat", "pres"),
  use_reprate = TRUE,
  full_range = TRUE,
  show_title = TRUE,
  method = 3,
  nonreport_window = 6,
  reporting_rule = "any_non_na",
  require_all = FALSE,
  weighting = FALSE,
  weight_var = NULL,
  weight_window = 12,
  exclude_current_x = TRUE,
  cold_start = "median_within_y",
  fill_palette = "Zissou1",
  facet_label = NULL,
  facet_ncol = 4,
  target_language = "en",
  source_language = "en",
  lang_cache_path = tempdir(),
  plot_path = NULL,
  compress_image = FALSE,
  image_overwrite = TRUE,
  compression_speed = 1,
  compression_verbose = TRUE,
  plot_scale = 1,
  plot_width = NULL,
  plot_height = NULL,
  plot_dpi = 300,
  show_plot = TRUE,
  ...
)
```

## Arguments

- data:

  Data frame of health facility data.

- shapefile:

  sf object containing administrative boundaries (adm1 or adm2).

- x_var:

  Character. Time variable (e.g., "yearmon", "year", "month").

- adm_var:

  Character. Administrative variable in both `data` and `shapefile`
  (e.g., "adm1" or "adm2").

- vars_of_interest:

  Character vector. Variable(s) to compute reporting rates for. These
  will be displayed in the plot title and subtitle.

- hf_col:

  Character. Health facility ID column, if required. Default is NULL.

- key_indicators:

  Character vector. Indicator variables used to determine facility
  activity status. Facilities that never report any of these indicators
  will be excluded from denominators. Default is c("allout", "conf",
  "test", "treat", "pres").

- use_reprate:

  Logical. If TRUE, plot reporting rate (facilities that reported); if
  FALSE, plot missing rate (facilities that didn't report). Default is
  TRUE.

- full_range:

  Logical. If TRUE, the fill scale will use the full range from 0
  to 100. If FALSE, the fill scale will use the range of values present
  in the data. Default is TRUE.

- show_title:

  Logical. If TRUE, display the plot title. If FALSE, hide the title.
  Default is TRUE.

- method:

  Character or numeric. Classification method for facility activity
  status. Can be numeric (1, 2, 3) or character ("method1", "method2",
  "method3"). Defaults to 3. See
  [`classify_facility_activity`](https://ahadi-analytics.github.io/sntutils/reference/classify_facility_activity.md)
  for details.

- nonreport_window:

  Integer. Minimum number of consecutive non-reporting months to
  classify a facility as inactive in method 3. Defaults to 6.

- reporting_rule:

  Character. Defines what counts as reporting: `"any_non_na"` (default,
  counts NA as non-reporting, 0 counts as reported) or `"positive_only"`
  (requires \>0 value to count as reported).

- require_all:

  Logical. When TRUE and multiple vars_of_interest are provided,
  calculates the proportion of facilities reporting ALL variables
  (complete data). When FALSE (default), calculates per-variable
  reporting rates. Only applies to facility-level analysis (when hf_col
  is provided).

- weighting:

  Logical. If TRUE, compute weighted reporting rates based on facility
  size. Default is FALSE.

- weight_var:

  Character. Weight variable if weighting = TRUE.

- weight_window:

  Integer. Number of periods for typical size. Default is 12.

- exclude_current_x:

  Logical. Exclude current period in weighting. Default is TRUE.

- cold_start:

  Character. Cold-start strategy ("median_within_y" or "median_global").
  Default is "median_within_y".

- fill_palette:

  Character. Not used - kept for backward compatibility. Color palette
  is automatically selected based on use_reprate parameter to match
  reporting_rate_plot() styling.

- facet_label:

  Character. Optional custom label for the administrative level in the
  title. If NULL, uses adm_var value. Default is NULL.

- facet_ncol:

  Integer. Number of facet columns. Default is 4.

- target_language:

  Character. ISO 639-1 code for translation. Default "en".

- source_language:

  Character. Source language for translation. Default "en".

- lang_cache_path:

  Path to directory for storing translation cache. Default is tempdir().

- plot_path:

  Character. Path to save the plot. Can be either a directory path
  (filename will be auto-generated) or a full file path ending in .png.
  If NULL, plot is not saved.

- compress_image:

  Logical. If TRUE, compress the saved plot. Default is FALSE.

- image_overwrite:

  Logical. If TRUE, overwrite existing files. Default is TRUE.

- compression_speed:

  Integer. Speed/quality trade-off from 1 (brute-force) to 10 (fastest).
  Default is 1.

- compression_verbose:

  Logical. Controls output verbosity. FALSE = silent, TRUE = verbose.
  Default is TRUE.

- plot_scale:

  Numeric. Scaling factor for saved plots. Values \> 1 increase size, \<
  1 decrease size. Default is 1.

- plot_width:

  Numeric. Width of saved plot in inches. If NULL, width is calculated
  automatically based on data.

- plot_height:

  Numeric. Height of saved plot in inches. If NULL, height is calculated
  automatically based on data.

- plot_dpi:

  Numeric. Resolution of saved plot in dots per inch. Default is 300.

- show_plot:

  Logical. Display plot (TRUE) or return invisibly (FALSE). Default is
  TRUE.

- ...:

  Additional arguments passed to internal functions.

## Value

A ggplot2 object.

## Examples

``` r
if (FALSE) { # \dontrun{
# Example: adm1-level map over months
reporting_rate_map(
  data = hf_data,
  shapefile = adm1_sf,
  x_var = "month",
  adm_var = "district",
  vars_of_interest = "malaria"
)
} # }
```
