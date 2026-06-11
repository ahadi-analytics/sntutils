# Create Outlier Detection Plots

This function creates plots to visualize outliers in data using the same
detection methods as
[`detect_outliers()`](https://ahadi-analytics.github.io/sntutils/reference/detect_outliers.md).
Only values above the upper thresholds are highlighted as outliers. The
plotting function inherits all parameters from the detection function
for seamless integration.

## Usage

``` r
outlier_plot(
  data,
  column,
  record_id = "record_id",
  admin_level = c("adm1", "adm2"),
  spatial_level = "hf_uid",
  date = "date",
  time_mode = c("across_time", "within_year", "seasonal"),
  value_type = c("count", "rate"),
  strictness = c("balanced", "lenient", "strict", "advanced"),
  methods = c("iqr", "median", "mean", "consensus"),
  sd_multiplier = 3,
  mad_constant = 1.4826,
  mad_multiplier = 9,
  iqr_multiplier = 2,
  consensus_rule = 3,
  min_n = 8,
  reporting_rate_col = NULL,
  reporting_rate_min = 0.5,
  key_indicators_hf = NULL,
  classify_outbreaks = FALSE,
  outbreak_min_run = 2,
  outbreak_prop_tolerance = 0.9,
  outbreak_max_gap = 12,
  show_outbreaks = FALSE,
  consensus_colors = TRUE,
  return_plots = "all",
  year_breaks = 2,
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
  verbose = TRUE
)
```

## Arguments

- data:

  Data frame containing the indicator to analyse.

- column:

  Name of the numeric column to evaluate.

- record_id:

  Unique record identifier column.

- admin_level:

  Character vector of administrative level columns for parallel
  grouping, ordered from higher to lower resolution. Defaults to
  `c("adm1", "adm2")`.

- spatial_level:

  Character string specifying the finest spatial unit for analysis
  (e.g., "hf_uid" for facility-level). When specified, `admin_level`
  defines grouping boundaries while `spatial_level` defines the unit of
  analysis. This prevents excessive grouping while maintaining spatial
  granularity. Default is `hf_uid`.

- date:

  Date column (Date, POSIXt, or parseable character string). Year,
  month, and yearmon are automatically derived from this column.

- time_mode:

  Pooling strategy: `"across_time"`, `"within_year"`, or `"seasonal"`.
  Seasonal mode groups by month across all years (e.g., all Januaries
  together), useful for detecting values that are unusual for a specific
  month regardless of year.

- value_type:

  Indicator type: `"count"` or `"rate"`. Counts floor lower bounds at 0.

- strictness:

  Strictness preset: `"lenient"`, `"balanced"`, `"strict"`, or
  `"advanced"`. Presets map to method multipliers. If not `"advanced"`,
  any manual multipliers are **ignored**.

- methods:

  Character vector specifying which outlier detection methods to plot:
  "iqr" (Interquartile Range), "median" (Median Absolute Deviation),
  "mean" (Mean +/- SD), and/or "consensus". Default is
  `c("iqr", "median", "mean", "consensus")`. For consensus, at least two
  other methods must be selected.

- sd_multiplier:

  Width (in SD units) for the mean method (used only when
  `strictness = "advanced"`).

- mad_constant:

  Constant passed to [`stats::mad()`](https://rdrr.io/r/stats/mad.html)
  in advanced mode (default 1.4826).

- mad_multiplier:

  Width multiplier for the MAD method (advanced mode).

- iqr_multiplier:

  Tukey fence multiplier for the IQR method (advanced mode).

- consensus_rule:

  Number of methods that must agree (`1`, `2`, or `3`) for the consensus
  flag to call an outlier. Default `2`.

- min_n:

  Minimum observations required in the active comparison bucket before
  flagging is attempted (applies to any seasonal bucket or fallback).

- reporting_rate_col:

  Optional column with reporting completeness in `[0, 1]`.

- reporting_rate_min:

  Minimum acceptable reporting rate. Rows below the threshold receive
  `reason = "low_reporting"` and are not flagged.

- key_indicators_hf:

  Optional character vector of indicator names used to determine
  facility activeness. If supplied, the function uses a fast path to
  filter out inactive facility-months. A facility-month is considered
  active if ANY of the specified key indicators have non-NA values.
  Inactive facility-months are excluded from outlier detection. If
  `NULL` (default), activeness filtering is skipped. Typical indicators
  include `"allout"`, `"test"`, or `"conf"`. This adjustment prevents
  false positives caused by facilities that start or stop reporting
  mid-period.

- classify_outbreaks:

  Logical. When `TRUE` (default), applies outbreak classification to
  distinguish between isolated outliers and sustained outbreak patterns.
  Consecutive outliers meeting the outbreak criteria are reclassified
  from "outlier" to "outbreak". This is particularly useful for
  epidemiological surveillance to identify disease outbreak patterns.
  Set to `FALSE` to disable outbreak classification.

- outbreak_min_run:

  Integer. Minimum number of consecutive outliers required to classify
  as an outbreak (default `2`). Must be \>= 2.

- outbreak_prop_tolerance:

  Numeric. Proportional tolerance for outbreak consistency (default
  `0.9`). Values within this tolerance of the run median are considered
  consistent. Range: (0, 1).

- outbreak_max_gap:

  Integer. Maximum allowed gap (non-outlier months) between outliers
  that can still be considered part of the same outbreak (default `1`).
  For example, with `outbreak_max_gap = 12`, the pattern
  "outlier-normal-outlier-outlier" would be classified as one outbreak
  of length 3, rather than separate incidents. Set to `0` for strict
  consecutive-only outbreaks. Useful for real-world data with reporting
  gaps.

- show_outbreaks:

  Logical. When `TRUE`, displays outbreaks separately from outliers in
  the plot using a distinct color (#52AAC2 teal). The subtitle will show
  separate counts for outliers (red) and outbreaks (teal). When `FALSE`
  (default), outbreaks and outliers are shown together in red. Only
  relevant when `classify_outbreaks = TRUE`.

- consensus_colors:

  Logical. When `TRUE` (default), creates a single plot showing
  consensus strength across all methods using graduated colors. Point
  colors indicate how many methods flagged each observation: grey
  (normal), light red (1 method, weak signal), medium red (2 methods,
  moderate signal), dark red (3 methods, strong signal). Requires at
  least 2 detection methods (excluding "consensus"). When enabled,
  `show_outbreaks` is automatically set to `FALSE` as the two
  visualization modes are mutually exclusive. A legend is displayed
  showing the color-to-strength mapping. Set to `FALSE` for standard
  multi-method plot behavior (separate plot per method).

- return_plots:

  Character vector specifying which plots to return. Can be any subset
  of the methods being computed: "iqr", "median", "mean", "consensus",
  or "all" (default). For example, if
  `methods = c("iqr", "median", "consensus")` and
  `return_plots = c("iqr", "consensus")`, only the IQR and consensus
  plots will be returned. All computed plots are still created and saved
  (if `plot_path` is specified), but only the requested ones are
  returned.

- year_breaks:

  Numeric value specifying the interval for x-axis breaks. Default `2`.
  For example, `2` shows every second tick and `3` every third.

- target_language:

  Character string specifying the language for plot labels. Defaults to
  `"en"` (English). Use ISO 639-1 language codes.

- source_language:

  Source language code. If NULL, auto-detection is used. Defaults to
  "en".

- lang_cache_path:

  Path to directory for storing translation cache. Defaults to
  tempdir().

- plot_path:

  Character string specifying the path where plots should be saved. If
  NULL (default), plots are not saved.

- compress_image:

  Logical. If TRUE, will compress the saved plot. Defaults to FALSE

- image_overwrite:

  Logical. If TRUE, will overwrite existing files. Defaults to TRUE.

- compression_speed:

  Integer. Speed/quality trade-off from 1 (brute-force) to 10 (fastest).
  Default is 1.

- compression_verbose:

  Logical. Controls output verbosity. FALSE = silent, TRUE = verbose.
  Defaults to TRUE.

- plot_scale:

  Numeric. Scaling factor for saved plots. Values \> 1 increase size, \<
  1 decrease size. Default is 1.

- plot_width:

  Numeric. Width of saved plot in inches. If NULL (default), width is
  calculated based on number of facets.

- plot_height:

  Numeric. Height of saved plot in inches. If `NULL` (default), height
  is calculated from facet count.

- plot_dpi:

  Numeric. Resolution of saved plot in dots per inch. Default is 300.

- show_plot:

  Logical. If `FALSE`, the plot is returned invisibly (not displayed).
  Useful when only saving plots. Default is `TRUE`.

- verbose:

  Logical. When `TRUE`, prints an informative summary describing
  methods, pooling, strictness, guardrails, and consensus rule. Default
  is `FALSE`.

## Value

If a single method is specified, returns a ggplot object. If multiple
methods are specified, returns a named list of ggplot objects (one per
method). When `show_plot` is `FALSE`, returns invisibly.

## Details

The function creates scatter plots showing outliers detected using the
specified statistical methods. Each method produces a separate plot with
points colored by outlier status. By default, both outliers and
outbreaks are shown in red, with normal values in grey. When
`show_outbreaks = TRUE`, outbreaks are displayed in teal (#52AAC2) to
distinguish them from isolated outliers (red). For consensus plots, the
caption shows the consensus rule used. The plots are faceted by
administrative levels and include summary statistics in the subtitle.
Facet labels longer than 15 characters are automatically truncated with
"..." to prevent overcrowding.

## Examples

``` r
if (FALSE) { # \dontrun{
# Create plots for all methods (default) - returns named list
all_plots <- outlier_plot(
  data = malaria_data,
  column = "confirmed_cases",
  date = "date",
  record_id = "facility_id"
)
# Access individual plots: all_plots$iqr, all_plots$median, etc.

# Single method - returns single ggplot object
iqr_plot <- outlier_plot(
  data = malaria_data,
  column = "confirmed_cases",
  date = "date",
  record_id = "facility_id",
  methods = "iqr"
)

# Multiple specific methods
selected_plots <- outlier_plot(
  data = malaria_data,
  column = "confirmed_cases",
  date = "date",
  record_id = "facility_id",
  methods = c("iqr", "median"),
  time_mode = "by_month",
  strictness = "strict",
  year_breaks = 6
)

# Include consensus (requires 2+ other methods)
with_consensus <- outlier_plot(
  data = malaria_data,
  column = "confirmed_cases",
  date = "date",
  record_id = "facility_id",
  methods = c("iqr", "median", "mean", "consensus"),
  consensus_rule = 2,
  plot_path = "outliers.png"  # Saves as outliers_iqr.png, etc.
)

# Detect at facility level but visualize at district level
district_plots <- outlier_plot(
  data = malaria_data,  # Contains facility-level outlier detection results
  column = "confirmed_cases",
  date = "date",
  record_id = "facility_id",
  spatial_level = "facility_id",  # Detection at facility level
  admin_level = c("adm1", "adm2"),  # Plot/facet at district level
  methods = c("consensus")
)

# Show outbreaks separately in teal color
outbreak_plot <- outlier_plot(
  data = malaria_data,
  column = "confirmed_cases",
  date = "date",
  record_id = "facility_id",
  methods = "consensus",
  classify_outbreaks = TRUE,
  show_outbreaks = TRUE  # Outbreaks shown in teal, outliers in red
)

# Compute multiple methods but return only specific plots
selected <- outlier_plot(
  data = malaria_data,
  column = "confirmed_cases",
  date = "date",
  record_id = "facility_id",
  methods = c("iqr", "median", "mean", "consensus"),
  return_plots = c("iqr", "consensus"),  # Only return these two
  plot_path = "outliers.png"  # All methods still saved to disk
)
# Returns list with only $iqr and $consensus plots

# Consensus color visualization showing agreement strength
consensus_plot <- outlier_plot(
  data = malaria_data,
  column = "confirmed_cases",
  date = "date",
  record_id = "facility_id",
  methods = c("iqr", "median", "mean"),
  consensus_colors = TRUE  # Single plot with color-coded consensus strength
)
# Returns single ggplot with graduated colors:
# grey = normal, light red = 1 method, medium red = 2 methods, dark red = 3 methods
} # }
```
