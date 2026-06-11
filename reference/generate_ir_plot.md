# Generate insecticide resistance trend plot

Creates a faceted time-series plot showing insecticide resistance or
mortality trends with multiple projection scenarios.

Inspired by
[`facetted_map_gradient()`](https://ahadi-analytics.github.io/sntutils/reference/facetted_map_gradient.md)
from sntutils. Supports flexible value columns, faceting, and automatic
PNG compression.

## Usage

``` r
generate_ir_plot(
  data,
  value_col = "mean_ir",
  facet_col = "adm1",
  ncol = 4,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  y_label = "Mortalité (mean)\n",
  legend_title = NULL,
  continue_trend_start_year = 2010,
  year_cut = 2025,
  width = 16,
  height = 9,
  output_file = NULL,
  dpi = 300,
  compress_image = TRUE,
  image_overwrite = TRUE,
  compression_speed = 1,
  compression_verbose = TRUE,
  show_plot = TRUE
)
```

## Arguments

- data:

  Data frame containing pre-filtered IR data with year, value column,
  facet column, and trendmethod column.

- value_col:

  Character scalar. Name of y-axis value column. Default is `"mean_ir"`.

- facet_col:

  Character scalar. Name of facetting column. Default is `"adm1"`.

- ncol:

  Numeric. Number of facet columns. Default is `4`.

- title:

  Character scalar. Plot title. If `NULL`, no title is shown.

- subtitle:

  Character scalar. Plot subtitle. If `NULL`, no subtitle is shown.

- caption:

  Character scalar. Plot caption. If `NULL`, no caption is shown.

- y_label:

  Character scalar. Y-axis label. Default is
  `"Mortalit\u00e9 (mean)\n"`.

- legend_title:

  Character scalar. Legend title for trend method. If `NULL`, a default
  title is generated.

- continue_trend_start_year:

  Numeric. First year to show in plot. Default is `2010`.

- year_cut:

  Numeric. Year to show vertical reference line indicating projection
  start. Default is `2025`.

- width:

  Numeric. Plot width in inches. Default is `16`.

- height:

  Numeric. Plot height in inches. Default is `9`.

- output_file:

  Optional character scalar. File path where the plot should be saved.
  If `NULL`, the plot is not saved to disk.

- dpi:

  Numeric. Resolution (dots per inch) for saved output. Default is
  `300`.

- compress_image:

  Logical. Compress PNG using
  [`compress_png()`](https://ahadi-analytics.github.io/sntutils/reference/compress_png.md)
  after saving. Default is `TRUE`.

- image_overwrite:

  Logical. Overwrite existing PNG during compression. Default is `TRUE`.

- compression_speed:

  Numeric. Compression speed from `1` (best quality, slowest) to `10`
  (fastest, lower quality). Default is `1`.

- compression_verbose:

  Logical. Print compression statistics. Default is `TRUE`.

- show_plot:

  Logical. Display the plot. If `FALSE`, returns plot object invisibly.
  Default is `TRUE`.

## Value

A `ggplot` object. If `output_file` is provided, the plot is also
written to disk using `ggsave()` and optionally compressed.

## Examples

``` r
if (FALSE) { # \dontrun{
ir_trends <- data.frame(
  adm0 = "BDI",
  adm1 = rep(c("Province A", "Province B"), each = 48),
  year = rep(2010:2030, 4),
  trendmethod = rep(c("constantTrend", "hybridTrend"), each = 42),
  mean_ir = runif(96, 0.6, 0.95)
)

plot_obj <- generate_ir_plot(
  data = ir_trends,
  value_col = "mean_ir",
  facet_col = "adm1",
  title = "Deltamethrin Mortality Trends",
  output_file = here::here("outputs", "ir_trends.png")
)
} # }
```
