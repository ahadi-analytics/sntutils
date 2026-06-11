# Plot Faceted Choropleth Maps from sf Data with Continuous Gradient

Creates a faceted choropleth map from an `sf` object using a continuous
numeric variable for fill colors displayed as a gradient. The function
is designed for programmatic mapping of continuous indicators over space
and time, with optional administrative boundary overlays and optional
file export.

The function expects the fill variable to be continuous numeric data
(e.g. incidence rates, prevalence percentages, continuous coverage
metrics) and maps it to a color gradient.

Typical use cases include:

- Mapping continuous incidence rates by district over multiple years

- Visualising continuous prevalence or coverage indicators

- Producing consistent, publication-ready spatial outputs with gradients

## Usage

``` r
facetted_map_gradient(
  data,
  fill_col,
  colors = c("#7b0d0d", "#c0392b", "#e67e22", "#f7dc6f", "#d6eaf8", "#5dade2", "#1a5276"),
  limits = NULL,
  facet_col = NULL,
  facet_row = NULL,
  adm1_shp = NULL,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  fill_label = NULL,
  ncol = 3,
  output_file = NULL,
  width = 7,
  height = 10,
  dpi = 300,
  scale = 1,
  title_size = 14,
  subtitle_size = 11,
  legend_title_size = 10,
  compress_image = TRUE
)
```

## Arguments

- data:

  An `sf` object containing geometries and attributes to plot. Must
  include the columns specified in `fill_col` and `facet_col`.

- fill_col:

  Character scalar. Name of the column in `data` used for polygon fill.
  This column should be numeric and contain continuous values.

- colors:

  Character vector of hex color codes for the gradient. The gradient
  will interpolate smoothly between these colors. Default is a 7-color
  diverging palette from dark red through yellow to dark blue.

- limits:

  Optional numeric vector of length 2 specifying the minimum and maximum
  values for the color scale (e.g., `c(0, 100)` for percentages). When
  `NULL`, limits are determined from the data range. Default is `NULL`.

- facet_col:

  Optional character scalar. Name of the column used for faceting
  columns, typically a time variable such as `year`. When `NULL`, no
  faceting is applied and a single map is produced. Default is `NULL`.

- facet_row:

  Optional character scalar. Name of the column used for faceting rows.
  When provided, the function uses `facet_grid(row ~ col)` instead of
  `facet_wrap()`. Default is `NULL`.

- adm1_shp:

  Optional `sf` object containing higher-level administrative boundaries
  (e.g. ADM1). When provided, boundaries are overlaid as thin black
  outlines. Default is `NULL`.

- title:

  Optional character scalar. Plot title. Default is `NULL` (no title).

- subtitle:

  Optional character scalar. Plot subtitle. Default is `NULL` (no
  subtitle).

- caption:

  Optional character scalar. Plot caption (annotation at bottom right).
  Default is `NULL` (no caption).

- fill_label:

  Optional character scalar. Legend title for the fill scale. Default is
  `NULL` (no legend title).

- ncol:

  Integer. Number of columns in the facet layout. Only used when
  `facet_row` is `NULL` (i.e., when using `facet_wrap()`). Default is
  `3`.

- output_file:

  Optional character scalar. File path where the plot should be saved.
  If `NULL`, the plot is not saved to disk.

- width:

  Numeric. Width of the saved plot in inches. Default is `7`.

- height:

  Numeric. Height of the saved plot in inches. Default is `10`.

- dpi:

  Numeric. Resolution (dots per inch) for saved output. Default is
  `300`.

- scale:

  Numeric. Scaling factor for the plot. Values greater than 1 make text
  and elements relatively smaller; values less than 1 make them larger.
  Default is `1`.

- title_size:

  Numeric. Font size for the plot title. Default is `14`.

- subtitle_size:

  Numeric. Font size for the plot subtitle. Default is `11`.

- legend_title_size:

  Numeric. Font size for the legend title. Default is `10`.

- compress_image:

  Logical. Compress PNG using
  [`compress_png()`](https://ahadi-analytics.github.io/sntutils/reference/compress_png.md)
  after saving. Defaults to TRUE.

## Value

A `ggplot` object. If `output_file` is provided, the plot is also
written to disk using `ggsave()`.

## Details

- Uses `geom_sf()` for spatial rendering.

- Uses `scale_fill_gradientn()` to create a smooth color gradient.

- Faceting is implemented via `facet_wrap()` or `facet_grid()` using a
  dynamically constructed formula.

- The function works directly with continuous numeric data without
  binning.

## Examples

``` r
if (FALSE) { # \dontrun{
# continuous incidence rates
facetted_map_gradient(
  data = incidence_shp,
  fill_col = "incidence_rate",
  facet_col = "year",
  adm1_shp = shp_file$adm1,
  title = "Continuous Malaria Incidence by District (ADM2), Burundi",
  subtitle = "All-age crude incidence aggregated annually",
  fill_label = "Cases per 1,000 population",
  ncol = 3,
  output_file = here::here(
    "outputs",
    "burundi_continuous_incidence.png"
  )
)

# percentage coverage with fixed 0-100 scale
facetted_map_gradient(
  data = coverage_shp,
  fill_col = "coverage_pct",
  limits = c(0, 100),
  facet_col = "year",
  title = "ITN Coverage (%)",
  fill_label = "Coverage (%)"
)
} # }
```
