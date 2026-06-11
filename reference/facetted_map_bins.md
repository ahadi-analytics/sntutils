# Plot Faceted Choropleth Maps from sf Data with Discrete Bins

Creates a faceted choropleth map from an `sf` object using a binned
categorical variable for fill colors. The function is designed for
programmatic mapping of indicators over space and time, with optional
administrative boundary overlays and optional file export.

The function assumes that the fill variable is already discretised (e.g.
incidence bins, prevalence bins, rate categories) and mapped explicitly
to colors via `fill_colors`.

Typical use cases include:

- Mapping crude or adjusted incidence by district over multiple years

- Visualising binned prevalence or coverage indicators

- Producing consistent, publication-ready spatial outputs

## Usage

``` r
facetted_map_bins(
  data,
  fill_col,
  facet_col = NULL,
  facet_row = NULL,
  adm1_shp = NULL,
  fill_colors,
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
  This column should be a factor or character vector with levels
  matching `fill_colors`.

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

- fill_colors:

  Named character vector of colors. Names must correspond exactly to the
  levels in `fill_col`. Colors should be hex codes.

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

- Uses `scale_fill_manual()` with explicit legend control to ensure
  consistent color interpretation across plots.

- Faceting is implemented via `facet_wrap()` using a dynamically
  constructed formula.

- The function does not perform binning internally. All binning logic
  should be applied upstream.

## Examples

``` r
if (FALSE) { # \dontrun{
# example incidence color scale (cases per 1,000 population)
incidence_colors <- c(
  "0-5"       = "#c6dbef",
  "5-50"      = "#9ecae1",
  "50-100"    = "#4292c6",
  "100-250"   = "#fb6a4a",
  "250-500"   = "#cb181d",
  "500-5,000" = "#7f0000"
)

facetted_map_bins(
  data = incidence_shp,
  fill_col = "n1_incidence_bin",
  facet_col = "year",
  adm1_shp = shp_file$adm1,
  fill_colors = incidence_colors,
  title = "Crude Malaria Incidence by District (ADM2), Burundi",
  subtitle = "All-age crude incidence aggregated annually",
  fill_label = "Cases per 1,000 population",
  ncol = 3,
  output_file = here::here(
    "outputs",
    "burundi_crude_incidence.png"
  )
)
} # }
```
