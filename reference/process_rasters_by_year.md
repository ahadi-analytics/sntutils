# Process Year-Indexed Rasters Against Time-Varying Admin Boundaries

Wrapper around
[`process_raster_with_boundaries()`](https://ahadi-analytics.github.io/sntutils/reference/process_raster_with_boundaries.md)
for the case where the admin boundaries change over time (one row per
admin x year in the shapefile, identified by a year column). For each
raster, detects the year from the filename, filters the shapefile to
just that year's boundaries, extracts zonal statistics against those
polygons, and row-binds the results.

## Usage

``` r
process_rasters_by_year(
  raster_files,
  shapefile,
  id_cols = c("adm0", "adm1", "adm2"),
  year_col = "year",
  aggregations = c("mean"),
  layer_to_process = 1,
  raster_is_density = FALSE,
  year_extractor = NULL
)
```

## Arguments

- raster_files:

  Character vector of raster file paths.

- shapefile:

  An `sf` object whose rows are admin units valid for a specific year.
  Each (admin x year) combination is a row; the year is stored in the
  column named by `year_col`.

- id_cols:

  Character vector of shapefile columns to carry into the output (e.g.,
  admin names or codes). Default: `c("adm0", "adm1", "adm2")`.

- year_col:

  Character. Name of the year column in `shapefile`. Default: `"year"`.

- aggregations:

  Vector of aggregation methods. Same options as for
  [`process_raster_with_boundaries()`](https://ahadi-analytics.github.io/sntutils/reference/process_raster_with_boundaries.md).
  Default: `c("mean")`. For population count rasters, use `"sum"`.

- layer_to_process:

  Integer or character. Layer index for multi-layer rasters. Default: 1.

- raster_is_density:

  Logical. Convert density to counts using cell area before extraction.
  Default: FALSE.

- year_extractor:

  Optional function `(filename) -> year`, used when the default "last
  plausible 4-digit year in filename" heuristic picks the wrong year.
  See
  [`extract_time_components()`](https://ahadi-analytics.github.io/sntutils/reference/extract_time_components.md).

## Value

A tibble with one row per (admin, year) combination, holding `id_cols`,
`year`, and the requested aggregation columns. Rasters whose year is not
present in the shapefile are skipped with a warning.

## Examples

``` r
if (FALSE) { # \dontrun{
# population rasters with year-varying districts
shp_by_year <- sf::read_sf("districts_by_year.gpkg")
raster_paths <- list.files(
  "data/worldpop/raw", pattern = "_pop_.*\\.tif$", full.names = TRUE
)
pop_by_admin <- process_rasters_by_year(
  raster_files = raster_paths,
  shapefile = shp_by_year,
  id_cols = c("adm0", "adm1", "adm2"),
  aggregations = "sum"
)
} # }
```
