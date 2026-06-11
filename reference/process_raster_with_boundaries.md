# Process raster data with administrative boundaries

This function processes a single raster file by extracting zonal
statistics within administrative boundaries. It handles CHIRPS rainfall
data format by default, extracting dates from filenames, but works with
any raster data.

## Usage

``` r
process_raster_with_boundaries(
  raster_file,
  shapefile,
  id_cols = c("adm0", "adm1"),
  aggregations = c("mean"),
  raster_is_density = FALSE,
  layer_to_process = 1,
  pop_raster_file = NULL,
  weight_na_as_zero = TRUE,
  pattern_info = NULL,
  time_components = NULL
)
```

## Arguments

- raster_file:

  Path to raster file (.tif format)

- shapefile:

  sf/SpatVector object with administrative boundaries. Must be in the
  same coordinate reference system (CRS) as the raster data

- id_cols:

  Vector of column names from the shapefile to include in output.
  Default is c("adm0", "adm1") for country and first admin level

- aggregations:

  Vector of aggregation methods to calculate. Supported values are
  "mean", "sum", "median", "weighted_mean", and "weighted_median". The
  two weighted options require `pop_raster_file`. Default is c("mean")

- raster_is_density:

  Logical indicating if raster values represent density. If TRUE, values
  are converted from density to counts using cell area. Default is
  FALSE.

- layer_to_process:

  Integer or character. Specifies which layer in a multi-layer raster to
  extract. If the raster contains multiple layers (e.g., different years
  or indicators), this argument selects the layer to be processed.
  Default is 1

- pop_raster_file:

  Optional path to a population raster used as weights for
  `"weighted_mean"` and `"weighted_median"` aggregations. Reprojected
  and resampled to match `raster_file`. Default is `NULL`.

- weight_na_as_zero:

  Logical. If `TRUE`, missing weights are treated as zero; if `FALSE`,
  as `NA`. Only used when weighted aggregations are requested. Default
  is `TRUE`.

- pattern_info:

  Optional. Pre-parsed time pattern from
  [`detect_time_pattern()`](https://ahadi-analytics.github.io/sntutils/reference/detect_time_pattern.md).
  If NULL, detected from filename.

- time_components:

  Optional. Pre-parsed time components from
  [`extract_time_components()`](https://ahadi-analytics.github.io/sntutils/reference/extract_time_components.md).
  If NULL, extracted from filename.

## Value

A data frame containing:

- file_name: Name of processed raster file

- Selected ID columns from shapefile (specified by id_cols)

- Time columns based on detected pattern: - For monthly data: year,
  month - For yearly data: year

- Aggregated values with column names matching aggregation methods

## Details

The function performs the following steps:

1.  Validates requested aggregation methods

2.  Detects and extracts date information from filename

3.  Loads raster data and handles no-data values (-9999)

4.  If raster_is_density is TRUE, converts density to counts using cell
    area

5.  Calculates zonal statistics using exactextractr. When weighted
    aggregations are requested, loads the population raster, reprojects
    and resamples it to match the value raster, then uses it as per-cell
    weights

6.  Combines results with shapefile attributes

7.  Returns a clean data frame without geometry

## Examples

``` r
if (FALSE) { # \dontrun{
# Process single CHIRPS file with district-level boundaries
districts <- sf::st_read("districts.shp")
results <- process_raster_with_boundaries(
  raster_file = "chirps-v2.0.2022.01.tif",
  shapefile = districts,
  id_cols = c("COUNTRY", "DISTRICT"),
  aggregations = c("mean", "sum", "median")
)

# Population-weighted statistics alongside plain ones
results_weighted <- process_raster_with_boundaries(
  raster_file = "pfpr_2022.tif",
  shapefile = districts,
  id_cols = c("COUNTRY", "DISTRICT"),
  aggregations = c("mean", "weighted_mean", "weighted_median"),
  pop_raster_file = "worldpop_2022.tif"
)
} # }
```
