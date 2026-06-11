# Process Weighted Raster Data in Batch

Processes multiple raster files with population weights, matching value
rasters with corresponding population rasters by year.

## Usage

``` r
process_weighted_raster_collection(
  value_raster_dir,
  pop_raster_dir,
  shapefile,
  id_cols = c("adm0", "adm1", "adm2"),
  value_pattern = "\\.tiff$",
  pop_pattern = "\\.tif$",
  value_layer_to_process = 1,
  weight_na_as_zero = TRUE,
  stat_type = "mean",
  year_extractor = NULL
)
```

## Arguments

- value_raster_dir:

  Character. Directory containing value raster files

- pop_raster_dir:

  Character. Directory containing population raster files

- shapefile:

  sf/SpatVector object. Shapefile for spatial aggregation

- id_cols:

  Character vector. Column names for spatial identifiers (default:
  c("adm0", "adm1", "adm2"))

- value_pattern:

  Character. Regex pattern to match value raster files (default:
  "\\tiff\$")

- pop_pattern:

  Character. Regex pattern to match population raster files (default:
  "\\tif\$")

- value_layer_to_process:

  Integer or character. Specifies which layer in a multi-layer raster to
  extract. If the raster contains multiple layers (e.g., different years
  or indicators), this argument selects the layer to be processed.
  Default is 1.

- weight_na_as_zero:

  Logical. Treat NA weights as 0 (default: TRUE)

- stat_type:

  Character string specifying statistic type:

  - "mean": Population-weighted mean using exactextractr's weighted_mean
    function

  - "median": Population-weighted median using
    matrixStats::weightedMedian() approach that finds the value where
    50% of the population weight lies below and 50% above, interpolating
    between pixel values when needed

  - "both": Returns both weighted mean and weighted median statistics
    (default: "mean")

- year_extractor:

  Optional function. A custom function that accepts a filename
  (character) and returns a year as integer or character. Passed through
  to
  [`extract_time_components()`](https://ahadi-analytics.github.io/sntutils/reference/extract_time_components.md).
  Use this when the default "last plausible 4-digit year in 1980-2099"
  heuristic picks the wrong year (for example with malariaAtlas
  filenames that contain multiple year-like tokens). Defaults to `NULL`,
  which uses the built-in extractor.

## Value

A data frame containing:

- Spatial identifiers from id_cols

- Weighted mean values

- Metadata (filename, year, month, semester, quarter, date)

## Examples

``` r
if (FALSE) { # \dontrun{
results <- process_weighted_raster_collection(
  value_raster_dir = "path/to/values",
  pop_raster_dir = "path/to/population",
  shapefile = my_shapefile,
  id_cols = c("adm0", "adm1")
)
} # }
```
