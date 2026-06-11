# Process multiple raster files from a directory

This function processes multiple raster files in a directory by applying
process_raster_with_boundaries() to each file and combining the results.
Includes progress tracking and error handling.

## Usage

``` r
process_raster_collection(
  directory,
  shapefile,
  id_cols = c("adm0", "adm1", "adm2"),
  pattern = "\\.tif$",
  aggregations = c("mean"),
  layer_to_process = 1,
  raster_is_density = FALSE,
  year_extractor = NULL
)
```

## Arguments

- directory:

  Directory containing raster files to process

- shapefile:

  sf/SpatVector object with administrative boundaries. Must be in the
  same coordinate reference system (CRS) as the raster data

- id_cols:

  Vector of column names from shapefile to include in output. Default is
  c("adm0", "adm1", "adm2") for country and first admin level

- pattern:

  Regular expression pattern to match raster files. Default is "\\tif\$"
  to match all .tif files

- aggregations:

  Vector of aggregation methods to calculate. Supported values are
  "mean", "sum", and "median". Default is c("mean")

- layer_to_process:

  Integer or character. Specifies which layer in a multi-layer raster to
  extract. If the raster contains multiple layers (e.g., different years
  or indicators), this argument selects the layer to be processed.
  Default is 1.

- raster_is_density:

  Logical indicating if raster values represent density. If TRUE, values
  are converted from density to counts using cell area. Default is
  FALSE.

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

A combined data frame containing results from all processed files,
sorted by time unit if available. Has the same structure as output from
process_raster_with_boundaries() but with multiple files combined.

## Details

The function:

1.  Lists all files in directory matching the pattern

2.  Creates a progress bar for tracking

3.  Processes each file using process_raster_with_boundaries()

4.  Combines results into a single data frame

5.  Sorts by time unit if available

## Examples

``` r
if (FALSE) { # \dontrun{
# Process all CHIRPS files in a directory
districts <- sf::st_read("districts.shp")
results <- process_raster_collection(
  directory = "chirps_data",
  shapefile = districts,
  id_cols = c("COUNTRY", "DISTRICT"),
  pattern = "chirps.*\\.tif$",
  aggregations = c("mean", "sum")
)
} # }
```
