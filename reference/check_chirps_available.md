# List Available CHIRPS Raster Files for a Dataset

Scrapes the UCSB CHIRPS archive to list available `.tif.gz` raster files
for a given dataset (e.g., `"africa_monthly"`). Extracts year and month
from filenames where possible.

## Usage

``` r
check_chirps_available(dataset_code = "africa_monthly")
```

## Arguments

- dataset_code:

  Character. One of the dataset codes from
  [`chirps_options()`](https://ahadi-analytics.github.io/sntutils/reference/chirps_options.md),
  such as `"africa_monthly"`.

## Value

A tibble with the following columns:

- file_name:

  The filename of the `.tif.gz` raster.

- year:

  Extracted year (YYYY) from filename.

- month:

  Extracted month (MM) from filename, if available.

- dataset:

  The dataset code queried.

Returns `NULL` if the dataset cannot be accessed.

## Examples

``` r
if (FALSE) { # \dontrun{
# List available Africa monthly files
check_chirps_available("africa_monthly")
} # }
```
