# List Available Monthly CHIRPS Dataset Options

Returns a tibble with supported **monthly** CHIRPS datasets available
for download using the
[`download_chirps()`](https://ahadi-analytics.github.io/sntutils/reference/download_chirps.md)
function. Each entry includes the dataset code, descriptive label, and
the subdirectory path on the CHIRPS FTP server where `.tif.gz` files are
stored.

## Usage

``` r
chirps_options()
```

## Value

A tibble with 3 columns:

- dataset:

  Machine-readable dataset code (e.g., `"africa_monthly"`)

- label:

  Descriptive label for user display

- subdir:

  Subdirectory path to the CHIRPS TIFF archive

## Examples

``` r
chirps_options()
#> # A tibble: 4 × 4
#>   dataset             frequency label                                 subdir    
#>   <chr>               <chr>     <chr>                                 <chr>     
#> 1 global_monthly      monthly   Global (Monthly)                      global_mo…
#> 2 africa_monthly      monthly   Africa (Monthly)                      africa_mo…
#> 3 camer-carib_monthly monthly   Caribbean & Central America (Monthly) camer-car…
#> 4 EAC_monthly         monthly   East African Community (Monthly)      EAC_month…
```
