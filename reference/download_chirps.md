# Download CHIRPS Raster Data from UCSB Archive

Downloads `.tif.gz` CHIRPS rainfall data files from the UCSB Climate
Hazards Group archive for a specified dataset and date range. Supports
monthly, dekadal (3 per month), and pentadal (6 per month) resolutions.
Files are downloaded and optionally unzipped to a local directory.

## Usage

``` r
download_chirps(dataset, start, end = NULL, out_dir = ".", unzip = TRUE)
```

## Arguments

- dataset:

  Character. One of the dataset codes listed in
  [`chirps_options()`](https://ahadi-analytics.github.io/sntutils/reference/chirps_options.md).

- start:

  Character. Start date in `"YYYY-MM"` format (e.g., `"2020-01"`).

- end:

  Character. End date in `"YYYY-MM"` format. If `NULL`, only `start`
  month is downloaded.

- out_dir:

  Directory path where downloaded files will be saved. Will be created
  if it does not exist.

- unzip:

  Logical. If `TRUE`, the `.tif.gz` files will be unzipped after
  download.

## Value

No return value. Side effects include downloading CHIRPS `.tif.gz` and
`.tif` files to the specified folder.

## Details

Use
[`chirps_options()`](https://ahadi-analytics.github.io/sntutils/reference/chirps_options.md)
to view all available datasets and their metadata.

## Examples

``` r
# View available datasets
chirps_options()
#> # A tibble: 4 × 4
#>   dataset             frequency label                                 subdir    
#>   <chr>               <chr>     <chr>                                 <chr>     
#> 1 global_monthly      monthly   Global (Monthly)                      global_mo…
#> 2 africa_monthly      monthly   Africa (Monthly)                      africa_mo…
#> 3 camer-carib_monthly monthly   Caribbean & Central America (Monthly) camer-car…
#> 4 EAC_monthly         monthly   East African Community (Monthly)      EAC_month…

# Download Africa monthly CHIRPS for Jan–Mar 2022
if (FALSE) { # \dontrun{
download_chirps(
  dataset = "africa_monthly",
  start = "2022-01",
  end = "2022-03",
  out_dir = "chirps_data"
)
} # }
```
