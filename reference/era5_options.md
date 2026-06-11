# List Available ERA5 Dataset Options

Returns a tibble with supported ERA5 datasets available for download
from the Copernicus Climate Data Store (CDS). Each entry includes the
dataset code, descriptive label, CDS identifier, and common variables.

## Usage

``` r
era5_options()
```

## Value

A tibble with columns:

- dataset:

  Machine-readable dataset code (e.g., `"daily_single_levels"`)

- label:

  Descriptive label for user display

- cds_id:

  CDS dataset identifier

- product_type:

  Product type for CDS request (if applicable)

- common_vars:

  Example variable names for this dataset

- description:

  Brief description of the dataset type

## Examples

``` r
era5_options()
#> # A tibble: 4 × 6
#>   dataset                label       cds_id product_type common_vars description
#>   <chr>                  <chr>       <chr>  <chr>        <chr>       <chr>      
#> 1 daily_single_levels    Daily Stat… deriv… NA           2m_tempera… Pre-aggreg…
#> 2 monthly_single_levels  Monthly Me… reana… monthly_ave… 2m_tempera… Pre-aggreg…
#> 3 hourly_single_levels   Hourly Dat… reana… reanalysis   2m_tempera… Hourly atm…
#> 4 hourly_pressure_levels Hourly Dat… reana… reanalysis   temperatur… Hourly upp…
```
