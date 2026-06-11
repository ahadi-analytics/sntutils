# List Available ERA5 Files for a Dataset

Checks what ERA5 data is available from CDS for a given dataset and time
period. This is informational only - actual availability depends on CDS
server status.

## Usage

``` r
check_era5_available(
  dataset_code = "daily_single_levels",
  years = 2020,
  months = 1:12,
  variables = NULL
)
```

## Arguments

- dataset_code:

  Character. One of the dataset codes from
  [`era5_options()`](https://ahadi-analytics.github.io/sntutils/reference/era5_options.md).

- years:

  Integer vector. Years to check (e.g., `2020:2023`).

- months:

  Integer vector. Months to check (1-12). Default is all months.

- variables:

  Character vector. CDS variable names to display info for.

## Value

A tibble with dataset information.

## Examples

``` r
if (FALSE) { # \dontrun{
check_era5_available("daily_single_levels", years = 2020, months = 1:3,
                     variables = c("2m_temperature"))
} # }
```
