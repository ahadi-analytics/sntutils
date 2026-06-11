# Download ERA5 Data from Copernicus CDS

Downloads ERA5 data from the Copernicus Climate Data Store (CDS) using
the ecmwfr package. Supports daily and monthly pre-aggregated products.
Requires CDS API credentials.

## Usage

``` r
download_era5(
  dataset,
  cds_key = Sys.getenv("ERA5_API_KEY"),
  years,
  months,
  variables,
  daily_stat = c("mean", "maximum", "minimum", "sum"),
  daily_hfreq = c("1_hourly", "3_hourly", "6_hourly"),
  daily_tz = "UTC+00:00",
  hours = 0:23,
  pressure_levels = NULL,
  bbox = NULL,
  out_dir = "era5_data",
  overwrite = FALSE,
  aggregated_dir = NULL,
  aggregated_format = "qs2"
)
```

## Arguments

- dataset:

  Character. One of the dataset codes listed in
  [`era5_options()`](https://ahadi-analytics.github.io/sntutils/reference/era5_options.md).

- cds_key:

  Character. CDS API credentials. Defaults to
  `Sys.getenv("ERA5_API_KEY")`. For new CDS (ecmwfr \>= 2.0.0), use just
  the Personal Access Token (PAT). For old CDS, use "uid:apikey" format.
  Get from https://cds.climate.copernicus.eu/profile

- years:

  Integer vector. Years to download (e.g., `2020:2022`).

- months:

  Integer vector. Months to download (1-12).

- variables:

  Character vector. CDS variable names (e.g.,
  `c("2m_temperature", "total_precipitation")`).

- daily_stat:

  Character. For daily dataset: `"mean"`, `"maximum"`, `"minimum"`, or
  `"sum"`. Note: The daily statistics dataset can be unreliable.
  Consider using hourly data instead.

- daily_hfreq:

  Character. For daily dataset: `"1_hourly"`, `"3_hourly"`, or
  `"6_hourly"`.

- daily_tz:

  Character. Time zone for daily data (e.g., `"UTC+00:00"`).

- hours:

  Integer vector. Hours to download for hourly datasets (0-23). Default
  is all hours.

- pressure_levels:

  Integer vector. Pressure levels in hPa for pressure level data (e.g.,
  `c(1000, 850, 500)`).

- bbox:

  Numeric vector. Bounding box as `c(xmin, ymin, xmax, ymax)` in
  lon/lat.

- out_dir:

  Character. Directory path where files will be saved.

- overwrite:

  Logical. If `TRUE`, overwrite existing files.

- aggregated_dir:

  Character. Optional directory to save aggregated data files. If NULL
  (default), no aggregation is performed. If specified, downloads are
  read and aggregated by variable, with filenames like
  "era5_precip_Jan2020-Dec2024_v20250919.qs2"

- aggregated_format:

  Character. Format for aggregated files: "qs2" (default), "parquet",
  "feather", "rds", "csv"

## Value

A tibble with download information including file paths and status.

## Details

Use
[`era5_options()`](https://ahadi-analytics.github.io/sntutils/reference/era5_options.md)
to view all available datasets and their metadata.

## Examples

``` r
# View available datasets
era5_options()
#> # A tibble: 4 × 6
#>   dataset                label       cds_id product_type common_vars description
#>   <chr>                  <chr>       <chr>  <chr>        <chr>       <chr>      
#> 1 daily_single_levels    Daily Stat… deriv… NA           2m_tempera… Pre-aggreg…
#> 2 monthly_single_levels  Monthly Me… reana… monthly_ave… 2m_tempera… Pre-aggreg…
#> 3 hourly_single_levels   Hourly Dat… reana… reanalysis   2m_tempera… Hourly atm…
#> 4 hourly_pressure_levels Hourly Dat… reana… reanalysis   temperatur… Hourly upp…

if (FALSE) { # \dontrun{
# Download monthly temperature data (uses ERA5_API_KEY env var)
download_era5(
  dataset = "monthly_single_levels",
  years = 2020,
  months = 1:3,
  variables = c("2m_temperature"),
  bbox = c(29.0, -4.5, 30.9, -2.3),
  out_dir = "era5_data"
)

# Download daily precipitation totals
download_era5(
  dataset = "daily_single_levels",
  years = 2020,
  months = 1,
  variables = c("total_precipitation"),
  daily_stat = "sum",
  daily_hfreq = "1_hourly",
  daily_tz = "UTC+00:00",
  bbox = c(29.0, -4.5, 30.9, -2.3),
  out_dir = "era5_data"
)

# Download hourly temperature and wind data
download_era5(
  dataset = "hourly_single_levels",
  years = 2020,
  months = 1,
  variables = c(
    "2m_temperature",
    "10m_u_component_of_wind",
    "10m_v_component_of_wind"
  ),
  hours = c(0, 6, 12, 18),  # Every 6 hours
  bbox = c(29.0, -4.5, 30.9, -2.3),
  out_dir = "era5_data"
)

# Download pressure level data
download_era5(
  dataset = "hourly_pressure_levels",
  years = 2020,
  months = 1,
  variables = c("temperature", "relative_humidity"),
  pressure_levels = c(1000, 850, 500),  # Surface, 850hPa, 500hPa
  hours = c(0, 12),  # Twice daily
  bbox = c(29.0, -4.5, 30.9, -2.3),
  out_dir = "era5_data"
)
} # }
```
