# Download and process NASA POWER daily climate data

Downloads daily climate variables from the NASA POWER API for
representative administrative units, processes them into daily and
monthly datasets, and generates data dictionaries for each output.

## Usage

``` r
download_process_nasapower(
  adm_sf,
  admin_cols = c("adm0", "adm1", "adm2"),
  start_date = "2025-01-01",
  end_date = "2025-12-31",
  power_vars = c("PRECTOTCORR", "T2M_MAX", "T2M_MIN", "T2M", "TS", "TS_MAX", "TS_MIN",
    "RH2M", "T2MDEW"),
  power_community = "ag",
  max_retries = 3,
  dict_language = "fr"
)
```

## Arguments

- adm_sf:

  An sf object containing administrative polygons. Must include columns
  specified in `admin_cols` and have a valid CRS.

- admin_cols:

  Character vector of administrative identifier columns, ordered from
  highest to lowest level. The last column is treated as the unit
  identifier. Default is `c("adm0", "adm1", "adm2")`.

- start_date:

  Start date in `"YYYY-MM-DD"` format.

- end_date:

  End date in `"YYYY-MM-DD"` format.

- power_vars:

  Character vector of NASA POWER variable codes to download. Drop a code
  to exclude that variable from the outputs; add a code to opt in. Codes
  recognised by the rename map (clean column name in parentheses):
  `PRECTOTCORR` (rainfall_mm), `T2M` (air_temperature_c),
  `T2M_MAX`/`T2M_MIN` (max/min_air_temperature_c), `TS`
  (land_temperature_c), `TS_MAX`/`TS_MIN` (max/min_land_temperature_c),
  `RH2M` (humidity_pct), `T2MDEW` (dewpoint_c), `T2MWET`
  (wet_bulb_temperature_c), `QV2M` (specific_humidity_gkg), `PS`
  (surface_pressure_kpa), `WS2M`/`WS2M_MAX`/`WS2M_MIN` (wind_speed_ms),
  `ALLSKY_SFC_SW_DWN` (solar_radiation_kwhm2), `CLOUD_AMT`
  (cloud_amount_pct), `GWETTOP` (surface_soil_wetness), `GWETROOT`
  (root_soil_wetness), `GWETPROF` (profile_soil_wetness). Unrecognised
  codes are downloaded but dropped from the cleaned daily/monthly
  outputs.

- power_community:

  NASA POWER community parameter. Default is "ag" (agroclimatology).

- max_retries:

  Maximum retry attempts per unit. Default is 3.

- dict_language:

  Language for data dictionaries. Default is "fr".

## Value

A list with four elements:

- daily:

  Tibble of daily climate data with admin columns plus an `elevation_m`
  column (MERRA-2 grid-cell elevation, static per polygon, parsed from
  the POWER response metadata).

- monthly:

  Tibble of monthly aggregated climate data. For each requested
  variable: `mean_`, `median_`, `min_`, `max_` versions; rainfall
  additionally gets `total_`. `elevation_m` is carried through as a
  static column.

- dict_daily:

  Data dictionary for daily data

- dict_monthly:

  Data dictionary for monthly data

## Details

For each polygon, a single representative point (centroid) is used to
download data. Returned values correspond to NASA POWER grid-cell
averages (approximately 0.5 degree resolution) and should be interpreted
as regional conditions rather than point measurements.

The function is robust to partial download failures. Administrative
units that fail to download after the specified number of retries are
skipped with a warning. Execution stops only if no units return data.

## Examples

``` r
if (FALSE) { # \dontrun{
# load administrative boundaries
adm_sf <- sf::st_read("path/to/admin_boundaries.shp")

# download NASA POWER data for 2024
result <- download_process_nasapower(
  adm_sf = adm_sf,
  admin_cols = c("adm0", "adm1", "adm2"),
  start_date = "2024-01-01",
  end_date = "2024-12-31"
)

# access daily and monthly data
daily_data <- result$daily
monthly_data <- result$monthly
} # }
```
