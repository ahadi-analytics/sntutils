# Download Population Rasters from WorldPop

Downloads population raster files from WorldPop for specified countries
and years. Automatically selects the appropriate dataset based on years:
legacy (2000-2020) or R2025A (2015-2030).

## Usage

``` r
download_worldpop(
  country_codes,
  years = 2000:2020,
  type = "count",
  resolution = "1km",
  dest_dir = here::here(),
  quiet = FALSE
)
```

## Arguments

- country_codes:

  Character vector of ISO country codes (e.g., "GBR", "GIN"). Pass
  `"GLOBAL"` (case-insensitive) to download the worldwide mosaic from
  the `0_Mosaicked` directory instead of a per-country raster.
  `"GLOBAL"` only works for years \>= 2015 at 1km resolution and
  `type = "count"`.

- years:

  Numeric vector of years to download data for (2000-2030).

- type:

  Character; either "density" for persons per sq km or "count" for total
  population count (default: "count"). Note: density is only available
  for years 2000-2020.

- resolution:

  Character; either "1km" (default) or "100m". The 100m resolution is
  only available for years 2015-2030 and is not available for the GLOBAL
  mosaic.

- dest_dir:

  Destination directory for downloaded files (default: current dir)

- quiet:

  Logical; if TRUE, suppresses progress messages (default: FALSE)

## Value

Invisible list containing:

- files: Vector of paths to downloaded/existing files

- counts: Named vector of successful downloads per country

## Details

The function automatically selects the appropriate WorldPop dataset:

### Legacy Dataset (years \< 2015)

Downloads UN-adjusted population rasters from WorldPop's Global
2000-2020 dataset. Available at 1km resolution only. Supports both count
and density.

### R2025A Dataset (years \>= 2015)

Downloads constrained population count rasters from WorldPop's Global
2015-2030 dataset (R2025A release). Available at 1km and 100m
resolution. Population is constrained to built-up areas.

If your year range spans both datasets (e.g., 2010:2020), the function
will automatically download from both sources.

Files are downloaded to the specified directory, with existing files
skipped. Progress is shown during downloads and a summary is provided
upon completion.

## Examples

``` r
if (FALSE) { # \dontrun{
# Download population data for Guinea (auto-selects dataset)
download_worldpop("GIN", years = 2015:2020)

# Download legacy data (2000-2014)
download_worldpop("GIN", years = 2000:2010)

# Download spanning both datasets
download_worldpop("GIN", years = 2010:2020)

# Download 100m resolution data (R2025A only)
download_worldpop("GIN", years = 2020, resolution = "100m")
} # }
```
