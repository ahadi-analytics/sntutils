# Download WorldPop Population Raster Data for Specific Age Bands

Downloads WorldPop population rasters for the specified age bands and
years. When `sex = "both"` and `year >= 2015`, downloads the pre-summed
`"t"` (total) raster instead of fetching male and female separately and
summing locally. Automatically selects legacy (2000-2014) or R2025A /
R2024B (2015-2030) dataset based on year.

## Usage

``` r
download_worldpop_age_band(
  country_codes,
  years,
  age_range = c(1, 9),
  sex = "both",
  resolution = "1km",
  release = "R2025A",
  out_dir = ".",
  quiet = FALSE
)
```

## Arguments

- country_codes:

  Character vector. ISO3 country codes (e.g., "TUN", "BDI"). Pass
  `"GLOBAL"` (case-insensitive) to download the worldwide mosaic from
  the `0_Mosaicked` directory instead of a per-country raster.
  `"GLOBAL"` only works for years \>= 2015 at 1km resolution.

- years:

  Numeric vector. Years for which to download data (2000-2030).

- age_range:

  Numeric vector of length 2 specifying the lower and upper age range
  bounds (e.g., c(1, 9) for ages 1-9). Default: c(1, 9).

- sex:

  Character. Which sex to download: "both" (default, total population),
  "m" (male only), or "f" (female only). For years \>= 2015 `"both"` is
  satisfied with the single `t` (total) variant; for years \< 2015 male
  and female are downloaded separately and summed locally.

- resolution:

  Character. Either "1km" (default) or "100m". The 100m resolution is
  only available for years \>= 2015 and is not available for the GLOBAL
  mosaic.

- release:

  Character. WorldPop release for years \>= 2015: "R2025A" (default,
  constrained / built-up areas only) or "R2024B" (unconstrained, covers
  all populated areas via interpolation). Ignored for years \< 2015 (the
  legacy dataset is always used). The two releases answer different
  methodological questions and are not directly comparable.

- out_dir:

  Character string. Directory where downloaded files will be saved.
  Default: ".".

- quiet:

  Logical; if TRUE, suppresses progress messages (default: FALSE).

## Value

No return value. Files saved to output directory with pattern:
`{iso3}_{sex}_{lower}_{upper}_{year}.tif` where sex is "total", "m", or
"f", and upper is "80plus" when the 80+ band is included. For GLOBAL
requests, `iso3` is `global`.

## Details

### Data Source

The function automatically selects the appropriate dataset:

#### Legacy Dataset (years \< 2015)

- URL:
  https://data.worldpop.org/GIS/AgeSex_structures/Global_2000_2020_1km/

- Resolution: 1km only

- Sex variants: `m`, `f` only (no `t` total available)

- Files: `{iso3}_{sex}_{code}_{year}_1km.tif`

#### R2025A Dataset (years \>= 2015, default)

- URL:
  https://data.worldpop.org/GIS/AgeSex_structures/Global_2015_2030/R2025A/

- Constrained to built-up areas

- Resolution: 1km or 100m

- Sex variants: `m`, `f`, `t` (total)

- Files: `{iso3}_{sex}_{code}_{year}_CN_{res}_R2025A{_UA}_v1.tif`

#### R2024B Dataset (years \>= 2015, optional)

- URL:
  https://data.worldpop.org/GIS/AgeSex_structures/Global_2015_2030/R2024B/

- Unconstrained

- Resolution: 1km or 100m

- Sex variants: `m`, `f`, `t` (total)

- Files: `{iso3}_{sex}_{code}_{year}_UC_{res}_R2024B{_UA}_v1.tif`

#### Global Mosaic

Passing `country_codes = "GLOBAL"` swaps the `{ISO3}` URL segment for
`0_Mosaicked` and uses `global_` instead of an ISO3 prefix in the
filename. Available for years \>= 2015 at 1km only. The worldwide files
are large (R2025A: ~280 MB, R2024B: ~650 MB per band).

### Band Combination Logic

If the exact age range is not covered by a single WorldPop band, the
function combines adjacent bands. For example:

- Requesting ages 2-9 will combine bands 1-4 and 5-9

- Requesting ages 0-10 will combine bands 0-1, 1-4, 5-9, and 10-14

### Age Bands

Available bands: 0, 1-4, 5-9, 10-14, 15-19, 20-24, 25-29, 30-34, 35-39,
40-44, 45-49, 50-54, 55-59, 60-64, 65-69, 70-74, 75-79, 80+

## Examples

``` r
if (FALSE) { # \dontrun{
# Download age 1-9 data for Burundi (both sexes; uses R2025A 't' variant)
download_worldpop_age_band(
  country_codes = "BDI",
  years = 2020:2024,
  age_range = c(1, 9),
  out_dir = "data/worldpop"
)

# Download female-only age 1-9 data
download_worldpop_age_band(
  country_codes = "BDI",
  years = 2020:2024,
  age_range = c(1, 9),
  sex = "f",
  out_dir = "data/worldpop"
)

# Use R2024B unconstrained release instead of the R2025A default
download_worldpop_age_band(
  country_codes = "BDI",
  years = 2020,
  release = "R2024B",
  out_dir = "data/worldpop"
)

# Download the worldwide mosaic (large file, ~280 MB per band)
download_worldpop_age_band(
  country_codes = "GLOBAL",
  years = 2020,
  age_range = c(0, 4),
  out_dir = "data/worldpop"
)
} # }
```
