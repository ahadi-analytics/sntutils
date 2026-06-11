# Download WorldPop DUG Urbanicity Rasters

Downloads Degree of Urbanisation Grid (DUG) rasters from WorldPop for
the specified countries, years, and classification layers. Both L1
(DEGURBA 3-class) and L2 (sub-classified) layers are downloaded by
default.

## Usage

``` r
download_worldpop_urbanicity(
  country_codes,
  years = 2015:2024,
  layers = c("L1", "L2"),
  release = "R2025A",
  version = "v1",
  dest_dir = here::here(),
  quiet = FALSE
)
```

## Arguments

- country_codes:

  Character vector of ISO3 country codes (e.g., "DZA", "GIN").
  Case-insensitive; uppercased for URL building.

- years:

  Numeric vector of years to download (2015-2030). Default: 2015:2024
  (full available historical range).

- layers:

  Character vector. Subset of `c("L1", "L2")`. Default: both. L1 is the
  DEGURBA 3-class scheme endorsed by the UN Statistical Commission (3 =
  urban centre, 2 = urban cluster, 1 = rural). L2 is the finer-grained
  sub-classification.

- release:

  Character. WorldPop release tag. Default: "R2025A".

- version:

  Character. WorldPop version tag. Default: "v1".

- dest_dir:

  Destination directory for downloaded files (default:
  [`here::here()`](https://here.r-lib.org/reference/here.html)).

- quiet:

  Logical; if TRUE, suppresses per-file progress messages (default:
  FALSE).

## Value

Invisible list containing:

- files: Character vector of paths to downloaded/existing files

- counts: Named integer vector of successful files per country

## Details

Files are pulled from
`https://data.worldpop.org/GIS/DUG/Global_2015_2030/{release}/{version}/{year}/{ISO3}/`
with filenames of the form
`{ISO3}_DUG_{year}_GRID_{layer}_{release}_{version}.tif`.

Coverage on the WorldPop mirror is limited to **2015-2030**. Pre-2015
historical years and post-2030 projections from the underlying JRC /
Copernicus GHS-DUG product are hosted elsewhere and are out of scope for
this function.

Existing files are skipped (idempotent). Per-file failures (e.g., a
transient 404 for one country/year combination) are soft-failed with a
warning so the rest of the batch completes.

## Examples

``` r
if (FALSE) { # \dontrun{
# Download both L1 and L2 for Algeria, full historical range
download_worldpop_urbanicity("DZA")

# Multiple countries, narrower year range, L1 only
download_worldpop_urbanicity(
  country_codes = c("DZA", "GIN"),
  years = 2020:2024,
  layers = "L1",
  dest_dir = here::here("data/worldpop/dug")
)
} # }
```
