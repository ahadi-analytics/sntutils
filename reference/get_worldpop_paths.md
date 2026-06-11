# Download WorldPop Rasters and Get Paths

Downloads WorldPop population rasters for multiple age groups in one
call and returns file paths as a nested named list. Each group is a
named list keyed by year.

## Usage

``` r
get_worldpop_paths(
  country_code,
  years,
  groups = list(total = list(age_range = NULL), u5 = list(age_range = c(0, 4)), wra =
    list(age_range = c(15, 49), sex = "f")),
  resolution = "1km",
  release = "R2025A",
  dest_dir = here::here(),
  download = TRUE,
  quiet = TRUE
)
```

## Arguments

- country_code:

  Character. Single ISO3 country code (e.g., "TGO"), or `"GLOBAL"`
  (case-insensitive) to download worldwide mosaics instead of
  per-country rasters. `"GLOBAL"` requires years \>= 2015 and 1km
  resolution.

- years:

  Numeric vector of years to download (2000-2030)

- groups:

  Named list of age group specifications. Each element should be a list
  with `age_range` (length-2 numeric vector or NULL for total
  population) and optionally `sex` ("both", "m", or "f"). Default
  includes total, u5, and wra groups.

- resolution:

  Character. "1km" (default) or "100m".

- release:

  Character. WorldPop release for age-band downloads at years \>= 2015:
  "R2025A" (default, constrained) or "R2024B" (unconstrained). Ignored
  for years \< 2015 and for total-population downloads (which always use
  R2025A for years \>= 2015).

- dest_dir:

  Character. Base directory for downloaded files. Total population goes
  to `dest_dir`, age bands go to `dest_dir/aged_rasters`.

- download:

  Logical. If TRUE (default), downloads rasters before building paths.
  Set FALSE to just get expected paths.

- quiet:

  Logical. If TRUE (default), suppresses per-file progress messages
  during download. A compact summary is always shown.

## Value

Named list of groups, each a named list of file paths keyed by year.

## Examples

``` r
if (FALSE) { # \dontrun{
# default groups: total, u5, wra
paths <- get_worldpop_paths(
  "TGO", years = c(2013, 2017),
  dest_dir = here::here("data/worldpop/raw")
)
paths$total$`2013`  # total population raster
paths$u5$`2017`     # under-5 raster
paths$wra$`2013`    # women of reproductive age raster

# custom groups
paths <- get_worldpop_paths(
  "TGO", years = c(2013, 2017),
  groups = list(
    total = list(age_range = NULL),
    u5 = list(age_range = c(0, 4)),
    "5_9" = list(age_range = c(5, 9)),
    "10_19" = list(age_range = c(10, 19)),
    "20plus" = list(age_range = c(20, Inf)),
    wra = list(age_range = c(15, 49), sex = "f")
  ),
  dest_dir = here::here("data/worldpop/raw")
)
} # }
```
