# Rasters

Every SNT analysis ends up turning a folder of `.tif`s into a tidy
tibble keyed by admin and time. `sntutils` exposes a small set of
**batch raster processors** that handle the boring parts: detecting
dates from filenames, aligning CRS, cropping to the shapefile extent,
computing zonal stats, and applying population weights when needed.

The same processors work for any raster source - the downloads articles
([Climate](https://ahadi-analytics.github.io/sntutils/articles/climate.md),
[WorldPop](https://ahadi-analytics.github.io/sntutils/articles/worldpop.md))
deposit files on disk, then these functions reduce them to admin-level
tibbles.

**For the methodology and conceptual background behind the steps in this
article, please check the [SNT Code
Library](https://ahadi-analytics.github.io/snt-code-library/):**

- [Extracting climate from
  rasters](https://ahadi-analytics.github.io/snt-code-library/english/library/data/climate/extract_raster_climate.html) -
  the analytical step-by-step.
- [Population
  rasters](https://ahadi-analytics.github.io/snt-code-library/english/library/data/population/population_raster.html) -
  WorldPop methodology.
- [MAP
  outputs](https://ahadi-analytics.github.io/snt-code-library/english/library/data/modeled/MAP_outputs.html) -
  Malaria Atlas Project rasters and how to use them in SNT.
- [IHME mortality
  rasters](https://ahadi-analytics.github.io/snt-code-library/english/library/data/modeled/IHME_mortality.html) -
  U5M raster methodology.
- [Modeled data
  overview](https://ahadi-analytics.github.io/snt-code-library/english/library/data/modeled.html) -
  when to lean on modeled vs measured surfaces.

## Basic zonal stats

The default for CHIRPS / ERA5-monthly / MODIS-monthly. Dates are
detected automatically from filenames; the function aligns CRS between
raster and shapefile, crops the raster to the shapefile extent before
extraction, and returns a tibble of admin x time x stat.

``` r

library(sntutils)

adm3_shp <- sf::st_read(system.file(
  "extdata", "sle_adm3_example.geojson", package = "sntutils"
))

raster_dir <- system.file("extdata", "chirps_test_rasters",
                          package = "sntutils")

rainfall_df <- process_raster_collection(
  directory    = raster_dir,
  shapefile    = adm3_shp,
  id_cols      = c("adm0", "adm1", "adm2", "adm3"),
  aggregations = c("mean"),
  pattern      = "\\.tif$"
)

rainfall_df |> utils::head()
#>                                file_name    adm1     adm2          adm3 year month      mean
#> 1   africa_monthly_chirps-v2.0.2020.01.tif EASTERN KAILAHUN          DEA 2020     1 12.871692
#> 2   africa_monthly_chirps-v2.0.2020.01.tif EASTERN KAILAHUN         JAHN 2020     1  9.820749
#> 3   africa_monthly_chirps-v2.0.2020.01.tif EASTERN KAILAHUN        JAWIE 2020     1 12.042542
#> 4   africa_monthly_chirps-v2.0.2020.01.tif EASTERN KAILAHUN   KISSI KAMA 2020     1  8.951293
```

Pass `aggregations = c("mean", "sum", "median", "max")` to get several
stats in one pass.

## Single raster, multiple boundaries

Use this when the same raster needs to be extracted to several
shapefiles (adm1, adm2, adm3 simultaneously), or with different ID
column sets.

``` r

process_raster_with_boundaries(
  raster_file = "01_data/1.5_environment/1.5a_climate/raw/chirps/chirps-v2.0.2020.01.tif",
  boundaries  = list(
    adm1 = sle_adm1_clean,
    adm2 = sle_adm2_clean,
    adm3 = sle_adm3_clean
  ),
  id_cols    = c("adm0_name", "adm1_name", "adm2_name", "adm3_name"),
  aggregations = c("mean", "sum")
)
```

## Time-varying boundaries

When the boundaries themselves change over time (post-redistricting in
Sierra Leone 2017, Togo 2019, recent DRC changes), this wrapper aligns
each raster’s year to the appropriate shapefile vintage.

``` r

process_rasters_by_year(
  directory     = "01_data/1.5_environment/1.5a_climate/raw/chirps",
  shapefiles    = list(
    `<=2017` = sle_adm2_pre_2017,
    `>=2018` = sle_adm2_post_2017
  ),
  id_cols       = c("adm0_name", "adm1_name", "adm2_name"),
  aggregations  = "mean"
)
```

Each raster is matched to the shapefile vintage whose year range it
falls into, then extracted normally. The output tibble carries an
explicit `boundary_vintage` column so downstream analyses know which
shapefile a row came from.

## Population-weighted extraction

For variables that need population weighting (e.g. mean temperature
weighted by population so we report an “experienced” value rather than
an areal average), point the weighted extractors at a population raster
of the same extent. Weights are applied per pixel during extraction.

### Population-weighted collection

``` r

process_weighted_raster_collection(
  directory      = "01_data/1.5_environment/1.5a_climate/raw/chirps",
  shapefile      = sle_adm2_clean,
  weight_raster  = "01_data/1.1_foundational/1.1c_population/1.1cii_worldpop_rasters/sle_ppp_2020_1km.tif",
  id_cols        = c("adm0_name", "adm1_name", "adm2_name"),
  aggregations   = c("mean", "sum")
)
```

### Population-weighted stacks

The same idea but for raster stacks (multiple bands per file) - useful
when WorldPop publishes a multi-year stack or an ERA5 NetCDF has many
variables in one file.

### Normalising weights within a polygon

The building block under the weighted extractors. Normalises a raster’s
values so they sum to 1 within each polygon - which is what lets the
weighted aggregations keep totals consistent across resolutions.

``` r

norm_pop <- normalize_raster_by_polygon(
  raster = terra::rast("sle_ppp_2020_1km.tif"),
  shp    = sle_adm2_clean,
  id_col = "adm2_name"
)
```

## IHME under-5 mortality

[`process_ihme_u5m_raster()`](https://ahadi-analytics.github.io/sntutils/reference/process_ihme_u5m_raster.md)
is a domain-specific helper that turns IHME’s under-5 mortality rasters
into admin-level tibbles in one call, applying the same crop / extract
pattern as the generic processors:

``` r

u5m_adm2 <- process_ihme_u5m_raster(
  directory = "01_data/1.2_epidemiology/1.2c_mortality_estimates/raw/ihme_u5m",
  shapefile = sle_adm2_clean,
  id_cols   = c("adm0_name", "adm1_name", "adm2_name")
)
```

## Filename utilities

The batch processors detect dates from filenames using **name
patterns**. These utilities are exposed for ad-hoc inspection and for
renaming external archives to a format the processors understand:

| Function | What it does |
|----|----|
| [`detect_time_pattern()`](https://ahadi-analytics.github.io/sntutils/reference/detect_time_pattern.md) | Inspect a list of filenames and report the date pattern detected (`YYYY`, `YYYY-MM`, `YYYYMM`, …). |
| [`extract_time_components()`](https://ahadi-analytics.github.io/sntutils/reference/extract_time_components.md) | Pull year / month / day from a single filename given the pattern. |
| [`clean_filenames()`](https://ahadi-analytics.github.io/sntutils/reference/clean_filenames.md) | Strip spaces, accents and punctuation from a directory of filenames so they’re processor-friendly. |
| [`tidy_malaria_raster_names()`](https://ahadi-analytics.github.io/sntutils/reference/tidy_malaria_raster_names.md) | Batch-rename a directory of MAP / IHME rasters to the SNT convention. |

``` r

# what date pattern do these CHIRPS files use?
detect_time_pattern(list.files("01_data/1.5_environment/1.5a_climate/raw/chirps"))
#> "YYYY.MM"

# tidy up MAP downloads so process_raster_collection() can detect dates
tidy_malaria_raster_names("01_data/1.5_environment/1.5b_accessibility/raw/map")
```

## An extraction pipeline, end to end

``` r

# 1. download climate (see the Climate article)
download_chirps(
  dataset = "africa_monthly",
  start   = "2020-01", end = "2024-12",
  out_dir = "01_data/1.5_environment/1.5a_climate/raw/chirps"
)

# 2. download population (see the WorldPop article)
download_worldpop(
  country_codes = "SLE",
  years         = 2020:2024,
  resolution    = "1km",
  dest_dir      = "01_data/1.1_foundational/1.1c_population/1.1cii_worldpop_rasters"
)

# 3. unweighted population sum by adm2-year - sum the WorldPop pixels
pop_adm2 <- process_raster_collection(
  directory    = "01_data/1.1_foundational/1.1c_population/1.1cii_worldpop_rasters",
  shapefile    = sle_adm2_clean,
  id_cols      = c("adm0_name", "adm1_name", "adm2_name"),
  aggregations = "sum",
  pattern      = "_ppp_\\d{4}_1km.*\\.tif$"
)

# 4. population-weighted mean rainfall by adm2-month
rain_adm2 <- process_weighted_raster_collection(
  directory     = "01_data/1.5_environment/1.5a_climate/raw/chirps",
  shapefile     = sle_adm2_clean,
  weight_raster = "01_data/1.1_foundational/1.1c_population/1.1cii_worldpop_rasters/sle_ppp_2020_1km.tif",
  id_cols       = c("adm0_name", "adm1_name", "adm2_name"),
  aggregations  = "mean"
)

# 5. join - one tibble keyed by admin and year
analysis_ready <- rain_adm2 |>
  dplyr::left_join(pop_adm2, by = c("adm0_name", "adm1_name", "adm2_name", "year"))
```

Two archives (climate + population), one tibble keyed by admin and
date - the canonical input for stratification and modelling.

> Want to **map the extracted tibble** as a small-area surface? See the
> [Climate article’s mapping
> section](https://ahadi-analytics.github.io/sntutils/articles/climate.html#mapping-small-area-climate-estimates)
> for a worked example using
> [`facetted_map_gradient()`](https://ahadi-analytics.github.io/sntutils/reference/facetted_map_gradient.md)
> and the
> [`get_palette()`](https://ahadi-analytics.github.io/sntutils/reference/get_palette.md)
> colour helpers. The same recipe works for any raster-extracted tibble.
