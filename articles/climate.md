# Climate

Every SNT study eventually needs climate covariates joined to admin
units. `sntutils` exposes a small, consistent API over the messy world
of public climate archives so we don’t write custom FTP / HTTPS /
Earthdata code per project.

The pattern across all download functions is the same: a thin
`<source>_options()` enumerates what’s available,
`check_<source>_available()` tells you which years and variables exist,
and `download_<source>()` pulls files into a local cache. The **raster
batch processors** at the bottom of this article then aggregate
everything to admin polygons.

For population rasters (WorldPop), see the [WorldPop
article](https://ahadi-analytics.github.io/sntutils/articles/worldpop.md).

**For the methodology and conceptual background behind the steps in this
article, please check the [SNT Code
Library](https://ahadi-analytics.github.io/snt-code-library/):**

- [Climate and environmental
  data](https://ahadi-analytics.github.io/snt-code-library/english/library/data/climate.html) -
  sources, resolutions, and how climate covariates feed stratification.
- [Extracting climate from
  rasters](https://ahadi-analytics.github.io/snt-code-library/english/library/data/climate/extract_raster_climate.html) -
  the analytical step-by-step.

## CHIRPS - rainfall

CHIRPS is the workhorse rainfall product for African SNT analyses. The
package supports the four standard CHIRPS monthly subsets.

``` r

library(sntutils)

# what's available
chirps_options()
#> # A tibble: 4 × 4
#>   dataset             frequency label                                 subdir
#>   <chr>               <chr>     <chr>                                 <chr>
#> 1 global_monthly      monthly   Global (Monthly)                      global_monthly/tifs
#> 2 africa_monthly      monthly   Africa (Monthly)                      africa_monthly/tifs
#> 3 camer-carib_monthly monthly   Caribbean & Central America (Monthly) camer-carib_monthly/tifs
#> 4 EAC_monthly         monthly   East African Community (Monthly)      EAC_monthly/tifs

# what years/months exist for a chosen subset
check_chirps_available(dataset_code = "africa_monthly")
#> ✔ africa_monthly: Data available from Jan 1981 to Mar 2025.

# download Africa monthly rainfall for Jan–Mar 2022
download_chirps(
  dataset = "africa_monthly",
  start   = "2022-01",
  end     = "2022-03",
  out_dir = "01_data/1.5_environment/1.5a_climate/raw/chirps"
)
```

[`download_chirps()`](https://ahadi-analytics.github.io/sntutils/reference/download_chirps.md)
is resumable - it never re-downloads a file that is already present and
the right size. The result is a directory of `.tif`s named
e.g. `chirps-v2.0.2022.01.tif`, ready for the raster batch processors
below.

## ERA5 - reanalysis

ERA5 (ECMWF reanalysis) gives temperature, dewpoint, wind, surface
pressure, and many other variables at hourly and daily resolution. The
download path goes through the Copernicus Climate Data Store and needs a
(free) API key.

``` r

# set CDS API key once - see https://cds.climate.copernicus.eu
Sys.setenv(CDS_USER = "<uid>", CDS_KEY = "<key>")

# list known datasets and their variable catalogues
era5_options()

# what years / variables exist for a chosen dataset
check_era5_available(
  dataset_code = "reanalysis-era5-land-monthly-means",
  years        = 2018:2024,
  months       = 1:12
)

# download daily 2m temperature and dewpoint over Sierra Leone
download_era5(
  dataset   = "reanalysis-era5-land",
  variables = c("2m_temperature", "2m_dewpoint_temperature"),
  years     = 2020:2023,
  months    = 1:12,
  bbox      = c(-13.5, 6.9, -10.2, 10.0),       # xmin, ymin, xmax, ymax
  daily_stat   = "mean",
  daily_hfreq  = "1_hourly",
  daily_tz     = "Africa/Freetown",
  out_dir      = "01_data/1.5_environment/1.5a_climate/raw/era5",
  aggregated_dir    = "01_data/1.5_environment/1.5a_climate/processed/era5",
  aggregated_format = "tif"
)
```

[`get_era5_metadata()`](https://ahadi-analytics.github.io/sntutils/reference/get_era5_metadata.md)
and
[`print_era5_metadata()`](https://ahadi-analytics.github.io/sntutils/reference/print_era5_metadata.md)
extract the embedded metadata from any ERA5 NetCDF without loading the
data into memory - useful for auditing what’s in the cache.
[`read_era5()`](https://ahadi-analytics.github.io/sntutils/reference/read_era5.md)
opens NetCDF files with sensible defaults (CRS, dimension renaming, unit
conversion).
[`migrate_era5_filenames()`](https://ahadi-analytics.github.io/sntutils/reference/migrate_era5_filenames.md)
renames older ERA5 caches to the current naming convention.

## MODIS - land surface

MODIS provides EVI, NDVI, surface temperature, fire and land-cover
layers. The
[`download_modis()`](https://ahadi-analytics.github.io/sntutils/reference/download_modis.md)
function wraps NASA Earthdata search + download and clips each tile to a
bounding box or shapefile.

``` r

# what products and bands are supported
modis_options(search = "MOD11")

# download monthly land surface temperature for Sierra Leone
download_modis(
  shapefile = sle_adm2_clean,
  start     = "2022-01-01",
  end       = "2022-12-31",
  product   = "MOD11A2",
  band      = "LST_Day_1km",
  out_dir   = "01_data/1.5_environment/1.5a_climate/raw/modis",
  username  = Sys.getenv("EARTHDATA_USER"),
  password  = Sys.getenv("EARTHDATA_PASS")
)
```

The output is a tidy set of GeoTIFFs with date-suffixed filenames, ready
for the raster processors.

## NASA POWER - point time series

For agro-climate variables (rainfall, irradiance, humidity, wind speed,
several temperature flavours) at point locations,
[`download_process_nasapower()`](https://ahadi-analytics.github.io/sntutils/reference/download_process_nasapower.md)
is the fastest path: it issues one query per polygon centroid and
returns a tidy long tibble already joined to admin codes.

``` r

power_df <- download_process_nasapower(
  adm_sf       = sle_adm2_clean,
  admin_cols   = c("adm1_name", "adm2_name"),
  start_date   = "2020-01-01",
  end_date     = "2023-12-31",
  power_vars   = c("PRECTOTCORR", "T2M", "T2M_MIN", "T2M_MAX",
                   "RH2M", "WS2M", "ALLSKY_SFC_SW_DWN"),
  power_community = "ag",
  max_retries  = 3,
  dict_language = "en"
)

dplyr::glimpse(power_df)
#> Rows: ~ 1.7M
#> Columns: 11
#> $ adm1_name <chr> "Eastern", "Eastern", "Eastern", ...
#> $ adm2_name <chr> "Kailahun", "Kailahun", ...
#> $ date      <date> 2020-01-01, 2020-01-02, ...
#> $ PRECTOTCORR <dbl> 0.0, 0.0, 0.32, ...
#> $ T2M       <dbl> 25.4, 25.9, 26.1, ...
```

## Extracting to admin units

The download functions deposit `.tif`s on disk. To turn those into a
tidy tibble keyed by admin and date, use the [batch raster
processors](https://ahadi-analytics.github.io/sntutils/articles/rasters.md) -
[`process_raster_collection()`](https://ahadi-analytics.github.io/sntutils/reference/process_raster_collection.md)
for plain zonal stats,
[`process_weighted_raster_collection()`](https://ahadi-analytics.github.io/sntutils/reference/process_weighted_raster_collection.md)
for population-weighted extraction,
[`process_rasters_by_year()`](https://ahadi-analytics.github.io/sntutils/reference/process_rasters_by_year.md)
when boundaries change between vintages, and
[`process_ihme_u5m_raster()`](https://ahadi-analytics.github.io/sntutils/reference/process_ihme_u5m_raster.md)
for IHME under-5 mortality rasters. The Rasters article walks through
each in detail.

## Mapping small-area climate estimates

Once climate is extracted to admin,
[`facetted_map_gradient()`](https://ahadi-analytics.github.io/sntutils/reference/facetted_map_gradient.md)
and
[`facetted_map_bins()`](https://ahadi-analytics.github.io/sntutils/reference/facetted_map_bins.md)
are the small-area visualisation pair. They both take an `sf` object
(boundaries + indicator on the same rows) and a colour vector, so we
join the extraction output to our boundaries first and then pick colours
from
[`get_palette()`](https://ahadi-analytics.github.io/sntutils/reference/get_palette.md).

``` r

# 1. extract CHIRPS to adm2 (see the Rasters article)
rain_adm2 <- process_raster_collection(
  directory    = "01_data/1.5_environment/1.5a_climate/raw/chirps",
  shapefile    = sle_adm2_clean,
  id_cols      = c("adm0_name", "adm1_name", "adm2_name"),
  aggregations = "mean"
)

# 2. summarise to annual and join to the sf so each polygon carries its value
rain_annual_sf <- rain_adm2 |>
  dplyr::group_by(adm0_name, adm1_name, adm2_name, year) |>
  dplyr::summarise(rain_mm = mean(mean, na.rm = TRUE), .groups = "drop") |>
  dplyr::left_join(sle_adm2_clean, y = _, by = c("adm0_name", "adm1_name", "adm2_name")) |>
  sf::st_as_sf()
```

### Continuous scale

For a continuous indicator like rainfall,
[`facetted_map_gradient()`](https://ahadi-analytics.github.io/sntutils/reference/facetted_map_gradient.md)
draws a small-multiples choropleth straight from the joined `sf`. Pick
colours via
[`get_palette()`](https://ahadi-analytics.github.io/sntutils/reference/get_palette.md) -
[`list_palettes()`](https://ahadi-analytics.github.io/sntutils/reference/list_palettes.md)
lists all built-in palettes (`"ylgnbu"`, `"blues"`, `"viridis"`,
`"spectral"`, …).

``` r

list_palettes()
#> [1] "default"  "byor"     "gyor"     "ylord"    "ylgnbu"   "blues"
#> [7] "greens"   "reds"     "oranges"  "purples"  "bupu"     "orrd"
#> ... etc

facetted_map_gradient(
  data      = rain_annual_sf,
  fill_col  = "rain_mm",
  facet_col = "year",
  colors    = get_palette("ylgnbu"),
  limits    = c(0, 300),
  ncol      = 3,
  title     = "Mean monthly rainfall (mm) - Sierra Leone",
  fill_label = "Rainfall (mm)",
  caption   = "Source: CHIRPS v2.0, monthly Africa subset."
)
```

### Discrete bands

When the indicator is more naturally read in categories - e.g. low /
moderate / high rainfall - bin the values with
[`auto_bin()`](https://ahadi-analytics.github.io/sntutils/reference/auto_bin.md)
(or manually) and pair
[`facetted_map_bins()`](https://ahadi-analytics.github.io/sntutils/reference/facetted_map_bins.md)
with a named-colour vector from
[`get_palette()`](https://ahadi-analytics.github.io/sntutils/reference/get_palette.md)
of the same length as the bin levels.

``` r

rain_banded_sf <- rain_annual_sf |>
  dplyr::mutate(
    rain_band = auto_bin(rain_mm, n_breaks = 5, style = "quantile")
  )

# one colour per band, in the bin order
bin_colors <- stats::setNames(
  get_palette("ylgnbu", n = nlevels(rain_banded_sf$rain_band)),
  levels(rain_banded_sf$rain_band)
)

facetted_map_bins(
  data        = rain_banded_sf,
  fill_col    = "rain_band",
  facet_col   = "year",
  fill_colors = bin_colors,
  title       = "Mean monthly rainfall, quantile bands",
  fill_label  = "mm / month"
)
```

Both helpers can write a compressed PNG straight to disk by passing
`output_file = "..."`. They also accept an optional `adm1_shp` overlay
for higher-admin outlines on top of the adm2 fills - useful for SNT
reports where district fills sit inside region borders.

## A climate pipeline, end to end

``` r

# 1. download CHIRPS monthly rainfall
download_chirps(
  dataset = "africa_monthly",
  start   = "2020-01",
  end     = "2024-12",
  out_dir = "01_data/1.5_environment/1.5a_climate/raw/chirps"
)

# 2. download ERA5 daily temperature for the same period
download_era5(
  dataset      = "reanalysis-era5-land",
  variables    = c("2m_temperature"),
  years        = 2020:2024,
  months       = 1:12,
  bbox         = c(-13.5, 6.9, -10.2, 10.0),
  daily_stat   = "mean",
  out_dir      = "01_data/1.5_environment/1.5a_climate/raw/era5"
)

# 3. extract both to adm2, population-weighted by the 2020 WorldPop surface
rain_adm2 <- process_weighted_raster_collection(
  directory     = "01_data/1.5_environment/1.5a_climate/raw/chirps",
  shapefile     = sle_adm2_clean,
  weight_raster = "01_data/1.1_foundational/1.1c_population/1.1cii_worldpop_rasters/sle_ppp_2020_1km.tif",
  id_cols       = c("adm1_name", "adm2_name"),
  aggregations  = "mean"
)

temp_adm2 <- process_weighted_raster_collection(
  directory     = "01_data/1.5_environment/1.5a_climate/raw/era5",
  shapefile     = sle_adm2_clean,
  weight_raster = "01_data/1.1_foundational/1.1c_population/1.1cii_worldpop_rasters/sle_ppp_2020_1km.tif",
  id_cols       = c("adm1_name", "adm2_name"),
  aggregations  = "mean"
)

# 4. combine - ready for stratification and modelling
climate_adm2 <- rain_adm2 |>
  dplyr::left_join(
    temp_adm2,
    by = c("adm1_name", "adm2_name", "year", "month"),
    suffix = c("_rain", "_temp")
  )
```

That’s two major archives reduced to one tibble keyed by admin and
date - the canonical climate input to the stratification and modelling
steps downstream.
