# Download MODIS Data from NASA

Searches NASA's CMR catalog, downloads MODIS HDF files via `httr2`,
extracts the requested band, mosaics tiles, crops to the area of
interest, scales values, and saves one GeoTIFF per time step. Output
files are named `modis_{band}_{YYYY}_{MM}.tif` and work directly with
[`process_raster_collection()`](https://ahadi-analytics.github.io/sntutils/reference/process_raster_collection.md).

## Usage

``` r
download_modis(
  shapefile,
  start,
  end,
  product = "MOD13A3",
  band = "1 km monthly EVI",
  out_dir = ".",
  username = NULL,
  password = NULL,
  version = "061",
  scale_factor = "auto",
  keep_hdf = FALSE,
  overwrite = FALSE,
  max_workers = max(1L, parallel::detectCores() - 1L)
)
```

## Arguments

- shapefile:

  An `sf` object defining the area of interest. Reprojected to WGS84
  (EPSG:4326) for the NASA query.

- start:

  Character. Start date in any common format (e.g. `"2023-01-01"`).

- end:

  Character. End date in any common format.

- product:

  Character. MODIS product short name (default `"MOD13A3"` for monthly
  vegetation indices at 1 km).

- band:

  Character. HDF subdataset name to extract (default
  `"1 km monthly EVI"`). Use `terra::describe(hdf_file)` on a downloaded
  HDF to see available subdatasets.

- out_dir:

  Character. Output directory for GeoTIFFs (default `"."`). Created if
  it does not exist.

- username:

  Character or `NULL`. NASA Earthdata username. Falls back to
  `Sys.getenv("EARTHDATA_USERNAME")`.

- password:

  Character or `NULL`. NASA Earthdata password. Falls back to
  `Sys.getenv("EARTHDATA_PASSWORD")`.

- version:

  Character. MODIS collection version (default `"061"`).

- scale_factor:

  `"auto"`, numeric, or `NULL`. When `"auto"` (default), the scale
  factor is looked up from a built-in table of known MODIS bands (e.g.
  0.0001 for EVI/NDVI, 0.02 for LST). Pass a number to override, or
  `NULL` to skip scaling.

- keep_hdf:

  Logical. Keep raw HDF files after processing? Default `FALSE` (deletes
  them). When `TRUE`, saved to `{out_dir}/hdf/`.

- overwrite:

  Logical. Re-download and reprocess existing files? Default `FALSE`.

- max_workers:

  Integer. Maximum concurrent HDF downloads. Defaults to
  `parallel::detectCores() - 1`. Note: MODIS downloads are
  network-bound, so more workers is not always faster — NASA LPDAAC
  throttles above ~8 concurrent connections. Cap manually if you see
  connection errors.

## Value

Invisible character vector of output GeoTIFF paths.

## Details

Use
[`modis_options()`](https://ahadi-analytics.github.io/sntutils/reference/modis_options.md)
to browse available products.

## Examples

``` r
if (FALSE) { # \dontrun{
boundary <- sf::st_read("guinea_adm0.geojson")

# download monthly EVI for 2023
paths <- download_modis(
  shapefile = boundary,
  start     = "2023-01-01",
  end       = "2023-12-31",
  out_dir   = "data/evi",
  username  = "my_user",
  password  = "my_pass"
)

# then use the existing raster processing pipeline
result <- process_raster_collection(
  directory    = "data/evi",
  shapefile    = sf::st_read("guinea_adm3.geojson"),
  id_cols      = c("adm0", "adm1", "adm2", "adm3"),
  aggregations = "mean"
)

# download NDVI instead
download_modis(
  shapefile = boundary,
  start     = "2023-01-01",
  end       = "2023-12-31",
  band      = "1 km monthly NDVI",
  out_dir   = "data/ndvi"
)

# keep raw HDF files for inspection
download_modis(
  shapefile = boundary,
  start     = "2023-01-01",
  end       = "2023-12-31",
  out_dir   = "data/evi",
  keep_hdf  = TRUE
)
} # }
```
