###############################################################################
#                TOGO — Download and process climate data
#
#  This script demonstrates 4 approaches for obtaining climate data
#  as tidy, admin-level tables ready for analysis:
#
#    1. CHIRPS  — Rainfall           (public, no credentials needed)
#    2. MODIS   — Vegetation / LST   (NASA Earthdata account needed)
#    3. ERA5    — Temperature, rain   (Copernicus CDS API key needed)
#    4. POWER   — Point-based daily   (public, no credentials needed)
#
#  All 4 pipelines follow the same pattern:
#    download -> process rasters -> tidy tibble -> save
#
#  (POWER skips the raster step — it returns tidy data directly.)
#
#  NOTE: for demonstration purposes, we only download the year 2024.
#  In production, simply widen the dates (e.g. start = "2015-01",
#  end = "2025-12") to cover multiple years.
#
#  ---- Required credentials ----
#
#  Two sources require credentials (free to obtain):
#
#  MODIS (NASA Earthdata):
#    1. Create an account at https://urs.earthdata.nasa.gov/
#    2. Add these lines to your .Renviron file
#       (open with: usethis::edit_r_environ()):
#
#         EARTHDATA_USERNAME=your_username
#         EARTHDATA_PASSWORD=your_password
#
#    3. Restart R so the variables are loaded.
#    4. In your code, retrieve them with:
#         Sys.getenv("EARTHDATA_USERNAME")
#         Sys.getenv("EARTHDATA_PASSWORD")
#
#  ERA5 (Copernicus CDS):
#    1. Create an account at https://cds.climate.copernicus.eu/
#    2. Copy your API key from your profile:
#       https://cds.climate.copernicus.eu/profile
#    3. Add to .Renviron:
#
#         ERA5_API_KEY=your_api_key
#
#    4. Restart R. Retrieve with:
#         Sys.getenv("ERA5_API_KEY")
#
#  CHIRPS and POWER are public — no credentials needed.
#
###############################################################################

cli::cli_h1("Togo — Download and process climate data")

## ---------------------------------------------------------------------------
## 0. Setup, data, and parameters
## ---------------------------------------------------------------------------

cli::cli_process_start(
  "Setting up environment and loading data",
  msg_done = "Environment set up and data loaded"
)

# country identifiers
iso3 <- "tgo"
adm0_name <- "Togo"

# project paths
paths <- sntutils::setup_project_paths(
  base_path = Sys.getenv("AHADI_ONEDRIVE_PROJECT"),
  quiet = TRUE
)

clim_path <- here::here(paths$climate)

# load admin-2 shapefile (districts)
shp_adm2 <- sntutils::read_snt_data(
  path = here::here(paths$admin_shp, "processed"),
  data_name = glue::glue("{iso3}_shp_list"),
  file_formats = c("qs2")
)$final_spat_vec$adm2 |>
  dplyr::group_by(adm0, adm1, adm2) |>
  dplyr::summarise(
    geometry = sf::st_union(geometry),
    .groups = "drop"
  )

cli::cli_process_done()


###############################################################################
#
#  1. CHIRPS — Rainfall
#
#  Source: UCSB Climate Hazards Center
#  Credentials: NONE (public data)
#  Resolution: ~5 km, monthly
#  Output: one GeoTIFF per month
#
#  Useful functions:
#    sntutils::chirps_options()            — list available datasets
#    sntutils::check_chirps_available()    — check what's online
#    sntutils::download_chirps()           — download
#    sntutils::process_raster_collection() — extract admin-level stats
#
###############################################################################

cli::cli_h1("1. CHIRPS — Rainfall")

## -- download ---------------------------------------------------------------

# downloading 2024 only for this demo;
# change start/end to cover more years
sntutils::download_chirps(
  dataset = "africa_monthly",
  start = "2024-01",
  end = "2024-12",
  out_dir = here::here(clim_path, "chirps", "raw")
)

## -- process ----------------------------------------------------------------

rain_data <- sntutils::process_raster_collection(
  directory = here::here(clim_path, "chirps", "raw"),
  shapefile = shp_adm2,
  aggregations = c("sum", "mean", "median")
) |>
  dplyr::mutate(
    location = paste(adm1, "~", adm2),
    date = paste(year, month, "01", sep = "-") |> as.Date(),
    yearmon = factor(
      sntutils::translate_yearmon(date),
      levels = sntutils::translate_yearmon(sort(unique(date)))
    )
  ) |>
  dplyr::distinct(
    adm0, adm1, adm2, location,
    year, month, yearmon,
    total_rainfall_mm = sum,
    mean_rainfall_mm = mean,
    median_rainfall_mm = median
  )

## -- save -------------------------------------------------------------------

sntutils::write_snt_data(
  obj = list(
    data = rain_data,
    dict = sntutils::build_dictionary(rain_data, language = "fr")
  ),
  data_name = glue::glue("{iso3}_chirps_rainfall"),
  path = here::here(clim_path, "chirps", "processed"),
  file_formats = c("qs2", "xlsx")
)


###############################################################################
#
#  2. NASA MODIS — Vegetation (EVI)
#
#  Source: NASA CMR / LP DAAC
#  Credentials: NASA Earthdata username + password
#     Register here -> https://urs.earthdata.nasa.gov/
#
#  Resolution: 1 km, monthly
#  Output: one GeoTIFF per month (mosaicked, cropped, scaled)
#
#  Useful functions:
#    sntutils::modis_options()             — browse products
#    sntutils::modis_options("vegetation") — filter by keyword
#    sntutils::download_modis()            — download
#    sntutils::process_raster_collection() — extract admin-level stats
#
#  Tip: set credentials once in .Renviron so you don't paste them
#       in every script:
#
#    EARTHDATA_USERNAME=your_username
#    EARTHDATA_PASSWORD=your_password
#
###############################################################################

cli::cli_h1("2. MODIS — EVI (vegetation index)")

## -- download ---------------------------------------------------------------

# downloading 2024 only for this demo
sntutils::download_modis(
  shapefile = shp_adm2,
  start = "2024-01-01",
  end = "2024-12-31",
  product = "MOD13A3",
  band = "1 km monthly EVI",
  out_dir = here::here(clim_path, "evi", "raw"),
  username = Sys.getenv("EARTHDATA_USERNAME"),
  password = Sys.getenv("EARTHDATA_PASSWORD")
)

## -- process ----------------------------------------------------------------

evi_data <- sntutils::process_raster_collection(
  directory = here::here(clim_path, "evi", "raw"),
  shapefile = shp_adm2,
  aggregations = c("mean", "median")
) |>
  dplyr::mutate(
    location = paste(adm1, "~", adm2),
    date = paste(year, month, "01", sep = "-") |> as.Date(),
    yearmon = factor(
      sntutils::translate_yearmon(date),
      levels = sntutils::translate_yearmon(sort(unique(date)))
    )
  ) |>
  dplyr::distinct(
    adm0, adm1, adm2, location,
    year, month, yearmon,
    mean_evi = mean,
    median_evi = median
  )

## -- save -------------------------------------------------------------------

sntutils::write_snt_data(
  obj = list(
    data = evi_data,
    dict = sntutils::build_dictionary(evi_data, language = "fr")
  ),
  data_name = glue::glue("{iso3}_modis_evi"),
  path = here::here(clim_path, "evi", "processed"),
  file_formats = c("qs2", "xlsx")
)


###############################################################################
#
#  3. ERA5 — Temperature, rainfall and soil moisture
#
#  Source: Copernicus Climate Data Store (CDS)
#  Credentials: CDS API key
#     Get it here -> https://cds.climate.copernicus.eu/profile
#
#  Resolution: ~31 km, monthly
#  Output: NetCDF files -> converted to tidy tibble with read_era5()
#
#  Unlike CHIRPS/MODIS, ERA5 delivers NetCDF files. You read them with
#  sntutils::read_era5(), which automatically converts Kelvin to
#  Celsius and metres to mm when convert_units = TRUE.
#
#  Useful functions:
#    sntutils::era5_options()       — list available datasets
#    sntutils::download_era5()      — download
#    sntutils::read_era5()          — NetCDF -> tidy tibble
#
#  Tip: set your key in .Renviron:
#    ERA5_API_KEY=your_cds_api_key
#
###############################################################################

cli::cli_h1("3. ERA5 — Temperature, Rainfall and Soil Moisture")

## -- download ---------------------------------------------------------------

# togo bounding box: xmin, ymin, xmax, ymax
tgo_bbox <- c(-0.2, 6.0, 1.9, 11.2)

# downloading 2024 only for this demo
sntutils::download_era5(
  dataset = "monthly_single_levels",
  years = 2024,
  months = 1:12,
  variables = c(
    "2m_temperature",
    "total_precipitation",
    "volumetric_soil_water_layer_1"
  ),
  bbox = tgo_bbox,
  out_dir = here::here(clim_path, "era5", "raw"),
  cds_key = Sys.getenv("ERA5_API_KEY")
)

## -- read and tidy ----------------------------------------------------------

era5_files <- list.files(
  here::here(clim_path, "era5", "raw"),
  pattern = "\\.nc$",
  full.names = TRUE
)

era5_tidy <- sntutils::read_era5(
  nc_files = era5_files,
  country = "TGO",
  convert_units = TRUE
)

# aggregate to admin-2 monthly means
# era5 data comes as a lon/lat grid; we convert to sf, spatial-join
# with admin boundaries, then group
era5_sf <- sf::st_as_sf(
  era5_tidy,
  coords = c("lon", "lat"),
  crs = 4326
)

era5_joined <- sf::st_join(era5_sf, shp_adm2, join = sf::st_within)

era5_data <- era5_joined |>
  sf::st_drop_geometry() |>
  dplyr::filter(!is.na(adm2)) |>
  dplyr::group_by(adm0, adm1, adm2, year, month, variable) |>
  dplyr::summarise(
    value = mean(value, na.rm = TRUE),
    .groups = "drop"
  ) |>
  tidyr::pivot_wider(
    names_from = variable,
    values_from = value
  ) |>
  dplyr::mutate(
    location = paste(adm1, "~", adm2),
    date = paste(year, month, "01", sep = "-") |> as.Date(),
    yearmon = factor(
      sntutils::translate_yearmon(date),
      levels = sntutils::translate_yearmon(sort(unique(date)))
    )
  )

## -- save -------------------------------------------------------------------

sntutils::write_snt_data(
  obj = list(
    data = era5_data,
    dict = sntutils::build_dictionary(era5_data, language = "fr")
  ),
  data_name = glue::glue("{iso3}_era5_climate"),
  path = here::here(clim_path, "era5", "processed"),
  file_formats = c("qs2", "xlsx")
)


###############################################################################
#
#  4. NASA POWER — Point-based daily climate (no rasters)
#
#  Source: NASA POWER API
#  Credentials: NONE (public data)
#  Resolution: ~50 km grid, point-sampled inside admin polygons
#  Output: tidy daily + monthly tibbles — no raster step needed
#
#  POWER samples random points inside each admin polygon, queries the
#  NASA POWER API for each point, then returns the median across
#  points. Default variables: rainfall, temperature (min/mean/max),
#  surface temperature, relative humidity.
#
#  This is the simplest approach — one function call returns
#  everything.
#
#  Useful functions:
#    sntutils::download_process_nasapower() — download + process
#
###############################################################################

cli::cli_h1("4. NASA POWER — Daily Climate")

## -- download and process (one step) ----------------------------------------

power_result <- sntutils::download_process_nasapower(
  adm_sf = shp_adm2,
  admin_cols = c("adm0", "adm1", "adm2"),
  start_date = "2024-01-01",
  end_date = "2024-12-31"
)

# the result is a list with daily + monthly data and dictionaries
power_daily <- power_result$daily
power_monthly <- power_result$monthly

## -- save -------------------------------------------------------------------

sntutils::write_snt_data(
  obj = list(
    daily = power_daily,
    monthly = power_monthly,
    dict_daily = power_result$dict_daily,
    dict_monthly = power_result$dict_monthly
  ),
  data_name = glue::glue("{iso3}_nasapower_climate"),
  path = here::here(clim_path, "nasapower", "processed"),
  file_formats = c("qs2", "xlsx")
)


###############################################################################
## Finished
###############################################################################

invisible(gc())

cli::cli_rule(
  left = "All climate downloads complete",
  right = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
)
