# sntutils

`sntutils` is an R package developed by
[AHADI](https://appliedhealthanalytics.org) to support the **Subnational
Tailoring (SNT)** of malaria interventions. It bundles the small,
repeated operations every SNT support analysis does - reading DHIS2
exports, harmonising admin names across shapefile vintages, validating
facility coordinates, calculating reporting rates, extracting climate
and population rasters to admin units, and rendering
country-team-language plots and maps.

> **Full documentation, with worked examples for every workflow, lives
> at <https://ahadi-analytics.github.io/sntutils/>**

The pkgdown site is the canonical reference. This README is a short
orientation and an index of the package’s surface area.

## Install

``` r

# install pak if needed
install.packages("pak")

# install sntutils from GitHub
pak::pkg_install("ahadi-analytics/sntutils")
```

System dependencies: `sntutils` uses `sf` and `terra`, which require
GDAL, GEOS and PROJ. On macOS install with
`brew install gdal proj geos`; on Ubuntu use the GDAL PPA.

## Quick start

``` r

library(sntutils)

# 1. read a DHIS2-style export (any common format works)
sl_dhis2 <- read(
  system.file("extdata", "sl_exmaple_dhis2.rds", package = "sntutils")
)

# 2. clean it
sl_dhis2 <- sl_dhis2 |>
  standardize_names() |>
  autoparse_dates(date_cols = "date") |>
  dplyr::rename(year_mon = date) |>
  dplyr::mutate(
    hf_uid = vdigest(paste0(adm1, adm2, hf), algo = "xxhash32")
  )

# 3. calculate district-month reporting rates
calculate_reporting_metrics(
  data             = sl_dhis2,
  vars_of_interest = c("conf", "pres"),
  x_var            = "year_mon",
  y_var            = "adm2",
  hf_col           = "hf_uid",
  key_indicators   = c("allout", "test", "treat", "conf", "pres")
)
```

The [Get started
article](https://ahadi-analytics.github.io/sntutils/articles/getting-started.html)
walks through the same example end-to-end with plotted output.

## What’s inside

The package exports ~100 functions, grouped by workflow stage. Each row
links to the article that covers that group in depth.

### [Read & clean →](https://ahadi-analytics.github.io/sntutils/articles/data-io-and-cleaning.html)

Read and write any common SNT format, parse messy dates, infer column
types, standardise admin and facility names, build data dictionaries.

| Function | What it does |
|----|----|
| [`read()`](https://ahadi-analytics.github.io/sntutils/reference/read.md), [`write()`](https://ahadi-analytics.github.io/sntutils/reference/write.md) | Read / write CSV, Excel, Stata, SPSS, RDS, GeoJSON, shapefile, … |
| [`read_snt_data()`](https://ahadi-analytics.github.io/sntutils/reference/read_snt_data.md), [`write_snt_data()`](https://ahadi-analytics.github.io/sntutils/reference/write_snt_data.md) | Atomic, hashed reads / writes with sidecar metadata |
| [`autoparse_dates()`](https://ahadi-analytics.github.io/sntutils/reference/autoparse_dates.md), `available_date_formats` | Detect and standardise mixed date formats |
| [`auto_parse_types()`](https://ahadi-analytics.github.io/sntutils/reference/auto_parse_types.md), [`detect_factors()`](https://ahadi-analytics.github.io/sntutils/reference/detect_factors.md) | Infer numeric / integer / factor / date types |
| [`standardize_names()`](https://ahadi-analytics.github.io/sntutils/reference/standardize_names.md), [`clean_filenames()`](https://ahadi-analytics.github.io/sntutils/reference/clean_filenames.md) | Tidy admin / facility names and file names |
| [`prep_geonames()`](https://ahadi-analytics.github.io/sntutils/reference/prep_geonames.md) | Interactive admin-name harmonisation with caching |
| [`build_dictionary()`](https://ahadi-analytics.github.io/sntutils/reference/build_dictionary.md), [`snt_data_dict()`](https://ahadi-analytics.github.io/sntutils/reference/snt_data_dict.md), [`check_snt_var()`](https://ahadi-analytics.github.io/sntutils/reference/check_snt_var.md) | Variable dictionaries |

### [Spatial →](https://ahadi-analytics.github.io/sntutils/articles/spatial.html)

Validate admin geometries and facility coordinates, crosswalk between
shapefile vintages, fuzzy-match facilities, render maps.

| Function | What it does |
|----|----|
| [`download_shapefile()`](https://ahadi-analytics.github.io/sntutils/reference/download_shapefile.md) | Pull WHO geohub boundaries by ISO3 and admin level |
| [`validate_process_spatial()`](https://ahadi-analytics.github.io/sntutils/reference/validate_process_spatial.md) | Validate / repair admin shapefiles |
| [`validate_process_coordinates()`](https://ahadi-analytics.github.io/sntutils/reference/validate_process_coordinates.md) | Validate facility lat / lon, drop low-precision rows |
| [`crosswalk_shapefiles_sf()`](https://ahadi-analytics.github.io/sntutils/reference/crosswalk_shapefiles_sf.md) | Area-weighted overlap between two shapefile vintages |
| [`fuzzy_match_facilities()`](https://ahadi-analytics.github.io/sntutils/reference/fuzzy_match_facilities.md), [`calculate_match_stats()`](https://ahadi-analytics.github.io/sntutils/reference/calculate_match_stats.md) | Match DHIS2 facilities to the master list |
| [`dhis2_map()`](https://ahadi-analytics.github.io/sntutils/reference/dhis2_map.md) | Rename DHIS2 columns via a dictionary |
| [`plot_admin_map_distinct()`](https://ahadi-analytics.github.io/sntutils/reference/plot_admin_map_distinct.md), [`facetted_map_bins()`](https://ahadi-analytics.github.io/sntutils/reference/facetted_map_bins.md), [`facetted_map_gradient()`](https://ahadi-analytics.github.io/sntutils/reference/facetted_map_gradient.md) | Categorical and continuous admin maps |
| [`get_palette()`](https://ahadi-analytics.github.io/sntutils/reference/get_palette.md), [`list_palettes()`](https://ahadi-analytics.github.io/sntutils/reference/list_palettes.md) | AHADI-branded plot palettes |

### [Reporting rates →](https://ahadi-analytics.github.io/sntutils/articles/reporting-rates.html)

Measure how completely facilities are reporting, by time and admin unit.

| Function | What it does |
|----|----|
| [`calculate_reporting_metrics()`](https://ahadi-analytics.github.io/sntutils/reference/calculate_reporting_metrics.md) | Reporting / missing rate; three scenarios (facility, two-dim, time-only) |
| [`calculate_reporting_metrics_dates()`](https://ahadi-analytics.github.io/sntutils/reference/calculate_reporting_metrics_dates.md) | Reporting rate from open / close dates |
| [`reporting_rate_plot()`](https://ahadi-analytics.github.io/sntutils/reference/reporting_rate_plot.md), [`reporting_rate_map()`](https://ahadi-analytics.github.io/sntutils/reference/reporting_rate_map.md) | Plot and map reporting completeness |
| [`classify_facility_activity()`](https://ahadi-analytics.github.io/sntutils/reference/classify_facility_activity.md), [`get_active_facilities()`](https://ahadi-analytics.github.io/sntutils/reference/get_active_facilities.md), [`facility_reporting_plot()`](https://ahadi-analytics.github.io/sntutils/reference/facility_reporting_plot.md) | Per-facility activity status and timelines |
| [`compare_methods_plot()`](https://ahadi-analytics.github.io/sntutils/reference/compare_methods_plot.md) | Compare two reporting-rule choices side by side |
| [`validate_routine_hf_data()`](https://ahadi-analytics.github.io/sntutils/reference/validate_routine_hf_data.md) | Structural checks before any of the above |

### [Data quality →](https://ahadi-analytics.github.io/sntutils/articles/data-quality.html)

Cascade consistency, outlier detection (3 methods), correction and
imputation.

| Function | What it does |
|----|----|
| [`consistency_check()`](https://ahadi-analytics.github.io/sntutils/reference/consistency_check.md), [`consistency_map()`](https://ahadi-analytics.github.io/sntutils/reference/consistency_map.md) | Flag cascade violations (e.g. tests \< confirmed cases) |
| [`detect_outliers()`](https://ahadi-analytics.github.io/sntutils/reference/detect_outliers.md), [`outlier_plot()`](https://ahadi-analytics.github.io/sntutils/reference/outlier_plot.md) | Detect outliers with mean / median / IQR rules |
| [`correct_outliers()`](https://ahadi-analytics.github.io/sntutils/reference/correct_outliers.md), [`impute_outlier_ma()`](https://ahadi-analytics.github.io/sntutils/reference/impute_outlier_ma.md), [`impute_higher_admin()`](https://ahadi-analytics.github.io/sntutils/reference/impute_higher_admin.md) | Replace flagged values |
| [`fallback_diff()`](https://ahadi-analytics.github.io/sntutils/reference/fallback_diff.md), [`fallback_row_sum()`](https://ahadi-analytics.github.io/sntutils/reference/fallback_row_sum.md), [`safe_sum()`](https://ahadi-analytics.github.io/sntutils/reference/safe_sum.md) | Defensive numerical helpers |

### [Climate →](https://ahadi-analytics.github.io/sntutils/articles/climate.html)

CHIRPS, ERA5, MODIS, NASA POWER download wrappers.

| Function | What it does |
|----|----|
| [`download_chirps()`](https://ahadi-analytics.github.io/sntutils/reference/download_chirps.md), [`check_chirps_available()`](https://ahadi-analytics.github.io/sntutils/reference/check_chirps_available.md), [`chirps_options()`](https://ahadi-analytics.github.io/sntutils/reference/chirps_options.md) | CHIRPS monthly rainfall |
| [`download_era5()`](https://ahadi-analytics.github.io/sntutils/reference/download_era5.md), [`check_era5_available()`](https://ahadi-analytics.github.io/sntutils/reference/check_era5_available.md), [`era5_options()`](https://ahadi-analytics.github.io/sntutils/reference/era5_options.md), [`read_era5()`](https://ahadi-analytics.github.io/sntutils/reference/read_era5.md), [`get_era5_metadata()`](https://ahadi-analytics.github.io/sntutils/reference/get_era5_metadata.md), [`print_era5_metadata()`](https://ahadi-analytics.github.io/sntutils/reference/print_era5_metadata.md), [`migrate_era5_filenames()`](https://ahadi-analytics.github.io/sntutils/reference/migrate_era5_filenames.md) | ERA5 reanalysis |
| [`download_modis()`](https://ahadi-analytics.github.io/sntutils/reference/download_modis.md), [`modis_options()`](https://ahadi-analytics.github.io/sntutils/reference/modis_options.md) | MODIS land-surface variables |
| [`download_process_nasapower()`](https://ahadi-analytics.github.io/sntutils/reference/download_process_nasapower.md) | NASA POWER agro-climate at points |

### [WorldPop →](https://ahadi-analytics.github.io/sntutils/articles/worldpop.html)

WorldPop downloads (totals, age bands, urbanicity, global mosaic),
extrapolation and SNT-shape reshape.

| Function | What it does |
|----|----|
| [`download_worldpop()`](https://ahadi-analytics.github.io/sntutils/reference/download_worldpop.md) | Total population, legacy + R2025A, 1 km / 100 m, count / density |
| [`download_worldpop_age_band()`](https://ahadi-analytics.github.io/sntutils/reference/download_worldpop_age_band.md) | Population for a specified age range and sex |
| [`download_worldpop_urbanicity()`](https://ahadi-analytics.github.io/sntutils/reference/download_worldpop_urbanicity.md) | Urban / peri-urban / rural classification |
| [`get_worldpop_paths()`](https://ahadi-analytics.github.io/sntutils/reference/get_worldpop_paths.md) | Resolve where downloaded files live |
| [`extrapolate_pop()`](https://ahadi-analytics.github.io/sntutils/reference/extrapolate_pop.md) | Fill years between or beyond observed years |
| [`snt_process_population()`](https://ahadi-analytics.github.io/sntutils/reference/snt_process_population.md) | Reshape into canonical SNT long format |

### [Rasters →](https://ahadi-analytics.github.io/sntutils/articles/rasters.html)

Batch raster processors that turn any raster archive (climate,
population, MAP, IHME) into admin-keyed tibbles.

| Function | What it does |
|----|----|
| [`process_raster_collection()`](https://ahadi-analytics.github.io/sntutils/reference/process_raster_collection.md), [`process_raster_with_boundaries()`](https://ahadi-analytics.github.io/sntutils/reference/process_raster_with_boundaries.md), [`process_rasters_by_year()`](https://ahadi-analytics.github.io/sntutils/reference/process_rasters_by_year.md) | Batch zonal stats; time-varying boundaries |
| [`process_weighted_raster_collection()`](https://ahadi-analytics.github.io/sntutils/reference/process_weighted_raster_collection.md), [`process_weighted_raster_stacks()`](https://ahadi-analytics.github.io/sntutils/reference/process_weighted_raster_stacks.md), [`normalize_raster_by_polygon()`](https://ahadi-analytics.github.io/sntutils/reference/normalize_raster_by_polygon.md) | Population-weighted extraction |
| [`process_ihme_u5m_raster()`](https://ahadi-analytics.github.io/sntutils/reference/process_ihme_u5m_raster.md) | IHME under-5 mortality rasters to admin tibble |
| [`tidy_malaria_raster_names()`](https://ahadi-analytics.github.io/sntutils/reference/tidy_malaria_raster_names.md), [`detect_time_pattern()`](https://ahadi-analytics.github.io/sntutils/reference/detect_time_pattern.md), [`extract_time_components()`](https://ahadi-analytics.github.io/sntutils/reference/extract_time_components.md), [`clean_filenames()`](https://ahadi-analytics.github.io/sntutils/reference/clean_filenames.md) | Raster-naming utilities |

### [DHS →](https://ahadi-analytics.github.io/sntutils/articles/dhs.html)

Discover, download and query DHS / MIS indicators via the API; open DHS
parquet microdata via DuckDB.

| Function | What it does |
|----|----|
| [`check_dhs_indicators()`](https://ahadi-analytics.github.io/sntutils/reference/check_dhs_indicators.md) | DHS API indicator catalogue |
| [`download_dhs_indicators()`](https://ahadi-analytics.github.io/sntutils/reference/download_dhs_indicators.md) | National / subnational indicator values |
| [`get_dhs_data()`](https://ahadi-analytics.github.io/sntutils/reference/get_dhs_data.md) | Register DHS parquet datasets as DuckDB views |

### [Project utilities →](https://ahadi-analytics.github.io/sntutils/articles/project-and-utilities.html)

Folder scaffolding, paths, translation, hashing, image compression,
small numeric helpers.

| Function | What it does |
|----|----|
| [`setup_project_paths()`](https://ahadi-analytics.github.io/sntutils/reference/setup_project_paths.md), [`ahadi_path()`](https://ahadi-analytics.github.io/sntutils/reference/ahadi_path.md) | Resolve standardised project paths |
| [`create_data_structure()`](https://ahadi-analytics.github.io/sntutils/reference/create_data_structure.md), [`initialize_project_structure()`](https://ahadi-analytics.github.io/sntutils/reference/initialize_project_structure.md) | Build the AHADI folder skeleton |
| [`clear_snt_cache()`](https://ahadi-analytics.github.io/sntutils/reference/clear_snt_cache.md) | Reset in-memory caches |
| [`translate_text()`](https://ahadi-analytics.github.io/sntutils/reference/translate_text.md), [`translate_text_vec()`](https://ahadi-analytics.github.io/sntutils/reference/translate_text_vec.md), [`translate_yearmon()`](https://ahadi-analytics.github.io/sntutils/reference/translate_yearmon.md), [`french_malaria_acronyms()`](https://ahadi-analytics.github.io/sntutils/reference/french_malaria_acronyms.md) | Cached translation and locale-aware formatting |
| [`compress_png()`](https://ahadi-analytics.github.io/sntutils/reference/compress_png.md) | PNG compression for reports |
| [`vdigest()`](https://ahadi-analytics.github.io/sntutils/reference/vdigest.md) | Vectorised hashing for stable IDs |
| [`big_mark()`](https://ahadi-analytics.github.io/sntutils/reference/big_mark.md), [`sum2()`](https://ahadi-analytics.github.io/sntutils/reference/sum2.md), [`mean2()`](https://ahadi-analytics.github.io/sntutils/reference/mean2.md), [`median2()`](https://ahadi-analytics.github.io/sntutils/reference/median2.md) | NA-safe numeric helpers |
| [`get_model()`](https://ahadi-analytics.github.io/sntutils/reference/get_model.md), [`generate_ir_plot()`](https://ahadi-analytics.github.io/sntutils/reference/generate_ir_plot.md), [`run_resistance_trend()`](https://ahadi-analytics.github.io/sntutils/reference/run_resistance_trend.md) | IR / resistance-trend plotting |
| [`auto_bin()`](https://ahadi-analytics.github.io/sntutils/reference/auto_bin.md), [`prepare_plot_data()`](https://ahadi-analytics.github.io/sntutils/reference/prepare_plot_data.md), [`get_pathway_vars()`](https://ahadi-analytics.github.io/sntutils/reference/get_pathway_vars.md) | Internal plot building blocks |

### EMOD simulation inputs

Used by AHADI’s malaria modelling team to feed EMOD-style simulations
from SNT data.

| Function | What it does |
|----|----|
| [`build_emod_demog()`](https://ahadi-analytics.github.io/sntutils/reference/build_emod_demog.md), [`build_emod_demog_from_wpp()`](https://ahadi-analytics.github.io/sntutils/reference/build_emod_demog_from_wpp.md) | Build demographic JSON |
| [`write_emod_demog_by_adm2()`](https://ahadi-analytics.github.io/sntutils/reference/write_emod_demog_by_adm2.md) | Per-adm2 demography files |
| [`read_emod_weather()`](https://ahadi-analytics.github.io/sntutils/reference/read_emod_weather.md), [`write_emod_weather()`](https://ahadi-analytics.github.io/sntutils/reference/write_emod_weather.md), [`write_emod_weather_by_adm2()`](https://ahadi-analytics.github.io/sntutils/reference/write_emod_weather_by_adm2.md) | Weather input files |

## Where to start

1.  Read [Get
    started](https://ahadi-analytics.github.io/sntutils/articles/getting-started.html)
    for a 5-minute tour and a tiny end-to-end example.
2.  Pick the workflow stage you’re working on from the **Articles** menu
    on the site.
3.  Use the **Reference** menu when you need the full argument list for
    a single function.

## Contributing

Issues and pull requests welcome:
<https://github.com/ahadi-analytics/sntutils/issues>.

Each function should ship with roxygen docs (including a runnable
`@examples` block) and a test in `tests/testthat/`.

## License

CC BY 4.0.
