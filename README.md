
<!-- badges -->

[![R-CMD-check](https://github.com/ahadi-analytics/sntutils/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ahadi-analytics/sntutils/actions/workflows/R-CMD-check.yaml)
[![pkgdown](https://github.com/ahadi-analytics/sntutils/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/ahadi-analytics/sntutils/actions/workflows/pkgdown.yaml)
[![CodeFactor](https://www.codefactor.io/repository/github/ahadi-analytics/sntutils/badge)](https://www.codefactor.io/repository/github/ahadi-analytics/sntutils)
![Coverage](man/badges/coverage.svg)

# sntutils <img src="man/figures/logo.png" align="right" height="140" alt="sntutils hex logo" />

`sntutils` is an R package developed by [AHADI](https://appliedhealthanalytics.org) to support the **Subnational Tailoring (SNT)** of malaria interventions. It bundles the small, repeated operations every SNT support analysis does - reading DHIS2 exports, harmonising admin names across shapefile vintages, validating facility coordinates, calculating reporting rates, extracting climate and population rasters to admin units, and rendering country-team-language plots and maps.

> **Full documentation, with worked examples for every workflow, lives at <https://ahadi-analytics.github.io/sntutils/>**

The pkgdown site is the canonical reference. This README is a short orientation and an index of the package's surface area.

## Install

```r
# install pak if needed
install.packages("pak")

# install sntutils from GitHub
pak::pkg_install("ahadi-analytics/sntutils")
```

System dependencies: `sntutils` uses `sf` and `terra`, which require GDAL, GEOS and PROJ. On macOS install with `brew install gdal proj geos`; on Ubuntu use the GDAL PPA.

## Quick start

```r
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

The [Get started article](https://ahadi-analytics.github.io/sntutils/articles/getting-started.html) walks through the same example end-to-end with plotted output.

## What's inside

The package exports ~100 functions, grouped by workflow stage. Each row links to the article that covers that group in depth.

### [Read & clean →](https://ahadi-analytics.github.io/sntutils/articles/data-io-and-cleaning.html)

Read and write any common SNT format, parse messy dates, infer column types, standardise admin and facility names, build data dictionaries.

| Function | What it does |
| --- | --- |
| `read()`, `write()` | Read / write CSV, Excel, Stata, SPSS, RDS, GeoJSON, shapefile, … |
| `read_snt_data()`, `write_snt_data()` | Atomic, hashed reads / writes with sidecar metadata |
| `autoparse_dates()`, `available_date_formats` | Detect and standardise mixed date formats |
| `auto_parse_types()`, `detect_factors()` | Infer numeric / integer / factor / date types |
| `standardize_names()`, `clean_filenames()` | Tidy admin / facility names and file names |
| `prep_geonames()` | Interactive admin-name harmonisation with caching |
| `build_dictionary()`, `snt_data_dict()`, `check_snt_var()` | Variable dictionaries |

### [Spatial →](https://ahadi-analytics.github.io/sntutils/articles/spatial.html)

Validate admin geometries and facility coordinates, crosswalk between shapefile vintages, fuzzy-match facilities, render maps.

| Function | What it does |
| --- | --- |
| `download_shapefile()` | Pull WHO geohub boundaries by ISO3 and admin level |
| `validate_process_spatial()` | Validate / repair admin shapefiles |
| `validate_process_coordinates()` | Validate facility lat / lon, drop low-precision rows |
| `crosswalk_shapefiles_sf()` | Area-weighted overlap between two shapefile vintages |
| `fuzzy_match_facilities()`, `calculate_match_stats()` | Match DHIS2 facilities to the master list |
| `dhis2_map()` | Rename DHIS2 columns via a dictionary |
| `plot_admin_map_distinct()`, `facetted_map_bins()`, `facetted_map_gradient()` | Categorical and continuous admin maps |
| `get_palette()`, `list_palettes()` | AHADI-branded plot palettes |

### [Reporting rates →](https://ahadi-analytics.github.io/sntutils/articles/reporting-rates.html)

Measure how completely facilities are reporting, by time and admin unit.

| Function | What it does |
| --- | --- |
| `calculate_reporting_metrics()` | Reporting / missing rate; three scenarios (facility, two-dim, time-only) |
| `calculate_reporting_metrics_dates()` | Reporting rate from open / close dates |
| `reporting_rate_plot()`, `reporting_rate_map()` | Plot and map reporting completeness |
| `classify_facility_activity()`, `get_active_facilities()`, `facility_reporting_plot()` | Per-facility activity status and timelines |
| `compare_methods_plot()` | Compare two reporting-rule choices side by side |
| `validate_routine_hf_data()` | Structural checks before any of the above |

### [Data quality →](https://ahadi-analytics.github.io/sntutils/articles/data-quality.html)

Cascade consistency, outlier detection (3 methods), correction and imputation.

| Function | What it does |
| --- | --- |
| `consistency_check()`, `consistency_map()` | Flag cascade violations (e.g. tests < confirmed cases) |
| `detect_outliers()`, `outlier_plot()` | Detect outliers with mean / median / IQR rules |
| `correct_outliers()`, `impute_outlier_ma()`, `impute_higher_admin()` | Replace flagged values |
| `fallback_diff()`, `fallback_row_sum()`, `safe_sum()` | Defensive numerical helpers |

### [Climate →](https://ahadi-analytics.github.io/sntutils/articles/climate.html)

CHIRPS, ERA5, MODIS, NASA POWER, IHME - plus the batch raster processors that bring them to admin units.

| Function | What it does |
| --- | --- |
| `download_chirps()`, `check_chirps_available()`, `chirps_options()` | CHIRPS monthly rainfall |
| `download_era5()`, `check_era5_available()`, `era5_options()`, `read_era5()`, `get_era5_metadata()`, `print_era5_metadata()`, `migrate_era5_filenames()` | ERA5 reanalysis |
| `download_modis()`, `modis_options()` | MODIS land-surface variables |
| `download_process_nasapower()` | NASA POWER agro-climate at points |
| `process_ihme_u5m_raster()` | IHME under-5 mortality rasters → admin tibble |
| `process_raster_collection()`, `process_raster_with_boundaries()`, `process_rasters_by_year()` | Batch zonal stats |
| `process_weighted_raster_collection()`, `process_weighted_raster_stacks()`, `normalize_raster_by_polygon()` | Population-weighted extraction |
| `tidy_malaria_raster_names()`, `detect_time_pattern()`, `extract_time_components()` | Raster-naming utilities |

### [WorldPop →](https://ahadi-analytics.github.io/sntutils/articles/worldpop.html)

WorldPop downloads (totals, age bands, urbanicity, global mosaic), extrapolation and SNT-shape reshape.

| Function | What it does |
| --- | --- |
| `download_worldpop()` | Total population, legacy + R2025A, 1 km / 100 m, count / density |
| `download_worldpop_age_band()` | Population for a specified age range and sex |
| `download_worldpop_urbanicity()` | Urban / peri-urban / rural classification |
| `get_worldpop_paths()` | Resolve where downloaded files live |
| `extrapolate_pop()` | Fill years between or beyond observed years |
| `snt_process_population()` | Reshape into canonical SNT long format |

### [DHS →](https://ahadi-analytics.github.io/sntutils/articles/dhs.html)

Discover, download and query DHS / MIS indicators via the API; open DHS parquet microdata via DuckDB.

| Function | What it does |
| --- | --- |
| `check_dhs_indicators()` | DHS API indicator catalogue |
| `download_dhs_indicators()` | National / subnational indicator values |
| `get_dhs_data()` | Register DHS parquet datasets as DuckDB views |

### [Project utilities →](https://ahadi-analytics.github.io/sntutils/articles/project-and-utilities.html)

Folder scaffolding, paths, translation, hashing, image compression, small numeric helpers.

| Function | What it does |
| --- | --- |
| `setup_project_paths()`, `ahadi_path()` | Resolve standardised project paths |
| `create_data_structure()`, `initialize_project_structure()` | Build the AHADI folder skeleton |
| `clear_snt_cache()` | Reset in-memory caches |
| `translate_text()`, `translate_text_vec()`, `translate_yearmon()`, `french_malaria_acronyms()` | Cached translation and locale-aware formatting |
| `compress_png()` | PNG compression for reports |
| `vdigest()` | Vectorised hashing for stable IDs |
| `big_mark()`, `sum2()`, `mean2()`, `median2()` | NA-safe numeric helpers |
| `get_model()`, `generate_ir_plot()`, `run_resistance_trend()` | IR / resistance-trend plotting |
| `auto_bin()`, `prepare_plot_data()`, `get_pathway_vars()` | Internal plot building blocks |

### EMOD simulation inputs

Used by AHADI's malaria modelling team to feed EMOD-style simulations from SNT data.

| Function | What it does |
| --- | --- |
| `build_emod_demog()`, `build_emod_demog_from_wpp()` | Build demographic JSON |
| `write_emod_demog_by_adm2()` | Per-adm2 demography files |
| `read_emod_weather()`, `write_emod_weather()`, `write_emod_weather_by_adm2()` | Weather input files |

## Where to start

1. Read [Get started](https://ahadi-analytics.github.io/sntutils/articles/getting-started.html) for a 5-minute tour and a tiny end-to-end example.
2. Pick the workflow stage you're working on from the **Articles** menu on the site.
3. Use the **Reference** menu when you need the full argument list for a single function.

## Contributing

Issues and pull requests welcome: <https://github.com/ahadi-analytics/sntutils/issues>.

Each function should ship with roxygen docs (including a runnable `@examples` block) and a test in `tests/testthat/`.

## License

CC BY 4.0.
