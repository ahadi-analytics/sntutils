# Package index

## Data import and export

Read and write a wide range of tabular and spatial formats, with an
SNT-aware wrapper that hashes, fingerprints and writes sidecar metadata.

- [`read()`](https://ahadi-analytics.github.io/sntutils/reference/read.md)
  : Read in Data and Shapefiles from Various File Formats
- [`write()`](https://ahadi-analytics.github.io/sntutils/reference/write.md)
  : Save Data and Shapefiles to Various File Formats
- [`read_snt_data()`](https://ahadi-analytics.github.io/sntutils/reference/read_snt_data.md)
  : Read the most-recently modified saved dataset
- [`write_snt_data()`](https://ahadi-analytics.github.io/sntutils/reference/write_snt_data.md)
  : Write an object to standardized filenames in one or more formats

## Data cleaning and standardisation

Type inference, date parsing, name and column harmonisation, and
dictionary building.

- [`auto_parse_types()`](https://ahadi-analytics.github.io/sntutils/reference/auto_parse_types.md)
  : Infer column types using readr, then layer factor detection

- [`autoparse_dates()`](https://ahadi-analytics.github.io/sntutils/reference/autoparse_dates.md)
  : Parse Dates in a Data Frame

- [`available_date_formats`](https://ahadi-analytics.github.io/sntutils/reference/available_date_formats.md)
  :

  Available date formats for `autoparse_dates` function

- [`standardize_names()`](https://ahadi-analytics.github.io/sntutils/reference/standardize_names.md)
  : Standardize name-like strings with optional steps

- [`clean_filenames()`](https://ahadi-analytics.github.io/sntutils/reference/clean_filenames.md)
  : Clean Filenames

- [`prep_geonames()`](https://ahadi-analytics.github.io/sntutils/reference/prep_geonames.md)
  : Interactive Admin Name Cleaning and Matching

- [`build_dictionary()`](https://ahadi-analytics.github.io/sntutils/reference/build_dictionary.md)
  : build a compact data dictionary

- [`snt_data_dict()`](https://ahadi-analytics.github.io/sntutils/reference/snt_data_dict.md)
  : Load and flatten the SNT variable tree

- [`check_snt_var()`](https://ahadi-analytics.github.io/sntutils/reference/check_snt_var.md)
  : Detect and display the structural components of an SNT variable name

## Spatial validation and mapping

Validate and harmonise admin geometries and facility coordinates,
crosswalk between shapefile vintages, fuzzy-match names, and render
maps.

- [`validate_process_spatial()`](https://ahadi-analytics.github.io/sntutils/reference/validate_process_spatial.md)
  : Spatial Vector Validation and Cleaning
- [`validate_process_coordinates()`](https://ahadi-analytics.github.io/sntutils/reference/validate_process_coordinates.md)
  : Coordinate Validation and Cleaning
- [`fuzzy_match_facilities()`](https://ahadi-analytics.github.io/sntutils/reference/fuzzy_match_facilities.md)
  : Facility name matching across datasets (DHIS2 vs MFL)
- [`calculate_match_stats()`](https://ahadi-analytics.github.io/sntutils/reference/calculate_match_stats.md)
  : Calculate and report geo-naming match statistics
- [`crosswalk_shapefiles_sf()`](https://ahadi-analytics.github.io/sntutils/reference/crosswalk_shapefiles_sf.md)
  : Create an area-weighted crosswalk between old and new admin polygons
- [`dhis2_map()`](https://ahadi-analytics.github.io/sntutils/reference/dhis2_map.md)
  : Crosswalk DHIS2 dataset using dictionary
- [`plot_admin_map_distinct()`](https://ahadi-analytics.github.io/sntutils/reference/plot_admin_map_distinct.md)
  : Color admin groups so touching neighbors never share a color
- [`facetted_map_bins()`](https://ahadi-analytics.github.io/sntutils/reference/facetted_map_bins.md)
  : Plot Faceted Choropleth Maps from sf Data with Discrete Bins
- [`facetted_map_gradient()`](https://ahadi-analytics.github.io/sntutils/reference/facetted_map_gradient.md)
  : Plot Faceted Choropleth Maps from sf Data with Continuous Gradient
- [`get_palette()`](https://ahadi-analytics.github.io/sntutils/reference/get_palette.md)
  : Get a color palette
- [`list_palettes()`](https://ahadi-analytics.github.io/sntutils/reference/list_palettes.md)
  : List available palette names
- [`download_shapefile()`](https://ahadi-analytics.github.io/sntutils/reference/download_shapefile.md)
  : Download WHO Administrative Boundaries with Partial Update

## Routine surveillance — reporting rates

Quantify and visualise how completely health facilities are reporting,
by time and admin unit.

- [`calculate_reporting_metrics()`](https://ahadi-analytics.github.io/sntutils/reference/calculate_reporting_metrics.md)
  : Calculate reporting/missing rate and proportion of reporting
  facilities
- [`calculate_reporting_metrics_dates()`](https://ahadi-analytics.github.io/sntutils/reference/calculate_reporting_metrics_dates.md)
  : Calculate reporting metrics based on facility date ranges
- [`reporting_rate_plot()`](https://ahadi-analytics.github.io/sntutils/reference/reporting_rate_plot.md)
  : Plot Missing data or Reporting Rate over time
- [`reporting_rate_map()`](https://ahadi-analytics.github.io/sntutils/reference/reporting_rate_map.md)
  : Plot reporting rate maps over time
- [`facility_reporting_plot()`](https://ahadi-analytics.github.io/sntutils/reference/facility_reporting_plot.md)
  : Plot monthly reporting activity by health facility
- [`classify_facility_activity()`](https://ahadi-analytics.github.io/sntutils/reference/classify_facility_activity.md)
  : Classify health facility activity status by reporting behaviour
- [`get_active_facilities()`](https://ahadi-analytics.github.io/sntutils/reference/get_active_facilities.md)
  : Get active facilities from a dataset
- [`validate_routine_hf_data()`](https://ahadi-analytics.github.io/sntutils/reference/validate_routine_hf_data.md)
  : Orchestrates a suite of validation checks on routine HF data. It
  standardizes column resolution, selects indicators, runs
  missing/duplicate/future/logic/ outlier checks, compiles a summary,
  and optionally translates and saves.

## Routine surveillance — data quality

Cascade consistency checks, outlier detection and correction, imputation
helpers.

- [`consistency_check()`](https://ahadi-analytics.github.io/sntutils/reference/consistency_check.md)
  : Consistency Check Function
- [`consistency_map()`](https://ahadi-analytics.github.io/sntutils/reference/consistency_map.md)
  : Consistency violation map
- [`detect_outliers()`](https://ahadi-analytics.github.io/sntutils/reference/detect_outliers.md)
  : Detect outliers with guardrails and consensus
- [`outlier_plot()`](https://ahadi-analytics.github.io/sntutils/reference/outlier_plot.md)
  : Create Outlier Detection Plots
- [`correct_outliers()`](https://ahadi-analytics.github.io/sntutils/reference/correct_outliers.md)
  : Correct outliers using temporal neighbors
- [`impute_outlier_ma()`](https://ahadi-analytics.github.io/sntutils/reference/impute_outlier_ma.md)
  : Impute outliers using moving average from adjacent time points
- [`impute_higher_admin()`](https://ahadi-analytics.github.io/sntutils/reference/impute_higher_admin.md)
  : Impute higher administrative level using a lookup table
- [`compare_methods_plot()`](https://ahadi-analytics.github.io/sntutils/reference/compare_methods_plot.md)
  : Compare facility activity classification methods (multilingual)
- [`fallback_diff()`](https://ahadi-analytics.github.io/sntutils/reference/fallback_diff.md)
  : Fallback Absolute Difference Between Two Vectors (type-preserving)
- [`fallback_row_sum()`](https://ahadi-analytics.github.io/sntutils/reference/fallback_row_sum.md)
  : Smart row-wise sum with missing data handling and type preservation
- [`safe_sum()`](https://ahadi-analytics.github.io/sntutils/reference/safe_sum.md)
  : Row-safe sum for grouped aggregation

## Climate and environmental downloads

Pull rainfall, temperature, land cover and other environmental rasters
from public archives.

- [`download_chirps()`](https://ahadi-analytics.github.io/sntutils/reference/download_chirps.md)
  : Download CHIRPS Raster Data from UCSB Archive
- [`check_chirps_available()`](https://ahadi-analytics.github.io/sntutils/reference/check_chirps_available.md)
  : List Available CHIRPS Raster Files for a Dataset
- [`chirps_options()`](https://ahadi-analytics.github.io/sntutils/reference/chirps_options.md)
  : List Available Monthly CHIRPS Dataset Options
- [`download_era5()`](https://ahadi-analytics.github.io/sntutils/reference/download_era5.md)
  : Download ERA5 Data from Copernicus CDS
- [`check_era5_available()`](https://ahadi-analytics.github.io/sntutils/reference/check_era5_available.md)
  : List Available ERA5 Files for a Dataset
- [`era5_options()`](https://ahadi-analytics.github.io/sntutils/reference/era5_options.md)
  : List Available ERA5 Dataset Options
- [`get_era5_metadata()`](https://ahadi-analytics.github.io/sntutils/reference/get_era5_metadata.md)
  : Get Metadata from ERA5 NetCDF File
- [`print_era5_metadata()`](https://ahadi-analytics.github.io/sntutils/reference/print_era5_metadata.md)
  : Print ERA5 Metadata Summary
- [`read_era5()`](https://ahadi-analytics.github.io/sntutils/reference/read_era5.md)
  : Read and Process ERA5 NetCDF Files to Tidy Data
- [`migrate_era5_filenames()`](https://ahadi-analytics.github.io/sntutils/reference/migrate_era5_filenames.md)
  : Migrate ERA5 Filenames to New Format
- [`download_modis()`](https://ahadi-analytics.github.io/sntutils/reference/download_modis.md)
  : Download MODIS Data from NASA
- [`modis_options()`](https://ahadi-analytics.github.io/sntutils/reference/modis_options.md)
  : List Available MODIS Products
- [`download_process_nasapower()`](https://ahadi-analytics.github.io/sntutils/reference/download_process_nasapower.md)
  : Download and process NASA POWER daily climate data
- [`process_ihme_u5m_raster()`](https://ahadi-analytics.github.io/sntutils/reference/process_ihme_u5m_raster.md)
  : Extract Under-5 Mortality Values from IHME Raster Stack

## Population downloads and processing

WorldPop downloads (totals, age-bands, urbanicity), extrapolation, and
SNT-shaped population outputs.

- [`download_worldpop()`](https://ahadi-analytics.github.io/sntutils/reference/download_worldpop.md)
  : Download Population Rasters from WorldPop
- [`download_worldpop_age_band()`](https://ahadi-analytics.github.io/sntutils/reference/download_worldpop_age_band.md)
  : Download WorldPop Population Raster Data for Specific Age Bands
- [`download_worldpop_urbanicity()`](https://ahadi-analytics.github.io/sntutils/reference/download_worldpop_urbanicity.md)
  : Download WorldPop DUG Urbanicity Rasters
- [`get_worldpop_paths()`](https://ahadi-analytics.github.io/sntutils/reference/get_worldpop_paths.md)
  : Download WorldPop Rasters and Get Paths
- [`extrapolate_pop()`](https://ahadi-analytics.github.io/sntutils/reference/extrapolate_pop.md)
  : Extrapolate Population Estimates for Target Years
- [`snt_process_population()`](https://ahadi-analytics.github.io/sntutils/reference/snt_process_population.md)
  : Summarise population by available admin levels and build a
  dictionary

## Raster batch processing

Aggregate raster stacks to admin units (zonal stats), with optional
population weighting and time-varying boundaries.

- [`process_raster_collection()`](https://ahadi-analytics.github.io/sntutils/reference/process_raster_collection.md)
  : Process multiple raster files from a directory
- [`process_raster_with_boundaries()`](https://ahadi-analytics.github.io/sntutils/reference/process_raster_with_boundaries.md)
  : Process raster data with administrative boundaries
- [`process_rasters_by_year()`](https://ahadi-analytics.github.io/sntutils/reference/process_rasters_by_year.md)
  : Process Year-Indexed Rasters Against Time-Varying Admin Boundaries
- [`process_weighted_raster_collection()`](https://ahadi-analytics.github.io/sntutils/reference/process_weighted_raster_collection.md)
  : Process Weighted Raster Data in Batch
- [`process_weighted_raster_stacks()`](https://ahadi-analytics.github.io/sntutils/reference/process_weighted_raster_stacks.md)
  : Process Weighted Raster Stacks
- [`normalize_raster_by_polygon()`](https://ahadi-analytics.github.io/sntutils/reference/normalize_raster_by_polygon.md)
  : Normalize Raster Values by Polygon Regions
- [`tidy_malaria_raster_names()`](https://ahadi-analytics.github.io/sntutils/reference/tidy_malaria_raster_names.md)
  : Normalize malariaAtlas Raster Filenames

## DHS indicators

Bulk download and lookup of DHS/MIS indicators.

- [`download_dhs_indicators()`](https://ahadi-analytics.github.io/sntutils/reference/download_dhs_indicators.md)
  : Query DHS API Directly via URL Parameters
- [`check_dhs_indicators()`](https://ahadi-analytics.github.io/sntutils/reference/check_dhs_indicators.md)
  : Check DHS Indicator List from API
- [`get_dhs_data()`](https://ahadi-analytics.github.io/sntutils/reference/get_dhs_data.md)
  : Load DHS Parquet datasets using DuckDB

## EMOD demography and weather inputs

Build demographic and climate inputs for EMOD-style simulations from SNT
data.

- [`build_emod_demog()`](https://ahadi-analytics.github.io/sntutils/reference/build_emod_demog.md)
  : Build EMOD Demographics JSON for a Single Node
- [`build_emod_demog_from_wpp()`](https://ahadi-analytics.github.io/sntutils/reference/build_emod_demog_from_wpp.md)
  : Build EMOD Demographic Inputs from UN WPP 2024 Data
- [`write_emod_demog_by_adm2()`](https://ahadi-analytics.github.io/sntutils/reference/write_emod_demog_by_adm2.md)
  : Write EMOD Demographics JSON Files by ADM2
- [`read_emod_weather()`](https://ahadi-analytics.github.io/sntutils/reference/read_emod_weather.md)
  : Read EMOD weather binary files into a data.frame
- [`write_emod_weather()`](https://ahadi-analytics.github.io/sntutils/reference/write_emod_weather.md)
  : Write EMOD binary weather files (.bin + .json) from a data.frame
- [`write_emod_weather_by_adm2()`](https://ahadi-analytics.github.io/sntutils/reference/write_emod_weather_by_adm2.md)
  : Write EMOD weather files per adm2 (one folder per district)

## Project structure and paths

Create the AHADI hierarchical data folders, set up a full project
skeleton, resolve standardised paths.

- [`setup_project_paths()`](https://ahadi-analytics.github.io/sntutils/reference/setup_project_paths.md)
  : Setup project paths and environment for SNT pipeline
- [`create_data_structure()`](https://ahadi-analytics.github.io/sntutils/reference/create_data_structure.md)
  : Create Hierarchical Data Folder Structure (AHADI Style)
- [`initialize_project_structure()`](https://ahadi-analytics.github.io/sntutils/reference/initialize_project_structure.md)
  : Initialize Full Project Folder Structure
- [`ahadi_path()`](https://ahadi-analytics.github.io/sntutils/reference/ahadi_path.md)
  : Resolve path inside AHADI OneDrive shared library
- [`clear_snt_cache()`](https://ahadi-analytics.github.io/sntutils/reference/clear_snt_cache.md)
  : clear snt variable tree cache

## Translation and localisation

Cached Google Translate wrappers plus locale-aware year-month
formatting.

- [`translate_text()`](https://ahadi-analytics.github.io/sntutils/reference/translate_text.md)
  : Translate text to target language with persistent file cache
- [`translate_text_vec()`](https://ahadi-analytics.github.io/sntutils/reference/translate_text_vec.md)
  : Vectorized version of translate_text function
- [`translate_yearmon()`](https://ahadi-analytics.github.io/sntutils/reference/translate_yearmon.md)
  : Convert date to yearmon format with localized month names
- [`french_malaria_acronyms()`](https://ahadi-analytics.github.io/sntutils/reference/french_malaria_acronyms.md)
  : French malaria acronyms mapping

## Plotting helpers and trend models

Reusable building blocks for SNT plots — palettes, model wrappers, IR
plots.

- [`get_model()`](https://ahadi-analytics.github.io/sntutils/reference/get_model.md)
  : Fit regression model for insecticide resistance trends
- [`generate_ir_plot()`](https://ahadi-analytics.github.io/sntutils/reference/generate_ir_plot.md)
  : Generate insecticide resistance trend plot
- [`run_resistance_trend()`](https://ahadi-analytics.github.io/sntutils/reference/run_resistance_trend.md)
  : Generate insecticide resistance trend scenarios
- [`prepare_plot_data()`](https://ahadi-analytics.github.io/sntutils/reference/prepare_plot_data.md)
  : Prepare data for reporting rate or missing data visualization
- [`get_pathway_vars()`](https://ahadi-analytics.github.io/sntutils/reference/get_pathway_vars.md)
  : Get Upstream and Downstream Variables for Malaria Pathway Indicators
- [`auto_bin()`](https://ahadi-analytics.github.io/sntutils/reference/auto_bin.md)
  : Automatically bin numeric data for choropleth maps
- [`detect_factors()`](https://ahadi-analytics.github.io/sntutils/reference/detect_factors.md)
  : Detect factor-like character columns (low-cardinality only)
- [`detect_time_pattern()`](https://ahadi-analytics.github.io/sntutils/reference/detect_time_pattern.md)
  : Detect time pattern in filenames
- [`extract_time_components()`](https://ahadi-analytics.github.io/sntutils/reference/extract_time_components.md)
  : Extract time components from a filename

## Numeric and hashing utilities

Small but very-used helpers that show up across SNT pipelines.

- [`big_mark()`](https://ahadi-analytics.github.io/sntutils/reference/big_mark.md)
  : Format Numbers with Thousand Separator
- [`sum2()`](https://ahadi-analytics.github.io/sntutils/reference/sum2.md)
  : Sum values with automatic NA handling
- [`mean2()`](https://ahadi-analytics.github.io/sntutils/reference/mean2.md)
  : Calculate mean with automatic NA handling
- [`median2()`](https://ahadi-analytics.github.io/sntutils/reference/median2.md)
  : Calculate median with automatic NA handling
- [`vdigest()`](https://ahadi-analytics.github.io/sntutils/reference/vdigest.md)
  : Vectorized version of digest::digest
- [`compress_png()`](https://ahadi-analytics.github.io/sntutils/reference/compress_png.md)
  : Compress PNG Files in a Directory or a Single PNG File with pngquant
