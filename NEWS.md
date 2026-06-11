# sntutils 2.0.0 (2026-06-11)

## Documentation

* New `pkgdown` site with workflow vignettes covering data cleaning,
  spatial preparation, climate downloads and raster processing. README
  rewritten to point at the site.
* New hex sticker logo at `man/figures/logo.png`.

## New features

### Climate and earth-observation downloads

* `download_nasapower()` pulls NASA POWER climate variables (temperature,
  precipitation, humidity, wind, radiation) plus dewpoint, elevation and
  11 more added in this release. Samples multiple points per polygon
  (default 3) for better representativeness.
* `download_modis()` and `modis_options()` pull NASA MODIS imagery
  directly, replacing the earlier `download_appeears()` wrapper.
* `download_worldpop_urbanicity()` downloads WorldPop urbanicity layers.
* `download_worldpop_age_band()` gains a `sex` parameter, handles the
  80+ band correctly, and respects a `quiet` flag.
* `get_worldpop_paths()` wraps multi-group WorldPop download and path
  building in a single call.
* `download_worldpop()` now uses the `t` (total) variant by default and
  exposes a `GLOBAL` mosaic option.

### Raster processing

* `process_rasters_by_year()` wraps `process_raster_collection()` for
  pipelines where the admin boundary changes year over year.
* `process_raster_with_boundaries()` gains optional population weighting
  and an optional time-pattern detection switch.
* Rasters are cropped to the shapefile extent before extraction, cutting
  runtime substantially on large global rasters.
* Compact `YYYYMM` date pattern now supported by `process_rasters()`
  helpers alongside `YYYY` and `YYYY-MM`.

### Mapping and visualization

* `facetted_map_bins()` and `facetted_map_gradient()` draw faceted
  choropleths for discrete and continuous data. Both support `caption`,
  fixed colour-scale `limits` and configurable text sizes.
* Insecticide-resistance trend plotting functions added, with built-in
  PNG compression.
* `auto_bin()` rewritten with robust outlier handling and adaptive
  labels; companion colour palette utilities added.

### EMOD pipeline

* `write_emod_demog()` writes EMOD demographics JSON with correct nested
  `AgeDistribution` arrays.
* `read_emod_weather()` and `write_emod_weather_by_adm2()` added, with a
  `folder_case` parameter and correct filename handling when
  `climate_profile` is empty.

### Reporting and data quality

* New reporting-rate functions computed directly from date columns.
* Reporting-rate plots and outlier detection extended with more
  diagnostics and standardized plot filenames for OneDrive sync.
* Outlier correction and cascade logic refined.

### Spatial validation

* `validate_process_spatial()` runs a multi-stage boundary-artifact
  cleaning pass (`make_valid`, s2 toggling, zero-width buffer, hole
  removal) so aggregated geometries are always valid and render cleanly
  without double lines or polygon overlap.
* `.clean_coord_string()` helper hardens coordinate parsing against
  varied input formats.
* `crosswalk_shapefiles_sf()` exported and now works at adm1 without
  requiring an `adm2` column.

### Dictionary and variable tree

* `var_tree.yml` extended with ACT, IRS, ANC, EPI, SMC, IPTp4+, ITN, N5
  cascade, care-seeking adjustment factors, severe anemia, household
  size, vaccine, health-facility variables, trained-provider
  care-seeking, and rate/incidence/coverage indicators.

### File handling

* `clean_filenames()` refactored as a pure string operation with an
  explicit path-mapping helper.

## Bug fixes

* `crosswalk_shapefiles_sf()` no longer requires an `adm2` column when
  matching at adm1.
* `auto_bin()` rounding bug corrected.
* `clean_filenames()` no longer drops column names when piped through
  `as.data.frame()`.
* `download_worldpop_age_band()` recovers from corrupt cached files.
* `download_worldpop()` and friends handle download timeouts gracefully.
* `fuzzy_match_facilities()` adds error handling for cloud-storage
  timeouts.
* NASA POWER download no longer emits unnamed-column warnings when
  `n_sample = 1`.
* Two pre-existing test flakes fixed.

## Breaking changes

* `download_appeears()` and its documentation removed in favour of
  `download_modis()`. Migrate by replacing calls with `download_modis()`
  and pulling product/band names from `modis_options()`.

## Internals

* Atomic write path hardened on Windows: retrying `fs::file_move`,
  resilience to path and lock failures, tests skipped on Windows where
  reliable behaviour can't be guaranteed.
* CI workflows slimmed down — live integrations gated, oldrel-1 dropped
  now that `ggrepel` requires R >= 4.5.0; Windows dependency install
  step added.
* System-dependency install script for macOS, Ubuntu and Fedora.
* Workflow added to block AI mentions in commit messages.

# sntutils 1.12.6 (2026-04-23)

## New features

* `tidy_malaria_raster_names()` normalizes filenames emitted by
  `malariaAtlas::getRaster()` (strips bounding-box coordinates and download
  stamps, keeps `...Rate.YYYY.tiff`). Idempotent; also removes the
  `getRaster/` artifact directory when it renames files.
* `extract_time_components()` gains a `year_extractor` argument. Pass a
  custom function (filename -> year) when the default "last plausible
  4-digit year in 1980-2099" heuristic picks the wrong token.
* `process_raster_collection()` and `process_weighted_raster_collection()`
  expose `year_extractor` as a pass-through to `extract_time_components()`.

## Breaking changes

* `detect_time_pattern()` now uses `base::all()` instead of `base::any()`
  when deciding whether a batch of filenames belongs to a given time
  pattern. Batches containing files of mixed granularity no longer get
  silently misclassified as the most specific pattern present. Callers
  relying on the previous lenient behaviour should pre-filter their file
  lists.
* `detect_time_pattern()` tightens its yearly regex to plausible years
  only (`19[89]\d` or `20\d{2}`) with digit word boundaries, so tokens
  like `202005` (a `YYYYMM` stamp) no longer match as a year.

## Internals

* `process_weighted_raster_collection()` and `process_raster_collection()`
  now iterate via `purrr::map()` with `.progress = ...`, replacing the
  hand-rolled `progress::progress_bar` and `for` loop. Population rasters
  are matched using word-boundary regex (`(?<!\d)YYYY(?!\d)`) so year
  substrings no longer false-match longer digit runs.
* `purrr` (>= 1.0.0) moved from `Suggests` to `Imports`.

# sntutils 1.12.5 (2026-03-02)

## New features

* `download_modis()` and `modis_options()` for NASA MODIS imagery.
* EMOD pipeline: `write_emod_demog()`, `read_emod_weather()` and
  `write_emod_weather_by_adm2()`.
* `facetted_map_gradient()` for continuous-data faceted choropleths.
  `facetted_map_bins()` gains `caption`, fixed colour-scale `limits`
  and text-sizing controls.
* Insecticide-resistance trend plotting (with PNG compression).
* WorldPop: `get_worldpop_paths()` multi-group wrapper;
  `download_worldpop_age_band()` gains a `sex` parameter, fixes the
  80+ band, and respects a `quiet` flag.
* `correct_outliers()` for temporal outlier correction.
* Compact `YYYYMM` date pattern in `process_rasters` helpers.
* Togo climate-download example scripts (EN + FR).
* DHS indicator definitions for ACT, IRS, ANC, EPI, SMC and IPTp4+
  added to var_tree.

## Bug fixes

* `validate_process_spatial()` runs a multi-stage boundary-artifact
  cleaning pass (`make_valid`, s2 toggling, zero-width buffer, hole
  removal) so aggregated geometries are always valid and render cleanly
  without double lines or polygon overlap.
* `crosswalk_shapefiles_sf()` no longer requires an `adm2` column at
  adm1 level.
* `download_worldpop_age_band()` recovers from corrupt cached files;
  download timeout fixed.
* `fuzzy_match_facilities()` handles cloud-storage timeouts.
* `clean_filenames()` no longer drops column names through piped
  `as.data.frame()`.
* `auto_bin()` rounding bug.
* NASA POWER no longer emits unnamed-column warnings when
  `n_sample = 1`.
* EMOD weather filename correct when `climate_profile` is empty.
* R CMD check warnings and notes.

## Breaking changes

* `download_appeears()` removed in favour of `download_modis()`.

## Internals

* `clean_filenames()` refactored as a pure string operation with an
  explicit path-mapping helper.
* Workflow added to block AI mentions in commit messages.
* macOS/Ubuntu/Fedora system-dependency install script.
* CI: oldrel-1 dropped (`ggrepel` requires R >= 4.5.0).

# sntutils 1.12.4 (2026-02-02)

## New features

* `download_appeears()` and `appeears_options()` for NASA AppEEARS
  data (later replaced by `download_modis()` in 1.12.5).
* `facetted_map_bins()` works without facets, and gains text sizing
  control and optional title/subtitle.
* PNG compression applied to dictionary outputs.

## Bug fixes

* `auto_bin()` rounding bug.
* Variable-tree apostrophe preservation in names.
* IDP population paths added to folder structure.

# sntutils 1.12.3 (2026-01-09)

## New features

* `download_nasapower()` for NASA POWER climate data. Samples multiple
  points per polygon (default 3) for better representativeness.
* `crosswalk_shapefiles_sf()` exported.
* New reporting-rate function computed directly from date columns.
* `auto_bin()` and companion colour palette utilities for mapping.
* `ahadi_path()` for resolving OneDrive shared-library paths.
* var_tree extended with incidence cascade, MBG estimates,
  trained-provider care-seeking variables and rate/coverage terms.

## Bug fixes

* `prep_geonames` UTF-8 handling.
* ISO country-code joins use names rather than coordinates.
* `rnaturalearth` no longer prompts to download already-cached data.
* Population data enforces unique admin-year rows.

## Breaking changes

* `validation_terms` dataset deprecated; migrate to `var_tree`.
* `rate` / `rate_cat` renamed to `incid_rate` / `incid_rate_cat`.

## Internals

* `arrow` moved to Suggests.
* Windows dependency install step added to CI.

# sntutils 1.12.2 (2025-12-16)

## New features

* `facetted_map_bins()` for faceted choropleth mapping.
* `crosswalk_shapefiles_sf()` added (initial).
* New climate variables in var_tree.
* DHS severe anemia indicators in var_tree.

## Enhancements

* Plot filenames shortened and standardized for OneDrive sync.
* Translation logic improvements and variable label updates.
* Tighter `data_name` validation.

## Bug fixes

* Context-specific name matching in user interaction.

# sntutils 1.12.1 (2025-12-11)

## Enhancements

* Reporting-rate plots and outlier detection extended with more
  diagnostics.
* Outlier correction and cascade logic refined.

## Bug fixes

* Validation output trims `neighbor_values`.

## Internals

* CI: missing system dependencies installed.
* System dependencies and R package versions updated.

# sntutils 1.12.0 (2025-12-05)

## Enhancements

* Validation and outlier-correction workflow consolidated and overhauled,
  building on the foundation laid in 1.11.x.

# sntutils 1.11.9 (2025-11-28)

## New features

* `correct_outliers()` for temporal outlier correction.
* `validation_terms` dictionary and data-dictionary support.
* Consensus colour visualization in `outlier_plot()`.
* `hf_name_col` support in consistency checks.

## Enhancements

* Caching for SNT variable tree, with improved dictionary filtering.
* `dhis2_map()` mapping validation improved.
* Missing-data checks extended with detailed summaries.

# sntutils 1.11.8 (2025-11-20)

## New features

* `consistency_map()` for spatial violation mapping.
* `require_all` parameter for complete-data reporting metrics.
* `check_snt_var()` gains fuzzy matching and dynamic DHS ITN
  age-group support.
* `show_outbreaks` and `return_plots` parameters in `outlier_plot()`.
* `x_axis_breaks` parameter in `reporting_rate_plot()`.

## Enhancements

* Consistency-plot layering and styling improvements.
* Project folder structure and path setup refactor.
* `reporting_rate_map()` shows CLI config message.

## Bug fixes

* Facility-count inconsistencies in `reporting_rate_map()`.
* Pairing-list input in data-check function.

# sntutils 1.11.7 (2025-11-14)

## New features

* `get_active_facilities()` helper.
* Multilingual plot captions with facility counts.
* Reporting-rate functions gain flexible facility analysis and
  CLI feedback.

## Bug fixes

* S3 object splicing error in `facility_reporting_plot()`.
* Each facet now has its own axis range.
* Default `hf_col` set; test failures fixed.

# sntutils 1.11.6 (2025-10-31)

## New features

* `get_dhs_data()` for DuckDB-backed DHS dataset access.
* Climate variables added to raster download logic.

## Enhancements

* Facility activity classification and plots refactor.
* Outlier-detection summary message simplified.
* CHIRPS file detection improved; `snt_var_tree` rebuilt.
* Default `spatial_level = "hf_uid"` in outlier functions.
* `consistency_check` axis expansion.

# sntutils 1.11.5 (2025-10-30)

## Enhancements

* `fuzzy_match_facilities()` step order refactored; docs updated.

# sntutils 1.11.4 (2025-10-29)

## Enhancements

* `fuzzy_match_facilities()` enforces one-to-one matching.
* `mfl_only_count` calculation refactored.

## Bug fixes

* NA facet handling in `facility_reporting_plot()`.

## Breaking changes

* All hashing functionality removed from `write_snt_data()`.

# sntutils 1.11.3 (2025-10-28)

## New features

* `facet_col` parameter validated and preserved in
  `facility_reporting_plot()`.
* Universal console-clearing helper added.
* Column-width parameter.

# sntutils 1.11.2 (2025-10-27)

## Enhancements

* Admin-level parameters refactored.
* Facility-activeness checks enhanced.
* Outlier-detection defaults updated and output streamlined.

# sntutils 1.11.1 (2025-10-27)

## New features

* Advanced outlier-detection parameters.

# sntutils 1.11.0 (2025-10-27)

## New features

* `trailing_tolerance` passed through any function using
  `classify_facility_activity()`.

# sntutils 1.10.1 (2025-10-27)

## New features

* Time-series plot added to `compare_methods_plot()`.

## Enhancements

* Facility activity classification logic refactored.

# sntutils 1.10.0 (2025-10-24)

* Version bump; no user-visible changes.

# sntutils 1.9.0 (2025-10-21)

## New features

* `compare_methods_plot()` for comparing outlier-detection methods.
* Facility activity classification options on reporting functions.
* Outbreak classification in outlier detection.
* SNT variable tree sourced from package dataset.
* Translation overrides for statistical labels
  (e.g. "mean" → "Moyenne" in French).

## Enhancements

* `detect_outliers()` now considers the same month in both past and
  future years.
* Method-description captions on facility reporting plots.

## Breaking changes

* Seasonality feature removed from the outlier function.

## Bug fixes

* Line-width warning.
* Deprecated `label.padding` parameter.

# sntutils 1.8.0 (2025-10-13)

## New features

* SNT variable dictionary added; `build_dict()` can use the package
  data dictionary.
* New function to display the data dictionary.
* `extrapolate_pop()` supports multiple population columns.
* Option to drop columns if not mapped.
* Facility plotting can output binary active/inactive results.
* `classify_facility_activity()` methods 2 and 3 added; configurable
  breaks parameter.

## Enhancements

* Reduced dependency footprint: `foreach` dropped; `ggrepel` replaces
  `shadowtext`.
* `write_snt_data()` `n_saved` switch for version retention (replaces
  archiving).
* `renv` setup added to the repo.

## Bug fixes

* `standardize_names()` no longer introduces auto-generated names
  from `vapply()`.
* FOSA naming.
* Reporting-function translation and factoring.

# sntutils 1.7.0 (2025-10-10)

## New features

* File-based caching for outlier detection.
* Parallelized testing.

## Enhancements

* Outlier-detection performance optimization.
* `outlier_plot()` works at the health-facility level.

## Bug fixes

* Outlier save-path.

# sntutils 1.6.4 (2025-08-30)

## New features

* `validate_process_coordinates()` and `validate_process_spatial()` —
  the full spatial-validation pipeline.
* `read_snt_data()` / `write_snt_data()` with standardized formats;
  `qs2` save/read format.
* `build_dictionary()`, `prep_geonames()`, `standardize_names()`.
* `setup_project_paths()` and `create_data_structure()`.
* `fuzzy_match_facilities()` with comprehensive matching stats.
* `classify_facility_activity()` and `facility_reporting_plot()`.
* `plot_admin_map_distinct()`.
* `dhis2_map()`.
* `download_era5()` for ERA5 climate data.
* Parquet and Feather support in `read()` / `write()`.
* GeoJSON support in `read()` / `write()`.
* `consistency_check()` gains a `facet_by` parameter.
* `outlier_plot()` gains save/translate options, `year_breaks`, and
  a meaningful return value.
* Reporting-rate weighting.
* Health-facility data validation (`prep_health_facilities`).

## Enhancements

* Translation system: multilingual plot captions, French acronyms,
  cached translations.
* Excel sheet formatting.
* Outlier detection with seasonality and guardrails (later removed
  in 1.9.0).
* UTF-8 handling for data and sheet names.

## Internals

* `qs2` adopted for save/read.
* `pngquant` detection improvements.
* Critical packages moved from Suggests to Imports.

# sntutils 1.6.3 (2025-08-22)

## New features

* New function to download WHO shapefiles.

## Documentation

* Various updates.

# sntutils 1.6.2 (2025-08-14)

## New features

* `prep_geonames()` enhancements: casing preservation, alternate
  unmatched-data export.
* `calculate_match_stats()` properly excludes NA before counting.

## Breaking changes

* `download_chirps2.0()` renamed to `download_chirps()`.
* `who_region` parameter and stratify functionality removed.

# sntutils 1.6.1 (2025-08-05)

## Enhancements

* Reporting rate uses proportion in plots, 0–1 elsewhere.

# sntutils 1.6.0 (2025-08-05)

* Version bump; paired with 1.6.1 on the same day.

# sntutils 1.5.1 (2025-07-25)

## New features

* Clinical-pathway streams used for imputation.

## Documentation

* README and reporting-rate page refactor.

# sntutils 1.5.0 (2025-07-14)

## New features

* `fallback_diff()` and `fallback_row_sum()`.

## Enhancements

* Raster-processing weighting updates and improved documentation.

## Breaking changes

* `na_to_zero` options removed.

# sntutils 1.4.0 (2025-03-03)

Initial public version of the package, established as `sntutils`
(renamed mid-development from `snt` / `snt-utils`).

## Core features

* Data I/O — `read()` and `write()` supporting CSV, XLSX, GeoJSON,
  shapefiles and more.
* Data quality — `consistency_check()`, `missing_plot()`,
  `auto_parse_types()`, `autoparse_dates()`, `standardize_names()`,
  outlier detection.
* Spatial — initial shapefile validation and cleaning.
* Reporting — initial reporting-rate function.
* Translation system with caching, plus vectorized `translate_text()`.
* PNG compression (`compress_png()`).
* `impute_higher_admin()` for admin-level imputation.

## Raster processing

* `process_raster_collection()` and `process_raster_with_boundaries()`.
* `normalize_raster_by_polygon()`.
* `extrapolate_pop()` for population extrapolation.

## Downloads

* `download_worldpop()` (and age-band variant).
* `download_chirps()` for CHIRPS precipitation data plus available
  dates check.
* DHS indicator download functions.

## Project utilities

* `create_data_structure()` for the SNT folder hierarchy.
* `clean_filenames()` for date-laden file paths.

## Internals

* `vdigest()` — vectorized digest.
* GitHub Actions for R CMD check and test coverage.
