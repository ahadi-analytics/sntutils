# Get started

## What is `sntutils`?

`sntutils` is an R package developed by AHADI to support the
**Subnational Tailoring (SNT)** of malaria interventions. It bundles the
small, repeated operations you find yourself doing in every country
support analysis: reading messy DHIS2 exports, harmonising admin names
across vintages of shapefiles, validating facility coordinates,
calculating reporting rates, extracting climate and population rasters
to admin units, and producing plots and maps in the language the country
team works in.

The package is built around four ideas:

- **One way in, one way out.**
  [`read()`](https://ahadi-analytics.github.io/sntutils/reference/read.md)
  and
  [`write()`](https://ahadi-analytics.github.io/sntutils/reference/write.md)
  handle every common format.
  [`read_snt_data()`](https://ahadi-analytics.github.io/sntutils/reference/read_snt_data.md)
  and
  [`write_snt_data()`](https://ahadi-analytics.github.io/sntutils/reference/write_snt_data.md)
  add hashing and metadata sidecars when you need a reproducible audit
  trail.
- **Tidy outputs.** Every function returns a tibble or an `sf` data
  frame with stable, documented column names - never a list of lists.
- **Country-aware defaults.** Functions accept `target_language` so
  plots, labels and month names match the team’s working language.
- **Small surface area, big leverage.** The R/ folder has ~50 source
  files; this site groups them by what they do, not what they are.

**New to Subnational Tailoring?** The [AHADI SNT Code
Library](https://ahadi-analytics.github.io/snt-code-library/) is the
methodology-level companion to this package - country examples,
analytical reasoning, and the workflow behind every step `sntutils`
automates. Good starting points:
[About](https://ahadi-analytics.github.io/snt-code-library/english/library/front_matter/about.html),
[For
Analysts](https://ahadi-analytics.github.io/snt-code-library/english/library/front_matter/for_analysts.html),
[Producing high-quality
outputs](https://ahadi-analytics.github.io/snt-code-library/english/library/front_matter/producing_outputs.html).

## Install

``` r

# 1) install pak if needed
install.packages("pak")

# 2) install sntutils from GitHub
pak::pkg_install("ahadi-analytics/sntutils")
```

System dependencies: `sntutils` uses `sf` and `terra`, which require
GDAL, GEOS and PROJ. On macOS install them with
`brew install gdal proj geos`; on Ubuntu use the GDAL PPA. RStudio and
recent Posit Workbench builds already ship with these.

## The shape of an SNT pipeline

Most SNT projects move through the same stages. `sntutils` provides a
function (or a small family of functions) at each step:

| Stage | Typical task | Key sntutils functions |
|----|----|----|
| 1\. Project setup | Create the AHADI folder skeleton, get standard paths | [`initialize_project_structure()`](https://ahadi-analytics.github.io/sntutils/reference/initialize_project_structure.md), [`setup_project_paths()`](https://ahadi-analytics.github.io/sntutils/reference/setup_project_paths.md) |
| 2\. Ingest | Read CSV / Excel / Stata / RDS / shapefile inputs | [`read()`](https://ahadi-analytics.github.io/sntutils/reference/read.md), [`read_snt_data()`](https://ahadi-analytics.github.io/sntutils/reference/read_snt_data.md) |
| 3\. Clean | Parse dates, infer types, standardise admin names | [`autoparse_dates()`](https://ahadi-analytics.github.io/sntutils/reference/autoparse_dates.md), [`auto_parse_types()`](https://ahadi-analytics.github.io/sntutils/reference/auto_parse_types.md), [`standardize_names()`](https://ahadi-analytics.github.io/sntutils/reference/standardize_names.md), [`prep_geonames()`](https://ahadi-analytics.github.io/sntutils/reference/prep_geonames.md) |
| 4\. Validate | Check facility coordinates and admin geometries | [`validate_process_coordinates()`](https://ahadi-analytics.github.io/sntutils/reference/validate_process_coordinates.md), [`validate_process_spatial()`](https://ahadi-analytics.github.io/sntutils/reference/validate_process_spatial.md) |
| 5\. Analyse | Reporting rates, consistency, outliers | [`calculate_reporting_metrics()`](https://ahadi-analytics.github.io/sntutils/reference/calculate_reporting_metrics.md), [`consistency_check()`](https://ahadi-analytics.github.io/sntutils/reference/consistency_check.md), [`detect_outliers()`](https://ahadi-analytics.github.io/sntutils/reference/detect_outliers.md) |
| 6\. Enrich | Pull climate / population / DHS, extract to admin units | [`download_chirps()`](https://ahadi-analytics.github.io/sntutils/reference/download_chirps.md), [`download_era5()`](https://ahadi-analytics.github.io/sntutils/reference/download_era5.md), [`download_worldpop()`](https://ahadi-analytics.github.io/sntutils/reference/download_worldpop.md), [`process_raster_collection()`](https://ahadi-analytics.github.io/sntutils/reference/process_raster_collection.md) |
| 7\. Communicate | Maps, plots, translated labels, compressed PNGs | [`reporting_rate_plot()`](https://ahadi-analytics.github.io/sntutils/reference/reporting_rate_plot.md), [`dhis2_map()`](https://ahadi-analytics.github.io/sntutils/reference/dhis2_map.md), [`translate_text()`](https://ahadi-analytics.github.io/sntutils/reference/translate_text.md), [`compress_png()`](https://ahadi-analytics.github.io/sntutils/reference/compress_png.md) |

The articles in the **Workflows** menu walk through each stage in
detail. This page just shows the whole pipeline once, end-to-end.

## A tiny end-to-end example

Below we read a Sierra Leone DHIS2 sample, parse its dates, standardise
column names, calculate reporting rates by district-month, and draw a
plot. The dataset ships with the package.

``` r

library(sntutils)

# 1. read - sntutils::read() picks the importer from the file extension
sl_dhis2 <- read(
  system.file("extdata", "sl_exmaple_dhis2.rds", package = "sntutils")
)

# 2. clean - make column names lowercase_with_underscores and parse dates
sl_dhis2 <- sl_dhis2 |>
  standardize_names() |>
  autoparse_dates(date_cols = "date") |>
  dplyr::rename(year_mon = date) |>
  dplyr::mutate(
    hf_uid    = vdigest(paste0(adm1, adm2, hf), algo = "xxhash32"),
    record_id = vdigest(paste(hf_uid, year_mon),  algo = "xxhash32")
  )

# 3. analyse - reporting rate by district-month
rates <- calculate_reporting_metrics(
  data             = sl_dhis2,
  vars_of_interest = c("conf", "pres"),
  x_var            = "year_mon",
  y_var            = "adm2",
  hf_col           = "hf_uid",
  key_indicators   = c("allout", "test", "treat", "conf", "pres")
)

tail(rates)
#> # A tibble: 6 × 6
#>   year_mon adm2                                  rep   exp reprate missrate
#>   <chr>    <chr>                               <int> <int>   <dbl>    <dbl>
#> 1 2023-12  Moyamba District Council              106   108   0.981   0.0185
#> 2 2023-12  Port Loko City Council                  2     2   1       0
#> 3 2023-12  Port Loko District Council             99   103   0.961   0.0388
#> 4 2023-12  Pujehun District Council               96   104   0.923   0.0769
#> 5 2023-12  Tonkolili District Council            109   115   0.948   0.0522
#> 6 2023-12  Western Area Rural District Council    62    64   0.969   0.0312

# 4. visualise - facility-level reporting plot, in English
reporting_rate_plot(
  data             = sl_dhis2,
  vars_of_interest = "conf",
  x_var            = "year_mon",
  y_var            = "adm2",
  hf_col           = "hf_uid",
  key_indicators   = c("allout", "test", "treat", "conf", "pres")
)
```

That’s a complete SNT mini-pipeline: read → clean → metric → plot, with
no external state and no manual format wrangling. The rest of the
articles drill into each step.

## Where to next

- **Data I/O and cleaning** - every
  [`read()`](https://ahadi-analytics.github.io/sntutils/reference/read.md)/[`write()`](https://ahadi-analytics.github.io/sntutils/reference/write.md)
  shortcut plus name and date harmonisation.
- **Spatial validation and mapping** - admin geometries, coordinates,
  shapefile crosswalks, fuzzy facility matching.
- **Reporting rates and data quality** - the three reporting-rate
  scenarios, consistency checks and outlier methods.
- **Climate and population downloads** - CHIRPS, ERA5, MODIS, NASA
  POWER, WorldPop and DHS, and how to extract them to admin units.
- **Project setup and utilities** - folder scaffolding, paths, caching,
  translation and the small numeric helpers.

If something is missing or surprising, please open an issue on
[GitHub](https://github.com/ahadi-analytics/sntutils/issues) - the
package evolves with the SNT support work we do.
