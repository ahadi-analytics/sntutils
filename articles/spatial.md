# Spatial validation and mapping

Spatial work is where SNT analyses fail most quietly. Geometry might be
invalid, CRS might disagree, district names might have been renamed
between the shapefile and the DHIS2 export, or facility coordinates
might land in the ocean. `sntutils` provides a stack of focused
functions to find and fix each of these issues, then a small mapping
layer to plot the result.

## Downloading boundaries: `download_shapefile()`

For most SNT work the WHO `geohub` boundaries are the canonical input.
[`download_shapefile()`](https://ahadi-analytics.github.io/sntutils/reference/download_shapefile.md)
pulls them by ISO3 code and admin level and returns an `sf` object ready
to use.

``` r

library(sntutils)

# pull adm2 boundaries for Sierra Leone and Togo
boundaries <- download_shapefile(
  country_codes = c("SLE", "TGO"),
  admin_level   = "adm2",
  latest        = TRUE,
  dest_path     = "01_data/1.1_foundational/1.1a_admin_boundaries"
)

class(boundaries)
#> [1] "sf"         "tbl_df"     "tbl"        "data.frame"
```

When `dest_path` is `NULL` the file is downloaded to a session-scoped
cache and not persisted across runs.

## Validating admin geometries: `validate_process_spatial()`

Once a shapefile is in hand,
[`validate_process_spatial()`](https://ahadi-analytics.github.io/sntutils/reference/validate_process_spatial.md)
runs a battery of checks — invalid geometry, mixed Z/M dimensions, wrong
CRS, missing admin codes, duplicated admin names — and (with
`fix_issues = TRUE`) attempts safe automatic repairs. It always returns
the same shape: a list with a cleaned `sf` object plus a tibble of
issues found.

``` r

boundaries_clean <- validate_process_spatial(
  shp          = boundaries,
  name         = "WHO geohub SLE adm2",
  adm0_col     = "adm0_name",
  adm1_col     = "adm1_name",
  adm2_col     = "adm2_name",
  fix_issues   = TRUE,
  geometry_crs = 4326,
  drop_z       = TRUE
)

names(boundaries_clean)
#> [1] "shp"     "issues"  "summary"
```

The `issues` tibble is the audit trail — keep it next to the shapefile
on disk so reviewers can see what was changed.

## Validating point coordinates: `validate_process_coordinates()`

For facility data,
[`validate_process_coordinates()`](https://ahadi-analytics.github.io/sntutils/reference/validate_process_coordinates.md)
checks that lat/lon columns parse, sit within plausible bounds, have
enough decimal precision to be real readings (default
`min_decimals = 3`), and fall inside the country polygon if you pass
`adm0_sf`.

``` r

hf_clean <- validate_process_coordinates(
  data         = hf_raw,
  name         = "SLE master facility list",
  lon_col      = "longitude",
  lat_col      = "latitude",
  adm0_sf      = boundaries_clean$shp |>
    dplyr::filter(adm0_name == "Sierra Leone"),
  geometry_crs = 4326,
  min_decimals = 3,
  id_col       = "facility_uid",
  fix_issues   = TRUE
)

hf_clean$summary
#> ● 4,213 input rows
#> ● 4,189 valid coordinates retained
#> ●     8 rows dropped (low precision)
#> ●    16 rows dropped (outside adm0)
```

Returns an `sf` of valid POINT geometry plus an `issues` tibble of
dropped or flagged rows.

## Crosswalking shapefile vintages: `crosswalk_shapefiles_sf()`

When a country redistricts (Sierra Leone 2017, Togo 2019, several recent
DRC changes), historical surveillance data is keyed to the old
boundaries and new analyses to the new.
[`crosswalk_shapefiles_sf()`](https://ahadi-analytics.github.io/sntutils/reference/crosswalk_shapefiles_sf.md)
computes the area-weighted overlap between two `sf` layers so we can
reproject indicators forward or backward.

``` r

xwalk <- crosswalk_shapefiles_sf(
  old_sf = sle_adm2_2014,
  new_sf = sle_adm2_2022,
  level  = "adm2",
  old_suffix = "_old",
  min_weight = 0.01,        # drop slivers <1%
  area_crs   = 32629,       # Sierra Leone equal-area
  verbose    = TRUE
)

xwalk |> dplyr::select(adm2_old, adm2_name, weight, primary) |> head()
#> # A tibble: 6 × 4
#>   adm2_old        adm2_name       weight primary
#>   <chr>           <chr>            <dbl> <lgl>
#> 1 Bo District     Bo District      0.998 TRUE
#> 2 Bo District     Bo City          0.002 FALSE
#> 3 Bonthe          Bonthe District  1.000 TRUE
#> 4 …
```

Multiply by `weight` and
`dplyr::group_by(adm2_name) |> summarise(sum())` to push aggregate
counts from old units to new.

## Fuzzy-matching facilities: `fuzzy_match_facilities()`

DHIS2 facility names rarely line up perfectly with the master facility
list.
[`fuzzy_match_facilities()`](https://ahadi-analytics.github.io/sntutils/reference/fuzzy_match_facilities.md)
runs a staged matching pipeline:

1.  exact match,
2.  match after
    [`standardize_names()`](https://ahadi-analytics.github.io/sntutils/reference/standardize_names.md)
    normalisation,
3.  string-distance match using one or more methods (`jw`, `lv`, `osa`,
    …),
4.  optional interactive picker for unresolved rows,

and returns a tibble of best matches plus diagnostics by stage.

``` r

matches <- fuzzy_match_facilities(
  target_df       = dhis2_facilities,        # what we're cleaning
  lookup_df       = mfl_facilities,          # the reference list
  admin_cols      = c("adm1", "adm2", "adm3"),
  hf_col_name     = "hf",
  uid_col         = "hf_uid",
  fuzzy_methods   = c("jw", "osa"),
  fuzzy_threshold = 95,
  match_interactivity = TRUE,
  save_path = "01_data/1.1_foundational/1.1b_health_facilities/processed"
)

matches$results |>
  dplyr::count(match_status)
#> # A tibble: 3 × 2
#>   match_status     n
#>   <chr>        <int>
#> 1 high          4012
#> 2 medium         147
#> 3 low             54
```

[`calculate_match_stats()`](https://ahadi-analytics.github.io/sntutils/reference/calculate_match_stats.md)
summarises the same results by method so we can compare matching
strategies side by side.

## Joining DHIS2 columns: `dhis2_map()`

[`dhis2_map()`](https://ahadi-analytics.github.io/sntutils/reference/dhis2_map.md)
renames a DHIS2 export’s columns using a name-mapping dictionary, so we
can keep the upstream names intact on disk and only remap when we load
the data.

``` r

mapped <- dhis2_map(
  data    = sl_dhis2_raw,
  dict    = dhis2_label_lookup,
  new_col = "snt_name",
  old_col = "dhis2_label",
  drop_unmatched = FALSE
)
```

## Drawing maps

### `plot_admin_map_distinct()` — categorical fills

For administrative reference maps and any other categorical layer:

``` r

plot_admin_map_distinct(
  sf_data = boundaries_clean$shp,
  fill_col = "adm1_name",
  title   = "Sierra Leone — districts coloured by region",
  palette = "ahadi_main"
)
```

Available palettes are listed via
[`list_palettes()`](https://ahadi-analytics.github.io/sntutils/reference/list_palettes.md);
pull a specific palette with
[`get_palette()`](https://ahadi-analytics.github.io/sntutils/reference/get_palette.md).

### `facetted_map_bins()` and `facetted_map_gradient()`

When the map needs to faceted by year, indicator, scenario or
intervention, these two helpers produce consistent small-multiples with
the SNT plotting defaults baked in:

``` r

# bins — discrete legend, good for incidence categories
facetted_map_bins(
  data    = incidence_long,
  sf_data = boundaries_clean$shp,
  facet_col = "year",
  fill_col  = "incidence_per_1000",
  bins      = c(0, 10, 50, 100, 250, 500, Inf),
  title     = "Annual malaria incidence per 1,000 — Sierra Leone"
)

# gradient — continuous legend, good for reporting completeness
facetted_map_gradient(
  data    = reporting_long,
  sf_data = boundaries_clean$shp,
  facet_col = "year_mon",
  fill_col  = "reprate",
  limits  = c(0, 1),
  title   = "Monthly reporting completeness"
)
```

Both functions accept `target_language` so legend titles and labels can
be translated automatically (see the [Project setup and
utilities](https://ahadi-analytics.github.io/sntutils/articles/project-and-utilities.md)
article).

## A spatial pipeline, end to end

``` r

# 1. pull boundaries
sle_adm2 <- download_shapefile(
  country_codes = "SLE",
  admin_level   = "adm2",
  latest        = TRUE
)

# 2. validate them
sle_adm2_clean <- validate_process_spatial(
  shp = sle_adm2, name = "SLE adm2",
  adm0_col = "adm0_name", adm1_col = "adm1_name", adm2_col = "adm2_name",
  fix_issues = TRUE
)$shp

# 3. validate facility coordinates against the polygon
hf_geo <- validate_process_coordinates(
  data    = hf_raw,
  lon_col = "longitude", lat_col = "latitude",
  adm0_sf = sle_adm2_clean,
  id_col  = "facility_uid",
  fix_issues = TRUE
)$shp

# 4. fuzzy-match DHIS2 facility names to the master list
match_results <- fuzzy_match_facilities(
  target_df  = sl_dhis2 |> dplyr::distinct(adm1, adm2, adm3, hf, hf_uid),
  lookup_df  = hf_geo,
  admin_cols = c("adm1", "adm2", "adm3"),
  hf_col_name = "hf",
  uid_col    = "hf_uid"
)$results

# 5. plot
plot_admin_map_distinct(
  sf_data  = sle_adm2_clean,
  fill_col = "adm1_name",
  title    = "Sierra Leone — adm2 by region"
)
```

By the end of this pipeline we have a single, validated `sf` boundary
file, a validated `sf` facility-points file, a high-confidence link
between DHIS2 facility names and the MFL, and a baseline map. Everything
that follows — reporting rates, climate extraction, population weighting
— assumes this foundation.
