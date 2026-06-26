# Setup project paths and environment for SNT pipeline

Creates a list of standardized paths for the SNT pipelines Detects a
project root automatically and constructs all data/cache/output paths
with short, memorable names. Validates that folders exist and alerts if
they don't. Use initialize_project_structure() to create the folders.

## Usage

``` r
setup_project_paths(base_path = NULL, quiet = FALSE)
```

## Arguments

- base_path:

  Character path to project root. If NULL, try to detect a root
  (here::here(), then rprojroot); fallback to getwd().

- quiet:

  Logical; suppress warning messages. Default FALSE.

## Value

Named list of absolute paths: core, admin_shp, physical_feat, hf, chw,
pop_national, pop_worldpop, pop_displaced, cache, dhis2, pfpr_est,
mortality_est, interventions, drug_eff, climate, accessibility,
land_use, dhs, ento, commodities, finance, final_data, val, interm,
final, model, val_fig, val_tbl, interm_fig, interm_tbl, final_fig,
final_tbl, model_fig, model_tbl.

## Examples

``` r
tmp <- tempdir()
paths <- setup_project_paths(base_path = tmp, quiet = TRUE)
paths$admin_shp
#> /tmp/RtmpRm0pCI/01_data/1.1_foundational/1.1a_admin_boundaries
paths$val_fig
#> /tmp/RtmpRm0pCI/03_outputs/3.1_validation/figures
paths$interm_tbl
#> /tmp/RtmpRm0pCI/03_outputs/3.2_intermediate_products/tables
```
