# Create an area-weighted crosswalk between old and new admin polygons

Purpose This function constructs a reproducible lookup table linking old
and new administrative boundary systems using polygon overlap. It is
designed to handle boundary splits and merges explicitly and to support
backcasting or forward aggregation of indicators across administrative
changes.

## Usage

``` r
crosswalk_shapefiles_sf(
  old_sf,
  new_sf,
  level = c("adm0", "adm1", "adm2"),
  old_suffix = "_old",
  min_weight = 0.001,
  area_crs = 32735,
  keep_overlap_geometry = FALSE,
  min_primary_weight = 0.5,
  min_secondary_weight = 0.1,
  coverage_tol = 0.05,
  verbose = TRUE,
  include_areas = FALSE,
  include_reverse_prop = FALSE
)
```

## Arguments

- old_sf:

  sf object. Old administrative boundaries. Must contain columns adm0,
  adm1, adm2 and be in EPSG:4326.

- new_sf:

  sf object. New administrative boundaries. Must contain columns adm0,
  adm1, adm2 and be in EPSG:4326.

- level:

  character. One of "adm0", "adm1", "adm2". Determines which
  administrative level is crosswalked.

- old_suffix:

  character. Suffix appended to old admin names.

- min_weight:

  numeric. Minimum old-to-new area proportion to retain.

- area_crs:

  integer. EPSG code for a planar equal-area CRS.

- keep_overlap_geometry:

  logical. If TRUE, include overlap geometries.

- min_primary_weight:

  numeric. Minimum weight to label as primary.

- min_secondary_weight:

  numeric. Minimum weight for secondary overlaps.

- coverage_tol:

  numeric. Tolerance for area coverage diagnostics.

- verbose:

  logical. If TRUE, print CLI progress messages.

- include_areas:

  logical. If TRUE, include area columns in km2.

- include_reverse_prop:

  logical. If TRUE, include new-to-old proportion.

  Outputs Returns a list with:

  - data: data.frame with crosswalk mappings and diagnostics

  - dictionary: data.frame describing each column

  - metadata: data.frame with run parameters and timestamps

  - overlap_geometry: (if keep_overlap_geometry = TRUE) sf object

  Key columns in data output

  - overlap_prop: proportion of old unit area overlapping new unit

  - is_primary: TRUE if dominant mapping for old unit (overlap_prop \>=
    0.5)

  - n_new_adm: number of new units overlapping the old unit

  - split_flag: TRUE if old unit overlaps multiple new units

  - n_old_per_new: number of old units overlapping the new unit

  - merge_flag: TRUE if multiple old units merged into new unit

  Assumptions

  - Areas are computed in a planar CRS.

  - Attributes (admin names) are spatially constant within polygons.

  - Weights are area-based, not population-weighted.

## Details

Weights are area-based and assume uniform distribution within polygons.
All geometric operations are performed in a planar equal-area CRS.

Inputs

## Examples

``` r
if (FALSE) { # \dontrun{
cw_adm2 <- crosswalk_shapefiles_sf(
  old_sf = adm2_old,
  new_sf = adm2_new,
  level = "adm2",
  min_weight = 0.001,
  area_crs = 32735
)
} # }
```
