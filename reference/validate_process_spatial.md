# Spatial Vector Validation and Cleaning

Validates spatial vector data and creates admin-level aggregations with
geometry hashes. Follows the same validation approach as
validate_process_coordinates() with modular checks and standardized
outputs.

## Usage

``` r
validate_process_spatial(
  shp,
  name = "shapefile",
  adm0_col = NULL,
  adm1_col = NULL,
  adm2_col = NULL,
  adm3_col = NULL,
  adm4_col = NULL,
  fix_issues = TRUE,
  quiet = FALSE,
  geometry_crs = 4326,
  drop_z = TRUE
)
```

## Arguments

- shp:

  An sf object to validate

- name:

  Character string identifying the dataset (for reporting)

- adm0_col:

  Character string, column name for country level

- adm1_col:

  Character string, column name for region/province level

- adm2_col:

  Character string, column name for district level

- adm3_col:

  Character string, column name for chiefdom/commune level

- adm4_col:

  Character string, column name for sub-chiefdom/village level

- fix_issues:

  Logical, whether to attempt automatic fixes

- quiet:

  Logical, whether to suppress progress messages

- geometry_crs:

  Target CRS for output geometry (default EPSG:4326). After CRS
  validation/fixes, geometries are transformed to this CRS for
  subsequent processing and output.

- drop_z:

  Logical, whether to drop Z/M coordinates before validation. Defaults
  to TRUE to avoid s2 warnings for 2D processing.

## Value

A list with validation results. Key elements:

- `issues`: Character vector of issues detected.

- `invalid_rows`: sf/data.frame of rows failing basic geometry checks.

- `checks$duplicate_rows`: Duplicates by admin names + geometry.

- `checks$all_shared_locations`: All records sharing geometry.

- `checks$all_shared_locations_ungrouped`: Only the duplicate rows at
  shared geometry.

- `checks$invalid_geometries`: Rows with invalid geometries before
  fixing.

- `checks$empty_geometries`: Rows with empty geometries.

- `checks$self_intersecting`: Rows with self-intersecting geometries.

- `column_dictionary`: Data frame with column_name and description.

- `final_spat_vec`: List of admin-level aggregations (adm0, adm1, adm2,
  adm3, adm4).

- `geometry_types`: Tibble summarizing geometry types and counts.

- `spatial_extent`: Named vector with xmin, ymin, xmax, ymax.

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic validation with Sierra Leone data (adm3)
result <- validate_process_spatial(
  shp = shp_raw,
  name = "Sierra Leone boundaries",
  adm0_col = "country",
  adm1_col = "region",
  adm2_col = "district",
  adm3_col = "chiefdom"
)

# Validation with adm4
result <- validate_process_spatial(
  shp = shp_raw,
  name = "Sierra Leone boundaries",
  adm0_col = "country",
  adm1_col = "region",
  adm2_col = "district",
  adm3_col = "chiefdom",
  adm4_col = "village"
)

# Access cleaned admin levels
shp_adm0 <- result$final_spat_vec$adm0
shp_adm1 <- result$final_spat_vec$adm1
shp_adm2 <- result$final_spat_vec$adm2
shp_adm3 <- result$final_spat_vec$adm3
shp_adm4 <- result$final_spat_vec$adm4

# Check validation status
if (length(result$issues) == 0) {
  cli::cli_alert_success("Ready for SNT analysis!")
}

# Access invalid rows (if any)
invalid_rows <- result$invalid_rows

# Access duplicate rows (if any)
duplicate_rows <- result$checks$duplicate_rows
shared_locations <- result$checks$all_shared_locations

# Check column structure as data frame
col_dict <- result$column_dictionary
# Shows data frame with shapefile, old_name, new_name, description
print(col_dict)
} # }
```
