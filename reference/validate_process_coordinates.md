# Coordinate Validation and Cleaning

Validates coordinate data (points) and returns a cleaned sf object with
standardized columns. Accepts either an sf object with POINT geometry or
a data.frame with longitude and latitude columns. Checks include missing
coordinates, DMS detection with optional conversion (via the suggested
'parzer' package), range, precision, flip detection, and country
containment.

## Usage

``` r
validate_process_coordinates(
  data,
  name = "points",
  lon_col = NULL,
  lat_col = NULL,
  adm0_sf = NULL,
  geometry_crs = 4326,
  min_decimals = 4,
  id_col = NULL,
  fix_issues = TRUE,
  quiet = FALSE
)
```

## Arguments

- data:

  An `sf` object with POINT geometry or a `data.frame` containing
  coordinate columns.

- name:

  Character string identifying the dataset (for reporting).

- lon_col:

  Character string, column name for longitude (if `data` is not sf).

- lat_col:

  Character string, column name for latitude (if `data` is not sf).

- adm0_sf:

  Optional `sf` polygon/multipolygon of the country to validate points
  against (EPSG:4326 preferred). When provided, the function tests that
  all points fall within the given shape. No admin attributes are joined
  to the points.

- geometry_crs:

  Target CRS for output geometry and for operations with `adm0_sf`
  (default EPSG:4326). Lat/Lon attributes remain in EPSG:4326.

- min_decimals:

  Minimum decimal places required for lon/lat to be considered precise
  (default 4).

- id_col:

  Optional character name of a unique identifier column (e.g., facility
  ID). When provided, duplicate reporting includes IDs for
  coordinate-duplicate rows to aid de-dup decisions.

- fix_issues:

  Logical, whether to attempt automatic fixes (drop invalids, set CRS,
  standardize columns, remove duplicates, attempt flip fixes when
  `adm0_sf` is available).

- quiet:

  Logical, whether to suppress progress messages.

## Value

A list with validation results. Key elements:

- `issues`: Character vector of issues detected.

- `final_points_df`: Cleaned sf object of points.

- `invalid_rows`: sf/data.frame of rows failing basic coordinate checks.

- `checks$duplicate_rows`: Duplicates by ID + coordinates (standardized
  sf).

- `checks$all_shared_locations`: All records sharing a location.

- `checks$all_shared_locations_ungrouped`: Only the duplicate rows at
  shared locations.

- `checks$coordinates_adm0`: Points outside ADM0 with original lon/lat
  restored from geometry.

- `column_dictionary`: Data frame mapping old to standardized column
  names.

## Examples

``` r
if (FALSE) { # \dontrun{
# Example data with common issues: missing, DMS, flipped, imprecise
df <- data.frame(
  hf = paste0("HF_", 1:6),
  lon = c(-13.26077, "13\u00B015'38\"W", 8.5, -12.09057, NA, 181),
  lat = c(8.464283, "8\u00B027'51\"N", -13.26, 8.770261, 8.1, 0.1)
)

# Simple rectangle as country polygon (e.g., Sierra Leone-ish bbox)
adm0_sf <- sf::st_as_sfc(
  sf::st_bbox(c(xmin = -14, ymin = 6.9, xmax = -10, ymax = 10.1),
              crs = sf::st_crs(4326))
) |>
  sf::st_sf()

# Validate and auto-fix issues (DMS parsing requires 'parzer')
res <- validate_process_coordinates(
  data = df,
  name = "hf_points",
  lon_col = "lon",
  lat_col = "lat",
  adm0_sf = adm0_sf,
  min_decimals = 4,
  fix_issues = TRUE,
  quiet = TRUE
)

# Cleaned points
res$final_points_df
} # }
```
