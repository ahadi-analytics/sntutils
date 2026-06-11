# Normalize Raster Values by Polygon Regions

This function normalizes raster values within each polygon region by
dividing the values by the total sum for that region. This creates
proportional values that sum to 1 within each polygon.

## Usage

``` r
normalize_raster_by_polygon(raster, shp, id_col)
```

## Arguments

- raster:

  A SpatRaster object containing the values to be normalized

- shp:

  A spatial vector (SpatVector) containing the polygon regions

- id_col:

  Character string specifying the column name in shp that contains the
  region identifiers

## Value

A SpatRaster object with normalized values where the sum within each
polygon region equals 1

## Details

The function processes each polygon region separately by:

1.  Masking the raster to the region boundaries

2.  Calculating the total sum for the region

3.  Dividing all values in that region by the total The results are then
    mosaicked back together into a single raster.

## Examples

``` r
if (FALSE) { # \dontrun{
normalized <- normalize_raster_by_polygon(
  raster = population_raster,
  shp = admin_boundaries,
  id_col = "region_id"
)
} # }
```
