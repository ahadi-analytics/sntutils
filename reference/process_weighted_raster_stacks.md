# Process Weighted Raster Stacks

This function processes raster stacks by calculating weighted statistics
based on population data for each shape in the input shapefile across
multiple years.

## Usage

``` r
process_weighted_raster_stacks(
  value_raster,
  pop_raster,
  shape,
  value_var = "indicator_weighted",
  start_year = 2000,
  stat_type = "mean"
)
```

## Arguments

- value_raster:

  A SpatRaster object containing the values to be weighted

- pop_raster:

  A SpatRaster object containing population weights

- shape:

  A sf/shapefile object containing geometries and shape_id column

- value_var:

  Character string for naming the weighted value column

- start_year:

  Numeric indicating the starting year for the time series

- stat_type:

  Character string specifying statistic type:

  - "mean": Population-weighted mean using exactextractr's weighted_mean
    function

  - "median": Population-weighted median using
    matrixStats::weightedMedian() approach that finds the value where
    50% of the population weight lies below and 50% above, interpolating
    between pixel values when needed

  - "both": Returns both weighted mean and weighted median statistics
    (default: "mean")

## Value

A tibble with columns for shape_id, year, and the weighted values
