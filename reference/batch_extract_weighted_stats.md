# Batch Extraction of Weighted Mean from Raster Using Population Weights

Computes population-weighted means of a value raster (e.g., PfPR2-10)
using a corresponding population raster as weights, across
administrative boundaries.

## Usage

``` r
batch_extract_weighted_stats(
  value_raster_file,
  pop_raster_file,
  shapefile,
  id_cols = c("adm0", "adm1", "adm2"),
  value_layer_to_process = 1,
  weight_na_as_zero = TRUE,
  stat_type = "mean",
  pattern_info = NULL,
  time_components = NULL
)
```

## Arguments

- value_raster_file:

  Path to the raster file containing the values to weight (e.g.,
  PfPR2-10)

- pop_raster_file:

  Path to the population raster file used for weighting

- shapefile:

  sf or SpatVector object of administrative boundaries

- id_cols:

  Character vector of administrative columns to keep

- value_layer_to_process:

  Integer or character. Specifies which layer in a multi-layer raster to
  extract. If the raster contains multiple layers (e.g., different years
  or indicators), this argument selects the layer to be processed.
  Default is 1.

- weight_na_as_zero:

  Logical; if TRUE, treats NA weights as zero (default: TRUE)

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

- pattern_info:

  Optional. Pre-parsed time pattern from
  [`detect_time_pattern()`](https://ahadi-analytics.github.io/sntutils/reference/detect_time_pattern.md).
  If NULL, detected from filename.

- time_components:

  Optional. Pre-parsed time components from
  [`extract_time_components()`](https://ahadi-analytics.github.io/sntutils/reference/extract_time_components.md).
  If NULL, extracted from filename.

## Value

Data frame with id_cols and population-weighted mean values
