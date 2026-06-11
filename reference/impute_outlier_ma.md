# Impute outliers using moving average from adjacent time points

Replaces outlier values (those exceeding a threshold) with the moving
average of adjacent non-outlier values. Useful for smoothing time series
data where outliers should be replaced with locally consistent values
rather than removed.

## Usage

``` r
impute_outlier_ma(x, threshold)
```

## Arguments

- x:

  Numeric vector of values. Should be sorted by time within groups
  before calling this function.

- threshold:

  Numeric scalar. Values strictly greater than this threshold are
  considered outliers and will be imputed.

## Value

Numeric vector of the same length as `x`, with outliers replaced by the
rounded moving average of valid adjacent values. If no valid adjacent
values exist, the original value is retained.

## Details

The function works as follows:

1.  For each value, it checks if the previous and next values are
    non-outliers (i.e., \<= threshold)

2.  If a value exceeds the threshold, it calculates the mean of valid
    adjacent values (previous and/or next)

3.  The outlier is replaced with the rounded moving average

4.  If no valid adjacent values exist (both neighbors are also outliers
    or NA), the original value is kept

This approach preserves local trends while removing spike anomalies.
It's particularly useful for time series where outliers represent data
errors rather than true signal.

## Examples

``` r
# simple example with one outlier
x <- c(10, 12, 100, 14, 11)
impute_outlier_ma(x, threshold = 50)
#> [1] 10 12 13 14 11
# Returns: c(10, 12, 13, 14, 11) - the 100 is replaced with mean(12, 14) = 13

# consecutive outliers - only outer edges get imputed
x <- c(10, 100, 200, 15)
impute_outlier_ma(x, threshold = 50)
#> [1] 10 10 15 15
# Returns: c(10, 10, 15, 15) - each outlier uses its one valid neighbor

# use with dplyr for grouped time series
if (FALSE) { # \dontrun{
data |>
  dplyr::arrange(adm2, date) |>
  dplyr::group_by(adm2) |>
  dplyr::mutate(
    cases_imputed = impute_outlier_ma(cases, threshold = upper_bound)
  ) |>
  dplyr::ungroup()
} # }
```
