# Row-safe sum for grouped aggregation

Returns `NA_real_` if all values are `NA`; otherwise returns the sum
with missing values treated as zero.

## Usage

``` r
safe_sum(x)
```

## Arguments

- x:

  A numeric vector.

## Value

A single numeric value: `NA_real_` if all values in `x` are missing,
otherwise the numeric sum with `NA`s ignored.

## Details

This is useful in
[`dplyr::summarise()`](https://dplyr.tidyverse.org/reference/summarise.html)
calls where you want a sum that respects full-missing groups by yielding
`NA_real_`, and otherwise sums while ignoring `NA`s.

## Examples

``` r
# All missing -> NA
sntutils:::`safe_sum`(c(NA, NA))
#> [1] NA

# Mixed missing -> sum of non-missing
sntutils:::`safe_sum`(c(1, NA, 2))
#> [1] 3

# No missing -> normal sum
sntutils:::`safe_sum`(c(1, 2, 3))
#> [1] 6
```
