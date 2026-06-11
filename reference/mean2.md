# Calculate mean with automatic NA handling

A wrapper around base::mean() that automatically removes NA values

## Usage

``` r
mean2(x)
```

## Arguments

- x:

  A numeric vector

## Value

The mean of all non-NA values in x

## Examples

``` r
mean2(c(1, 2, NA, 4)) # Returns 2.333333
#> [1] 2.333333
mean2(c(NA, NA, 10, 20)) # Returns 15
#> [1] 15
```
