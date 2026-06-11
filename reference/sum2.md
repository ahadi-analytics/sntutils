# Sum values with automatic NA handling

A wrapper around base::sum() that automatically removes NA values

## Usage

``` r
sum2(x)
```

## Arguments

- x:

  A numeric vector

## Value

The sum of all non-NA values in x

## Examples

``` r
sum2(c(1, 2, NA, 4)) # Returns 7
#> [1] 7
sum2(c(NA, NA, 5, 10)) # Returns 15
#> [1] 15
```
