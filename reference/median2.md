# Calculate median with automatic NA handling

A wrapper around base::median() that automatically removes NA values

## Usage

``` r
median2(x)
```

## Arguments

- x:

  A numeric vector

## Value

The median of all non-NA values in x

## Examples

``` r
median2(c(1, 2, NA, 4)) # Returns 2.333333
#> [1] 2
median2(c(NA, NA, 10, 20)) # Returns 15
#> [1] 15
```
