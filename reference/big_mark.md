# Format Numbers with Thousand Separator

This function formats numbers by adding a thousand separator (big mark)
and optionally rounding to a specified number of decimal places.

## Usage

``` r
big_mark(x, decimals = NULL, big_mark = ",")
```

## Arguments

- x:

  A numeric vector to be formatted.

- decimals:

  An integer specifying the number of decimal places to round to.
  Default is NULL, which means no rounding is performed.

- big_mark:

  A character to use as the thousand separator. Default is ",".

## Value

A character vector of formatted numbers.

## Examples

``` r
big_mark(1234567.89)
#> [1] "1,234,568"
big_mark(c(1234.56, 7890123.45), decimals = 2, big_mark = ",")
#> [1] "1,234.56"     "7,890,123.45"
```
