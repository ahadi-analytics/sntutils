# Available date formats for `autoparse_dates` function

A character vector containing all supported date formats that can be
parsed by
[`autoparse_dates()`](https://ahadi-analytics.github.io/sntutils/reference/autoparse_dates.md).
These formats follow standard date-time components:

- d: day

- m: month

- Y/y: year (Y=4 digits, y=2 digits)

- B: full month name

- b: abbreviated month name

- H: hour

- M: minute

- S: second

## Usage

``` r
available_date_formats
```

## Format

A character vector of date format strings

## Examples

``` r
head(available_date_formats)
#> [1] "dmY" "mdY" "Ymd" "Ydm" "dmy" "mdy"
```
