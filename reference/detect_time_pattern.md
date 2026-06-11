# Detect time pattern in filenames

Supports daily (YYYY-MM-DD), monthly (YYYY-MM) and yearly (YYYY)
formats, using all formats declared in `available_date_formats`.

## Usage

``` r
detect_time_pattern(filenames)
```

## Arguments

- filenames:

  Character vector of file paths or names.

## Value

A list with two elements:

- pattern:

  One of `"daily"`, `"monthly"`, or `"yearly"`.

- parser:

  Function(x) that parses character vector `x` into Date/POSIXct, via
  [`lubridate::parse_date_time()`](https://lubridate.tidyverse.org/reference/parse_date_time.html).
