# Print method for dhs_duckdb objects

Displays a formatted summary of the DHS DuckDB database including loaded
tables, row counts, and available partition columns for filtering.

## Usage

``` r
# S3 method for class 'dhs_duckdb'
print(x, ...)
```

## Arguments

- x:

  A `dhs_duckdb` object created by
  [`get_dhs_data()`](https://ahadi-analytics.github.io/sntutils/reference/get_dhs_data.md)

- ...:

  Additional arguments (unused)

## Value

Invisibly returns the input object. Called for side effects.
