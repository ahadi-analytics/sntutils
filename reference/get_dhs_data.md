# Load DHS Parquet datasets using DuckDB

Creates a DuckDB connection and registers selected DHS parquet datasets
as views for efficient querying. Skips corrupted parquet files
automatically.

## Usage

``` r
get_dhs_data(path = here::here("01_data", "parquet"), types = NULL)
```

## Arguments

- path:

  Character string. Root directory containing parquet datasets.

- types:

  Optional character vector of DHS file type codes to load. If NULL, all
  supported types are loaded.

## Value

A list of class "dhs_duckdb" containing data tables, metadata, and
connection.
