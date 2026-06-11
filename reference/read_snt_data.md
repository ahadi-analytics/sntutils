# Read the most-recently modified saved dataset

Read the most-recently modified saved dataset

## Usage

``` r
read_snt_data(path, data_name, file_formats = NULL, quiet = TRUE)
```

## Arguments

- path:

  Directory to look in.

- data_name:

  Base name (without extension).

- file_formats:

  Character vector of allowed formats (e.g. "csv","rds"). If NULL,
  consider any supported format found.

- quiet:

  Logical; when FALSE prints which file was read.

## Value

The loaded R object (data.frame, list, etc.). Aborts if not found.

## Examples

``` r
if (FALSE) { # \dontrun{
x <- read_snt_data(tmp, "my_population_data", "csv", quiet = FALSE)
} # }
```
