# Clean Filenames

Cleans a vector of filenames by removing common standalone numbers that
appear in more than half of the filenames, condensing multiple
separators, and removing leading/trailing separators. This is a pure
string operation — no files are renamed on disk.

## Usage

``` r
clean_filenames(filenames, verbose = FALSE)
```

## Arguments

- filenames:

  A character vector of file paths or basenames.

- verbose:

  Logical. If `TRUE`, emits an info message when no cleaning is
  performed. Default is `FALSE`.

## Value

A character vector of cleaned filenames (same length as input).
Directory paths are preserved if provided.

## Examples

``` r
clean_filenames(c("file_001.txt", "file_002.txt"))
#> [1] "file_001.txt" "file_002.txt"
clean_filenames(c("/path/to/file_001.txt", "/path/to/file_002.txt"))
#> [1] "/path/to/file_001.txt" "/path/to/file_002.txt"
```
