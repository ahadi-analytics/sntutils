# Extract time components from a filename

Uses the pattern and parser returned by
[`detect_time_pattern()`](https://ahadi-analytics.github.io/sntutils/reference/detect_time_pattern.md)
to parse the date embedded in `filename` and computes derived units
(month, semester, quarter).

## Usage

``` r
extract_time_components(filename, info, year_extractor = NULL)
```

## Arguments

- filename:

  Character string of file path or name

- info:

  List as returned by
  [`detect_time_pattern()`](https://ahadi-analytics.github.io/sntutils/reference/detect_time_pattern.md),
  containing:

  pattern

  :   One of `"monthly"` or `"yearly"`.

  parser

  :   Function to parse character dates into Date/POSIXct.

- year_extractor:

  Optional function to extract year from filename. If provided, this
  function is used instead of automatic year detection for yearly data.
  Should take a character string (filename) and return a character
  string (year). Useful for filenames with multiple 4-digit numbers
  where heuristics may fail.

## Value

A list with:

- year:

  Integer year

- month:

  Integer month (1–12) or `NA` for yearly data

- semester:

  Integer semester (1–2) or `NA`

- quarter:

  Integer quarter (1–4) or `NA`

- date:

  Date object for the first day of the period

## Note

For yearly data, when multiple plausible years (1980-2099) are present
in the filename, the last one is returned. This heuristic assumes
dataset version stamps appear early in the filename and data years
appear later. For deterministic control, pass a custom `year_extractor`
function.

## Examples

``` r
info <- detect_time_pattern("rainfall_2022-01.tif")
extract_time_components("rainfall_2022-01.tif", info)
#> $year
#> [1] 2022
#> 
#> $month
#> [1] 1
#> 
#> $semester
#> [1] 1
#> 
#> $quarter
#> [1] 1
#> 
#> $date
#> [1] "2022-01-01"
#> 
info <- detect_time_pattern("temp_2020.tif")
extract_time_components("temp_2020.tif", info)
#> $year
#> [1] 2020
#> 
#> $month
#> [1] NA
#> 
#> $semester
#> [1] NA
#> 
#> $quarter
#> [1] NA
#> 
#> $date
#> [1] "2020-01-01"
#> 
# Custom year extractor for complex filenames
my_extractor <- function(f) {
  stringr::str_extract(f, "Rate\\.(\\d{4})\\.tiff$", group = 1)
}
extract_time_components("Malaria_Rate.2020.tiff", info, my_extractor)
#> $year
#> [1] 2020
#> 
#> $month
#> [1] NA
#> 
#> $semester
#> [1] NA
#> 
#> $quarter
#> [1] NA
#> 
#> $date
#> [1] "2020-01-01"
#> 
```
