# Infer column types using readr, then layer factor detection

use readr::type_convert() to infer non-factor types (numeric, integer,
date, datetime, logical). then propose factors via low-cardinality
rules. protect id-like names and leading-zero codes. return the dataset
by default, and include the metadata plan only when requested.

## Usage

``` r
auto_parse_types(
  data,
  max_levels = 50,
  max_unique_ratio = 0.2,
  protect_patterns = c("id$", "uid$", "code$", "ref$", "key$"),
  keep_leading_zero_chars = TRUE,
  prefer_logical_for_binary = TRUE,
  apply = TRUE,
  return = c("data", "both", "plan")
)
```

## Arguments

- data:

  data.frame or tibble.

- max_levels:

  integer. max distinct values for factor. default 50.

- max_unique_ratio:

  numeric (0, 1\]. max unique/n for factor. default 0.2.

- protect_patterns:

  character regexes for protected names. default c("id\$", "uid\$",
  "code\$", "ref\$", "key\$").

- keep_leading_zero_chars:

  logical. keep character if any value has leading zeros in digit-only
  strings. default TRUE.

- prefer_logical_for_binary:

  logical. kept for api compatibility, not used when delegating to
  readr. default TRUE.

- apply:

  logical. if TRUE, apply factor conversions on top of parsed types.
  default TRUE.

- return:

  one of "data", "both", "plan". default "data".

## Value

invisible object depending on `return`:

- "data": tibble of parsed data (and factors if apply = TRUE)

- "both": list(plan = tibble, data = tibble as above)

- "plan": tibble only

## Examples

``` r
df <- tibble::tibble(
  id  = c("001", "002", "003"),
  sex = c("M", "F", "F"),
  age = c("1", "2", "3"),
  dt  = c(
    "2024-01-01 12:00:00",
    "2024-01-02 00:00:00",
    "2024-01-03 01:02:03"
  )
)

# parsed types + inferred factors
dat <- auto_parse_types(df, apply = TRUE, return = "data")

# dataset and plan
both <- auto_parse_types(df, apply = TRUE, return = "both")
both$plan |> dplyr::select(name, current_type, proposed_type, rule)
#> # A tibble: 4 × 4
#>   name  current_type proposed_type rule             
#>   <chr> <chr>        <chr>         <chr>            
#> 1 id    character    character     protected_by_name
#> 2 sex   character    character     readr:character  
#> 3 age   character    integer       readr:integer    
#> 4 dt    character    POSIXct       readr:POSIXct    
dplyr::glimpse(both$data)
#> Rows: 3
#> Columns: 4
#> $ id  <chr> "001", "002", "003"
#> $ sex <chr> "M", "F", "F"
#> $ age <int> 1, 2, 3
#> $ dt  <dttm> 2024-01-01 12:00:00, 2024-01-02 00:00:00, 2024-01-03 01:02:03
```
