# Detect factor-like character columns (low-cardinality only)

identify character columns that look categorical using simple
heuristics. protects id-like names and leading zeros. returns a compact
tibble invisibly.

## Usage

``` r
detect_factors(
  data,
  max_levels = 50,
  max_unique_ratio = 0.2,
  protect_patterns = c("id$", "uid$", "code$", "ref$", "key$"),
  keep_leading_zero_chars = TRUE
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

## Value

invisible tibble with: name, n, n_non_na, n_unique, unique_ratio,
proposed, reason

## Examples

``` r
tibble::tibble(adm = c("A","B","A"), code = c("01","02","03")) |>
  detect_factors()
```
