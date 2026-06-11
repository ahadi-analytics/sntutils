# Standardize name-like strings with optional steps

Applies a sequence of standardization steps to a vector of names. Each
step can be enabled or disabled. By default, all steps are enabled.

## Usage

``` r
standardize_names(
  name_vec,
  to_upper = TRUE,
  replace_punct = TRUE,
  squish_spaces = TRUE,
  normalize_accents = TRUE,
  normalize_spaces = TRUE,
  roman_to_arabic = TRUE,
  sort_tokens = TRUE
)
```

## Arguments

- name_vec:

  Atomic vector. Values to standardize.

- to_upper:

  Logical. Convert to uppercase. Default is TRUE.

- replace_punct:

  Logical. Replace punctuation with spaces. Default is TRUE.

- squish_spaces:

  Logical. Trim/condense spaces. Default is TRUE.

- normalize_accents:

  Logical. Convert accents to ASCII. Default is TRUE.

- normalize_spaces:

  Logical. Normalize all space-like chars to " ". Default is TRUE.

- roman_to_arabic:

  Logical. Convert roman numerals I..IX to 1..9 at token boundaries.
  Default is TRUE.

- sort_tokens:

  Logical. Sort tokens with letters first, numbers last, both groups
  alphabetically. Default is TRUE.

## Value

Character vector of standardized names.

## Examples

``` r
# basic usage
standardize_names(c("Chp Kpalime-III", "Kpalimé CHP 3"))
#> [1] "CHP KPALIME 3" "CHP KPALIME 3"

# disable token sorting and roman conversion
standardize_names(
  c("USP Hanyigba-Todzi ii", "USP Hanyigba Todzi 2"),
  roman_to_arabic = FALSE, sort_tokens = FALSE
)
#> [1] "USP HANYIGBA TODZI II" "USP HANYIGBA TODZI 2" 

# keep accents and case, only squish spaces and replace punctuation
standardize_names(
  c("Hôpital d’Adéta", "Hopital d'Adeta"),
  to_upper = FALSE, normalize_accents = FALSE
)
#> [1] "Adéta Hôpital d" "Adeta Hopital d"
```
