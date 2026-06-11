# Validation Terms Dictionary

Multilingual labels for validation-related column names used in
[`validate_routine_hf_data()`](https://ahadi-analytics.github.io/sntutils/reference/validate_routine_hf_data.md)
output.

## Usage

``` r
validation_terms
```

## Format

A named list where each element is a variable name containing a list of
language codes (en, fr, pt) mapping to translated labels:

- check:

  Label for validation check type

- issues_found:

  Label for number of issues found

- total_records:

  Label for total record count

- percent:

  Label for percentage

## See also

[`validate_routine_hf_data`](https://ahadi-analytics.github.io/sntutils/reference/validate_routine_hf_data.md)

## Examples

``` r
data(validation_terms)
validation_terms$check$en
#> [1] "Check"
validation_terms$check$fr
#> [1] "Vérification"
```
