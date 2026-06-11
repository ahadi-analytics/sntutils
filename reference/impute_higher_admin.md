# Impute higher administrative level using a lookup table

This function imputes a higher-level administrative unit (e.g., adm1)
based on a lower-level administrative unit (e.g., adm2) using a lookup
table. It allows for dynamic column names and lookup table structures.

## Usage

``` r
impute_higher_admin(
  target_df,
  target_lower_col,
  target_higher_col,
  lookup_df,
  lookup_lower_col,
  lookup_higher_col
)
```

## Arguments

- target_df:

  A dataframe containing the lower-level administrative unit column.

- target_lower_col:

  A string specifying the name of the lower-level admin column (e.g.,
  adm2).

- target_higher_col:

  A string specifying the name of the higher-level admin column to be
  created.

- lookup_df:

  A dataframe containing the mapping of lower to higher-level
  administrative units.

- lookup_lower_col:

  A string specifying the name of the lower-level column in the lookup
  table.

- lookup_higher_col:

  A string specifying the name of the higher-level column in the lookup
  table.

## Value

A dataframe with an additional higher-level column assigned based on the
lookup.

## Examples

``` r
# Example lookup table
adm1_lookup <- dplyr::tribble(
  ~adm2, ~adm1,
  "Boffa", "Bok\u00e9",
  "Boke", "Bok\u00e9",
  "Fria", "Bok\u00e9"
)

# Example dataset
data <- dplyr::tibble(
  district = c("Boffa", "Fria", "Unknown")
)

# Impute higher-level administrative unit
cleaned_data <- impute_higher_admin(
  target_df = data,
  target_lower_col = "district",
  target_higher_col = "region",
  lookup_df = adm1_lookup,
  lookup_lower_col = "adm2",
  lookup_higher_col = "adm1"
)
```
