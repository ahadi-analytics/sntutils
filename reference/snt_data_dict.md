# Load and flatten the SNT variable tree

Loads the bilingual indicator hierarchy from the package's
`snt_var_tree` dataset and returns a tidy table of all variables, their
domains, labels, and disaggregation structure (if defined). You can
optionally specify a single domain (e.g. `"routine_data"`,
`"stock_management"`) to filter the returned results.

## Usage

``` r
snt_data_dict(flatten = TRUE, include_schema = TRUE, domain = NULL)
```

## Arguments

- flatten:

  Logical; if TRUE, returns a tidy tibble of variable metadata.

- include_schema:

  Logical; if TRUE, also loads the schema and attaches it as an
  attribute.

- domain:

  Optional character string; if provided, returns only variables
  belonging to that top-level domain (e.g. `"routine_data"`,
  `"anemia"`).

## Value

A tibble with columns:

- `domain`: the top-level domain (e.g. metadata, routine_data)

- `snt_var_name`: standardized variable name

- `label_en`, `label_fr`, `label_pt`: multilingual labels

- `disagg`: character vector of applicable disaggregations (if
  available)

## Examples

``` r

if (FALSE) { # \dontrun{
# Load all variable metadata
sntutils::snt_data_dict()

# Load only routine_data variables
sntutils::snt_data_dict(domain = "routine_data")
} # }
```
