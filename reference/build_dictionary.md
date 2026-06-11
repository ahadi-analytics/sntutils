# build a compact data dictionary

create a tidy dictionary from a data.frame (sf supported). infer a
simple type per column, attach english labels (optionally overridden by
a csv map), and report stats: missing %, unique count, example values,
and min/max for numeric/date/datetime. optionally add a translated label
column.

## Usage

``` r
build_dictionary(
  data,
  labels_path = base::getOption("snt.labels_en_path", NULL),
  language = NULL,
  max_levels = 50L,
  n_examples = 3L,
  trans_cache_path = NULL,
  override_yaml = FALSE,
  verbose = TRUE
)
```

## Arguments

- data:

  data.frame or tibble; sf columns allowed.

- labels_path:

  optional csv with columns like `name,label` to override english
  labels.

- language:

  optional iso code (e.g., "fr") to add `label_<language>`.

- max_levels:

  max factor levels to summarize in notes. default 50.

- n_examples:

  number of example values to show. default 3.

- trans_cache_path:

  optional cache dir for translate_text_vec().

- override_yaml:

  logical; if TRUE, CSV labels override YAML labels. default FALSE (YAML
  takes precedence).

- verbose:

  logical; if TRUE (default), prints info messages such as fuzzy match
  notifications.

## Value

tibble with: variable, type, label_en, n, n_missing, pct_missing,
n_unique, example_values, min, max, notes, and optionally
`label_<language>`.

## Details

english labels are merged as: internal defaults, then csv overrides.
unknown variables fall back to their column name.

performance: the snt variable tree is cached in a package environment on
first use. subsequent calls reuse the flattened tree. the cache
automatically refreshes when the tree version changes (tracked via
\_meta\$last_updated).

## Examples

``` r
dd <- build_dictionary(dplyr::as_tibble(iris))
dd |> dplyr::select(variable, type, label_en) |> utils::head()
#> # A tibble: 5 × 3
#>   variable     type   label_en    
#>   <chr>        <chr>  <chr>       
#> 1 Sepal.Length double Sepal.Length
#> 2 Sepal.Width  double Sepal.Width 
#> 3 Petal.Length double Petal.Length
#> 4 Petal.Width  double Petal.Width 
#> 5 Species      factor Species     
```
