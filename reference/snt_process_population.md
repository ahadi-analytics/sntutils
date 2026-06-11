# Summarise population by available admin levels and build a dictionary

Produces population summaries at the admin levels present in `pop_data`
(any of adm0..adm3) and returns a list containing per-level tables, a
bilingual (EN + optional FR) data dictionary, and a record of which
admin levels were found. By default, column types are first inferred via
[`auto_parse_types()`](https://ahadi-analytics.github.io/sntutils/reference/auto_parse_types.md)
with `apply = TRUE` and `return = "data"`.

## Usage

``` r
snt_process_population(
  pop_data,
  pop_cols = "pop",
  translate = TRUE,
  language = NULL,
  trans_cache_path = if (requireNamespace("here", quietly = TRUE)) {
    
    here::here("cache/translations")
 } else {
     "cache/translations"
 },
  infer_types = TRUE
)
```

## Arguments

- pop_data:

  data.frame/tibble with `adm0`, optional `adm1`/`adm2`/`adm3`, `year`,
  and population column(s). `year` may be integer, numeric, Date/POSIXt,
  factor, or character; it is coerced to integer years when feasible.

- pop_cols:

  character vector of population column names to process. Default is
  "pop" for backward compatibility.

- translate:

  logical; when TRUE, add a translated label column (default FR unless
  `language` is provided) using the translation cache.

- language:

  Optional ISO code (e.g., "fr"). When provided, a `label_<language>`
  column is added to the dictionary and placed immediately after
  `label_en`.

- trans_cache_path:

  character path to the translation cache directory. default:
  `here::here("cache/translations")` if available, else
  `"cache/translations"`.

- infer_types:

  logical; when TRUE (default) coerce `pop_data` first using
  `auto_parse_types(apply = TRUE, return = "data")`.

## Value

named list with elements (only levels present are included):

- `pop_data_adm0`, `pop_data_adm1`, `pop_data_adm2`, `pop_data_adm3`
  (aggregated by level and year)

- `data_dictionary` (from
  [`build_dictionary()`](https://ahadi-analytics.github.io/sntutils/reference/build_dictionary.md))

- `levels_present` (character vector of admin levels detected)

## Details

### Data uniqueness requirement

Input data must have unique rows per admin-year at the finest level
present. If your data contains sex, age, or residence strata, aggregate
these upstream before passing to this function. The function will error
if duplicates are detected to prevent silent double counting.

### Automatic proportion handling

The function automatically detects which population columns contain
proportions (all non-NA values \<= 1) versus counts. Proportion columns
are aggregated using the mean, while count columns are summed. If a
column contains mixed data (some values \<= 1 and some \> 1), the
function will abort with an error message.

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic usage with default "pop" column
example_pop <- tibble::tibble(
  adm0 = c("A", "A", "B"),
  adm1 = c("X", "X", "Y"),
  adm2 = c("P", "Q", "R"),
  year = c(2020L, 2020L, 2021L),
  pop  = c(100, 50, 70)
)
out <- snt_process_population(example_pop)
names(out)
out$levels_present

# Using custom population column names
example_multi_pop <- tibble::tibble(
  adm0 = c("A", "A", "B"),
  adm1 = c("X", "X", "Y"),
  year = c(2020L, 2020L, 2021L),
  total_pop = c(100, 50, 70),
  under5_pop = c(20, 10, 15)
)
out_multi <- snt_process_population(
  example_multi_pop,
  pop_cols = c("total_pop", "under5_pop")
)
names(out_multi)
} # }
```
