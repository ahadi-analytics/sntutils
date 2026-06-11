# Facility name matching across datasets (DHIS2 vs MFL)

Orchestrates a multi-step matching pipeline between a *target* dataset
(e.g., DHIS2 facilities) and a *lookup* dataset (e.g., MFL). Steps
include exact match, standardized-name match, fuzzy match, and
interactive stratified match (using
[`sntutils::prep_geonames()`](https://ahadi-analytics.github.io/sntutils/reference/prep_geonames.md)).
Returns consolidated results and QA summaries.

## Usage

``` r
fuzzy_match_facilities(
  target_df,
  lookup_df,
  admin_cols = c("adm0", "adm1", "adm2"),
  hf_col_name = "hf",
  uid_col = "hf_uid",
  steps = c(exact = TRUE, standardization = TRUE, fuzzy = TRUE, interactive = TRUE),
  lookup_cols = character(),
  match_interactivity = TRUE,
  fuzzy_methods = c("jw", "lv"),
  fuzzy_threshold = 95L,
  score_exact = 100L,
  score_standardization = 100L,
  score_interactive = 95L,
  status_cuts = c(high = 95, medium = 80, low = 70),
  include_missing_name_rows = TRUE,
  save_path = NULL,
  matching_cache_path = NULL,
  save_stem = "facility_matching",
  summary_language = "en",
  verbose = TRUE
)
```

## Arguments

- target_df:

  Tibble/data.frame with facility names and admin columns.

- lookup_df:

  Tibble/data.frame with facility names and admin columns.

- admin_cols:

  Character vector of admin columns ordered high to low. Default
  c("adm0", "adm1", "adm2").

- hf_col_name:

  Character. Facility name column used in both data frames. Default
  "hf".

- uid_col:

  Character. Column in `target_df` uniquely identifying each facility.
  Default "hf_uid".

- steps:

  Named logical vector toggling steps: c(exact = TRUE, standardization =
  TRUE, fuzzy = TRUE, interactive = FALSE).

- lookup_cols:

  Character vector of `lookup_df` columns to append to the match results
  and returned target table. Default
  [`character()`](https://rdrr.io/r/base/character.html).

- match_interactivity:

  Logical flag forwarded to the interactive matching helper. Default
  `TRUE`.

- fuzzy_methods:

  Character vector of string distance methods to combine. Supported:
  c("jw", "lv"). Default c("jw", "lv").

- fuzzy_threshold:

  Integer in 0-100 for acceptance. Default 95.

- score_exact:

  Integer score for exact matches. Default 100.

- score_standardization:

  Integer score for standardized matches. Default 100.

- score_interactive:

  Integer score for interactive matches. Default 95.

- status_cuts:

  Named numeric thresholds for status classification: c(high = 95,
  medium = 80, low = 70).

- include_missing_name_rows:

  Logical. If TRUE, adds rows with missing name as
  `match_method = "missing_name"`. Default TRUE.

- save_path:

  Character. Directory path root for outputs. Default NULL; when NULL,
  results are not written to disk.

- matching_cache_path:

  Character. Directory for interactive cache and unmatched files.
  Default here::here(paths\$cache).

- save_stem:

  Character. Base filename stem for saved outputs. Default
  "facility_matching".

- summary_language:

  Character vector choosing summary language columns. Supported codes:
  "en", "fr", "pt". Default "en".

- verbose:

  Logical. If TRUE, prints a CLI summary box preview of matching
  results. Default TRUE.

## Value

A list with results, by_method, dhis2_only, mfl_only, target_augmented,
summary_table, coverage_summary, and params. One-to-one matching is
always enforced, ensuring each MFL facility is matched by at most one
DHIS2 facility.
