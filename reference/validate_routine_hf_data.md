# Orchestrates a suite of validation checks on routine HF data. It standardizes column resolution, selects indicators, runs missing/duplicate/future/logic/ outlier checks, compiles a summary, and optionally translates and saves.

Orchestrates a suite of validation checks on routine HF data. It
standardizes column resolution, selects indicators, runs
missing/duplicate/future/logic/ outlier checks, compiles a summary, and
optionally translates and saves.

## Usage

``` r
validate_routine_hf_data(
  data,
  id_col = "record_id",
  facility_col = "hf_uid",
  date_col = "date",
  yearmon_col = "yearmon",
  year_col = "year",
  month_col = "month",
  admin_cols = c("adm0", "adm1", "adm2", "adm3"),
  admin_guid_cols = c("adm0_guid", "adm1_guid", "adm2_guid", "adm3_guid"),
  core_id_cols = NULL,
  indicators = NULL,
  missing_vars = NULL,
  consistency_pairs = NULL,
  outlier_pairs = NULL,
  check_future_dates = TRUE,
  check_duplicates = TRUE,
  check_outliers = TRUE,
  check_facility_activeness = TRUE,
  hf_name_col = "hf",
  key_indicators = NULL,
  nonreport_window = 6,
  reporting_rule = "any_non_na",
  min_reporting_rate = 0.5,
  outlier_methods = c("iqr", "median", "mean", "consensus"),
  time_mode = "across_time",
  outlier_strictness = "balanced",
  sd_multiplier = 3,
  mad_constant = 1.4826,
  mad_multiplier = 9,
  iqr_multiplier = 2,
  min_n = 8,
  consensus_rule = 1,
  n_neighbour_impute = 5,
  output_path = NULL,
  output_name = "validation_of_hf_routine_data",
  output_formats = c("xlsx", "rds"),
  build_dictionary = FALSE,
  verbose = TRUE,
  language = "en"
)
```

## Arguments

- data:

  data.frame. Routine facility dataset.

- id_col:

  character. Unique record id. Default "record_id".

- facility_col:

  character. Facility id. Default "hf_uid".

- date_col:

  character. Date column. Default "date".

- yearmon_col:

  character. Year-month col. Default "yearmon".

- year_col:

  character. Year col. Default "year".

- month_col:

  character. Month col. Default "month".

- admin_cols:

  character. Admin columns (adm0..adm3). Default
  c("adm0","adm1","adm2","adm3").

- admin_guid_cols:

  character. Admin GUID columns. Default
  c("adm0_guid","adm1_guid","adm2_guid","adm3_guid").

- core_id_cols:

  character\|NULL. If NULL, uses the set above.

- indicators:

  character\|NULL. Numeric indicators to validate. If NULL, auto-detect
  from numeric columns after excluding core ids.

- missing_vars:

  character\|NULL. Additional variables to check for missing data beyond
  core_id_cols. Core ID columns are always checked. If NULL, checks both
  core_id_cols and indicators (default behavior). Use this to limit
  missing data checks to specific indicators while always including core
  IDs.

- consistency_pairs:

  list\|NULL. Each element is list(input=, output=). If NULL, defaults
  generated from common malaria cascade rules.

- outlier_pairs:

  list\|NULL. Pairs for outlier detection and correction. Structure:
  list(input = c("test"), output = c("conf")). Uses same format as
  consistency_pairs. The cascade rule is: input \>= output. Both
  directions are checked automatically: (1) output outliers validated as
  output \<= input, (2) input outliers validated as input \>= output.
  Corrections use neighbor median. If NULL, defaults from malaria
  cascade.

- check_future_dates:

  logical. Default TRUE.

- check_duplicates:

  logical. Default TRUE.

- check_outliers:

  logical. Default TRUE.

- check_facility_activeness:

  logical. Check facility activeness. Default TRUE.

- hf_name_col:

  character. Facility name column. Default "hf".

- key_indicators:

  character\|NULL. Key indicators for activeness check. If NULL, uses
  all indicators. Default NULL.

- nonreport_window:

  integer. Minimum number of consecutive non-reporting months to
  classify a facility as inactive in method 3. Defaults to 6.

- reporting_rule:

  character. Defines what counts as reporting for activeness:
  "any_non_na" (default, counts NA as non-reporting, 0 counts as
  reported) or "positive_only" (requires \>0 value to count as
  reported).

- min_reporting_rate:

  numeric. Minimum reporting rate threshold. Default 0.5.

- outlier_methods:

  character. Any of c("iqr","median","mean").

- time_mode:

  character. Time mode for outlier detection: "across_time",
  "within_year", or "seasonal". Seasonal compares same month across
  years. Default "across_time".

- outlier_strictness:

  character. Outlier detection strictness: "balanced", "lenient",
  "strict", "advanced". Default "balanced".

- sd_multiplier:

  numeric. Standard deviation multiplier for outlier detection. Default
  3.

- mad_constant:

  numeric. MAD constant for outlier detection. Default 1.4826.

- mad_multiplier:

  numeric. MAD multiplier for outlier detection. Default 9.

- iqr_multiplier:

  numeric. IQR multiplier for outlier detection. Default 2.

- min_n:

  numeric. Minimum sample size for outlier detection. Default 8.

- consensus_rule:

  numeric. Number of methods that must agree for consensus outlier flag.
  Default 1.

- n_neighbour_impute:

  integer. Number of neighboring time periods (before and after) to use
  for computing the imputation median. Default 5 (uses 5 before + 5
  after = 10 values).

- output_path:

  character\|NULL. If provided, results are saved to this path.

- output_name:

  character. Base output name. Default
  "validation_routine_data_results".

- output_formats:

  character. Any of c("xlsx","rds"). Default c("xlsx","rds").

- build_dictionary:

  logical. Build data dictionary. Default TRUE.

- verbose:

  logical. CLI messages. Default TRUE.

- language:

  character. ISO-639-1 language code for output labels ("en", "fr",
  "pt"). Default "en".

## Value

invisible named list with elements: Summary, Missing values, Missing
values detail, Duplicate records, Future dates, Consistency summary,
Consistency details, Outlier summary, Outlier detailed, HF activeness
summary, HF activeness episodes, Data dictionary

## Examples

``` r
if (FALSE) { # \dontrun{
validate_routine_hf_data(
  data = dhis2_data,
  indicators = c("conf","test","susp","maltreat")
)
} # }
```
