# Correct outliers using temporal neighbors

Corrects outlier values by replacing them with the median of neighboring
months (month before and month after). Applies a consistency rule to
ensure corrected values do not exceed an upper bound variable.

## Usage

``` r
correct_outliers(
  data,
  target_vars,
  consistency_vars,
  outlier_flag_col = "outlier_flag_consensus",
  group_cols,
  date_col = "date",
  verbose = TRUE
)
```

## Arguments

- data:

  Data frame containing outlier flags and variables to correct.

- target_vars:

  Character vector. Columns to correct (e.g., c("conf", "maltreat")).
  Each target is paired with the corresponding element in
  `consistency_vars`.

- consistency_vars:

  Character vector. Upper bound columns (e.g., c("test", "conf")). Must
  be the same length as `target_vars`. For each pair, the corrected
  value must be \<= consistency variable.

- outlier_flag_col:

  Character. Column containing outlier flags. Values of "outlier" are
  corrected. Default: "outlier_flag_consensus".

- group_cols:

  Character vector. Columns defining the time series groups (e.g.,
  c("hf_uid") for facility-level). Neighbors are found within groups.

- date_col:

  Character. Date column name. Default: "date".

- verbose:

  Logical. Print summary messages. Default: TRUE.

## Value

The input data frame with two new columns per target variable:

- `{target}_corrected`:

  The corrected value, or original if no correction was applied.

- `{target}_correction_flag`:

  Reason for correction status: "corrected", "failed_consistency",
  "missing_neighbors", or "not_outlier".

## Details

For each target variable flagged as an outlier:

1.  Retrieve the value from the month before (lag) and month after
    (lead)

2.  If either neighbor is missing, no correction is applied

3.  Compute the median of the two neighbors

4.  Check if the median exceeds the consistency variable for that row

5.  If the consistency check fails, the corrected value is set to NA

6.  Otherwise, replace the outlier with the median

## Examples

``` r
if (FALSE) { # \dontrun{
# After running detect_outliers()
result <- detect_outliers(data, column = "conf", ...)

# Correct single variable: conf bounded by test
corrected <- correct_outliers(
  data = result,
  target_vars = "conf",
  consistency_vars = "test",
  group_cols = c("hf_uid"),
  date_col = "date"
)

# Correct multiple variables at once
corrected <- correct_outliers(
  data = result,
  target_vars = c("conf", "maltreat", "maladm"),
  consistency_vars = c("test", "conf", "conf"),
  group_cols = c("hf_uid"),
  date_col = "date"
)
} # }
```
