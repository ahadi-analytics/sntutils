# Classify health facility activity status by reporting behaviour

Builds a balanced monthly panel for all facilities and months, flags
reporting on key indicators, and classifies each facility-month into
activity status according to one of three methods.

## Usage

``` r
classify_facility_activity(
  data,
  hf_col,
  date_col = "date",
  key_indicators = c("test", "pres", "conf"),
  method = 1,
  nonreport_window = 6,
  reporting_rule = "any_non_na",
  binary_classification = FALSE
)
```

## Arguments

- data:

  Data frame containing routine health facility records.

- hf_col:

  Character. Column storing health facility identifiers.

- date_col:

  Character. Column storing observation dates. Defaults to "date".

- key_indicators:

  Character vector with columns defining reporting activity. Defaults to
  `c("test", "pres", "conf")`.

- method:

  Character or numeric. Classification method - can be numeric (1, 2, 3)
  or character ("method1", "method2", "method3", "all").

- nonreport_window:

  Integer. Minimum number of consecutive non-reporting months to
  classify a facility as inactive in method 3. Defaults to 6.

- reporting_rule:

  Character. Defines what counts as reporting: `"any_non_na"` (default,
  counts NA as non-reporting, 0 counts as reported) or `"positive_only"`
  (requires \>0 value to count as reported).

- binary_classification:

  Logical. If TRUE, collapses categories into "Active" vs "Inactive".
  Defaults to FALSE.

## Value

Data frame with original columns plus reporting and activity status. If
`method = "all"`, includes all three activity classification columns.

## Details

Three activity classification methods are supported:

- **Method 1 (Permanent activation):** Facility is active from first
  report onwards, remains active afterwards.

- **Method 2 (First-Last activation):** Facility is active only between
  first and last report dates.

- **Method 3 (Dynamic):** Facility is active, but becomes inactive if it
  misses `nonreport_window` consecutive months. Reactivates if reporting
  resumes.

The `reporting_rule` parameter controls how reporting is flagged:

- `"any_non_na"`: Any non-missing value counts as reported (including
  0).

- `"positive_only"`: Only values strictly \>0 count as reported.
