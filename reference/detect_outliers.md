# Detect outliers with guardrails and consensus

`detect_outliers()` evaluates a numeric indicator by administrative unit
and time using three methods (mean + SD, median + MAD, Tukey upper
fence) to identify unusually HIGH values only (potential outbreaks or
data anomalies). Low values are NOT flagged as outliers. It enforces
reporting and sample-size guardrails, supports strictness presets.
Optional outbreak classification distinguishes between isolated outliers
and sustained outbreak patterns using gap-aware clustering that can
bridge short interruptions in the outbreak signal. It returns per-method
flags (optional), a consensus classification, scale statistics, and
metadata.

## Usage

``` r
detect_outliers(
  data,
  column,
  record_id = "record_id",
  admin_level = c("adm1", "adm2"),
  spatial_level = "hf_uid",
  date = "date",
  time_mode = c("across_time", "within_year", "seasonal"),
  value_type = c("count", "rate"),
  strictness = c("balanced", "lenient", "strict", "advanced"),
  methods = c("iqr", "median", "mean", "consensus"),
  sd_multiplier = 3,
  mad_constant = 1.4826,
  mad_multiplier = 9,
  iqr_multiplier = 2,
  consensus_rule = 3,
  output_profile = c("standard", "lean", "audit"),
  min_n = 8,
  reporting_rate_col = NULL,
  reporting_rate_min = 0.5,
  key_indicators_hf = NULL,
  classify_outbreaks = FALSE,
  outbreak_min_run = 2,
  outbreak_prop_tolerance = 0.9,
  outbreak_max_gap = 12,
  verbose = TRUE
)
```

## Arguments

- data:

  Data frame containing the indicator to analyse.

- column:

  Name of the numeric column to evaluate.

- record_id:

  Unique record identifier column.

- admin_level:

  Character vector of administrative level columns for parallel
  grouping, ordered from higher to lower resolution. Defaults to
  `c("adm1", "adm2")`.

- spatial_level:

  Character string specifying the finest spatial unit for analysis
  (e.g., "hf_uid" for facility-level). When specified, `admin_level`
  defines grouping boundaries while `spatial_level` defines the unit of
  analysis. This prevents excessive grouping while maintaining spatial
  granularity. Default is `hf_uid`.

- date:

  Date column (Date, POSIXt, or parseable character string). Year,
  month, and yearmon are automatically derived from this column.

- time_mode:

  Pooling strategy: `"across_time"`, `"within_year"`, or `"seasonal"`.
  Seasonal mode groups by month across all years (e.g., all Januaries
  together), useful for detecting values that are unusual for a specific
  month regardless of year.

- value_type:

  Indicator type: `"count"` or `"rate"`. Counts floor lower bounds at 0.

- strictness:

  Strictness preset: `"lenient"`, `"balanced"`, `"strict"`, or
  `"advanced"`. Presets map to method multipliers. If not `"advanced"`,
  any manual multipliers are **ignored**.

- methods:

  Character vector specifying which outlier detection methods to use:
  "iqr" (Interquartile Range), "median" (Median Absolute Deviation),
  "mean" (Mean +/- SD), and/or "consensus". Default is
  `c("iqr", "median", "mean", "consensus")`. For consensus, at least two
  other methods must be selected.

- sd_multiplier:

  Width (in SD units) for the mean method (used only when
  `strictness = "advanced"`).

- mad_constant:

  Constant passed to [`stats::mad()`](https://rdrr.io/r/stats/mad.html)
  in advanced mode (default 1.4826).

- mad_multiplier:

  Width multiplier for the MAD method (advanced mode).

- iqr_multiplier:

  Tukey fence multiplier for the IQR method (advanced mode).

- consensus_rule:

  Number of methods that must agree (`1`, `2`, or `3`) for the consensus
  flag to call an outlier. Default `2`.

- output_profile:

  Controls the amount of detail returned: `"lean"` (minimal columns: id,
  admin, date, value, consensus flag, reason), `"standard"` (lean +
  per-method flags + bounds + seasonality mode), `"audit"` (all columns
  for full reproducibility). Default `"standard"`.

- min_n:

  Minimum observations required in the active comparison bucket before
  flagging is attempted (applies to any seasonal bucket or fallback).

- reporting_rate_col:

  Optional column with reporting completeness in `[0, 1]`.

- reporting_rate_min:

  Minimum acceptable reporting rate. Rows below the threshold receive
  `reason = "low_reporting"` and are not flagged.

- key_indicators_hf:

  Optional character vector of indicator names used to determine
  facility activeness. If supplied, the function uses a fast path to
  filter out inactive facility-months. A facility-month is considered
  active if ANY of the specified key indicators have non-NA values.
  Inactive facility-months are excluded from outlier detection. If
  `NULL` (default), activeness filtering is skipped. Typical indicators
  include `"allout"`, `"test"`, or `"conf"`. This adjustment prevents
  false positives caused by facilities that start or stop reporting
  mid-period.

- classify_outbreaks:

  Logical. When `TRUE` (default), applies outbreak classification to
  distinguish between isolated outliers and sustained outbreak patterns.
  Consecutive outliers meeting the outbreak criteria are reclassified
  from "outlier" to "outbreak". This is particularly useful for
  epidemiological surveillance to identify disease outbreak patterns.
  Set to `FALSE` to disable outbreak classification.

- outbreak_min_run:

  Integer. Minimum number of consecutive outliers required to classify
  as an outbreak (default `2`). Must be \>= 2.

- outbreak_prop_tolerance:

  Numeric. Proportional tolerance for outbreak consistency (default
  `0.9`). Values within this tolerance of the run median are considered
  consistent. Range: (0, 1).

- outbreak_max_gap:

  Integer. Maximum allowed gap (non-outlier months) between outliers
  that can still be considered part of the same outbreak (default `1`).
  For example, with `outbreak_max_gap = 12`, the pattern
  "outlier-normal-outlier-outlier" would be classified as one outbreak
  of length 3, rather than separate incidents. Set to `0` for strict
  consecutive-only outbreaks. Useful for real-world data with reporting
  gaps.

- verbose:

  Logical. When `TRUE`, prints an informative summary showing which
  methods are being applied, the pooling strategy, strictness settings,
  guardrails, and consensus rule. Default is `FALSE`.

## Value

Tibble with outlier classifications and metadata. Columns include:
identifiers (`record_id`, admin levels, `yearmon`, `year`, derived
`month`), `column_name`, `value`, `value_type`, scale stats (`mean`,
`sd`, `median`, `mad`, `q1`, `q3`, `iqr`), method bounds, `n_in_group`,
guardrail `reason`, method flags (optional), `outlier_flag_consensus`,
strictness multipliers, and (if activeness filtering was applied)
`activeness_applied` and `key_indicators_used`.

**Outlier flag categories are simplified to three intuitive groups:**

- `"normal"`: Values within expected statistical bounds

- `"outlier"`: Values exceeding thresholds (potential
  anomalies/outbreaks)

- `"insufficient_data"`: Various data quality issues preventing
  determination (consolidates insufficient_n, insufficient_evidence,
  unstable_scale, etc.)

## Details

**Workflow**

1.  Validation & prep: confirm required columns, coerce target to
    numeric safely, derive month from `yearmon`.

2.  Activeness filtering (if `key_indicators_hf` is supplied): call
    [`classify_facility_activity()`](https://ahadi-analytics.github.io/sntutils/reference/classify_facility_activity.md)
    to tag inactive facilities. Inactive facility-months are excluded
    from detection and assigned `reason = "inactive_facility"`.

3.  Strictness: presets map to (SD, MAD, IQR) multipliers; advanced mode
    honours manual multipliers. On across-time fallback the strictness
    shifts one step toward lenient.

4.  Guardrails: rows failing `reporting_rate_min` or `min_n` bypass
    flagging and record `reason` (`low_reporting`, `insufficient_n`,
    `insufficient_data`).

5.  Flagging: each method checks if values exceed the UPPER threshold
    only (e.g., mean + multiplier x SD). Low values are never flagged.
    Methods are suppressed when scales are unstable (`sd`, `mad`, or
    `iqr` equals zero).

6.  Consensus: final `outlier_flag_consensus` requires at least
    `consensus_rule` methods to agree over available (non-suppressed)
    methods.

**Facility activeness adjustment**

When inactive or newly activated health facilities are included in
aggregated totals, apparent spikes or dips can occur that do not
represent real epidemiological changes. For example, if ten new
facilities start reporting in Matoto in mid-2022, the total number of
confirmed cases rises even if incidence per facility remains constant.
To prevent such artefacts, `detect_outliers()` uses a fast, optimized
approach for activeness filtering. When `key_indicators_hf` is
specified, the function checks if each facility-month has ANY non-NA
values in the specified key indicators. Only facility-months with at
least one reported key indicator contribute to the comparison pool for
outlier detection. This lightweight approach avoids the computational
overhead of full activity classification while still preventing false
positives from inactive facilities.

**Presets** (for high outliers only)

- lenient: values \> mean + 4.0 x SD, median + 12 x MAD, or Q3 + 3.0 x
  IQR

- balanced: values \> mean + 3.0 x SD, median + 9 x MAD, or Q3 + 2.0 x
  IQR

- strict: values \> mean + 2.5 x SD, median + 6 x MAD, or Q3 + 1.5 x IQR

- advanced: use user-supplied multipliers

Note: Only values ABOVE the upper thresholds are flagged. Low values are
always classified as "normal" regardless of how far below the
mean/median.

The returned tibble always contains identifiers, scale statistics,
bounds, strictness metadata, and the guardrail reason. When
`output_profile = "standard"` or `"audit"`, method-specific flags are
included alongside the consensus.

## Examples

``` r
if (FALSE) { # \dontrun{
# NOTE: Only HIGH values are flagged as outliers (e.g., disease outbreaks).
# Low values are always considered "normal".

# 1) Minimal consensus output at adm1-only level
detect_outliers(
  data = malaria_data,
  column = "confirmed_cases",
  date = "date",
  record_id = "facility_id",
  admin_level = c("adm1"),        # ignore adm2 if not present
  time_mode = "across_time"
)

# 2) Within-year comparison
detect_outliers(
  data = malaria_data,
  column = "confirmed_cases",
  date = "date",
  admin_level = c("adm1"),
  time_mode = "within_year",
  consensus_rule = 2,
  output_profile = "audit"
)

# 3) Advanced overrides for rates
detect_outliers(
  data = malaria_data,
  column = "positivity_rate",
  date = "date",
  value_type = "rate",
  strictness = "advanced",
  sd_multiplier = 2.5,
  mad_multiplier = 7,
  iqr_multiplier = 1.8,
  min_n = 10,
  output_profile = "audit"
)

# 4) With facility activeness filtering
detect_outliers(
  data = malaria_data,
  column = "conf",
  date = "date",
  admin_level = c("adm1", "adm2"),
  time_mode = "across_time",
  key_indicators_hf = c("allout", "test", "conf")
)

# 5) With binary activeness classification
detect_outliers(
  data = malaria_data,
  column = "conf",
  date = "date",
  admin_level = c("adm1", "adm2"),
  time_mode = "across_time",
  key_indicators_hf = c("allout", "test", "conf")
)

# 6) Seasonal comparison (same month across years)
# Compares all Januaries together, all Februaries together, etc.
# Useful for detecting values unusual for a specific month
detect_outliers(
  data = malaria_data,
  column = "conf",
  date = "date",
  admin_level = c("adm1", "adm2"),
  time_mode = "seasonal"
)
} # }
```
