# Calculate reporting/missing rate and proportion of reporting facilities

This function calculates reporting metrics for health facility data
across three common scenarios. **IMPORTANT**: When facility
identification (`hf_col`) and `key_indicators` are provided, the
function excludes inactive facilities from denominators, ensuring that
missing rates reflect only the proportion of ACTIVE facilities that
failed to report, not all facilities in the dataset.

## Usage

``` r
calculate_reporting_metrics(
  data,
  vars_of_interest,
  x_var,
  y_var = NULL,
  hf_col = NULL,
  key_indicators = c("allout", "conf", "test", "treat", "pres"),
  method = 3,
  nonreport_window = 6,
  reporting_rule = "any_non_na",
  require_all = FALSE,
  weighting = FALSE,
  weight_var = NULL,
  weight_window = 12,
  exclude_current_x = TRUE,
  cold_start = "median_within_y"
)
```

## Arguments

- data:

  A data frame containing health facility data.

- vars_of_interest:

  Character vector of variable names to assess reporting (used for
  numerator).

- x_var:

  Character. Name of the primary grouping variable (e.g., time period).

- y_var:

  Character. Optional. Name of the second grouping variable (e.g.,
  district).

- hf_col:

  Character. Optional (defaults to NULL). Name of the column containing
  unique health facility IDs. When provided, enables facility-level
  analysis and filtering of inactive facilities (if key_indicators are
  specified). Can be used with or without y_var. Required when weighting
  = TRUE.

- key_indicators:

  Optional. Character vector of indicators used to define facility
  activity in scenario 1. Defaults to
  `c("allout", "conf", "test", "treat", "pres")`.

- method:

  Character or numeric. Classification method for facility activity
  status. Can be numeric (1, 2, 3) or character ("method1", "method2",
  "method3"). Defaults to 3. See
  [`classify_facility_activity`](https://ahadi-analytics.github.io/sntutils/reference/classify_facility_activity.md)
  for details.

- nonreport_window:

  Integer. Minimum number of consecutive non-reporting months to
  classify a facility as inactive in method 3. Defaults to 6.

- reporting_rule:

  Character. Defines what counts as reporting: `"any_non_na"` (default,
  counts NA as non-reporting, 0 counts as reported) or `"positive_only"`
  (requires \>0 value to count as reported).

- require_all:

  Logical. When TRUE and multiple vars_of_interest are provided,
  calculates the proportion of facilities reporting ALL variables
  (complete data). When FALSE (default), calculates per-variable
  reporting rates. Only applies to facility-level analysis (when hf_col
  is provided).

- weighting:

  Logical. Whether to use weighted reporting rates. When TRUE,
  facilities are weighted by their typical size, giving more importance
  to larger facilities in the overall reporting rate calculation. This
  provides a volume-adjusted measure of data completeness. Default is
  FALSE.

- weight_var:

  Character. Name of the variable to use as proxy for facility size
  (e.g., "allout" for total outpatients, "test" for tests done). This
  should be a count variable that reflects facility activity/size. If
  NULL and weighting is TRUE, will auto-select from allout, test, conf
  (in that order).

- weight_window:

  Integer. Number of periods for rolling window to calculate typical
  facility size. A facility's weight is based on its average size over
  the past weight_window periods. Larger windows provide more stable
  weights but may miss recent changes. Default is 12.

- exclude_current_x:

  Logical. Whether to exclude current period when calculating weights.
  If TRUE, prevents current reporting from influencing its own weight
  (avoids circularity). Default is TRUE.

- cold_start:

  Character. Method for handling facilities with insufficient history
  (\< weight_window periods). Options:

  - "median_within_y" (default): Uses median size of facilities within
    the same y_var group (e.g., same district)

  - "median_global": Uses median size across all facilities

## Value

A tibble with the number of reporting (`rep`) and expected (`exp`)
facilities or records, and the computed `reprate` and `missrate`.

**Understanding the metrics:**

- `exp` (expected): When `hf_col` is provided with `key_indicators`,
  this is the count of ACTIVE facilities only. Without `hf_col`, it's
  the count of all data rows (including inactive facilities).

- `rep` (reported): Count of facilities/rows that reported (non-NA
  values).

- `reprate`: Reporting rate (rep/exp). With proper facility
  identification, this is the percentage of ACTIVE facilities that
  reported.

- `missrate`: Missing rate (1 - reprate). With proper facility
  identification, this is the percentage of ACTIVE facilities that
  failed to report. **Without `hf_col`, this may be artificially high
  due to inactive facilities.**

If weighting is TRUE, additional columns are included:

- `reprate_w`: Weighted reporting rate (0-1)

- `missrate_w`: Weighted missing rate (0-1)

- `avg_<weight_var>`: Average raw value of the weight variable (e.g.,
  avg_allout)

- `min_<weight_var>`: Minimum raw value of the weight variable

- `max_<weight_var>`: Maximum raw value of the weight variable

## Details

1.  **Proportion of facilities reporting a single variable**

    - When `vars_of_interest` contains a single variable:

    - Calculates the proportion of active facilities (as defined by
      reporting on `key_indicators`) that reported the variable in a
      given period.

    - Requires `hf_col`. Can be used with or without `y_var`.

2.  **Per-variable facility reporting rates**

    - When `vars_of_interest` contains multiple variables and `hf_col`
      is provided:

    - Calculates reporting/missing rates for EACH variable separately,
      counting distinct facilities that reported each specific variable.

    - Uses facility-level analysis, filtering to active facilities only
      (if `key_indicators` specified).

    - Can be used with or without `y_var`.

3.  **Row-level reporting rates (without facility filtering)**

    - When `hf_col` is NOT provided:

    - Computes reporting/missing rates based on data rows, not
      facilities.

    - **WARNING**: This counts all data rows including inactive
      facilities, potentially inflating missing rates.

    - Use scenarios 1 or 2 (with `hf_col`) for accurate facility-based
      metrics.

### Weighted Reporting Rate Calculation

When `weighting = TRUE`, the function calculates volume-adjusted
reporting rates that give more importance to larger facilities. This is
useful when you want the overall reporting rate to reflect the
proportion of patient visits or services covered rather than just the
proportion of facilities.

The weighting algorithm works as follows:

1.  **Calculate typical facility size**: For each facility, compute the
    rolling mean of `weight_var` over the past `weight_window` periods.
    This represents the facility's typical size/volume.

2.  **Handle cold starts**: For facilities with insufficient history:

    - If `cold_start = "median_within_y"`: Use the median typical size
      of facilities in the same group (y_var)

    - If `cold_start = "median_global"`: Use the overall median typical
      size

3.  **Normalize weights**: Within each time period and group, weights
    are normalized to sum to 1. This ensures that larger facilities get
    proportionally more weight.

4.  **Calculate weighted rates**:

    - `reprate_w = sum(weight * reported) / sum(weight)`

    - `missrate_w = 1 - reprate_w`

### Example Interpretation

If a district has 10 facilities where:

- 3 large facilities (80% of patient volume) all report

- 7 small facilities (20% of patient volume) with only 4 reporting

Then:

- Unweighted reporting rate = 7/10 = 70%

- Weighted reporting rate \u2248 84% (reflecting that most patient
  volume is covered)

## Examples

``` r
# Example with dates instead of month names for compatibility
hf_data <- data.frame(
  month = rep(as.Date(c("2024-01-01", "2024-02-01", "2024-03-01")), each = 10),
  district = rep(c("North", "South"), each = 5, times = 3),
  facility_id = rep(1:5, times = 6),
  conf = c(
    10, 0, 15, NA, 8, 12, 0, NA, 7, 9,
    11, 0, 14, 6, NA, 13, 8, 10, 0, 12,
    9, 7, 0, 11, 14, 8, NA, 12, 10, 15
  ),
  pres = c(
    5, 0, NA, 7, 3, 6, 0, 4, NA, 2,
    8, 0, 6, NA, 4, 7, 3, 0, 5, 6,
    4, 0, 7, 5, NA, 6, 0, 8, 4, 3
  ),
  allout = c(
    5, 0, NA, 7, 3, 6, 0, 4, NA, 2,
    8, 0, 6, NA, 4, 7, 3, 0, 5, 6,
    4, 0, 7, 5, NA, 6, 0, 8, 4, 3
  )
)

# Scenario 1: Proportion of active facilities reporting
# any data (using numeric method)
calculate_reporting_metrics(
  data = hf_data,
  vars_of_interest = c("conf"),
  x_var = "month",
  y_var = "district",
  hf_col = "facility_id",
  key_indicators = c("allout", "conf", "pres"),
  method = 3  # Can also use "method3"
)
#> ℹ Facility activity classification:
#>   5 of 5 facilities are active (100%)
#>   0 facilities are inactive (0%)
#> ✔ Returning data filtered to 5 active facilities (of 5 total)
#> # A tibble: 6 × 6
#>   month      district   rep   exp reprate missrate
#>   <date>     <chr>    <int> <int>   <dbl>    <dbl>
#> 1 2024-01-01 North        4     5     0.8      0.2
#> 2 2024-01-01 South        4     5     0.8      0.2
#> 3 2024-02-01 North        4     5     0.8      0.2
#> 4 2024-02-01 South        5     5     1        0  
#> 5 2024-03-01 North        5     5     1        0  
#> 6 2024-03-01 South        4     5     0.8      0.2

# Scenario 2: Reporting rate by month and district
calculate_reporting_metrics(
  data = hf_data,
  vars_of_interest = c("conf"),
  x_var = "month",
  y_var = "district",
  hf_col = NULL
)
#> Warning: ! No facility identification column (hf_col) provided.
#> ℹ Unable to exclude inactive facilities from denominators.
#> ℹ This may result in inaccurate rates if inactive facilities are present.
#> ℹ Consider providing 'hf_col' parameter to enable facility activity
#>   classification.
#> # A tibble: 6 × 7
#>   month      district variable   exp   rep reprate missrate
#>   <date>     <chr>    <fct>    <int> <int>   <dbl>    <dbl>
#> 1 2024-01-01 North    conf         5     4     0.8      0.2
#> 2 2024-01-01 South    conf         5     4     0.8      0.2
#> 3 2024-02-01 North    conf         5     4     0.8      0.2
#> 4 2024-02-01 South    conf         5     5     1        0  
#> 5 2024-03-01 North    conf         5     5     1        0  
#> 6 2024-03-01 South    conf         5     4     0.8      0.2

# Scenario 3: Reporting trends over time
calculate_reporting_metrics(
  data = hf_data,
  vars_of_interest = c("conf"),
  x_var = "month",
  hf_col = NULL
)
#> Warning: ! No facility identification column (hf_col) provided.
#> ℹ Unable to exclude inactive facilities from denominators.
#> ℹ This may result in inaccurate rates if inactive facilities are present.
#> ℹ Consider providing 'hf_col' and 'y_var' parameters to enable facility
#>   activity classification.
#> # A tibble: 3 × 6
#>   month      variable   exp   rep reprate missrate
#>   <date>     <fct>    <int> <int>   <dbl>    <dbl>
#> 1 2024-01-01 conf        10     8     0.8      0.2
#> 2 2024-02-01 conf        10     9     0.9      0.1
#> 3 2024-03-01 conf        10     9     0.9      0.1


# Example with weighted reporting rate
# Create data with facilities of different sizes
weighted_data <- data.frame(
  month = rep(1:6, each = 5),
  district = rep("A", 30),
  hf_id = rep(c("Large1", "Large2", "Small1", "Small2", "Small3"), 6),
  # Large facilities see ~1000 patients, small ones ~100
  allout = c(
    # Month 1-3: Historical data for weight calculation
    1050, 980, 95, 110, 105,  # Month 1
    1100, 1020, 100, 98, 112,  # Month 2
    990, 1080, 105, 102, 108,  # Month 3
    # Month 4-6: Current periods
    1070, 1050, 98, 105, 110,  # Month 4
    1020, 990, 102, 108, 95,   # Month 5
    1100, 1030, 110, 100, 105  # Month 6
  ),
  # Reporting pattern: large facilities always report, small ones sporadic
  malaria = c(
    # Month 1-3
    50, 48, 5, NA, 6,    # Month 1
    55, 51, NA, 4, NA,   # Month 2
    49, 54, 6, NA, 5,    # Month 3
    # Month 4-6
    52, 53, NA, NA, 6,   # Month 4
    51, 49, 5, 6, NA,    # Month 5
    54, 52, NA, 5, NA    # Month 6
  )
)

# Compare unweighted vs weighted reporting rates
unweighted_result <- calculate_reporting_metrics(
  data = weighted_data,
  vars_of_interest = "malaria",
  x_var = "month",
  y_var = "district",
  hf_col = "hf_id",
  weighting = FALSE
)
#> Warning: ! Some key indicators not found in data
#> ℹ Using only: allout
#> ℹ Facility activity classification:
#>   5 of 10 facilities are active (50%)
#>   5 facilities are inactive (50%)
#> ✔ Returning data filtered to 5 active facilities (of 10 total)

weighted_result <- calculate_reporting_metrics(
  data = weighted_data,
  vars_of_interest = "malaria",
  x_var = "month",
  y_var = "district",
  hf_col = "hf_id",
  weighting = TRUE,
  weight_var = "allout",
  weight_window = 3,
  exclude_current_x = TRUE
)
#> Warning: ! >25% cold starts in 1 periods
#> ℹ Consider adjusting weight_window or data range
#> Warning: ! Some key indicators not found in data
#> ℹ Using only: allout
#> ℹ Facility activity classification:
#>   5 of 10 facilities are active (50%)
#>   5 facilities are inactive (50%)
#> ✔ Returning data filtered to 5 active facilities (of 10 total)

# Unweighted: counts facilities equally (e.g., 3/5 = 60%)
# Weighted: reflects patient volume covered (e.g., ~88% if large facilities report)
```
