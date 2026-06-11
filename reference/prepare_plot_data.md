# Prepare data for reporting rate or missing data visualization

This function processes health facility data to prepare it for
visualizing reporting rates or missing data patterns. It supports three
scenarios:

1.  reporting rate by time and admin unit (x + y, single var)

2.  missing rate by variable over time (x only, multiple vars)

3.  proportion of facilities reporting in each (x, y) group (needs hf)

## Usage

``` r
prepare_plot_data(
  data,
  x_var,
  y_var = NULL,
  vars_of_interest,
  by_facility = FALSE,
  hf_col = "hf_uid",
  reprate_col = NULL,
  use_reprate = TRUE,
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

  Original data frame

- x_var:

  Character. Time variable (e.g. yearmon)

- y_var:

  Character. Optional grouping variable (e.g. district)

- vars_of_interest:

  Character vector of variables to analyze

- by_facility:

  Logical. If TRUE, compute by facility

- hf_col:

  Character. Name of health facility ID column (required if by_facility)

- reprate_col:

  Character. Optional name of column containing pre-calculated reporting
  rates. If provided, these values will be used instead of calculating
  reporting rates. Default is NULL.

- use_reprate:

  Logical. If TRUE, return reporting rate. Else, missing rate

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
  reporting rates. Only applies to facility-level analysis (when
  by_facility = TRUE).

- weighting:

  Logical. Whether to use weighted reporting rates. When TRUE,
  facilities are weighted by their typical size, giving more importance
  to larger facilities in the overall reporting rate calculation.
  Default is FALSE.

- weight_var:

  Character. Name of the variable to use as proxy for facility size
  (e.g., "allout" for total outpatients). Required when weighting is
  TRUE. Default is NULL.

- weight_window:

  Integer. Number of periods for rolling window to calculate typical
  facility size. Default is 12.

- exclude_current_x:

  Logical. Whether to exclude current period when calculating weights.
  If TRUE, prevents current reporting from influencing its own weight.
  Default is TRUE.

- cold_start:

  Character. Method for handling facilities with insufficient history.
  Options: "median_within_y" (default) or "median_global".

## Value

A list with plot_data and plotting metadata

## Examples

``` r
# Sample data
hf_data <- data.frame(
  month = rep(as.Date(c("2024-01-01", "2024-02-01", "2024-03-01")), each = 10),
  district = rep(c("North", "South"), each = 5, times = 3),
  facility_id = rep(1:5, times = 6),
  malaria = c(
    10, 0, 15, NA, 8, 12, 0, NA, 7, 9,
    11, 0, 14, 6, NA, 13, 8, 10, 0, 12,
    9, 7, 0, 11, 14, 8, NA, 12, 10, 15
  ),
  pneumonia = c(
    5, 0, NA, 7, 3, 6, 0, 4, NA, 2,
    8, 0, 6, NA, 4, 7, 3, 0, 5, 6,
    4, 0, 7, 5, NA, 6, 0, 8, 4, 3
  )
)

# Scenario 1: Reporting rate by district and month for each variable
district_plot_data <- prepare_plot_data(
  data = hf_data,
  x_var = "month",
  y_var = "district",
  vars_of_interest = c("malaria", "pneumonia"),
  use_reprate = TRUE
)
#> Warning: ! No facility identification column (hf_col) provided.
#> ℹ Unable to exclude inactive facilities from denominators.
#> ℹ This may result in inaccurate rates if inactive facilities are present.
#> ℹ Consider providing 'hf_col' parameter to enable facility activity
#>   classification.
# Returns list with plot_data and metadata for district-level visualization

# Scenario 2: Missing rate of variables over time (months only)
variable_plot_data <- prepare_plot_data(
  data = hf_data,
  x_var = "month",
  vars_of_interest = c("malaria", "pneumonia"),
  use_reprate = FALSE
)
#> Warning: ! No facility identification column (hf_col) provided.
#> ℹ Unable to exclude inactive facilities from denominators.
#> ℹ This may result in inaccurate rates if inactive facilities are present.
#> ℹ Consider providing 'hf_col' and 'y_var' parameters to enable facility
#>   activity classification.
# Returns list with plot_data and metadata for variable-level visualization

# Scenario 3: Proportion of facilities reporting in each district by month
facility_plot_data <- prepare_plot_data(
  data = hf_data,
  x_var = "month",
  y_var = "district",
  vars_of_interest = "malaria",
  by_facility = TRUE,
  hf_col = "facility_id"
)
#> ℹ No key indicators provided - including all facilities in denominators
#> ℹ Consider providing 'key_indicators' to exclude inactive facilities
# Returns list with plot_data and metadata for facility-level visualization
```
