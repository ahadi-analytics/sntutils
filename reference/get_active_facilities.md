# Get active facilities from a dataset

This helper function identifies and returns only active facilities from
a dataset based on their reporting patterns. It's a convenience wrapper
around
[`classify_facility_activity`](https://ahadi-analytics.github.io/sntutils/reference/classify_facility_activity.md)
that filters the data to include only facilities classified as "Active".

## Usage

``` r
get_active_facilities(
  data,
  hf_col,
  date_col = "date",
  key_indicators = c("allout", "conf", "test", "treat", "pres"),
  method = 3,
  nonreport_window = 6,
  reporting_rule = "any_non_na",
  return_summary = FALSE
)
```

## Arguments

- data:

  A data frame containing health facility data

- hf_col:

  Character. Name of the column containing unique facility IDs

- date_col:

  Character. Name of the date column. Default is "date"

- key_indicators:

  Character vector of indicator columns used to determine reporting
  activity. Default is c("allout", "conf", "test", "treat", "pres")

- method:

  Numeric or character. Classification method (1, 2, 3, "method1",
  "method2", "method3"). Default is 3 (dynamic activation/inactivation)

- nonreport_window:

  Integer. Number of consecutive non-reporting periods before a facility
  is considered inactive (for method 3). Default is 6

- reporting_rule:

  Character. What counts as reporting: "any_non_na" (default, 0 counts
  as reported) or "positive_only" (requires \>0)

- return_summary:

  Logical. If TRUE, returns a summary tibble instead of filtered data.
  Default is FALSE

## Value

If `return_summary = FALSE` (default): Returns the input data frame
filtered to include only rows for active facilities, with an added
`activity_status` column.

If `return_summary = TRUE`: Returns a summary tibble with columns:

- `n_total`: Total number of unique facilities

- `n_active`: Number of active facilities

- `n_inactive`: Number of inactive facilities

- `pct_active`: Percentage of facilities that are active

- `pct_inactive`: Percentage of facilities that are inactive

## Details

This function is particularly useful for:

- Pre-filtering data before calculating reporting rates

- Understanding the proportion of operational facilities

- Ensuring denominators only include facilities that should be reporting

The function uses
[`classify_facility_activity`](https://ahadi-analytics.github.io/sntutils/reference/classify_facility_activity.md)
internally to determine facility status based on reporting patterns in
the key indicators.

## See also

[`classify_facility_activity`](https://ahadi-analytics.github.io/sntutils/reference/classify_facility_activity.md),
[`calculate_reporting_metrics`](https://ahadi-analytics.github.io/sntutils/reference/calculate_reporting_metrics.md)

## Examples

``` r
# Create sample data
sample_data <- data.frame(
  date = rep(as.Date(c("2024-01-01", "2024-02-01", "2024-03-01")), each = 4),
  facility_id = rep(c("HF001", "HF002", "HF003", "HF004"), 3),
  district = rep(c("North", "North", "South", "South"), 3),
  allout = c(
    100, 150, 0, NA,    # Month 1: HF001 and HF002 active,
                        #  HF003 reports 0, HF004 never reports
    110, 160, 0, NA,    # Month 2
    105, 155, NA, NA    # Month 3: HF003 stops reporting
  ),
  conf = c(
    10, 15, 0, NA,
    11, 16, 0, NA,
    10, 15, NA, NA
  )
)

# Get only active facilities
active_only <- get_active_facilities(
  data = sample_data,
  hf_col = "facility_id",
  date_col = "date",
  key_indicators = c("allout", "conf")
)
#> ℹ Facility activity classification:
#>   3 of 4 facilities are active (75%)
#>   1 facilities are inactive (25%)
#> ✔ Returning data filtered to 3 active facilities (of 4 total)

# Get summary of facility activity
activity_summary <- get_active_facilities(
  data = sample_data,
  hf_col = "facility_id",
  date_col = "date",
  key_indicators = c("allout", "conf"),
  return_summary = TRUE
)
#> ℹ Facility activity classification:
#>   3 of 4 facilities are active (75%)
#>   1 facilities are inactive (25%)
```
