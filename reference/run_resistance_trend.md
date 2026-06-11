# Generate insecticide resistance trend scenarios

Extends mortality time series beyond a specified year using one of three
projection methods: constant trend (flat-line), continued trend (model
extrapolation), or hybrid trend (average of both).

Supports flexible grouping and can handle any IR metric column name.

## Usage

``` r
run_resistance_trend(
  df_IR,
  year_cut = 2025,
  n_years_to_add = 5,
  continue_trend_start_year = 2010,
  model_type = "unchanged",
  trendmethod = c("constantTrend", "continuedTrend", "hybridTrend"),
  group_cols = c("adm0", "adm1"),
  value_col = "mean_ir"
)
```

## Arguments

- df_IR:

  Data frame containing insecticide resistance data with grouping
  columns, year column, and value column.

- year_cut:

  Numeric. Last year of observed data. Projections start from
  `year_cut + 1`. Default is `2025`.

- n_years_to_add:

  Numeric. Number of years to project forward. Default is `5`.

- continue_trend_start_year:

  Numeric. First year to include in model fitting for continued trend.
  Default is `2010`.

- model_type:

  Character scalar. Regression family for continued trend. One of
  `"gaussian"`, `"binomial"`, `"quasibinomial"`, `"lm"`, `"gam"`, or
  `"unchanged"`. If `"unchanged"`, uses existing data values directly
  without refitting. Default is `"unchanged"`.

- trendmethod:

  Character scalar. Projection method: `"constantTrend"` (flat-line from
  last observed value), `"continuedTrend"` (model extrapolation), or
  `"hybridTrend"` (average of constant and continued). Default is
  `"constantTrend"`.

- group_cols:

  Character vector. Names of grouping columns (e.g., administrative
  units, insecticides). Default is `c("adm0", "adm1")`.

- value_col:

  Character scalar. Name of the mortality/resistance value column.
  Default is `"mean_ir"`.

## Value

A data frame with grouping columns, `year`, `trendmethod`, and the value
column specified in `value_col`.

## Examples

``` r
if (FALSE) { # \dontrun{
ir_data <- data.frame(
  adm0 = "BDI",
  adm1 = rep(c("Province A", "Province B"), each = 16),
  year = rep(2010:2025, 2),
  mean_ir = runif(32, 0.6, 0.95)
)

constant_proj <- run_resistance_trend(
  df_IR = ir_data,
  year_cut = 2025,
  n_years_to_add = 5,
  trendmethod = "constantTrend"
)

hybrid_proj <- run_resistance_trend(
  df_IR = ir_data,
  year_cut = 2025,
  trendmethod = "hybridTrend"
)
} # }
```
