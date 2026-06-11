# Fit regression model for insecticide resistance trends

Fits a regression model predicting mortality (y) from year (x) for a
single administrative unit or insecticide slice.

Supports multiple model families including GLM (gaussian, binomial,
quasibinomial), linear regression, and generalized additive models
(GAM).

## Usage

``` r
get_model(df, model_type = "quasibinomial")
```

## Arguments

- df:

  Data frame containing columns `x` (year) and `y` (mortality
  proportion).

- model_type:

  Character scalar. One of `"gaussian"`, `"binomial"`,
  `"quasibinomial"`, `"lm"`, or `"gam"`. Default is `"quasibinomial"`.

## Value

A fitted model object of class `glm`, `lm`, or `gam` depending on
`model_type`.

## Examples

``` r
if (FALSE) { # \dontrun{
trend_data <- data.frame(
  x = 2010:2025,
  y = c(0.95, 0.93, 0.91, 0.89, 0.87, 0.85, 0.83, 0.81,
        0.79, 0.77, 0.75, 0.73, 0.71, 0.69, 0.67, 0.65)
)

model <- get_model(trend_data, model_type = "quasibinomial")
summary(model)
} # }
```
