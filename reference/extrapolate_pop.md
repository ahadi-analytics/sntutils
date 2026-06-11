# Extrapolate Population Estimates for Target Years

Fills missing population values for specified years by applying
multipliers to the nearest available year within each location group.
Can handle multiple population columns simultaneously. Supports
extending forward beyond the latest data or backward before the earliest
data. Automatically calculates growth rates from existing data when
multipliers are not provided.

## Usage

``` r
extrapolate_pop(
  data,
  year_col,
  pop_cols,
  group_cols,
  years_to_extrap,
  multiplier = NULL
)
```

## Arguments

- data:

  A data frame containing population data with year, multiple population
  columns, and location columns.

- year_col:

  The name of the year column (unquoted or character).

- pop_cols:

  A character vector of population column names to extrapolate.

- group_cols:

  A character vector of grouping column names defining location.

- years_to_extrap:

  A vector of target years to extrapolate. Can be unnamed (e.g., c(2021,
  2022)) or named with specific multipliers (e.g., c(`2021` = 1.5,
  `2022` = 1.3)).

- multiplier:

  A single numeric multiplier to apply to all years when
  `years_to_extrap` is unnamed (e.g., 1.5). Can also be a named
  list/vector with multipliers for each population column. For
  year-specific multipliers, use a nested list structure like
  `list(pop_total = c('2021' = 1.03, '2022' = 1.025))`. If NULL and
  sufficient data exists, growth rates will be calculated automatically.

## Value

A data frame with updated population estimates for all specified
population columns and years.

## Examples

``` r

# Dummy data for 3 districts over 3 years with multiple population columns
dummy_data <- expand.grid(
 adm0 = "COUNTRYX",
 adm1 = c("RegionA", "RegionB"),
 adm2 = c("District1", "District2"),
 year = 2018:2020
) |>
 dplyr::mutate(
   adm3 = paste0(adm2, "_Subarea"),
   pop_total = sample(1000:5000, size = dplyr::n(), replace = TRUE),
   pop_0_11m = pop_total * 0.08,
   pop_0_4y = pop_total * 0.15,
   pop_u15 = pop_total * 0.45
 ) |>
 dplyr::arrange(adm0, adm1, adm2, year)

# Example with automatic growth rate calculation (no multiplier provided)
extrapolate_pop(
  data = dummy_data,
  year_col = "year",
  pop_cols = c("pop_total", "pop_0_11m", "pop_0_4y", "pop_u15"),
  group_cols = c("adm0", "adm1", "adm2", "adm3"),
  years_to_extrap = c(2021, 2022)
)
#> # A tibble: 20 × 9
#>    adm0     adm1    adm2      adm3     year pop_total pop_0_11m pop_0_4y pop_u15
#>    <fct>    <fct>   <fct>     <chr>   <int>     <dbl>     <dbl>    <dbl>   <dbl>
#>  1 COUNTRYX RegionA District1 Distri…  2018      3456      276.     518.   1555.
#>  2 COUNTRYX RegionA District1 Distri…  2019      3975      318      596.   1789.
#>  3 COUNTRYX RegionA District1 Distri…  2020      3338      267.     501.   1502.
#>  4 COUNTRYX RegionA District1 Distri…  2021      3866      309      580    1740 
#>  5 COUNTRYX RegionA District1 Distri…  2022      4477      358      672    2015 
#>  6 COUNTRYX RegionA District2 Distri…  2018      4349      348.     652.   1957.
#>  7 COUNTRYX RegionA District2 Distri…  2019      3584      287.     538.   1613.
#>  8 COUNTRYX RegionA District2 Distri…  2020      4951      396.     743.   2228.
#>  9 COUNTRYX RegionA District2 Distri…  2021      5734      459      860    2580 
#> 10 COUNTRYX RegionA District2 Distri…  2022      6641      532      996    2988 
#> 11 COUNTRYX RegionB District1 Distri…  2018      2331      186.     350.   1049.
#> 12 COUNTRYX RegionB District1 Distri…  2019      2005      160.     301.    902.
#> 13 COUNTRYX RegionB District1 Distri…  2020      2447      196.     367.   1101.
#> 14 COUNTRYX RegionB District1 Distri…  2021      2834      227      425    1275 
#> 15 COUNTRYX RegionB District1 Distri…  2022      3282      263      492    1477 
#> 16 COUNTRYX RegionB District2 Distri…  2018      2112      169.     317.    950.
#> 17 COUNTRYX RegionB District2 Distri…  2019      4030      322.     604.   1814.
#> 18 COUNTRYX RegionB District2 Distri…  2020      4357      349.     654.   1961.
#> 19 COUNTRYX RegionB District2 Distri…  2021      5046      404      757    2271 
#> 20 COUNTRYX RegionB District2 Distri…  2022      5844      468      877    2630 

# Example with same multiplier for all columns
extrapolate_pop(
  data = dummy_data,
  year_col = "year",
  pop_cols = c("pop_total", "pop_0_11m", "pop_0_4y", "pop_u15"),
  group_cols = c("adm0", "adm1", "adm2", "adm3"),
  years_to_extrap = c(2021, 2022),
  multiplier = 1.03
)
#> # A tibble: 20 × 9
#>    adm0     adm1    adm2      adm3     year pop_total pop_0_11m pop_0_4y pop_u15
#>    <fct>    <fct>   <fct>     <chr>   <int>     <dbl>     <dbl>    <dbl>   <dbl>
#>  1 COUNTRYX RegionA District1 Distri…  2018      3456      276.     518.   1555.
#>  2 COUNTRYX RegionA District1 Distri…  2019      3975      318      596.   1789.
#>  3 COUNTRYX RegionA District1 Distri…  2020      3338      267.     501.   1502.
#>  4 COUNTRYX RegionA District1 Distri…  2021      3438      275      516    1547 
#>  5 COUNTRYX RegionA District1 Distri…  2022      3541      283      531    1593 
#>  6 COUNTRYX RegionA District2 Distri…  2018      4349      348.     652.   1957.
#>  7 COUNTRYX RegionA District2 Distri…  2019      3584      287.     538.   1613.
#>  8 COUNTRYX RegionA District2 Distri…  2020      4951      396.     743.   2228.
#>  9 COUNTRYX RegionA District2 Distri…  2021      5100      408      765    2295 
#> 10 COUNTRYX RegionA District2 Distri…  2022      5253      420      788    2364 
#> 11 COUNTRYX RegionB District1 Distri…  2018      2331      186.     350.   1049.
#> 12 COUNTRYX RegionB District1 Distri…  2019      2005      160.     301.    902.
#> 13 COUNTRYX RegionB District1 Distri…  2020      2447      196.     367.   1101.
#> 14 COUNTRYX RegionB District1 Distri…  2021      2520      202      378    1134 
#> 15 COUNTRYX RegionB District1 Distri…  2022      2596      208      389    1168 
#> 16 COUNTRYX RegionB District2 Distri…  2018      2112      169.     317.    950.
#> 17 COUNTRYX RegionB District2 Distri…  2019      4030      322.     604.   1814.
#> 18 COUNTRYX RegionB District2 Distri…  2020      4357      349.     654.   1961.
#> 19 COUNTRYX RegionB District2 Distri…  2021      4488      359      673    2019 
#> 20 COUNTRYX RegionB District2 Distri…  2022      4623      370      693    2080 

# Example with different multipliers for each column
extrapolate_pop(
  data = dummy_data,
  year_col = "year",
  pop_cols = c("pop_total", "pop_0_11m", "pop_0_4y", "pop_u15"),
  group_cols = c("adm0", "adm1", "adm2", "adm3"),
  years_to_extrap = c(2021, 2022),
  multiplier = list(
    pop_total = 1.025,
    pop_0_11m = 1.030,
    pop_0_4y = 1.028,
    pop_u15 = 1.020
  )
)
#> # A tibble: 20 × 9
#>    adm0     adm1    adm2      adm3     year pop_total pop_0_11m pop_0_4y pop_u15
#>    <fct>    <fct>   <fct>     <chr>   <int>     <dbl>     <dbl>    <dbl>   <dbl>
#>  1 COUNTRYX RegionA District1 Distri…  2018      3456      276.     518.   1555.
#>  2 COUNTRYX RegionA District1 Distri…  2019      3975      318      596.   1789.
#>  3 COUNTRYX RegionA District1 Distri…  2020      3338      267.     501.   1502.
#>  4 COUNTRYX RegionA District1 Distri…  2021      3421      275      515    1532 
#>  5 COUNTRYX RegionA District1 Distri…  2022      3507      283      529    1563 
#>  6 COUNTRYX RegionA District2 Distri…  2018      4349      348.     652.   1957.
#>  7 COUNTRYX RegionA District2 Distri…  2019      3584      287.     538.   1613.
#>  8 COUNTRYX RegionA District2 Distri…  2020      4951      396.     743.   2228.
#>  9 COUNTRYX RegionA District2 Distri…  2021      5075      408      763    2273 
#> 10 COUNTRYX RegionA District2 Distri…  2022      5202      420      784    2318 
#> 11 COUNTRYX RegionB District1 Distri…  2018      2331      186.     350.   1049.
#> 12 COUNTRYX RegionB District1 Distri…  2019      2005      160.     301.    902.
#> 13 COUNTRYX RegionB District1 Distri…  2020      2447      196.     367.   1101.
#> 14 COUNTRYX RegionB District1 Distri…  2021      2508      202      377    1123 
#> 15 COUNTRYX RegionB District1 Distri…  2022      2571      208      388    1145 
#> 16 COUNTRYX RegionB District2 Distri…  2018      2112      169.     317.    950.
#> 17 COUNTRYX RegionB District2 Distri…  2019      4030      322.     604.   1814.
#> 18 COUNTRYX RegionB District2 Distri…  2020      4357      349.     654.   1961.
#> 19 COUNTRYX RegionB District2 Distri…  2021      4466      359      672    2000 
#> 20 COUNTRYX RegionB District2 Distri…  2022      4578      370      691    2040 

# Example with year-specific multipliers for each column
extrapolate_pop(
  data = dummy_data,
  year_col = "year",
  pop_cols = c("pop_total", "pop_0_11m"),
  group_cols = c("adm0", "adm1", "adm2", "adm3"),
  years_to_extrap = c(2021, 2022),
  multiplier = list(
    pop_total = c(`2021` = 1.03, `2022` = 1.025),
    pop_0_11m = c(`2021` = 1.035, `2022` = 1.030)
  )
)
#> # A tibble: 20 × 9
#>    adm0     adm1    adm2      adm3     year pop_total pop_0_11m pop_0_4y pop_u15
#>    <fct>    <fct>   <fct>     <chr>   <int>     <dbl>     <dbl>    <dbl>   <dbl>
#>  1 COUNTRYX RegionA District1 Distri…  2018      3456      276.     518.   1555.
#>  2 COUNTRYX RegionA District1 Distri…  2019      3975      318      596.   1789.
#>  3 COUNTRYX RegionA District1 Distri…  2020      3338      267.     501.   1502.
#>  4 COUNTRYX RegionA District1 Distri…  2021      3438      276       NA      NA 
#>  5 COUNTRYX RegionA District1 Distri…  2022      3524      284       NA      NA 
#>  6 COUNTRYX RegionA District2 Distri…  2018      4349      348.     652.   1957.
#>  7 COUNTRYX RegionA District2 Distri…  2019      3584      287.     538.   1613.
#>  8 COUNTRYX RegionA District2 Distri…  2020      4951      396.     743.   2228.
#>  9 COUNTRYX RegionA District2 Distri…  2021      5100      410       NA      NA 
#> 10 COUNTRYX RegionA District2 Distri…  2022      5228      422       NA      NA 
#> 11 COUNTRYX RegionB District1 Distri…  2018      2331      186.     350.   1049.
#> 12 COUNTRYX RegionB District1 Distri…  2019      2005      160.     301.    902.
#> 13 COUNTRYX RegionB District1 Distri…  2020      2447      196.     367.   1101.
#> 14 COUNTRYX RegionB District1 Distri…  2021      2520      203       NA      NA 
#> 15 COUNTRYX RegionB District1 Distri…  2022      2583      209       NA      NA 
#> 16 COUNTRYX RegionB District2 Distri…  2018      2112      169.     317.    950.
#> 17 COUNTRYX RegionB District2 Distri…  2019      4030      322.     604.   1814.
#> 18 COUNTRYX RegionB District2 Distri…  2020      4357      349.     654.   1961.
#> 19 COUNTRYX RegionB District2 Distri…  2021      4488      361       NA      NA 
#> 20 COUNTRYX RegionB District2 Distri…  2022      4600      372       NA      NA 
```
