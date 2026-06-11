# Fallback Absolute Difference Between Two Vectors (type-preserving)

Computes the difference between two numeric or integer vectors
element-wise. If both values are present, returns
`pmax(abs(col1 - col2), minimum)`. If only one value is present, returns
the non-missing value or `minimum`, whichever is greater. If both are
missing, returns `NA` of the appropriate type.

## Usage

``` r
fallback_diff(col1, col2, minimum = 0)
```

## Arguments

- col1:

  Numeric or integer vector. First input column.

- col2:

  Numeric or integer vector. Second input column.

- minimum:

  Numeric or integer scalar. Minimum allowable value for the result
  (default is `0`).

## Value

An integer or numeric vector with the same length as the inputs. Each
element is:

- the absolute difference between `col1` and `col2`, if both are
  non-missing,

- the non-missing value if only one is present,

- `NA_integer_` or `NA_real_` if both are missing.

In all cases, the result is constrained to be no less than `minimum`.

## Details

The return type matches the inputs:

- If both inputs are integer vectors (and `minimum` is an integer
  scalar), the result is integer with `NA_integer_` where both are
  missing.

- Otherwise, the result is numeric (double) with `NA_real_` where both
  are missing.

## Examples

``` r
fallback_diff(5, 3)        # 2
#> [1] 2
fallback_diff(NA, 4)       # 4
#> [1] 4
fallback_diff(7, NA)       # 7
#> [1] 7
fallback_diff(4, 9)        # 0
#> [1] 5
fallback_diff(NA, NA)      # NA_real_
#> [1] NA

xi <- c(5L, NA, 7L)
yi <- c(3L, 4L, NA_integer_)
fallback_diff(xi, yi)      # integer output
#> [1] 2 4 7
```
