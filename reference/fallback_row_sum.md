# Smart row-wise sum with missing data handling and type preservation

Computes the row-wise sum across multiple input vectors, while allowing
control over how many non-missing values must be present for a valid
result. If the number of non-`NA` values in a row is below
`min_present`, the result is set to `NA` of the appropriate type.

## Usage

``` r
fallback_row_sum(..., min_present = 1, .keep_zero_as_zero = TRUE)
```

## Arguments

- ...:

  Numeric or integer vectors of equal length to be summed row-wise.

- min_present:

  Integer. Minimum number of non-`NA` values required per row to return
  a sum. Rows with fewer than `min_present` non-`NA` values return
  `NA_integer_` if all inputs are integer, otherwise `NA_real_`.

- .keep_zero_as_zero:

  Logical. Currently unused, reserved for future development. Defaults
  to `TRUE`.

## Value

An integer or numeric vector of row-wise sums, depending on the input
types, with appropriate `NA` values where insufficient data are present.

## Details

The return type matches the inputs:

- If all inputs are integer vectors, the result is an integer vector
  with `NA_integer_` where insufficient data are present.

- Otherwise, the result is a numeric (double) vector with `NA_real_`
  where insufficient data are present.

## Examples

``` r
# all integer inputs -> integer output
x <- c(1L, 2L, NA_integer_)
y <- c(3L, NA_integer_, 4L)
fallback_row_sum(x, y)
#> [1] 4 2 4
typeof(fallback_row_sum(x, y))  # "integer"
#> [1] "integer"

# mixed integer and numeric inputs -> numeric output
z <- c(1, 2, NA)
fallback_row_sum(x, z)
#> [1]  2  4 NA
typeof(fallback_row_sum(x, z))  # "double"
#> [1] "double"

# using min_present to control NA behaviour
fallback_row_sum(c(1, NA), c(2, NA), min_present = 1)
#> [1]  3 NA
```
