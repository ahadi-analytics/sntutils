# Calculate and report geo-naming match statistics

Compares entries in a dataset against a lookup across specified admin
levels (e.g., country, province/state/region, district, subdistrict,
settlement) and reports match statistics to the console.

## Usage

``` r
calculate_match_stats(
  data,
  lookup_data,
  level0 = NULL,
  level1 = NULL,
  level2 = NULL,
  level3 = NULL,
  level4 = NULL
)
```

## Arguments

- data:

  A data frame containing the target data to be matched.

- lookup_data:

  A data frame serving as the reference for matching.

- level0:

  Column name (country) present in both `data` and `lookup_data`.

- level1:

  Column name (province/state/region) present in both datasets.

- level2:

  Column name (district) present in both datasets.

- level3:

  Column name (subdistrict) present in both datasets.

- level4:

  Column name (settlement) present in both datasets.

## Value

Invisibly returns `NULL`. Output is produced via `cli` messages.

## Details

- Input columns supplied via `level*` are normalized to lower case
  before matching.

- *Base N* for each side is the count of **unique hierarchical names**
  formed from the supplied levels (e.g., `level0_level1_...`). Per-level
  rows show matches out of each side's Base N.

- Console output includes:

  1.  a two-column summary (Target vs Lookup as base N),

  2.  side-level completeness messages (success/info/warning),

  3.  a per-level report of **missing names** (NA or empty strings) on
      either side. These missing names are **not included in N** only if
      the implementation drops them before counting (see Note below).

## Note

If you want missing names (NA/empty) **excluded from Base N**, ensure
the key-building step drops them before counting (see example patch
below).

## Examples

``` r
# minimal runnable example (toy data)
data <- data.frame(
  country = c("Kenya", "Kenya", "Uganda"),
  district = c("Nairobi", "Kisumu", "Kampala")
)
lookup <- data.frame(
  country = c("Kenya", "Uganda"),
  district = c("Nairobi", "Kampala")
)
calculate_match_stats(
  data, lookup, level0 = "country", level2 = "district"
)
#> 
#> ── ℹ Match Summary ─────────────────────────────────────────────────────────────
#> 
#> ℹ Data has names not in lookup.
#> 
#> Target data as base N                                                       
#> • country (level0): 2 out of 2 matched                                      
#> • district (level2): 2 out of 3 matched                                     
#> Lookup data as base N                                                       
#> • country (level0): 2 out of 2 matched                                      
#> • district (level2): 2 out of 2 matched                                     
```
