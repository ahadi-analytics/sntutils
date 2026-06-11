# Automatically bin numeric data for choropleth maps

Selects an appropriate binning method based on data distribution and
returns bins with matching colors. Three methods are available:
head-tail breaks for highly skewed data, hybrid (quantile + tail) for
moderately skewed data, and pure quantile for roughly symmetric data.

## Usage

``` r
auto_bin(
  x,
  palette = "default",
  bin = 6,
  decimals = 2,
  round_to = 50,
  reverse = FALSE,
  labels = NULL,
  outlier_threshold = NULL,
  outlier_color = "#636363",
  outlier_label = NULL
)
```

## Arguments

- x:

  Numeric vector to bin.

- palette:

  Character. Either a preset name or a custom character vector of hex
  colors. Use
  [`list_palettes()`](https://ahadi-analytics.github.io/sntutils/reference/list_palettes.md)
  to see available presets.

- bin:

  Integer. Number of bins. Default is 6.

- decimals:

  Integer. Number of decimal places in labels. Default is 2.

- round_to:

  Numeric. Round break points to this increment. Default is 50. Set to
  NULL for raw values (useful for decimal data like rates).

- reverse:

  Logical. Reverse the color order? Default is FALSE.

- labels:

  Character vector. Optional custom bin labels. When provided, skips
  automatic binning and uses these labels with breaks parsed from label
  strings. Supports formats: "0–50", "50-100", "\>1000". Example:
  c("0–50", "50–100", "100–250", "250–450", "450–700", "700–1000",
  "\>1000")

- outlier_threshold:

  Numeric. Optional threshold to create a separate outlier bin for
  values above this threshold. Useful for metrics like TPR where values
  \>1 are unusual. Default is NULL (no outlier handling).

- outlier_color:

  Character. Hex color for the outlier bin. Only used when
  outlier_threshold is specified. Default is "#636363" (dark grey).

- outlier_label:

  Character. Optional custom label for the outlier bin. when NULL
  (default), uses the auto-generated format.

## Value

A list with:

- bins:

  Ordered factor of bin labels for each value in `x`

- colors:

  Named character vector mapping labels to colors

- counts:

  Data frame with bin labels and counts (n)

- method:

  Character. The binning method used: "headtail", "hybrid", "quantile",
  or "custom"

- diagnostics:

  List with prop_zero, skew_ratio, and tail_share

## Details

Method selection logic:

- **headtail**: prop_zero \> 0.1, skew_ratio \> 4, or tail_share \> 0.4

- **hybrid**: skew_ratio \> 2

- **quantile**: otherwise

- **custom**: when labels parameter is provided

Use
[`list_palettes()`](https://ahadi-analytics.github.io/sntutils/reference/list_palettes.md)
to see all available palette names.

## Examples

``` r
# Simulated malaria incidence data
set.seed(42)
incidence <- c(rep(0, 20), rgamma(80, shape = 2, rate = 0.01))

result <- auto_bin(incidence)
table(result$bins)
#> 
#>    0–50  50–100 100–200 200–250 250–350    >350 
#>      35      14      14      14      15       8 
result$method
#> [1] "headtail"
result$colors
#>      0–50    50–100   100–200   200–250   250–350      >350 
#> "#DEEBF7" "#B6D4E9" "#75B3D8" "#FB8969" "#E94534" "#A50F15" 

# Use named palette
auto_bin(incidence, palette = "byor")$colors
#>      0–50    50–100   100–200   200–250   250–350      >350 
#> "#084594" "#3E89C2" "#B3D2EA" "#FDC96A" "#ED4728" "#800026" 

# Use custom palette
auto_bin(incidence, palette = c("#ffffcc", "#a1dab4", "#41b6c4", "#225ea8"))
#> $bins
#>   [1] 0–50    0–50    0–50    0–50    0–50    0–50    0–50    0–50    0–50   
#>  [10] 0–50    0–50    0–50    0–50    0–50    0–50    0–50    0–50    0–50   
#>  [19] 0–50    0–50    250–350 50–100  100–200 0–50    100–200 50–100  >350   
#>  [28] 100–200 0–50    0–50    250–350 250–350 250–350 200–250 250–350 0–50   
#>  [37] 50–100  >350    >350    200–250 200–250 50–100  0–50    0–50    200–250
#>  [46] 250–350 250–350 50–100  50–100  50–100  200–250 50–100  100–200 50–100 
#>  [55] 100–200 0–50    200–250 0–50    200–250 100–200 200–250 50–100  >350   
#>  [64] 200–250 100–200 200–250 250–350 100–200 250–350 50–100  200–250 >350   
#>  [73] 50–100  100–200 >350    100–200 0–50    0–50    250–350 250–350 >350   
#>  [82] 0–50    250–350 200–250 0–50    200–250 100–200 0–50    100–200 0–50   
#>  [91] 100–200 0–50    250–350 >350    50–100  250–350 250–350 50–100  200–250
#> [100] 100–200
#> Levels: 0–50 < 50–100 < 100–200 < 200–250 < 250–350 < >350
#> 
#> $colors
#>      0–50    50–100   100–200   200–250   250–350      >350 
#> "#FFFFCC" "#C6E8BD" "#8DD2B7" "#54BDC0" "#3492B8" "#225EA8" 
#> 
#> $counts
#>       bin  n
#> 1    0–50 35
#> 2  50–100 14
#> 3 100–200 14
#> 4 200–250 14
#> 5 250–350 15
#> 6    >350  8
#> 
#> $method
#> [1] "headtail"
#> 
#> $diagnostics
#> $diagnostics$prop_zero
#> [1] 0.2
#> 
#> $diagnostics$skew_ratio
#> [1] 2.111871
#> 
#> $diagnostics$tail_share
#> [1] 0.2264725
#> 
#> 

# Reverse colors (high = light, low = dark)
auto_bin(incidence, reverse = TRUE)$colors
#>      0–50    50–100   100–200   200–250   250–350      >350 
#> "#A50F15" "#E94534" "#FB8969" "#75B3D8" "#B6D4E9" "#DEEBF7" 

# Use custom labels
custom_labels <- c("0–50", "50–100", "100–250", "250–450", "450–700", "700–1000", ">1000")
result <- auto_bin(incidence * 10, palette = "byor", labels = custom_labels)
table(result$bins)
#> 
#>    0–50  50–100 100–250 250–450 450–700  700–1K     >1K 
#>      20       0       1       3      13       7      56 
result$method  # Returns "custom"
#> [1] "custom"

# Handle outliers above threshold (useful for TPR)
set.seed(123)
tpr <- c(runif(80, 0.5, 0.95), runif(20, 1.05, 1.3))
result <- auto_bin(tpr, palette = "rdbu", bin = 5, outlier_threshold = 1.0)
table(result$bins)
#> 
#> 0.50–0.60 0.60–0.70 0.70–0.85 0.85–0.95     >1.00 
#>        20        20        20        20        20 
# Custom outlier label for data validation
result <- auto_bin(tpr, outlier_threshold = 1.0, outlier_label = "Suspect Values")
result$colors  # Last bin (>1.00) will be grey
#>      0.50–0.60      0.60–0.70      0.70–0.75      0.75–0.85      0.85–0.95 
#>      "#DEEBF7"      "#A8CEE4"      "#B3A0A3"      "#F35A40"      "#A50F15" 
#> Suspect Values 
#>      "#636363" 
```
