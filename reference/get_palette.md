# Get a color palette

Returns a color palette by name or passes through custom colors. When
`n` is specified, interpolates to return exactly that many colors.

## Usage

``` r
get_palette(palette, n = NULL)
```

## Arguments

- palette:

  Character. Either a preset name or a custom character vector of hex
  colors. Use
  [`list_palettes()`](https://ahadi-analytics.github.io/sntutils/reference/list_palettes.md)
  to see available presets.

- n:

  Integer. Number of colors to return. If NULL (default), returns the
  full base palette.

## Value

Character vector of hex colors.

## Details

Available palettes (use
[`list_palettes()`](https://ahadi-analytics.github.io/sntutils/reference/list_palettes.md)
to see all):

**Custom:** default, byor, gyor

**Sequential (ColorBrewer):** ylord, ylgnbu, blues, greens, reds,
oranges, purples, bupu, orrd

**Diverging (ColorBrewer):** rdylgn, rdbu, spectral, piyg

**Viridis-style:** viridis, magma, plasma

**Two-color:** redblue, bluered, redgreen, greenred, yellowred,
yellowblue, whiteblue, whitered, whitegreen

## Examples

``` r
# See available palettes
list_palettes()
#>  [1] "default"    "byor"       "gyor"       "ylord"      "ylgnbu"    
#>  [6] "blues"      "greens"     "reds"       "oranges"    "purples"   
#> [11] "bupu"       "orrd"       "rdylgn"     "rdbu"       "spectral"  
#> [16] "piyg"       "viridis"    "magma"      "plasma"     "redblue"   
#> [21] "bluered"    "redgreen"   "greenred"   "yellowred"  "yellowblue"
#> [26] "whiteblue"  "whitered"   "whitegreen"

# Get full palette
get_palette("blues")
#> [1] "#f7fbff" "#deebf7" "#c6dbef" "#9ecae1" "#6baed6" "#4292c6" "#2171b5"
#> [8] "#084594"

# Get specific number of colors
get_palette("ylord", n = 5)
#> [1] "#FFFFCC" "#FEDE80" "#FD9F44" "#F54026" "#B10026"
get_palette("viridis", n = 7)
#> [1] "#440154" "#433980" "#31688E" "#22908B" "#35B779" "#91D641" "#FDE725"

# Use custom colors
get_palette(c("#ffffcc", "#41b6c4", "#225ea8"), n = 10)
#>  [1] "#FFFFCC" "#D4EECA" "#AADEC8" "#80CEC6" "#56BEC4" "#3DACC0" "#3698BA"
#>  [8] "#2F85B4" "#2871AE" "#225EA8"
```
