# List Available MODIS Products

Queries NASA's Common Metadata Repository (CMR) for MODIS products
available from the LPDAAC archive. Returns a tibble you can filter to
find the `product` value needed by
[`download_modis()`](https://ahadi-analytics.github.io/sntutils/reference/download_modis.md).

## Usage

``` r
modis_options(search = NULL)
```

## Arguments

- search:

  Character or `NULL`. Optional search term to filter results
  (case-insensitive). Searches product names and descriptions.

## Value

A tibble with columns `product`, `version`, and `description`.

## Details

Requires internet access.

## Examples

``` r
if (FALSE) { # \dontrun{
# list all MODIS products
modis_options()

# search for vegetation index products
modis_options("vegetation")

# search for land surface temperature
modis_options("LST")
} # }
```
