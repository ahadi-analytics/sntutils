# Download WHO Administrative Boundaries with Partial Update

Downloads administrative boundaries from WHO's ArcGIS services for
specified countries and administrative levels. The function supports
incremental updates, downloading only missing data when appending to
existing files. It automatically converts date fields from milliseconds
to proper Date format and can filter for current boundaries or include
historical boundaries.

## Usage

``` r
download_shapefile(
  country_codes,
  admin_level = "ADM2",
  latest = TRUE,
  dest_path = NULL
)
```

## Arguments

- country_codes:

  Character vector of ISO3 country codes (e.g. c("KEN","UGA")).

- admin_level:

  Character string specifying administrative level ("ADM0", "ADM1", or
  "ADM2"). Default is "ADM2".

- latest:

  Logical. If TRUE (default), returns only the latest/active boundaries.
  If FALSE, returns all historical boundaries.

- dest_path:

  File path where data is saved. If NULL (default), data is returned
  without saving to disk. When `latest = TRUE`, the output filename will
  include the suffix `_latest` before the extension.

## Value

An `sf` object containing administrative boundaries with the following
columns:

- adm0_code:

  ISO3 country code

- adm0:

  Country name

- adm1:

  Admin level 1 name (regions/states) - only for ADM1 and ADM2

- adm2:

  Admin level 2 name (districts) - only for ADM2

- start_date:

  Date when the boundary became effective

- end_date:

  Date when the boundary ceased (9999-12-31 for current)

- geometry:

  Spatial geometry column

If `dest_path` is provided, the data is also saved to that location.

## Details

The function retrieves boundary data from WHO's Global Administrative
Boundaries service hosted on ArcGIS. It uses the httr2 package for API
requests and sf for spatial data handling. Date fields are automatically
converted from milliseconds since epoch to R Date objects.

When `latest = TRUE` (default), only active boundaries are returned
(those with ENDDATE of 9999-12-31). When `latest = FALSE`, all
historical boundaries are included, useful for tracking boundary changes
over time (e.g., country splits or administrative reorganizations).

The incremental update feature checks existing files for already
downloaded countries and only fetches missing ones, making it efficient
for building large boundary datasets.

## Note

Requires the `httr2` package for API requests. Install with:
`install.packages("httr2")`

## See also

[`st_read`](https://r-spatial.github.io/sf/reference/st_read.html) for
reading spatial data,
[`st_write`](https://r-spatial.github.io/sf/reference/st_write.html) for
writing spatial data. WHO boundary services:
<https://services.arcgis.com/5T5nSi527N4F7luB/ArcGIS/rest/services>

## Examples

``` r

if (FALSE) { # \dontrun{
tf <- file.path(tempdir(), "test_env")

# Download latest ADM2 boundaries without saving to disk
kenya_districts <- download_shapefile(
  country_codes = "KEN",
  admin_level = "ADM2",
  latest = TRUE  # Only current boundaries
)

# Download all historical boundaries (e.g., to track boundary changes)
sudan_historical <- download_shapefile(
  country_codes = "SDN",
  admin_level = "ADM0",
  latest = FALSE  # Include historical boundaries
)
# This would show Sudan before and after South Sudan independence
# (2011-07-11)

# Download multiple countries and save to directory
# File will be saved as "who_shapefile_ken_uga_tza_adm0_latest.gpkg" (latest=TRUE)
africa_countries <- download_shapefile(
  country_codes = c("KEN", "UGA", "TZA"),
  admin_level = "ADM0",
  dest_path = tf
)

# Incremental update - add more countries to existing file
# Will automatically use the same filename for the same countries+admin combo
download_shapefile(
  country_codes = c("KEN", "UGA", "TZA", "RWA", "BDI"),
  admin_level = "ADM0",
  dest_path = tf
)
# Creates "who_shapefile_ken_uga_tza_rwa_bdi_adm0_latest.gpkg" (latest=TRUE)

# Different admin levels saved to different files automatically
# Saves as "who_shapefile_ken_adm1_latest.gpkg" (latest=TRUE)
download_shapefile("KEN", "ADM1", dest_path = tf)

# Saves as "who_shapefile_ken_adm2_latest.gpkg" (latest=TRUE)
download_shapefile("KEN", "ADM2", dest_path = tf)

# Multiple countries at ADM2 level
# Saves as "who_shapefile_com_syc_mus_adm2_latest.gpkg" (latest=TRUE)
download_shapefile(c("COM", "SYC", "MUS"), "ADM2", dest_path = tf)
} # }
```
