# Get Metadata from ERA5 NetCDF File

Extracts comprehensive metadata from an ERA5 NetCDF file including
resolution, variables, units, dimensions, and global attributes.

## Usage

``` r
get_era5_metadata(nc_file)
```

## Arguments

- nc_file:

  Character. Path to ERA5 NetCDF file.

## Value

A list containing:

- resolution:

  Spatial resolution in degrees (lon, lat)

- extent:

  Bounding box (lon_min, lon_max, lat_min, lat_max)

- dimensions:

  List of dimension information

- variables:

  Tibble of variable metadata (name, long_name, units, dimensions)

- global_attrs:

  List of global attributes

## Examples

``` r
if (FALSE) { # \dontrun{
file <- "era5_data/monthly_single_levels_era5_202001.nc"
metadata <- get_era5_metadata(file)
print(metadata$resolution)
print(metadata$variables)
} # }
```
