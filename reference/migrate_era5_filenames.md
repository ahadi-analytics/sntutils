# Migrate ERA5 Filenames to New Format

Renames ERA5 NetCDF files downloaded with the old naming scheme (without
variable names) to the new format that includes variable names in the
filename. This prevents conflicts when downloading different variables
for the same time period.

## Usage

``` r
migrate_era5_filenames(dir = "era5_data", dry_run = TRUE)
```

## Arguments

- dir:

  Character. Directory containing ERA5 NetCDF files. Default is
  "era5_data".

- dry_run:

  Logical. If TRUE, shows what would be renamed without actually
  renaming files. Default is TRUE for safety.

## Value

A tibble showing old and new filenames.

## Examples

``` r
if (FALSE) { # \dontrun{
# Preview what would be renamed
migrate_era5_filenames(dir = "era5_data", dry_run = TRUE)

# Actually rename the files
migrate_era5_filenames(dir = "era5_data", dry_run = FALSE)
} # }
```
