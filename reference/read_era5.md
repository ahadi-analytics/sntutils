# Read and Process ERA5 NetCDF Files to Tidy Data

Reads ERA5 NetCDF files and converts them to a tidy tibble format with
automatic country detection from bounding box coordinates.

## Usage

``` r
read_era5(
  nc_files,
  variables = NULL,
  bbox = NULL,
  country = NULL,
  convert_units = TRUE
)
```

## Arguments

- nc_files:

  Character vector. Paths to ERA5 NetCDF files to read.

- variables:

  Character vector. Variable names to extract. If NULL, extracts all
  variables.

- bbox:

  Numeric vector. Bounding box as `c(xmin, ymin, xmax, ymax)` for
  country detection.

- country:

  Character. Optional country name to use instead of auto-detection.

- convert_units:

  Logical. If `TRUE` (default), automatically convert units: Kelvin to
  Celsius for temperature, meters to millimeters for precipitation.

## Value

A tibble with columns:

- country:

  Country name (detected or provided)

- date:

  Date/datetime of observation

- year:

  Year

- month:

  Month

- variable:

  Climate variable name

- value:

  Variable value

- lon:

  Longitude

- lat:

  Latitude

- units:

  Variable units

## Examples

``` r
if (FALSE) { # \dontrun{
# Read downloaded ERA5 files
files <- list.files("era5_data", pattern = "\\.nc$", full.names = TRUE)

# Extract data with auto country detection
data <- read_era5(
  nc_files = files,
  variables = c("2m_temperature", "total_precipitation"),
  bbox = c(29.0, -4.5, 30.9, -2.3)
)

# Or specify country manually
data <- read_era5(
  nc_files = files,
  country = "Rwanda"
)
} # }
```
