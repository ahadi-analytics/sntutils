# Save Data and Shapefiles to Various File Formats

This function provides a unified interface for saving data to various
file formats supported by the
[`rio::export`](http://gesistsa.github.io/rio/reference/export.md)
function. Additionally, it supports fast binary format `.qs2` via the
optional `qs2` package. The format is automatically detected from the
file extension to simplify the saving process.

## Usage

``` r
write(data, file_path, ...)
```

## Arguments

- data:

  The dataset to be saved

- file_path:

  Character string specifying the path to the output file.

- ...:

  Additional arguments to be passed to the underlying write functions.
  These arguments are specific to the file format being saved. Please
  refer to the documentation of each package used for more information.

## Value

No return value, called for side effects.

## Examples

``` r
# Create temporary account
tmpdir <- tempfile()
dir.create(tmpdir)

# Save a CSV file
write(mtcars, file_path = file.path(tmpdir, "file.csv"))

# Save an Excel file
write(mtcars, file_path = file.path(tmpdir, "file.xlsx"))

# Save a Stata DTA file
write(mtcars, file_path = file.path(tmpdir, "file.dta"))

# Save an RDS file
# write(mtcars, file_path = file.path(tmpdir, "file.rds"))

# Save a qs2 file (requires 'qs2')

# Save an RData file
write(list(mtcars = mtcars, iris = iris),
       file_path = file.path(tmpdir, "file.RData"))

# For saving shapefiles
# make example shape data
my_shp <-  sf::st_sfc(
  sf::st_point(c(43, 23))) |>
    cbind(mtcars[1, ]) |>
    sf::st_as_sf(crs = sf::st_crs(4326))

# save a shapefile
# write(my_shp, file_path = file.path(tmpdir, "file.shp"))

# Remove the temporary directory and its contents
unlink(tmpdir, recursive = TRUE)
```
