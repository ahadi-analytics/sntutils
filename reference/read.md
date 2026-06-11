# Read in Data and Shapefiles from Various File Formats

This function provides a unified interface for reading data from various
file formats supported by the
[`import`](http://gesistsa.github.io/rio/reference/import.md),
[`read_sf`](https://r-spatial.github.io/sf/reference/st_read.html),
[`read_excel`](https://readxl.tidyverse.org/reference/read_excel.html),
and [`read_yaml`](https://yaml.r-lib.org/reference/read_yaml.html)
packages. Additionally, it supports fast binary format `.qs2` via the
optional `qs2` package. The format is automatically detected from the
file extension to simplify the importing process.

## Usage

``` r
read(file_path, ...)
```

## Arguments

- file_path:

  Character string specifying the path to the input file or a URL
  pointing to the dataset.

- ...:

  Additional arguments to be passed to the underlying read functions.
  These arguments are specific to the file format being imported. Please
  refer to the documentation of each package used for more information.

## Value

A data frame or appropriate R object containing the imported data.

## See also

[`import`](http://gesistsa.github.io/rio/reference/import.md),
[`read_sf`](https://r-spatial.github.io/sf/reference/st_read.html),
[`read_excel`](https://readxl.tidyverse.org/reference/read_excel.html),
[`read_yaml`](https://yaml.r-lib.org/reference/read_yaml.html), and
[`qs2::qs_read`](https://rdrr.io/pkg/qs2/man/qs_read.html) (or
`qs2::qread`) for reading `.qs2` files.

## Examples

``` r
# Locate test data directory
path <- system.file("extdata",
  package = "sntutils"
)

# Import a CSV file
data_csv <- read(file_path = file.path(path, "test_data.csv"))

# Import an Excel file
data_excel <- read(file_path = file.path(path, "test_data.xlsx"))

# Import a Stata DTA file
data_dta <- read(file_path = file.path(path, "test_data.dta"))

# Import an RDS file
data_rds <- read(file_path = file.path(path, "test_data.rds"))

data_rdata <- read(file_path = file.path(path, "test_data.RData"))
#> Warning: Missing `trust` will be set to FALSE by default for RData in 2.0.0.

# Import an SPSS file
data_spss <- read(file_path = file.path(path, "test_data.sav"))

# Import an shapefiles file (GeoJSON/json)
data_geojson <- read(file_path = file.path(path, "test_data.GeoJSON"))

# Import a YAML file
data_yaml <- read(file_path = file.path(path, "var_tree.yml"))

# Or access snt_var_tree as package data
data(snt_var_tree, package = "sntutils")
```
