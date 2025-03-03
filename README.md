# <img src="man/figures/logo.png" align="right" height="139" alt="" />
# snt

## What is snt?

`snt` is an R package developed by 'AHADI-Anlaytics'. It provides a suite of 
analytical tools to support the subnational tailoring (SNT) of malaria 
interventions, enabling evidence-based decision-making at the district level. 
It includes functions for data management and cleaning, malaria risk 
stratification, intervention targeting, geospatial analysis, and predictive 
modeling.

## :wrench: Installation
The package is yet to be available on Cran, but can be installed using `devtools` in R. The steps are as follows:

```r
# 1) Install devtools if you haven't already
install.packages("devtools")

# 2) Install the snt package from GitHub
devtools::install_github("ahadi-analytics/snt")
```

## Usage

### Data Management (Import, Export, and Manipulation)

#### `read` and `write` to Import and Export Data

Inspired by `rio`, the read function allows you to read data from a wide range of file formats. Additional reading options specific to each format can be passed through the ellipsis (...) argument. Similarly, the save function provides a simple way to export data into various formats.

``` r
# Load the poliprep package
library(snt)

# Reading a CSV file with a specific seperator
data_csv <- read("path/to/your/file.csv", sep = "\n")

# Import the first sheet from an Excel file
data_excel <- read("path/to/your/file/test_data.xlsx", sheet = 1)

# Import any shapefiles file (GeoJSON/shp)
data_geojson <- read("path/to/your/file/test_data.GeoJSON")
data_shp <- read("path/to/your/file/test_data.shp")

# Export a Stata DTA file
write(my_data, "path/to/your/file.dta")

# Export an RDS file
write(my_data, "path/to/your/file.rds")

# Export any shapefiles file (GeoJSON/shp)
write(my_shp, "path/to/your/file.GeoJson")
write(my_shp, "path/to/your/file.shp")

# Export an Excel file with sheets
write(
  list(my_data = my_data1, my_data2 = my_data2),"path/to/your/file.xlsx")
```

### Geolocation Handling

#### `prep_geonames` for Interactive Admin Name Cleaning and Matching

The `prep_geonames` function combines algorithmic matching with user interactivity to clean and standardize administrative names. It uses string distance calculations for initial matching and allows users to make final corrections interactively, with all decisions saved for future use. The function supports a user-provided lookup dataset as a reference or defaults to internal WHO geonames data if no lookup is provided. Additionally, it supports hierarchical stratification across up to six administrative levels. Cached user decisions enhance consistency and efficiency in subsequent sessions. For users who prefer to run the code without interactivity, the function can be executed with `interactive = FALSE`.
 
```r
target_df <- data.frame(
  country = c("ANGOLA", "UGA", "ZAMBIA"),
  province = c("CABONDA", "TESO", "LUSAKA"),
  district = c("BALIZE", "BOKEDEA", "RAFUNSA")
)

cleaned_df <- harmonize_admin_names(
  target_df,
  level0 = "country",
  level1 = "province",
  level2 = "district",
  interactive = TRUE
)
```
Here is a short video to demonstrate the full interactivity of `harmonize_admin_names` which was formally known as `prep_geonames`:

https://github.com/user-attachments/assets/ffa69a93-a982-43c4-9673-1165f997fd96

### Date Handling

#### `autoparse_dates` for Parsing Dates in a Data Frame

The `autoparse_dates` function parses and standardizes date columns in a data frame, ensuring consistency in date formats. This is particularly useful when working with datasets containing multiple date formats or ambiguous date entries.

```r
# Example setup
df <- data.frame(
  mixed_formats = c("2023-10-03", "11.09.2022", "25-12-21 23:59", "2020-08-15T00:00:00Z"),
  iso8601 = c("2021-03-20T00:01:00.513+01:00", "2022-11-05T23:15:59.123+01:00",
              "2023-06-15T12:30:45.789Z", "2020-01-01T00:00:00.000-05:00")
)
```

Check the initial date formats:

```r
df$mixed_formats
#> [1] "2023-10-03"           "11.09.2022"
#> [3] "25-12-21 23:59"       "2020-08-15T00:00:00Z"
```

```r
parsed_df <- autoparse_dates(
  data = df,
  date_cols = c("mixed_formats", "iso8601"),
  output_format = "%Y-%m-%d"
)
```

Verify the parsed date columns:

```r
parsed_df$mixed_formats
#> [1] "2023-10-03" "2022-09-11" "2021-12-25" "2020-08-15"

parsed_df$iso8601
#> [1] "2021-03-20" "2022-11-05" "2023-06-15" "2020-01-01"
```

## :incoming_envelope: Contacting us

For any issues or questions about `snt`, please contact Mo at [moyusuf\@who.int](mailto:moyusuf@who.int).
