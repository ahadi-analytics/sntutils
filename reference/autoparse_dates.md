# Parse Dates in a Data Frame

This function parses date columns in a data frame using various date
formats. It standardizes the output to a specified format and provides
verbose feedback on parsing results.

## Usage

``` r
autoparse_dates(
  data,
  date_cols,
  output_format = "%Y-%m-%d",
  additional_format = NULL,
  verbose = TRUE
)
```

## Arguments

- data:

  A data frame containing columns with date values to parse.

- date_cols:

  A character vector specifying the names of columns with date values.

- output_format:

  A character string specifying the desired output format for the parsed
  dates. Default is `"%Y-%m-%d"`.

- additional_format:

  A character string specifying any additional formats that are not
  included in the default formats (see `available_date_formats`).
  Default is NULL.

- verbose:

  Logical. If `TRUE`, prints messages about parsing success or failure
  for each column. Default is `TRUE`.

## Value

Returns the original data frame with the specified date columns parsed
and formatted.

## Details

This function supports a wide range of date formats, including:

- Basic formats: `"dmY"`, `"mdY"`, `"Ymd"`, `"Ydm"`, `"dmy"`, `"mdy"`,
  `"ymd"`, `"ydm"`

- Formats with time: `"dmY HMS"`, `"mdY HMS"`, `"Ymd HMS"`, `"Ydm HMS"`,
  `"dmy HMS"`, `"mdy HMS"`, `"ymd HMS"`, `"ydm HMS"`

- Additional formats: `"Y-m-d"`, `"Y-m-d H:M:S"`, `"Y-m-d H:M"`,
  `"d-m-Y H:M:S"`, `"d.m.Y H:M:S"`, `"d.m.Y H:M"`, `"d-m-Y H:M"`,
  `"d.m.Y"`

- ISO 8601 format: e.g., `"2021-03-20T00:01:00.513+01:00"`

## Examples

``` r
# Define a sample data frame
df <- data.frame(
  `dmY` = c("03-10-2023", "11-09-2022", "25-12-2021", "15-08-2020"),
  `mdY` = c("10-03-2023", "09-11-2022", "12-25-2021", "08-15-2020"),
  `Ymd` = c("2023-10-03", "2022-09-11", "2021-12-25", "2020-08-15"),
  `Ydm` = c("2023-03-10", "2022-11-09", "2021-25-12", "2020-15-08"),
  `dmy` = c("03-10-23", "11-09-22", "25-12-21", "15-08-20"),
  `mdy` = c("10-03-23", "09-11-22", "12-25-21", "08-15-20"),
  `ymd` = c("23-10-03", "22-09-11", "21-12-25", "20-08-15"),
  `ydm` = c("23-03-10", "22-11-09", "21-25-12", "20-15-08"),
  `dmY_HMS` = c(
    "03-10-2023 14:30:00", "11-09-2022 05:45:12",
    "25-12-2021 23:59:59", "15-08-2020 00:00:00"
  ),
  `mdY_HMS` = c(
    "10-03-2023 14:30:00", "09-11-2022 05:45:12",
    "12-25-2021 23:59:59", "08-15-2020 00:00:00"
  ),
  `Ymd_HMS` = c(
    "2023-10-03 14:30:00", "2022-09-11 05:45:12",
    "2021-12-25 23:59:59", "2020-08-15 00:00:00"
  ),
  `Ydm_HMS` = c(
    "2023-03-10 14:30:00", "2022-11-09 05:45:12",
    "2021-25-12 23:59:59", "2020-15-08 00:00:00"
  ),
  `dmy_HMS` = c(
    "03-10-23 14:30:00", "11-09-22 05:45:12",
    "25-12-21 23:59:59", "15-08-20 00:00:00"
  ),
  `mdy_HMS` = c(
    "10-03-23 14:30:00", "09-11-22 05:45:12",
    "12-25-21 23:59:59", "08-15-20 00:00:00"
  ),
  `ymd_HMS` = c(
    "23-10-03 14:30:00", "22-09-11 05:45:12",
    "21-12-25 23:59:59", "20-08-15 00:00:00"
  ),
  `ydm_HMS` = c(
    "23-03-10 14:30:00", "22-11-09 05:45:12",
    "21-25-12 23:59:59", "20-15-08 00:00:00"
  ),
  `Y-m-d` = c("2023-10-03", "2022-09-11", "2021-12-25", "2020-08-15"),
  `Y-m-d_HMS` = c(
    "2023-10-03 14:30:00", "2022-09-11 05:45:12",
    "2021-12-25 23:59:59", "2020-08-15 00:00:00"
  ),
  `Y-m-d_HM` = c(
    "2023-10-03 14:30", "2022-09-11 05:45",
    "2021-12-25 23:59", "2020-08-15 00:00"
  ),
  `d-m-Y_HMS` = c(
    "03-10-2023 14:30:00", "11-09-2022 05:45:12",
    "25-12-2021 23:59:59", "15-08-2020 00:00:00"
  ),
  `d.m.Y_HMS` = c(
    "03.10.2023 14:30:00", "11.09.2022 05:45:12",
    "25.12.2021 23:59:59", "15.08.2020 00:00:00"
  ),
  `d.m.Y_HM` = c(
    "03.10.2023 14:30", "11.09.2022 05:45",
    "25.12.2021 23:59", "15.08.2020 00:00"
  ),
  `d-m-Y_HM` = c(
    "03-10-2023 14:30", "11-09-2022 05:45",
    "25-12-2021 23:59", "15-08-2020 00:00"
  ),
  `d.m.Y` = c("03.10.2023", "11.09.2022", "25.12.2021", "15.08.2020"),
  `iso8601` = c(
    "2021-03-20T00:01:00.513+01:00",
    "2022-11-05T23:15:59.123+01:00",
    "2023-06-15T12:30:45.789Z",
    "2020-01-01T00:00:00.000-05:00"
  ),
  `mixed_formats` = c(
    "2023-10-03", "11.09.2022",
    "25-12-21 23:59", "2020-08-15T00:00:00Z"
  ),
  stringsAsFactors = FALSE
)

# Parse the date columns
parsed_df <- autoparse_dates(df,
  date_cols = colnames(df),
  output_format = "%Y-%m-%d"
)
#> ✔ All columns have been successfully parsed to the given format
```
