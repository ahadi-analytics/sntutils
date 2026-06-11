# Query DHS API Directly via URL Parameters

Builds and queries DHS API for indicator data using URL-based access
instead of rdhs package.

## Usage

``` r
download_dhs_indicators(
  countryIds,
  indicatorIds,
  surveyIds = NULL,
  surveyYear = NULL,
  surveyYearStart = NULL,
  surveyYearEnd = NULL,
  breakdown = "subnational",
  f = "json"
)
```

## Arguments

- countryIds:

  Comma-separated DHS country code(s), e.g., "SL"

- indicatorIds:

  Comma-separated DHS indicator ID(s), e.g., "CM_ECMR_C_U5M"

- surveyIds:

  Optional comma-separated survey ID(s), e.g., "SL2016DHS"

- surveyYear:

  Optional exact survey year, e.g., "2016"

- surveyYearStart:

  Optional survey year range start

- surveyYearEnd:

  Optional survey year range end

- breakdown:

  One of: "national", "subnational", "background", "all"

- f:

  Format to return (default is "json")

## Value

A data.frame containing the `Data` portion of the API response.
