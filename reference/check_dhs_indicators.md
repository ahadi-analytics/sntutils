# Check DHS Indicator List from API

Check DHS Indicator List from API

## Usage

``` r
check_dhs_indicators(
  countryIds = NULL,
  indicatorIds = NULL,
  surveyIds = NULL,
  surveyYear = NULL,
  surveyYearStart = NULL,
  surveyYearEnd = NULL,
  surveyType = NULL,
  surveyCharacteristicIds = NULL,
  tagIds = NULL,
  returnFields = c("IndicatorId", "Label", "Definition", "MeasurementType"),
  perPage = NULL,
  page = NULL,
  f = "json"
)
```

## Arguments

- countryIds:

  DHS country code(s), e.g., "EG"

- indicatorIds:

  Specific indicator ID(s)

- surveyIds:

  Survey ID(s)

- surveyYear:

  Exact year

- surveyYearStart:

  Start of year range

- surveyYearEnd:

  End of year range

- surveyType:

  DHS survey type (e.g., "DHS", "MIS")

- surveyCharacteristicIds:

  Filter by survey characteristic ID

- tagIds:

  Filter by tag ID

- returnFields:

  Fields to return (default: IndicatorId, Label, Definition)

- perPage:

  Max results per page (default = 500)

- page:

  Specific page to return (default = 1)

- f:

  Format (default = "json")

## Value

A data.frame of indicators
