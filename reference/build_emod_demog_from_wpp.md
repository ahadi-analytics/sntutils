# Build EMOD Demographic Inputs from UN WPP 2024 Data

Pulls age structure and vital rates from the UN World Population
Prospects 2024 and returns EMOD-ready AgeDistribution, birth rate, and
death rate fields.

## Usage

``` r
build_emod_demog_from_wpp(country, year)
```

## Arguments

- country:

  Character. Country name as used in wpp2024 (e.g. "Burundi", "Kenya",
  "Nigeria").

- year:

  Integer. Reference year for demographic data.

## Value

A named list with:

- age_distribution:

  List with DistributionValues (CDF), ResultScaleFactor, ResultValues
  (age in days).

- crude_birth_rate:

  Numeric. Annual births per 1000.

- crude_death_rate:

  Numeric. Annual deaths per 1000.

- birth_rate_daily:

  Numeric. BirthRate for EMOD NodeAttributes.

- country:

  Character. Matched country name.

- year:

  Integer. Reference year used.

## Examples

``` r
if (FALSE) { # \dontrun{
demog <- build_emod_demog_from_wpp("Burundi", 2022)
demog$crude_birth_rate
} # }
```
