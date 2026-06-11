# Write EMOD Demographics JSON Files by ADM2

Writes one demographics JSON per admin-2 unit, mirroring the folder
layout used by
[`write_emod_weather_by_adm2()`](https://ahadi-analytics.github.io/sntutils/reference/write_emod_weather_by_adm2.md).
Each folder gets `demographics.json` with matching `IdReference`,
`NodeID = 1`, and `NodeCount = 1`.

## Usage

``` r
write_emod_demog_by_adm2(
  df,
  output_dir,
  folder_case = "title",
  demog_prefix = "",
  demog_suffix = "",
  id_reference = "Legacy",
  individual_properties = NULL,
  age_distribution = NULL
)
```

## Arguments

- df:

  Data frame with columns: `adm2`, `lat`, `lon`, `pop`,
  `crude_death_rate`, `crude_birth_rate`.

- output_dir:

  Character. Root directory for output folders.

- folder_case:

  Case style for subfolder names: `"title"` (default), `"upper"`, or
  `"lower"`. Must match the case used by
  [`write_emod_weather_by_adm2()`](https://ahadi-analytics.github.io/sntutils/reference/write_emod_weather_by_adm2.md).

- demog_prefix:

  Character. Prefix before "demographics" in the filename. Default `""`
  gives `"demographics.json"`. Set to NULL to auto-prefix with the
  folder name.

- demog_suffix:

  Character. Optional suffix before .json (e.g. "\_wSMC_risk").

- id_reference:

  Character. IdReference string. Default "Legacy". Must match the
  climate files.

- individual_properties:

  Optional list of IP definitions.

- age_distribution:

  Optional age distribution list.

## Value

Invisible NULL. Called for side effect of writing files.

## Examples

``` r
if (FALSE) { # \dontrun{
df <- data.frame(
  adm2 = c("Bubanza", "Cibitoke"),
  lat = c(-3.08, -2.89),
  lon = c(29.37, 29.18),
  pop = c(100000L, 120000L),
  crude_death_rate = c(8.5, 8.5),
  crude_birth_rate = c(38.0, 38.0)
)
write_emod_demog_by_adm2(df, tempdir())
} # }
```
