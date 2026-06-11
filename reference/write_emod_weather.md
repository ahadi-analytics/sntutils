# Write EMOD binary weather files (.bin + .json) from a data.frame

R equivalent of Python's `emodpy_malaria.weather.csv_to_weather()`.
Writes one `.bin` / `.bin.json` pair per weather variable.

## Usage

``` r
write_emod_weather(
  df,
  weather_dir,
  climate_profile,
  attributes = NULL,
  id_reference = "Legacy",
  update_resolution = "CLIMATE_UPDATE_DAY"
)
```

## Arguments

- df:

  Data.frame with columns `node_id`, `steps`, `airtemp`, `humidity`,
  `rainfall`, `landtemp`.

- weather_dir:

  Output directory for the binary files.

- climate_profile:

  Name prefix for the output files.

- attributes:

  Optional list of EMOD JSON metadata (from
  [`read_emod_weather()`](https://ahadi-analytics.github.io/sntutils/reference/read_emod_weather.md)).
  If `NULL`, attributes are created automatically from the data.

- id_reference:

  EMOD ID reference (used when `attributes` is NULL).

- update_resolution:

  Climate update resolution (used when `attributes` is NULL).

## Value

The output directory path (invisibly).
