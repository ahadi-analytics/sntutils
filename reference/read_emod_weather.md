# Read EMOD weather binary files into a data.frame

R equivalent of Python's `emodpy_malaria.weather.weather_to_csv()`.
Reads paired `.bin` + `.bin.json` files for each weather variable.

## Usage

``` r
read_emod_weather(weather_dir, weather_file_prefix = "seasonal")
```

## Arguments

- weather_dir:

  Directory containing the weather files.

- weather_file_prefix:

  File prefix (e.g. `"seasonal"`).

## Value

A list with:

- data:

  Tibble with columns `node_id`, `steps`, `airtemp`, `humidity`,
  `rainfall`, `landtemp`.

- attributes:

  Parsed JSON metadata (list).
