# Write EMOD weather files per adm2 (one folder per district)

Wrapper around
[`write_emod_weather()`](https://ahadi-analytics.github.io/sntutils/reference/write_emod_weather.md)
that splits a multi-node climate data.frame by adm2, writing each as a
single-node EMOD weather set in its own subfolder.

## Usage

``` r
write_emod_weather_by_adm2(
  df,
  node_coord,
  weather_dir,
  folder_case = "title",
  ...
)
```

## Arguments

- df:

  Data.frame with columns `node_id`, `steps`, `airtemp`, `humidity`,
  `rainfall`, `landtemp`.

- node_coord:

  Data.frame mapping `node_id` to `adm2` (e.g. from shapefile
  centroids).

- weather_dir:

  Base output directory. Each adm2 gets a subfolder.

- folder_case:

  Case style for subfolder names: `"title"` (default), `"upper"`, or
  `"lower"`.

- ...:

  Additional arguments passed to
  [`write_emod_weather()`](https://ahadi-analytics.github.io/sntutils/reference/write_emod_weather.md).

## Value

The base output directory path (invisibly).
