# Data I/O and cleaning

Beginner

Most of an SNT analysis is data wrangling: pulling spreadsheets out of
DHIS2 exports, parsing dates that have been entered four different ways,
matching district names that disagree across files, and producing a
clean tibble the rest of the pipeline can trust. This article walks
through the I/O and cleaning functions in `sntutils` in the order you’d
actually use them on a real project.

## Reading and writing

[`read()`](https://ahadi-analytics.github.io/sntutils/reference/read.md)
and
[`write()`](https://ahadi-analytics.github.io/sntutils/reference/write.md)
are thin wrappers over `rio` plus the spatial ecosystem. They pick an
importer or exporter based on the file extension, so we can stop
thinking about whether something is
[`readr::read_csv()`](https://readr.tidyverse.org/reference/read_delim.html)
or
[`haven::read_dta()`](https://haven.tidyverse.org/reference/read_dta.html)
or
[`sf::st_read()`](https://r-spatial.github.io/sf/reference/st_read.html).

``` r

library(sntutils)

# tabular formats
df_csv    <- read("path/to/file.csv", sep = ",")
df_excel  <- read("path/to/file.xlsx", sheet = 1)
df_stata  <- read("path/to/file.dta")
df_spss   <- read("path/to/file.sav")
df_rds    <- read("path/to/file.rds")

# spatial formats — returns an sf object
sf_geojson  <- read("path/to/file.geojson")
sf_shape    <- read("path/to/file.shp")

# write in the format implied by the extension
write(df, "path/to/export.csv")
write(df, "path/to/export.xlsx")
write(df, "path/to/export.dta")
write(sf_geojson, "path/to/export.geojson")

# write multiple tibbles as sheets in one Excel file
write(
  list(facilities = hf_df, cases = case_df, pop = pop_df),
  "path/to/multi_sheet.xlsx"
)
```

[`read()`](https://ahadi-analytics.github.io/sntutils/reference/read.md)
defers to the underlying reader for arguments, so you can pass `sheet`,
`sep`, `skip`, `col_types` etc. exactly as you would directly.

### Reproducible reads: `read_snt_data()` and `write_snt_data()`

For data we want to track across runs — DHIS2 exports, processed
intermediate files, anything that will land in `01_data/` — use
[`write_snt_data()`](https://ahadi-analytics.github.io/sntutils/reference/write_snt_data.md)
and
[`read_snt_data()`](https://ahadi-analytics.github.io/sntutils/reference/read_snt_data.md)
instead. They write atomically, hash the payload, and drop a sidecar
JSON next to the file with the schema, row count, write timestamp and
hash.
[`read_snt_data()`](https://ahadi-analytics.github.io/sntutils/reference/read_snt_data.md)
looks for the file by **logical name** inside a directory, so the rest
of the pipeline doesn’t care about the extension.

``` r

# write — atomic, hashed, sidecar metadata next to the file
write_snt_data(
  obj  = cleaned_dhis2,
  path = "01_data/1.2_epidemiology/1.2a_routine_surveillance/processed",
  data_name = "sl_dhis2_clean",
  formats = c("rds", "csv")
)

# read — finds the most recent file with that logical name
sl_dhis2 <- read_snt_data(
  path = "01_data/1.2_epidemiology/1.2a_routine_surveillance/processed",
  data_name = "sl_dhis2_clean"
)
```

The sidecar makes it easy to detect upstream changes: if the input hash
hasn’t moved, downstream steps can be cached.

## Standardising column names

DHIS2 exports often arrive with names like
`"OPD Total - Suspected Malaria (U5)"`.
[`standardize_names()`](https://ahadi-analytics.github.io/sntutils/reference/standardize_names.md)
collapses these into something joinable:

``` r

standardize_names(c("Chp Kpalime-III", "Kpalimé CHP 3"))
#> [1] "CHP KPALIME 3" "CHP KPALIME 3"

# applied across a tibble
sl_dhis2 <- sl_dhis2 |>
  dplyr::mutate(
    dplyr::across(c(adm1, adm2, adm3, hf), standardize_names)
  )
```

The function lowercases or uppercases, strips accents and punctuation,
converts Roman numerals to digits, and sorts internal tokens so
`"CHP Kpalime III"` and `"III CHP Kpalime"` collapse to the same string.
Set `to_upper = FALSE` for `lowercase_with_underscores` (handy for
column names).

[`clean_filenames()`](https://ahadi-analytics.github.io/sntutils/reference/clean_filenames.md)
does the equivalent job for file names — useful when unzipping country
submissions where filenames carry spaces and accents.

## Parsing dates

[`autoparse_dates()`](https://ahadi-analytics.github.io/sntutils/reference/autoparse_dates.md)
walks a list of candidate formats and detects which one applies per
cell. It handles ISO-8601 with timezones, ambiguous dd/mm vs mm/dd
cases, and Excel serial dates.

``` r

df <- data.frame(
  mixed_dates   = c("2023-10-03", "11.09.2022", "25-12-21 23:59",
                    "2020-08-15T00:00:00Z"),
  iso8601_dates = c("2021-03-20T00:01:00.513+01:00",
                    "2022-11-05T23:15:59.123Z")
)

parsed <- autoparse_dates(
  data          = df,
  date_cols     = c("mixed_dates", "iso8601_dates"),
  output_format = "%Y-%m-%d"
)

parsed$mixed_dates
#> [1] "2023-10-03" "2022-09-11" "2021-12-25" "2020-08-15"

parsed$iso8601_dates
#> [1] "2021-03-20" "2022-11-05"
```

To see the formats it knows about, inspect `available_date_formats`.

## Inferring variable types

DHIS2 / Excel pipelines almost always leave numerics as character and
factor-like columns as freeform strings.
[`auto_parse_types()`](https://ahadi-analytics.github.io/sntutils/reference/auto_parse_types.md)
walks each column and converts it to the narrowest sensible type —
preserving leading zeros on ID columns it recognises by name suffix.

``` r

df <- tibble::tibble(
  id  = c("001", "002", "003"),
  sex = c("M", "F", "F"),
  age = c("1", "2", "3"),
  dob = c("2001-05-10", "2003-02-14", "2005-09-30")
)

auto_parse_types(df)
#> # A tibble: 3 × 4
#>   id    sex     age dob
#>   <chr> <fct> <int> <date>
#> 1 001   M         1 2001-05-10
#> 2 002   F         2 2003-02-14
#> 3 003   F         3 2005-09-30
```

[`detect_factors()`](https://ahadi-analytics.github.io/sntutils/reference/detect_factors.md)
is the lower-level function that decides whether a character column
should become a factor based on cardinality (`max_levels`).

## Harmonising admin names: `prep_geonames()`

The big one.
[`prep_geonames()`](https://ahadi-analytics.github.io/sntutils/reference/prep_geonames.md)
cleans hierarchical admin names by combining string-distance matching
with an interactive picker and a durable cache of decisions. It supports
up to six admin levels, accepts your own lookup table, and can also be
run non-interactively for batch jobs.

``` r

# input data with messy spellings
dhis2_dummy <- data.frame(
  country  = c("ANGOLA", "UGA", "ZAMBIA", "KEN"),
  province = c("CABONDA", "TESO", "LUSAKA", "NAIROBY"),
  district = c("BALIZE", "BOKEDEA", "RAFUNSA", "KIBRA")
)

# the reference table to match against
my_lookup <- data.frame(
  country  = c("Angola", "Uganda", "Zambia", "Kenya"),
  province = c("Cabinda", "Teso",   "Lusaka", "Nairobi"),
  district = c("Belize",  "Bukedea", "Rufunsa", "Kibera")
)

cleaned <- prep_geonames(
  target_df  = dhis2_dummy,
  lookup_df  = my_lookup,
  level0     = "country",
  level1     = "province",
  level2     = "district",
  interactive = TRUE
)
```

If no `lookup_df` is given,
[`prep_geonames()`](https://ahadi-analytics.github.io/sntutils/reference/prep_geonames.md)
falls back to WHO geonames shipped with the package. Decisions are
persisted to disk, so a second run on the same data is instant.

A demo of the interactive flow is in the [README on
GitHub](https://github.com/ahadi-analytics/sntutils#geolocation-name-cleaning).

## Building a data dictionary

Once a dataset is clean,
[`build_dictionary()`](https://ahadi-analytics.github.io/sntutils/reference/build_dictionary.md)
produces a tibble describing each variable — name, type, number of
unique values, range or top levels, and an English label if one is
available from a labels file. This is what we hand to country teams as
documentation.

``` r

dd <- build_dictionary(dplyr::as_tibble(iris))

dd |> dplyr::select(variable, type, label_en) |> utils::head()
#> # A tibble: 5 × 3
#>   variable     type    label_en
#>   <chr>        <chr>   <chr>
#> 1 Sepal.Length numeric NA
#> 2 Sepal.Width  numeric NA
#> 3 Petal.Length numeric NA
#> 4 Petal.Width  numeric NA
#> 5 Species      factor  NA
```

[`snt_data_dict()`](https://ahadi-analytics.github.io/sntutils/reference/snt_data_dict.md)
returns the package’s curated dictionary of standard SNT variable names
(`conf`, `pres`, `test`, `allout`, …) so we can validate a dataset
against the canonical schema:

``` r

# is "maltreat_u5" a known SNT variable?
check_snt_var("maltreat_u5")
#> ✔ maltreat_u5 — Malaria cases treated, under 5
```

[`clear_snt_cache()`](https://ahadi-analytics.github.io/sntutils/reference/clear_snt_cache.md)
resets the in-memory cache after editing labels.

## A clean pipeline, end to end

Putting it all together — the cleaning step from the [Get
started](https://ahadi-analytics.github.io/sntutils/articles/getting-started.md)
example, but with explicit intermediate writes so we can audit each
stage:

``` r

raw <- read("01_data/1.2_epidemiology/1.2a_routine_surveillance/raw/sl_dhis2.csv")

clean <- raw |>
  standardize_names() |>             # tidy column names
  autoparse_dates(date_cols = "date") |>   # normalise dates
  auto_parse_types() |>              # infer types
  dplyr::mutate(
    hf_uid = vdigest(paste0(adm1, adm2, hf), algo = "xxhash32")
  )

write_snt_data(
  obj       = clean,
  path      = "01_data/1.2_epidemiology/1.2a_routine_surveillance/processed",
  data_name = "sl_dhis2_clean",
  formats   = c("rds", "csv")
)
```

From here, downstream articles take this `clean` object and turn it into
reporting rates, consistency checks, maps, and climate-adjusted
indicators.
