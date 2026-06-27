# Read & clean

Most of an SNT analysis is data wrangling: pulling spreadsheets out of
DHIS2 exports, parsing dates that have been entered four different ways,
matching district names that disagree across files, and producing a
clean tibble the rest of the pipeline can trust. This article walks
through the I/O and cleaning functions in `sntutils` in the order you’d
actually use them on a real project.

**For the methodology and conceptual background behind the steps in this
article, please check the [SNT Code
Library](https://ahadi-analytics.github.io/snt-code-library/):**

- [DHIS2 import and
  preprocessing](https://ahadi-analytics.github.io/snt-code-library/english/library/data/routine_cases/import.html) -
  what to expect from a DHIS2 export and the cleaning steps it needs.
- [DHIS2
  extraction](https://ahadi-analytics.github.io/snt-code-library/english/library/data/routine_cases/dhis2_extraction.html) -
  how DHIS2 exports are produced upstream.
- [Routine-data
  context](https://ahadi-analytics.github.io/snt-code-library/english/library/data/routine_cases/context.html) -
  background on what’s collected and how.
- [Final clean
  database](https://ahadi-analytics.github.io/snt-code-library/english/library/data/routine_cases/final_database.html) -
  what the cleaned, joined dataset should look like.
- [Master facility
  lists](https://ahadi-analytics.github.io/snt-code-library/english/library/data/health_facilities/master_facility_lists.html) -
  matching DHIS2 to the MFL.

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

# spatial formats - returns an sf object
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

### Reproducible reads with metadata

For data we want to track across runs - DHIS2 exports, processed
intermediate files, anything that will land in `01_data/` - use
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

# write - atomic, hashed, sidecar metadata next to the file
write_snt_data(
  obj  = cleaned_dhis2,
  path = "01_data/1.2_epidemiology/1.2a_routine_surveillance/processed",
  data_name = "sl_dhis2_clean",
  formats = c("rds", "csv")
)

# read - finds the most recent file with that logical name
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
does the equivalent job for file names - useful when unzipping country
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

To see the formats it knows about, inspect `available_date_formats`:

``` r

utils::head(available_date_formats, 8)
#> [1] "%Y-%m-%d"      "%Y/%m/%d"      "%d-%m-%Y"      "%d/%m/%Y"
#> [5] "%d.%m.%Y"      "%d-%b-%Y"      "%d %B %Y"      "%Y-%m-%dT%H:%M:%S"
```

Pass an additional `formats =` vector to extend the list for a country
whose data carries an unusual encoding (some MIS exports use `%d %m %Y`
with a literal space, for example).

## Inferring variable types

DHIS2 / Excel pipelines almost always leave numerics as character and
factor-like columns as freeform strings. The two helpers below recover
the right types without losing the IDs that look numeric but aren’t.

### `auto_parse_types()` - one call, sensible types

Walks each column and converts to the narrowest sensible type. It
recognises ID-like columns by name suffix (`*_id`, `*_uid`, `*_code`,
etc.) and keeps them as character so leading zeros survive.

``` r

df <- tibble::tibble(
  hf_uid    = c("001", "002", "003"),       # preserved as character
  sex       = c("M", "F", "F"),             # detected as factor
  age       = c("1", "2", "3"),             # parsed to integer
  weight_kg = c("12.4", "15.0", "9.8"),     # parsed to double
  dob       = c("2001-05-10", "2003-02-14", "2005-09-30"),  # to Date
  notes     = c("ok", "review", NA_character_)  # stays character
)

auto_parse_types(df)
#> # A tibble: 3 × 6
#>   hf_uid sex     age weight_kg dob        notes
#>   <chr>  <fct> <int>     <dbl> <date>     <chr>
#> 1 001    M         1      12.4 2001-05-10 ok
#> 2 002    F         2      15   2003-02-14 review
#> 3 003    F         3       9.8 2005-09-30 <NA>
```

Control the factor-vs-character cut-off via `max_levels` (default 50):
columns with more unique values than that stay character, so a
`facility_name` column with 1700 distinct values doesn’t get coerced to
a 1700-level factor.

### `detect_factors()` - the cardinality check, exposed

[`detect_factors()`](https://ahadi-analytics.github.io/sntutils/reference/detect_factors.md)
is the lower-level helper that powers the factor decision in
[`auto_parse_types()`](https://ahadi-analytics.github.io/sntutils/reference/auto_parse_types.md).
Call it directly when you want to see what *would* be factorised before
committing to the conversion:

``` r

tibble::tibble(
  adm  = c("A", "B", "A", "C", "B"),       # 3 levels - good factor candidate
  code = c("01", "02", "03", "04", "05"),  # 5 unique - looks like an ID
  note = letters[1:5]                       # 5 unique - just text
) |>
  detect_factors(max_levels = 4)
#> # A tibble: 3 × 4
#>   variable n_unique would_factor reason
#>   <chr>       <int> <lgl>        <chr>
#> 1 adm             3 TRUE         under max_levels
#> 2 code            5 FALSE        looks like an ID column
#> 3 note            5 FALSE        over max_levels
```

The `reason` column makes it easy to audit type decisions when a country
submission looks suspicious.

## Harmonising admin names

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

## Fuzzy-matching facility lists

DHIS2 facility names rarely line up perfectly with the master facility
list.
[`fuzzy_match_facilities()`](https://ahadi-analytics.github.io/sntutils/reference/fuzzy_match_facilities.md)
runs a staged matching pipeline:

1.  exact match,
2.  match after
    [`standardize_names()`](https://ahadi-analytics.github.io/sntutils/reference/standardize_names.md)
    normalisation,
3.  string-distance match using one or more methods (`jw`, `lv`, `osa`,
    …),
4.  optional interactive picker for unresolved rows,

and returns a tibble of best matches plus diagnostics by stage.

``` r

matches <- fuzzy_match_facilities(
  target_df       = dhis2_facilities,        # what we're cleaning
  lookup_df       = mfl_facilities,          # the reference list
  admin_cols      = c("adm1", "adm2", "adm3"),
  hf_col_name     = "hf",
  uid_col         = "hf_uid",
  fuzzy_methods   = c("jw", "osa"),
  fuzzy_threshold = 95,
  match_interactivity = TRUE,
  save_path = "01_data/1.1_foundational/1.1b_health_facilities/processed"
)

matches$results |>
  dplyr::count(match_status)
#> # A tibble: 3 × 2
#>   match_status     n
#>   <chr>        <int>
#> 1 high          4012
#> 2 medium         147
#> 3 low             54
```

[`calculate_match_stats()`](https://ahadi-analytics.github.io/sntutils/reference/calculate_match_stats.md)
summarises the same results by method so we can compare matching
strategies side by side. The matched facility table is the bridge
between a DHIS2 export and the spatial master facility list used in
[Spatial](https://ahadi-analytics.github.io/sntutils/articles/spatial.md).

## Dictionaries: producing one, and using the curated SNT schema

Two flavours of dictionary live in `sntutils`. Both come up on every
country project, so they’re worth knowing well.

### The curated SNT variable dictionary

The package ships with a canonical schema of the SNT variable names that
the rest of `sntutils` expects (`conf`, `pres`, `test`, `allout`,
`maltreat_u5`, …).
[`snt_data_dict()`](https://ahadi-analytics.github.io/sntutils/reference/snt_data_dict.md)
returns this curated dictionary as a tibble, with bilingual labels and a
category tag for each variable. This is the source of truth for variable
naming across country teams.

``` r

snt_data_dict() |>
  dplyr::select(variable, category, label_en, label_fr) |>
  dplyr::filter(category == "case_management") |>
  utils::head()
#> # A tibble: 6 × 4
#>   variable    category         label_en                       label_fr
#>   <chr>       <chr>            <chr>                          <chr>
#> 1 allout      case_management  All-cause outpatient visits    Consultations externes ...
#> 2 susp        case_management  Suspected malaria cases        Cas suspects de paludisme
#> 3 test        case_management  Malaria tests performed        Tests de paludisme ...
#> 4 conf        case_management  Confirmed malaria cases        Cas confirmés
#> 5 pres        case_management  Presumed malaria cases         Cas présumés
#> 6 maltreat    case_management  Malaria cases treated          Cas traités
```

[`check_snt_var()`](https://ahadi-analytics.github.io/sntutils/reference/check_snt_var.md)
validates one or more variable names against the schema, with fuzzy
matching when names are close-but-not-exact:

``` r

check_snt_var("maltreat_u5")
#> ✔ maltreat_u5 - Malaria cases treated, under 5

check_snt_var("conf_uner5")     # typo
#> ✖ Unknown SNT variable: conf_uner5
#> ℹ Did you mean: conf_u5?
```

Use it as a sanity check at the start of every analysis, before joining
DHIS2 to anything else.

### Generate a dictionary for your own dataset

Once a dataset is clean,
[`build_dictionary()`](https://ahadi-analytics.github.io/sntutils/reference/build_dictionary.md)
produces a per-variable summary tibble - name, type, number of unique
values, value range or top levels, and (where they’re known) the English
label from the SNT schema. This is the artefact we hand to country teams
as documentation for any processed dataset.

``` r

dd <- build_dictionary(sl_dhis2)

dd |>
  dplyr::select(variable, type, n_unique, label_en) |>
  utils::head()
#> # A tibble: 6 × 4
#>   variable type        n_unique label_en
#>   <chr>    <chr>          <int> <chr>
#> 1 adm0     factor             1 Country (adm0)
#> 2 adm1     factor             5 Region (adm1)
#> 3 adm2     factor            17 District (adm2)
#> 4 adm3     factor           191 Chiefdom (adm3)
#> 5 hf       character       1702 Health facility name
#> 6 conf     numeric            - Confirmed malaria cases
```

The output drops cleanly into a multi-sheet Excel via
[`write()`](https://ahadi-analytics.github.io/sntutils/reference/write.md),
so we can ship a `data_dictionary.xlsx` alongside every processed
dataset without writing it by hand.

[`clear_snt_cache()`](https://ahadi-analytics.github.io/sntutils/reference/clear_snt_cache.md)
resets the in-memory cache after editing the underlying labels CSV -
call it if you’ve updated the schema mid-session.

### Crosswalking DHIS2 columns: `dhis2_map()`

[`dhis2_map()`](https://ahadi-analytics.github.io/sntutils/reference/dhis2_map.md)
renames a DHIS2 export’s columns using a name-mapping dictionary, so we
can keep the upstream column labels intact on disk and only remap to SNT
names when we load the data. It’s a small but critical step in every
DHIS2 import workflow, and a natural neighbour of
[`check_snt_var()`](https://ahadi-analytics.github.io/sntutils/reference/check_snt_var.md)
above - one renames, the other validates that the renamed columns hit
the canonical schema.

> **Used in:** the [DHIS2 import and
> preprocessing](https://ahadi-analytics.github.io/snt-code-library/english/library/data/routine_cases/import.html)
> chapter of the SNT Code Library walks through where this fits in the
> end-to-end DHIS2 cleaning pipeline.
> [`dhis2_map()`](https://ahadi-analytics.github.io/sntutils/reference/dhis2_map.md)
> is the helper that performs the rename step described there.

``` r

mapped <- dhis2_map(
  data    = sl_dhis2_raw,
  dict    = dhis2_label_lookup,
  new_col = "snt_name",
  old_col = "dhis2_label",
  drop_unmatched = FALSE
)
```

Pair it with
[`check_snt_var()`](https://ahadi-analytics.github.io/sntutils/reference/check_snt_var.md)
immediately after the rename to confirm the resulting column names match
the canonical SNT schema before joining DHIS2 to anything else.

## A clean pipeline, end to end

Putting it all together - the cleaning step from the [Get
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
