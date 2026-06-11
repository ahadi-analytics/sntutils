# Crosswalk DHIS2 dataset using dictionary

Given a dataset and its dictionary, build mappings between harmonised
names and raw headers, then rename dataset columns accordingly. Matching
is case-, space-, punctuation-, and accent-insensitive. Optionally
prints CLI diagnostics summarising matched and unmatched columns.

## Usage

``` r
dhis2_map(data, dict, new_col, old_col, verbose = TRUE, drop_unmatched = FALSE)
```

## Arguments

- data:

  a data.frame with DHIS2 columns to be renamed.

- dict:

  a data.frame containing the dictionary.

- new_col:

  column in `dict` with harmonised names (name or index).

- old_col:

  column in `dict` with raw/source names (name or index).

- verbose:

  logical; if TRUE (default) prints CLI messages.

- drop_unmatched:

  logical; if TRUE, drops columns that are not found in the dictionary.
  Default is FALSE.

## Value

the input dataset with renamed columns.
