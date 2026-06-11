# Resolve path inside AHADI OneDrive shared library

`ahadi_path()` builds a robust, OS-aware path to a file or folder inside
the AHADI OneDrive "Shared Library". It:

- detects the OS (Windows or macOS)

- scans common OneDrive roots (including CloudStorage on macOS)

- finds the "Shared Libraries" folder for the given organisation

- fuzzy-matches the requested library name

- navigates to a given base folder inside that library

- optionally joins a relative path

The function caches the resolved library root in an R option so that
subsequent calls are fast. Use `refresh = TRUE` to force re-detection.

## Usage

``` r
ahadi_path(
  relative = NULL,
  org = "Applied Health Analytics for Delivery and Innovation Inc",
  library = "AHADI Information - technical",
  base = "Documentation per topic/data/dhs_data",
  refresh = FALSE,
  verbose = FALSE
)
```

## Arguments

- relative:

  character. Relative path inside `base`. If `NULL`, the function
  returns the resolved `base` folder path.

- org:

  character. Organisation name as it appears in OneDrive shared library
  naming. This is cleaned internally for matching. Default is "Applied
  Health Analytics for Delivery and Innovation Inc".

- library:

  character. Display name of the OneDrive shared library. Example:
  "AHADI Information - technical".

- base:

  character. Base folder inside the library. Example: "Documentation per
  topic/data/dhs_data".

- refresh:

  logical. If `TRUE`, ignores any cached location and forces a fresh
  search.

- verbose:

  logical. If `TRUE`, prints diagnostic messages about how the path was
  resolved.

## Value

A character scalar giving an absolute path. If `relative` is `NULL`,
this is the resolved `base` path. Otherwise it is
`file.path(base, relative)`.

## Details

The detection logic follows several steps:

- detect OS using `Sys.info()[["sysname"]]`

- construct a list of candidate OneDrive root folders

- within each root, look for folders whose names suggest a OneDrive
  "Shared Libraries" mount

- inside those, fuzzy match the requested organisation and library names

- construct and validate the requested `base` folder

Matching is done on "cleaned" names (lowercase, no spaces or
punctuation). This makes the function resilient to cosmetic differences
in naming such as "AHADI Information - technical" vs "AHADI information
(technical)".

If the function cannot find a matching organisation, library, or base
folder, it fails with
[`cli::cli_abort()`](https://cli.r-lib.org/reference/cli_abort.html) and
prints the closest matches it could see.

## Examples

``` r
if (FALSE) { # \dontrun{
# get the base DHS data directory
base_path <- ahadi_path()

# get a specific file inside the base folder
file_path <- ahadi_path("surveys/2020/survey_data.csv")

# use a different library and base folder
other_path <- ahadi_path(
  relative = "my_file.xlsx",
  library = "AHADI Operations",
  base = "SOPs"
)

# force re-detection if the OneDrive setup changed
ahadi_path(refresh = TRUE, verbose = TRUE)
} # }
```
