# Write an object to standardized filenames in one or more formats

Writes an object (data.frame, list of data.frames, sf, etc.) to a
directory using standardized naming. Supports atomic writes, optional
pruning of older versioned files, and returns a structured summary.

When filenames are not versioned (no date and no tag) and a file already
exists, set `overwrite = TRUE` to replace it. Otherwise an error is
thrown.

## Usage

``` r
write_snt_data(
  obj,
  path,
  data_name,
  file_formats = "rds",
  date_format = "%Y-%m-%d",
  include_date = TRUE,
  version_tag = NULL,
  n_saved = 3,
  quiet = TRUE,
  overwrite = TRUE,
  ...
)
```

## Arguments

- obj:

  Object to write

- path:

  Directory to write into

- data_name:

  Base file name without extension

- file_formats:

  Character vector of formats. Default "rds".

- date_format:

  Date format for version tag. Default "%Y-%m-%d".

- include_date:

  Logical; append \_v. Default TRUE.

- version_tag:

  Optional explicit tag (conflicts with include_date)

- n_saved:

  Optional positive integer; keep only the newest `n_saved` versioned
  files per format. Default is 3.

- quiet:

  Logical; suppress info logs.

- overwrite:

  Logical; allow overwrite when not versioned.

- ...:

  Additional writer-specific arguments

## Value

tibble with columns format, path, ok, bytes, message

## Examples

``` r
# write_snt_data(head(mtcars), tempdir(), "cars", "rds",
#                include_date = FALSE, overwrite = TRUE)
```
