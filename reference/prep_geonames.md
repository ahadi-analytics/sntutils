# Interactive Admin Name Cleaning and Matching

This function streamlines the admin name cleaning process, leveraging
both algorithmic approaches and interactive user decisions. It combines
string distance algorithms for initial matching and offers user
interactivity for final decision-making, which are then saved for future
reference and sharing. Although the function does not require limiting
name matching exclusively to upper-level admins, optimal performance is
achieved by confining to stricter within-admin stratifications, ensuring
more accurate results. The function can also work with site names or
even any string matching that has lookup data.

## Usage

``` r
prep_geonames(
  target_df,
  lookup_df = NULL,
  level0 = NULL,
  level1 = NULL,
  level2 = NULL,
  level3 = NULL,
  level4 = NULL,
  cache_path = NULL,
  unmatched_export_path = NULL,
  method = "jw",
  interactive = TRUE,
  max_options = 200,
  preserve_case = FALSE,
  column_width = 60
)
```

## Arguments

- target_df:

  Data frame containing the admin names to clean.

- lookup_df:

  Lookup data frame for verifying admin names. If this is not provided,
  an internal version of WHO geoname data attached to sntutils is used.

- level0:

  level0 col name (country) in both 'data' and 'lookup_data'.

- level1:

  level1 col name (province) in both 'data' and 'lookup_data'.

- level2:

  level2 col name (district) in both 'data' and 'lookup_data'.

- level3:

  level3 col name (subdistrict) in both 'data' and 'lookup_data'.

- level4:

  level4 col name (settlement) in both 'data' and 'lookup_data'.

- cache_path:

  Optional; the path where the cache data frame is saved after user
  modifications. Supports all file formats supported by sntutils read()
  and write() functions including .rds, .csv, .xlsx, .dta, .tsv, and
  more. This path is also used to match and integrate previously
  established corrections into the current session. If NULL or the file
  does not exist at the provided path, users will be prompted to specify
  a new path or create a new cache data frame.

- unmatched_export_path:

  Optional; path to save unmatched data after processing. The file will
  include complete rows with full hierarchical context, showing which
  column needs matching (typically the most granular level like health
  facilities) along with all administrative levels and metadata
  (timestamp, username, data sources). Supports all file formats
  supported by sntutils write() function based on the file extension.

- method:

  The string distance calculation method(s) to be used. Users can
  specify one or more algorithms from the
  [`stringdist`](https://rdrr.io/pkg/stringdist/man/stringdist.html)
  package to compute string distances between admin names. The function
  by default uses `"jw"` (Jaro-Winkler). Other options include: `"lv"`
  (Levenshtein), `"dl"` (Damerau-Levenshtein), `"lcs"` (Longest Common
  Subsequence), `"qgram"` (Q-Gram), `"jw"` (Jaro-Winkler), and
  `"soundex"`.

- interactive:

  Logical; if TRUE, prompts the user for interactive matching decisions.
  Defaults to FALSE.

- max_options:

  Maximum number of options to output for string distance matching.
  Default is 200.

- preserve_case:

  Logical; if TRUE, preserves the original case of admin names from the
  lookup data when returning matched values. If FALSE (default), returns
  all admin names in uppercase as before.

- column_width:

  Numeric; the maximum width (in characters) for each column in the
  interactive menu. Controls how much text is displayed before
  truncation. Default is 60 characters. Note that the actual text
  display width is approximately 8 characters less to accommodate number
  labels and truncation markers ("...").

## Value

A data frame with cleaned administrative names and saved data frame of
user decisions.

## Details

The function performs the following steps:

1.  Prepares the data by ensuring administrative names are in uppercase
    for consistent matching.

2.  Attempts to load a previously saved cache file if available, or
    initializes the cleaning process.

3.  Matches administrative names between `target_df` and `lookup_df`
    using string distance algorithms, running in parallel. Results are
    ranked by closeness.

4.  Engages the user through an interactive CLI menu to make decisions
    on ambiguous matches.

5.  Saves the user's decisions in a cache data frame, either to a
    specified path or by prompting the user for a location.

6.  Returns a cleaned data frame with updated administrative names based
    on user choices and algorithmic matching.

## Examples

``` r
# \donttest{
# Dummy target data
target_df <- data.frame(
  country = c("ANGOLA", "UGA", "ZAMBIA"),
  province = c("CABONDA", "TESO", "LUSAKA"),
  district = c("BALIZE", "BOKEDEA", "RAFUNSA"),
  subdistrict = c("AREA1", "AREA2", "AREA3")
)

# Dummy lookup data with correct spellings
lookup_df <- data.frame(
  country = c("ANGOLA", "ANGOLA", "UGANDA", "UGANDA", "ZAMBIA", "ZAMBIA"),
  province = c("CABINDA", "CABINDA", "TESO", "TESO", "LUSAKA", "LUSAKA"),
  district = c("BELIZE", "BUCO-ZAU", "BUKEDEA", "KUMI", "KAFUE", "LUSAKA"),
  stringsAsFactors = FALSE
)

# Interactively clean geonames
prep_geonames(
  target_df,
  lookup_df = lookup_df,
  level0 = "country", level1 = "province",
  level2 = "district",
  interactive = FALSE # replace with TRUE for interactivity
)
#> 
#> ── ℹ Match Summary ─────────────────────────────────────────────────────────────
#> 
#> ! Both sides have unmatched names; see per-level lines below.
#> 
#> Target data as base N                                                       
#> • country (level0): 2 out of 3 matched                                      
#> • province (level1): 1 out of 3 matched                                     
#> • district (level2): 0 out of 3 matched                                     
#> Lookup data as base N                                                       
#> • country (level0): 2 out of 3 matched                                      
#> • province (level1): 1 out of 3 matched                                     
#> • district (level2): 0 out of 6 matched                                     
#> ✔ In non-interactive mode. Exiting after matching with cache...
#>   country province district subdistrict
#> 1  ANGOLA  CABONDA   BALIZE       AREA1
#> 2     UGA     TESO  BOKEDEA       AREA2
#> 3  ZAMBIA   LUSAKA  RAFUNSA       AREA3
# }
```
