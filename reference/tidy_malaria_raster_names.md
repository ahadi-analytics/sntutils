# Normalize malariaAtlas Raster Filenames

Strips bounding-box coordinates and download-date metadata appended by
`malariaAtlas::getRaster()`, leaving only `...Rate.YYYY.tiff`. This
function renames files on disk and is idempotent: re-running after a
fresh download only touches newly-added files.

This is the **recommended first step** after downloading malariaAtlas
rasters to ensure downstream processing functions can correctly extract
years from filenames.

## Usage

``` r
tidy_malaria_raster_names(dir)
```

## Arguments

- dir:

  Character scalar. Directory containing downloaded `.tiff` files.

## Value

Invisibly, a tibble with columns `old_name` and `new_name` for files
that were renamed. Files already in canonical form are skipped.

## Examples

``` r
if (FALSE) { # \dontrun{
# Typical workflow with malariaAtlas
malariaAtlas::getRaster(
  dataset_id = "Malaria__202508_Global_Pf_Parasite_Rate",
  year = 2010:2020,
  file_path = "data/maps",
  shp = admin_boundaries
)

# Clean the filenames immediately after download
tidy_malaria_raster_names("data/maps")

# Now process with standard functions
sntutils::process_weighted_raster_collection(...)
} # }
```
