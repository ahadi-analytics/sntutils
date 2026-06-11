# Create Hierarchical Data Folder Structure (AHADI Style)

Creates a domain-sorted data folder hierarchy under 01_data/ using a
three-tier naming system (e.g., d1.3a_worldpop_rasters) with raw/ and
processed/ folders.

## Usage

``` r
create_data_structure(base_path = ".")
```

## Arguments

- base_path:

  Character. base_path project directory (default = current directory)

## Value

NULL (creates folders on disk) Initialize Full Project Folder Structure

This function sets up a clean, hierarchical folder system for Ahadi
projects:

- 01_data/ with nested numbered folders and raw/processed subfolders

- 02_scripts/

- 03_outputs/ with 3.1_validation/, 3.2_intermediate_products/,
  3.3_final_snt_outputs/, 3.4_model/ (each with figures/ and tables/)

- 04_reports/, 05_metadata_docs/

NULL (folders created on disk)
