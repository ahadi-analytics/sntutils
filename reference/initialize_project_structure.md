# Initialize Full Project Folder Structure

Sets up a clean Ahadi-style project hierarchy:

- 01_data/ with numbered domain folders

- 02_scripts/

- 03_outputs/ with 3.1_validation/, 3.2_intermediate_products/,
  3.3_final_snt_outputs/, 3.4_model/ (each with figures/ and tables/)

- 04_reports/, 05_metadata_docs/

## Usage

``` r
initialize_project_structure(base_path = ".")
```

## Arguments

- base_path:

  Character. Project root directory (default ".").

## Value

NULL (folders created on disk)
