# Consistency Check Function

This function performs logical consistency checks to ensure that the
number of inputs is greater than or equal to the number of outputs for
given columns in a dataset. It returns a ggplot2 object visualizing the
results and alerts the user to any inconsistencies where outputs exceed
inputs.

## Usage

``` r
consistency_check(
  data,
  inputs = NULL,
  outputs = NULL,
  tests = NULL,
  cases = NULL,
  plot_path = NULL,
  target_language = "en",
  source_language = "en",
  lang_cache_path = tempdir(),
  compress_image = FALSE,
  image_overwrite = TRUE,
  compression_speed = 1,
  compression_verbose = TRUE,
  plot_scale = 1,
  plot_width = NULL,
  plot_height = NULL,
  plot_dpi = 300,
  show_plot = TRUE,
  facet_by = NULL
)
```

## Arguments

- data:

  A data frame containing the input and output data.

- inputs:

  A character vector specifying the column names for the input data
  (e.g., malaria tests, all outpatients, suspected cases). These
  represent upstream events in the care cascade.

- outputs:

  A character vector specifying the column names for the output data
  (e.g., confirmed cases, malaria admissions, malaria deaths). These
  represent downstream events in the care cascade. The length of
  'inputs' and 'outputs' must be the same, and each element in 'inputs'
  corresponds to an element in 'outputs'.

- tests:

  **[Deprecated](https://rdrr.io/r/base/Deprecated.html)** Use `inputs`
  instead. A character vector specifying the column names for the test
  data.

- cases:

  **[Deprecated](https://rdrr.io/r/base/Deprecated.html)** Use `outputs`
  instead. A character vector specifying the column names for the case
  data.

- plot_path:

  Character. Directory path where the plot should be saved. If NULL
  (default), plot is not saved.

- target_language:

  A character string specifying the language for plot labels. Defaults
  to "en" (English). Use ISO 639-1 language codes.

- source_language:

  Source language code, defaults to "en"

- lang_cache_path:

  Path for translation cache, defaults to tempdir()

- compress_image:

  Logical. If TRUE, the saved image will be compressed. Default is FALSE

- image_overwrite:

  Logical. If TRUE, existing image will be overwritten. Default is TRUE.

- compression_speed:

  Integer from 1-10. Speed of compression (1=slow/high compression).
  Default is 1.

- compression_verbose:

  Logical. If TRUE, compression details will be printed. Default is
  TRUE.

- plot_scale:

  Numeric. Scaling factor for saved plots. Values \> 1 increase size, \<
  1 decrease size. Default is 1.

- plot_width:

  Numeric. Width of saved plot in inches. If NULL (default), width is
  calculated based on number of variables.

- plot_height:

  Numeric. Height of saved plot in inches. If NULL (default), height is
  calculated based on number of variables.

- plot_dpi:

  Numeric. Resolution of saved plot in dots per inch. Default is 300.

- show_plot:

  Logical. If FALSE, the plot is returned invisibly (not displayed).
  Useful when only saving plots. Default is TRUE.

- facet_by:

  Character. Column name to facet the plot by (e.g., "year", "month",
  "adm1"). When provided, only the first input/output pair is processed
  and the plot is faceted by this variable instead of by comparison.
  Useful for examining consistency patterns across time or geographical
  units. Default is NULL (facet by comparison).

## Value

A ggplot2 object showing the consistency between the number of inputs
and outputs. The x-axis represents the outputs, and the y-axis
represents the inputs. Each facet represents a variable pair, and the
diagonal line shows where the number of inputs equals the number of
outputs.

## Details

For malaria surveillance data, this function validates the logical
coherence of the care cascade by ensuring upstream events (inputs) are
greater than or equal to downstream events (outputs). Common malaria
data validation checks include:

**Common malaria variable definitions:**

- `allout` = all outpatients

- `susp` = suspected malaria cases

- `test` = malaria tests (or tested for malaria)

- `conf` = confirmed malaria cases

- `maltreat` = malaria cases treated

- `alladm` = all admissions (all hospital admissions)

- `maladm` = malaria admissions (hospital admissions for malaria)

- `alldth` = all deaths (total deaths)

- `maldth` = malaria deaths

**Typical logical validation checks:**

- susp vs test: suspected malaria \>= malaria tests

- allout vs susp: all outpatients \>= suspected malaria

- allout vs test: all outpatients \>= malaria tests

- test vs conf: malaria tests \>= confirmed malaria cases

- conf vs maltreat: confirmed malaria cases \>= malaria treated

- alladm vs maladm: all admissions \>= malaria admissions

- alldth vs maldth: all deaths \>= malaria deaths

- maladm vs maldth: malaria admissions \>= malaria deaths

## Examples

``` r
# Basic consistency checks for malaria surveillance data
if (FALSE) { # \dontrun{
# get path
path <- system.file(
  "extdata",
  "fake_epi_df_togo.rds",
  package = "epiCleanr"
)

# get example data
fake_epi_df_togo <- sntutils::read(path)

# Example 1: Check test-to-confirmation cascade
# Validates: malaria tests >= confirmed malaria cases
consistency_check(
  fake_epi_df_togo,
  inputs = c("malaria_tests"),
  outputs = c("malaria_cases")
)

# Example 2: Multiple validation checks at once
# Checks common malaria care cascade validations
consistency_check(
  fake_epi_df_togo,
  inputs = c("test", "allout", "conf", "alladm", "alldth", "maladm"),
  outputs = c("conf", "susp", "maltreat", "maladm", "maldth", "maldth")
)

# Example 3: Save the plot to disk
consistency_check(
  fake_epi_df_togo,
  inputs = c("malaria_tests", "all_outpatients"),
  outputs = c("malaria_cases", "suspected_malaria"),
  plot_path = tempdir()
)

# Example 4: Validate with multiple diseases
consistency_check(
  fake_epi_df_togo,
  inputs = c("malaria_tests", "cholera_tests"),
  outputs = c("malaria_cases", "cholera_cases")
)

# Example 5: Facet by year for time-series analysis (single pair only)
consistency_check(
  fake_epi_df_togo,
  inputs = c("malaria_tests"),
  outputs = c("malaria_cases"),
  facet_by = "year"
)

# Example 6: Facet by administrative unit (single pair only)
consistency_check(
  fake_epi_df_togo,
  inputs = c("test"),
  outputs = c("conf"),
  facet_by = "adm1"
)

# Old syntax (deprecated but still supported for backward
# compatibility)
consistency_check(
  fake_epi_df_togo,
  tests = c("malaria_tests"),
  cases = c("malaria_cases")
)
} # }
```
