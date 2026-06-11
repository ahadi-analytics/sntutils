# Detect and display the structural components of an SNT variable name

Parses a standardized SNT variable name (e.g. "conf_rdt_u5_priv") and
prints its logical components and bilingual label using CLI formatting.

## Usage

``` r
check_snt_var(
  var_name,
  schema = NULL,
  var_tree = NULL,
  return = FALSE,
  verbose = TRUE
)
```

## Arguments

- var_name:

  Character scalar - an SNT variable name.

- schema:

  Optional list; if not provided, automatically loaded from the
  package's snt_var_tree dataset.

- var_tree:

  Optional list; if not provided, automatically loaded from the
  package's snt_var_tree dataset.

- return:

  Logical; if TRUE, also returns a tibble invisibly.

- verbose:

  Logical; if TRUE (default), prints fuzzy match info messages.

## Value

Invisibly returns a tibble with parsed components and labels.

## Details

Dynamic age groups are supported for DHS ITN metrics. Variables
following patterns like `dhs_itn_use_0_5`, `dhs_itn_access_20_plus`, or
`dhs_n_individuals_15_49` are automatically recognized and labeled with
appropriate age range text in English, French, and Portuguese.

## Examples

``` r
# Dynamic DHS ITN age groups
check_snt_var("dhs_itn_use_0_5")
#> 
#> ── Detected variable structure ─────────────────────────────────────────────────
#> Variable: "dhs_itn_use_0_5"
#> Label (EN): DHS ITN use (0-5 years) (%)
#> Label (FR): Utilisation de MII DHS (0-5 ans) (%)
#> Label (PT): Uso de MILD DHS (0-5 anos) (%)
#> Domain: "dhs"
check_snt_var("dhs_itn_access_20_plus")
#> 
#> ── Detected variable structure ─────────────────────────────────────────────────
#> Variable: "dhs_itn_access_20_plus"
#> Label (EN): DHS ITN access (20+ years) (%)
#> Label (FR): Accès aux MII DHS (20+ ans) (%)
#> Label (PT): Acesso a MILD DHS (20+ anos) (%)
#> Domain: "dhs"
check_snt_var("dhs_n_individuals_5_10_low")
#> 
#> ── Detected variable structure ─────────────────────────────────────────────────
#> Variable: "dhs_n_individuals_5_10_low"
#> Label (EN): Dhs
#> Label (FR): Dhs
#> Label (PT): Dhs
#> Domain: "dhs"
```
