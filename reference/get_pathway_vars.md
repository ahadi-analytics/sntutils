# Get Upstream and Downstream Variables for Malaria Pathway Indicators

Determines upstream and downstream variables for a given malaria
indicator based on the clinical care pathway. Structural imputation is
only allowed for indicators that follow predictable relationships in the
pathway.

## Usage

``` r
get_pathway_vars(
  var_to_impute,
  malaria_pathway = c("susp", "test", "conf", "maltreat", "maladm", "maldth"),
  facility_type = NULL,
  suffix = NULL
)
```

## Arguments

- var_to_impute:

  Character. Variable name to analyze (e.g., "conf"). Do not include
  suffix here; suffix is appended separately if provided.

- malaria_pathway:

  Character vector. Ordered indicator sequence.

- facility_type:

  Character or NULL. Facility type. Used to restrict imputation for
  admissions ('maladm'), which only apply to inpatient- capable
  facilities. Examples include: "hospital", "referral", "inpatient",
  "health_facility" or "tertiary". All other facility types (e.g.,
  "health_post", "clinic") are treated as outpatient-only.

- suffix:

  Character or NULL. Optional suffix to append to variable names in
  output (e.g., "\_hf" to return "test_hf", "conf_hf").

## Value

A list with:

- var_to_impute:

  Indicator with suffix applied.

- upstream_vars:

  Indicators that logically precede it.

- downstream_vars:

  Indicators that logically follow it.

- use_structural:

  Logical. TRUE if structural imputation is allowed.

## Details

Assumes variable names match standard: 'susp', 'test', 'conf',
'maltreat', 'maladm', 'maldth'.

Indicator usage in imputation logic depends on facility type. Admissions
('maladm') are only included at inpatient-capable facilities.

+————+—————–+——————–+———————–+ \| Indicator \| Facility Type \| Upstream
Used \| Downstream Used \| +————+—————–+——————–+———————–+ \| susp \| All
\| — \| test, conf, maladm\*, \| \| \| \| \| maldth \| \| test \| All \|
susp \| conf, maladm\*, maldth \| \| conf \| All \| susp, test \|
maladm\*, maldth \| \| maltreat \| All \| susp, test, conf \| maladm\*,
maldth \| \| maladm \| Inpatient only \| susp, test, conf \| maldth \|
\| maladm \| Outpatient only \| — \| — \| \| maldth \| All \| susp,
test, conf, \| \| \| \| \| maladm\* \| — \|
+————+—————–+——————–+———————–+

- Only used if facility is inpatient-capable

## Examples

``` r
get_pathway_vars("test")
#> 
#> ── Malaria Pathway Summary ─────────────────────────────────────────────────────
#> ℹ Imputing for test
#> ✔ Upstream: susp
#> ✔ Downstream: conf, maldth
#> ℹ Note: "maladm" excluded from dependency logic due to outpatient facility.
#> ℹ Note: "maltreat" excluded from dependency logic due to presumptive treatment.
#> 
#> $var_to_impute
#> [1] "test"
#> 
#> $upstream_vars
#> [1] "susp"
#> 
#> $downstream_vars
#> [1] "conf"   "maldth"
#> 
#> $use_structural
#> [1] TRUE
#> 
get_pathway_vars("conf", suffix = "_hf")
#> 
#> ── Malaria Pathway Summary ─────────────────────────────────────────────────────
#> ℹ Imputing for conf_hf
#> ✔ Upstream: susp_hf, test_hf
#> ✔ Downstream: maldth_hf
#> ℹ Note: "maladm_hf" excluded from dependency logic due to outpatient facility.
#> ℹ Note: "maltreat_hf" excluded from dependency logic due to presumptive treatment.
#> 
#> $var_to_impute
#> [1] "conf_hf"
#> 
#> $upstream_vars
#> [1] "susp_hf" "test_hf"
#> 
#> $downstream_vars
#> [1] "maldth_hf"
#> 
#> $use_structural
#> [1] TRUE
#> 
get_pathway_vars("maltreat")
#> 
#> ── Malaria Pathway Summary ─────────────────────────────────────────────────────
#> ℹ Imputing for maltreat
#> ✔ Upstream: susp, test, conf
#> ✔ Downstream: maldth
#> ℹ Note: "maladm" excluded from dependency logic due to outpatient facility.
#> 
#> $var_to_impute
#> [1] "maltreat"
#> 
#> $upstream_vars
#> [1] "susp" "test" "conf"
#> 
#> $downstream_vars
#> [1] "maldth"
#> 
#> $use_structural
#> [1] TRUE
#> 
get_pathway_vars("maladm", facility_type = "hospital")
#> 
#> ── Malaria Pathway Summary ─────────────────────────────────────────────────────
#> ℹ Imputing for maladm
#> ✔ Upstream: susp, test, conf
#> ✔ Downstream: maldth
#> ℹ Note: "maltreat" excluded from dependency logic due to presumptive treatment.
#> 
#> $var_to_impute
#> [1] "maladm"
#> 
#> $upstream_vars
#> [1] "susp" "test" "conf"
#> 
#> $downstream_vars
#> [1] "maldth"
#> 
#> $use_structural
#> [1] TRUE
#> 
get_pathway_vars("maladm", facility_type = "health_post")
#> 
#> ── Malaria Pathway Summary ─────────────────────────────────────────────────────
#> ℹ Imputing for maladm
#> ✖ Admissions not applicable at outpatient facilities (e.g., health post).
#> ! Structural imputation excluded
#> ✔ Upstream: None
#> ✔ Downstream: None
#> $var_to_impute
#> [1] "maladm"
#> 
#> $upstream_vars
#> character(0)
#> 
#> $downstream_vars
#> character(0)
#> 
#> $use_structural
#> [1] FALSE
#> 
```
