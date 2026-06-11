# Build EMOD Demographics JSON for a Single Node

Creates the demographics list structure expected by EMOD for one admin
unit. Output matches the emod-api Python `from_template_node()` format.

## Usage

``` r
build_emod_demog(
  lat,
  lon,
  pop,
  name,
  crude_death_rate,
  crude_birth_rate,
  node_id = 1L,
  id_reference = "Legacy",
  age_distribution = NULL,
  individual_properties = NULL,
  initial_prevalence = 0,
  prevalence_distribution = c(0.2, 0.2)
)
```

## Arguments

- lat:

  Numeric. Latitude of the node centroid.

- lon:

  Numeric. Longitude of the node centroid.

- pop:

  Integer. Initial population for the node.

- name:

  Character. Name for the node (FacilityName).

- crude_death_rate:

  Numeric. Annual deaths per 1000.

- crude_birth_rate:

  Numeric. Annual births per 1000.

- node_id:

  Integer. Node ID. Default 1L. Must match the NodeID used in climate
  .bin.json files.

- id_reference:

  Character. IdReference string. Default "Legacy". Must match the
  IdReference in climate .bin.json.

- age_distribution:

  List with DistributionValues, ResultScaleFactor, ResultValues. NULL
  uses SSAfrica default.

- individual_properties:

  List of IP definitions. NULL uses default AgeGroup, DrugStatus,
  SMCAccess, VaccineStatus.

- initial_prevalence:

  Numeric. Default 0.

- prevalence_distribution:

  Numeric vector of length 2. Default c(0.2, 0.2).

## Value

A named list representing the demographics JSON.
