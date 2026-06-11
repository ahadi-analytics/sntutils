# Articles

### Getting started

- [Get started with
  sntutils](https://ahadi-analytics.github.io/sntutils/articles/getting-started.md):

  A 5-minute tour of what sntutils does, how the pieces fit together,
  and a small end-to-end example you can read straight through.

### Data preparation

- [Data I/O and
  cleaning](https://ahadi-analytics.github.io/sntutils/articles/data-io-and-cleaning.md):

  Read and write any common SNT format, parse messy date columns, infer
  variable types, standardise admin and facility names, and build a data
  dictionary you can hand to a country team.

- [Spatial validation and
  mapping](https://ahadi-analytics.github.io/sntutils/articles/spatial.md):

  Validate admin geometries and facility coordinates, crosswalk between
  shapefile vintages, fuzzy-match facilities across DHIS2 and the master
  facility list, and render publication-ready maps in the AHADI style.

### Routine surveillance

- [Reporting
  rates](https://ahadi-analytics.github.io/sntutils/articles/reporting-rates.md):

  How completely are health facilities reporting and where are the gaps.
  The three reporting-rate scenarios, the underlying formula, maps,
  facility-level activity classification.

- [Data quality — consistency, outliers,
  imputation](https://ahadi-analytics.github.io/sntutils/articles/data-quality.md):

  Cascade consistency checks, three-method outlier detection, correction
  and imputation for routine surveillance data.

### External data sources

- [Climate downloads and raster
  extraction](https://ahadi-analytics.github.io/sntutils/articles/climate.md):

  Pull rainfall, temperature, land-surface and agro-climate variables
  from CHIRPS, ERA5, MODIS and NASA POWER — then extract them to admin
  units, optionally population-weighted.

- [WorldPop population
  rasters](https://ahadi-analytics.github.io/sntutils/articles/worldpop.md):

  Download WorldPop population rasters — totals, age bands, urbanicity,
  per-country and global mosaics — then extrapolate to missing years and
  reshape into the canonical SNT population table.

- [DHS indicators and
  microdata](https://ahadi-analytics.github.io/sntutils/articles/dhs.md):

  Discover, fetch and query DHS / MIS indicators via the DHS API, and
  open large local DHS parquet datasets through DuckDB for analysis.

### Project plumbing

- [Project setup and
  utilities](https://ahadi-analytics.github.io/sntutils/articles/project-and-utilities.md):

  Scaffold an AHADI project structure, resolve standardised paths,
  translate plot labels with caching, hash records, compress images, and
  use the small numeric helpers that show up in every SNT script.
