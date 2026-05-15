# sntutils 1.12.6

## New features

* `tidy_malaria_raster_names()` normalizes filenames emitted by
  `malariaAtlas::getRaster()` (strips bounding-box coordinates and download
  stamps, keeps `...Rate.YYYY.tiff`). Idempotent; also removes the
  `getRaster/` artifact directory when it renames files.
* `extract_time_components()` gains a `year_extractor` argument. Pass a
  custom function (filename -> year) when the default "last plausible
  4-digit year in 1980-2099" heuristic picks the wrong token.
* `process_raster_collection()` and `process_weighted_raster_collection()`
  expose `year_extractor` as a pass-through to `extract_time_components()`.

## Breaking changes

* `detect_time_pattern()` now uses `base::all()` instead of `base::any()`
  when deciding whether a batch of filenames belongs to a given time
  pattern. Batches containing files of mixed granularity no longer get
  silently misclassified as the most specific pattern present. Callers
  relying on the previous lenient behaviour should pre-filter their file
  lists.
* `detect_time_pattern()` tightens its yearly regex to plausible years
  only (`19[89]\d` or `20\d{2}`) with digit word boundaries, so tokens
  like `202005` (a `YYYYMM` stamp) no longer match as a year.

## Internals

* `process_weighted_raster_collection()` and `process_raster_collection()`
  now iterate via `purrr::map()` with `.progress = ...`, replacing the
  hand-rolled `progress::progress_bar` and `for` loop. Population rasters
  are matched using word-boundary regex (`(?<!\d)YYYY(?!\d)`) so year
  substrings no longer false-match longer digit runs.
* `purrr` (>= 1.0.0) moved from `Suggests` to `Imports`.
