# ── Internal helpers ──────────────────────────────────────────────────────────

#' Parse EMOD NodeOffsets hex string into integer node IDs
#'
#' Each entry is 16 hex chars: 8-char node_id + 8-char byte-offset
#' (big-endian hex, matching Python's `f"{node_id:08x}{offset:08x}"`).
#'
#' @param hex_str The NodeOffsets hex string from the .json metadata.
#' @return Integer vector of node IDs.
#' @noRd
.parse_node_offsets <- function(hex_str) {
  n_entries <- nchar(hex_str) %/% 16L
  node_ids <- integer(n_entries)
  for (i in seq_len(n_entries)) {
    idx <- (i - 1L) * 16L
    node_ids[i] <- strtoi(substr(hex_str, idx + 1L, idx + 8L), base = 16L)
  }
  node_ids
}

#' Build EMOD NodeOffsets hex string from node IDs
#'
#' Matches Python's `f"{node_id:08x}{offset:08x}"` encoding
#' (big-endian hex, 16 chars per entry).
#'
#' @param node_ids Integer vector of node IDs.
#' @param n_steps  Number of time steps per node.
#' @return A single hex string encoding (node_id, byte_offset) pairs.
#' @noRd
.build_node_offsets_hex <- function(node_ids, n_steps) {
  hex_parts <- vapply(seq_along(node_ids), function(i) {
    byte_offset <- (i - 1L) * n_steps * 4L
    sprintf("%08x%08x", as.integer(node_ids[i]), as.integer(byte_offset))
  }, character(1))
  paste0(hex_parts, collapse = "")
}

#' Create default EMOD weather attributes (metadata)
#'
#' Produces the full JSON schema matching Python's `emodpy_malaria.weather`
#' output. Auto-calculated fields (`NodeCount`, `DatavaluePerCell`, etc.)
#' are filled in by [write_emod_weather()] before writing.
#'
#' @param n_steps Number of time steps per node (default 365).
#' @param n_nodes Number of nodes.
#' @param id_reference EMOD ID reference string.
#' @param update_resolution Climate update resolution string.
#' @param start_day_of_year Starting day of year (default 1 = Jan 1).
#' @param data_provenance Data provenance description.
#' @param original_data_years Years string (e.g. "2018-2025").
#' @return A list matching the EMOD .json metadata structure.
#' @noRd
.create_emod_weather_attributes <- function(
    n_steps = 365L,
    n_nodes = 1L,
    id_reference = "Legacy",
    update_resolution = "CLIMATE_UPDATE_DAY",
    start_day_of_year = 1L,
    data_provenance = "NASA POWER via sntutils",
    original_data_years = "Unspecified"
) {
  list(
    Metadata = list(
      Tool = "emodpy_malaria",
      DateCreated = format(Sys.time(), "%Y-%m-%d"),
      Author = "AHADI",
      IdReference = id_reference,
      WeatherSchemaVersion = "2.0",
      UpdateResolution = update_resolution,
      OriginalDataYears = original_data_years,
      StartDayOfYear = start_day_of_year,
      DataProvenance = data_provenance,
      Resolution = "Unspecified",
      WeatherCellCount = n_nodes,
      NumberDTKNodes = n_nodes,
      NodeCount = n_nodes,
      OffsetEntryCount = n_nodes,
      DatavaluePerCell = n_steps,
      DatavalueCount = n_steps
    ),
    NodeOffsets = ""
  )
}

# ── Exported functions ────────────────────────────────────────────────────────

#' Read EMOD weather binary files into a data.frame
#'
#' R equivalent of Python's `emodpy_malaria.weather.weather_to_csv()`.
#' Reads paired `.bin` + `.bin.json` files for each weather variable.
#'
#' @param weather_dir Directory containing the weather files.
#' @param weather_file_prefix File prefix (e.g. `"seasonal"`).
#'
#' @return A list with:
#' \describe{
#'   \item{data}{Tibble with columns `node_id`, `steps`,
#'     `airtemp`, `humidity`, `rainfall`, `landtemp`.}
#'   \item{attributes}{Parsed JSON metadata (list).}
#' }
#'
#' @export
read_emod_weather <- function(weather_dir, weather_file_prefix = "seasonal") {
  rlang::check_installed("jsonlite", reason = "to read EMOD .json metadata")

  var_suffixes <- c(
    airtemp  = "air_temperature_daily",
    humidity = "relative_humidity_daily",
    rainfall = "rainfall_daily",
    landtemp = "land_temperature_daily"
  )

  # read JSON metadata from first variable's .json file
  json_path <- file.path(
    weather_dir,
    paste0(weather_file_prefix, "_", var_suffixes[1], ".bin.json")
  )
  if (!file.exists(json_path)) {
    cli::cli_abort("JSON metadata not found: {.path {json_path}}")
  }
  attrs <- jsonlite::fromJSON(json_path)
  n_steps <- as.integer(attrs$Metadata$DatavalueCount)

  # parse NodeOffsets -> node IDs
  node_ids <- .parse_node_offsets(attrs$NodeOffsets)
  n_nodes <- length(node_ids)

  # read each .bin file (float32 LE)
  var_values <- lapply(var_suffixes, function(suffix) {
    bin_path <- file.path(
      weather_dir,
      paste0(weather_file_prefix, "_", suffix, ".bin")
    )
    if (!file.exists(bin_path)) {
      cli::cli_abort("Binary file not found: {.path {bin_path}}")
    }
    con <- file(bin_path, "rb")
    on.exit(close(con))
    readBin(
      con,
      what = "double", n = n_nodes * n_steps, size = 4L, endian = "little"
    )
  })

  # assemble tidy data.frame (node-major order)
  df <- tibble::tibble(
    node_id = rep(node_ids, each = n_steps),
    steps   = rep(seq_len(n_steps), times = n_nodes)
  )
  for (nm in names(var_suffixes)) {
    df[[nm]] <- var_values[[nm]]
  }

  list(data = df, attributes = attrs)
}

#' Write EMOD binary weather files (.bin + .json) from a data.frame
#'
#' R equivalent of Python's `emodpy_malaria.weather.csv_to_weather()`.
#' Writes one `.bin` / `.bin.json` pair per weather variable.
#'
#' @param df Data.frame with columns `node_id`, `steps`,
#'   `airtemp`, `humidity`, `rainfall`, `landtemp`.
#' @param weather_dir Output directory for the binary files.
#' @param climate_profile Name prefix for the output files.
#' @param attributes Optional list of EMOD JSON metadata (from
#'   [read_emod_weather()]). If `NULL`, attributes are created automatically
#'   from the data.
#' @param id_reference EMOD ID reference (used when `attributes` is NULL).
#' @param update_resolution Climate update resolution
#'   (used when `attributes` is NULL).
#'
#' @return The output directory path (invisibly).
#'
#' @export
write_emod_weather <- function(
    df,
    weather_dir,
    climate_profile,
    attributes = NULL,
    id_reference = "Legacy",
    update_resolution = "CLIMATE_UPDATE_DAY"
) {
  rlang::check_installed("jsonlite", reason = "to write EMOD .json metadata")

  dir.create(weather_dir, recursive = TRUE, showWarnings = FALSE)

  var_suffixes <- c(
    airtemp  = "air_temperature_daily",
    humidity = "relative_humidity_daily",
    rainfall = "rainfall_daily",
    landtemp = "land_temperature_daily"
  )

  # ensure sorted node-major
  df <- df[order(df$node_id, df$steps), ]

  node_ids <- sort(unique(df$node_id))
  n_steps <- max(df$steps)
  n_nodes <- length(node_ids)

  # build attributes internally if not supplied
  if (is.null(attributes)) {
    attributes <- .create_emod_weather_attributes(
      n_steps = n_steps,
      n_nodes = n_nodes,
      id_reference = id_reference,
      update_resolution = update_resolution
    )
  }

  # update auto-calculated metadata fields
  attributes$Metadata$DatavalueCount <- n_steps
  attributes$Metadata$DatavaluePerCell <- n_steps
  attributes$Metadata$WeatherCellCount <- n_nodes
  attributes$Metadata$NodeCount <- n_nodes
  attributes$Metadata$NumberDTKNodes <- n_nodes
  attributes$Metadata$OffsetEntryCount <- n_nodes
  attributes$NodeOffsets <- .build_node_offsets_hex(node_ids, n_steps)

  # write each variable as .bin + .bin.json
  for (var_name in names(var_suffixes)) {
    suffix <- var_suffixes[[var_name]]
    bin_path <- file.path(
      weather_dir,
      paste0(climate_profile, "_", suffix, ".bin")
    )
    json_path <- paste0(bin_path, ".json")

    # binary: float32 little-endian
    con <- file(bin_path, "wb")
    tryCatch(
      writeBin(
        as.double(df[[var_name]]), con, size = 4L, endian = "little"
      ),
      finally = close(con)
    )

    # json metadata
    jsonlite::write_json(
      attributes, json_path, auto_unbox = TRUE, pretty = TRUE
    )
  }

  # show path relative to project root (01_data/... instead of full abs path)
  rel_path <- sub("^.*?(01_data/)", "\\1", weather_dir)
  cli::cli_alert_success("EMOD weather files written to {.path {rel_path}}")
  invisible(weather_dir)
}

#' Write EMOD weather files per adm2 (one folder per district)
#'
#' Wrapper around [write_emod_weather()] that splits a multi-node climate
#' data.frame by adm2, writing each as a single-node EMOD weather set in
#' its own subfolder.
#'
#' @param df Data.frame with columns `node_id`, `steps`,
#'   `airtemp`, `humidity`, `rainfall`, `landtemp`.
#' @param node_coord Data.frame mapping `node_id` to `adm2`
#'   (e.g. from shapefile centroids).
#' @param weather_dir Base output directory. Each adm2 gets a subfolder.
#' @param ... Additional arguments passed to [write_emod_weather()].
#'
#' @return The base output directory path (invisibly).
#'
#' @export
write_emod_weather_by_adm2 <- function(df, node_coord, weather_dir, ...) {
  rlang::check_installed("purrr", reason = "to iterate over adm2 nodes")

  dots <- list(...)
  cli::cli_process_start("Writing EMOD weather files per adm2")

  purrr::pwalk(node_coord, function(node_id, adm2, ...) {
    adm2_clean <- tolower(gsub("[^A-Za-z0-9]+", "_", adm2)) |>
      stringr::str_to_title()

    adm2_df <- df[df$node_id == node_id, ]
    adm2_df$node_id <- 1L

    do.call(write_emod_weather, c(
      list(
        df = adm2_df,
        weather_dir = file.path(weather_dir, adm2_clean),
        climate_profile = adm2_clean
      ),
      dots
    ))
  })

  cli::cli_process_done()
  invisible(weather_dir)
}
