#' Clean shapefile boundary artefacts and slivers
#'
#' Diagnoses and fixes visual artefacts (sliver polygons, choppy black lines)
#' in shapefiles by applying a sequential set of repairs: precision snapping,
#' zero-buffer topology fix, sliver removal, and optionally a full topology
#' rebuild. Returns a cleaned sf object and a diagnostic summary list.
#'
#' @param shp_path Character. Path to the input shapefile or geojson.
#' @param metric_crs Integer. EPSG code for metric CRS used for area
#'   calculations. Default 32736 (UTM 36S, East Africa).
#' @param sliver_threshold_km2 Numeric. Area threshold in km² below which
#'   polygons are considered slivers. Default 0.01.
#' @param snap_precision Numeric. Precision value passed to
#'   `sf::st_set_precision()`. Default 1e5 (~1m grid).
#' @param group_col Character or NULL. Column name to group by when rebuilding
#'   topology (nuclear option). If NULL, rebuild step is skipped.
#'
#' @return A named list with:
#'   \item{shp_clean}{Cleaned sf object in original CRS}
#'   \item{diagnostics}{List of diagnostic counts pre-fix}
#'   \item{slivers_removed}{Integer count of slivers removed}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' result <- clean_boundary_artefacts(
#'   shp_path = here::here("data", "raw", "districts.geojson"),
#'   metric_crs = 32736,
#'   sliver_threshold_km2 = 0.01,
#'   snap_precision = 1e5,
#'   group_col = NULL
#' )
#' plot(sf::st_geometry(result$shp_clean))
#' }
clean_boundary_artefacts <- function(
  shp_path,
  metric_crs = 32736,
  sliver_threshold_km2 = 0.01,
  snap_precision = 1e5,
  group_col = NULL
) {
  # validate inputs
  if (!is.character(shp_path) || base::length(shp_path) != 1) {
    cli::cli_abort("shp_path must be a single character string")
  }
  if (!base::file.exists(shp_path)) {
    cli::cli_abort("file not found: {shp_path}")
  }
  if (!is.numeric(metric_crs)) {
    cli::cli_abort("metric_crs must be numeric")
  }
  if (!is.numeric(sliver_threshold_km2) || sliver_threshold_km2 < 0) {
    cli::cli_abort("sliver_threshold_km2 must be a positive number")
  }
  if (!is.numeric(snap_precision) || snap_precision <= 0) {
    cli::cli_abort("snap_precision must be a positive number")
  }

  # read shapefile
  shp <- sf::st_read(shp_path, quiet = TRUE)
  original_crs <- sf::st_crs(shp)

  # run diagnostics
  diagnostics <- .diagnose_boundary_issues(
    shp = shp,
    metric_crs = metric_crs,
    sliver_threshold_km2 = sliver_threshold_km2
  )

  # display diagnostics
  cli::cli_h1("shapefile diagnostics")
  cli::cli_alert_info("invalid geometries : {diagnostics$n_invalid}")
  cli::cli_alert_info(
    "slivers (< {sliver_threshold_km2} km\u00b2) : {diagnostics$n_slivers}"
  )
  cli::cli_alert_info(
    "overlapping polygons : {diagnostics$n_overlapping}"
  )
  cli::cli_alert_info(
    "coord precision \u2014 lon: {diagnostics$lon_precision} | ",
    "lat: {diagnostics$lat_precision}"
  )

  # apply fixes
  cli::cli_h1("applying fixes")

  shp_fixed <- .apply_precision_snap(shp, snap_precision)
  shp_fixed <- .apply_zero_buffer(shp_fixed)

  sliver_result <- .remove_slivers(
    shp = shp_fixed,
    metric_crs = metric_crs,
    sliver_threshold_km2 = sliver_threshold_km2,
    original_crs = original_crs
  )
  shp_fixed <- sliver_result$shp
  slivers_removed <- sliver_result$slivers_removed

  if (!is.null(group_col)) {
    shp_fixed <- .rebuild_topology(
      shp = shp_fixed,
      group_col = group_col,
      snap_precision = snap_precision
    )
  }

  # post-fix summary
  n_invalid_post <- base::sum(!sf::st_is_valid(shp_fixed), na.rm = TRUE)
  cli::cli_h1("post-fix summary")
  cli::cli_alert_success("slivers removed     : {slivers_removed}")
  cli::cli_alert_success("remaining invalid   : {n_invalid_post}")

  base::list(
    shp_clean = shp_fixed,
    diagnostics = diagnostics,
    slivers_removed = slivers_removed
  )
}

#' Diagnose boundary issues in shapefile
#'
#' @param shp sf object
#' @param metric_crs integer EPSG code
#' @param sliver_threshold_km2 numeric threshold
#'
#' @return list of diagnostic metrics
#' @noRd
.diagnose_boundary_issues <- function(
  shp,
  metric_crs,
  sliver_threshold_km2
) {
  # count invalid geometries
  n_invalid <- base::sum(!sf::st_is_valid(shp), na.rm = TRUE)

  # transform to metric and calculate areas
  shp_metric <- shp |>
    sf::st_transform(crs = metric_crs) |>
    dplyr::mutate(
      area_km2 = base::as.numeric(sf::st_area(geometry)) / 1e6
    )

  # count slivers
  n_slivers <- shp_metric |>
    dplyr::filter(area_km2 < sliver_threshold_km2) |>
    base::nrow()

  # count overlapping polygons
  n_overlapping <- shp |>
    sf::st_overlaps() |>
    base::lengths() |>
    (\(x) base::sum(x > 0))()

  # check coordinate precision
  coords <- sf::st_coordinates(shp)
  decimal_places <- function(x) {
    base::nchar(base::sub(".*\\.", "", base::as.character(x)))
  }
  lon_precision <- stats::median(decimal_places(coords[, "X"]))
  lat_precision <- stats::median(decimal_places(coords[, "Y"]))

  base::list(
    n_invalid = n_invalid,
    n_slivers = n_slivers,
    n_overlapping = n_overlapping,
    lon_precision = lon_precision,
    lat_precision = lat_precision
  )
}

#' Apply precision snapping to coordinates
#'
#' @param shp sf object
#' @param snap_precision numeric precision value
#'
#' @return sf object with snapped coordinates
#' @noRd
.apply_precision_snap <- function(shp, snap_precision) {
  cli::cli_alert("fix 1: snapping coordinates to precision grid...")

  shp |>
    sf::st_set_precision(snap_precision) |>
    sf::st_make_valid()
}

#' Apply zero-buffer topology clean
#'
#' @param shp sf object
#'
#' @return sf object with cleaned topology
#' @noRd
.apply_zero_buffer <- function(shp) {
  cli::cli_alert("fix 2: zero-buffer topology repair...")

  shp |>
    sf::st_buffer(dist = 0) |>
    sf::st_make_valid()
}

#' Remove sliver polygons
#'
#' @param shp sf object
#' @param metric_crs integer EPSG code
#' @param sliver_threshold_km2 numeric threshold
#' @param original_crs crs object
#'
#' @return list with cleaned sf object and count of slivers removed
#' @noRd
.remove_slivers <- function(
  shp,
  metric_crs,
  sliver_threshold_km2,
  original_crs
) {
  cli::cli_alert("fix 3: removing slivers...")

  # transform to metric and calculate areas
  shp_metric <- shp |>
    sf::st_transform(crs = metric_crs) |>
    dplyr::mutate(
      area_km2 = base::as.numeric(sf::st_area(geometry)) / 1e6
    )

  # remove slivers
  n_before <- base::nrow(shp_metric)
  shp_metric <- shp_metric |>
    dplyr::filter(area_km2 >= sliver_threshold_km2)
  slivers_removed <- n_before - base::nrow(shp_metric)

  # transform back to original crs
  shp_clean <- shp_metric |>
    sf::st_transform(crs = original_crs)

  base::list(
    shp = shp_clean,
    slivers_removed = slivers_removed
  )
}

#' Rebuild topology by grouping
#'
#' @param shp sf object
#' @param group_col character column name
#' @param snap_precision numeric precision value
#'
#' @return sf object with rebuilt topology
#' @noRd
.rebuild_topology <- function(shp, group_col, snap_precision) {
  cli::cli_alert(
    "fix 4: rebuilding topology by group column '{group_col}'..."
  )

  shp |>
    sf::st_set_precision(snap_precision) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_col))) |>
    dplyr::summarise(
      geometry = sf::st_union(geometry),
      .groups = "drop"
    ) |>
    sf::st_make_valid()
}
