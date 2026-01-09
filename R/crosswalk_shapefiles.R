#' Create an area-weighted crosswalk between old and new admin polygons
#'
#' Purpose
#' This function constructs a reproducible lookup table linking old and new
#' administrative boundary systems using polygon overlap. It is designed to
#' handle boundary splits and merges explicitly and to support backcasting or
#' forward aggregation of indicators across administrative changes.
#'
#' Weights are area-based and assume uniform distribution within polygons.
#' All geometric operations are performed in a planar equal-area CRS.
#'
#' Inputs
#' @param old_sf sf object. Old administrative boundaries.
#'   Must contain columns adm0, adm1, adm2 and be in EPSG:4326.
#' @param new_sf sf object. New administrative boundaries.
#'   Must contain columns adm0, adm1, adm2 and be in EPSG:4326.
#' @param level character. One of "adm0", "adm1", "adm2".
#'   Determines which administrative level is crosswalked.
#' @param old_suffix character. Suffix appended to old admin names.
#' @param min_weight numeric. Minimum old-to-new area proportion to retain.
#' @param area_crs integer. EPSG code for a planar equal-area CRS.
#' @param keep_overlap_geometry logical. If TRUE, include overlap geometries.
#' @param min_primary_weight numeric. Minimum weight to label as primary.
#' @param min_secondary_weight numeric. Minimum weight for secondary overlaps.
#' @param coverage_tol numeric. Tolerance for area coverage diagnostics.
#' @param verbose logical. If TRUE, print CLI progress messages.
#' @param include_areas logical. If TRUE, include area columns in km2.
#' @param include_reverse_prop logical. If TRUE, include new-to-old proportion.
#'
#' Outputs
#' Returns a list with:
#' - data: data.frame with crosswalk mappings and diagnostics
#' - dictionary: data.frame describing each column
#' - metadata: data.frame with run parameters and timestamps
#' - overlap_geometry: (if keep_overlap_geometry = TRUE) sf object
#'
#' Key columns in data output
#' - overlap_prop: proportion of old unit area overlapping new unit
#' - is_primary: TRUE if dominant mapping for old unit (overlap_prop >= 0.5)
#' - n_new_adm: number of new units overlapping the old unit
#' - split_flag: TRUE if old unit overlaps multiple new units
#' - n_old_per_new: number of old units overlapping the new unit
#' - merge_flag: TRUE if multiple old units merged into new unit
#'
#' Assumptions
#' - Areas are computed in a planar CRS.
#' - Attributes (admin names) are spatially constant within polygons.
#' - Weights are area-based, not population-weighted.
#'
#' @examples
#' \dontrun{
#' cw_adm2 <- crosswalk_shapefiles_sf(
#'   old_sf = adm2_old,
#'   new_sf = adm2_new,
#'   level = "adm2",
#'   min_weight = 0.001,
#'   area_crs = 32735
#' )
#' }
#' @export
crosswalk_shapefiles_sf <- function(
  old_sf,
  new_sf,
  level = c("adm0", "adm1", "adm2"),
  old_suffix = "_old",
  min_weight = 0.001,
  area_crs = 32735,
  keep_overlap_geometry = FALSE,
  min_primary_weight = 0.5,
  min_secondary_weight = 0.10,
  coverage_tol = 0.05,
  verbose = TRUE,
  include_areas = FALSE,
  include_reverse_prop = FALSE
) {
  level <- match.arg(level)

  if (verbose) {
    cli::cli_h1("Building admin crosswalk at {.val {level}} level")
  }

  required_name_cols <- c("adm0", "adm1", "adm2")

  # validate inputs ------------------------------------------------------------

  if (verbose) {
    cli::cli_alert_info("Validating inputs...")
  }

  assert_sf <- function(x, nm) {
    missing_cols <- setdiff(required_name_cols, names(x))

    if (length(missing_cols) > 0) {
      cli::cli_abort(c(
        "missing required columns in {nm}.",
        "x" = "missing: {paste(missing_cols, collapse = ', ')}"
      ))
    }

    if (!inherits(x, "sf")) {
      cli::cli_abort("{nm} must be an sf object.")
    }

    if (is.na(sf::st_crs(x)) || sf::st_crs(x)$epsg != 4326) {
      cli::cli_abort("{nm} must be in epsg:4326.")
    }
  }

  assert_sf(old_sf, "old_sf")
  assert_sf(new_sf, "new_sf")

  if (min_weight < 0 || min_weight >= 1) {
    cli::cli_abort("min_weight must be in [0, 1).")
  }

  if (min_primary_weight <= 0 || min_primary_weight > 1) {
    cli::cli_abort("min_primary_weight must be in (0, 1].")
  }

  # configure level ------------------------------------------------------------

  level_name_cols <- switch(
    level,
    adm0 = "adm0",
    adm1 = c("adm0", "adm1"),
    adm2 = c("adm0", "adm1", "adm2")
  )

  level_name_cols_old <- paste0(level_name_cols, old_suffix)

  # disable spherical geometry locally ----------------------------------------

  # old_s2_state <- sf::sf_use_s2()
  # on.exit(sf::sf_use_s2(old_s2_state), add = TRUE)
  # sf::sf_use_s2(FALSE)

  # prepare geometries ---------------------------------------------------------

  n_old <- nrow(old_sf)
  n_new <- nrow(new_sf)

  if (verbose) {
    cli::cli_alert_info(
      "Preparing geometries (transforming to EPSG:{.val {area_crs}})..."
    )
    cli::cli_bullets(c(
      " " = "Old boundaries: {.val {n_old}} units",
      " " = "New boundaries: {.val {n_new}} units"
    ))
  }

  old_prepped <-
    old_sf |>
    dplyr::select(dplyr::all_of(level_name_cols), geometry) |>
    sf::st_transform(area_crs) |>
    sf::st_make_valid() |>
    dplyr::mutate(
      old_id = dplyr::row_number(),
      area_old = sf::st_area(geometry)
    )

  new_prepped <-
    new_sf |>
    dplyr::select(dplyr::all_of(level_name_cols), geometry) |>
    sf::st_transform(area_crs) |>
    sf::st_make_valid() |>
    dplyr::mutate(
      new_id = dplyr::row_number(),
      area_new = sf::st_area(geometry)
    )

  # geometry-only intersection to avoid attribute warnings --------------------

  if (verbose) {
    cli::cli_alert_info("Computing polygon intersections...")
  }

  overlap_geom <-
    sf::st_intersection(
      sf::st_geometry(old_prepped),
      sf::st_geometry(new_prepped)
    )

  if (length(overlap_geom) == 0) {
    cli::cli_abort("no overlaps found between old and new layers.")
  }

  if (verbose) {
    cli::cli_alert_success(
      "Found {.val {length(overlap_geom)}} overlapping regions"
    )
  }

  # extract geometry column name safely
  geom_col <- attr(sf::st_as_sf(overlap_geom), "sf_column")

  overlap_sf <-
    sf::st_as_sf(overlap_geom) |>
    dplyr::mutate(
      old_id = as.integer(attr(.data[[geom_col]], "idx")[, 1]),
      new_id = as.integer(attr(.data[[geom_col]], "idx")[, 2]),
      overlap_area = sf::st_area(.data[[geom_col]])
    )

  # reattach attributes --------------------------------------------------------

  overlap_sf <-
    overlap_sf |>
    dplyr::left_join(
      old_prepped |>
        sf::st_drop_geometry() |>
        dplyr::rename_with(
          ~ paste0(.x, old_suffix),
          .cols = dplyr::all_of(level_name_cols)
        ),
      by = "old_id"
    ) |>
    dplyr::left_join(
      new_prepped |>
        sf::st_drop_geometry(),
      by = "new_id"
    ) |>
    dplyr::mutate(
      overlap_prop = units::drop_units(overlap_area / area_old),
      weight_new_to_old = units::drop_units(overlap_area / area_new)
    )

  # build crosswalk table ------------------------------------------------------

  if (verbose) {
    cli::cli_alert_info("Building crosswalk table...")
  }

  # determine which columns to keep
  base_cols <- c(
    level_name_cols_old,
    level_name_cols,
    "overlap_prop"
  )

  if (include_areas) {
    base_cols <- c(base_cols, "area_old", "area_new", "overlap_area")
  }

  if (include_reverse_prop) {
    base_cols <- c(base_cols, "weight_new_to_old")
  }

  crosswalk_tbl <-
    overlap_sf |>
    sf::st_drop_geometry() |>
    dplyr::filter(overlap_prop >= min_weight) |>
    dplyr::select(dplyr::all_of(base_cols)) |>
    dplyr::arrange(
      dplyr::across(dplyr::all_of(level_name_cols_old)),
      dplyr::desc(overlap_prop)
    )

  # convert areas to km2 if requested
  if (include_areas) {
    crosswalk_tbl <- crosswalk_tbl |>
      dplyr::mutate(
        area_old_km2 = round(units::drop_units(area_old) / 1e6, 2),
        area_new_km2 = round(units::drop_units(area_new) / 1e6, 2),
        overlap_area_km2 = round(units::drop_units(overlap_area) / 1e6, 2)
      ) |>
      dplyr::select(-area_old, -area_new, -overlap_area)
  }

  # rename reverse prop if requested
  if (include_reverse_prop) {
    crosswalk_tbl <- crosswalk_tbl |>
      dplyr::rename(overlap_prop_reverse = weight_new_to_old) |>
      dplyr::mutate(overlap_prop_reverse = round(overlap_prop_reverse, 2))
  }

  # coverage diagnostics -------------------------------------------------------

  if (verbose) {
    cli::cli_alert_info("Running coverage diagnostics...")
  }

  coverage_check <-
    crosswalk_tbl |>
    dplyr::group_by(dplyr::across(dplyr::all_of(level_name_cols_old))) |>
    dplyr::summarise(
      total_overlap_prop = sum(overlap_prop),
      n_new_adm = dplyr::n(),
      .groups = "drop"
    )

  if (any(abs(coverage_check$total_overlap_prop - 1) > coverage_tol)) {
    cli::cli_warn(
      paste0(
        "some old units have incomplete or excessive area coverage;",
        " inspect geometry."
      )
    )
  }

  # primary mapping and split flags -------------------------------------------

  primary_map <-
    crosswalk_tbl |>
    dplyr::group_by(dplyr::across(dplyr::all_of(level_name_cols_old))) |>
    dplyr::slice_max(overlap_prop, n = 1, with_ties = FALSE) |>
    dplyr::mutate(
      is_primary = overlap_prop >= min_primary_weight
    ) |>
    dplyr::ungroup()

  expanded_crosswalk <-
    crosswalk_tbl |>
    dplyr::left_join(
      primary_map |>
        dplyr::select(
          dplyr::all_of(level_name_cols_old),
          dplyr::all_of(level_name_cols),
          is_primary
        ),
      by = c(level_name_cols_old, level_name_cols)
    ) |>
    dplyr::left_join(
      coverage_check,
      by = level_name_cols_old
    ) |>
    dplyr::mutate(
      split_flag = n_new_adm > 1,
      is_primary = ifelse(is.na(is_primary), FALSE, is_primary)
    )

  # filter to primary + meaningful secondary overlaps --------------------------

  final_crosswalk <-
    expanded_crosswalk |>
    dplyr::filter(
      is_primary | overlap_prop >= min_secondary_weight
    ) |>
    # recalculate split diagnostics after filtering
    dplyr::group_by(dplyr::across(dplyr::all_of(level_name_cols_old))) |>
    dplyr::mutate(
      n_new_adm = dplyr::n(),
      total_overlap_prop = sum(overlap_prop),
      split_flag = n_new_adm > 1
    ) |>
    dplyr::ungroup() |>
    # add merge diagnostics (multiple old -> new)
    dplyr::group_by(dplyr::across(dplyr::all_of(level_name_cols))) |>
    dplyr::mutate(
      n_old_per_new = dplyr::n(),
      merge_flag = n_old_per_new > 1,
      overlap_prop = round(overlap_prop, 2),
      total_overlap_prop = round(total_overlap_prop, 2)
    ) |>
    dplyr::ungroup() |>
    dplyr::arrange(
      dplyr::across(dplyr::all_of(level_name_cols_old)),
      dplyr::desc(is_primary),
      dplyr::desc(overlap_prop)
    ) |>
    dplyr::select(
      dplyr::all_of(level_name_cols_old),
      dplyr::all_of(level_name_cols),
      overlap_prop,
      dplyr::any_of(c(
        "overlap_prop_reverse",
        "area_old_km2",
        "area_new_km2",
        "overlap_area_km2"
      )),
      is_primary,
      n_new_adm,
      total_overlap_prop,
      split_flag,
      n_old_per_new,
      merge_flag
    ) |>
    as.data.frame()

  # build dictionary -----------------------------------------------------------

  # base dictionary entries
  dict_cols <- c(
    level_name_cols_old,
    level_name_cols,
    "overlap_prop"
  )

  dict_en <- c(
    "Country name in the old admin boundary system",
    if (level %in% c("adm1", "adm2")) {
      "Level-1 admin name in the old boundary system"
    },
    if (level == "adm2") {
      "Level-2 admin name in the old boundary system"
    },
    "Country name in the new admin boundary system",
    if (level %in% c("adm1", "adm2")) {
      "Level-1 admin name in the new boundary system"
    },
    if (level == "adm2") {
      "Level-2 admin name in the new boundary system"
    },
    "Proportion of old unit area overlapping the new unit"
  )

  dict_fr <- c(
    "Nom du pays dans l'ancien decoupage administratif",
    if (level %in% c("adm1", "adm2")) {
      "Nom administratif de niveau 1 dans l'ancien decoupage"
    },
    if (level == "adm2") {
      "Nom administratif de niveau 2 dans l'ancien decoupage"
    },
    "Nom du pays dans le nouveau decoupage administratif",
    if (level %in% c("adm1", "adm2")) {
      "Nom administratif de niveau 1 dans le nouveau decoupage"
    },
    if (level == "adm2") {
      "Nom administratif de niveau 2 dans le nouveau decoupage"
    },
    "Proportion de l'ancienne unite chevauchant la nouvelle"
  )

  # add optional columns to dictionary
  if (include_reverse_prop) {
    dict_cols <- c(dict_cols, "overlap_prop_reverse")
    dict_en <- c(
      dict_en,
      "Proportion of new unit area overlapping the old unit"
    )
    dict_fr <- c(
      dict_fr,
      "Proportion de la nouvelle unite chevauchant l'ancienne"
    )
  }

  if (include_areas) {
    dict_cols <- c(
      dict_cols,
      "area_old_km2",
      "area_new_km2",
      "overlap_area_km2"
    )
    dict_en <- c(
      dict_en,
      "Area of old admin unit in square kilometers",
      "Area of new admin unit in square kilometers",
      "Area of overlap region in square kilometers"
    )
    dict_fr <- c(
      dict_fr,
      "Superficie de l'ancienne unite en kilometres carres",
      "Superficie de la nouvelle unite en kilometres carres",
      "Superficie de la zone de chevauchement en kilometres carres"
    )
  }

  # add remaining standard columns
  dict_cols <- c(
    dict_cols,
    "is_primary",
    "n_new_adm",
    "total_overlap_prop",
    "split_flag",
    "n_old_per_new",
    "merge_flag"
  )

  dict_en <- c(
    dict_en,
    paste0(
      "TRUE if main spatial match for old unit (overlap_prop >= ",
      min_primary_weight,
      ")"
    ),
    "Number of new units overlapping the same old unit",
    "Total proportion of old unit area covered by retained overlaps",
    "TRUE if old unit is split across multiple new units",
    "Number of old units overlapping the same new unit",
    "TRUE if multiple old units were merged into this new unit"
  )

  dict_fr <- c(
    dict_fr,
    paste0(
      "TRUE si correspondance principale (overlap_prop >= ",
      min_primary_weight,
      ")"
    ),
    "Nombre de nouvelles unites chevauchant la meme ancienne unite",
    "Proportion totale de l'ancienne unite couverte",
    "TRUE si l'ancienne unite est repartie sur plusieurs nouvelles",
    "Nombre d'anciennes unites chevauchant la meme nouvelle unite",
    "TRUE si plusieurs anciennes unites fusionnees dans cette nouvelle"
  )

  dictionary <- tibble::tibble(
    column_name = dict_cols,
    english_description = dict_en,
    description_fr = dict_fr
  )

  # return output --------------------------------------------------------------

  # summary stats
  n_rows <- nrow(final_crosswalk)
  n_old_units <- length(unique(
    interaction(final_crosswalk[level_name_cols_old])
  ))
  n_new_units <- length(unique(
    interaction(final_crosswalk[level_name_cols])
  ))
  n_splits <- sum(final_crosswalk$split_flag & final_crosswalk$is_primary)
  n_merges <- sum(final_crosswalk$merge_flag & final_crosswalk$is_primary)

  # build metadata
  metadata <- tibble::tibble(
    created_at = Sys.time(),
    level = level,
    area_crs = area_crs,
    min_weight = min_weight,
    min_primary_weight = min_primary_weight,
    min_secondary_weight = min_secondary_weight,
    n_old_input = n_old,
    n_new_input = n_new,
    n_mappings = n_rows,
    n_splits = n_splits,
    n_merges = n_merges,
    sf_version = as.character(utils::packageVersion("sf"))
  )

  result <- list(
    data = final_crosswalk,
    dictionary = dictionary,
    metadata = metadata
  )

  if (keep_overlap_geometry) {
    result$overlap_geometry <- overlap_sf |> sf::st_transform(4326)
  }

  if (verbose) {
    cli::cli_h2("Crosswalk complete")
    cli::cli_bullets(c(
      "v" = "Total mappings: {.val {n_rows}}",
      "v" = "Old units mapped: {.val {n_old_units}}",
      "v" = "New units mapped: {.val {n_new_units}}",
      "i" = "Splits detected: {.val {n_splits}}",
      "i" = "Merges detected: {.val {n_merges}}"
    ))
  }

  result
}
