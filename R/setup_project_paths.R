#' Setup project paths and environment for Burundi SNT analysis
#'
#' Creates a list of standardized paths for the Burundi SNT 2025 project.
#' Detects a project root automatically and constructs all data/cache/output
#' paths with short, memorable names. Optionally creates missing folders.
#'
#' @param base_path Character path to project root. If NULL, try to detect
#'   a root (here::here(), then rprojroot); fallback to getwd().
#' @param create Logical; create any missing directories. Default TRUE.
#' @param quiet  Logical; suppress creation messages. Default TRUE.
#'
#' @return Named list of absolute paths:
#'   core, admin_shp, hf, pop_national, pop_worldpop, cache,
#'   dhis2, pfpr_est, mortality_est, interventions, drug_eff,
#'   climate, accessibility, land_use, dhs, ento, commodities,
#'   validation_plots, validation_tables.
#'
#' @examples
#' paths <- setup_project_paths()
#' list.files(paths$admin_shp)
#' @export
setup_project_paths <- function(base_path = NULL, create = TRUE, quiet = TRUE) {
  # resolve root
  root <- NULL

  # prefer here::here() if available
  if (is.null(base_path)) {
    if (requireNamespace("here", quietly = TRUE)) {
      # here::here() returns a path; make absolute for safety
      root <- fs::path_abs(here::here())
    }
    # fallback to rprojroot heuristics
    if (
      is.null(root) &&
        requireNamespace("rprojroot", quietly = TRUE)
    ) {
      crit <- rprojroot::has_file(".here") |
        rprojroot::is_git_root
      root <- try(rprojroot::find_root(crit), silent = TRUE)
      if (inherits(root, "try-error")) {
        root <- NULL
      }
      if (!is.null(root)) root <- fs::path_abs(root)
    }
    # final fallback
    if (is.null(root)) root <- fs::path_abs(getwd())
  } else {
    root <- fs::path_abs(base_path)
  }

  # small join helper to keep width tidy
  jp <- function(...) fs::path(root, ...)

  # build tree
  paths <- list(
    core = root,
    # 1.1_foundational
    admin_shp = jp("01_data", "1.1_foundational", "1.1a_admin_boundaries"),
    hf = jp("01_data", "1.1_foundational", "1.1b_health_facilities"),
    pop_national = jp(
      "01_data",
      "1.1_foundational",
      "1.1c_population",
      "1.1ci_national"
    ),
    pop_worldpop = jp(
      "01_data",
      "1.1_foundational",
      "1.1c_population",
      "1.1cii_worldpop_rasters"
    ),
    cache = jp("01_data", "1.1_foundational", "1.1d_cache_files"),
    # 1.2_epidemiology
    dhis2 = jp("01_data", "1.2_epidemiology", "1.2a_routine_surveillance"),
    pfpr_est = jp("01_data", "1.2_epidemiology", "1.2b_pfpr_estimates"),
    mortality_est = jp(
      "01_data",
      "1.2_epidemiology",
      "1.2c_mortality_estimates"
    ),
    # 1.3_interventions
    interventions = jp("01_data", "1.3_interventions"),
    # 1.4_drug_efficacy_resistance
    drug_eff = jp("01_data", "1.4_drug_efficacy_resistance"),
    # 1.5_environment
    climate = jp("01_data", "1.5_environment", "1.5a_climate"),
    accessibility = jp("01_data", "1.5_environment", "1.5b_accessibility"),
    land_use = jp("01_data", "1.5_environment", "1.5c_land_use"),
    # 1.6_health_systems
    dhs = jp("01_data", "1.6_health_systems", "1.6a_dhs"),
    # 1.7_entomology
    ento = jp("01_data", "1.7_entomology"),
    # 1.8_commodities
    commodities = jp("01_data", "1.8_commodities"),
    # outputs
    validation_plots = jp("03_outputs", "plots", "validation"),
    validation_tables = jp("03_outputs", "tables", "validation")
  )

  # create directories if requested
  if (isTRUE(create)) {
    for (p in paths) {
      if (!fs::dir_exists(p)) {
        ok <- try(fs::dir_create(p, recurse = TRUE), silent = TRUE)
        if (inherits(ok, "try-error") && !quiet) {
          cli::cli_alert_warning("Could not create: {p}")
        } else if (!quiet) {
          cli::cli_alert_success("Created: {p}")
        }
      }
    }
  }

  # project options
  options(snt.labels_en_path = fs::path(paths$cache, "labels_en.csv"))
  options(snt.paths = paths)

  # return
  paths
}
