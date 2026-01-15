#' Setup project paths and environment for SNT pipeline
#'
#' Creates a list of standardized paths for the SNT pipelines
#' Detects a project root automatically and constructs all data/cache/output
#' paths with short, memorable names. Validates that folders exist and alerts
#' if they don't. Use initialize_project_structure() to create the folders.
#'
#' @param base_path Character path to project root. If NULL, try to detect
#'   a root (here::here(), then rprojroot); fallback to getwd().
#' @param quiet  Logical; suppress warning messages. Default FALSE.
#'
#' @return Named list of absolute paths:
#'   core, admin_shp, physical_feat, hf, chw, pop_national,
#'   pop_worldpop, cache, dhis2, pfpr_est, mortality_est, interventions,
#'   drug_eff, climate, accessibility, land_use, dhs, ento, commodities,
#'   val, interm, final,
#'   val_fig, val_tbl, interm_fig, interm_tbl, final_fig, final_tbl.
#'
#' @examples
#' tmp <- tempdir()
#' paths <- setup_project_paths(base_path = tmp, quiet = TRUE)
#' paths$admin_shp
#' paths$val_fig
#' paths$interm_tbl
#' @export
setup_project_paths <- function(base_path = NULL, quiet = FALSE) {
  # resolve root
  root <- NULL

  # prefer here::here() if available
  if (is.null(base_path)) {
    if (requireNamespace("here", quietly = TRUE)) {
      # here::here() returns a path; make absolute for safety
      root <- normalizePath(here::here(), winslash = "/", mustWork = FALSE)
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
      if (!is.null(root)) {
        root <- normalizePath(root, winslash = "/", mustWork = FALSE)
      }
    }
    # final fallback
    if (is.null(root)) {
      root <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
    }
  } else {
    root <- normalizePath(base_path, winslash = "/", mustWork = FALSE)
  }

  # small join helper to keep width tidy
  jp <- function(...) fs::path(root, ...)

  # build tree
  paths <- list(
    core = root,
    # 1.1_foundational
    admin_shp = jp("01_data", "1.1_foundational", "1.1a_admin_boundaries"),
    physical_feat = jp("01_data", "1.1_foundational", "1.1b_physical_features"),
    hf = jp("01_data", "1.1_foundational", "1.1c_health_facilities"),
    chw = jp(
      "01_data",
      "1.1_foundational",
      "1.1d_community_health_workers"
    ),
    pop_national = jp(
      "01_data",
      "1.1_foundational",
      "1.1e_population",
      "1.1ei_national"
    ),
    pop_worldpop = jp(
      "01_data",
      "1.1_foundational",
      "1.1e_population",
      "1.1eii_worldpop_rasters"
    ),
    cache = jp("01_data", "1.1_foundational", "1.1f_cache_files"),
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
    # outputs - parent directories
    val = jp("03_outputs", "3.1_validation"),
    interm = jp("03_outputs", "3.2_intermediate_products"),
    final = jp("03_outputs", "3.3_final_snt_outputs"),
    # outputs - figures and tables
    val_fig = jp("03_outputs", "3.1_validation", "figures"),
    val_tbl = jp("03_outputs", "3.1_validation", "tables"),
    interm_fig = jp("03_outputs", "3.2_intermediate_products", "figures"),
    interm_tbl = jp("03_outputs", "3.2_intermediate_products", "tables"),
    final_fig = jp("03_outputs", "3.3_final_snt_outputs", "figures"),
    final_tbl = jp("03_outputs", "3.3_final_snt_outputs", "tables")
  )

  # check directories exist and warn if not
  missing_paths <- character(0)
  for (nm in names(paths)) {
    p <- paths[[nm]]
    if (!fs::dir_exists(p)) {
      missing_paths <- c(missing_paths, nm)
    }
  }

  if (length(missing_paths) > 0 && !quiet) {
    cli::cli_alert_warning(
      "Missing {length(missing_paths)} path{?s}: {.val {missing_paths}}"
    )
    cli::cli_alert_info(
      "Run {.fn initialize_project_structure} to create the folder structure"
    )
  }

  # project options
  options(snt.labels_en_path = fs::path(paths$cache, "labels_en.csv"))
  options(snt.paths = paths)

  # return
  paths
}
