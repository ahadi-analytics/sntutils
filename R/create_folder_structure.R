#' Create Hierarchical Data Folder Structure (AHADI Style)
#'
#' Creates a domain-sorted data folder hierarchy under 01_data/ using a
#' three-tier naming system (e.g., d1.3a_worldpop_rasters) with raw/ and
#' processed/ folders.
#
#' @param base_path Character. base_path directory for the data folders.
#'                  Default is "01_data".
#'
#' @return NULL (creates folders on disk)

#' Initialize Full Project Folder Structure
#'
#' This function sets up a clean, hierarchical folder system for Ahadi projects:
#' - 01_data/ with nested numbered folders and raw/processed subfolders
#' - 02_scripts/
#' - 03_outputs/ with 3.1_validation/,
#'         3.2_intermediate_products/, 3.3_final_snt_outputs/
#' - 04_reports/, 05_metadata_docs/
#'
#' @param base_path Character. base_path project directory
#'    (default = current directory)
#'
#' @return NULL (folders created on disk)
#' @export
create_data_structure <- function(base_path = ".") {
  data_structure <- list(
    "1.1_foundational" = c(
      "1.1a_admin_boundaries",
      "1.1b_physical_features",
      "1.1c_health_facilities",
      "1.1d_community_health_workers",
      "1.1e_population/1.1ei_national",
      "1.1e_population/1.1eii_worldpop_rasters",
      "1.1f_cache_files/."
    ),
    "1.2_epidemiology" = c(
      "1.2a_routine_surveillance",
      "1.2b_pfpr_estimates",
      "1.2c_mortality_estimates"
    ),
    "1.3_interventions" = c(
      "1.3a_itns",
      "1.3b_iptp",
      "1.3c_smc",
      "1.3d_vap",
      "1.3e_anc",
      "1.3f_irs"
    ),
    "1.4_drug_efficacy_resistance" = ".",
    "1.5_environment" = c(
      "1.5a_climate",
      "1.5b_accessibility",
      "1.5c_land_use"
    ),
    "1.6_health_systems" = c(
      "1.6a_dhs"
    ),
    "1.7_entomology" = ".",
    "1.8_commodities" = "."
  )

  base_data <- fs::path(base_path, "01_data")

  for (domain in names(data_structure)) {
    domain_path <- fs::path(base_data, domain)
    entries <- data_structure[[domain]]

    if (identical(entries, ".")) {
      fs::dir_create(fs::path(domain_path, "raw"))
      fs::dir_create(fs::path(domain_path, "processed"))
    } else {
      for (entry in entries) {
        path_final <- fs::path(domain_path, entry)
        fs::dir_create(fs::path(path_final, "raw"))
        fs::dir_create(fs::path(path_final, "processed"))
      }
    }
  }

  # Other standard folders
  fs::dir_create(fs::path(base_path, "02_scripts"))
  fs::dir_create(fs::path(base_path, "03_outputs", "3.1_validation", "figures"))
  fs::dir_create(fs::path(base_path, "03_outputs", "3.1_validation", "tables"))
  fs::dir_create(fs::path(
    base_path,
    "03_outputs",
    "3.2_intermediate_products",
    "figures"
  ))
  fs::dir_create(fs::path(
    base_path,
    "03_outputs",
    "3.2_intermediate_products",
    "tables"
  ))
  fs::dir_create(fs::path(
    base_path,
    "03_outputs",
    "3.3_final_snt_outputs",
    "figures"
  ))
  fs::dir_create(fs::path(
    base_path,
    "03_outputs",
    "3.3_final_snt_outputs",
    "tables"
  ))
  fs::dir_create(fs::path(base_path, "04_reports"))
  fs::dir_create(fs::path(base_path, "05_metadata_docs"))

  invisible(NULL)
}

#' Initialize Full Project Folder Structure
#'
#' Sets up a clean Ahadi-style project hierarchy:
#' - 01_data/ with numbered domain folders
#' - 02_scripts/
#' - 03_outputs/ with 3.1_validation/,
#'        3.2_intermediate_products/, 3.3_final_snt_outputs/
#' - 04_reports/, 05_metadata_docs/
#'
#' @param base_path Character. Project root directory (default ".").
#'
#' @return NULL (folders created on disk)
#' @export
initialize_project_structure <- function(base_path = ".") {
  if (!fs::dir_exists(base_path)) {
    fs::dir_create(base_path)
  }

  # create data domains
  create_data_structure(base_path)

  # other folders
  fs::dir_create(fs::path(base_path, "02_scripts"))
  fs::dir_create(fs::path(base_path, "03_outputs", "3.1_validation", "figures"))
  fs::dir_create(fs::path(base_path, "03_outputs", "3.1_validation", "tables"))
  fs::dir_create(fs::path(
    base_path,
    "03_outputs",
    "3.2_intermediate_products",
    "figures"
  ))
  fs::dir_create(fs::path(
    base_path,
    "03_outputs",
    "3.2_intermediate_products",
    "tables"
  ))
  fs::dir_create(fs::path(
    base_path,
    "03_outputs",
    "3.3_final_snt_outputs",
    "figures"
  ))
  fs::dir_create(fs::path(
    base_path,
    "03_outputs",
    "3.3_final_snt_outputs",
    "tables"
  ))
  fs::dir_create(fs::path(base_path, "04_reports"))
  fs::dir_create(fs::path(base_path, "05_metadata_docs"))

  invisible(NULL)
}
