#' Spatial Vector Validation and Cleaning
#'
#' Validates spatial vector data and creates admin-level aggregations with
#' geometry hashes
#'
#' @param shp An sf object to validate
#' @param name Character string identifying the dataset (for reporting)
#' @param adm0_col Character string, column name for country level
#' @param adm1_col Character string, column name for region/province level
#' @param adm2_col Character string, column name for district level
#' @param adm3_col Character string, column name for chiefdom/commune level
#' @param fix_issues Logical, whether to attempt automatic fixes
#' @param quiet Logical, whether to suppress progress messages
#' @param geometry_crs Target CRS for output geometry (default EPSG:4326).
#'   After CRS validation/fixes, geometries are transformed to this CRS
#'   for subsequent processing and output.
#'
#' @return List with validation results and cleaned admin-level spatial vectors
#'
#' @examples
#' \dontrun{
#' # Basic validation with Sierra Leone data
#' result <- validate_process_spatial(
#'   shp = shp_raw,
#'   name = "Sierra Leone boundaries",
#'   adm0_col = "country",
#'   adm1_col = "region",
#'   adm2_col = "district",
#'   adm3_col = "chiefdom"
#' )
#'
#' # Access cleaned admin levels
#' shp_adm0 <- result$spat_vec$adm0
#' shp_adm1 <- result$spat_vec$adm1
#' shp_adm2 <- result$spat_vec$adm2
#' shp_adm3 <- result$spat_vec$adm3
#'
#' # Check validation status
#' if (result$passed) {
#'   cli::cli_alert_success("Ready for SNT analysis!")
#' }
#'
#' # Access invalid rows (if any)
#' invalid_rows <- result$invalid_rows
#'
#' # Access duplicate rows (if any)
#' duplicate_rows <- result$duplicate_rows
#' # duplicate_rows$row_duplicates - exact row duplicates
#' # duplicate_rows$geometry_duplicates - geometry-only duplicates
#'
#' # Check column structure as data frame
#' col_dict <- result$column_dictionary
#' # Shows data frame with old_name, new_name, description, shapefiles
#' print(col_dict)
#' }
#'
#' @export
validate_process_spatial <- function(
  shp,
  name = "shapefile",
  adm0_col = NULL,
  adm1_col = NULL,
  adm2_col = NULL,
  adm3_col = NULL,
  fix_issues = TRUE,
  quiet = FALSE,
  geometry_crs = 4326
) {
  # Input validation: try to coerce to sf if needed
  if (!inherits(shp, "sf")) {
    shp_try <- tryCatch(
      sf::st_as_sf(shp),
      error = function(e) NULL
    )
    if (inherits(shp_try, "sf")) {
      shp <- shp_try
    } else {
      cli::cli_abort(
        "Input must be an sf object or convertible via st_as_sf()"
      )
    }
  }

  if (fix_issues) {
    if (!quiet) {
      cli::cli_h2("Validating and Cleaning Spatial Vector for {name}")
    }
  } else {
    if (!quiet) {
      cli::cli_h2("Validating Spatial Vector for {name}")
    }
  }

  # Initialize results
  results <- list(
    name = name,
    timestamp = Sys.time(),
    passed = TRUE,
    issues = character(),
    spat_vec = list(),
    invalid_rows = NULL,
    duplicate_rows = NULL,
    column_dictionary = NULL
  )

  # Work with a copy
  shp_clean <- shp

  # Basic info
  n_rows <- nrow(shp)
  n_cols <- ncol(shp)

  if (!quiet) {
    cli::cli_alert_info("Processing {n_rows} rows, {n_cols} columns")
  }

  # 1. CRS check and fixes -----------------------------------------------------

  if (!quiet) {
    cli::cli_progress_step("Checking CRS...")
  }

  crs_info <- sf::st_crs(shp_clean)
  if (is.na(crs_info$input)) {
    results$issues <- c(results$issues, "Missing CRS")

    if (fix_issues) {
      # Auto-detect if coordinates look like lat/lon
      bbox <- sf::st_bbox(shp_clean)
      if (
        bbox[1] >= -180 && bbox[3] <= 180 && bbox[2] >= -90 && bbox[4] <= 90
      ) {
        shp_clean <- sf::st_set_crs(shp_clean, 4326)
        if (!quiet) cli::cli_alert_success("Set CRS to WGS84")
      }
    } else {
      results$passed <- FALSE
    }
  } else {
    if (!quiet) cli::cli_alert_success("CRS: {crs_info$input}")
  }

  # 2. Geometery validation and fixes ------------------------------------------

  if (!quiet) {
    cli::cli_progress_step("Validating geometries...")
  }

  validity_check <- sf::st_is_valid(shp_clean)
  invalid_count <- sum(!validity_check)

  if (invalid_count > 0) {
    results$issues <- c(
      results$issues,
      paste(invalid_count, "invalid geometries")
    )

    # Store invalid rows before fixing
    results$invalid_rows <- shp_clean[!validity_check, ]

    if (fix_issues) {
      shp_clean <- sf::st_make_valid(shp_clean)
      if (!quiet) {
        cli::cli_alert_success(
          "Fixed {invalid_count} invalid geometries"
        )
      }
    } else {
      results$passed <- FALSE
    }
  } else {
    if (!quiet) cli::cli_alert_success("All geometries valid")
  }

  # 2b. Transform to target CRS if requested -----------------------------------
  if (!is.na(sf::st_crs(shp_clean)$epsg) &&
      !is.na(geometry_crs) &&
      sf::st_crs(shp_clean)$epsg != geometry_crs) {
    shp_clean <- sf::st_transform(shp_clean, geometry_crs)
    if (!quiet) {
      cli::cli_alert_info("Transformed geometries to EPSG:{geometry_crs}")
    }
  }

  # 3. Duplicate removal -------------------------------------------------------

  if (fix_issues) {
    if (!quiet) {
      cli::cli_progress_step("Removing duplicates...")
    }

    # Remove exact row duplicates
    n_before <- nrow(shp_clean)
    shp_clean <- dplyr::distinct(shp_clean)
    row_dups_removed <- n_before - nrow(shp_clean)

    # Remove geometry duplicates
    geom_binary <- sf::st_as_binary(shp_clean$geometry)
    geom_dups <- sum(duplicated(geom_binary))

    if (geom_dups > 0) {
      shp_clean <- shp_clean[!duplicated(geom_binary), ]
      results$issues <- c(
        results$issues,
        paste(geom_dups, "duplicate geometries removed")
      )
      if (!quiet) {
        cli::cli_alert_success(
          "Removed {geom_dups} duplicate geometries"
        )
      }
    }

    if (row_dups_removed > 0) {
      results$issues <- c(
        results$issues,
        paste(
          row_dups_removed,
          "duplicate rows removed"
        )
      )
      if (!quiet) {
        cli::cli_alert_success(
          "Removed {row_dups_removed} duplicate rows"
        )
      }
    }
  }

  # 4. Column mapping and standardisation --------------------------------------

  if (fix_issues) {
    if (!quiet) {
      cli::cli_progress_step("Standardizing admin columns...")
    }

    # Check which admin columns are available
    available_cols <- names(shp_clean)
    admin_mapping <- list()

    # Map provided column names to standard names
    if (!is.null(adm0_col) && adm0_col %in% available_cols) {
      admin_mapping$adm0 <- adm0_col
    }
    if (!is.null(adm1_col) && adm1_col %in% available_cols) {
      admin_mapping$adm1 <- adm1_col
    }
    if (!is.null(adm2_col) && adm2_col %in% available_cols) {
      admin_mapping$adm2 <- adm2_col
    }
    if (!is.null(adm3_col) && adm3_col %in% available_cols) {
      admin_mapping$adm3 <- adm3_col
    }

    if (length(admin_mapping) == 0) {
      cli::cli_abort("No valid admin columns found. Check column names.")
    }

    # Create standardized columns
    shp_clean <- shp_clean |>
      dplyr::select(
        dplyr::any_of(unlist(admin_mapping)),
        geometry
      )

    # Rename columns to standard format
    for (std_name in names(admin_mapping)) {
      orig_name <- admin_mapping[[std_name]]
      if (orig_name != std_name && orig_name %in% names(shp_clean)) {
        shp_clean <- shp_clean |>
          dplyr::rename(!!std_name := !!orig_name)
      }
    }
  } else {
    # For validation only, still need admin mapping for column dictionary
    available_cols <- names(shp_clean)
    admin_mapping <- list()

    # Map provided column names to standard names
    if (!is.null(adm0_col) && adm0_col %in% available_cols) {
      admin_mapping$adm0 <- adm0_col
    }
    if (!is.null(adm1_col) && adm1_col %in% available_cols) {
      admin_mapping$adm1 <- adm1_col
    }
    if (!is.null(adm2_col) && adm2_col %in% available_cols) {
      admin_mapping$adm2 <- adm2_col
    }
    if (!is.null(adm3_col) && adm3_col %in% available_cols) {
      admin_mapping$adm3 <- adm3_col
    }

    if (length(admin_mapping) == 0) {
      cli::cli_abort("No valid admin columns found. Check column names.")
    }
  }

  # 5. Create geometry hash and admin GUIDS ------------------------------------

  if (fix_issues) {
    if (!quiet) {
      cli::cli_progress_step(
        "Creating geometry hashes and admin GUIDs..."
      )
    }

    # Create geometry hash using sntutils::vdigest
    shp_clean <- shp_clean |>
      dplyr::mutate(
        geometry_hash = sntutils::vdigest(geometry, algo = "xxhash64")
      )

    # Create admin-level GUIDs for each available admin level
    available_admin_levels <- names(admin_mapping)

    for (admin_level in available_admin_levels) {
      guid_col <- paste0(admin_level, "_guid")

      # Create concatenated string for hashing based on hierarchy
      if (admin_level %in% names(shp_clean)) {
        # Get admin level number
        level_num <- as.numeric(stringr::str_extract(admin_level, "\\d"))

        # Build concatenation columns (all admin levels up to current)
        concat_cols <- paste0("adm", 0:level_num)
        concat_cols <- concat_cols[concat_cols %in% names(shp_clean)]

        if (length(concat_cols) > 0) {
          shp_clean <- shp_clean |>
            dplyr::mutate(
              !!guid_col := sntutils::vdigest(
                paste(!!!rlang::syms(concat_cols), sep = "_"),
                algo = "xxhash32"
              )
            )
        }
      }
    }

    # Create column ordering with alternating admin/guid pairs
    admin_cols_ordered <- c()
    for (admin_level in c("adm0", "adm1", "adm2", "adm3")) {
      if (admin_level %in% available_admin_levels) {
        admin_cols_ordered <- c(
          admin_cols_ordered,
          admin_level,
          paste0(admin_level, "_guid")
        )
      }
    }

    # Ensure proper column ordering: admin levels with GUIDs,
    # then geometry_hash, then geometry
    shp_clean <- shp_clean |>
      dplyr::select(
        dplyr::any_of(admin_cols_ordered),
        geometry_hash,
        geometry
      )

    if (!quiet) cli::cli_alert_success("Added geometry hashes and admin GUIDs")
  }

  # 6. Create admin level aggregations -----------------------------------------

  if (fix_issues) {
    if (!quiet) {
      cli::cli_progress_step("Creating admin level aggregations...")
    }

    # Start with the finest level (should be adm3 or highest available)
    highest_level <- max(as.numeric(stringr::str_extract(
      names(admin_mapping),
      "\\d"
    )))
    base_level_name <- paste0("adm", highest_level)

    # Base level (finest resolution) - ensure proper column ordering
    results$spat_vec[[base_level_name]] <- shp_clean |>
      dplyr::select(
        dplyr::any_of(admin_cols_ordered),
        geometry_hash,
        geometry
      )

    # Create higher-level aggregations
    for (level in (highest_level - 1):0) {
      level_name <- paste0("adm", level)

      if (level_name %in% names(admin_mapping)) {
        # Get grouping columns (all admin levels up to current)
        group_cols <- paste0("adm", 0:level)
        group_cols <- group_cols[group_cols %in% names(shp_clean)]

        if (length(group_cols) > 0) {
          # Get the corresponding GUID columns for grouping
          guid_cols <- paste0(gsub("adm", "adm", group_cols), "_guid")
          guid_cols <- guid_cols[guid_cols %in% names(shp_clean)]

          # Create aggregation groups including both admin and guid columns
          all_group_cols <- c(group_cols, guid_cols)
          all_group_cols <- all_group_cols[all_group_cols %in% names(shp_clean)]

          agg_shp <- shp_clean |>
            dplyr::group_by(dplyr::across(dplyr::all_of(
              all_group_cols
            ))) |>
            dplyr::summarise(
              geometry = sf::st_union(geometry),
              .groups = "drop"
            ) |>
            dplyr::mutate(
              geometry_hash = sntutils::vdigest(geometry, algo = "xxhash64")
            )

          # Create proper column ordering for this admin level
          level_admin_cols <- c()
          for (admin_level in c("adm0", "adm1", "adm2", "adm3")) {
            if (admin_level %in% group_cols) {
              level_admin_cols <- c(level_admin_cols, admin_level)
              guid_col <- paste0(admin_level, "_guid")
              if (guid_col %in% names(agg_shp)) {
                level_admin_cols <- c(level_admin_cols, guid_col)
              }
            }
          }

          # Ensure proper column ordering
          agg_shp <- agg_shp |>
            dplyr::select(
              dplyr::any_of(level_admin_cols),
              geometry_hash,
              geometry
            ) |>
            sf::st_sf()

          results$spat_vec[[level_name]] <- agg_shp

          if (!quiet) {
            cli::cli_alert_success(
              "Created {level_name}: {nrow(agg_shp)} features"
            )
          }
        }
      }
    }
  }

  # 8. Create column dictionary ------------------------------------------------

  if (fix_issues) {
    if (!quiet) {
      cli::cli_progress_step("Creating column dictionary...")
    }

    # Create a single data frame for all columns across all spatial vectors
    dict_rows <- list()

    for (level_name in names(results$spat_vec)) {
      shp_level <- results$spat_vec[[level_name]]
      col_names <- names(shp_level)

      for (col_name in col_names) {
        # Find original column name
        old_name <- NA
        if (col_name %in% names(admin_mapping)) {
          old_name <- admin_mapping[[col_name]]
        } else if (stringr::str_detect(col_name, "^adm\\d$")) {
          # Check if this standardized name maps back to an original
          for (std_name in names(admin_mapping)) {
            if (std_name == col_name) {
              old_name <- admin_mapping[[std_name]]
              break
            }
          }
        }

        # Create description
        if (stringr::str_detect(col_name, "^adm\\d$")) {
          description <- paste(
            "Administrative level",
            stringr::str_extract(col_name, "\\d"),
            "name"
          )
        } else if (stringr::str_detect(col_name, "^adm\\d_guid$")) {
          adm_level <- stringr::str_extract(col_name, "\\d")
          description <- paste(
            "Administrative level",
            adm_level,
            "unique identifier (xxhash32)"
          )
        } else if (col_name == "geometry_hash") {
          description <- "Geometry unique identifier (xxhash64)"
        } else if (col_name == "geometry") {
          description <- "Spatial geometry (sf geometry column)"
        } else {
          description <- "Data column"
        }

        # Check if this row already exists
        existing_row <- FALSE
        for (i in seq_along(dict_rows)) {
          if (dict_rows[[i]]$new_name == col_name) {
            # Add this shapefile to the existing row
            dict_rows[[i]]$shapefiles <- paste(
              dict_rows[[i]]$shapefiles,
              level_name,
              sep = ", "
            )
            existing_row <- TRUE
            break
          }
        }

        # If not existing, create new row
        if (!existing_row) {
          dict_rows[[length(dict_rows) + 1]] <- data.frame(
            old_name = ifelse(is.na(old_name), col_name, old_name),
            new_name = col_name,
            description = description,
            shapefiles = level_name,
            stringsAsFactors = FALSE
          )
        }
      }
    }

    # Convert to data frame
    results$column_dictionary <- do.call(rbind, dict_rows)
    rownames(results$column_dictionary) <- NULL

    if (!quiet) {
      cli::cli_alert_success(
        paste0(
          "Created column dictionary data frame with ",
          "{nrow(results$column_dictionary)} columns"
        )
      )
    }
  }

  # 9. Final validation --------------------------------------------------------

  if (fix_issues) {
    if (!quiet) {
      cli::cli_progress_step(
        msg = "Check hash uniqueness for each level...",
        msg_done = "Hash is unique for each level"
      )
    }

    # Check hash uniqueness for each level
    for (level_name in names(results$spat_vec)) {
      shp_level <- results$spat_vec[[level_name]]
      hash_dups <- sum(duplicated(shp_level$geometry_hash))

      if (hash_dups > 0) {
        results$issues <- c(
          results$issues,
          paste(level_name, "has", hash_dups, "duplicate hashes")
        )
        results$passed <- FALSE
      }
    }
    cli::cli_process_done()
  }

  # 10. Summary ----------------------------------------------------------------
  if (!quiet) {
    if (results$passed) {
      if (fix_issues) {
        cli::cli_h2("Summary of Spatial Vector Validation and Cleaning")
        cli::cli_alert_info(
          "Created {length(results$spat_vec)} admin levels:"
        )
        for (level_name in names(results$spat_vec)) {
          n_features <- nrow(results$spat_vec[[level_name]])
          cli::cli_text("  \u2022 {level_name}: {n_features} features")
        }
      } else {
        cli::cli_h2("Summary of Spatial Vector Validation")
        cli::cli_alert_info("Validation completed - no cleaning performed")
      }
    } else {
      cli::cli_h2("\u2717 Issues found:")
      for (issue in unique(results$issues)) {
        cli::cli_text("  \u2022 {issue}")
      }
    }

    # Report invalid rows if any
    if (!is.null(results$invalid_rows)) {
      cli::cli_alert_info(
        paste0(
          "Invalid rows stored in results$invalid_rows ",
          "({nrow(results$invalid_rows)} rows)"
        )
      )
    }

    # Report duplicate rows if any
    if (!is.null(results$duplicate_rows)) {
      total_dups <- 0
      if (!is.null(results$duplicate_rows$row_duplicates)) {
        total_dups <- total_dups + nrow(results$duplicate_rows$row_duplicates)
      }
      if (!is.null(results$duplicate_rows$geometry_duplicates)) {
        total_dups <- total_dups +
          nrow(
            results$duplicate_rows$geometry_duplicates
          )
      }
      cli::cli_alert_info(
        paste0(
          "Duplicate rows stored in results$duplicate_rows ",
          "({total_dups} total duplicates)"
        )
      )
    }

    # Report column dictionary
    if (fix_issues) {
      cli::cli_alert_info(
        paste0(
          "Column dictionary available as data frame in ",
          "results$column_dictionary"
        )
      )
    }
  }

  return(results)
}
