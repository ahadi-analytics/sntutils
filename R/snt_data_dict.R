# package cache environment
.snt_cache <- new.env(parent = emptyenv())

#' Load and flatten the SNT variable tree
#'
#' Loads the bilingual indicator hierarchy from the package's `snt_var_tree` dataset
#' and returns a tidy table of all variables, their domains, labels, and
#' disaggregation structure (if defined). You can optionally specify a
#' single domain (e.g. `"routine_data"`, `"stock_management"`) to filter
#' the returned results.
#'
#' @param flatten Logical; if TRUE, returns a tidy tibble of variable metadata.
#' @param include_schema Logical; if TRUE, also loads the schema and attaches it
#'   as an attribute.
#' @param domain Optional character string; if provided, returns only variables
#'   belonging to that top-level domain (e.g. `"routine_data"`, `"anemia"`).
#'
#' @return A tibble with columns:
#'   - `domain`: the top-level domain (e.g. metadata, routine_data)
#'   - `snt_var_name`: standardized variable name
#'   - `label_en`, `label_fr`, `label_pt`: multilingual labels
#'   - `disagg`: character vector of applicable disaggregations (if available)
#'
#' @examples
#'
#' \dontrun{
#' # Load all variable metadata
#' sntutils::snt_data_dict()
#'
#' # Load only routine_data variables
#' sntutils::snt_data_dict(domain = "routine_data")
#'}
#'
#' @export
snt_data_dict <- function(
  flatten = TRUE,
  include_schema = TRUE,
  domain = NULL
) {
  # -- Load variable tree ------------------------------------------------------
  data("snt_var_tree", package = "sntutils", envir = environment())
  tree <- snt_var_tree

  # -- Optionally load schema --------------------------------------------------
  if (include_schema) {
    # Schema is already included in the snt_var_tree data object
    schema <- tree$schema
    if (!is.null(schema)) {
      base::attr(tree, "schema") <- schema
    }
  }

  # -- Flatten -----------------------------------------------------------------
  flat_tbl <- .flatten_tree_recursive(tree)

  # Extract domain from path
  flat_tbl <- flat_tbl |>
    dplyr::mutate(domain = stringr::str_extract(.data$path, "^[^.]+")) |>
    dplyr::select(domain, dplyr::everything(), -path)

  # Extract disaggregation info from variable names using schema
  flat_tbl$disagg <- purrr::map_chr(flat_tbl$snt_var_name, function(nm) {
    # Extract disaggregations from variable name using schema
    if (is.null(tree$schema)) return(NA_character_)
    
    # Get all disaggregation types from schema
    age_groups <- names(tree$schema$age_groups)
    pop_groups <- names(tree$schema$population_groups)
    test_types <- names(tree$schema$test_types)
    sectors <- names(tree$schema$sectors)
    service_levels <- names(tree$schema$service_levels)
    
    # Split variable name into tokens
    tokens <- unlist(strsplit(nm, "_"))
    
    # Find matching disaggregations
    found_disagg <- c()
    
    for (token in tokens) {
      if (token %in% age_groups) {
        found_disagg <- c(found_disagg, token)
      } else if (token %in% pop_groups) {
        found_disagg <- c(found_disagg, token)
      } else if (token %in% test_types) {
        found_disagg <- c(found_disagg, token)
      } else if (token %in% sectors) {
        found_disagg <- c(found_disagg, token)
      } else if (token %in% service_levels) {
        found_disagg <- c(found_disagg, token)
      }
    }
    
    if (length(found_disagg) > 0) {
      paste(found_disagg, collapse = ", ")
    } else {
      NA_character_
    }
  })

  # Filter by domain if requested
  if (!is.null(domain)) {
    flat_tbl <- dplyr::filter(flat_tbl, .data$domain == !!domain)
    if (nrow(flat_tbl) == 0) {
      cli::cli_warn("No variables found for domain `{domain}`.")
    }
  }

  # Attach schema
  if (include_schema && base::exists("schema", inherits = FALSE)) {
    base::attr(flat_tbl, "schema") <- schema
  }

  flat_tbl
}

#' Flatten a nested variable tree
#'
#' Recursively traverses the nested YAML tree to produce a tidy table
#' of variable paths and bilingual labels.
#'
#' @param tree A nested list (as read from `var_tree.yml`)
#' @param parent_path Internal; parent node path
#'
#' @return A tibble with columns:
#'   - `path`: dot-delimited hierarchical path
#'   - `snt_var_name`: standardized variable name
#'   - `label_en`: English label
#'   - `label_fr`: French label
#'   - `label_pt`: Portuguese label
#'
#' @keywords internal
#' @noRd
.flatten_tree_recursive <- function(tree, parent_path = NULL) {
  rows <- list()

  for (name in names(tree)) {
    # Skip meta nodes and description fields
    if (name %in% c("_meta", "_description")) next

    node <- tree[[name]]

    # if node is a variable leaf
    if (is.list(node) && "snt_var_name" %in% names(node)) {
      path <- if (is.null(parent_path)) {
        name
      } else {
        paste(parent_path, name, sep = ".")
      }
      rows[[length(rows) + 1]] <- tibble::tibble(
        path = path,
        snt_var_name = node$snt_var_name,
        label_en = node$label_en %||% NA_character_,
        label_fr = node$label_fr %||% NA_character_,
        label_pt = node$label_pt %||% NA_character_
      )
    } else if (is.list(node)) {
      # if node is a branch, recurse
      sub_rows <- .flatten_tree_recursive(
        node,
        parent_path = if (is.null(parent_path)) {
          name
        } else {
          paste(parent_path, name, sep = ".")
        }
      )
      rows <- c(rows, list(sub_rows))
    }
  }

  dplyr::bind_rows(rows)
}

#' get cached flattened tree (internal)
#' @description auto-refreshes if tree version changes
#' @noRd
.get_flat_tree_cached <- function() {
  data("snt_var_tree", package = "sntutils", envir = environment())

  # get current tree version from metadata
  current_version <- snt_var_tree$`_meta`$last_updated %||% "unknown"

  # refresh cache if empty or version changed
  if (base::is.null(.snt_cache$flat_tree) ||
      !base::identical(.snt_cache$tree_version, current_version)) {
    .snt_cache$flat_tree <- .flatten_tree_recursive(snt_var_tree)
    .snt_cache$tree_version <- current_version
    # clear lookup caches when tree changes
    .snt_cache$lookup_en <- NULL
    .snt_cache$lookup_fr <- NULL
    .snt_cache$lookup_pt <- NULL
  }

  .snt_cache$flat_tree
}

#' get cached schema (internal)
#' @description auto-refreshes if tree version changes
#' @noRd
.get_schema_cached <- function() {
  data("snt_var_tree", package = "sntutils", envir = environment())

  current_version <- snt_var_tree$`_meta`$last_updated %||% "unknown"

  if (base::is.null(.snt_cache$schema) ||
      !base::identical(.snt_cache$schema_version, current_version)) {
    .snt_cache$schema <- snt_var_tree$schema
    .snt_cache$schema_version <- current_version
  }

  .snt_cache$schema
}

#' get cached lookup vector for labels (internal)
#' @description uses cached flat tree; auto-refreshes when tree changes
#' @noRd
.get_lookup_vector_cached <- function(lang = "en") {
  cache_key <- base::paste0("lookup_", lang)

  if (base::is.null(.snt_cache[[cache_key]])) {
    flat <- .get_flat_tree_cached()
    lang_col <- base::paste0("label_", lang)

    if (lang_col %in% base::names(flat)) {
      .snt_cache[[cache_key]] <- stats::setNames(
        flat[[lang_col]],
        flat$snt_var_name
      )
    } else {
      .snt_cache[[cache_key]] <- stats::setNames(
        flat$label_en,
        flat$snt_var_name
      )
    }
  }

  .snt_cache[[cache_key]]
}

# internal helper: detect dynamic DHS ITN age group patterns
.detect_dhs_itn_dynamic <- function(var_name) {
  # pattern for metric variables: dhs_itn_(use|access|use_if_access)_<age>(_low|_upp)?
  metric_pattern <- "^dhs_itn_(use|access|use_if_access)_([0-9]+_[0-9]+|[0-9]+_plus)(_low|_upp)?$"

  # pattern for count variables: dhs_n_(individuals|used_itn|with_access)_<age>
  count_pattern <- "^dhs_n_(individuals|used_itn|with_access)_([0-9]+_[0-9]+|[0-9]+_plus)$"

  # try metric pattern first
  if (grepl(metric_pattern, var_name)) {
    matches <- regmatches(var_name, regexec(metric_pattern, var_name))[[1]]
    return(list(
      metric_type = matches[2],
      age_suffix = matches[3],
      ci_type = if (nchar(matches[4]) > 0) gsub("^_", "", matches[4]) else NA_character_,
      is_count = FALSE
    ))
  }

  # try count pattern
  if (grepl(count_pattern, var_name)) {
    matches <- regmatches(var_name, regexec(count_pattern, var_name))[[1]]
    return(list(
      metric_type = matches[2],
      age_suffix = matches[3],
      ci_type = NA_character_,
      is_count = TRUE
    ))
  }

  # no match
  return(NULL)
}

# internal helper: format age suffix to readable label
.format_dhs_itn_age_label <- function(age_suffix, language = "en") {
  # parse age suffix
  if (grepl("_plus$", age_suffix)) {
    # open-ended: "20_plus" -> "20+"
    age_lower <- gsub("_plus$", "", age_suffix)

    if (language == "en") {
      return(paste0(age_lower, "+ years"))
    } else if (language == "fr") {
      return(paste0(age_lower, "+ ans"))
    } else if (language == "pt") {
      return(paste0(age_lower, "+ anos"))
    }
  } else {
    # range: "0_5" -> "0-5"
    parts <- strsplit(age_suffix, "_")[[1]]
    age_range <- paste(parts[1], parts[2], sep = "-")

    if (language == "en") {
      return(paste0(age_range, " years"))
    } else if (language == "fr") {
      return(paste0(age_range, " ans"))
    } else if (language == "pt") {
      return(paste0(age_range, " anos"))
    }
  }

  return(age_suffix)
}

# internal helper: build complete DHS ITN label
.build_dhs_itn_label <- function(metric_type, age_suffix, ci_type = NA, language = "en") {
  age_label <- .format_dhs_itn_age_label(age_suffix, language)

  # build base metric label
  if (language == "en") {
    base_label <- switch(metric_type,
      "use" = "DHS ITN use",
      "access" = "DHS ITN access",
      "use_if_access" = "DHS ITN use if access",
      "individuals" = "DHS number of individuals",
      "used_itn" = "DHS number used ITN",
      "with_access" = "DHS number with access",
      metric_type
    )

    # add age group
    label <- paste0(base_label, " (", age_label, ")")

    # add percentage or count indicator
    if (metric_type %in% c("use", "access", "use_if_access")) {
      label <- paste0(label, " (%)")
    }

    # add CI suffix if present
    if (!is.na(ci_type)) {
      ci_label <- if (ci_type == "low") "lower CI" else "upper CI"
      label <- paste0(label, " ", ci_label)
    }

  } else if (language == "fr") {
    base_label <- switch(metric_type,
      "use" = "Utilisation de MII DHS",
      "access" = "Acc\u00e8s aux MII DHS",
      "use_if_access" = "Utilisation de MII si acc\u00e8s DHS",
      "individuals" = "Nombre d'individus DHS",
      "used_itn" = "Nombre ayant utilis\u00e9 MII DHS",
      "with_access" = "Nombre avec acc\u00e8s DHS",
      metric_type
    )

    label <- paste0(base_label, " (", age_label, ")")

    if (metric_type %in% c("use", "access", "use_if_access")) {
      label <- paste0(label, " (%)")
    }

    if (!is.na(ci_type)) {
      ci_label <- if (ci_type == "low") "IC inf\u00e9rieur" else "IC sup\u00e9rieur"
      label <- paste0(label, " ", ci_label)
    }

  } else if (language == "pt") {
    base_label <- switch(metric_type,
      "use" = "Uso de MILD DHS",
      "access" = "Acesso a MILD DHS",
      "use_if_access" = "Uso de MILD se acesso DHS",
      "individuals" = "N\u00famero de indiv\u00edduos DHS",
      "used_itn" = "N\u00famero que usou MILD DHS",
      "with_access" = "N\u00famero com acesso DHS",
      metric_type
    )

    label <- paste0(base_label, " (", age_label, ")")

    if (metric_type %in% c("use", "access", "use_if_access")) {
      label <- paste0(label, " (%)")
    }

    if (!is.na(ci_type)) {
      ci_label <- if (ci_type == "low") "IC inferior" else "IC superior"
      label <- paste0(label, " ", ci_label)
    }
  }

  return(label)
}

#' Detect and display the structural components of an SNT variable name
#'
#' Parses a standardized SNT variable name (e.g. "conf_rdt_u5_priv")
#' and prints its logical components and bilingual label using CLI formatting.
#'
#' @details
#' Dynamic age groups are supported for DHS ITN metrics. Variables following
#' patterns like \code{dhs_itn_use_0_5}, \code{dhs_itn_access_20_plus}, or
#' \code{dhs_n_individuals_15_49} are automatically recognized and labeled
#' with appropriate age range text in English, French, and Portuguese.
#'
#' @param var_name Character scalar - an SNT variable name.
#' @param schema Optional list; if not provided, automatically loaded from
#'   the package's snt_var_tree dataset.
#' @param var_tree Optional list; if not provided, automatically loaded from
#'   the package's snt_var_tree dataset.
#' @param return Logical; if TRUE, also returns a tibble invisibly.
#' @param verbose Logical; if TRUE (default), prints fuzzy match info messages.
#'
#' @return Invisibly returns a tibble with parsed components and labels.
#'
#' @examples
#' # Dynamic DHS ITN age groups
#' check_snt_var("dhs_itn_use_0_5")
#' check_snt_var("dhs_itn_access_20_plus")
#' check_snt_var("dhs_n_individuals_5_10_low")
#'
#' @export
check_snt_var <- function(
  var_name,
  schema = NULL,
  var_tree = NULL,
  return = FALSE,
  verbose = TRUE
) {
  # -- Load schema if not provided ---------------------------------------------
  if (base::is.null(schema)) {
    schema <- .get_schema_cached()
  }

  # -- Load variable tree for label lookup -------------------------------------
  if (base::is.null(var_tree)) {
    flat_tree <- .get_flat_tree_cached()
  } else if (base::is.list(var_tree) && "flat_tree" %in% base::names(var_tree)) {
    # pre-flattened tree passed in
    flat_tree <- var_tree$flat_tree
  } else {
    # custom tree needs flattening
    flat_tree <- .flatten_tree_recursive(var_tree)
  }

  # -- Collect suffix groups ---------------------------------------------------
  test_types <- names(schema$test_types)
  age_groups <- names(schema$age_groups)
  pop_groups <- names(schema$population_groups)
  sectors <- names(schema$sectors)
  service_levels <- names(schema$service_levels)

  # -- Tokenize ---------------------------------------------------------------
  tokens <- unlist(strsplit(var_name, "_"))

  components <- list(
    domain = tokens[[1]],
    test_type = NA_character_,
    service_level = NA_character_,
    age_group = NA_character_,
    population_group = NA_character_,
    sector = NA_character_
  )

  for (t in tokens[-1]) {
    if (t %in% test_types) {
      components$test_type <- t
    } else if (t %in% service_levels) {
      components$service_level <- t
    } else if (t %in% age_groups) {
      components$age_group <- t
    } else if (t %in% pop_groups) {
      components$population_group <- t
    } else if (t %in% sectors) {
      components$sector <- t
    }
  }

  # -- Attempt to get label from var_tree -------------------------------------
  label_en <- NA_character_
  label_fr <- NA_character_
  label_pt <- NA_character_
  matched_var_name <- var_name

  if (!is.null(flat_tree)) {
    # try exact match first
    match_row <- dplyr::filter(flat_tree, .data$snt_var_name == var_name)

    # if no exact match, try dynamic DHS ITN pattern detection
    if (nrow(match_row) == 0) {
      dhs_dynamic <- .detect_dhs_itn_dynamic(var_name)

      if (!is.null(dhs_dynamic)) {
        # generate labels for all three languages
        label_en <- .build_dhs_itn_label(
          dhs_dynamic$metric_type,
          dhs_dynamic$age_suffix,
          dhs_dynamic$ci_type,
          "en"
        )
        label_fr <- .build_dhs_itn_label(
          dhs_dynamic$metric_type,
          dhs_dynamic$age_suffix,
          dhs_dynamic$ci_type,
          "fr"
        )
        label_pt <- .build_dhs_itn_label(
          dhs_dynamic$metric_type,
          dhs_dynamic$age_suffix,
          dhs_dynamic$ci_type,
          "pt"
        )

        # set domain component
        components$domain <- "dhs"

        # skip fuzzy matching since we found a dynamic match
      } else {
        # no dynamic match, proceed with fuzzy matching
        # normalize both the input and tree names (remove underscores, lowercase)
        normalized_input <- base::tolower(base::gsub("_", "", var_name))
      flat_tree_normalized <- flat_tree |>
        dplyr::mutate(
          normalized = base::tolower(base::gsub("_", "", .data$snt_var_name))
        )

      # compute string distances on normalized names
      distances <- utils::adist(
        normalized_input,
        flat_tree_normalized$normalized
      )[1, ]

      # find minimum distance
      min_dist <- base::min(distances)

      # accept fuzzy matches with distance <= 1 on normalized names
      # (handles underscore variations and single-character typos)
      if (min_dist <= 1) {
        best_idx <- base::which.min(distances)
        match_row <- flat_tree_normalized[best_idx, ]
        matched_var_name <- match_row$snt_var_name

        if (verbose) {
          cli::cli_alert_info(
            "No exact match for {.val {var_name}}; using fuzzy match: {.val {matched_var_name}}"
          )
        }

        # re-tokenize using the matched variable name
        tokens <- unlist(strsplit(matched_var_name, "_"))
        components <- list(
          domain = tokens[[1]],
          test_type = NA_character_,
          service_level = NA_character_,
          age_group = NA_character_,
          population_group = NA_character_,
          sector = NA_character_
        )

        for (t in tokens[-1]) {
          if (t %in% test_types) {
            components$test_type <- t
          } else if (t %in% service_levels) {
            components$service_level <- t
          } else if (t %in% age_groups) {
            components$age_group <- t
          } else if (t %in% pop_groups) {
            components$population_group <- t
          } else if (t %in% sectors) {
            components$sector <- t
          }
        }
      }
      }
    }

    if (nrow(match_row) >= 1) {
      label_en <- match_row$label_en[1]
      label_fr <- match_row$label_fr[1]
      label_pt <- match_row$label_pt[1]
    }
  }

  # -- Determine domain labels ------------------------------------------------
  domain_label_en <- stringr::str_to_title(components$domain)
  domain_label_fr <- stringr::str_to_title(components$domain)
  domain_label_pt <- stringr::str_to_title(components$domain)

  if (!is.null(flat_tree)) {
    dom_row <- dplyr::filter(flat_tree, .data$snt_var_name == components$domain)
    if (nrow(dom_row) >= 1) {
      domain_label_en <- dom_row$label_en[1] %||% domain_label_en
      domain_label_fr <- dom_row$label_fr[1] %||% domain_label_fr
      if ("label_pt" %in% names(dom_row)) {
        domain_label_pt <- dom_row$label_pt[1] %||% domain_label_pt
      }
    }
  }

  # -- Construct fallback label if missing ------------------------------------
  if (is.na(label_en) || !nzchar(label_en)) {
    label_en <- glue::glue(
      "{domain_label_en}",
      if (!is.na(components$test_type)) {
        glue::glue(" ({schema$test_types[[components$test_type]]$label_en})")
      } else {
        ""
      },
      if (!is.na(components$age_group)) {
        glue::glue(" - {schema$age_groups[[components$age_group]]$label_en}")
      } else {
        ""
      },
      if (!is.na(components$population_group)) {
        glue::glue(
          " - {schema$population_groups[[components$population_group]]$label_en}"
        )
      } else {
        ""
      },
      if (!is.na(components$sector)) {
        glue::glue(" [{schema$sectors[[components$sector]]$label_en}]")
      } else {
        ""
      },
      if (!is.na(components$service_level)) {
        glue::glue(
          " ({schema$service_levels[[components$service_level]]$label_en})"
        )
      } else {
        ""
      }
    ) |>
      stringr::str_squish()
  }

  if (is.na(label_fr) || !nzchar(label_fr)) {
    label_fr <- glue::glue(
      "{domain_label_fr}",
      if (!is.na(components$test_type)) {
        glue::glue(" ({schema$test_types[[components$test_type]]$label_fr})")
      } else {
        ""
      },
      if (!is.na(components$age_group)) {
        glue::glue(" - {schema$age_groups[[components$age_group]]$label_fr}")
      } else {
        ""
      },
      if (!is.na(components$population_group)) {
        glue::glue(
          " - {schema$population_groups[[components$population_group]]$label_fr}"
        )
      } else {
        ""
      },
      if (!is.na(components$sector)) {
        glue::glue(" [{schema$sectors[[components$sector]]$label_fr}]")
      } else {
        ""
      },
      if (!is.na(components$service_level)) {
        glue::glue(
          " ({schema$service_levels[[components$service_level]]$label_fr})"
        )
      } else {
        ""
      }
    ) |>
      stringr::str_squish()
  }

  if (is.na(label_pt) || !nzchar(label_pt)) {
    label_pt <- glue::glue(
      "{domain_label_pt}",
      if (!is.na(components$test_type)) {
        glue::glue(" ({schema$test_types[[components$test_type]]$label_pt})")
      } else {
        ""
      },
      if (!is.na(components$age_group)) {
        glue::glue(" - {schema$age_groups[[components$age_group]]$label_pt}")
      } else {
        ""
      },
      if (!is.na(components$population_group)) {
        glue::glue(
          " - {schema$population_groups[[components$population_group]]$label_pt}"
        )
      } else {
        ""
      },
      if (!is.na(components$sector)) {
        glue::glue(" [{schema$sectors[[components$sector]]$label_pt}]")
      } else {
        ""
      },
      if (!is.na(components$service_level)) {
        glue::glue(
          " ({schema$service_levels[[components$service_level]]$label_pt})"
        )
      } else {
        ""
      }
    ) |>
      stringr::str_squish()
  }

  # -- CLI Output (only if return is FALSE) ----------------------------------
  if (!return) {
    cli::cli_h1("Detected variable structure")
    cli::cli_text("Variable: {.val {matched_var_name}}")
    if (matched_var_name != var_name) {
      cli::cli_text("Original input: {.val {var_name}}")
    }
    cli::cli_text("Label (EN): {.emph {label_en}}")
    cli::cli_text("Label (FR): {.emph {label_fr}}")
    cli::cli_text("Label (PT): {.emph {label_pt}}")

    valid_components <- purrr::keep(
      components,
      ~ !is.na(.x) && !identical(.x, "") && !is.null(.x)
    )

    cli::cli_ul()
    for (nm in names(valid_components)) {
      value <- valid_components[[nm]]
      cli::cli_text("{.field {stringr::str_to_title(nm)}}: {.val {value}}")
    }
    cli::cli_end()
  }

  # save original input before creating tibble
  original_var_name <- var_name

  out <- tibble::tibble(
    var_name = matched_var_name,
    original_input = original_var_name,
    domain = components$domain,
    test_type = components$test_type,
    service_level = components$service_level,
    age_group = components$age_group,
    population_group = components$population_group,
    sector = components$sector,
    label_en = label_en,
    label_fr = label_fr,
    label_pt = label_pt
  )

  if (return) {
    return(out)
  } else {
    invisible(out)
  }
}

#' clear snt variable tree cache
#'
#' @description
#' manually clears the internal cache of flattened variable tree and schema.
#' normally not needed - cache auto-refreshes when tree version changes.
#' only useful for advanced debugging or testing.
#'
#' @return invisible NULL
#' @export
clear_snt_cache <- function() {
  base::rm(list = base::ls(envir = .snt_cache), envir = .snt_cache)
  invisible(NULL)
}
