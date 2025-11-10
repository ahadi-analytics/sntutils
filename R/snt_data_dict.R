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

#' Detect and display the structural components of an SNT variable name
#'
#' Parses a standardized SNT variable name (e.g. "conf_rdt_u5_priv")
#' and prints its logical components and bilingual label using CLI formatting.
#'
#' @param var_name Character scalar — an SNT variable name.
#' @param schema Optional list; if not provided, automatically loaded from
#'   the package's snt_var_tree dataset.
#' @param var_tree Optional list; if not provided, automatically loaded from
#'   the package's snt_var_tree dataset.
#' @param return Logical; if TRUE, also returns a tibble invisibly.
#'
#' @return Invisibly returns a tibble with parsed components and labels.
#' @export
check_snt_var <- function(
  var_name,
  schema = NULL,
  var_tree = NULL,
  return = FALSE
) {
  # -- Load schema if not provided ---------------------------------------------
  if (is.null(schema)) {
    data("snt_var_tree", package = "sntutils", envir = environment())
    schema <- snt_var_tree$schema
    if (is.null(schema)) {
      cli::cli_abort("Schema not found in snt_var_tree dataset")
    }
  }

  # -- Load variable tree for label lookup -------------------------------------
  if (is.null(var_tree)) {
    data("snt_var_tree", package = "sntutils", envir = environment())
    flat_tree <- .flatten_tree_recursive(snt_var_tree)
  } else {
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

  if (!is.null(flat_tree)) {
    match_row <- dplyr::filter(flat_tree, .data$snt_var_name == var_name)
    if (nrow(match_row) == 1) {
      label_en <- match_row$label_en
      label_fr <- match_row$label_fr
      label_pt <- match_row$label_pt
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
    cli::cli_text("Variable: {.val {var_name}}")
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

  out <- tibble::tibble(
    var_name = var_name,
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
