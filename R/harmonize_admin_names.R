#' Universal Console Clearing
#'
#' Attempts multiple methods to clear the console across different R environments
#' including RStudio, VSCode, Positron, and various terminal applications.
#'
#' @return Invisible NULL
#' @keywords internal
#' @noRd
.clear_console <- function() {
  if (!interactive()) return(invisible(NULL))

  # Method 1: Form feed (works in RStudio)
  cat("\014")

  # Method 2: ANSI escape codes (works in most terminals including VSCode)
  cat("\033[2J\033[H")

  # Method 3: System call for Unix/Mac terminals
  if (.Platform$OS.type == "unix") {
    system("clear", ignore.stdout = TRUE, ignore.stderr = TRUE)
  }

  # Method 4: System call for Windows
  if (.Platform$OS.type == "windows") {
    system("cls", ignore.stdout = TRUE, ignore.stderr = TRUE)
  }

  invisible(NULL)
}

#' Save Dataframe Using sntutils Write Function
#'
#' Prompts the user for confirmation before saving a dataframe to a file.
#' Supports all file formats supported by the sntutils write() function
#' including RDS, CSV, Excel, DTA, TSV, and more. If the specified file path
#' does not exist or is NULL, the user is prompted to provide a new path.
#' A default file name is used if no valid path is provided. If a cache file
#' already exists, the function merges new data with existing data, ensuring
#' that the most recent entries (based on `created_time`) are retained.
#'
#' @param data_to_save DataFrame to be saved.
#' @param default_save_path Default path for saving the dataframe. If not
#'                     provided or invalid, the user is prompted for a new path.
#'                     File extension determines format (e.g., .rds, .csv, .xlsx,
#'                     .dta, .tsv). Defaults to \code{NULL}.
#'
#' @return Invisible \code{NULL}. The function's primary purpose is to saving a
#'        file, not to return a value.
#'
#' @examples
#' # handle_file_save(data_to_save, "path/to/default/location.rds")
#' # handle_file_save(data_to_save, "path/to/default/location.csv")
#' # handle_file_save(data_to_save, "path/to/default/location.xlsx")
#' # handle_file_save(data_to_save, "path/to/default/location.dta")
#' # handle_file_save(data_to_save, "path/to/default/location.tsv")
#'
#' @keywords internal
#' @noRd
handle_file_save <- function(data_to_save, default_save_path = NULL) {
  cache_path <- default_save_path

  while (TRUE) {
    # Prompt user for confirmation
    confirm_save <- tolower(
      readline("Do you want to save the cleaned cache file? [y/n]: ")
    )

    if (confirm_save == "y") {
      # Validate or set a default cache path
      if (is.null(cache_path) || dir.exists(cache_path)) {
        cli::cli_alert_warning(
          "The specified path is null or is a directory."
        )

        # Prompt for a valid file path
        cache_path <- readline(
          prompt = "Enter the new file path (including filename) for saving: "
        )

        # Ensure the file path includes a filename
        if (cache_path == "" || dir.exists(cache_path)) {
          cache_path <- file.path(getwd(), "prepped_geoname_cache.rds")
          cli::cli_alert_info(
            "No valid path provided. Using default: {cache_path}"
          )
        }
      }

      # Ensure parent directory exists
      cache_dir <- dirname(cache_path)
      if (!dir.exists(cache_dir)) {
        dir.create(cache_dir, recursive = TRUE)
      }

      # Ensure we are working with a file, not a directory
      if (dir.exists(cache_path)) {
         cli::cli_abort("`cache_path` should be a file path, not a directory.")
      }

      # Set up file lock to prevent concurrent overwrites
      lock_path <- paste0(cache_path, ".lock")
      lock <- filelock::lock(lock_path, timeout = 10000)
      on.exit(filelock::unlock(lock))
      file.remove(lock_path)

      # Load existing cache if available using sntutils read()
      existing_cache <- if (file.exists(cache_path)) {
        tryCatch(
          read(cache_path),
          error = function(e) NULL
        )
      } else {
        NULL
      }

      # Merge with existing cache if applicable
      if (!is.null(existing_cache) && nrow(existing_cache) > 0) {
        merged_cache <- dplyr::bind_rows(existing_cache, data_to_save) |>
          dplyr::arrange(dplyr::desc(created_time)) |>
          dplyr::distinct(level, name_to_match, .keep_all = TRUE)
      } else {
        merged_cache <- data_to_save
      }

      # Save the merged cache using sntutils write()
      write(merged_cache, cache_path)
      cli::cli_alert_success("File saved successfully to {cache_path}.")
      break
    } else if (confirm_save == "n") {
      cli::cli_alert_info("File not saved. Proceeding without saving...")
      break
    } else {
      cli::cli_alert_warning("Invalid input. Please respond with 'y' or 'n'.")
    }
  }
}

#' Get Hierarchical Administrative Combinations
#'
#' Helper function to extract unique hierarchical combinations of administrative
#' levels from a dataframe. This ensures proper counting of admin units as
#' combinations rather than isolated values.
#'
#' @param df A dataframe containing administrative level columns
#' @param levels Character vector of column names representing hierarchical
#'  admin levels
#'
#' @return A dataframe with unique combinations of the specified levels,
#'         excluding rows with any NA values
#'
#' @details This function is used internally by calculate_match_stats to ensure
#'          administrative units are counted as hierarchical combinations
#'          (e.g., country-province-district) rather than as isolated unique
#'          values
#'
#' @examples
#' # df <- data.frame(
#' #   country = c("A", "A", "B"),
#' #   province = c("P1", "P2", "P1"),
#' #   district = c("D1", "D1", "D2")
#' # )
#' # get_hierarchical_combinations(df, c("country", "province", "district"))
#'
#' @keywords internal
#' @noRd
get_hierarchical_combinations <- function(df, levels) {
  if (length(levels) == 0 || is.null(levels)) {
    return(data.frame())
  }

  # If input is an sf object, drop geometry to avoid list columns
  if (inherits(df, "sf")) {
    df <- sf::st_drop_geometry(df)
  }

  # Filter to only the specified levels that exist in the dataframe
  existing_levels <- levels[levels %in% names(df)]
  if (length(existing_levels) == 0) {
    return(data.frame())
  }

  # Remove rows with any NA values and get unique combinations
  df_subset <- df[, existing_levels, drop = FALSE]
  df_clean <- df_subset[stats::complete.cases(df_subset), , drop = FALSE]
  unique(df_clean)
}

#' Calculate and report geo-naming match statistics
#'
#' Compares entries in a dataset against a lookup across specified admin levels
#' (e.g., country, province/state/region, district, subdistrict, settlement)
#' and reports match statistics to the console.
#'
#' @param data A data frame containing the target data to be matched.
#' @param lookup_data A data frame serving as the reference for matching.
#' @param level0 Column name (country) present in both `data` and `lookup_data`.
#' @param level1 Column name (province/state/region) present in both datasets.
#' @param level2 Column name (district) present in both datasets.
#' @param level3 Column name (subdistrict) present in both datasets.
#' @param level4 Column name (settlement) present in both datasets.
#'
#' @details
#' - Input columns supplied via `level*` are normalized to lower case before
#'   matching.
#' - *Base N* for each side is the count of **unique hierarchical names**
#'   formed from the supplied levels (e.g., `level0_level1_...`). Per-level
#'   rows show matches out of each side's Base N.
#' - Console output includes:
#'   1) a two-column summary (Target vs Lookup as base N),
#'   2) side-level completeness messages (success/info/warning),
#'   3) a per-level report of **missing names** (NA or empty strings) on
#'      either side. These missing names are **not included in N** only if the
#'      implementation drops them before counting (see Note below).
#'
#' @note
#' If you want missing names (NA/empty) **excluded from Base N**, ensure the
#' key-building step drops them before counting (see example patch below).
#'
#' @return Invisibly returns `NULL`. Output is produced via `cli` messages.
#'
#' @examples
#' # minimal runnable example (toy data)
#' data <- data.frame(
#'   country = c("Kenya", "Kenya", "Uganda"),
#'   district = c("Nairobi", "Kisumu", "Kampala")
#' )
#' lookup <- data.frame(
#'   country = c("Kenya", "Uganda"),
#'   district = c("Nairobi", "Kampala")
#' )
#' calculate_match_stats(
#'   data, lookup, level0 = "country", level2 = "district"
#' )
#' @export
calculate_match_stats <- function(
  data,
  lookup_data,
  level0 = NULL,
  level1 = NULL,
  level2 = NULL,
  level3 = NULL,
  level4 = NULL
) {

  # normalize case
  levels_vec <- c(level0, level1, level2, level3, level4) |>
    (\(v) v[!vapply(v, is.null, logical(1))])()

  if (length(levels_vec) > 0) {
    for (lv in levels_vec) {
      if (lv %in% names(data)) {
        data[[lv]] <- tolower(as.character(data[[lv]]))
      }
      if (lv %in% names(lookup_data)) {
        lookup_data[[lv]] <- tolower(as.character(lookup_data[[lv]]))
      }
    }
  }

  # helpers ---------------------------------------------------------------

  compose_fields <- function(...) {
    f <- c(...)
    f[!vapply(f, is.null, logical(1))]
  }

  build_keys <- function(df, fields) {
    if (length(fields) == 0) {
      return(character(0))
    }
    combos <- get_hierarchical_combinations(df, fields)
    if (nrow(combos) == 0) {
      return(character(0))
    }
    if (length(fields) == 1) {
      return(unique(combos[[fields]]))
    }
    unique(do.call(paste, c(combos[fields], sep = "_")))
  }

  resolve_label <- function(label, fallback) {
    if (is.null(label) || is.na(label) || identical(label, "")) {
      fallback
    } else {
      label
    }
  }

  big_mark <- function(n) {
    if (is.na(n)) "NA" else formatC(n, format = "d", big.mark = ",")
  }

  paint_matches <- function(matches, total) {
    num <- big_mark(matches)
    if (!is.na(matches) && !is.na(total) && matches < total) {
      cli::col_red(num)
    } else {
      num
    }
  }

  results <- list()

  process_level <- function(level_key, level_num, fields, label) {
    data_keys <- build_keys(data, fields)
    lookup_keys <- build_keys(lookup_data, fields)
    matches <- length(base::intersect(data_keys, lookup_keys))
    results[[level_key]] <<- list(
      label = resolve_label(label, level_key),
      level_num = level_num,
      matches = matches,
      total_data = length(data_keys),
      total_lookup = length(lookup_keys)
    )
  }

  # compute per-level
  if (!is.null(level0)) {
    process_level("level0", 0L, compose_fields(level0), level0)
  }
  if (!is.null(level1)) {
    f1 <- if (!is.null(level0)) {
      compose_fields(level0, level1)
    } else {
      compose_fields(level1)
    }
    process_level("level1", 1L, f1, level1)
  }
  if (!is.null(level2)) {
    process_level("level2", 2L, compose_fields(level0, level1, level2), level2)
  }
  if (!is.null(level3)) {
    process_level(
      "level3",
      3L,
      compose_fields(level0, level1, level2, level3),
      level3
    )
  }
  if (!is.null(level4)) {
    process_level(
      "level4",
      4L,
      compose_fields(level0, level1, level2, level3, level4),
      level4
    )
  }

  # enforce display order
  ordered_keys <- c("level0", "level1", "level2", "level3", "level4")
  rows <- results[ordered_keys]
  rows <- rows[!vapply(rows, is.null, logical(1))]

  # headers
  left_hdr <- "Target data as base N"
  right_hdr <- "Lookup data as base N"

  bullet <- "\u2022"

  build_left <- function(r) {
    glue::glue(
      "{bullet} {r$label} (level{r$level_num}): ",
      "{paint_matches(r$matches, r$total_data)} out of ",
      "{big_mark(r$total_data)} matched"
    )
  }
  build_right <- function(r) {
    glue::glue(
      "{bullet} {r$label} (level{r$level_num}): ",
      "{paint_matches(r$matches, r$total_lookup)} out of ",
      "{big_mark(r$total_lookup)} matched"
    )
  }

  left_lines <- if (length(rows)) {
    vapply(rows, build_left, character(1))
  } else {
    character(0)
  }
  right_lines <- if (length(rows)) {
    vapply(rows, build_right, character(1))
  } else {
    character(0)
  }

  # two columns
  content_vec <- cli::ansi_columns(
    c(
      cli::col_cyan(left_hdr),
      left_lines,
      cli::col_cyan(right_hdr),
      right_lines
    ),
    width = max(40L, cli::console_width() - 4L),
    max_cols = 2,
    fill = "col",
    align = "left",
    sep = "  "
  )

  cli::cli_h1(glue::glue("{cli::symbol$info} Match Summary"))
   cat("\n")
  # compute completeness by side across all reported levels
  # (complete = all matches equal denominators on that side)
  target_complete <- length(rows) > 0 &&
    all(vapply(
      rows,
      \(r) isTRUE(r$matches == r$total_data),
      logical(1)
    ))
  lookup_complete <- length(rows) > 0 &&
    all(vapply(
      rows,
      \(r) isTRUE(r$matches == r$total_lookup),
      logical(1)
    ))

  # emit side-by-side summary message
  if (target_complete && lookup_complete) {
    # both sides perfect
    cli::cli_alert_success(
      "Hierarchies are aligned across data and lookup."
    )
  } else if (target_complete && !lookup_complete) {
    # target perfect, lookup has extras
    cli::cli_alert_info("Lookup has extra names not in data.")
  } else if (!target_complete && lookup_complete) {
    # lookup perfect, target has extras
    cli::cli_alert_info("Data has names not in lookup.")
  } else {
    # both imperfect
    cli::cli_alert_warning(
      "Both sides have unmatched names; see per-level lines below."
    )
  }

  # check and report missing names per provided level columns
  # (counts of NA or empty-string values in each dataset)
  if (length(levels_vec) > 0) {
    # count missing in data
    miss_data <- vapply(
      levels_vec,
      \(lv) {
        if (lv %in% names(data)) {
          sum(is.na(data[[lv]]) | data[[lv]] == "", na.rm = TRUE)
        } else {
          0L
        }
      },
      integer(1)
    )

    # count missing in lookup
    miss_lookup <- vapply(
      levels_vec,
      \(lv) {
        if (lv %in% names(lookup_data)) {
          sum(is.na(lookup_data[[lv]]) | lookup_data[[lv]] == "", na.rm = TRUE)
        } else {
          0L
        }
      },
      integer(1)
    )

    any_missing <- any(miss_data > 0L | miss_lookup > 0L)

    if (isTRUE(any_missing)) {
      # header alert
      cli::cli_alert_warning(
        "Missing names detected in supplied levels (not included in N)."
      )

      # per-level report lines (only show sides with missing values)
      for (i in seq_along(levels_vec)) {
        msgs <- c()
        if (miss_data[i] > 0L) {
          msgs <- c(msgs, glue::glue("data = {miss_data[i]}"))
        }
        if (miss_lookup[i] > 0L) {
          msgs <- c(msgs, glue::glue("lookup = {miss_lookup[i]}"))
        }
        if (length(msgs) > 0L) {
          cli::cli_text(cli::col_silver(
            glue::glue("- {levels_vec[i]}: {paste(msgs, collapse = ', ')}")
          ))
        }
      }
    }
  }
   cat("\n")
  # final box, no borders
  cli::cat_line(content_vec)
}

#' Format a single choice for display
#'
#' @param index Numeric index of the choice
#' @param choice Character string of the choice text
#' @param width Total width of the formatted choice
#' @return Formatted choice string
#' @noRd
format_choice <- function(index, choice, width) {
  number_part <- sprintf("%3d: ", index)
  remaining_width <- width - nchar(number_part)
  if (nchar(choice) > remaining_width - 4) { # -4 for "... "
    choice_part <- paste0(substr(choice, 1, remaining_width - 4), "... ")
  } else {
    choice_part <- choice
  }
  choice_part <- stringr::str_pad(choice_part, remaining_width, "right")
  paste0(number_part, choice_part)
}

#' Format all choices into columns
#'
#' @param choices Vector of choice strings
#' @param num_columns Number of columns to display
#' @param column_width Width of each column
#' @return Vector of formatted choice strings, one per row
#' @noRd
format_choices <- function(choices, num_columns, column_width = 60) {
  num_choices <- length(choices)
  rows_per_column <- ceiling(num_choices / num_columns)

  formatted_choices <- character(rows_per_column)
  for (i in 1:rows_per_column) {
    row_parts <- character(num_columns)
    for (j in 1:num_columns) {
      index <- (j - 1) * rows_per_column + i
      if (index <= num_choices) {
        row_parts[j] <- format_choice(index, choices[index], column_width)
      } else {
        row_parts[j] <- paste(rep(" ", column_width), collapse = "")
      }
    }
    formatted_choices[i] <- paste(row_parts, collapse = "")
  }
  formatted_choices
}

#' Display a Custom Menu and Capture User Choice
#'
#' An alternative to base R's `menu` function, using `cli` for enhanced
#' interactivity and style. Displays a menu with given options and special
#' actions, capturing user selection through a custom prompt.
#'
#' @param title The menu title.
#' @param main_header The main header for the menu display.
#' @param choices_input Vector of option strings to display.
#' @param special_actions Named list of special actions with string identifiers.
#' @param prompt String to display for user input prompt.
#' @param column_width Numeric; the maximum width (in characters) for each
#'        column in the menu display. Default is 60.
#'
#' @return The selected option's identifier (numeric or special action key).
#' @importFrom cli cli_h1 cli_h2 cli_text cli_alert_warning
#' @examples
#' # display_custom_menu("Choose an option:", "Main Menu",
#' #                     c("Option 1", "Option 2"),
#' #                     list(x = "Skip", y = "Save"), "Your choice:", 20)
#'
#' @keywords internal
#' @noRd
display_custom_menu <- function(title, main_header, choices_input,
                                special_actions, prompt,
                                column_width = 60) {
  cli::cli_h1(main_header)
  cli::cli_h2(title)

  num_choices <- length(choices_input)
  num_columns <- if (num_choices > 50) 3 else if (num_choices > 25) 2 else 1

  formatted_choices <- format_choices(choices_input, num_columns,
    column_width = column_width
  )
  cat("\n")
  cat(formatted_choices, sep = "\n")
  cat("\n")

  for (key in names(special_actions)) {
    cat(sprintf("%s: %s\n", key, special_actions[[key]]))
  }

  cat("\n")
  repeat {
    choice <- tolower(readline(prompt = paste0(prompt)))
    if (choice %in% c(
      as.character(seq_along(choices_input)),
      tolower(names(special_actions))
    )) {
      break
    }
    cat("Invalid choice, please try again.\n")
  }

  choice
}

#' Calculate String distances Between Admin Names
#'
#' Computes the string distances between administrative names to be cleaned and
#' a set of lookup administrative names using the specified method. Returns the
#' top N closest matches for each name.
#'
#' @param admins_to_clean A vector of administrative names to be cleaned.
#' @param lookup_admins A vector of administrative names for lookup.
#' @param method The method used to calculate string distances (e.g., "jaro",
#'        "levenshtein"). It can take a number of alo
#'
#' @return A dataframe detailing top N matches for each name, including
#'        algorithm_name, name, matches, distances, and ranks.
#'
#'
#' @examples
#' # calculate_string_distance(c("New York", "Los Angeles"),
#' # c("New York", "Los Angeles", "Chicago"),
#' #   method = "lv"
#' # )
#'
#' @keywords internal
#' @noRd
calculate_string_distance <- function(
    admins_to_clean, lookup_admins, method) {
  # Create a data frame with all combinations
  results <- tidyr::expand_grid(
    name_to_match = admins_to_clean,
    matched_names = lookup_admins
  ) |> dplyr::distinct()

  # Calculate distances for each pair
  results <- results |>
    dplyr::rowwise() |>
    dplyr::mutate(
      distance = stringdist::stringdist(
        name_to_match, matched_names,
        method = method
      )
    ) |>
    dplyr::ungroup()

  # Sort results for each name_to_match based on distance
  results <- results |>
    dplyr::group_by(name_to_match) |>
    dplyr::arrange(distance, .by_group = TRUE) |>
    dplyr::mutate(match_rank = dplyr::row_number()) |>
    dplyr::ungroup()

  # Add algorithm name
  results$algorithm_name <- method

  # Reorder columns
  results <- results |>
    dplyr::select(
      algorithm_name, name_to_match,
      matched_names, distance, match_rank
    )

  results
}

#' Interact with Users for Data Cleaning Choices
#'
#' Presents an interactive CLI menu using `cli` for users to make selections on
#' data cleaning choices, particularly for administrative names like countries,
#' provinces, districts, subdistricts and settlements. It allows users to
#' replace, skip, save, or exit, incorporating user feedback into the data
#' cleaning process.
#'
#' @param input_data Data frame containing admin names etc.,.
#' @param levels The avaiable admin levels withnin the prep_geoname function
#' @param level The admins level being cleaned, i.ie level1 or even disrict.
#' @param clear_console Logical, whether to clear the console before showing
#'                  prompts; defaults to TRUE.
#' @param max_options Maximum number of options to display in the menu.
#'       Default is 200.
#' @param column_width Numeric; the maximum width (in characters) for each
#'       column in the interactive menu display. Default is 60.
#'
#' @return Data frame of user-selected replacements if any; otherwise,
#'          provides feedback based on user actions.
#'
#' @examples
#' # handle_user_interaction(my_data, "level1", TRUE, TRUE, 20)
#'
#' @keywords internal
#' @noRd
handle_user_interaction <- function(input_data, levels, level,
                                    clear_console = TRUE,
                                    max_options,
                                    column_width = 60) {
  # Interactivity --------------------------------------------------------------

  # set up the messaging prompts at the start of the function
  prompts <- c(
    "What'll it be?:",
    "Your next move?:",
    "How shall we proceed?:",
    "Pick your path:",
    "Let's make a choice!:",
    "Forge ahead to:",
    "Plot your trajectory:",
    "Navigate your destiny:",
    "Select and soar!:",
    "Decisions, decisions!:",
    "Where to next?:"
  )
  prompt <- sample(prompts)[1]

  # filter out missing cachees
  input_data <- input_data |>
    dplyr::filter(
      !is.na(matched_names) & !is.na(name_to_match)
    )

  # set cachees for looping - use unique (name, context) pairs to avoid

  # cross-context contamination when same name appears in multiple hierarchies
  unique_contexts <- input_data |>
    dplyr::distinct(name_to_match, long_geo)

  # initialize empty lists to store user choices
  user_choices <- list()
  user_choice <- NULL

  # loop through unmatched records in input_data
  # Initialize the index
  i <- 1
  while (i <= nrow(unique_contexts)) {
    # clear console
    if (clear_console) {
      .clear_console()
    }

    # Define color using crayon function
    red <- crayon::red
    bl <- crayon::blue
    gr <- crayon::green
    b <- crayon::bold
    p <- crayon::underline

    # set up choices -----------------------------------------------------------
    # select the cache to clean and suggested replacements
    name_to_clean <- unique_contexts$name_to_match[i]
    current_long_geo <- unique_contexts$long_geo[i]
    replacement_name <- input_data |>
      dplyr::filter(
        name_to_match == name_to_clean,
        long_geo == current_long_geo
      ) |>
      dplyr::distinct(matched_names) |>
      # narrow down to top max_options
      dplyr::slice(0:max_options) |>
      dplyr::pull() |>
      stringr::str_to_title()

    # get unique long names for this specific context
    unique_geo_long <- input_data |>
      dplyr::filter(
        name_to_match == name_to_clean,
        long_geo == current_long_geo
      ) |>
      dplyr::distinct()

    # set output title ---------------------------------------------------------

    # set up main header to keep track
    main_header <- glue::glue(
      "{stringr::str_to_title(level)} {i} of {nrow(unique_contexts)}"
    )

    if (!is.na(levels[2]) && level == levels[2]) {
      level_label <- "level1"
      long_geo <- unique_geo_long$long_geo[1]
      str_cache <- stringr::str_to_title(name_to_clean)
      str_long_geo <- stringr::str_to_title(long_geo)
      title <- glue::glue(
        "Which {level} name would you like to replace {b(red(str_cache))}",
        " within {bl(str_long_geo)}?"
      )
    } else if (!is.na(levels[3]) && level == levels[3]) {
      level_label <- "level2"
      long_geo <- unique_geo_long$long_geo[1]
      long_geo_country <- stringr::str_to_title(
        strsplit(long_geo, "_")[[1]][[1]]
      )
      long_geo_province <- stringr::str_to_title(
        strsplit(long_geo, "_")[[1]][[2]]
      )
      str_cache <- stringr::str_to_title(name_to_clean)
      title <- glue::glue(
        "Which {level} name would you like to replace {b(red(str_cache))}",
        " within the {gr(long_geo_province)} {levels[2]} ",
        "of {bl(long_geo_country)}?"
      )
    } else if (!is.na(levels[4]) && level == levels[4]) {
      level_label <- "level3"
      long_geo <- unique_geo_long$long_geo[1]
      long_geo_split <- strsplit(long_geo, "_")[[1]]
      long_geo_country <- stringr::str_to_title(long_geo_split[1])
      long_geo_province <- stringr::str_to_title(long_geo_split[2])
      long_geo_district <- stringr::str_to_title(long_geo_split[3])
      str_cache <- stringr::str_to_title(name_to_clean)
      title <- glue::glue(
        "Which {level} name would you like to replace {b(red(str_cache))}",
        " within the {gr(long_geo_district)} {levels[3]} of ",
        "{gr(long_geo_province)} {levels[2]} in {bl(long_geo_country)}?"
      )
    } else if (!is.na(levels[5]) && level == levels[5]) {
      level_label <- "level4"
      long_geo <- unique_geo_long$long_geo[1]
      long_geo_split <- strsplit(long_geo, "_")[[1]]
      long_geo_country <- stringr::str_to_title(long_geo_split[1])
      long_geo_province <- stringr::str_to_title(long_geo_split[2])
      long_geo_district <- stringr::str_to_title(long_geo_split[3])
      long_geo_ward <- stringr::str_to_title(long_geo_split[4])
      str_cache <- stringr::str_to_title(name_to_clean)
      title <- glue::glue(
        "Which {level} name would you like to replace {b(red(str_cache))}",
        " within the {gr(long_geo_ward)} {levels[4]} of ",
        "{gr(long_geo_district)} {levels[3]} of ",
        "{gr(long_geo_province)} {levels[2]} in {bl(long_geo_country)}?"
      )
    } else if (!is.na(levels[1]) && level == levels[1]) {
      level_label <- "level0"
      long_geo <- unique_geo_long$long_geo[1]
      str_cache <- stringr::str_to_title(name_to_clean)
      title <- glue::glue(
        "Which {level} name would you like to replace ",
        "{b(red(str_cache))} with?"
      )
    } else {
      long_geo <- unique_geo_long$long_geo[1]
      str_cache <- stringr::str_to_title(name_to_clean)
      title <- glue::glue(
        "Which {level} name would you like to replace ",
        "{b(red(str_cache))} with?"
      )
    }

    # action set up ------------------------------------------------------------
    special_actions <- list(
      "B" = "Go Back",
      "S" = "Skip this one",
      "E" = "Save and exit",
      "Q" = "Exit without saving",
      "M" = "Enter name manually"
    )

    # present the menu to the user ---------------------------------------------
    user_choice <- display_custom_menu(
      title, main_header,
      replacement_name,
      special_actions,
      prompt = prompt,
      column_width = column_width
    )

    # handle user choices ------------------------------------------------------
    if (user_choice == "b") { # Go Back
      if (i > 1) {
        i <- i - 1
        next
      } else {
        cli::cli_alert_warning("You can't go back further.")
      }
    } else if (user_choice == "s") { # Skip this one
      cli::cli_alert_info("You are skipping this one...")
      i <- i + 1
      if (clear_console) .clear_console()
      next
    } else if (user_choice == "e") { # Save and exit
      if (length(user_choices) > 0) {
        cli::cli_alert_success("Choices saved successfully. Exiting...")
      } else {
        cli::cli_alert_warning("No choices to save.")
      }
      break # exit the loop entirely
    } else if (user_choice == "q") { # Exit without saving
      confirm_exit <- tolower(
        readline("Are you sure you want to exit without saving? [y/n]: ")
      )
      if (confirm_exit == "y") {
        cli::cli_alert_danger("You have exited without saving...")
        return(NULL)
      } else {
        cli::cli_alert_info("Returning to menu...")
      }
    } else if (user_choice == "m") { # Enter name manually
      manual_name <- readline(prompt = "Enter the name manually: ")
      if (manual_name != "") {
        replace_int <- toupper(gsub("\u00A0", " ", manual_name))
        user_choices[[length(user_choices) + 1]] <- data.frame(
          level = level_label,
          name_to_match = name_to_clean,
          replacement = replace_int,
          longname_to_match = ifelse(
            level != levels[1], paste0(long_geo, "_", name_to_clean),
            name_to_clean
          ),
          longname_corrected = ifelse(
            level != levels[1], paste0(long_geo, "_", replace_int), long_geo
          ),
          created_time = format(Sys.time(), tz = "UTC", usetz = TRUE)
        )
        cli::cli_alert_success("Manual name entered successfully.")
      } else {
        cli::cli_alert_warning("No name entered. Returning to menu...")
      }
      i <- i + 1
      if (clear_console) .clear_console()
    } else {
      suppressWarnings({
        replace_int <- toupper(replacement_name[as.integer(user_choice)])
        user_choices[[length(user_choices) + 1]] <- data.frame(
          level = level_label,
          name_to_match = name_to_clean,
          replacement = replace_int,
          longname_to_match = ifelse(
            level != levels[1], paste0(long_geo, "_", name_to_clean),
            name_to_clean
          ),
          longname_corrected = ifelse(
            level != levels[1], paste0(long_geo, "_", replace_int), long_geo
          ),
          created_time = format(Sys.time(), tz = "UTC", usetz = TRUE)
        )
      })
      i <- i + 1
      if (clear_console) .clear_console()
    }
  }

  # Aggregation user-chosen replacements into df -------------------------------

  # clear console
  if (clear_console) {
    .clear_console()
  }

  if (length(user_choices) != 0) {
    # Combine user choices into a single data frame
    user_choices_df <- dplyr::bind_rows(user_choices) |>
      # fix longname_corrected for country
      dplyr::mutate(
        longname_to_match = dplyr::if_else(
          level == "level0", replacement, longname_to_match
        ),
        longname_corrected = dplyr::if_else(
          level == "level0", replacement, longname_corrected
        )
      ) |>
      # drop duplicates as a result of going back on choices
      dplyr::group_by(longname_to_match) |>
      dplyr::slice_max(created_time, with_ties = FALSE) |>
      dplyr::ungroup()

    cli::cli_alert_success(
      "Your selections have been successfully saved. Exiting..."
    )
    # return results
    user_choices_df
  } else {
    cli::cli_alert_warning(
      "No selections were made to save. Exiting..."
    )
    data.frame(
      level = NULL,
      name_to_match = NULL,
      replacement = NULL,
      longname_to_match = NULL,
      longname_corrected = NULL,
      created_time = NULL
    )
  }
}

#' Clean UTF-8 Encoding for Geographic Names
#'
#' Converts text to UTF-8 encoding and removes special characters that may
#' cause matching issues, keeping only letters, numbers, spaces, hyphens,
#' and underscores.
#'
#' @param x Character vector to clean
#' @return Character vector with cleaned UTF-8 text
#' @keywords internal
#' @noRd
.clean_utf8 <- function(x) {
  x |>
    stringi::stri_enc_toutf8(validate = FALSE) |>
    stringi::stri_replace_all_regex(
      "[^\\p{L}\\p{N}\\s\\-\\_]",
      ""
    )
}

#' Construct Long Geographic Names
#'
#' This function creates a composite geographic identifier by concatenating
#' values from specified administrative level columns within a dataframe.
#' @param data A dataframe containing the geographic data.
#' @param level0 level0 col name (country) in both 'data' and 'lookup_data'.
#' @param level1 level1 col name (province) in both 'data' and 'lookup_data'.
#' @param level2 level2 col name (district) in both 'data' and 'lookup_data'.
#' @param level3 level3 col name (subdistrict) in both 'data' and 'lookup_data'.
#' @param level4 level3 col name (settlements) in both 'data' and 'lookup_data'.
#'
#' @return Returns the dataframe with an additional column `long_geo` that
#'         contains the concatenated geographic identifiers.
#'
#' @examples
#' # Assuming `data` is a dataframe with columns 'country', 'state', 'city',
#' # and 'subdistrict':
#' # data <- data.frame(
#' # country = c("USA", "USA", "Canada"),
#' #  state = c("California", NA, "Ontario"),
#' #  city = c("Los Angeles", "New York", "Toronto"),
#' #  subdistrict = c("Downtown", "Manhattan", "Scarborough")
#' # )
#' # result <- construct_geo_names(data, "country", "state", "city",
#' # subdistrict")
#' @noRd
construct_geo_names <- function(data, level0, level1, level2,
                                level3 = NULL, level4 = NULL) {
  data |>
    dplyr::rowwise() |>
    dplyr::mutate(long_geo = {
      non_null_adms <- NULL
      if (!is.null(level0) && !is.na(.data[[level0]])) {
        non_null_adms <- c(non_null_adms, .data[[level0]])
      }
      if (!is.null(level1) && !is.na(.data[[level1]])) {
        non_null_adms <- c(non_null_adms, .data[[level1]])
      }
      if (!is.null(level2) && !is.na(.data[[level2]])) {
        non_null_adms <- c(non_null_adms, .data[[level2]])
      }
      if (!is.null(level3) && !is.na(.data[[level3]])) {
        non_null_adms <- c(non_null_adms, .data[[level3]])
      }
      if (!is.null(level4) && !is.na(.data[[level4]])) {
        non_null_adms <- c(non_null_adms, .data[[level4]])
      }
      paste(non_null_adms, collapse = "_")
    }) |>
    dplyr::ungroup()
}

#' Apply Case Mapping to Data Frame
#'
#' Internal helper function to apply original case from lookup data
#' to the matched values in the target data frame.
#'
#' @param df Data frame to apply case mapping to
#' @param case_mapping List of case mappings for each level
#' @param levels Vector of level column names
#'
#' @keywords internal
#' @noRd
apply_case_mapping <- function(df, case_mapping, levels) {
  if (is.null(case_mapping)) {
    return(df)
  }

  for (level in levels) {
    if (!is.null(level) && level %in% names(df) && level %in% names(case_mapping)) {
      mapping <- case_mapping[[level]]

      # Join with mapping to get original case
      df <- df |>
        dplyr::left_join(
          mapping,
          by = stats::setNames("uppercase", level)
        ) |>
        dplyr::mutate(
          !!rlang::sym(level) := dplyr::coalesce(original, .data[[level]])
        ) |>
        dplyr::select(-original)
    }
  }

  return(df)
}

#' Get User Identity
#'
#' Helper function to get the user identity from various environment variables
#'
#' @return Character string with the user identity
#' @keywords internal
#' @noRd
get_user_identity <- function() {
  if (nzchar(Sys.getenv("RSTUDIO_USER_IDENTITY"))) {
    Sys.getenv("RSTUDIO_USER_IDENTITY")
  } else if (nzchar(Sys.getenv("USER"))) {
    Sys.getenv("USER")
  } else if (nzchar(Sys.getenv("USERNAME"))) {
    Sys.getenv("USERNAME")
  } else {
    Sys.info()[["user"]]
  }
}

#' Export Unmatched Administrative Names
#'
#' Internal helper function to export complete rows with unmatched data
#' to a file with metadata, showing full hierarchical context.
#'
#' @param target_todo Data frame containing unmatched records
#' @param unmatched_export_path Path to save the unmatched data
#' @param level0 Name of level0 column
#' @param level1 Name of level1 column
#' @param level2 Name of level2 column
#' @param level3 Name of level3 column
#' @param level4 Name of level4 column
#'
#' @keywords internal
#' @noRd
export_unmatched_data <- function(target_todo, unmatched_export_path,
                                 level0, level1, level2, level3, level4) {

  if (nrow(target_todo) > 0) {
    # Get the source names from the tagged data
    target_name <- if ("target_data" %in% names(target_todo)) {
      unique(target_todo[["target_data"]])[1]
    } else {
      NA_character_
    }
    lookup_name <- if ("lookup_data" %in% names(target_todo)) {
      unique(target_todo[["lookup_data"]])[1]
    } else {
      NA_character_
    }

    # Build list of columns to keep - dynamic based on input
    cols_to_keep <- c()
    if (
      !is.null(level0) &&
        level0 %in% names(target_todo)
    ) {
      cols_to_keep <- c(cols_to_keep, level0)
    }
    if (
      !is.null(level1) &&
        level1 %in% names(target_todo)
    ) {
      cols_to_keep <- c(cols_to_keep, level1)
    }
    if (
      !is.null(level2) &&
        level2 %in% names(target_todo)
    ) {
      cols_to_keep <- c(cols_to_keep, level2)
    }
    if (
      !is.null(level3) &&
        level3 %in% names(target_todo)
    ) {
      cols_to_keep <- c(cols_to_keep, level3)
    }
    if (
      !is.null(level4) &&
        level4 %in% names(target_todo)
    ) {
      cols_to_keep <- c(cols_to_keep, level4)
    }

    # Identify the unmatched column (typically the lowest/most granular level)
    # Start from level4 (most granular) and work up
    unmatched_column <- NA
    if (!is.null(level4) && level4 %in% cols_to_keep) {
      unmatched_column <- level4  # e.g., "hf"
    } else if (!is.null(level3) && level3 %in% cols_to_keep) {
      unmatched_column <- level3  # e.g., "adm3"
    } else if (!is.null(level2) && level2 %in% cols_to_keep) {
      unmatched_column <- level2  # e.g., "adm2"
    } else if (!is.null(level1) && level1 %in% cols_to_keep) {
      unmatched_column <- level1  # e.g., "adm1"
    } else if (!is.null(level0) && level0 %in% cols_to_keep) {
      unmatched_column <- level0  # e.g., "adm0"
    }

    # Create the export dataframe with all context
    # Keep complete rows showing the hierarchical context
    unmatched_df <- target_todo |>
      dplyr::select(dplyr::all_of(cols_to_keep)) |>
      dplyr::distinct() |>
      dplyr::mutate(
        unmatched_column = unmatched_column,
        target_data = target_name,
        lookup_data = lookup_name,
        created_time = Sys.time(),
        name_of_creator = get_user_identity()
      ) |>
      # Reorganize columns - unmatched_column first, then admin levels,
      # then metadata
      dplyr::select(
        "unmatched_column",
        dplyr::all_of(cols_to_keep),
        "target_data",
        "lookup_data",
        "created_time",
        "name_of_creator"
      )

    # Save using sntutils write() function
    tryCatch({
      write(unmatched_df, unmatched_export_path)
      cli::cli_alert_success(
        "Unmatched data exported to: {.file {unmatched_export_path}}"
      )

      # Show summary of unmatched data
      # Extract the column name for display
      col_name <- if (!is.na(unmatched_column)) unmatched_column else "unknown"
      cli::cli_alert_info(
        "Exported {nrow(unmatched_df)} unmatched rows for column '{col_name}'"
      )
    }, error = function(e) {
      cli::cli_alert_warning(
        "Failed to export unmatched data: {e$message}"
      )
    })
  } else {
    cli::cli_alert_info("No unmatched values to export")
  }
}

#' Interactive Admin Name Cleaning and Matching
#'
#' This function streamlines the admin name cleaning process, leveraging both
#' algorithmic approaches and interactive user decisions. It combines string
#' distance algorithms for initial matching and offers user interactivity for
#' final decision-making, which are then saved for future reference and sharing.
#' Although the function does not require limiting name matching exclusively to
#' upper-level admins, optimal performance is achieved by confining to stricter
#' within-admin stratifications,
#' ensuring more accurate results. The function can also work with site names
#' or even any string matching that has lookup data.
#'
#' @param target_df Data frame containing the admin names to clean.
#' @param lookup_df Lookup data frame for verifying admin names. If this is not
#'                  provided, an internal version of WHO geoname data
#'                  attached to sntutils is used.
#' @param level0 level0 col name (country) in both 'data' and 'lookup_data'.
#' @param level1 level1 col name (province) in both 'data' and 'lookup_data'.
#' @param level2 level2 col name (district) in both 'data' and 'lookup_data'.
#' @param level3 level3 col name (subdistrict) in both 'data' and 'lookup_data'.
#' @param level4 level4 col name (settlement) in both 'data' and 'lookup_data'.
#' @param cache_path Optional; the path where the cache data frame is
#'        saved after user modifications. Supports all file formats supported
#'        by sntutils read() and write() functions including .rds, .csv, .xlsx,
#'        .dta, .tsv, and more. This path is also used to match and integrate
#'        previously established corrections into the current session. If NULL
#'        or the file does not exist at the provided path, users will be
#'        prompted to specify a new path or create a new cache data frame.
#' @param unmatched_export_path Optional; path to save unmatched data after
#'        processing. The file will include complete rows with full hierarchical
#'        context, showing which column needs matching (typically the most
#'        granular level like health facilities) along with all administrative
#'        levels and metadata (timestamp, username, data sources). Supports all
#'        file formats supported by sntutils write() function based on the file
#'        extension.
#' @param method The string distance calculation method(s) to be used. Users
#'        can specify one or more algorithms from the
#'        \code{\link[stringdist]{stringdist}} package to compute
#'        string distances between admin names. The function by
#'        default uses \code{"jw"} (Jaro-Winkler). Other options include:
#'        \code{"lv"} (Levenshtein), \code{"dl"}
#'        (Damerau-Levenshtein), \code{"lcs"} (Longest Common Subsequence),
#'        \code{"qgram"} (Q-Gram), \code{"jw"} (Jaro-Winkler), and
#'        \code{"soundex"}.
#' @param interactive Logical; if TRUE, prompts the user for interactive
#'        matching decisions. Defaults to FALSE.
#' @param max_options Maximum number of options to output for string distance
#'        matching. Default is 200.
#' @param preserve_case Logical; if TRUE, preserves the original case of admin
#'        names from the lookup data when returning matched values. If FALSE
#'        (default), returns all admin names in uppercase as before.
#' @param column_width Numeric; the maximum width (in characters) for each
#'        column in the interactive menu. Controls how much text is displayed
#'        before truncation. Default is 60 characters. Note that the actual
#'        text display width is approximately 8 characters less to accommodate
#'        number labels and truncation markers ("...").
#'
#' @details
#' The function performs the following steps:
#' 1. Prepares the data by ensuring administrative names are in uppercase for
#'    consistent matching.
#' 2. Attempts to load a previously saved cache file if available, or
#'    initializes the cleaning process.
#' 3. Matches administrative names between `target_df` and `lookup_df` using
#'    string distance algorithms, running in parallel. Results are ranked
#'    by closeness.
#' 4. Engages the user through an interactive CLI menu to make decisions on
#'    ambiguous matches.
#' 5. Saves the user's decisions in a cache data frame, either to a specified
#'    path or by prompting the user for a location.
#' 6. Returns a cleaned data frame with updated administrative names based on
#'    user choices and algorithmic matching.
#'
#' @return A data frame with cleaned administrative names and saved data frame
#'        of user decisions.
#'
#' @examples
#' \donttest{
#' # Dummy target data
#' target_df <- data.frame(
#'   country = c("ANGOLA", "UGA", "ZAMBIA"),
#'   province = c("CABONDA", "TESO", "LUSAKA"),
#'   district = c("BALIZE", "BOKEDEA", "RAFUNSA"),
#'   subdistrict = c("AREA1", "AREA2", "AREA3")
#' )
#'
#' # Dummy lookup data with correct spellings
#' lookup_df <- data.frame(
#'   country = c("ANGOLA", "ANGOLA", "UGANDA", "UGANDA", "ZAMBIA", "ZAMBIA"),
#'   province = c("CABINDA", "CABINDA", "TESO", "TESO", "LUSAKA", "LUSAKA"),
#'   district = c("BELIZE", "BUCO-ZAU", "BUKEDEA", "KUMI", "KAFUE", "LUSAKA"),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Interactively clean geonames
#' prep_geonames(
#'   target_df,
#'   lookup_df = lookup_df,
#'   level0 = "country", level1 = "province",
#'   level2 = "district",
#'   interactive = FALSE # replace with TRUE for interactivity
#' )
#' }
#'
#' @export
prep_geonames <- function(
  target_df,
  lookup_df = NULL,
  level0 = NULL,
  level1 = NULL,
  level2 = NULL,
  level3 = NULL,
  level4 = NULL,
  cache_path = NULL,
  unmatched_export_path = NULL,
  method = "jw",
  interactive = TRUE,
  max_options = 200,
  preserve_case = FALSE,
  column_width = 60
) {
  # Capture the names of the data frames at the beginning
  target_df_name <- paste(deparse(substitute(target_df)), collapse = "")
  lookup_df_name <- paste(deparse(substitute(lookup_df)), collapse = "")

  # Validation -----------------------------------------------------------------

  # Ensure higher levels cannot be used without corresponding lower levels
  if (!is.null(level1) && is.null(level0)) {
    cli::cli_abort("You cannot specify level1 without level0.")
  }
  if (!is.null(level2) && (is.null(level0) || is.null(level1))) {
    cli::cli_abort("You cannot specify level2 without both level0 and level1.")
  }
  if (
    !is.null(level3) && (is.null(level0) || is.null(level1) || is.null(level2))
  ) {
    cli::cli_abort(
      "You cannot specify level3 without level0, level1, and level2."
    )
  }

  # Prompt the user if using levels beyond level2 without a custom lookup_df
  if (is.null(lookup_df) && (!is.null(level3) || !is.null(level4))) {
    cli::cli_alert_warning(
      paste0(
        "The default lookup data only supports up to level2 (district). ",
        "Levels 3 and 4 will not have lookup matches unless a custom ",
        "lookup_df is provided."
      )
    )

    user_choice <- tolower(
      readline(
        paste0(
          "\nWould you like to:\n",
          "1. Continue with cleaning only up to level2",
          " (higher levels will be ignored)\n",
          "2. Exit and provide a custom lookup_df for level3 and level4\n",
          "Enter your choice (1 or 2): "
        )
      )
    )

    if (user_choice == "1") {
      cli::cli_alert_info(
        "Proceeding with cleaning up to level2 only. Ignoring level3 and level4."
      )
      level3 <- NULL
      level4 <- NULL
    } else {
      cli::cli_alert_info(
        paste0(
          "Exiting function. Please provide a valid ",
          "lookup_df to clean levels 3 and 4."
        )
      )
      return(invisible(NULL))
    }
  }

  # Ensure lookup_df contains necessary columns if provided
  if (!is.null(lookup_df)) {
    required_columns <- NULL
    if (!is.null(level0)) {
      required_columns <- c(required_columns, level0)
    }
    if (!is.null(level1)) {
      required_columns <- c(required_columns, level1)
    }
    if (!is.null(level2)) {
      required_columns <- c(required_columns, level2)
    }
    if (!is.null(level3)) {
      required_columns <- c(required_columns, level3)
    }
    if (!is.null(level4)) {
      required_columns <- c(required_columns, level4)
    }

    missing_columns <- setdiff(required_columns, colnames(lookup_df))
    if (length(missing_columns) > 0) {
      cli::cli_abort(
        paste(
          "The following columns are missing in lookup_df:",
          paste(missing_columns, collapse = ", ")
        )
      )
    }
  }

  # Ensure target_df contains necessary columns
  required_columns <- NULL
  if (!is.null(level0)) {
    required_columns <- c(required_columns, level0)
  }
  if (!is.null(level1)) {
    required_columns <- c(required_columns, level1)
  }
  if (!is.null(level2)) {
    required_columns <- c(required_columns, level2)
  }
  if (!is.null(level3)) {
    required_columns <- c(required_columns, level3)
  }
  if (!is.null(level4)) {
    required_columns <- c(required_columns, level4)
  }

  missing_columns <- setdiff(required_columns, colnames(target_df))
  if (length(missing_columns) > 0) {
    cli::cli_abort(
      paste(
        "The following columns are missing in target_df:",
        paste(missing_columns, collapse = ", ")
      )
    )
  }

  # Ensure method is supported
  supported_methods <- c(
    "jw",
    "osa",
    "lv",
    "dl",
    "hamming",
    "lcs",
    "qgram",
    "cosine",
    "jaccard",
    "soundex"
  )
  if (!(method %in% supported_methods)) {
    cli::cli_abort(
      paste(
        "Unsupported method:",
        method,
        ".
        Supported methods are:",
        paste(supported_methods, collapse = ", ")
      )
    )
  }

  # Ensure interactive is logical
  if (!is.logical(interactive)) {
    cli::cli_abort("interactive must be a logical value (TRUE or FALSE).")
  }

  # Ensure cache_path is a valid file path if provided
  if (!is.null(cache_path) && !dir.exists(dirname(cache_path))) {
    cli::cli_abort("The directory for cache_path does not exist.")
  }

  # Validation: Ensure lookup_df is not empty if provided
  if (!is.null(lookup_df) && nrow(lookup_df) == 0) {
    cli::cli_abort("The lookup_df is empty.")
  }

  # Ensure level0, level1, level2, and level3 are valid column names
  if (!is.null(level0) && !(level0 %in% colnames(target_df))) {
    cli::cli_abort(paste("The column", level0, "is not in target_df."))
  }
  if (!is.null(level1) && !(level1 %in% colnames(target_df))) {
    cli::cli_abort(paste("The column", level1, "is not in target_df."))
  }
  if (!is.null(level2) && !(level2 %in% colnames(target_df))) {
    cli::cli_abort(paste("The column", level2, "is not in target_df."))
  }
  if (!is.null(level3) && !(level3 %in% colnames(target_df))) {
    cli::cli_abort(paste("The column", level3, "is not in target_df."))
  }
  if (!is.null(level4) && !(level4 %in% colnames(target_df))) {
    cli::cli_abort(paste("The column", level4, "is not in target_df."))
  }

  # Step 0: Setup target and lookup datasets -----------------------------------

  # Get the internal shapefile if lookup data is not provided
  if (is.null(lookup_df)) {
    cli::cli_abort(
      c(
        "No lookup data provided.",
        "i" = "The internal WHO shapefile (shp_global) has been removed from this package.",
        "i" = "Please provide your own lookup data using the 'lookup_df' parameter.",
        "i" = "You can download WHO administrative boundaries from:",
        "i" = "https://hub.arcgis.com/datasets/WHO::polio-administrative-boundaries"
      )
    )
  }

  # Create the levels vector
  levels <- c(
    if (exists("level0")) level0 else NULL,
    if (exists("level1")) level1 else NULL,
    if (exists("level2")) level2 else NULL,
    if (exists("level3")) level3 else NULL,
    if (exists("level4")) level4 else NULL
  )

  # Store original case lookup values if preserve_case is TRUE
  lookup_case_mapping <- NULL
  if (preserve_case && !is.null(lookup_df)) {
    # Create a mapping from uppercase to original case for each level
    lookup_case_mapping <- list()

    for (level in levels) {
      if (level %in% names(lookup_df)) {
        # Get unique mappings from uppercase to original case
        original_values <- lookup_df[[level]]
        uppercase_values <- .clean_utf8(original_values) |> toupper()

        # Create mapping dataframe
        mapping_df <- data.frame(
          uppercase = uppercase_values,
          original = original_values,
          stringsAsFactors = FALSE
        ) |>
          dplyr::distinct()

        lookup_case_mapping[[level]] <- mapping_df
      }
    }
  }

  # Clean UTF-8 encoding and ensure uppercase for matching
  target_df <- target_df |>
    dplyr::mutate(
      dplyr::across(
        dplyr::any_of(levels),
        ~ .clean_utf8(.x) |> toupper()
      )
    )

  lookup_df <- lookup_df |>
    dplyr::mutate(
      dplyr::across(
        dplyr::any_of(levels),
        ~ .clean_utf8(.x) |> toupper()
      )
    )

  # Step 1: Configure cache if saved cache file exists available ---------------

  if (!is.null(cache_path) && !file.exists(cache_path)) {
    # Alert the user about the missing cache file, including the path
    cli::cli_alert_info(
      paste0("The specified cache file '", cache_path, "' does not exist.")
    )

    if (isTRUE(interactive)) {
      # Ask the user if they want to proceed and create a new cache file
      user_input <- readline(
        paste0(
          "Are you aware that the cache file is missing?",
          " Proceed to create a new one? (yes/no): "
        )
      )

      # Check the user's response
      if (!(tolower(user_input) %in% c("yes", "y"))) {
        cli::cli_alert_info("Exiting without creating a new cache file.")
        # Exit the function or stop execution as appropriate
      } else {
        cli::cli_alert_info("Proceeding to create a new cache file...")
      }
    } else {
      cli::cli_alert_info(
        "Non-interactive session detected; proceeding to create a new cache file."
      )
    }
  }

  # load saved cache file
  if (!is.null(cache_path) && file.exists(cache_path)) {
    # load the cache file using sntutils read() function
    saved_cache_df <- tryCatch(
      read(cache_path),
      error = function(e) {
        cli::cli_abort(
          "Failed to read cache file: {e$message}"
        )
      }
    )

    # harmonise column names in case using old version of cache file
    saved_cache_df <- saved_cache_df |>
      dplyr::rename(level0_prepped = any_of("country_prepped")) |>
      dplyr::rename(level1_prepped = any_of("province_prepped")) |>
      dplyr::rename(level2_prepped = any_of("district_prepped")) |>
      dplyr::rename(level3_prepped = any_of("subdistrict_prepped")) |>
      dplyr::rename(level4_prepped = any_of("settlements_prepped")) |>
      dplyr::mutate(
        level = dplyr::case_when(
          level == "country" ~ "level0",
          level == "province" ~ "level1",
          level == "district" ~ "level2",
          level == "subdistrict" ~ "level3",
          level == "settlements" ~ "level4",
          TRUE ~ level
        ),
        level3_prepped = if ("level3_prepped" %in% names(saved_cache_df)) {
          level3_prepped
        } else {
          NA_character_
        },
        level4_prepped = if ("level4_prepped" %in% names(saved_cache_df)) {
          level4_prepped
        } else {
          NA_character_
        },
      ) |>
      dplyr::select(dplyr::everything(), level3_prepped, level4_prepped)
  } else {
    # if the cache file does not exist, create an empty data frame
    saved_cache_df <- data.frame()
    target_todo <- target_df
  }

  # if the cache file exists, merge it with the target data and replace
  # incorrect names with correct ones.
  if (!is.null(saved_cache_df) && nrow(saved_cache_df) > 0) {
    # join with saved_cache_df based on cleaned names
    # a admin eval at time in case not all exist
    if (!is.null(level0)) {
      target_df <- target_df |>
        dplyr::left_join(
          saved_cache_df |>
            dplyr::filter(level == "level0") |>
            dplyr::distinct(name_to_match, level0_prepped),
          by = stats::setNames("name_to_match", level0)
        ) |>
        dplyr::mutate(
          !!rlang::sym(level0) := stringr::str_replace_all(!!rlang::sym(level0), "\n", ""),
          !!rlang::sym(level0) := dplyr::coalesce(level0_prepped, .data[[level0]])
        )
    }

    if (!is.null(level1)) {
      target_df <- target_df |>
        dplyr::left_join(
          saved_cache_df |>
            dplyr::filter(level == "level1") |>
            dplyr::distinct(name_to_match, level0_prepped, level1_prepped),
          by = stats::setNames(
            c("level0_prepped", "name_to_match"),
            c(level0, level1)
          )
        ) |>
        dplyr::mutate(
          !!rlang::sym(level1) := stringr::str_replace_all(!!rlang::sym(level1), "\n", ""),
          !!rlang::sym(level1) := dplyr::coalesce(level1_prepped, .data[[level1]])
        )
    }

    if (!is.null(level2)) {
      target_df <- target_df |>
        dplyr::left_join(
          saved_cache_df |>
            dplyr::filter(level == "level2") |>
            dplyr::distinct(
              name_to_match,
              level0_prepped,
              level1_prepped,
              level2_prepped
            ),
          by = stats::setNames(
            c("level0_prepped", "level1_prepped", "name_to_match"),
            c(level0, level1, level2)
          )
        ) |>
        dplyr::mutate(
          !!rlang::sym(level2) := stringr::str_replace_all(!!rlang::sym(level2), "\n", ""),
          !!rlang::sym(level2) := dplyr::coalesce(level2_prepped, .data[[level2]])
        )
    }

    if (!is.null(level3)) {
      target_df <- target_df |>
        dplyr::left_join(
          saved_cache_df |>
            dplyr::filter(level == "level3") |>
            dplyr::distinct(
              name_to_match,
              level0_prepped,
              level1_prepped,
              level2_prepped,
              level3_prepped
            ),
          by = stats::setNames(
            c(
              "level0_prepped",
              "level1_prepped",
              "level2_prepped",
              "name_to_match"
            ),
            c(level0, level1, level2, level3)
          )
        ) |>
        dplyr::mutate(
          !!rlang::sym(level3) := stringr::str_replace_all(!!rlang::sym(level3), "\n", ""),
          !!rlang::sym(level3) := dplyr::coalesce(level3_prepped, .data[[level3]])
        )
    }

    if (!is.null(level4)) {
      target_df <- target_df |>
        dplyr::left_join(
          saved_cache_df |>
            dplyr::filter(level == "level4") |>
            dplyr::distinct(
              name_to_match,
              level0_prepped,
              level1_prepped,
              level2_prepped,
              level3_prepped,
              level4_prepped
            ),
          by = stats::setNames(
            c(
              "level0_prepped",
              "level1_prepped",
              "level2_prepped",
              "level3_prepped",
              "name_to_match"
            ),
            c(level0, level1, level2, level3, level4) # Changed level3 to level4
          )
        ) |>
        dplyr::mutate(
          !!rlang::sym(level4) := stringr::str_replace_all(!!rlang::sym(level4), "\n", ""),
          !!rlang::sym(level4) := dplyr::coalesce(level4_prepped, .data[[level4]])
        )
    }

    # remove prepped columns
    target_df <- target_df |>
      dplyr::select(-matches("_prepped$"))
  }

  # Step 2: Filter out for those where there is a match ------------------------

  # get the original data (with cache applied if it was loaded)
  orig_df <- target_df

  # Dynamically filter for missing geolocations
  filter_na_expr <- purrr::map(levels, ~ rlang::expr(is.na(!!.x)))
  target_df_na <- target_df |> dplyr::filter(!!!filter_na_expr)

  # Dynamically filter for non-missing geolocations
  filter_not_na_expr <- purrr::map(levels, ~ rlang::expr(!is.na(!!.x)))
  target_df <- target_df |> dplyr::filter(!!!filter_not_na_expr)

  # dynamically construct the long geonames on target data
  target_df <- construct_geo_names(
    target_df,
    level0,
    level1,
    level2,
    level3,
    level4
  )
  lookup_df <- construct_geo_names(
    lookup_df,
    level0,
    level1,
    level2,
    level3,
    level4
  )

  # filter to matched rows
  target_done <- target_df |>
    dplyr::filter(
      (long_geo %in%
        unique(lookup_df[["long_geo"]]))
    )

  # reduce down to only unmatched rows
  target_todo <- target_df |>
    dplyr::filter(
      !(long_geo %in% unique(lookup_df[["long_geo"]]))
    )

  calculate_match_stats(
    target_df,
    lookup_df,
    level0,
    level1,
    level2,
    level3,
    level4
  )

  # Early return with finalised_df
  if (nrow(target_todo) == 0) {
    cli::cli_alert_success(
      "All records matched; process completed. Exiting..."
    )

    # Apply case mapping if preserve_case is TRUE
    if (preserve_case) {
      orig_df <- apply_case_mapping(orig_df, lookup_case_mapping, levels)
    }

    return(orig_df)
  }

  # return if non-interactive.
  if (!interactive) {
    cli::cli_alert_success(
      "In non-interactive mode. Exiting after matching with cache..."
    )

    # Export unmatched data before returning in non-interactive mode
    if (!is.null(unmatched_export_path) && nrow(target_todo) > 0) {
      # Tag the target_todo with source names
      target_todo_tagged <- target_todo |>
        dplyr::mutate(
          target_data = target_df_name,
          lookup_data = lookup_df_name
        )

      export_unmatched_data(
        target_todo_tagged,
        unmatched_export_path,
        level0,
        level1,
        level2,
        level3,
        level4
      )
    }

    # Apply case mapping if preserve_case is TRUE
    if (preserve_case) {
      orig_df <- apply_case_mapping(orig_df, lookup_case_mapping, levels)
    }

    return(orig_df)
  }

  cli::cli_alert_info(
    "Partial match completed. There are still matches to be made."
  )

  user_input <-
    readline("Would you like to do interactive matching? (yes/no):")

  if (!(tolower(user_input) %in% c("yes", "y"))) {
    cli::cli_alert_info(
      "Exiting without interactive matching..."
    )

    # Apply case mapping if preserve_case is TRUE
    if (preserve_case) {
      orig_df <- apply_case_mapping(orig_df, lookup_case_mapping, levels)
    }

    return(orig_df)
  }

  # Step 3: String distance matching in interactivity --------------------------

  # initialize empty lists to store results
  unmatched_df_group <- list()
  cleaned_dfs <- list()

  # Initialize flag variable
  skip_to_end <- FALSE

  for (level in levels) {
    top_res_list <- list()
    replacement_df <- NULL

    # Check if the current level has a hierarchical parent
    if (level %in% c(levels[2], levels[3], levels[4], levels[5])) {
      # Set up the grouping level (previous level in hierarchy)
      grouping_level <- levels[which(levels == level) - 1]

      for (group in unique(target_todo[[grouping_level]])) {
        if (!(group %in% unique(lookup_df[[grouping_level]]))) {
          skip_to_end <- TRUE
          break
        }

        lookup_df_group <- lookup_df |>
          dplyr::filter(.data[[grouping_level]] == group)

        unmatched_df_group <- target_todo |>
          dplyr::filter(.data[[grouping_level]] == group) |>
          dplyr::filter(!(.data[[level]] %in% unique(lookup_df_group[[level]])))

        if (nrow(unmatched_df_group) == 0) {
          next
        }

        # Dynamically create a long_geo from the previous levels
        if (level == levels[2]) {
          long_geo_group <- paste(
            group,
            sep = "_"
          )
        } else if (level == levels[3]) {
          long_geo_group <- paste(
            unmatched_df_group[[level0]][1],
            group,
            sep = "_"
          )
        } else if (level == levels[4]) {
          long_geo_group <- paste(
            unmatched_df_group[[level0]][1],
            unmatched_df_group[[level1]][1],
            group,
            sep = "_"
          )
        } else if (level == levels[5]) {
          long_geo_group <- paste(
            unmatched_df_group[[level0]][1],
            unmatched_df_group[[level1]][1],
            unmatched_df_group[[level2]][1],
            group,
            sep = "_"
          )
        } else {
          long_geo_group <- group
        }

        top_res <-
          calculate_string_distance(
            unmatched_df_group[[level]],
            lookup_df_group[[level]],
            method = method
          ) |>
          dplyr::select(-match_rank, -algorithm_name) |>
          dplyr::group_by(name_to_match, matched_names) |>
          dplyr::slice_min(distance, with_ties = FALSE) |>
          dplyr::distinct(name_to_match, .keep_all = TRUE) |>
          dplyr::arrange(name_to_match, distance) |>
          dplyr::select(name_to_match, matched_names) |>
          dplyr::ungroup() |>
          dplyr::mutate(long_geo = long_geo_group)

        top_res_list[[group]] <- top_res
      }

      if (skip_to_end) {
        break
      }

      top_res <- do.call(rbind, top_res_list)
    } else {
      unmatched_df_group <- target_todo |>
        dplyr::filter(!(.data[[level]] %in% unique(lookup_df[[level]])))

      if (nrow(unmatched_df_group) == 0) {
        next
      }

      top_res <-
        calculate_string_distance(
          unmatched_df_group[[level]],
          lookup_df[[level]],
          method = method
        ) |>
        dplyr::select(-match_rank, -algorithm_name) |>
        dplyr::group_by(name_to_match, matched_names) |>
        dplyr::slice_min(distance, with_ties = FALSE) |>
        dplyr::distinct(name_to_match, .keep_all = TRUE) |>
        dplyr::arrange(name_to_match, distance) |>
        dplyr::select(name_to_match, matched_names) |>
        dplyr::ungroup() |>
        dplyr::mutate(long_geo = name_to_match)
    }

    if (!is.null(top_res) && nrow(top_res) > 0) {
      cli::cli_alert_info("Handling user interaction for level: {level}")
      replacement_df <- handle_user_interaction(
        input_data = top_res,
        levels = levels,
        level = level,
        max_options = max_options,
        column_width = column_width
      )

      if (!is.null(replacement_df) && nrow(replacement_df) > 0) {
        cleaned_dfs[[level]] <- replacement_df
        cli::cli_alert_success("Replacements made for level: {level}")
      } else {
        cli::cli_alert_warning("No replacements made for level: {level}")
      }
    }

    # Always update target_todo, even if no replacements were made
    if (!is.null(replacement_df) && nrow(replacement_df) > 0) {
      target_todo <- target_todo |>
        dplyr::left_join(
          replacement_df |>
            dplyr::select(
              !!rlang::sym(level) := name_to_match,
              replacement
            ),
          by = level
        ) |>
        dplyr::mutate(
          !!rlang::sym(level) := ifelse(
            is.na(replacement),
            .data[[level]],
            replacement
          )
        ) |>
        dplyr::select(-replacement)
    }

    target_todo <- construct_geo_names(
      target_todo,
      level0,
      level1,
      level2,
      level3,
      level4
    )

    if (skip_to_end) {
      cli::cli_alert_danger("Skipping to end due to unmatched higher level")
      break
    }
  }

  # Step 4: clean up the cache file and save -----------------------------------

  if (length(cleaned_dfs) > 0 && any(sapply(cleaned_dfs, nrow) > 0)) {
    # clean up the cache df
    suppressWarnings(
      cleaned_cache_joined <- dplyr::bind_rows(cleaned_dfs) |>
        tidyr::separate(
          longname_corrected,
          into = c(
            "level0_prepped",
            "level1_prepped",
            "level2_prepped",
            "level3_prepped",
            "level4_prepped"
          ),
          sep = "_",
          extra = "drop"
        ) |>
        dplyr::mutate(
          level0_prepped = dplyr::if_else(
            level == "level0",
            replacement,
            level0_prepped
          ),
          level1_prepped = dplyr::if_else(
            level == "level1",
            replacement,
            level1_prepped
          ),
          level2_prepped = dplyr::if_else(
            level == "level2",
            replacement,
            level2_prepped
          ),
          level3_prepped = dplyr::if_else(
            level == "level3",
            replacement,
            level3_prepped
          ),
          level4_prepped = dplyr::if_else(
            level == "level4",
            replacement,
            level4_prepped
          ),
          dplyr::across(.cols = -created_time, ~ dplyr::na_if(.x, ""))
        ) |>
        # add username
        dplyr::mutate(name_of_creator = get_user_identity())
    )

    # combine cleaned data frames
    final_cache_dfs <-
      dplyr::bind_rows(saved_cache_df, cleaned_cache_joined) |>
      dplyr::mutate(
        longname_to_match = NA,
        longname_to_match = dplyr::case_when(
          is.na(longname_to_match) & level == "level0" ~ name_to_match,
          is.na(longname_to_match) & level == "level1" ~
            paste(level0_prepped, name_to_match, sep = "_"),
          is.na(longname_to_match) & level == "level2" ~
            paste(level0_prepped, level1_prepped, name_to_match, sep = "_"),
          is.na(longname_to_match) & level == "level3" ~
            paste(
              level0_prepped,
              level1_prepped,
              level2_prepped,
              name_to_match,
              sep = "_"
            ),
          is.na(longname_to_match) & level == "level4" ~
            paste(
              level0_prepped,
              level1_prepped,
              level2_prepped,
              level3_prepped,
              name_to_match,
              sep = "_"
            )
        )
      ) |>
      dplyr::select(
        level,
        name_to_match,
        replacement,
        # longname_corrected
        longname_to_match,
        level0_prepped,
        level1_prepped,
        level2_prepped,
        level3_prepped,
        level4_prepped,
        created_time,
        name_of_creator
      ) |>
      dplyr::arrange(created_time) |>
      dplyr::distinct(longname_to_match, .keep_all = TRUE) |>
      dplyr::select(-longname_to_match)

    # file saving
    handle_file_save(final_cache_dfs, default_save_path = cache_path)
  } else {
    cli::cli_alert_warning(
      "No cleanings were made. Cache will not be updated."
    )
  }

  # Step 5: Combine the cleaned data frames ------------------------------------

  finalised_df <- dplyr::bind_rows(target_done, target_todo, target_df_na) |>
    dplyr::select(-long_geo)

  # get stats
  calculate_match_stats(
    finalised_df,
    lookup_df,
    level0,
    level1,
    level2,
    level3,
    level4
  )

  # Step 6: Export unmatched data after all matching is complete ---------------
  # Export the final unmatched data (after interactive matching) if requested
  if (!is.null(unmatched_export_path) && nrow(target_todo) > 0) {
    # Tag the target_todo with source names
    target_todo_tagged <- target_todo <- target_todo |>
      # filter out the matched
      dplyr::filter(!(long_geo %in% unique(lookup_df$long_geo))) |>
      dplyr::mutate(
        target_data = target_df_name,
        lookup_data = lookup_df_name
      )

    export_unmatched_data(
      target_todo_tagged,
      unmatched_export_path,
      level0,
      level1,
      level2,
      level3,
      level4
    )
  }

  gc() # clean up memory

  # Apply case mapping if preserve_case is TRUE
  if (preserve_case) {
    finalised_df <- apply_case_mapping(
      finalised_df,
      lookup_case_mapping,
      levels
    )
  }

  # return the final data frame
  finalised_df
}

#' Impute higher administrative level using a lookup table
#'
#' This function imputes a higher-level administrative unit (e.g., adm1) based
#' on a lower-level administrative unit (e.g., adm2) using a lookup table.
#' It allows for dynamic column names and lookup table structures.
#'
#' @param target_df A dataframe containing the lower-level administrative unit
#'    column.
#' @param target_lower_col A string specifying the name of the lower-level admin
#'    column (e.g., adm2).
#' @param target_higher_col A string specifying the name of the higher-level
#'    admin column to be created.
#' @param lookup_df A dataframe containing the mapping of lower to higher-level
#'    administrative units.
#' @param lookup_lower_col A string specifying the name of the lower-level
#'    column in the lookup table.
#' @param lookup_higher_col A string specifying the name of the higher-level
#'    column in the lookup table.
#'
#' @return A dataframe with an additional higher-level column assigned based on
#'    the lookup.
#'
#' @examples
#' # Example lookup table
#' adm1_lookup <- dplyr::tribble(
#'   ~adm2, ~adm1,
#'   "Boffa", "Bok\u00e9",
#'   "Boke", "Bok\u00e9",
#'   "Fria", "Bok\u00e9"
#' )
#'
#' # Example dataset
#' data <- dplyr::tibble(
#'   district = c("Boffa", "Fria", "Unknown")
#' )
#'
#' # Impute higher-level administrative unit
#' cleaned_data <- impute_higher_admin(
#'   target_df = data,
#'   target_lower_col = "district",
#'   target_higher_col = "region",
#'   lookup_df = adm1_lookup,
#'   lookup_lower_col = "adm2",
#'   lookup_higher_col = "adm1"
#' )
#'
#' @export
impute_higher_admin <- function(target_df,
                                target_lower_col,
                                target_higher_col,
                                lookup_df,
                                lookup_lower_col,
                                lookup_higher_col) {
  target_df <- target_df |>
    dplyr::mutate(
      !!rlang::sym(target_higher_col) := dplyr::recode(
        .data[[target_lower_col]],
        !!!stats::setNames(
          lookup_df[[lookup_higher_col]],
          lookup_df[[lookup_lower_col]]
        )
      )
    )
  target_df
}
