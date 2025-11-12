#' Load DHS Parquet datasets using DuckDB
#'
#' @description
#' Creates a DuckDB connection and registers selected DHS parquet datasets as
#' views for efficient querying. Skips corrupted parquet files automatically.
#'
#' @param path Character string. Root directory containing parquet datasets.
#' @param types Optional character vector of DHS file type codes to load.
#'   If NULL, all supported types are loaded.
#'
#' @return A list of class "dhs_duckdb" containing data tables, metadata, and
#' connection.
#'
#' @export
get_dhs_data <- function(
  path = here::here("01_data", "parquet"),
  types = NULL
) {
  if (!fs::dir_exists(path)) {
    cli::cli_abort("Path not found: {path}")
  }

  con <- DBI::dbConnect(duckdb::duckdb())

  file_types <- c(
    "GE" = "Geographic Dataset",
    "IR" = "Women's Recode",
    "PR" = "Household Member Records",
    "HR" = "Household Records",
    "KR" = "Children's Recode",
    "BR" = "Birth Recode",
    "MR" = "Men's Recode",
    "WI" = "Wealth Index"
  )

  if (!is.null(types)) {
    types <- toupper(types)
    invalid <- setdiff(types, names(file_types))
    if (length(invalid) > 0) {
      cli::cli_abort(
        "Invalid DHS file type(s): {paste(invalid, collapse = ', ')}"
      )
    }
    file_types <- file_types[types]
  }

  cli::cli_inform(c(">" = "Registering DHS datasets in DuckDB..."))
  datasets <- list()
  bad_files <- character()

  for (ft in names(file_types)) {
    type_dir <- fs::path(path, ft)
    files <- fs::dir_ls(
      type_dir,
      recurse = TRUE,
      glob = "*.parquet",
      fail = FALSE
    )

    if (length(files) == 0) {
      next
    }

    # validate each parquet file
    valid_files <- purrr::keep(files, function(f) {
      tryCatch(
        {
          arrow::read_parquet(f, as_data_frame = FALSE)
          TRUE
        },
        error = function(e) {
          bad_files <<- c(bad_files, f)
          FALSE
        }
      )
    })

    if (length(valid_files) == 0) {
      cli::cli_alert_warning(
        "No valid {ft} parquet files found (all corrupted)."
      )
      next
    }

    view_name <- tolower(ft)
    valid_files_sql <- paste(valid_files, collapse = "','")

    query <- glue::glue(
      "CREATE OR REPLACE VIEW {view_name} AS
       SELECT * FROM read_parquet(
         ['{valid_files_sql}'],
         union_by_name = TRUE,
         filename = TRUE,
         hive_partitioning = TRUE
       )"
    )

    tryCatch(
      {
        DBI::dbExecute(con, query)
        datasets[[view_name]] <- dplyr::tbl(con, view_name)
        cli::cli_alert_success("Registered {file_types[[ft]]} ({ft})")
      },
      error = function(e) {
        cli::cli_alert_warning("Could not load {ft}: {e$message}")
      }
    )
  }

  if (length(bad_files) > 0) {
    cli::cli_alert_warning(
      paste0(length(bad_files), " corrupted parquet file(s) skipped.")
    )
    cli::cli_inform(bad_files)
  }

  # load metadata safely
  md_root <- fs::path(path, "_metadata")
  read_if_exists <- function(filename) {
    f <- fs::path(md_root, filename)
    if (fs::file_exists(f)) arrow::read_parquet(f) else NULL
  }

  out <- c(
    datasets,
    list(
      con = con,
      dictionary = read_if_exists("variable_dictionary.parquet"),
      file_summary = read_if_exists("file_summary.parquet"),
      survey_summary = read_if_exists("survey_summary.parquet"),
      variable_frequency = read_if_exists("variable_frequency.parquet"),
      path = path
    )
  )

  class(out) <- "dhs_duckdb"
  cli::cli_alert_success("DHS datasets loaded in DuckDB.")
  return(out)
}

#' Print method for dhs_duckdb objects
#'
#' @description
#' Displays a formatted summary of the DHS DuckDB database including loaded
#' tables, row counts, and available partition columns for filtering.
#'
#' @param x A `dhs_duckdb` object created by `get_dhs_data()`
#' @param ... Additional arguments (unused)
#'
#' @return Invisibly returns the input object. Called for side effects.
#' @export
print.dhs_duckdb <- function(x, ...) {
  file_type_names <- c("ge", "ir", "pr", "hr", "kr", "br", "mr", "wi")
  loaded_types <- intersect(file_type_names, names(x))
  dataset_info <- character()

  # expected partition columns
  partition_cols <- c("country_code", "survey_year", "survey_id")
  partitions_found <- list()

  for (ft in loaded_types) {
    if (!is.null(x[[ft]])) {
      tryCatch(
        {
          count_query <- glue::glue("SELECT COUNT(*) as n FROM {ft}")
          n_rows <- DBI::dbGetQuery(x$con, count_query)$n
          schema_query <- glue::glue("SELECT * FROM {ft} LIMIT 0")
          n_cols <- ncol(DBI::dbGetQuery(x$con, schema_query))

          dataset_info <- c(
            dataset_info,
            paste0(
              "  $",
              ft,
              ": ",
              format(n_rows, big.mark = ","),
              " rows x ",
              n_cols,
              " columns"
            )
          )

          # record which partition columns exist
          cols_in_table <- DBI::dbListFields(x$con, ft)
          found <- intersect(partition_cols, cols_in_table)
          if (length(found) > 0) {
            partitions_found[[ft]] <- paste(found, collapse = ", ")
          }
        },
        error = function(e) {
          dataset_info <- c(
            dataset_info,
            paste0("  $", ft, ": [error loading]")
          )
        }
      )
    }
  }

  # shorten path
  short_path <- x$path
  home_dir <- fs::path_home()
  short_path <- gsub(home_dir, "~", short_path)
  if (nchar(short_path) > 70) {
    parts <- unlist(strsplit(short_path, "/"))
    short_path <- paste0(
      parts[1],
      "/.../",
      paste(tail(parts, 3), collapse = "/")
    )
  }

  # partition summary (just names)
  partition_summary <- if (length(partitions_found) > 0) {
    purrr::imap_chr(partitions_found, function(v, ft) {
      paste0("  $", ft, " partitions → ", v)
    })
  } else {
    "  No partition columns detected."
  }

  # assemble display text
  label <- c(
    cli::col_cyan(cli::style_bold("DHS DuckDB Database")),
    "",
    cli::col_white(paste0(cli::style_bold("Path: "), short_path)),
    cli::col_white(
      paste0(
        cli::style_bold("Tables loaded: "),
        length(loaded_types),
        " (",
        paste(toupper(loaded_types), collapse = ", "),
        ")"
      )
    ),
    "",
    cli::col_white(cli::style_bold("Datasets:")),
    dataset_info,
    "",
    cli::col_white(cli::style_bold("Partitions available for filtering:")),
    partition_summary,
    "",
    cli::col_white(cli::style_bold("Example usage:")),
    cli::col_silver("# Using dplyr:"),
    cli::col_silver("x$pr |>"),
    cli::col_silver("  filter(country_code == 'KE', survey_year == 2016) |>"),
    cli::col_silver("  collect()"),
    "",
    cli::col_silver("# Using SQL:"),
    cli::col_silver("DBI::dbGetQuery(x$con, \""),
    cli::col_silver("  SELECT * FROM pr"),
    cli::col_silver("  WHERE country_code = 'KE' AND survey_year = 2016"),
    cli::col_silver("\")")
  )

  cat(
    cli::boxx(
      label,
      border_style = "round",
      border_col = "cyan",
      padding = 1,
      margin = 1,
      float = "left",
      align = "left"
    ),
    "\n"
  )
  cli::cli_alert_success("Database ready for use.")
  invisible(x)
}
