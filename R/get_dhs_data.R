#' Load DHS Parquet datasets using DuckDB
#'
#' @description
#' Creates a DuckDB connection and registers selected DHS parquet datasets as
#' views for efficient querying. Handles varying schemas using
#' `union_by_name = TRUE`. Returns a structured list with table references and
#' metadata.
#'
#' @param path Character string. Root directory containing parquet datasets
#'   organized by file type (GE, IR, PR, etc.). Defaults to
#'   `"01_data/parquet"` under the project root.
#' @param types Optional character vector of DHS file type codes to load
#'   (e.g. `c("IR", "PR")`). If `NULL`, all supported types are loaded.
#'
#' @return
#' A list of class `"dhs_duckdb"` with data tables, metadata, and connection.
#'
#' @examples
#' \dontrun{
#' # load all DHS datasets
#' dhs <- get_dhs_data()
#'
#' # load only PR and IR
#' dhs_small <- get_dhs_data(types = c("PR", "IR"))
#' }
#'
#' @export
get_dhs_data <- function(
  path = here::here("01_data", "parquet"),
  types = NULL
) {
  # validate input path
  if (!fs::dir_exists(path)) {
    cli::cli_abort("Path not found: {path}")
  }

  # create DuckDB connection
  con <- DBI::dbConnect(duckdb::duckdb())

  # supported DHS file types
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

  # restrict to requested types
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

  for (ft in names(file_types)) {
    type_dir <- fs::path(path, ft)
    files <- fs::dir_ls(
      type_dir,
      recurse = TRUE,
      glob = "*.parquet",
      fail = FALSE
    )

    if (length(files) > 0) {
      view_name <- tolower(ft)
      type_path <- fs::path(path, ft, "**/*.parquet")

      query <- glue::glue(
        "CREATE OR REPLACE VIEW {view_name} AS
         SELECT * FROM read_parquet(
           '{type_path}',
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
          cli::cli_alert_warning(
            "Could not load {ft}: {stringr::str_extract(e$message, '[^:]+')}"
          )
        }
      )
    }
  }

  # load metadata components
  md_root <- fs::path(path, "_metadata")
  cli::cli_inform(c(">" = "Loading metadata components..."))

  read_if_exists <- function(filename) {
    f <- fs::path(md_root, filename)
    if (fs::file_exists(f)) arrow::read_parquet(f) else NULL
  }

  dictionary <- read_if_exists("variable_dictionary.parquet")
  file_summary <- read_if_exists("file_summary.parquet")
  survey_summary <- read_if_exists("survey_summary.parquet")
  variable_frequency <- read_if_exists("variable_frequency.parquet")

  # output
  out <- c(
    datasets,
    list(
      con = con,
      dictionary = dictionary,
      file_summary = file_summary,
      survey_summary = survey_summary,
      variable_frequency = variable_frequency,
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
#' tables, row counts, and usage examples.
#'
#' @param x A `dhs_duckdb` object created by `get_dhs_data()`
#' @param ... Additional arguments (unused)
#'
#' @return
#' Invisibly returns the input object. Called for side effects (printing).
#'
#' @noRd
print.dhs_duckdb <- function(x, ...) {
  file_type_names <- c("ge", "ir", "pr", "hr", "kr", "br", "mr", "wi")
  loaded_types <- intersect(file_type_names, names(x))

  # get row counts - INSTANT with DuckDB!
  dataset_info <- character()
  for (ft in loaded_types) {
    if (!is.null(x[[ft]])) {
      tryCatch(
        {
          # duckdb count is instant from metadata
          count_query <- glue::glue(
            "SELECT COUNT(*) as n FROM {ft}"
          )
          n_rows <- DBI::dbGetQuery(x$con, count_query)$n

          # get column count
          schema_query <- glue::glue(
            "SELECT * FROM {ft} LIMIT 0"
          )
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

  # content
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
    cli::col_white(cli::style_bold("Example usage:")),
    cli::col_silver("# Using dplyr:"),
    cli::col_silver("x$ir |>"),
    cli::col_silver("  filter(country_code == 'KE') |>"),
    cli::col_silver("  collect()"),
    "",
    cli::col_silver("# Using SQL:"),
    cli::col_silver("DBI::dbGetQuery(x$con, \""),
    cli::col_silver("  SELECT * FROM ir"),
    cli::col_silver("  WHERE country_code = 'KE'"),
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
