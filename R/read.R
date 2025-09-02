#' Read in Data and Shapefiles from Various File Formats
#'
#' This function provides a unified interface for reading data from various
#' file formats supported by the \code{\link[rio]{import}},
#' \code{\link[sf]{read_sf}}, and \code{\link[readxl]{read_excel}} packages.
#' Additionally, it supports fast binary formats \code{.qs} and \code{.qs2}
#' via the optional \code{qs2} package. The format is automatically detected
#' from the file extension to simplify the importing process.
#'
#' @param file_path Character string specifying the path to the input file or
#'   a URL pointing to the dataset.
#' @param ... Additional arguments to be passed to the underlying read
#'   functions. These arguments are specific to the file format being imported.
#'   Please refer to the documentation of each package used for more
#'   information.
#'
#' @return A data frame or appropriate R object containing the imported data.
#'
#' @examples
#' # Locate test data directory
#' path <- system.file("extdata",
#'   package = "sntutils"
#' )
#'
#' # Import a CSV file
#' data_csv <- read(file_path = file.path(path, "test_data.csv"))
#'
#' # Import an Excel file
#' data_excel <- read(file_path = file.path(path, "test_data.xlsx"))
#'
#' # Import a Stata DTA file
#' data_dta <- read(file_path = file.path(path, "test_data.dta"))
#'
#' # Import an RDS file
#' data_rds <- read(file_path = file.path(path, "test_data.rds"))
#'
#' data_rdata <- read(file_path = file.path(path, "test_data.RData"))
#'
#' # Import an SPSS file
#' data_spss <- read(file_path = file.path(path, "test_data.sav"))
#'
#' # Import an shapefiles file (GeoJSON/json)
#' data_geojson <- read(file_path = file.path(path, "test_data.GeoJSON"))
#'
#' @seealso \code{\link[rio]{import}},
#'         \code{\link[sf]{read_sf}}, \code{\link[readxl]{read_excel}},
#'         and \code{qs2::qread} for reading \code{.qs}/\code{.qs2} files.
#'
#' @export
read <- function(file_path, ...) {
  # Check if the input file path is a URL and if so, read from URL directly
  if (grepl("^http[s]?://", file_path, ignore.case = TRUE)) {
    return(rio::import(file_path, ...))
  }

  # Extract the file extension from the input file path
  file_ext <- tools::file_ext(file_path) |> tolower()

  # List of supported formats
  supported_formats_rio <- c(
    "csv", "tsv", "txt", "csvy", "sas7bdat", "sav",
    "dta", "xpt", "rdata", "tsv"
  )

  # Excel formats
  excel_formats <- c("xls", "xlsx")

  # Handle different file formats
  if (file_ext %in% excel_formats) {
    readxl::read_excel(file_path, ...)
  } else if (file_ext %in% supported_formats_rio) { # for tabular data
    rio::import(file_path, ...)
  } else if (file_ext == "rds") {
    con <- archive::file_read(file_path)
    res <- readRDS(con)
    close(con)
    res
  } else if (file_ext %in% c("qs", "qs2")) {
    # Prefer qs2 if installed and exports qread; otherwise fall back to qs
    if (requireNamespace("qs2", quietly = TRUE)) {
      ns <- asNamespace("qs2")
      if (exists("qread", envir = ns, mode = "function")) {
        return(get("qread", envir = ns)(file_path, ...))
      }
    }
    if (requireNamespace("qs", quietly = TRUE)) {
      return(qs::qread(file_path, ...))
    }
    stop(
      paste0(
        "Reading '.", file_ext, "' requires the 'qs2' or 'qs' package. ",
        "Please install one of them: install.packages('qs2') or install.packages('qs')."
      )
    )
  } else if (file_ext %in% c("shp", "json", "geojson")) { # for shapefiles
    sf::read_sf(file_path, ...)
  } else {
    stop(
      paste0(
        "File format '", file_ext,
        "' not supported by 'rio', 'sf', or 'readxl'. ",
        "Please refer to the package documentation for a full list ",
        "of supported formats."
      )
    )
  }
}
