#' Save Data and Shapefiles to Various File Formats
#'
#' This function provides a unified interface for saving data to various
#' file formats supported by the \code{rio::export}
#' function. The format is automatically detected from the file extension to
#' simplify the saving process.
#'
#' @param data The dataset to be saved
#' @param file_path Character string specifying the path to the output file.
#' @param ... Additional arguments to be passed to the underlying write
#'   functions. These arguments are specific to the file format being saved.
#'   Please refer to the documentation of each package used for more
#'   information.
#'
#' @return No return value, called for side effects.
#'
#' @examples
#' # Create temporary account
#' tmpdir <- tempfile()
#' dir.create(tmpdir)
#'
#' # Save a CSV file
#' write(mtcars, file_path = file.path(tmpdir, "file.csv"))
#'
#' # Save an Excel file
#' write(mtcars, file_path = file.path(tmpdir, "file.xlsx"))
#'
#' # Save a Stata DTA file
#' write(mtcars, file_path = file.path(tmpdir, "file.dta"))
#'
#' # Save an RDS file
#' #write(mtcars, file_path = file.path(tmpdir, "file.rds"))
#'
#' # Save an RData file
#' write(list(mtcars = mtcars, iris = iris),
#'        file_path = file.path(tmpdir, "file.RData"))
#'
#' # For saving shapefiles
#' # make example shape data
#' my_shp <-  sf::st_sfc(
#'   sf::st_point(c(43, 23))) |>
#'     cbind(mtcars[1, ]) |>
#'     sf::st_as_sf(crs = sf::st_crs(4326))
#'
#' # save an RDS file
#' # write(my_shp, file_path = file.path(tmpdir, "file.shp"))
#'
#' # Remove the temporary directory and its contents
#' unlink(tmpdir, recursive = TRUE)

#' @importFrom rio import
#' @importFrom rio install_formats
#' @importFrom tools file_ext
#'
#' @export
write <- function(data, file_path, ...) {

  # Extract the file extension from the input file path
  file_ext <- tools::file_ext(file_path) |> tolower()

  # List of supported formats
  supported_formats_rio <- c(
    "csv", "tsv", "xlsx", "rdata", "dta"
  )

  if (file_ext %in% supported_formats_rio) {
    rio::export(data, file_path, ...)
  } else if (file_ext %in% "rds") {

    con = archive::file_write(file = file_path, filter = "xz")
    open(con)
    saveRDS(data, con)
    close(con)

  } else if (file_ext %in% "shp") { # shp shapefiles
    sf::write_sf(data, file_path, ...)
  } else if (file_ext %in% c("json", "geojson")) { # json shapefiles
    sf::write_sf(data, file_path, driver = "GeoJSON", ...)
  } else {
    stop(
      paste(
        "File format '", file_ext, "' not supported by 'rio'.",
        "Please refer to the package documentation for a full list",
        "of supported formats."
      )
    )
  }
}
