#' Try parsing dates with multiple formats
#'
#' This helper function attempts to parse dates using specified formats and
#' handles ISO 8601 formatted dates (containing 'T') separately using ymd_hms.
#'
#' @param x A character vector of dates to parse
#' @param formats A character vector of date formats to try (see ?strptime for
#'   format specifications)
#' @return A POSIXct vector of parsed dates
try_parsing <- function(x, formats) {
  parsed <- lubridate::parse_date_time(x,
    orders = formats, quiet = TRUE
  )

  # Handle special cases
  iso_indices <- grepl("T", x)
  parsed[iso_indices] <- lubridate::ymd_hms(x[iso_indices], quiet = TRUE)

  return(parsed)
}

#' Available date formats for `autoparse_dates` function
#'
#' A character vector containing all supported date formats that can be parsed
#' by `autoparse_dates()`. These formats follow standard date-time components:
#' \itemize{
#'   \item d: day
#'   \item m: month
#'   \item Y/y: year (Y=4 digits, y=2 digits)
#'   \item B: full month name
#'   \item b: abbreviated month name
#'   \item H: hour
#'   \item M: minute
#'   \item S: second
#' }
#' @format A character vector of date format strings
#' @examples
#' head(available_date_formats)
#' @export
available_date_formats <- c(
  "dmY", "mdY", "Ymd", "Ydm",
  "dmy", "mdy", "ymd", "ydm",
  "dmY HMS", "mdY HMS", "Ymd HMS", "Ydm HMS",
  "dmy HMS", "mdy HMS", "ymd HMS", "ydm HMS",
  "Y-m-d", "Y-m-d H:M:S", "Y-m-d H:M",
  "y-m-d", "y-m-d H:M:S", "y-m-d H:M",
  "d-m-Y H:M:S", "d.m.Y H:M:S", "d.m.Y H:M",
  "d-m-y H:M:S", "d.m.y H:M:S", "d.m.y H:M",
  "d B Y", "B d, Y", "Y B d",
  "d B y", "B d, y", "y B d",
  "d b Y", "b d, Y", "Y b d",
  "d b y", "b d, y", "y b d",
  "dmY", "mdY", "d-m-Y H:M", "d.m.Y", "d.m.Y H:M",
  "dmy", "mdy", "d-m-y H:M", "d.m.y", "d.m.y H:M",
  "d/m/Y", "d/m/Y H:M:S", "d/m/Y H:M", "d/m/Y HMS", "d/m/Y HM",
  "d/m/y", "d/m/y H:M:S", "d/m/y H:M", "d/m/y HMS", "d/m/y HM",
  "Y/m/d", "Y/m/d H:M:S", "Y/m/d H:M", "Y/m/d HMS", "Y/m/d HM",
  "y/m/d", "y/m/d H:M:S", "y/m/d H:M", "y/m/d HMS", "y/m/d HM"
)

#' Parse Dates in a Data Frame
#'
#' This function parses date columns in a data frame using various date
#' formats. It standardizes the output to a specified format and provides
#' verbose feedback on parsing results.
#'
#' @param data A data frame containing columns with date values to parse.
#' @param date_cols A character vector specifying the names of columns with
#'    date values.
#' @param output_format A character string specifying the desired output format
#'    for the parsed dates. Default is \code{"\%Y-\%m-\%d"}.
#' @param additional_format A character string specifying any additional formats
#'    that are not included in the default formats
#'    (see `available_date_formats`). Default is NULL.
#' @param verbose Logical. If \code{TRUE}, prints messages about parsing
#'   success or failure for each column. Default is \code{TRUE}.
#'
#' @return Returns the original data frame with the specified date columns
#'  parsed and formatted.
#'
#' @details
#' This function supports a wide range of date formats, including:
#' \itemize{
#'   \item Basic formats: \code{"dmY"}, \code{"mdY"}, \code{"Ymd"},
#'   \code{"Ydm"}, \code{"dmy"}, \code{"mdy"}, \code{"ymd"}, \code{"ydm"}
#'   \item Formats with time: \code{"dmY HMS"}, \code{"mdY HMS"},
#'   \code{"Ymd HMS"}, \code{"Ydm HMS"}, \code{"dmy HMS"}, \code{"mdy HMS"},
#'   \code{"ymd HMS"}, \code{"ydm HMS"}
#'   \item Additional formats: \code{"Y-m-d"}, \code{"Y-m-d H:M:S"},
#'   \code{"Y-m-d H:M"}, \code{"d-m-Y H:M:S"}, \code{"d.m.Y H:M:S"},
#'   \code{"d.m.Y H:M"}, \code{"d-m-Y H:M"}, \code{"d.m.Y"}
#'   \item ISO 8601 format: e.g., \code{"2021-03-20T00:01:00.513+01:00"}
#' }
#'
#' @examples
#' # Define a sample data frame
#' df <- data.frame(
#'   `dmY` = c("03-10-2023", "11-09-2022", "25-12-2021", "15-08-2020"),
#'   `mdY` = c("10-03-2023", "09-11-2022", "12-25-2021", "08-15-2020"),
#'   `Ymd` = c("2023-10-03", "2022-09-11", "2021-12-25", "2020-08-15"),
#'   `Ydm` = c("2023-03-10", "2022-11-09", "2021-25-12", "2020-15-08"),
#'   `dmy` = c("03-10-23", "11-09-22", "25-12-21", "15-08-20"),
#'   `mdy` = c("10-03-23", "09-11-22", "12-25-21", "08-15-20"),
#'   `ymd` = c("23-10-03", "22-09-11", "21-12-25", "20-08-15"),
#'   `ydm` = c("23-03-10", "22-11-09", "21-25-12", "20-15-08"),
#'   `dmY_HMS` = c(
#'     "03-10-2023 14:30:00", "11-09-2022 05:45:12",
#'     "25-12-2021 23:59:59", "15-08-2020 00:00:00"
#'   ),
#'   `mdY_HMS` = c(
#'     "10-03-2023 14:30:00", "09-11-2022 05:45:12",
#'     "12-25-2021 23:59:59", "08-15-2020 00:00:00"
#'   ),
#'   `Ymd_HMS` = c(
#'     "2023-10-03 14:30:00", "2022-09-11 05:45:12",
#'     "2021-12-25 23:59:59", "2020-08-15 00:00:00"
#'   ),
#'   `Ydm_HMS` = c(
#'     "2023-03-10 14:30:00", "2022-11-09 05:45:12",
#'     "2021-25-12 23:59:59", "2020-15-08 00:00:00"
#'   ),
#'   `dmy_HMS` = c(
#'     "03-10-23 14:30:00", "11-09-22 05:45:12",
#'     "25-12-21 23:59:59", "15-08-20 00:00:00"
#'   ),
#'   `mdy_HMS` = c(
#'     "10-03-23 14:30:00", "09-11-22 05:45:12",
#'     "12-25-21 23:59:59", "08-15-20 00:00:00"
#'   ),
#'   `ymd_HMS` = c(
#'     "23-10-03 14:30:00", "22-09-11 05:45:12",
#'     "21-12-25 23:59:59", "20-08-15 00:00:00"
#'   ),
#'   `ydm_HMS` = c(
#'     "23-03-10 14:30:00", "22-11-09 05:45:12",
#'     "21-25-12 23:59:59", "20-15-08 00:00:00"
#'   ),
#'   `Y-m-d` = c("2023-10-03", "2022-09-11", "2021-12-25", "2020-08-15"),
#'   `Y-m-d_HMS` = c(
#'     "2023-10-03 14:30:00", "2022-09-11 05:45:12",
#'     "2021-12-25 23:59:59", "2020-08-15 00:00:00"
#'   ),
#'   `Y-m-d_HM` = c(
#'     "2023-10-03 14:30", "2022-09-11 05:45",
#'     "2021-12-25 23:59", "2020-08-15 00:00"
#'   ),
#'   `d-m-Y_HMS` = c(
#'     "03-10-2023 14:30:00", "11-09-2022 05:45:12",
#'     "25-12-2021 23:59:59", "15-08-2020 00:00:00"
#'   ),
#'   `d.m.Y_HMS` = c(
#'     "03.10.2023 14:30:00", "11.09.2022 05:45:12",
#'     "25.12.2021 23:59:59", "15.08.2020 00:00:00"
#'   ),
#'   `d.m.Y_HM` = c(
#'     "03.10.2023 14:30", "11.09.2022 05:45",
#'     "25.12.2021 23:59", "15.08.2020 00:00"
#'   ),
#'   `d-m-Y_HM` = c(
#'     "03-10-2023 14:30", "11-09-2022 05:45",
#'     "25-12-2021 23:59", "15-08-2020 00:00"
#'   ),
#'   `d.m.Y` = c("03.10.2023", "11.09.2022", "25.12.2021", "15.08.2020"),
#'   `iso8601` = c(
#'     "2021-03-20T00:01:00.513+01:00",
#'     "2022-11-05T23:15:59.123+01:00",
#'     "2023-06-15T12:30:45.789Z",
#'     "2020-01-01T00:00:00.000-05:00"
#'   ),
#'   `mixed_formats` = c(
#'     "2023-10-03", "11.09.2022",
#'     "25-12-21 23:59", "2020-08-15T00:00:00Z"
#'   ),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Parse the date columns
#' parsed_df <- autoparse_dates(df,
#'   date_cols = colnames(df),
#'   output_format = "%Y-%m-%d"
#' )
#'
#' @export
autoparse_dates <- function(data, date_cols,
                            output_format = "%Y-%m-%d",
                            additional_format = NULL,
                            verbose = TRUE) {
  if (!is.character(date_cols)) date_cols <- as.character(date_cols)

  parse_results <- vector("list", length(date_cols))
  names(parse_results) <- date_cols

  # set up formats by also including additional formats
  formats <- c(available_date_formats, additional_format)

  data |>
    dplyr::mutate(
      dplyr::across(dplyr::all_of(date_cols), \(x) {
        parsed <- try_parsing(x, formats)
        parse_results[[dplyr::cur_column()]] <<- sum(is.na(parsed))

        if (output_format %in% "%Y-%m-%d") {
          as.Date(parsed)
        } else {
          as.Date(parsed) |> format(output_format)
        }
      })
    ) -> data

  if (verbose) {
    purrr::iwalk(
      parse_results, \(result, col) {
        if (result > 0) {
          cli::cli_alert_warning(
            "Warning: {result} dates could not be parsed in column '{col}'"
          )
        }
      }
    )

    if (all(
      purrr::map_lgl(parse_results, \(x) x == 0)
    )) {
      cli::cli_alert_success(
        "All columns have been successfully parsed to the given format"
      )
    }
  }

  return(data)
}
