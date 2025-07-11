% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/validate_process_spatial.R
\name{validate_process_spatial}
\alias{validate_process_spatial}
\title{Spatial Vector Validation and Cleaning}
\usage{
validate_process_spatial(
  shp,
  name = "shapefile",
  adm0_col = NULL,
  adm1_col = NULL,
  adm2_col = NULL,
  adm3_col = NULL,
  fix_issues = TRUE,
  quiet = FALSE
)
}
\arguments{
\item{shp}{An sf object to validate}

\item{name}{Character string identifying the dataset (for reporting)}

\item{adm0_col}{Character string, column name for country level}

\item{adm1_col}{Character string, column name for region/province level}

\item{adm2_col}{Character string, column name for district level}

\item{adm3_col}{Character string, column name for chiefdom/commune level}

\item{fix_issues}{Logical, whether to attempt automatic fixes}

\item{quiet}{Logical, whether to suppress progress messages}
}
\value{
List with validation results and cleaned admin-level spatial vectors
}
\description{
Validates spatial vector data and creates admin-level aggregations with
geometry hashes
}
\examples{
\dontrun{
# Basic validation with Sierra Leone data
result <- validate_process_spatial(
  shp = shp_raw,
  name = "Sierra Leone boundaries",
  adm0_col = "country",
  adm1_col = "region",
  adm2_col = "district",
  adm3_col = "chiefdom"
)

# Access cleaned admin levels
shp_adm0 <- result$spat_vec$adm0
shp_adm1 <- result$spat_vec$adm1
shp_adm2 <- result$spat_vec$adm2
shp_adm3 <- result$spat_vec$adm3

# Check validation status
if (result$passed) {
  cli::cli_alert_success("Ready for SNT analysis!")
}

# Access invalid rows (if any)
invalid_rows <- result$invalid_rows

# Access duplicate rows (if any)
duplicate_rows <- result$duplicate_rows
# duplicate_rows$row_duplicates - exact row duplicates
# duplicate_rows$geometry_duplicates - geometry-only duplicates

# Check column structure as data frame
col_dict <- result$column_dictionary
# Shows data frame with old_name, new_name, description, shapefiles
print(col_dict)
}

}
