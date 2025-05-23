% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extrapolate_pop.R
\name{extrapolate_pop}
\alias{extrapolate_pop}
\title{Extrapolate Population Estimates for Future Years}
\usage{
extrapolate_pop(
  data,
  year_col,
  pop_col,
  group_cols,
  years_to_extrap,
  multiplier = NULL
)
}
\arguments{
\item{data}{A data frame containing population data with year, population,
and location columns.}

\item{year_col}{The name of the year column (unquoted or character).}

\item{pop_col}{The name of the population column (unquoted or character).}

\item{group_cols}{A character vector of grouping column names defining
location.}

\item{years_to_extrap}{A vector of target years to extrapolate. Can be
unnamed (e.g., c(2021, 2022)) or named with specific multipliers
(e.g., c(\code{2021} = 1.5, \code{2022} = 1.3)).}

\item{multiplier}{A single numeric multiplier to apply to all years when
\code{years_to_extrap} is unnamed (e.g., 1.5). Ignored if \code{years_to_extrap}
provides named multipliers.}
}
\value{
A data frame with updated population estimates for the specified
years.
}
\description{
Fills missing population values for specified future years by applying
a multiplier to the previous year's population estimate, grouped by location.
Supports applying the same multiplier to all years or custom multipliers per
year. Automatically expands data to a full year range.
}
\examples{

# Dummy data for 3 districts over 4 years
dummy_data <- expand.grid(
 adm0 = "COUNTRYX",
 adm1 = c("RegionA", "RegionB"),
 adm2 = c("District1", "District2"),
 year = 2018:2020
) |>
 dplyr::mutate(
   adm3 = paste0(adm2, "_Subarea"),
   total_pop = sample(1000:5000, size = dplyr::n(), replace = TRUE)
 ) |>
 dplyr::arrange(adm0, adm1, adm2, year)

# Example with the same multiplier for all years
extrapolate_pop(
  data = dummy_data,
  year_col = "year",
  pop_col = "total_pop",
  group_cols = c("adm0", "adm1", "adm2", "adm3"),
  years_to_extrap = c(2021, 2022),
  multiplier = 1.5
)

# Example with different multipliers for each year
extrapolate_pop(
  data = dummy_data,
  year_col = "year",
  pop_col = "total_pop",
  group_cols = c("adm0", "adm1", "adm2", "adm3"),
  years_to_extrap = c(`2021` = 1.5, `2022` = 1.3)
)

}
