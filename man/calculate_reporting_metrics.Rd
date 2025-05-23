% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reporting_rate.R
\name{calculate_reporting_metrics}
\alias{calculate_reporting_metrics}
\title{Calculate reporting/missing rate and proportion of reporting facilities}
\usage{
calculate_reporting_metrics(
  data,
  vars_of_interest,
  x_var,
  y_var = NULL,
  hf_col = NULL
)
}
\arguments{
\item{data}{A data frame containing health facility data}

\item{vars_of_interest}{Character vector of variable names to assess
reporting.}

\item{x_var}{Character. Name of first grouping variable (e.g. time period)}

\item{y_var}{Character. Name of second grouping variable (e.g. district);
can be NULL}

\item{hf_col}{Character. Name of column containing unique health facility
IDs; required only for facility-level}
}
\value{
A tibble with calculated rates depending on Scenario
}
\description{
This function calculates reporting metrics for health facility data:
\enumerate{
\item Scenario 1: reporting rate by (x_var, y_var) — how often values appear in
a group (all vars_of_interest)
\item Scenario 2: reporting rate of variables over time (x_var only)
\item Scenario 3: proportion of health facilities in y_var that reported any
value
}
}
\details{
Note: All Scenarios treat 0 values as NA to ensure consistent reporting
metrics
}
\examples{
# Sample data
hf_data <- data.frame(
  month = rep(c("Jan", "Feb", "Mar"), each = 10),
  district = rep(c("North", "South"), each = 5, times = 3),
  facility_id = rep(1:5, times = 6),
  malaria = c(
    10, 0, 15, NA, 8, 12, 0, NA, 7, 9,
    11, 0, 14, 6, NA, 13, 8, 10, 0, 12,
    9, 7, 0, 11, 14, 8, NA, 12, 10, 15
  ),
  pneumonia = c(
    5, 0, NA, 7, 3, 6, 0, 4, NA, 2,
    8, 0, 6, NA, 4, 7, 3, 0, 5, 6,
    4, 0, 7, 5, NA, 6, 0, 8, 4, 3
  )
)

# Scenario 1: Reporting rate by district and month for each variable
calculate_reporting_metrics(
  data = hf_data,
  vars_of_interest = c("malaria", "pneumonia"),
  x_var = "month",
  y_var = "district"
)
# Result shows reporting rate for each variable by district and month

# Scenario 2: Reporting rate of variables over time (months only)
calculate_reporting_metrics(
  data = hf_data,
  vars_of_interest = c("malaria", "pneumonia"),
  x_var = "month"
)
# Result shows reporting/missing rates for each variable by month

# Scenario 3: Proportion of facilities in each district reporting any data
# by month
calculate_reporting_metrics(
  data = hf_data,
  vars_of_interest = c("malaria", "pneumonia"),
  x_var = "month",
  y_var = "district",
  hf_col = "facility_id"
)
# Result shows proportion of facilities that reported
# any data by district and month
}
