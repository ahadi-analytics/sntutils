% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/outlier_detection.R
\name{detect_outliers}
\alias{detect_outliers}
\title{Detect outliers in a dataset using multiple statistical methods}
\usage{
detect_outliers(
  data,
  column,
  iqr_multiplier = 1.5,
  record_id = "record_id",
  adm1 = "adm1",
  adm2 = "adm2",
  yearmon = "yearmon",
  year = "year"
)
}
\arguments{
\item{data}{A data frame containing the data to analyze}

\item{column}{Character string specifying the column name to check for
outliers}

\item{iqr_multiplier}{Numeric value specifying the IQR multiplier for Tukey's
method (default = 1.5)}

\item{record_id}{Character string specifying the column name for record ID
(default = "record_id")}

\item{adm1}{Character string specifying the column name for administrative
level 1 (default = "adm1")}

\item{adm2}{Character string specifying the column name for administrative
level 2 (default = "adm2")}

\item{yearmon}{Character string specifying the column name for year-month
(default = "yearmon")}

\item{year}{Character string specifying the column name for year
(default = "year")}
}
\value{
A data frame with the record_id, specified column, and outlier
classification information containing:
\itemize{
\item record_id: Identifier for each record
\item column_name: The name of the analyzed column
\item value: The value in the specified column
\item outliers_moyenne: Identifies outliers using mean ± 3 standard deviations
\item outliers_halper: Identifies outliers using Hampel identifier
(median ± 15*MAD)
\item outliers_iqr: Identifies outliers using Tukey's fences
(Q1/Q3 ± custom*IQR)
\item moyenne_lower_bound: Lower bound for mean-based detection
\item moyenne_upper_bound: Upper bound for mean-based detection
\item hampel_lower_bound: Lower bound for Hampel identifier
\item hampel_upper_bound: Upper bound for Hampel identifier
\item iqr_lower_bound: Lower bound for IQR-based detection
\item iqr_upper_bound: Upper bound for IQR-based detection
\item iqr_value: The calculated IQR value
}
}
\description{
This function identifies outliers in a specified numeric column using three
different statistical approaches: parametric (mean ± 3SD), Hampel identifier
(median ± 15 * MAD), and Tukey's fences (quartiles ± custom IQR).
}
\details{
The function groups data by administrative units (adm1, adm2), health
facility (hf), and year before calculating statistics. Each method has
different sensitivity to outliers, with the Hampel identifier being more
robust against extreme values.

\strong{About \code{iqr_multiplier}:}
The \code{iqr_multiplier} controls the sensitivity of Tukey's fences (IQR method).
\itemize{
\item The standard value is \strong{1.5} (used in boxplots) for "mild" outliers.
\item A value of \strong{3} is sometimes used for "extreme" outliers.
\item Higher values make the method less sensitive (fewer outliers detected).
\item Lower values make it more sensitive (more outliers detected).
}

Returns NULL if the specified column doesn't exist or isn't numeric.
}
\examples{
\dontrun{
# Detect outliers in the "confirmed_cases" column with default IQR multiplier
outlier_results <- detect_outliers(malaria_data, "confirmed_cases")

# Use custom IQR multiplier
outlier_results <- detect_outliers(malaria_data, "confirmed_cases",
                                  iqr_multiplier = 2)
}
}
