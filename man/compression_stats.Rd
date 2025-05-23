% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compress_png.R
\name{compression_stats}
\alias{compression_stats}
\title{Calculate Compression Statistics}
\usage{
compression_stats(filename, init_size, final_size, verbosity = FALSE)
}
\arguments{
\item{filename}{String, The name of the file being compressed}

\item{init_size}{Numeric. Initial file size in bytes before compression.}

\item{final_size}{Numeric. Final file size in bytes after compression.}

\item{verbosity}{Logical. Controls output verbosity. FALSE = silent,
TRUE = verbose.}
}
\value{
A list containing compression statistics:
\item{initial_size}{Original file size in bytes}
\item{final_size}{Compressed file size in bytes}
\item{bytes_saved}{Number of bytes saved}
\item{percent_saved}{Percentage of size reduction}
}
\description{
This function calculates statistics for file compression operations,
including bytes saved and percentage reduction.
}
\examples{
# Basic usage
stats <- compression_stats("mylovely.plot.png", 100000, 75000)
str(stats)

# With verbosity to display results
if (interactive()) {
  compression_stats("mylovely.plot.png", 5242880, 3145728, verbosity = TRUE)
}
}
