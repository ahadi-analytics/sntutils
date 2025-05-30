% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reporting_rate.R
\name{translate_plot_labels}
\alias{translate_plot_labels}
\title{Translate plot labels to specified language}
\usage{
translate_plot_labels(
  plot,
  target_language,
  source_language = "en",
  lang_cache_path = tempdir()
)
}
\arguments{
\item{plot}{A ggplot2 object}

\item{target_language}{Target language code}

\item{source_language}{Source language code (default: NULL)}

\item{lang_cache_path}{Path for translation cache (default: tempdir())}
}
\value{
The plot with translated labels
}
\description{
Translate plot labels to specified language
}
