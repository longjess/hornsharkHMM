% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/exploring_lab_data.R
\name{pairs_plot}
\alias{pairs_plot}
\title{Create correlation plot}
\usage{
pairs_plot(data, filename)
}
\arguments{
\item{data}{A dataframe}

\item{filename}{Name of image file}
}
\value{
None
}
\description{
Saves correlation plot as an image file.
}
\examples{
filename <- "Custom_Lady_27Mar17_dynamic.csv"
data <- read.csv(filename)
dynamic_data <- data \%>\% select(X_dynamic, Y_dynamic, Z_dynamic)
pairs_plot(dynamic_data, "Lady_27Mar17_correlation.png")
}
