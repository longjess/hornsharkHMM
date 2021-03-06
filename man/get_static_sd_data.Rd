% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/exploring_lab_data_sd.R
\name{get_static_sd_data}
\alias{get_static_sd_data}
\title{Get file of standard deviations of static data}
\usage{
get_static_sd_data(old_filename, filename)
}
\arguments{
\item{old_filename}{Name of csv file containing static data. Must have
columns Time, X_static, Y_static, Z_static, Behavior, Prey.}

\item{filename}{Name of new csv file containing standard deviations}
}
\value{
None
}
\description{
Standard deviations are taken every second (25 data points).
}
\examples{
old_filename <- "Custom_BigDaddy_3Apr17_static.csv"
filename <- "Mean_BigDaddy_3Apr17_static.csv"
get_static_sd_data(old_filename, filename)
}
