% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/exploring_lab_data_log_odba.R
\name{hist_plot_log_odba}
\alias{hist_plot_log_odba}
\title{Create histogram from log ODBA data}
\usage{
hist_plot_log_odba(data, filename)
}
\arguments{
\item{data}{A dataframe with column ODBA}

\item{filename}{String containing the first part of filename for the
image files to be created}
}
\value{
None
}
\description{
Creates image file containing a histogram for log ODBA.
}
\examples{
filename <- "Custom_Lady_27Mar17_dynamic.csv"
data <- read.csv(filename)
hist_plot_log_odba(data, "Lady_27Mar17_histogram")
}
