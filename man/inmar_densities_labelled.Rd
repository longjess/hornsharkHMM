% Generated by roxygen2: do not edit by hand
% Please edit documentation in
%   R/indep_multivariate_autoregressive_hmm_functions.R
\name{inmar_densities_labelled}
\alias{inmar_densities_labelled}
\title{Get matrix of state dependent probability densities, for some states known}
\usage{
inmar_densities_labelled(x, mod, m, q, k, n, state)
}
\arguments{
\item{x}{Matrix of observations, rows represent each variable}

\item{mod}{List of parameters}

\item{m}{Number of states}

\item{n}{Number of observations}

\item{state}{List of state values, if provided. 0 represents an unknown state value.}
}
\value{
n x m matrix of state dependent probability densities
}
\description{
Get matrix of state dependent probability densities, for some states known
}
