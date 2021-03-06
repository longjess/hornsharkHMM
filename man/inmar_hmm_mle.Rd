% Generated by roxygen2: do not edit by hand
% Please edit documentation in
%   R/indep_multivariate_autoregressive_hmm_functions.R
\name{inmar_hmm_mle}
\alias{inmar_hmm_mle}
\title{Maximum likelihood estimation of multivariate normal parameters}
\usage{
inmar_hmm_mle(
  x,
  m,
  q,
  k,
  mu0,
  sigma0,
  gamma0,
  phi0,
  delta0 = NULL,
  stationary = TRUE,
  hessian = FALSE,
  steptol = 1e-06,
  iterlim = 100,
  stepmax = 100,
  state = NULL
)
}
\arguments{
\item{x}{Matrix of observations, rows represent each variable}

\item{m}{Number of states}

\item{mu0}{List of vectors of length m, initial values for means for
white noise}

\item{sigma0}{List of vectors of length m,
initial values for standard deviations}

\item{gamma0}{Initial values for ransition probabiilty matrix, size m x m}

\item{phi0}{List of matrices of size k x q, initial values for
autoregressive parameters.}

\item{delta0}{Optional, vector of length m containing initial values
initial distribution}

\item{stationary}{Boolean, whether the HMM is stationary or not}

\item{hessian}{Boolean, whether to return the inverse hessian}

\item{state}{List of state values, if provided. 0 represents an unknown state value.}
}
\value{
List of results
}
\description{
Maximum likelihood estimation of multivariate normal parameters
}
