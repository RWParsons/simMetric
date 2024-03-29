% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rejection.R
\name{rejection}
\alias{rejection}
\title{Calculate the rejection}
\usage{
rejection(
  p,
  alpha = 0.05,
  get = c("rejection", "rejection_mcse"),
  na.rm = FALSE,
  ...
)
}
\arguments{
\item{p}{P-values from the models.}

\item{alpha}{The nominal significance level specified. The default is \code{0.05}.}

\item{get}{A character vector containing the values returned by the function.}

\item{na.rm}{A logical value indicating whether NA values for \code{p} should be removed before rejection calculation.}

\item{...}{Additional arguments to be ignored.}
}
\value{
A named vector containing the estimate and the Monte Carlo standard error for the rejection.
}
\description{
Calculates the rejection (\%) of the model p-values, according to the specified alpha, and the Monte Carlo standard error for this estimate.
}
\examples{
rejection(p=runif(200, min=0, max=1))
}
