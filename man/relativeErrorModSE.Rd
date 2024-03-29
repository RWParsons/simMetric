% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/relativeErrorModSE.R
\name{relativeErrorModSE}
\alias{relativeErrorModSE}
\title{Calculates the relative (\%) error in model standard error}
\usage{
relativeErrorModSE(
  se,
  estimates,
  get = c("relativeErrorModSE", "relativeErrorModSE_mcse"),
  na.rm = FALSE,
  ...
)
}
\arguments{
\item{se}{A numeric vector containing the standard errors from the model(s).}

\item{estimates}{A numeric vector containing the estimates from the model(s).}

\item{get}{A character vector containing the values returned by the function.}

\item{na.rm}{A logical value indicating whether NA values for \code{se} and \code{estimates} should be removed before modSE and empSE calculation.}

\item{...}{Additional arguments to be ignored.}
}
\value{
A named vector containing the estimate and the Monte Carlo standard error for the relative (\%) error in model standard error.
}
\description{
Calculates the relative (\%) error in model standard error and the (approximate) Monte Carlo standard error of this estimate.
}
\examples{
relativeErrorModSE(se=rnorm(n=1000, mean=10, sd=0.5), estimates=rnorm(n=1000))
}
