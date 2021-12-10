#' Calculate the bias-eliminated coverage
#'
#' Estimate the bias-eliminated coverage and the Monte Carlo standard error of this estimate given a vector of confidence intervals and the true value.
#'
#' @param ll A numeric vector containing the lower limits of the confidence intervals.
#' @param ul A numeric vector containing the upper limits of the confidence intervals.
#' @param estimates A numeric vector containing the estimates from the model(s).
#' @param get A character vector containing the values returned by the function.
#' @param na.rm A logical value indicating whether NA values for ll and ul should be removed before coverage estimation.
#' @param ... Additional arguments to be ignored.
#'
#' @return A named vector containing the estimate and the Monte Carlo standard error for the coverage.
#' @export
#'
#' @examples biasEliminatedCoverage(estimates=rnorm(4), ll=c(-1, -1, -1, -1), ul=c(1, 1, 1, -0.5))
biasEliminatedCoverage <- function(estimates, ll, ul, get=c("biasEliminatedCoverage", "biasEliminatedCoverage_mcse"), na.rm=FALSE, ...){
  assertthat::assert_that(length(!is.na(ul) > 0 & !is.na(ll)) > 0)
  x <- c()

  if(na.rm){
    ul <- ul[!is.na(ul) & !is.na(ll)]
    ll <- ll[!is.na(ul) & !is.na(ll)]
    estimates <- estimates[!is.na(estimates)]
  }
  assertthat::assert_that(length(ll) == length(ul) & length(ul) == length(estimates))

  if(any(is.na(c(ul, ll)))){
    x["biasEliminatedCoverage"] <- NA
    x["biasEliminatedCoverage_mcse"] <- NA
    return(x[get])
  }

  assertthat::assert_that(all(ll < ul))
  mean_estimate <- mean(estimates)
  covered <- mean_estimate <= ul & mean_estimate >= ll
  n <- length(covered)

  x["biasEliminatedCoverage"] <- mean(covered)
  x["biasEliminatedCoverage_mcse"] <- sqrt((x["biasEliminatedCoverage"]*(1-x["biasEliminatedCoverage"]))/n)
  return(x[get])
}
