#' Calculate the coverage
#'
#' Estimate the coverage and the Monte Carlo standard error of this estimate given a vector of confidence intervals and the true value.
#'
#' @param true_value The true value which should be covered by the interval.
#' @param ll A numeric vector containing the lower limits of the confidence intervals.
#' @param ul A numeric vector containing the upper limits of the confidence intervals.
#' @param get A character vector containing the values returned by the function.
#' @param na.rm A logical value indicating whether NA values for ll and ul should be removed before coverage estimation.
#' @param ... Additional arguments to be ignored.
#'
#' @return A named vector containing the estimate and the Monte Carlo standard error for the coverage.
#' @export
#'
#' @examples coverage(true_value=0, ll=c(-1, -1, -1, -1), ul=c(1, 1, 1, -0.5))
coverage <- function(true_value, ll, ul, get=c("coverage", "coverage_mcse"), na.rm=FALSE, ...){
  assertthat::assert_that(length(!is.na(ul) & !is.na(ll)) > 0)
  x <- c()

  if(na.rm){
    ul <- ul[!is.na(ul) & !is.na(ll)]
    ll <- ll[!is.na(ul) & !is.na(ll)]
  }
  assertthat::assert_that(length(ll) == length(ul))

  if(any(is.na(c(ul, ll)))){
    x["coverage"] <- NA
    x["coverage_mcse"] <- NA
    return(x[get])
  }

  assertthat::assert_that(all(ll < ul))
  covered <- true_value <= ul & true_value >= ll
  n <- length(covered)

  x["coverage"] <- mean(covered)
  x["coverage_mcse"] <- sqrt((x["coverage"]*(1-x["coverage"]))/n)
  return(x[get])
}
