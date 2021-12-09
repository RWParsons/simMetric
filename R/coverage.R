#' Calculate the coverage
#'
#' Calculate the coverage given a vector of confidence intervals and the true value
#'
#' @param true_value The true value which should be covered by the interval
#' @param ll A numeric vector containing the lower limits of the confidence intervals
#' @param ul A numeric vector containing the upper limits of the confidence intervals
#' @param get a character vector containing the values returned by the function.
#' @param na.rm a logical value indicating whether NA values for ll and ul should be removed before coverage estimation.
#'
#' @return named vector containing the estimate and the monte carlo standard error for the coverage.
#' @export
#'
#' @examples coverage(true_value=0, ll=rnorm(100, -1), ul=rnorm(100, 1))
coverage <- function(true_value, ll, ul, get=c("coverage", "coverage_mcse"), na.rm=FALSE){
  assertthat::assert_that(length(ll) == length(ul))

  x <- c()
  covered <- true_value <= ul & true_value >= ll

  if(all(is.na(covered))){
    x["coverage"] <- NA
    x["coverage_mcse"] <- NA
    return(x[get])
  }

  if(na.rm){
    covered <- covered(!is.na(covered))
  }

  n <- length(covered)
  x["coverage"] <- mean(covered)
  x["coverage_mcse"] <- sqrt((x["coverage"]*(1-x["coverage"]))/n)
  return(x[get])
}
