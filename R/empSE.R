#' Calculate the empirical standard error
#'
#' Calculates the empirical standard error of the model estimates and its Monte Carlo standard error.
#'
#' @param estimates A numeric vector containing the estimates from the model(s).
#' @param get A character vector containing the values returned by the function.
#' @param na.rm A logical value indicating whether NA values for `estimates` should be removed before empSE calculation.
#' @param ... Additional arguments to be ignored.
#'
#' @return A named vector containing the estimate and the Monte Carlo standard error for the empirical standard error.
#' @export
#'
#' @examples empSE(estimates=rnorm(100))
empSE <- function(estimates, get=c("empSE", "empSE_mcse"), na.rm=FALSE, ...){
  assertthat::assert_that(length(!is.na(estimates)) > 0)

  x <- c()
  if(na.rm){
    estimates <- estimates[!is.na(estimates)]
  }

  deviations <- estimates - mean(estimates)

  if(any(is.na(deviations))){
    x["empSE"] <- NA
    x["empSE_mcse"] <- NA
    return(x[get])
  }

  n <- length(deviations)
  x["empSE"] <- sqrt(sum(deviations^2) / (n-1))
  x["empSE_mcse"] <- x["empSE"]/sqrt(2*(n-1))
  return(x[get])
}
