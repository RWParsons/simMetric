#' Calculate the Mean Squared Error
#'
#' Calculates the Mean Squared Error of the model estimates from the true value and the Monte Carlo standard error for this estimate.
#'
#' @param true_value The true value which is being estimated.
#' @param estimates A numeric vector containing the estimates from the model(s).
#' @param get A character vector containing the values returned by the function.
#' @param na.rm A logical value indicating whether NA values for `estimates` should be removed before MSE calculation.
#' @param ... Additional arguments to be ignored.
#'
#' @return A named vector containing the estimate and the Monte Carlo standard error for the bias.
#' @export
#'
#' @examples mse(true_value=0, estimates=rnorm(100))
mse <- function(true_value, estimates, get=c("mse", "mse_mcse"), na.rm=FALSE, ...){
  assertthat::assert_that(length(!is.na(estimates)) > 0)

  x <- c()
  squared_errors <- (estimates - true_value)^2

  if(na.rm){
    squared_errors <- squared_errors[!is.na(squared_errors)]
  }

  if(any(is.na(squared_errors))){
    x["mse"] <- NA
    x["mse_mcse"] <- NA
    return(x[get])
  }

  n <- length(squared_errors)
  x['mse'] <- sum(squared_errors)/n
  x['mse_mcse'] <- sqrt(sum((squared_errors - x['mse'])^2)/(n*(n-1)))
  return(x[get])
}
