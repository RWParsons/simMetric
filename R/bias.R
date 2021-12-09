#' Calculate the bias
#'
#' Calculates the bias of the model estimates from the true value and the Monte Carlo standard error for this estimate.
#'
#' @param true_value The true value which is being estimated.
#' @param estimates A numeric vector containing the estimates from the model(s).
#' @param get A character vector containing the values returned by the function.
#' @param na.rm A logical value indicating whether NA values for `estimates` should be removed before bias calculation.
#' @param ... Additional arguments to be ignored.
#'
#' @return A named vector containing the estimate and the Monte Carlo standard error for the bias.
#' @export
#'
#' @examples bias(true_value=0, estimates=rnorm(100))
bias <- function(true_value, estimates, get=c("bias", "bias_mcse"), na.rm=FALSE, ...){
  assertthat::assert_that(length(!is.na(estimates - true_value)) > 0)

  x <- c()
  biases <- estimates - true_value

  if(na.rm){
    biases <- biases[!is.na(biases)]
  }

  if(any(is.na(biases))){
    x["bias"] <- NA
    x["bias_mcse"] <- NA
    return(x[get])
  }

  n <- length(biases)
  x["bias"] <- mean(biases)
  x["bias_mcse"] <- sum(biases^2) * (1/(n*(n-1)))
  return(x[get])
}
