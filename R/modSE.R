#' Calculates the average model standard error
#'
#' Calculates the average model standard error and the Monte Carlo standard error of this estimate.
#'
#' @param se A numeric vector containing the standard errors from the model(s).
#' @param get A character vector containing the values returned by the function.
#' @param na.rm A logical value indicating whether NA values for `se` should be removed before modSE calculation.
#' @param ... Additional arguments to be ignored.
#'
#' @return A named vector containing the estimate and the Monte Carlo standard error for the average model standard error.
#' @export
#'
#' @examples modSE(se=runif(n=20, min=1, max=1.5))
modSE <- function(se, get=c("modSE", "modSE_mcse"), na.rm=FALSE, ...){
  assertthat::assert_that(length(!is.na(se)) > 0)
  assertthat::assert_that(all(se[!is.na(se)] > 0))

  x <- c()
  if(na.rm){
    se <- se[!is.na(se)]
  }

  if(any(is.na(se))){
    x["modSE"] <- NA
    x["modSE_mcse"] <- NA
    return(x[get])
  }

  n <- length(se)
  x["modSE"] <- sqrt(sum(se^2)/n)
  numerator <- (1/(n-1)) * sum( (se^2 - x['modSE']^2)^2 )
  x['modSE_mcse'] <- sqrt(numerator/(4*n * (x['modSE']^2)))
  return(x[get])
}
