#' Calculates the relative (%) error in model standard error
#'
#' Calculates the relative (%) error in model standard error and the (approximate) Monte Carlo standard error of this estimate.
#'
#' @param se A numeric vector containing the standard errors from the model(s).
#' @param estimates A numeric vector containing the estimates from the model(s).
#' @param get A character vector containing the values returned by the function.
#' @param na.rm A logical value indicating whether NA values for `se` and `estimates` should be removed before modSE and empSE calculation.
#' @param ... Additional arguments to be ignored.
#'
#' @return A named vector containing the estimate and the Monte Carlo standard error for the relative (%) error in model standard error.
#' @export
#'
#' @examples relativeErrorModSE(se=rnorm(n=1000, mean=10, sd=0.5), estimates=rnorm(n=1000))
relativeErrorModSE <- function(se, estimates, get=c("relativeErrorModSE", "relativeErrorModSE_mcse"), na.rm=FALSE, ...){
  assertthat::assert_that(length(!is.na(se)) == length(!is.na(estimates)))
  n <- length(!is.na(se))

  if(any(is.na(c(se, estimates))) & na.rm==FALSE){
    x["relativeErrorModSE"] <- NA
    x["relativeErrorModSE_mcse"] <- NA
    return(x[get])
  }

  x <- c()
  empSE_est <- empSE(estimates=estimates, get="empSE", na.rm=na.rm)
  modSE_est <- modSE(se=se, get="modSE", na.rm=na.rm)

  x["relativeErrorModSE"] <- 100*((modSE_est/empSE_est) - 1)
  numerator <- (1/(n-1)) * sum( (se^2 - modSE_est^2)^2 )
  x['relativeErrorModSE_mcse'] <- 100*(modSE_est/empSE_est)*sqrt(numerator/(4*n*(modSE_est^4)) + (1/(2*(n-1))) )


  return(x[get])
}
