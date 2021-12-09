#' Calculate the rejection
#'
#' Calculates the rejection (%) of the model p-values, according to the specified alpha, and the Monte Carlo standard error for this estimate.
#'
#' @param p P-values from the models.
#' @param alpha The nominal significance level specified. The default is `0.05`.
#' @param get A character vector containing the values returned by the function.
#' @param na.rm A logical value indicating whether NA values for `p` should be removed before rejection calculation.
#'
#' @return A named vector containing the estimate and the Monte Carlo standard error for the rejection.
#' @export
#'
#' @examples rejection(p=runif(200, min=0, max=1))
rejection <- function(p, alpha=0.05, get=c("rejection", "rejection_mcse"), na.rm=FALSE){
  assertthat::assert_that(length(!is.na(p)) > 0)

  x <- c()
  if(na.rm){
    p <- p[!is.na(p)]
  }

  rejections <- p <= alpha

  if(any(is.na(rejections))){
    x["rejection"] <- NA
    x["rejection_mcse"] <- NA
    return(x[get])
  }

  n <- length(p)
  x["rejection"] <- mean(rejections)
  x["rejection_mcse"] <- sqrt((x["rejection"] * (1-x["rejection"]))/n)
  return(x)
}
