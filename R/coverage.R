
#' Calculate the coverage
#'
#' Calculate the coverage given a vector of confidence intervals and the true value
#'
#' @param mu_true numeric
#' @param ll numeric vector
#' @param ul numeric vector
#' @param ...
#'
#' @return
#' @export
#'
#' @examples coverage(true_value=0, ll=rnorm(100, -1), ul=rnorm(100, 1))
coverage <- function(true_value, ll, ul, ...){
  assertthat::assert_that(length(ll) == length(ul))
  n <- length(ll)
  x <- c()
  if(sum(!is.na(ul)) == 0){
    x["coverage"] <- NA
    x["coverage_mcse"] <- NA
    return(x)
  }

  covered <- true_value <= ul & true_value >= ll
  x["coverage"] <- mean(covered)
  x["coverage_mcse"] <- sqrt((x["coverage"]*(1-x["coverage"]))/n)

  return(x)
}
