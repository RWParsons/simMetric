#' Calculates the relative (%) increase in precision between two methods
#'
#' Calculates the relative (%) increase in precision between two competing methods (B vs A). As this metric compares two methods directly, it cannot be used in `join_metrics()`.
#'
#' @param estimates_A A numeric vector containing the estimates from model A.
#' @param estimates_B A numeric vector containing the estimates from model B.
#' @param get A character vector containing the values returned by the function.
#' @param na.rm A logical value indicating whether NA values for `estimates` should be removed before empSE calculation.
#'
#' @return A named vector containing the estimate and the Monte Carlo standard error for the relative (%) increase in precision of method B versus method A.
#' @export
#'
#' @examples relativePrecision(estimates_A = rnorm(n = 1000), estimates_B = rnorm(n = 1000))
relativePrecision <- function(estimates_A, estimates_B, get = c("relativePrecision", "relativePrecision_mcse"), na.rm = FALSE) {
  assertthat::assert_that(length(!is.na(estimates_A)) == length(!is.na(estimates_B)))
  n <- length(!is.na(estimates_A))
  x <- c()

  if (any(is.na(c(estimates_A, estimates_B))) & na.rm == FALSE) {
    x["relativePrecision"] <- NA
    x["relativePrecision_mcse"] <- NA
    return(x[get])
  }

  if (na.rm) {
    estimates_A <- estimates_A[!is.na(estimates_A)]
    estimates_B <- estimates_B[!is.na(estimates_B)]
  }

  empSE_A <- empSE(estimates = estimates_A, get = "empSE")
  empSE_B <- empSE(estimates = estimates_B, get = "empSE")

  x["relativePrecision"] <- 100 * ((empSE_A / empSE_B)^2 - 1)
  x["relativePrecision_mcse"] <- 200 * ((empSE_A / empSE_B)^2) * sqrt((1 - stats::cor(x = estimates_A, y = estimates_B)^2) / (n - 1))

  return(x[get])
}
