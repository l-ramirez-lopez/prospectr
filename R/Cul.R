#' @title Cochran C critical value
#'
#' @description
#' Upper limit critical value Cul for one-sided test on balanced design
#' @usage
#' Cul(a,n,N)
#' @param a significance level.
#' @param n number of points per series.
#' @param N number of data series.
#' @keywords internal
#' @author Antoine Stevens
#' @references <https://en.wikipedia.org/wiki/Cochran's_C_test>
#'
Cul <- function(a, n, N) {
  Fc <- qf(a / N, n - 1, (n - 1) * (N - 1), lower.tail = FALSE)
  value <- 1 / (1 + ((N - 1) / Fc))
  return(value)
}
