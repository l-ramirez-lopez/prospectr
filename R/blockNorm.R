#' @title Sum of squares block weighting
#' @description
#' Sum of squares block weighting: allows to scale blocks of variables,
#' but keeping the relative weights of the variables inside a block.
#' @usage
#' blockNorm(X, targetnorm = 1)
#' @param X a numeric matrix to transform (optionally a data frame that can
#' be coerced to a numerical matrix).
#' @param targetnorm desired sum of squares for a block of variables
#' (default = 1)
#' @return a list with components `Xscaled`, the scaled matrix and `f`, the
#' scaling factor
#' @author Antoine Stevens
#' @examples
#' X <- matrix(rnorm(100), ncol = 10)
#' # Block normalize to sum of square equals to 1
#' res <- blockNorm(X, targetnorm = 1)
#' sum(res$Xscaled^2) # check
#' @seealso
#' \code{\link{blockScale}}, \code{\link{standardNormalVariate}},
#' \code{\link{detrend}}
#' @references
#' Eriksson, L., Johansson, E., Kettaneh, N., Trygg, J.,
#' Wikstrom, C., and Wold, S., 2006. Multi- and Megavariate Data Analysis.
#' MKS Umetrics AB.
#' @details
#' The function computes a scaling factor, which, multiplied by the
#' input matrix,
#' produces a matrix with a pre--determined sum of squares.
#' @note
#' This is a \R port of the \file{MBnorm.m} function of the MB matlab toolbox
#' by Fran van den Berg. 
#' @export
blockNorm <- function(X, targetnorm = 1) {
  if (!any(class(X) %in% c("matrix", "data.frame"))) {
    stop("X must be either a matrix or optionally a data.frame")
  }

  if (is.data.frame(X)) {
    X <- as.matrix(X)
  }

  if (targetnorm == 1) {
    f <- sum(X^2, na.rm = TRUE)^0.5
  } else {
    fmax <- sum(X^2, na.rm = TRUE)
    fmaxn <- sum((X / fmax)^2, na.rm = TRUE)
    if (fmaxn > targetnorm) {
      fmin <- fmax
      fminn <- fmaxn
      while (fminn > targetnorm) {
        fmin <- fmin * 10
        fminn <- sum((X / fmin)^2, na.rm = TRUE)
      }
    } else {
      fmin <- fmax
      fminn <- fmaxn
      while (fmaxn < targetnorm) {
        fmax <- fmax / 10
        fmaxn <- sum((X / fmax)^2, na.rm = TRUE)
      }
    }
    n <- fmaxn
    while ((targetnorm - n)^2 > 1e-12) {
      f <- (fmin + fmax) / 2
      n <- sum((X / f)^2, na.rm = TRUE)
      if (n > targetnorm) {
        fmax <- f
      } else {
        fmin <- f
      }
    }
  }
  Xn <- X / f
  return(list(Xscaled = Xn, f = f))
}
