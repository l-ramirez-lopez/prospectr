#' @title Savitzky-Golay smoothing and differentiation
#' @description
#' \loadmathjax
#' Savitzky-Golay smoothing and derivative of a data matrix or vector.
#' @usage
#' savitzkyGolay(X, m, p, w, delta.wav)
#' @param X a numeric matrix or vector to process (optionally a data frame that
#' can be coerced to a numerical matrix).
#' @param m an integer indcating the differentiation order.
#' @param p an integer indicating the polynomial order.
#' @param w an integer indicating the window size (must be odd).
#' @param delta.wav (optional) sampling interval.
#' @author Antoine Stevens and \href{https://orcid.org/0000-0002-5369-5120}{Leonardo Ramirez-Lopez}
#' @examples
#' data(NIRsoil)
#' opar <- par(no.readonly = TRUE)
#' par(mfrow = c(2, 1), mar = c(4, 4, 2, 2))
#'
#' # plot of the 10 first spectra
#' matplot(as.numeric(colnames(NIRsoil$spc)),
#'   t(NIRsoil$spc[1:10, ]),
#'   type = "l",
#'   xlab = "",
#'   ylab = "Absorbance"
#' )
#'
#' mtext("Raw spectra")
#' NIRsoil$spc_sg <- savitzkyGolay(
#'   X = NIRsoil$spc,
#'   m = 1,
#'   p = 3,
#'   w = 11,
#'   delta.wav = 2
#' )
#'
#' matplot(as.numeric(colnames(NIRsoil$spc_sg)),
#'   t(NIRsoil$spc_sg[1:10, ]),
#'   type = "l",
#'   xlab = "Wavelength /nm",
#'   ylab = "1st derivative"
#' )
#'
#' mtext("1st derivative spectra")
#' par(opar)
#' @details
#' The Savitzky-Golay algorithm fits a local polynomial regression on the signal.
#' It requires evenly spaced data points. Mathematically, it operates simply as
#' a weighted sum over a given window:
#'
#' \mjdeqn{ x_j\ast = \frac{1}{N}\sum_{h=-k}^{k}{c_hx_{j+h}}}{ x_j ast = 1/N \sum_{h=-k}^{k} c_hx_{j+h}}
#'
#' where \mjeqn{x_j\ast}{x_j ast} is the new value, \mjeqn{N}{N} is a
#' normalizing coefficient, \mjeqn{k}{k} is the gap size on each side of
#' \mjeqn{j}{j} and \mjeqn{c_h}{c_h} are pre-computed coefficients, that depends
#' on the chosen polynomial order and degree.
#'
#' The sampling interval specified with the `delta.wav` argument is used for
#' scaling and get numerically correct derivatives.
#'
#' The convolution function is written in C++/Rcpp for faster computations.
#'
#' @references
#' Luo, J., Ying, K., He, P., & Bai, J. (2005). Properties of Savitzky–Golay
#' digital differentiators. Digital Signal Processing, 15(2), 122-136.
#'
#' Savitzky, A., and Golay, M.J.E., 1964. Smoothing and
#' differentiation of data by simplified least squares procedures.
#' Anal. Chem. 36, 1627-1639.
#'
#' Schafer, R. W. (2011). What is a Savitzky-Golay filter? (lecture notes). IEEE
#' Signal processing magazine, 28(4), 111-117.
#'
#' Wentzell, P.D., and Brown, C.D., 2000. Signal processing in analytical
#' chemistry. Encyclopedia of Analytical Chemistry, 9764-9800.
#' @export
#'

savitzkyGolay <- function(X, m, p, w, delta.wav) {
  if (is.data.frame(X)) {
    X <- as.matrix(X)
  }

  if (w %% 2 != 1) {
    stop("needs an odd filter length w")
  }
  if (p >= w) {
    stop("filter length w mus be greater than polynomial order p")
  }
  if (p < m) {
    stop("polynomial order p must be geater or equal to differentiation order m")
  }

  gap <- (w - 1) / 2
  basis <- outer(-gap:gap, 0:p, "^")
  A <- solve(crossprod(basis, basis), tol = 0) %*% t(basis)

  if (is.matrix(X)) {
    if (w >= ncol(X)) {
      stop("filter length w must be lower than ncol(X)")
    }
    output <- factorial(m) * convCppM(X, A[m + 1, ])
    g <- (w - 1) / 2
    colnames(output) <- colnames(X)[(g + 1):(ncol(X) - g)]
    rownames(output) <- rownames(X)
  }

  if (is.vector(X)) {
    if (w >= length(X)) {
      stop("filter length w must be lower than length(X)")
    }
    output <- factorial(m) * convCppV(X, A[m + 1, ])
    g <- (w - 1) / 2
    names(output) <- names(X)[(g + 1):(length(X) - g)]
  }
  # scaling
  if (!missing(delta.wav)) {
    output <- output / delta.wav^m
  }

  return(output)
}
