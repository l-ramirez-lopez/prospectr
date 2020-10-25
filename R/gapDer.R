#' @title Gap-Segment derivative
#' @description
#' Gap-Segment derivatives of a data matrix or vector
#' @usage
#' gapDer(X, m = 1, w = 1, s = 1, delta.wav)
#' @param X a numeric matrix or vector` to transform (optionally a data frame
#' that can be coerced to a numerical matrix).
#' @param m the order of the derivative, between 1 and 4 (default = 1).
#' @param w the filter length (should be odd and >=1), i.e. the spacing
#' between points over which the derivative is computed.
#' @param s the segment size, i.e. the range over which the points are averaged
#' (default = 1, i.e. no smoothing corresponding to 'Norris' Gap Derivative).
#' @param delta.wav the sampling interval (or band spacing).
#' @author Antoine Stevens
#' @details
#' The sampling interval specified with the `delta.wav` argument is used for
#' scaling and get numerically correct derivatives.
#'
#' The convolution function is written in C++/Rcpp for faster computations.
#' @examples
#' data(NIRsoil)
#' opar <- par(no.readonly = TRUE)
#' par(mfrow = c(2, 2), mar = c(4, 4, 2, 2))
#' # plot of the 10 first spectra
#' matplot(as.numeric(colnames(NIRsoil$spc)),
#'   t(NIRsoil$spc[1:10, ]),
#'   type = "l",
#'   xlab = "",
#'   ylab = "Absorbance"
#' )
#' mtext("Raw spectra")
#'
#' der <- gapDer(NIRsoil$spc, m = 1, w = 1, s = 1, delta.wav = 2)
#' matplot(as.numeric(colnames(der)),
#'   t(der[1:10, ]),
#'   type = "l",
#'   xlab = "Wavelength /nm",
#'   ylab = "gap derivative"
#' )
#'
#' mtext("1st derivative spectra")
#' der <- gapDer(NIRsoil$spc, m = 1, w = 11, s = 1, delta.wav = 2)
#' matplot(as.numeric(colnames(der)), t(der[1:10, ]),
#'   type = "l",
#'   xlab = "Wavelength /nm",
#'   ylab = "gap derivative"
#' )
#'
#' mtext("1st derivative spectra with a window size = 11 nm")
#' der <- gapDer(NIRsoil$spc, m = 1, w = 11, s = 10, delta.wav = 2)
#' matplot(as.numeric(colnames(der)), t(der[1:10, ]),
#'   type = "l",
#'   xlab = "Wavelength /nm",
#'   ylab = "gap derivative"
#' )
#' mtext("1st derivative spectra with: window size: 11 nm, smoothing: 10 nm")
#' par(opar)
#' 
#' @references Hopkins (2002). NIR News 14(5), 10.
#' @seealso \code{\link{savitzkyGolay}}, \code{\link{movav}}, 
#' \code{\link{binning}}, \code{\link{continuumRemoval}}
#' @return a matrix or vector with the filtered signal(s)
#' @export
#'
gapDer <- function(X, m = 1, w = 1, s = 1, delta.wav) {
  if (w < 1 | !w %% 2) {
    stop("w should be odd and >= 1")
  }
  if (m < 1 | m > 4) {
    stop("m should be between 1 and 4")
  }
  if (s < 1) {
    stop("s should be  >=1")
  }

  if (is.data.frame(X)) {
    X <- as.matrix(X)
  }
  zw <- rep(0, w)
  os <- rep(1, s)

  if (m == 1) {
    fp <- c(-os, zw, os)
    nmr <- 1
  } else if (m == 2) {
    fp <- c(os, zw, -2 * os, zw, os)
    nmr <- 2
  } else if (m == 3) {
    fp <- c(-os, zw, -3 * os, zw, -3 * os, zw, os)
    nmr <- 6
  } else {
    fp <- c(os, zw, -4 * os, zw, 6 * os, zw, -4 * os, zw, os)
    nmr <- 24
  }
  j <- (length(fp) - 1) / 2
  j <- -j:j
  nf <- 1 / nmr * sum((j^m) * fp)
  f <- fp / nf # filter

  if (is.matrix(X)) {
    if (w >= ncol(X)) {
      stop("filter length w should be lower than ncol(X)")
    }
    output <- convCppM(X, f) # Convolution
    g <- (length(f) - 1) / 2
    colnames(output) <- colnames(X)[(g + 1):(ncol(X) - g)]
    rownames(output) <- rownames(X)
  }

  if (is.vector(X)) {
    if (w >= length(X)) {
      stop("filter length w should be lower than length(X)")
    }
    output <- convCppV(X, f) # Convolution
    g <- (w - 1) / 2
    names(output) <- names(X)[((g + 1):(length(X) - g))]
  }

  if (!missing(delta.wav)) {
    output <- output / delta.wav^m
  }

  return(output)
}
