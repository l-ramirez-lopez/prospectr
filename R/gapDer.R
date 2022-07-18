#' @title Gap-Segment derivative
#' @description
#' Gap-Segment derivatives of a data matrix or vector
#' @usage
#' gapDer(X, m = 1, w = 1, s = 1, delta.wav)
#' @param X a numeric matrix or vector` to transform (optionally a data frame
#' that can be coerced to a numerical matrix).
#' @param m an integer indicating the order of the derivative, larger than 1
#' (default is 1). Note that this function allows for high order derivatives
#' (e.g. m = 6).
#' @param w an integer indicating the gap size (must be odd and >=1), i.e. the spacing
#' between points over which the derivative is computed.
#' @param s an integer indicating the segment size (must be odd and >=1), i.e.
#' the range over which the points are averaged (default = 1, i.e. no
#' smoothing corresponding to Norris-Gap Derivative).
#' @param delta.wav the sampling interval (or band spacing).
#' @author Antoine Stevens and \href{https://orcid.org/0000-0002-5369-5120}{Leonardo Ramirez-Lopez}
#' @details
#' In this type of derivatives, the gap size denotes the length of the x
#' interval that separates the two segments that are averaged.  A detailed
#' explanation of gap segment derivatives can be found in Hopkins (2001).
#'
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
#' der <- gapDer(NIRsoil$spc, m = 1, w = 11, s = 5, delta.wav = 2)
#' matplot(as.numeric(colnames(der)), t(der[1:10, ]),
#'   type = "l",
#'   xlab = "Wavelength /nm",
#'   ylab = "gap derivative"
#' )
#' mtext("1st derivative spectra with: window size: 11 nm, smoothing: 5 nm")
#' par(opar)
#' @references
#' Hopkins, D. W. (2001). What is a Norris derivative?. NIR news, 12(3), 3-5.
#' @seealso \code{\link{savitzkyGolay}}, \code{\link{movav}},
#' \code{\link{binning}}, \code{\link{continuumRemoval}}
#' @return a matrix or vector with the filtered signal(s)
#' @export
#'
gapDer <- function(X, m = 1, w = 1, s = 1, delta.wav) {
  if (w < 1 | !w %% 2) {
    stop("w must be odd and >= 1")
  }
  if (m < 1 | m > 4) {
    stop("m must be between 1 and 4")
  }
  if (s < 1 | !s %% 2) {
    stop("s must be odd and >= 1")
  }

  filter_length <- m * w + (m + 1) * s

  if (filter_length > ncol(X)) {
    stop("the current parameters produce a filter with a length larger than the number of variables in X")
  }

  if (is.data.frame(X)) {
    X <- as.matrix(X)
  }

  zw <- rep(0, w)
  os <- rep(1, s)
  nmr <- factorial(m)

  # if (m == 1) {
  #   fp <- c(-os, zw, os)
  # } else if (m == 2) {
  #   fp <- c(os, zw, -2 * os, zw, os)
  # } else if (m == 3) {
  #   fp <- c(-os, zw, 3 * os, zw, -3 * os, zw, os)
  # } else {
  #   fp <- c(os, zw, -4 * os, zw, 6 * os, zw, -4 * os, zw, os)
  # }

  # Compute the filter in more efficient way allowing for higher order
  # derivatives
  fp <- NULL
  for (k in 0:(m - 1)) {
    ck <- c((-1)^(m - k) * choose(m, k) * os, zw)
    fp <- c(fp, ck)
  }
  fp <- c(fp, os)

  j <- (length(fp) - 1) / 2
  j <- -j:j
  nf <- 1 / nmr * sum((j^m) * fp)
  sg_filter <- fp / nf # filter

  if (is.matrix(X)) {
    if (w >= ncol(X)) {
      stop("filter length w must be lower than ncol(X)")
    }
    output <- convCppM(X, sg_filter) # Convolution
    g <- (length(sg_filter) - 1) / 2
    colnames(output) <- colnames(X)[(g + 1):(ncol(X) - g)]
    rownames(output) <- rownames(X)
  }

  if (is.vector(X)) {
    if (w >= length(X)) {
      stop("filter length w must be lower than length(X)")
    }
    output <- convCppV(X, sg_filter) # Convolution
    g <- (w - 1) / 2
    names(output) <- names(X)[((g + 1):(length(X) - g))]
  }

  if (!missing(delta.wav)) {
    output <- output / delta.wav^m
  }

  return(output)
}
