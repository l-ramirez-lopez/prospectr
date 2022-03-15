#' @title Moving average
#' @description
#' A simple moving average of a matrix or vector using a convolution
#' function written in C++/Rcpp for fast computing
#' @usage
#' movav(X, w)
#' @param X a numeric matrix or vector to process (optionally a data frame that can
#' be coerced to a numerical matrix).
#' @param w filter length.
#' @author Antoine Stevens
#' @examples
#' data(NIRsoil)
#' wav <- as.numeric(colnames(NIRsoil$spc))
#' # adding some noise
#' NIRsoil$spc_noise <- NIRsoil$spc + rnorm(length(NIRsoil$spc), 0, 0.001)
#' matplot(wav,
#'   t(NIRsoil$spc_noise[1:10, ]),
#'   type = "l",
#'   lty = 1,
#'   xlab = "Wavelength /nm",
#'   ylab = "Absorbance",
#'   col = "grey"
#' )
#'
#' # window size of 11 bands
#' NIRsoil$spc_mov <- movav(NIRsoil$spc_noise, w = 15)
#' # smoothed data
#' matlines(as.numeric(colnames(NIRsoil$spc_mov)),
#'   t(NIRsoil$spc_mov[1:10, ]),
#'   type = "l",
#'   lty = 1
#' )
#' @return a matrix or vector with the filtered signal(s)
#' @seealso \code{\link{savitzkyGolay}}, \code{\link{gapDer}},
#' \code{\link{binning}}, \code{\link{continuumRemoval}}
#' @export
#'
movav <- function(X, w) {
  if (is.data.frame(X)) {
    X <- as.matrix(X)
  }
  if (missing(w)) {
    stop("filter length w should be specified")
  }
  if (w < 1) {
    stop("filter length w should be > 0")
  }
  if (w == 1) {
    return(X)
  }

  f <- rep(1, w) / w # filter

  if (is.matrix(X)) {
    if (w >= ncol(X)) {
      stop("filter length w should be lower than ncol(X)")
    }
    output <- convCppM(X, f) # Convolution
    g <- ceiling((w - 1) / 2)
    colnames(output) <- colnames(X)[((g + w %% 2):(ncol(X) - g))]
    rownames(output) <- rownames(X)
  }

  if (is.vector(X)) {
    if (w >= length(X)) {
      stop("filter length w should be lower than length(X)")
    }
    output <- convCppV(X, f) # Convolution
    g <- (w - 1) / 2
    names(output) <- names(X)[((g + w %% 2):(length(X) - g))]
  }

  return(output)
}
