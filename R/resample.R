#' @title Resample spectral data
#' @description
#' Resample a data matrix or vector to new coordinates (e.g.
#' band positions) using spline or linear interpolation. This function is a
#' simple wrapper around \code{\link{approx}} and \code{\link{splinefun}} in
#' \pkg{base}.
#' @usage
#' resample(X, wav, new.wav, interpol = "spline", ...)
#' @param X numeric matrix or vector to resample (optionally a data frame that
#' can be coerced to a numerical matrix).
#' @param wav a numeric vector giving the original band positions.
#' @param new.wav a numeric vector giving the new band positions.
#' @param interpol the interpolation method: 'linear' or 'spline' (default).
#' @param ... additional arguments to be passed to the \code{\link{splinefun}}
#' function when \code{interpol = 'spline'}.
#' @author Antoine Stevens and \href{https://orcid.org/0000-0002-5369-5120}{Leonardo Ramirez-Lopez}
#' @examples
#' data(NIRsoil)
#' wav <- as.numeric(colnames(NIRsoil$spc))
#' # increase spectral resolution by 2
#' NIRsoil$spc_resampled <- resample(NIRsoil$spc, wav, seq(1100, 2498, 2))
#' dim(NIRsoil$spc)
#' dim(NIRsoil$spc_resampled)
#' @return
#' a matrix or vector with resampled values.
#' @seealso
#' \code{\link{resample2}}
#' @export
#'
resample <- function(X, wav, new.wav, interpol = "spline", ...) {
  if (is.data.frame(X)) {
    X <- as.matrix(X)
  }
  if (missing(wav)) {
    stop("wav argument must be specified")
  }

  if (!interpol %in% c("linear", "spline")) {
    stop("Argument 'interpol' must be either 'linear or 'spline'")
  }

  resfun <- function(x, interpol) {
    if (interpol == "linear") {
      approx(x = wav, y = x, xout = new.wav, method = "linear")$y
    } else {
      splinefun(x = wav, y = x, ...)(new.wav)
    }
  }

  if (is.matrix(X)) {
    if (length(wav) != ncol(X)) {
      stop("length(wav) must be equal to ncol(X)")
    }

    output <- t(apply(X, 1, resfun, interpol))
    rownames(output) <- rownames(X)
    colnames(output) <- new.wav
  } else {
    if (length(wav) != length(X)) {
      stop("length(wav) must be equal to length(X)")
    }
    output <- resfun(X, interpol)
    names(output) <- new.wav
  }

  return(output)
}
