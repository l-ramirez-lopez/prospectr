#' MSC (Multiplicative Scatter Correction)
#'
#' The \code{msc} function implements the multiplicative scatter correction based on the mean spectrum.
#'
#' @param x a \code{matrix} - the spectral matrix (each spectrum is a row).
#'
#' @return a \code{matrix} of corrected spectra.
#'
#' @author
#' Guillaume Hans
#'
#' @references
#' Geladi, P., MacDougall, D., and Martens, H. (1985) Linearization and Scatter-Correction for Near-Infrared Reflectance Spectra of Meat.
#' Applied Spectroscopy, 39(3):491-500.
#'
#' @seealso \code{\link{snv}} \code{\link{mean}}
#'
#' @examples
#' tranf_msc <- msc(NIRsoil$spc)
#' @export
msc <- function(x) {
  x <- as.matrix(x)
  Z <- cbind(1, colMeans(x))
  B <- t(solve(crossprod(Z), t(x %*% Z)))
  new_spec <- (x - B[, 1]) / B[, 2]

  return(new_spec)
}

## FIXME: add predition method?
