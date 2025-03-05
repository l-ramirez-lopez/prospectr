#' @title Detrending spectral data
#' @description
#' \loadmathjax
#' Normalizes each row of an input matrix by applying a SNV transformation
#' followed by fitting a second order linear model and returning the fitted
#' residuals.
#' @usage
#' detrend(X, wav, p = 2)
#' @param X a numeric matrix or vector to process  (optionally a data frame that
#' can be coerced to a numerical matrix)
#' @param wav the wavelengths/ band centers.
#' @param p an integer larger than 1 indicating the polynomial order (default is
#' 2, as in the original paper of Barnes et al., 1989).
#' @author Antoine Stevens and \href{https://orcid.org/0000-0002-5369-5120}{Leonardo Ramirez-Lopez}
#' @examples
#' data(NIRsoil)
#' wav <- as.numeric(colnames(NIRsoil$spc))
#' # conversion to reflectance
#' opar <- par(no.readonly = TRUE)
#' par(mfrow = c(2, 1), mar = c(4, 4, 2, 2))
#' # plot of the 10 first spectra
#' matplot(wav, t(NIRsoil$spc[1:10, ]),
#'   type = "l",
#'   xlab = "",
#'   ylab = "Absorbance"
#' )
#' mtext("Raw spectra")
#' det <- detrend(NIRsoil$spc, wav)
#' matplot(wav, t(det[1:10, ]),
#'   type = "l",
#'   xlab = "Wavelength /nm",
#'   ylab = "Absorbance"
#' )
#' mtext("Detrend spectra")
#' par(opar)
#' @details The detrend is a row-wise transformation that allows to correct for
#' wavelength-dependent scattering effects (variations in curvilinearity). A
#' \mjeqn{p}{p} order polynomial is fit for each spectrum (\mjeqn{x_i}{x_i})
#' using the vector of bands (\mjeqn{\lambda}{\lambda}, e.g. wavelengths) as
#' explanatory variable as follows:
#'
#' \mjdeqn{x_i = a\lambda^p + ... + b\lambda + c + e_i}{x_i = a\lambda^p + ... + b\lambda + c + e_i}
#'
#' were a, b, c are estimated by least squares, and \mjeqn{e_i}{e_i} are the
#' spectral residuals of the least square fit. The residuals of the \mjeqn{i}{i}th
#' correspond to the \mjeqn{i}{i}th detrended spectrum.
#'
#' @seealso \code{\link{standardNormalVariate}}, \code{\link{blockScale}},
#'  \code{\link{blockNorm}}
#' @references Barnes RJ, Dhanoa MS, Lister SJ. 1989. Standard normal variate
#' transformation and de-trending of near-infrared diffuse reflectance spectra.
#' Applied spectroscopy, 43(5): 772-777.
#' @return a matrix or vector with the detrended data.
#' @export

detrend <- function(X, wav, p = 2) {
  if (missing(wav)) {
    stop("argument wav must be specified")
  }

  if (is.data.frame(X)) {
    X <- as.matrix(X)
  }

  was_vec <- is.vector(X)
  if (p < 1) {
    stop("'p' must be an integer larger than 0")
  }

  if (p != round(p)) {
    stop("'p' must be an integer")
  }

  if (is.vector(X)) {
    nms <- names(X)
    X <- matrix(X, ncol = length(X))
  }

  xpoly <- stats::poly(wav, p)
  # SNV transformation
  X <- sweep(X, 1, rowMeans(X), "-")
  X <- sweep(X, 1, apply(X, 1, sd), "/")

  # get the residuals output <- t(apply(X, 1, function(y) lm.fit(x= xpoly,y)$residuals))
  output <- residLm(X, xpoly) # using Rcpp ...

  if (was_vec) {
    output <- as.vector(output)
    names(output) <- nms
  } else {
    dimnames(output) <- list(rownames(X), colnames(X))
  }
  return(output)
}
