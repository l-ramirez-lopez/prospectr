#' @title Detrend transformation
#' @description
#' Normalizes each row of an input `data.frame` or `matrix` by applying a SNV transformation
#' followed by fitting a second order linear model and returning the fitted residuals.
#' @usage
#' detrend(X, wav)
#' @param X a numeric `data.frame`, `matrix` or `vector` to process.
#' @param wav the wavelengths/ band centers.
#' @author Antoine Stevens
#' @examples
#' data(NIRsoil)
#' wav <- as.numeric(colnames(NIRsoil$spc))
#' # conversion to reflectance
#' spc <- 1 / 10^NIRsoil$spc
#' opar <- par(no.readonly = TRUE)
#' par(mfrow = c(2, 1), mar = c(4, 4, 2, 2))
#' # plot of the 10 first spectra
#' matplot(wav, t(spc[1:10, ]),
#'   type = "l",
#'   xlab = "",
#'   ylab = "Reflectance"
#' )
#' mtext("Raw spectra")
#' det <- detrend(spc, wav)
#' matplot(wav, t(det[1:10, ]),
#'   type = "l",
#'   xlab = "Wavelength /nm",
#'   ylab = "Reflectance"
#' )
#' mtext("Detrend spectra")
#' par(opar)
#' @details The detrend is a row-wise transformation that allows to correct for wavelength-dependent
#' scattering effects (variations in curvilinearity). A second-degree polynomial is fit through each spectrum:
#' \deqn{x_i = a\lambda^2 + b\lambda + c + e_i}
#' were \eqn{x_i} is the spectrum, \eqn{\lambda} is the wavelength vector, a, b, c are estimated by least square,
#' and \eqn{e_i} are the residuals of the least square fit. Then, a detrend spectrum corresponds to:
#' \deqn{x\ast_i = x_i - (a\lambda^2 + b\lambda + c = e_i)}
#' @seealso \code{\link{standardNormalVariate}}, \code{\link{blockScale}}, \code{\link{blockNorm}}
#' @references Barnes RJ, Dhanoa MS, Lister SJ. 1989. Standard normal variate transformation and de-trending of near-infrared diffuse reflectance spectra. Applied spectroscopy, 43(5): 772-777.
#' @return a `matrix` or `vector` with detrend values
#' @export
#'
detrend <- function(X, wav) {
  if (missing(wav)) {
    stop("argument wav should be specified")
  }

  if (is.data.frame(X)) {
    X <- as.matrix(X)
  }

  was.vec <- is.vector(X)

  if (is.vector(X)) {
    nms <- names(X)
    X <- matrix(X, ncol = length(X))
  }

  xpoly <- stats::poly(wav, 2)
  # SNV transformation
  X <- sweep(X, 1, rowMeans(X), "-")
  X <- sweep(X, 1, apply(X, 1, sd), "/")

  # get the residuals output <- t(apply(X, 1, function(y) lm.fit(x= xpoly,y)$residuals))
  output <- residLm(X, xpoly) # using Rcpp ...

  if (was.vec) {
    output <- as.vector(output)
    names(output) <- nms
  } else {
    dimnames(output) <- list(rownames(X), colnames(X))
  }
  return(output)
}
