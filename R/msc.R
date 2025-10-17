#' @title Multiplicative Scatter Correction (msc)
#'
#' @description
#' \loadmathjax
#' \ifelse{html}{\out{<a href='https://www.tidyverse.org/lifecycle/#maturing'><img src='figures/lifecycle-maturing.svg' alt='Maturing lifecycle'></a>}}{\strong{Maturing}}
#'
#' This function implements the multiplicative scatter correction method
#' which attempts to remove physical light scatter by accounting for additive
#' and multiplicative effects (Geladi et al., 1985).
#'
#' @usage
#' msc(X, ref_spectrum = colMeans(X))
#'
#' @param X a numeric matrix of spectral data.
#' @param ref_spectrum a numeric vector corresponding to an "ideal" reference
#' spectrum (e.g. free of scattering effects). By default the function uses the
#' mean spectrum of the input \code{X}. See details. Note that this argument was
#' previously named as `reference_spc`, however, it has been renamed to
#' `ref_spectrum` to emphasize that this argument is a vector and not a
#' matrix of spectra.
#'
#' @details
#' The Multiplicative Scatter Correction (MSC) is a normalization method that
#' attempts to account for additive and multiplicative effects by aligning each
#' spectrum (\mjeqn{x_i}{x_i}) to an ideal reference one (\mjeqn{x_r}{x_r}) as
#' follows:
#'
#' \mjdeqn{x_i = m_i x_r + a_i}{x_i = m_i x_r + a_i}
#' \mjdeqn{MSC(x_i) = \frac{a_i - x_i}{m_i}}{MSC(x_i) = {a_i - x_i}/{m_i}}
#'
#' where \mjeqn{a_i}{a_i} and \mjeqn{m_i}{m_i} are the additive and
#' multiplicative terms respectively.
#' @return
#' a matrix of normalized spectral data with an attribute which indicates the
#' reference spectrum used.
#' @author
#' \href{https://orcid.org/0000-0002-5369-5120}{Leonardo Ramirez-Lopez} and Guillaume Hans
#'
#' @references
#' Geladi, P., MacDougall, D., and Martens, H. 1985. Linearization and
#' Scatter-Correction for Near-Infrared Reflectance Spectra of Meat.
#' Applied Spectroscopy, 39(3):491-500.
#'
#' @seealso \code{\link{standardNormalVariate}}, \code{\link{detrend}},
#' \code{\link{blockScale}}, \code{\link{blockNorm}}
#'
#' @examples
#' data(NIRsoil)
#' NIRsoil$msc_spc <- msc(X = NIRsoil$spc)
#'
#' # 10 first msc spectra
#' matplot(
#'   x = as.numeric(colnames(NIRsoil$msc_spc)),
#'   y = t(NIRsoil$msc_spc[1:10, ]),
#'   type = "l",
#'   xlab = "wavelength, nm",
#'   ylab = "msc"
#' )
#'
#' # another example
#' spectra_a <- NIRsoil$spc[1:40, ]
#' spectra_b <- NIRsoil$spc[-(1:40), ]
#'
#' spectra_a_msc <- msc(spectra_a, colMeans(spectra_a))
#'
#' # correct spectra_a based on the reference spectrum used to correct
#' # spectra_a
#'
#' spectra_b_msc <- msc(
#'   spectra_b,
#'   ref_spectrum = attr(spectra_a_msc, "Reference spectrum")
#' )
#' @export


msc <- function(X, ref_spectrum = colMeans(X)) {
  X <- as.matrix(X)

  if (!is.vector(ref_spectrum)) {
    stop("'ref_spectrum' must be a vector")
  }

  if (ncol(X) != length(ref_spectrum)) {
    stop("The number of column in X must be equal to the length of 'ref_spectrum'")
  }
  offsets_slopes <- get_msc_coeff(X, ref_spectrum)
  Xz <- sweep(X, MARGIN = 1, STATS = offsets_slopes[1, ], FUN = "-", check.margin = FALSE)
  Xz <- sweep(Xz, MARGIN = 1, STATS = offsets_slopes[2, ], FUN = "/", check.margin = FALSE)
  attr(Xz, "Reference spectrum:") <- ref_spectrum
  Xz
}
