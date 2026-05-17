#' @title Continuum Removal
#' @description
#'
#' \ifelse{html}{\out{<a href='https://www.tidyverse.org/lifecycle/#maturing'><img src='figures/lifecycle-maturing.svg' alt='Maturing lifecycle'></a>}}{\strong{Maturing}}
#'
#' Compute the continuum removed values of a data matrix or vector
#' @usage
#' continuumRemoval(X, wav, type = c("R", "A"),
#'                  interpol = c("linear", "spline"),
#'                  method = c("division", "subtraction"))
#' @param X a numeric matrix or vector to process (optionally a data frame that can
#' be coerced to a numerical matrix).
#' @param wav a numeric vector of band positions of length equal to \code{ncol(X)}
#' (or \code{length(X)} if \code{X} is a vector). If not provided, integer
#' indices \code{1:ncol(X)} are used.
#' @param type the type of data: 'R' for reflectance (default), 'A' for
#' absorbance.
#' @param interpol the interpolation method between points on the convex hull:
#' 'linear' (default) or 'spline'.
#' @param method normalization method: 'division' (default) or 'subtraction'
#' (see details section).
#' @author Antoine Stevens & \href{https://orcid.org/0000-0002-5369-5120}{Leonardo Ramirez-Lopez}
#' @return a matrix or vector with the filtered spectra.
#' @examples
#' data(NIRsoil)
#' wav <- as.numeric(colnames(NIRsoil$spc))
#' # plot of the 10 first abs spectra
#' matplot(wav,
#'   t(NIRsoil$spc[1:10, ]),
#'   type = "l",
#'   ylim = c(0, .6),
#'   xlab = "Wavelength /nm",
#'   ylab = "Abs"
#' )
#' #  # type = 'A' is used for absorbance spectra
#' cr <- continuumRemoval(NIRsoil$spc, wav, type = "A")
#' matlines(wav, t(cr[1:10, ]))
#' @seealso
#' \code{\link{savitzkyGolay}}, \code{\link{movav}},
#' \code{\link{gapDer}}, \code{\link{binning}}
#' @details
#' The continuum removal technique was introduced by Clark and Roush (1984)
#' to highlight absorption features in spectra by removing the effect of
#' the overall spectral shape (albedo). It is widely used in remote sensing
#' and spectroscopy to isolate and compare absorption band depths across
#' samples or sensors.
#'
#' The algorithm identifies points lying on the convex hull (upper envelope)
#' of a spectrum, connects them by linear or spline interpolation to form a
#' continuum line, and normalises the spectrum against that line either by
#' division or subtraction. Division (default) yields values in \[0, 1\] for
#' reflectance spectra, where 1 indicates no absorption. Subtraction yields
#' residuals relative to the continuum.
#'
#' When \code{type = "A"} (absorbance), spectra are inverted to reflectance
#' before computing the continuum and back-transformed afterwards.
#'
#' At wavelengths where both the spectral value and the continuum are zero,
#' the continuum-removed value is set to 1 (no absorption feature), since
#' division of zero by zero is undefined.
#' @references
#' Clark, R.N., and Roush, T.L., 1984. Reflectance Spectroscopy: Quantitative
#' Analysis Techniques for Remote Sensing Applications. J. Geophys. Res. 89,
#' 6329-6340.
#' @export
continuumRemoval <- function(
    X,
    wav,
    type = c("R", "A"),
    interpol = c("linear", "spline"),
    method = c("division", "sustraction")
) {
  
  if (!missing(method) && method == "substraction") {
    warning("'substraction' is a typo; please use 'subtraction'. Continuing with 'subtraction'.")
    method <- "subtraction"
  }
  
  if (is.data.frame(X)) {
    X <- as.matrix(X)
  }
  
  type <- match.arg(type)
  interpol <- match.arg(interpol)
  method <- match.arg(method)
  
  if (type == "A") {
    X <- 1 / X
  }
  
  if (is.matrix(X)) {
    if (missing(wav)) {
      wav <- seq_len(ncol(X))
    }
    if (length(wav) != ncol(X)) {
      stop("length(wav) should be equal to ncol(X)")
    }
    
    cont <- t(apply(X, 1, function(x) cr_fun(x, wav, interpol)))
  } else {
    cont <- cr_fun(X, wav, interpol)
  }
  
  
  if (method == "division") {
    cr <- X / cont
    cr <- X / cont
    cr[X == 0 & cont == 0] <- 1
  } # like ENVI
  else {
    cr <- 1 + X - cont
  }
  
  if (type == "A") {
    cr <- 1 / cr - 1
  }
  
  if (is.matrix(X)) {
    colnames(cr) <- wav
    rownames(cr) <- rownames(X)
  } else {
    names(cr) <- wav
  }
  
  return(cr)
}


#' @noRd
#' @export
cr_fun <- function(x, wav, interpol) {
  neighbor_left <- (wav[2] - wav[1]) / 1000
  neighbor_right <- (wav[length(wav)] - wav[length(wav) - 1]) / 1000
  y_bound <- min(x) - (max(x) - min(x))
  
  id <- sort(
    chull(
      c(wav[1] - neighbor_left, wav, wav[length(wav)] + neighbor_right),
      c(y_bound, x, y_bound)
    )
  )
  id <- id[-c(1, length(id))] - 1
  
  cont <- switch(
    interpol,
    linear = approx(x = wav[id], y = x[id], xout = wav, method = "linear")$y,
    spline = splinefun(x = wav[id], y = x[id])(wav)
  )
  return(cont)
}
