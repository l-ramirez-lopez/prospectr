#' @title baseline
#' @description
#'
#' \ifelse{html}{\out{<a href='https://www.tidyverse.org/lifecycle/#maturing'><img src='figures/lifecycle-maturing.svg' alt='Maturing lifecycle'></a>}}{\strong{Maturing}}
#'
#' Fits a baseline to each spectrum in a matrix and removes it from the
#' corresponding input spectrum. A vector can also be passed to this function.
#' @usage
#' baseline(X, wav)
#' @param X a numeric matrix or vector to process (optionally a data frame that
#' can be coerced to a numerical matrix).
#' @param wav optional. A numeric vector of band positions.
#' @author \href{https://orcid.org/0000-0002-5369-5120}{Leonardo Ramirez-Lopez}
#' with contributions from Mervin Manalili
#' @return a matrix or vector with the baselined spectra. The resulting matrix
#' is output with an attribute called \code{baselines} which contain the spectra
#' of the fitted baselines.
#'
#' This function is similar to \code{\link{continuumRemoval}} and it might
#' replace some of its functionality in the future.
#'
#' @examples
#' data(NIRsoil)
#' wav <- as.numeric(colnames(NIRsoil$spc))
#' # plot of the 5 first absorbance spectra
#' matplot(wav,
#'   t(NIRsoil$spc[1:5, ]),
#'   type = "l",
#'   ylim = c(0, .6),
#'   xlab = "Wavelength /nm",
#'   ylab = "Absorbance"
#' )
#'
#' bs <- baseline(NIRsoil$spc, wav)
#' matlines(wav, t(bs[1:5, ]))
#'
#' fitted_baselines <- attr(bs, "baselines")
#' matlines(wav, t(fitted_baselines[1:5, ]))
#' title("Original spectra, baselines and baselined spectra")
#' @seealso
#' \code{\link{savitzkyGolay}}, \code{\link{movav}},
#' \code{\link{gapDer}}, \code{\link{binning}}, \code{\link{continuumRemoval}}
#' @details
#' The baseline function find points lying on the convex hull
#' of a spectrum, connects the points by linear interpolation and
#' subtracts the interpolated line (baseline) from the corresponding spectrum.
#' @export

baseline <- function(X, wav) {
  if (is.data.frame(X)) {
    X <- as.matrix(X)
  }

  if (missing(wav)) {
    wav <- 1:ncol(X)
  }

  wav <- c(
    wav[1] - diff(wav[1:2]),
    wav,
    wav[length(wav)] + diff(wav[(length(wav) - 1):length(wav)])
  )

  # make sure the edges will be well above any peak
  edges <- abs(apply(X, 1, "max")) + abs(apply(X, 1, "min"))
  edges <- edges * 2

  X <- cbind(edges, X, edges)
  colnames(X) <- wav

  ## simple baseline function
  simple_bs <- function(x, wav) {
    id <- sort(chull(wav, x))
    id <- id[-c(1, length(id))]
    hull_line <- approx(x = wav[id], y = x[id], xout = wav, method = "linear")$y
    return(hull_line)
  }

  if (is.matrix(X)) {
    if (missing(wav)) {
      wav <- seq_len(ncol(X))
    }
    if (length(wav) != ncol(X)) {
      stop("length(wav) must be equal to ncol(X)")
    }
    hull_line <- t(apply(X, 1, function(x) simple_bs(x, wav)))
  } else {
    hull_line <- simple_bs(X, wav)
  }

  hull_line <- hull_line[, -c(1, ncol(hull_line))]
  X <- X[, -c(1, ncol(X))]

  baselined <- X - hull_line

  wav <- wav[-c(1, length(wav))]

  if (is.matrix(X)) {
    colnames(hull_line) <- colnames(baselined) <- wav
    rownames(hull_line) <- rownames(baselined) <- rownames(X)
  } else {
    hull_line <- as.vector(hull_line)
    names(hull_line) <- names(baselined) <- wav
  }
  attr(baselined, "baselines") <- hull_line
  return(baselined)
}
