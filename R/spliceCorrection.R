#' @title Splice correction of a spectral matrix
#' @description
#' Corrects discontinuities (splices) in a spectral matrix by performing linear 
#' interpolation at the boundaries between different detectors in the spectrometer.
#' This method is commonly used for near-infrared (NIR) spectra where splicing 
#' artifacts occur due to the transition between multiple detectors in the 
#' spectrometer.
#'
#' @usage
#' spliceCorrection(X, wav, splice = c(1000, 1830), interpol.bands = 10)
#'
#' @param X A numeric matrix of spectra or a vector representing a single 
#' spectrum to be corrected (optionally a data frame that can be coerced to a 
#' numeric matrix). Rows represent each spectrum and columns the band positions. 
#' @param wav A numeric vector representing the band positions (e.g. wavelengths)
#' corresponding to the spectral data.
#' @param splice A numeric vector of length 1 or 2 specifying the band 
#' (e.g. wavelength) positions where splicing occurs.
#' The default values \code{c(1000, 1830)} refer to typical splice positions for 
#' NIR spectrometers like the ASD FieldSpec Pro.
#' For other spectrometers, different splice positions may be applicable (see 
#' Details).
#' @param interpol.bands The number of bands around the splice positions to use 
#' for linear interpolation. Default is 10 bands.
#' 
#' @details
#' This function addresses splicing artifacts caused by transitioning between 
#' different detectors, often seen in NIR spectrometers.
#' Splicing occurs when data from multiple detectors are combined, potentially 
#' introducing steps or discontinuities in the spectral data. 
#' This correction method uses linear interpolation across a specified number 
#' of bands around the splice points to smooth transitions and eliminate these 
#' artifacts.
#'
#' The default splice positions are set for the ASD FieldSpec Pro spectrometer, 
#' which has detectors at 1000 nm and 1830 nm. 
#' For other NIR instruments, common splice positions include:
#' \itemize{
#'   \item{XDS (FOSS): 1100 nm}
#'   \item{ProxiMate (BUCHI Labortechnik): 900 nm}
#' }
#'
#' @return A matrix containing the spectra with corrected splices, ensuring 
#' smoother transitions across detectors.
#' @author
#' Antoine Stevens and 
#' \href{https://orcid.org/0000-0002-5369-5120}{Leonardo Ramirez-Lopez}
#' @export


spliceCorrection <- function(X, wav, splice = c(1000, 1830), interpol.bands = 10) {
  extrapfun <- function(x, y, xout) {
    fit <- lm(y ~ x)
    fit$coefficients[1] + fit$coefficients[2] * xout
  }

  if (length(splice) < 1 | length(splice) > 2) {
    stop("splice must be a numeric vector of length 1 or 2")
  }

  if (length(splice) == 1) {
    index_b <- ncol(X)
  } else {
    index_b <- which(wav == splice[2])
  }

  has_three_regions <- length(splice) == 2

  if (is.data.frame(X)) {
    X <- as.matrix(X)
  }

  was_vec <- is.vector(X)
  if (is.vector(X)) {
    nms <- names(X)
    X <- matrix(X, ncol = length(X))
  }
  if (missing(wav)) {
    wav <- seq_len(ncol(X))
  }

  if (length(wav) != ncol(X)) {
    stop("length(wav) must be equal to ncol(X)")
  }

  index <- which(wav %in% splice)

  if (!length(index)) {
    stop("splice positions not found in wav")
  }

  Xa <- X[, 1:index[1], drop = FALSE]
  Xb <- X[, (index[1] + 1):index_b, drop = FALSE]

  tmp_first <- Xb[, 1:interpol.bands, drop = FALSE]
  w_first <- wav[(index[1] + 1):(index[1] + interpol.bands)]
  pred_Xa <- apply(tmp_first, 1, function(y) extrapfun(x = w_first, y = y, xout = splice[1]))
  offset_a <- Xa[, ncol(Xa)] - pred_Xa

  if (has_three_regions) {
    Xc <- X[, (index[2] + 1):ncol(X), drop = FALSE]
    tmp_second <- Xb[, (ncol(Xb) - interpol.bands + 1):ncol(Xb), drop = FALSE]
    w_second <- wav[(index[2] - interpol.bands + 1):index[2]]
    pred_Xb <- apply(tmp_second, 1, function(y) extrapfun(x = w_second, y = y, xout = splice[2]))
    offset_b <- Xc[, 1] - pred_Xb
    output <- cbind(sweep(Xa, 1, offset_a, "-"), Xb, sweep(Xc, 1, offset_b, "-"))
  } else {
    output <- cbind(sweep(Xa, 1, offset_a, "-"), Xb)
  }

  if (was_vec) {
    output <- as.vector(output)
    names(output) <- nms
  } else {
    dimnames(output) <- list(rownames(X), colnames(X))
  }
  return(output)
}
