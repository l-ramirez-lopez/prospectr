#' @title Splice correction of a spectral matrix acquired with an ASD spectrometer
#' @description
#' Corrects steps in an input spectral matrix by linear interpolation of the
#' values of the edges of the middle sensor
#'
#' @usage
#'
#' spliceCorrection(X, wav, splice = c(1000, 1830), interpol.bands = 10)
#'
#' @param X a numeric matrix or vector to transform (optionally a data frame that can
#' be coerced to a numerical matrix).
#' @param wav a numeric vector with band positions.
#' @param splice a numeric vector of length 1 or 2 with the positions of the
#' splice(s). Default:
#' \code{c(1000, 1830)} (as for the ASD FieldSpec Pro spectrometer of Malvern
#' Panalytical). See details.
#' @param interpol.bands the number of interpolation bands.
#' @details
#' This function uses by default the positions for the ASD FieldSpec Pro
#' spectroradiometer (Malvern Panalytical) which usually exhibit
#' steps at the splice of the three built-in detectors,
#' positioned at 1000 nm (end of VNIR detector) and 1830 nm (end of SWIR1 detector).
#' The data corresponding to the spectral region after the first step is used as
#' reference for correcting the first region and the laste region (if 2 steps
#' are supplied).
#' Other typical examples of splice artifacts caused by concatenating data
#' captured by different detectors inside the spectrometer:
#' \itemize{
#'  \item{XDS (FOSS): 1100 nm}
#'  \item{ProxiMate (BUCHI Labortechnik): 900 nm}
#' }
#'
#'
#' @return a matrix with the splice corrected data.
#' @author Antoine Stevens and \href{https://orcid.org/0000-0002-5369-5120}{Leonardo Ramirez-Lopez}
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
