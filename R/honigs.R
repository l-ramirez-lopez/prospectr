#' @title Honigs algorithm for calibration sampling
#' @description
#' Select calibration samples from a data matrix using the Honings et al. (1985)
#' method
#' @usage
#' honigs(X, k, type)
#' @param X a numeric matrix with absorbance or continuum-removed reflectance
#' values (optionally a data frame that can be coerced to a numerical matrix).
#' @param k the number of samples to select for calibration.
#' @param type type of data: 'A' for absorbance (default), 'R' for reflectance,
#' 'CR' for continuum-removed reflectance
#' @author Antoine Stevens
#' @return a `list` with components:
#' \itemize{
#'  \item{'`model`': numeric vector giving the row indices of the input data
#'  selected for calibration}
#'  \item{'`test`': numeric vector giving the row indices of the remaining
#'  observations}
#'  \item{'`bands`': indices of the columns used during the selection procedure}
#' }
#' @examples
#' data(NIRsoil)
#' sel <- honigs(NIRsoil$spc, k = 10, type = "A")
#' wav <- as.numeric(colnames(NIRsoil$spc))
#' # spectral library
#' matplot(wav,
#'   t(NIRsoil$spc),
#'   type = "l",
#'   xlab = "wavelength /nm",
#'   ylab = "Abs",
#'   col = "grey50"
#' )
#' # plot calibration spectra
#' matlines(wav,
#'   t(NIRsoil$spc[sel$model, ]),
#'   type = "l",
#'   xlab = "wavelength /nm",
#'   ylab = "Abs",
#'   lwd = 2,
#'   lty = 1
#' )
#' # add bands used during the selection process
#' abline(v = wav[sel$bands])
#' @details
#' The Honigs algorithm is a simple method to select calibration samples based
#' on their absorption features. Absorbance, reflectance and continuum-removed
#' reflectance values (see \code{\link{continuumRemoval}}) can be used (`type`
#'  argument).
#' The algorithm can be described as follows: let \eqn{A} be a matrix of
#' \eqn{(i \times j)} absorbance values:
#'
#' \enumerate{
#'  \item the observation (row) with the maximum absolute absorbance
#'  (\eqn{max(|A|)}) is selected and assigned to the calibration set.
#'  \item a vector of weights \eqn{W} is computed as \eqn{A_j/max_A} where
#'  \eqn{A_j} is the column of \eqn{A} having the maximum absolute absorbance
#'  and \eqn{max_A} is the absorbance value corresponding to the maximum
#'  absolute absorbance of \eqn{A}
#'  \item each row \eqn{A_i} is multiplied by the corresponding weight \eqn{W_i}
#'  and the resulting vector is subtracted from the original row \eqn{A_i}.
#'  \item the row of the selected observation and the column with the maximum
#'  absolute absorbance is removed from the matrix
#'  \item go back to step 1 and repeat the procedure until the desired number
#'  of selected samples is reached
#' }
#'
#' The observation with the maximum absorbance is considered to have
#' an unusual composition. The algorithm selects therefore this observation and
#' remove from other samples the selected absorption feature by subtraction.
#' Samples with low concentration related to this absorption will then have
#' large negative absorption after the subtraction step
#' and hence will be likely to be selected rapidly by the selection procedure
#' as well.
#'
#' @note The selection procedure is sensitive to noisy features in the signal.
#' The number of samples selected `k` selected by the algorithm cannot be
#' greater than the number of wavelengths.
#' @references
#' Honigs D.E., Hieftje, G.M., Mark, H.L. and Hirschfeld, T.B. 1985.
#' Unique-sample selection via Near-Infrared spectral substraction.
#' Analytical Chemistry, 57, 2299-2303
#'
#' @seealso \code{\link{kenStone}}, \code{\link{naes}}, \code{\link{duplex}},
#' \code{\link{shenkWest}}
#' @export
#'
honigs <- function(X, k, type = c("A", "R", "CR")) {
  if (missing(k)) {
    stop("'k' must be specified")
  }
  if (k < 2) {
    stop("'k' should be higher than 2")
  }
  if (ncol(X) < 2) {
    stop("'X' must have at least 2 columns")
  }
  if (k >= nrow(X) | k >= ncol(X)) {
    stop("'k' should be lower than nrow(X) or ncol(X)")
  }
  if (is.data.frame(X)) {
    X <- as.matrix(X)
  }

  type <- match.arg(type)
  if (type == "CR") {
    X <- 1 - X
  }
  # conversion to absorbance
  if (type == "R") {
    X <- -log10(X)
  }


  n <- nini <- 1:nrow(X)
  p <- 1:ncol(X)
  model <- rep(NA, k)
  psel <- rep(NA, k)
  # pdf('test.pdf')
  for (i in seq_along(model)) {
    aX <- abs(X)
    maxx <- max(aX)
    idx <- c(which(aX == maxx, arr.ind = TRUE))
    model[i] <- n[idx[1]]
    psel[i] <- p[idx[2]]
    n <- n[-idx[1]]
    weight <- X[, idx[2]] / X[idx[1], idx[2]] # weighting factor
    x <- t(X[idx[1], ] %o% weight)

    # matplot(t(X),type='l') lines(X[idx[1]],col=0,cex=1) abline(v=p[idx[2]])

    X <- X - x # subtraction
    p <- p[-idx[2]]
    X <- X[-idx[1], -idx[2]]
  }
  model <- model[!is.na(model)]
  psel <- psel[!is.na(psel)]
  return(list(model = model, test = nini[-model], bands = psel))
}
