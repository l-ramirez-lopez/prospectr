#' @title Signal binning
#' @description
#' Compute average values of a signal in pre-determined bins (col-wise subsets).
#' The bin size can be determined either directly or by specifying the number of
#' bins. Sometimes called boxcar transformation in signal processing
#' @usage
#' binning(X, bins, bin.size)
#' @param X a numeric matrix or vector to process (optionally a data frame that
#' can be coerced to a numerical matrix).
#' @param bins the number of bins.
#' @param bin.size the desired size of the bins.
#' @author Antoine Stevens & \href{https://orcid.org/0000-0002-5369-5120}{Leonardo Ramirez-Lopez}
#' @examples
#' data(NIRsoil)
#' wav <- as.numeric(colnames(NIRsoil$spc))
#'
#' # 5 first spectra
#' matplot(wav, t(NIRsoil$spc[1:5, ]),
#'   type = "l",
#'   xlab = "Wavelength /nm",
#'   ylab = "Absorbance"
#' )
#'
#' NIRsoil$spc_binned <- binning(NIRsoil$spc, bin.size = 20)
#'
#' # bin means
#' matpoints(as.numeric(colnames(NIRsoil$spc_binned)),
#'   t(NIRsoil$spc_binned[1:5, ]),
#'   pch = 1:5
#' )
#'
#' NIRsoil$spc_binned <- binning(NIRsoil$spc, bins = 20)
#' dim(NIRsoil$spc_binned) # 20 bins
#'
#' # 5 first spectra
#' matplot(wav,
#'   t(NIRsoil$spc[1:5, ]),
#'   type = "l",
#'   xlab = "Wavelength /nm",
#'   ylab = "Absorbance"
#' )
#'
#' # bin means
#' matpoints(as.numeric(colnames(NIRsoil$spc_binned)),
#'   t(NIRsoil$spc_binned[1:5, ]),
#'   pch = 1:5
#' )
#' @return
#' a matrix or vector with average values per bin.
#' @seealso
#' \code{\link{savitzkyGolay}}, \code{\link{movav}},
#' \code{\link{gapDer}}, \code{\link{continuumRemoval}}
#' @importFrom stats aggregate
#' @export
#'

binning <- function(X, bins, bin.size) {
  if (is.data.frame(X)) {
    X <- as.matrix(X)
  }
  if (!missing(bins) & !missing(bin.size)) {
    stop("either 'bins' or 'bin.size' must be specified")
  }
  if (missing(bins) & missing(bin.size)) {
    return(X)
  }

  if (is.matrix(X)) {
    nv <- ncol(X)
  } else {
    nv <- length(X)
  }


  if (missing(bins) & !missing(bin.size)) {
    b <- findInterval(
      1:nv,
      seq(1, nv, bin.size),
      left.open = FALSE
    )
  } else {
    bins <- bins + 1
    b <- findInterval(
      1:nv,
      round(seq(1, nv, length.out = bins), 3), # round to 3 to avoid the famous floating math imprecision bug of R
      rightmost.closed = TRUE,
      left.open = FALSE
    )
  }

  n_classes <- max(b)

  if (is.matrix(X)) {
    output <- matrix(0, nrow(X), n_classes)

    # for (i in seq_len(n_classes)) {
    #   output[, i] <- rowMeans(X[, b == i, drop = F])
    # }

    output <- aggregate(t(X), by = list(bin = b), FUN = mean)
    output <- t(output[order(output[, 1]), -1])
    colnames(output) <- colnames(X)[ceiling(tapply(b, b, function(x) mean(which(b == x[1]), na.rm = TRUE)))] # find colnames
    rownames(output) <- rownames(X)
  } else {
    output <- tapply(X, b, mean)
    names(output) <- names(X)[ceiling(tapply(b, b, function(x) mean(which(b == x[1]), na.rm = TRUE)))]
  }

  return(output)
}
