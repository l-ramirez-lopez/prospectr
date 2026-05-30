#' @title Standard normal variate transformation
#'
#' @description
#' \loadmathjax
#' This function normalizes each row of an input matrix by
#' subtracting each row by its mean and dividing it by its standard deviation
#' @usage
#' standardNormalVariate(X)
#' @param X a numeric matrix of spectral data (optionally a data frame that can
#' be coerced to a numerical matrix). Optionally, a vector can be provided, 
#' in which case it will be treated as a single spectrum.
#' @author Antoine Stevens and Leonardo Ramirez-Lopez
#' @examples
#' data(NIRsoil)
#' NIRsoil$spc_snv <- standardNormalVariate(X = NIRsoil$spc)
#' # 10 first snv spectra
#' matplot(
#'   x = as.numeric(colnames(NIRsoil$spc_snv)),
#'   y = t(NIRsoil$spc_snv[1:10, ]),
#'   type = "l",
#'   xlab = "wavelength, nm",
#'   ylab = "snv"
#' )
#' \dontrun{
#' apply(NIRsoil$spc_snv, 1, sd) # check
#' }
#'
#' @return a matrix of normalized spectral data.
#' @details
#' SNV is simple way for normalizing spectral data that intends to correct for
#' light scatter.
#' It operates row-wise:
#'
#' \mjdeqn{SNV_i = \frac{x_i - \bar{x}_i}{s_i}}{SNV_i = \frac{x_i - \bar{x}_i}{s_i}}
#'
#' where \mjeqn{x_i}{x_i} is the signal of the \mjeqn{i}{i}th observation,
#' \mjeqn{\bar{x}_i}{\bar{x}_i} is its mean and \mjeqn{s_i}{s_i} its standard
#' deviation.
#' @seealso \code{\link{msc}}, \code{\link{detrend}}, \code{\link{blockScale}},
#' \code{\link{blockNorm}}
#' @references Barnes RJ, Dhanoa MS, Lister SJ. 1989. Standard normal variate
#' transformation and de-trending of near-infrared diffuse reflectance spectra.
#' Applied spectroscopy, 43(5): 772-777.
#' @export
standardNormalVariate <- function(X) {
  if (is.vector(X)) {
    X <- matrix(X, nrow = 1, dimnames = list(NULL, names(X)))
  }
  if (!any(class(X) %in% c("matrix", "data.frame"))) {
    stop("X must be a vector, matrix or optionally a data.frame")
  }
  X  <- as.matrix(X)
  mn <- rowMeans(X, na.rm = TRUE)
  X  <- X - mn
  # number of non-NA values per row for correct denominator
  n_obs <- rowSums(!is.na(X))
  sds   <- sqrt(rowSums(X^2, na.rm = TRUE) / (n_obs - 1))
  X / sds
}