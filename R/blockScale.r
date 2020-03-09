#' @title Hard or soft block scaling
#'
#' @description
#' Hard or soft block scaling of a spectral matrix to constant group variance.
#' In multivariate calibration, block scaling is used to down-weight variables, when one block of variables dominates other blocks.
#' With hard block scaling, the variables in a block are scaled so that the sum of their variances equals 1. Wen soft block scaling
#' is used, the variables are scaled such that the sum of variable variances is equal to the square root of the number of variables in a particular block.
#' @usage
#' blockScale(X, type = 'hard', sigma2 = 1)
#' @param X \code{data.frame} or \code{matrix} to transform
#' @param type type of block scaling: 'hard' or 'soft'
#' @param sigma2 desired total variance of a block (ie sum of the variances of all variables, default = 1), applicable when \code{type = 'hard'}
#' @return a \code{list} with \code{Xscaled}, the scaled matrix and \code{f}, the scaling factor
#' @author Antoine Stevens
#' @examples
#' X <- matrix(rnorm(100),ncol=10)
#' # Hard block scaling
#' res <- blockScale(X)
#' # sum of column variances == 1
#' apply(res$Xscaled, 2, var) 
#' @seealso \code{\link{blockNorm}}, \code{\link{standardNormalVariate}}, \code{\link{detrend}}
#' @references Eriksson, L., Johansson, E., Kettaneh, N., Trygg, J., Wikstrom, C., and Wold, S., 2006. Multi- and Megavariate Data Analysis. MKS Umetrics AB.
#' @export
#'
blockScale <- function(X, type = "hard", sigma2 = 1) {
    
    if (!class(X) %in% c("matrix", "data.frame")) 
        stop("X should be a matrix or data.frame")
    
    if (is.data.frame(X)) 
        X <- as.matrix(X)
    
    w <- apply(X, 2, sd)
    if (type == "soft") 
        f <- w * (length(w)^0.25) else f <- w * (length(w)^0.5)/sigma2^0.5
    list(Xscaled = t(t(X)/f), f = f)
} 
