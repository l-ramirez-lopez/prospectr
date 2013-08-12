#' @title Sum of squares block weighting
#' @description
#' Sum of squares block weighting: allows to scale blocks of variables, 
#' but keeping the relative weights of the variables inside a block.
#' @usage
#' blockNorm(X,targetnorm=1)
#' @param X \code{data.frame} or \code{matrix} to transform
#' @param targetnorm desired sum of squares for a block of variables (default = 1)
#' @return a \code{list} with components \code{Xscaled}, the scaled matrix and \code{f}, the scaling factor
#' @author Antoine Stevens
#' @examples
#' X <- matrix(rnorm(100),ncol=10)
#' # Block normalize to sum of square = 1
#' res <- blockNorm(X,1)
#' sum(res$Xscaled^2) # check
#' @seealso \code{\link{blockScale}}, \code{\link{standardNormalVariate}}, \code{\link{detrend}}
#' @references Eriksson, L., Johansson, E., Kettaneh, N., Trygg, J., Wikstrom, C., and Wold, S., 2006. Multi- and Megavariate Data Analysis. MKS Umetrics AB.
#' @details The function computes a scaling factor, which, multiplied by the input \code{matrix},
#' produces a \code{matrix} with a pre--determined sum of squares.
#' @note
#' This is a \R port of the \file{MBnorm.m} function of the MB matlab toolbox by Fran van den Berg (\url{http://www.models.life.ku.dk/~courses/MBtoolbox/mbtmain.htm})
#' @export
blockNorm <- function(X, targetnorm = 1) {
    
    if (!class(X) %in% c("matrix", "data.frame")) 
        stop("X should be a matrix or data.frame")
    
    if (is.data.frame(X)) 
        X <- as.matrix(X)
    
    if (targetnorm == 1) {
        f <- sum(X^2, na.rm = T)^0.5
    } else {
        fmax <- sum(X^2, na.rm = T)
        fmaxn <- sum((X/fmax)^2, na.rm = T)
        if (fmaxn > targetnorm) {
            fmin <- fmax
            fminn <- fmaxn
            while (fminn > targetnorm) {
                fmin <- fmin * 10
                fminn <- sum((X/fmin)^2, na.rm = T)
            }
        } else {
            fmin <- fmax
            fminn <- fmaxn
            while (fmaxn < targetnorm) {
                fmax <- fmax/10
                fmaxn <- sum((X/fmax)^2, na.rm = T)
            }
        }
        n <- fmaxn
        while ((targetnorm - n)^2 > 1e-12) {
            f <- (fmin + fmax)/2
            n <- sum((X/f)^2, na.rm = T)
            if (n > targetnorm) 
                fmax <- f else fmin <- f
        }
    }
    Xn <- X/f
    list(Xscaled = Xn, f = f)
} 
