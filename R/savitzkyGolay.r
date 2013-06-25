#' @title Savitzky-Golay transformation
#' @description
#' Savitzky-Golay smoothing and derivative of a data \code{matrix}, \code{data.frame} or \code{vector}.
#' @usage
#' savitzkyGolay(X,m,p,w,delta.wav)
#' @param X a numeric \code{data.frame}, \code{matrix} or \code{vector} to transform
#' @param m differentiation order
#' @param p polynomial order
#' @param w window size (must be odd)
#' @param delta.wav optional sampling interval
#' @author Antoine Stevens
#' @examples
#' data(NIRsoil)
#' spc <- 1/10^NIRsoil$spc # conversion to reflectance
#' opar <- par(no.readonly = TRUE)
#' par(mfrow=c(2,1),mar=c(4,4,2,2))
#' # plot of the 10 first spectra
#' matplot(as.numeric(colnames(spc)),t(spc[1:10,]),type="l",xlab="",ylab="Reflectance") 
#' mtext("Raw spectra") 
#' sg <- savitzkyGolay(X = spc,1,3,11,delta.wav=2)
#' matplot(as.numeric(colnames(sg)),t(sg[1:10,]),type="l",xlab="Wavelength /nm",ylab="1st derivative") 
#' mtext("1st derivative spectra")
#' par(opar)
#' @details
#' The Savitzky-Golay algorithm fits a local polynomial regression on the signal. It requires evenly spaced data points.
#' Mathematically, it operates simply as a weighted sum over a given window:
#' \deqn{ x_j\ast = \frac{1}{N}\sum_{h=-k}^{k}{c_hx_{j+h}}}
#' where \eqn{x_j\ast} is the new value, \eqn{N} is a normalizing coefficient,
#' \eqn{k} is the gap size on each side of \eqn{j} and \eqn{c_h}
#' are pre-computed coefficients, that depends on the chosen polynomial order and degree.
#' 
#' The sampling interval specified with the \code{delta.wav} argument is used for scaling and get numerically correct derivatives.
#' The convolution function is written in C++/Rcpp for faster computations.
#' @references Savitzky, A., and Golay, M.J.E., 1964. Smoothing and differentiation of data by simplified least squares procedures. Anal. Chem. 36, 1627-1639.
#' 
#' Wentzell, P.D., and Brown, C.D., 2000. Signal processing in analytical chemistry. Encyclopedia of Analytical Chemistry, 9764-9800.
#' @seealso \code{\link[signal]{sgolayfilt}}
#' @export
#'

savitzkyGolay <- function(X, m, p, w, delta.wav) {
    
    if (is.data.frame(X))
        X <- as.matrix(X)
        
    if (w%%2 != 1) 
        stop("needs an odd filter length w")
    if (p >= w) 
        stop("filter length w should be greater than polynomial order p")
        
    gap <- (w - 1)/2
    basis <- outer(-gap:gap, 0:p, "^")
    A <- solve(crossprod(basis, basis),tol=1e-1000) %*% t(basis)
    
    if(is.matrix(X)){
      if (w >= ncol(X)) 
        stop("filter length w should be lower than ncol(X)")
      output <- factorial(m) * convCppM(X, A[m + 1, ])
      g <- (w - 1)/2
      colnames(output) <- colnames(X)[(g + 1):(ncol(X) - g)]
      rownames(output) <- rownames(X)
    }
    
    if(is.vector(X)){
      if (w >= length(X)) 
        stop("filter length w should be lower than length(X)")
      output <- factorial(m) * convCppV(X, A[m + 1, ])
      g <- (w - 1)/2
      names(output) <- names(X)[(g + 1):(length(X) - g)]
    }   
    
    if (!missing(delta.wav)) # scaling
        output <- output/delta.wav^m

    return(output)
} 
