#' @title Splice correction of a spectral matrix acquired with an ASD spectrometer
#' @description
#' Corrects steps in an input spectral matrix by linear interpolation of the values of the edges of the middle sensor
#' @usage
#' spliceCorrection(X,wav,splice=c(1000,1830),interpol.bands=10)
#' @param X numeric \code{data.frame}, \code{matrix} or \code{vector} to transform
#' @param wav numeric \code{vector} with band positions
#' @param splice numeric \code{vector} of the two positions of the splices, default = c(1000,1830)
#' corresponding to the splices of the ASD FieldSpec Pro spectrometer.
#' @param interpol.bands number of interpolation bands
#' @return a \code{matrix} with the splice corrected data
#' @author Antoine Stevens
#' @details
#' Spectra acquired with an ASD FieldSpec Pro spectroradiometer usually exhibit steps at the splice of the three built-in sensors,
#' positioned at 1000 and 1830 nm.
#' @export

spliceCorrection <- function(X, wav, splice = c(1000, 1830), interpol.bands = 10){
  
  if (is.data.frame(X)) 
        X <- as.matrix(X)
    
    was.vec <- is.vector(X)
    if (is.vector(X)){
        nms <- names(X)
        X <- matrix(X,ncol=length(X))        
    }
    if (missing(wav)) 
        wav <- seq_len(ncol(X))
    if (length(wav) != ncol(X)) 
        stop("length(wav) should be equal to ncol(X)")
    
    index <- which(wav %in% splice)
    
    X1 <- X[, 1:index[1],drop=F]
    X2 <- X[, (index[1] + 1):index[2],drop=F]
    X3 <- X[, (index[2] + 1):ncol(X),drop=F]
    
    tmp1 <- X2[, 1:interpol.bands,drop=F]
    tmp2 <- X2[, (ncol(X2) - interpol.bands + 1):ncol(X2),drop=F]
    
    w1 <- wav[(index[1] + 1):(index[1] + interpol.bands)]
    w2 <- wav[(index[2] - interpol.bands + 1):index[2]]
    
    extrapfun <- function(x, y, xout) {
        fit <- lm(y ~ x)
        fit$coefficients[1] + fit$coefficients[2] * xout
    }
    
    pred.X1 <- apply(tmp1, 1, function(y) extrapfun(x = w1, y = y, xout = splice[1]))
    pred.X2 <- apply(tmp2, 1, function(y) extrapfun(x = w2, y = y, xout = splice[2]))
      
    offset1 <- X1[, ncol(X1)] - pred.X1
    offset2 <- X3[, 1] - pred.X2
    output <- cbind(sweep(X1, 1, offset1, "-"), X2, sweep(X3, 1, offset2, "-"))
    if(was.vec) {
        output <- as.vector(output)
        names(output) <- nms
     } else {       
        dimnames(output) <- list(rownames(X),colnames(X))
     }
    return(output)
} 
