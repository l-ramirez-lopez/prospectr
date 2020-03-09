#' @title Resample spectral data
#' @description
#' Resample a data \code{matrix}, \code{data.frame} or \code{vector} to new coordinates (e.g. band positions)
#' using spline or linear interpolation. This function is a simple wrapper around \code{\link{approx}}
#' and \code{\link{splinefun}} in \pkg{base}.
#' @usage
#' resample(X, wav, new.wav, interpol)
#' @param X numeric \code{data.frame}, \code{matrix} or \code{vector} to resample
#' @param wav a numeric vector giving the original band positions
#' @param new.wav a numeric vector giving the new band positions
#' @param interpol interpolation method: 'linear' or 'spline'
#' @author Antoine Stevens
#' @examples
#' data(NIRsoil)
#' wav <- as.numeric(colnames(NIRsoil$spc))
#' spc <- 1/10^NIRsoil$spc # conversion to reflectance
#' resampled <- resample(spc, wav, seq(1100, 2498, 2)) # increase spectral resolution by 2
#' dim(spc)
#' dim(resampled)
#' @return a \code{matrix} or \code{vector} with resampled values
#' @seealso \code{\link{resample2}}
#' @export
#'
resample <- function(X, wav, new.wav, interpol = c("linear", "spline")) {
    
    if (is.data.frame(X)) 
        X <- as.matrix(X)
    if (missing(wav)) 
        stop("wav argument should be specified")
    
    interpol <- match.arg(interpol)
    
    resfun <- function(x, interpol) {
        if (interpol == "linear") {
            approx(x = wav, y = x, xout = new.wav, method = "linear")$y
        } else {
            splinefun(x = wav, y = x)(new.wav)
        }
    }
    
    if (is.matrix(X)) {
        if (length(wav) != ncol(X)) 
            stop("length(wav) should be equal to ncol(X)")
        
        output <- t(apply(X, 1, resfun, interpol))
        rownames(output) <- rownames(X)
        colnames(output) <- new.wav
    } else {
        if (length(wav) != length(X)) 
            stop("length(wav) should be equal to length(X)")
        output <- resfun(X, interpol)
        names(output) <- new.wav
    }
    
    return(output)
} 
