#' @title Continuum Removal
#' @description
#' Compute the continuum removed values of a data \code{matrix}, \code{data.frame}, or \code{vector} as implemented in ENVI
#' @usage
#' continuumRemoval(X,wav,type,interpol,method,parallel=FALSE)
#' @param X numeric \code{data.frame}, \code{matrix} or \code{vector} to process
#' @param wav optional. numeric \code{vector} of band positions
#' @param type type of data: 'R' for reflectance (default), 'A' for absorbance
#' @param interpol interpolation method between points on the convex hull: 'linear' (default) or 'spline'
#' @param method normalization method: 'division' (default) or 'substraction' (see details section)
#' @param parallel logical value. if \code{TRUE}, apply the function in parallel using the parallel backend 
#' provided by \code{\link{foreach}}
#' @author Antoine Stevens
#' @return a \code{matrix} or \code{vector} with the filtered signal(s)
#' @examples
#' data(NIRsoil)
#' wav <- as.numeric(colnames(NIRsoil$spc)) 
#' # plot of the 10 first abs spectra
#' matplot(wav,t(NIRsoil$spc[1:10,]),type="l",ylim=c(0,.6),xlab="Wavelength /nm",ylab="Abs") 
#' # type = 'A' is used for absorbance spectra
#' cr <- continuumRemoval(NIRsoil$spc,wav,type="A") 
#' matlines(wav,t(cr[1:10,])) 
#' @seealso \code{\link[signal]{sgolayfilt}}, \code{\link{savitzkyGolay}}, \code{\link{movav}}, \code{\link{gapDer}}, 
#' \code{\link{binning}}
#' @details  The continuum removal technique was introduced by Clark and Roush (1984)
#' as a method to highlight energy absorption features of minerals. 
#' It can be viewed as a way to perform albedo normalization.
#' The algorithm find points lying on the convex hull (local maxima or envelope) of a spectrum, 
#' connects the points by linear or spline interpolation and
#' normalizes the spectrum by dividing (or substracting) the input data by the interpolated line.
#' @references Clark, R.N., and Roush, T.L., 1984. Reflectance Spectroscopy: Quantitative Analysis Techniques for Remote Sensing Applications. J. Geophys. Res. 89, 6329-6340.
#' @export

continuumRemoval <- function(X, wav, type = c("R", "A"), interpol = c("linear", "spline"), method = c("division", "substraction"), 
    parallel = FALSE) {
   require(foreach);require(iterators)
   if (is.data.frame(X)) 
        X <- as.matrix(X)
      
    type <- match.arg(type)
    interpol <- match.arg(interpol)
    method <- match.arg(method)
    
    if (type == "A") 
        X <- 1/X
    
    crfun <- function(x, wav, interpol) {
        id <- sort(chull(c(wav[1] - 1, wav, wav[length(wav)] + 1), c(0, x, 0)))
        id <- id[-c(1, length(id))] - 1
        cont <- switch(interpol, linear = {
            approx(x = wav[id], y = x[id], xout = wav, method = "linear")$y
        }, spline = {
            splinefun(x = wav[id], y = x[id])(wav)
        })
        return(cont)
    }
    
    if(is.matrix(X)){
      if (missing(wav)) 
        wav <- seq_len(ncol(X))
      if (length(wav) != ncol(X)) 
        stop("length(wav) should be equal to ncol(X)")
      
      
      if (parallel) {
          # copy-pasted from plyr:::llply
          if (!require("foreach") & require("iterators")) {
              stop("foreach and iterators package required for parallel operation", call. = FALSE)
          }
          if (getDoParWorkers() == 1) {
              warning("No parallel backend registered", call. = TRUE)
          }
          cont <- foreach(i = iter(X, by = "row")) %dopar% crfun(i, wav, interpol)
          cont <- do.call(rbind, cont)
      } else {
          cont <- t(apply(X, 1, function(x) crfun(x, wav, interpol)))
      }
      
    } else {
        cont <- crfun(X,wav,interpol)
    }
    
    
    if (method == "division") 
        cr <- X/cont # like ENVI
    else 
        cr <- 1 + X - cont    
    
    if (type == "A") 
        cr <- 1/cr - 1
   
    if(is.matrix(X)){
        colnames(cr) <- wav
        rownames(cr) <- rownames(X)
    } else {
        names(cr) <- wav
    }
    
    return(cr)
} 
