#' @title Detrend transformation
#' @description
#' Normalizes each row of an input \code{data.frame} or \code{matrix} by applying a SNV transformation
#' followed by fitting a second order linear model and returning the fitted residuals.
#' @usage
#' detrend(X,wav,parallel=FALSE)
#' @param X numeric \code{data.frame}, \code{matrix} or \code{vector} to process
#' @param wav wavelengths/ band centers
#' @param parallel logical value. if \code{TRUE}, apply the function in parallel using the parallel backend provided by foreach
#' @author Antoine Stevens
#' @examples
#' data(NIRsoil)
#' wav <- as.numeric(colnames(NIRsoil$spc))
#' spc <- 1/10^NIRsoil$spc # conversion to reflectance
#' opar <- par(no.readonly = TRUE)
#' par(mfrow=c(2,1),mar=c(4,4,2,2))
#' matplot(wav,t(spc[1:10,]),type="l",xlab="",ylab="Reflectance") # plot of the 10 first spectra
#' mtext("Raw spectra")
#' det <- detrend(spc,wav)
#' matplot(wav,t(det[1:10,]),type="l",xlab="Wavelength /nm",ylab="Reflectance") 
#' mtext("Detrend spectra")
#' par(opar)
#' @details The detrend is a row-wise transformation that allows to correct for wavelength-dependent 
#' scattering effects (variations in curvilinearity). A second-degree polynomial is fit through each spectrum:
#' \deqn{x_i = a\lambda^2 + b\lambda + c + e_i}
#' were \eqn{x_i} is the spectrum, \eqn{\lambda} is the wavelength vector, a, b, c are estimated by least square, 
#' and \eqn{e_i} are the residuals of the least square fit. Then, a detrend spectrum corresponds to:
#' \deqn{x\ast_i = x_i - (a\lambda^2 + b\lambda + c = e_i)}
#' @seealso \code{\link{standardNormalVariate}}, \code{\link{blockScale}}, \code{\link{blockNorm}}
#' @references Barnes RJ, Dhanoa MS, Lister SJ. 1989. Standard normal variate transformation and de-trending of near-infrared diffuse reflectance spectra. Applied spectroscopy, 43(5): 772-777.
#' @return a \code{matrix} or \code{vector} with detrend values
#' @export
#'
detrend <- function(X, wav, parallel = FALSE) {
    require(foreach);require(iterators)
    if(missing(wav))
        stop("argument wav should be specified")
  
    if (is.data.frame(X)) 
        X <- as.matrix(X)
    
    was.vec <- is.vector(X)
    
    if (is.vector(X)){
         nms <- names(X)
         X <- matrix(X,ncol=length(X))        
    }
    
    xpoly <- stats:::poly(wav,2)
    # SNV transformation
    X <- sweep(X, 1, rowMeans(X), "-")
    X <- sweep(X, 1, apply(X, 1, sd), "/")
    
    if (parallel) {
        # copy-pasted from plyr:::llply
        if (!require("foreach") & require("iterators")) {
            stop("foreach and iterators package required for parallel operation", call. = FALSE)
        }
        if (getDoParWorkers() == 1) {
            warning("No parallel backend registered", call. = TRUE)
        }
        output <- foreach(y = iter(X, by = "row"), .combine = "rbind") %dopar% lm.fit(x= xpoly,as.vector(y))$residuals  # get the residuals
    } else {        
        # get the residuals
        output <- t(apply(X, 1, function(y) lm.fit(x= xpoly,y)$residuals))
    }
    
    if(was.vec) {
        output <- as.vector(output)
        names(output) <- nms
    } else {       
        dimnames(output) <- list(rownames(X),colnames(X))
    }
    return(output)
} 
