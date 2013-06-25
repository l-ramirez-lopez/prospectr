#' @title Signal binning
#' @description
#' Compute average values of a signal in pre-determined bins (col-wise subsets).
#' The bin size can be determined either directly or by specifying the number of bins.
#' Sometimes called boxcar transformation in signal processing
#' @usage
#' binning(X,bins,bin.size)
#' @param X numeric \code{data.frame}, \code{matrix} or \code{vector} to process
#' @param bins number of bins
#' @param bin.size desired size of the bins
#' @author Antoine Stevens & Leonardo Ramirez-Lopez
#' @examples
#' data(NIRsoil)
#' spc <- 1/10^NIRsoil$spc # conversion to reflectance
#' wav <- as.numeric(colnames(spc))
#' matplot(wav,t(spc[1:5,]),type="l",xlab="Wavelength /nm",ylab="Reflectance") # 5 first spectra
#' binned <- binning(spc,bin.size=20)
#' matpoints(as.numeric(colnames(binned)),t(binned[1:5,]),pch=1:5) # bin means
#' binned <- binning(spc,bins=20) 
#' dim(binned) # 20 bins
#' matplot(wav,t(spc[1:5,]),type="l",xlab="Wavelength /nm",ylab="Reflectance") # 5 first spectra
#' matpoints(as.numeric(colnames(binned)),t(binned[1:5,]),pch=1:5) # bin means
#' @return a \code{matrix} or \code{vector} with average values per bin
#' @seealso \code{\link[signal]{sgolayfilt}}, \code{\link{savitzkyGolay}}, \code{\link{movav}}, \code{\link{gapDer}}, 
#' \code{\link{continuumRemoval}}
#' @export
#'
binning <- function(X, bins, bin.size) {
    
    if (is.data.frame(X)) 
        X <- as.matrix(X)
    if (!missing(bins) & !missing(bin.size)) 
        stop("EITHER 'bins' OR 'bin.size' must be specified")
    if (missing(bins) & missing(bin.size)) 
        return(X)
    
    if(is.matrix(X))
        p1 <- ncol(X)
    else 
        p1 <- length(X)
    
    if (missing(bins) & !missing(bin.size)) {
        b <- findInterval(1:p1, seq(1, p1, bin.size))
    } else {
        b <- findInterval(1:p1, seq(1, p1, length.out = bins + 1), rightmost.closed = T)
    }
    
    p2 <- max(b)
    
    if(is.matrix(X)){
        output <- matrix(0, nrow(X), p2)    
        for (i in seq_len(p2)) {
            output[, i] <- rowMeans(X[, b == i, drop = F])
        }
        colnames(output) <- colnames(X)[ceiling(tapply(b, b, function(x) mean(which(b == x[1]),na.rm=T)))]  # find colnames
        rownames(output) <- rownames(X)
    } else {
       output <- tapply(X,b,mean)
       names(output) <- names(X)[ceiling(tapply(b, b, function(x) mean(which(b == x[1]),na.rm=T)))]
    }
    
    return(output)
} 
