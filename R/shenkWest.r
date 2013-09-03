#' @title SELECT algorithm for calibration sampling
#'
#' @description
#' Select calibration samples from a large multivariate data using the SELECT algorithm as described in Shenk and Westerhaus (1991).
#' @usage
#' shenkWest(X,d.min=0.6,pc=0.95,rm.outlier=FALSE,.center = TRUE,.scale = FALSE)
#' @param X numeric \code{data.frame} or \code{matrix} 
#' @param d.min minimum distance (default = 0.6)
#' @param pc number of principal components retained in the computation distance in the standardized Principal Component space (Mahalanobis distance).
#' If \code{pc < 1}, the number of principal components kept corresponds to the number of components 
#' explaining at least (\code{pc * 100}) percent of the total variance (default = 0.95).
#' @param rm.outlier logical value. if \code{TRUE}, remove observations with a standardized mahalanobis distance
#' to the center of the data greater than 3 (default = \code{FALSE})
#' @param .center logical value indicating whether the input matrix should be centered before Principal Component 
#' Analysis. Default set to TRUE.
#' @param .scale logical value indicating whether the input matrix should be scaled before Principal Component 
#' Analysis. Default set to FALSE.
#' @author Antoine Stevens
#' @return a \code{list} with components:
#' \itemize{
#'  \item{'\code{model}'}{ numeric \code{vector} giving the row indices of the input data selected for calibration}
#'  \item{'\code{test}'}{ numeric \code{vector} giving the row indices of the remaining observations}
#'  \item{'\code{pc}'}{a numeric \code{matrix} of the scaled pc scores}
#' }
#' @details
#' The SELECT algorithm is an iterative procedure based on the standardized Mahalanobis distance between observations. 
#' First, the observation having the highest number of neighbours within a given minimum distance is selected and its neighbours
#' are discarded. The procedure is repeated until there is no observation left.
#' 
#' If the \code{rm.outlier} argument is set to \code{TRUE}, outliers will be removed before running the SELECT algorithm,
#' using the CENTER algorithm of Shenk and Westerhaus (1991), i.e. samples with a standardized Mahalanobis distance \code{>3} are removed.
#' @examples
#' data(NIRsoil)
#' sel <- shenkWest(NIRsoil$spc,pc=.99,d.min=.3,rm.outlier=FALSE)
#' plot(sel$pc[,1:2],xlab='PC1',ylab='PC2')
#' points(sel$pc[sel$model,1:2],pch=19,col=2)  # points selected for calibration 
#' # without outliers
#' sel <- shenkWest(NIRsoil$spc,pc=.99,d.min=.3,rm.outlier=TRUE)
#' plot(sel$pc[,1:2],xlab='PC1',ylab='PC2')
#' points(sel$pc[sel$model,1:2],pch=15,col=3)  # points selected for calibration 
#' @references Shenk, J.S., and Westerhaus, M.O., 1991. Population Definition, Sample Selection, and Calibration Procedures for Near Infrared Reflectance Spectroscopy. Crop Science 31, 469-474.
#' @seealso \code{\link{kenStone}}, \code{\link{duplex}}, \code{\link{puchwein}}
#' @export
#' 

shenkWest <- function(X, d.min = 0.6, pc = 0.95, rm.outlier = FALSE, .center = TRUE, .scale = FALSE) {
    
    if (is.data.frame(X)) 
        X <- as.matrix(X)
    
    # Compute scores of PCA
    pca <- prcomp(X, center = .center, scale = .scale)
    if (pc < 1) {
        pvar <- pca$sdev^2/sum(pca$sdev^2)
        pcsum <- cumsum(pvar) < pc
        if (any(pcsum)) 
            pc <- max(which(pcsum)) + 1 else pc <- 1
    }
    scores.ini <- scores <- sweep(pca$x[, 1:pc, drop = F], 2, pca$sdev[1:pc], "/")  # scaling of the scores  
    
    n <- nini <- 1:nrow(X)
    model <- NULL
    
    if (rm.outlier) {
        m <- fastDistV(scores, colMeans(scores), "euclid")  # squared mahalanobis distance 
        m <- m/pc  # standardized mahalanobis distance (also called GH, Global H distance)
        idx <- m <= 3
        scores <- scores[idx, ,drop=F]  # remove samples with H > 3
        n <- n[idx]
    }
    
    d <- fastDist(scores, scores, "euclid")  # NH - Neighbour mahalanobis H distance
    d <- d/pc  # standardized mahalanobis distance 
    d <- d < d.min  # distance treshold
    
    while (ncol(d) > 1) {
        idx <- which.max(colSums(d))
        knn <- which(d[, idx])
        if (length(knn) < 2) 
            break
        model <- c(model, n[idx])
        n <- n[-knn]
        d <- d[-knn, -knn, drop = F]
    }
    
    return(list(model = model, test = nini[-n], pc = scores.ini))
} 
