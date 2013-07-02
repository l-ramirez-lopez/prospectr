#' @title k-means sampling
#' @description Perform a k-means sampling on a \code{matrix} or \code{data.frame} for multivariate calibration
#' @usage 
#' naes(X,k,pc,iter.max = 10, method = 0)
#' @param X numeric \code{matrix} or \code{data.frame}
#' @param k either the number of calibration samples to select or a set of cluster centres to initiate the k-means clustering. 
#' @param pc optional. If not specified, k-means is run directly on the variable (Euclidean) space. 
#' Alternatively, a PCA is performed before k-means and \code{pc} is the number of principal components kept. If \code{pc < 1}, 
#' the number of principal components kept corresponds to the number of components explaining at least (\code{pc * 100}) percent of the total variance.
#' @param iter.max maximum number of iterations allowed for the k-means clustering. Default is \code{iter.max = 10} (see \code{?kmeans})
#' @param method the method used for selecting calibration samples within each cluster: either samples closest to the cluster
#' centers (\code{method = 0}, default), samples farthest away from the centre of the data (\code{method = 1}) or
#' random selection (\code{method = 2})
#' @param .center logical value indicating whether the input matrix should be centered before Principal Component 
#' Analysis. Default set to TRUE.
#' @param .scale logical value indicating whether the input matrix should be scaled before Principal Component 
#' Analysis. Default set to FALSE.
#' @return a \code{list} with components:
#' \itemize{
#'  \item{"\code{model}"}{ numeric \code{vector} giving the row indices of the input data selected for calibration}
#'  \item{"\code{test}"}{ numeric \code{vector} giving the row indices of the remaining observations}
#'  \item{"\code{pc}"}{ if the \code{pc} argument is specified, a numeric \code{matrix} of the scaled pc scores}
#'  \item{"\code{cluster}"}{ integer vector indicating the cluster to which each point was assigned}
#'  \item{"\code{centers}"}{ a \code{matrix} of cluster centres}
#' }
#' @details K-means sampling is a simple procedure based on cluster analysis to select calibration samples from large multivariate datasets.
#' The method can be described in three points (Naes et al.,2001):
#' 
#' \enumerate{
#'  \item Perform a PCA and decide how many principal component to keep, 
#'  \item Carry out a k-means clustering on the principal component scores and choose the number of resulting clusters to be equal to
#' the number of desired calibration samples, 
#'  \item Select one sample from each cluster.
#' }
#' @references 
#' Naes, T., 1987. The design of calibration in near infra-red reflectance analysis by clustering. Journal of Chemometrics 1, 121-134. 
#' 
#' Naes, T., Isaksson, T., Fearn, T., and Davies, T., 2002. A user friendly guide to multivariate calibration and classification. NIR Publications, Chichester, United Kingdom.
#' @examples
#' data(NIRsoil) 
#' sel <- naes(NIRsoil$spc,k=5,p=.99,method=0)
#' plot(sel$pc[,1],sel$pc[,2],col=sel$cluster+2) # clusters
#' # points selected for calibration with method = 0
#' points(sel$pc[sel$model,1],sel$pc[sel$model,2],col=2,pch=19,cex=1)
#' sel2 <- naes(NIRsoil$spc,k=sel$centers,p=.99,method=1) # pre-defined centers can also be provided
#' # points selected for calibration with method = 1 
#' points(sel$pc[sel2$model,1],sel$pc[sel2$model,2],col=1,pch=15,cex=1) 
#' @author Antoine Stevens and Leonardo Ramirez-Lopez
#' @seealso \code{\link{kenStone}}, \code{\link{honigs}}, \code{\link{duplex}}, \code{\link{shenkWest}}
#' @export

naes <- function(X, k, pc, iter.max = 10, method = 0,.center=TRUE,.scale=TRUE){
    
  if(is.data.frame(X))
    X <- as.matrix(X)  
  if(missing(k))
    stop("'k' must be a number or matrix")
  method <- match.arg(method)
  if(ncol(X)<2)
    stop("'X' must have at least 2 columns")  
  
  if(!method %in% 0:2)
    stop("'method' should be 0, 1 or 2")
  
  if(!missing(pc)){
    pca <- prcomp(X,center=.center,scale=.scale)
    if(pc<1){
      pvar<- pca$sdev^2/sum(pca$sdev^2) 
      pcsum <- cumsum(pvar)<pc
      if(any(pcsum))
        pc <- max(which(pcsum))+1
      else 
        pc <- 1
    } 
      X <- sweep(pca$x[,1:pc,drop=F],2,pca$sdev[1:pc],"/")      # scaling of the scores
  }
  
  if(length(k)>1){
    if(ncol(k)!=ncol(X))
      stop("number of columns in 'k' must be equal to the number of columns in 'X'")
    n <- nrow(k)
  } else {
    if(k<2)
      stop("'k' has to be higher than 2")
    if (k >= nrow(X))
      stop("'k' should be lower than nrow(X)")    
    n <- k
  }
  
  kM <- kmeans(x = X, centers = k, iter.max = iter.max, nstart = 1)    
  id <- 1:nrow(X) 
  
  if(method==0){
    # select sample within each cluster the closest to the center of the cluster
    model <- rep(NA,n)
    for(i in 1:n){
      idx <- kM$cluster==i
      d <- fastDistV(X[idx,,drop=F],kM$center[i,],"euclid") # Euclidean distance to the centre of the cluster
      model[i] <- id[idx][which.min(d)]      
    }
  } else if (method==1){
    # select sample within each cluster the farthest apart from the center of the data
    d <- fastDistV(X,colMeans(X),"euclid") # Euclidean distance to the centre
    model <- by(data.frame(id=id,d=d),kM$cluster,function(x)x$id[which.max(x$d)])
    attributes(model) <- NULL # delete attributes
  } else { #method==2
    # random sampling within each cluster
    model <- tapply(id,kM$cluster,function(x)sample(x,1)) 
  }  
  if(missing(pc))  
    return(list(model = model, test = id[-model], cluster = kM$cluster, centers=kM$centers))  
  else
    return(list(model = model, test = id[-model], pc = X, cluster = kM$cluster, centers=kM$centers))  
}