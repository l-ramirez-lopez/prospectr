#' @title Kennard-Stone algorithm for calibration sampling
#' @description Select calibration samples from a large multivariate data using the Kennard-Stone algorithm
#' @usage 
#' kenStone(X,k,pc,group)
#' @param X a numeric \code{matrix} 
#' @param k number of desired calibration samples
#' @param pc optional. If not specified, the Euclidean distance is used. Alternatively, \code{pc} is the number of principal
#' components retained in the computation of the Mahalanobis distance. 
#' If \code{pc < 1}, the number of principal components kept corresponds to the number of components 
#' explaining at least (\code{pc * 100}) percent of the total variance.
#' @param group An optional \code{factor} (or vector that can be coerced to a factor by \code{\link{as.factor}}) of length
#' equal to nrow(X), giving the identifier of related observations (e.g. samples of the same batch of measurements, 
#' , of the same origin, or of the same soil profile). When one observation is selected by the procedure all observations
#'  of the same group are removed together and assigned to the calibration set. This allows to select calibration points
#'  that are independent from the remaining points.
#' @return a \code{list} with components:
#' \itemize{
#'  \item{"\code{model}"}{ numeric \code{vector} giving the row indices of the input data selected for calibration}
#'  \item{"\code{test}"}{ numeric \code{vector} giving the row indices of the remaining observations}
#'  \item{"\code{pc}"}{ if the \code{pc} argument is specified, a numeric \code{matrix} of the scaled pc scores}
#'  }
#' @references 
#' Kennard, R.W., and Stone, L.A., 1969. Computer aided design of experiments. Technometrics 11, 137-148.
#' @examples
#' data(NIRsoil) 
#' sel <- kenStone(NIRsoil$spc,k=30,pc=.99)
#' plot(sel$pc[,1],sel$pc[,2],xlab="PC1",ylab="PC2")
#' points(sel$pc[sel$model,1],sel$pc[sel$model,2],pch=19,col=2)  # points selected for calibration  
#' # Test on artificial data
#' X <- expand.grid(1:20,1:20) + rnorm(1e5,0,.1)
#' plot(X[,1],X[,2],xlab="VAR1",ylab="VAR2")
#' sel <- kenStone(X,k=25)
#' points(X[sel$model,1],X[sel$model,2],pch=19,col=2)
#' @author Antoine Stevens & Leonardo Ramirez-Lopez
#' @details 
#' The Kennard--Stone algorithm allows to select samples with a uniform distribution over the predictor space (Kennard and Stone, 1969).
#' It starts by selecting the pair of points that are the farthest apart.
#' They are assigned to the calibration set and removed from the list of points. Then, the procedure assigns remaining points to the calibration set
#' by computing the distance between each unassigned points \eqn{i_0} and selected points \eqn{i} and finding the point \eqn{i_0} for which:
#' \deqn{
#'   d_{selected} = \max\limits_{i_0}(\min\limits_{i}(d_{i,i_{0}}))
#' }
#' This essentially selects point \eqn{i_0} which is the farthest apart from its closest neighbors \eqn{i} in the calibration set.
#' The algorithm uses the Euclidean distance to select the points. However, the Mahalanobis distance can also be used. This
#' can be achieved by performing a PCA analysis on the input data and computing the Euclidean distance on the truncated score 
#' matrix according to the following definition of the Mahalanobis \eqn{H} distance:
#' 
#' \deqn{
#'    H^{2}_{ij}  =  \sum\limits_{a=1}^{A}{(\hat{t}_{ia}-\hat{t}_{ja})^{2}/\hat{\lambda}_{a}}
#' }
#' 
#' where \eqn{\hat{t}_{ia}} is the a^th principal component score of point \eqn{i}, \eqn{\hat{t}_{ja}} is the corresponding value for point \eqn{j}, 
#' \eqn{\hat{\lambda}_a} is the eigenvalue of principal component \eqn{a} and \eqn{A} is the number of principal components included in the computation.
#' @seealso  \code{\link{duplex}}, \code{\link{shenkWest}}, \code{\link{naes}}, \code{\link{honigs}}
#' @export
#' 
kenStone <- function(X,k,pc,group){
  
  if(missing(k))
    stop("'k' must be specified")
  if(ncol(X)<2)
    stop("'X' must have at least 2 columns")
  if(k<2)
    stop("Invalid argument: 'k' should be higher than 2")
  if(is.data.frame(X))   
    x <- X <- as.matrix(X)
  if(!missing(pc)){
    pca <- prcomp(X,center=T,scale=F)
    if(pc<1){
      pvar<- pca$sdev^2/sum(pca$sdev^2) 
      pcsum <- cumsum(pvar)<pc
      if(any(pcsum))
        pc <- max(which(pcsum))+1
      else 
        pc <- 1
    } 
    scores <- X <- sweep(pca$x[,1:pc,drop=F],2,pca$sdev[1:pc],"/")      # scaling of the scores
  }
  
  m <- nrow(X)  
  n <- 1:m
  
  if (k >= m) 
    k <- m-1
  
  if(!missing(group)){ 
    if(length(group)!=nrow(X))
      stop("length(group) should be equal to nrow(X)")
    if(!is.factor(group)) {
      group <- as.factor(group)
      warning("group has been coerced to a factor")  
    }
    if(nlevels(group) > k) 
      k <- nlevels(group) - 1
  }
  
  # Fist two most distant points to model set
  D <- fastDist(X,X,"euclid")
  id<- c(arrayInd(which.max(D),rep(m,2)))
  rm(X);gc()
  if(!missing(group)){
    id <- which(group %in% group[id])
    group <- group[-id]
  }
  
  model <- n[id]
  n <- n[-id]
  ini <- i <- length(model)
  
  while(i<k){        
    if(i == ini){ # first loop
      d <- D[model,-model]
      mins <- do.call(pmin.int,lapply(1:nrow(d),function(i)d[i,]))
    } else {
      d <- rbind(mins,D[nid,-model])
      mins <- do.call(pmin.int,lapply(1:nrow(d),function(i)d[i,]))
    }
    id <- which.max(mins) 
    
    if(!missing(group)){
      id <- which(group %in% group[id])
      group <- group[-id]
    }    
    
    nid <- n[id]
    model <- c(model,nid)
    n <- n[-id]        
    mins <- mins[-id]
    i <- length(model)
  } 
  
  if(missing(pc))
    return(list(model=model,test=(1:m)[-model]))
  else
    return(list(model=model,test=(1:m)[-model],pc=scores))
}
