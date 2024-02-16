#' @title k-means sampling
#' @description
#' Perform a k-means sampling on a matrix for multivariate calibration
#' @usage
#' naes(X, k, pc, iter.max = 10, method = 0, .center = TRUE, .scale = FALSE)
#' @param X a numeric matrix (optionally a data frame that can
#' be coerced to a numerical matrix).
#' @param k either the number of calibration samples to select or a set of
#' cluster centres to initiate the k-means clustering.
#' @param pc optional. If not specified, k-means is run directly on the variable
#' (Euclidean) space.
#' Alternatively, a PCA is performed before k-means and `pc` is the number of
#' principal components kept. If `pc < 1`,the number of principal components
#' kept corresponds to the number of components explaining at least (`pc * 100`)
#' percent of the total variance.
#' @param iter.max maximum number of iterations allowed for the k-means
#' clustering. Default is `iter.max = 10` (see `?kmeans`).
#' @param method the method used for selecting calibration samples within each
#' cluster: either samples closest to the cluster.
#' centers (`method = 0`, default), samples farthest away from the centre of the
#' data (`method = 1`) or
#' random selection (`method = 2`).
#' @param .center logical value indicating whether the input matrix must be
#' centered before Principal Component Analysis. Default set to \code{TRUE}.
#' @param .scale logical value indicating whether the input matrix must be
#' scaled before Principal Component Analysis. Default set to \code{FALSE}.
#' @return a list with components:
#' \itemize{
#'  \item{'`model`': numeric vector giving the row indices of the input data
#'  selected for calibration}
#'  \item{'`test`': numeric vector giving the row indices of the remaining
#'  observations}
#'  \item{'`pc`': if the `pc` argument is specified, a numeric matrix of the
#'  scaled pc scores}
#'  \item{'`cluster`': integer vector indicating the cluster to which each
#'  point was assigned}
#'  \item{'`centers`': a matrix of cluster centres}
#' }
#' @details K-means sampling is a simple procedure based on cluster analysis to
#' select calibration samples from large multivariate datasets.
#' The method can be described in three points (Naes et al.,2001):
#'
#' \enumerate{
#'  \item Perform a PCA and decide how many principal component to keep,
#'  \item Carry out a k-means clustering on the principal component scores and
#'  choose the number of resulting clusters to be equal to
#' the number of desired calibration samples,
#'  \item Select one sample from each cluster.
#' }
#' @references
#' Naes, T., 1987. The design of calibration in near infra-red reflectance
#' analysis by clustering. Journal of Chemometrics 1, 121-134.
#'
#' Naes, T., Isaksson, T., Fearn, T., and Davies, T., 2002. A user friendly
#' guide to multivariate calibration and classification. NIR Publications,
#' Chichester, United Kingdom.
#' @examples
#' data(NIRsoil)
#' sel <- naes(NIRsoil$spc, k = 5, p = .99, method = 0)
#' # clusters
#' plot(sel$pc[, 1:2], col = sel$cluster + 2)
#' # points selected for calibration with method = 0
#' points(sel$pc[sel$model, 1:2],
#'   col = 2,
#'   pch = 19,
#'   cex = 1
#' )
#' # pre-defined centers can also be provided
#' sel2 <- naes(NIRsoil$spc,
#'   k = sel$centers,
#'   p = .99, method = 1
#' )
#' # points selected for calibration with method = 1
#' points(sel$pc[sel2$model, 1:2],
#'   col = 1,
#'   pch = 15,
#'   cex = 1
#' )
#' @author Antoine Stevens & \href{https://orcid.org/0000-0002-5369-5120}{Leonardo Ramirez-Lopez}
#' @seealso \code{\link{kenStone}}, \code{\link{honigs}}, \code{\link{duplex}},
#' \code{\link{shenkWest}}
#' @export

naes <- function(X, k, pc, iter.max = 10, method = 0, .center = TRUE, .scale = FALSE) {
  if (is.data.frame(X)) {
    X <- as.matrix(X)
  }
  if (missing(k)) {
    stop("'k' must be a number or matrix")
  }

  if (ncol(X) < 2) {
    stop("'X' must have at least 2 columns")
  }

  if (!method %in% 0:2) {
    stop("'method' must be 0, 1 or 2")
  }

  if (!missing(pc)) {
    pca <- prcomp(X, center = .center, scale = .scale)
    if (pc < 1) {
      pvar <- pca$sdev^2 / sum(pca$sdev^2)
      pcsum <- cumsum(pvar) < pc
      if (any(pcsum)) {
        pc <- max(which(pcsum)) + 1
      } else {
        pc <- 1
      }
    }
    X <- sweep(pca$x[, 1:pc, drop = F], 2, pca$sdev[1:pc], "/") # scaling of the scores
  }

  if (length(k) > 1) {
    if (ncol(k) != ncol(X)) {
      stop("number of columns in 'k' must be equal to the number of columns in 'X'")
    }
    n <- nrow(k)
  } else {
    if (k < 2) {
      stop("'k' has to be higher than 2")
    }
    if (k >= nrow(X)) {
      stop("'k' must be lower than nrow(X)")
    }
    n <- k
  }

  kM <- kmeans(x = X, centers = k, iter.max = iter.max, nstart = 1)
  id <- 1:nrow(X)

  if (method == 0) {
    # select sample within each cluster the closest to the center of the cluster
    model <- rep(NA, n)
    for (i in 1:n) {
      idx <- kM$cluster == i
      d <- fastDistV(X[idx, , drop = F], kM$center[i, ], "euclid") # Euclidean distance to the centre of the cluster
      model[i] <- id[idx][which.min(d)]
    }
  } else if (method == 1) {
    # select sample within each cluster the farthest apart from the center of the data
    d <- fastDistV(X, colMeans(X), "euclid") # Euclidean distance to the centre
    model <- by(data.frame(id = id, d = d), kM$cluster, function(x) x$id[which.max(x$d)])
    attributes(model) <- NULL # delete attributes
  } else {
    # method==2 random sampling within each cluster
    model <- tapply(id, kM$cluster, function(x) sample(x, 1))
  }
  if (missing(pc)) {
    return(list(model = model, test = id[-model], cluster = kM$cluster, centers = kM$centers))
  } else {
    return(list(model = model, test = id[-model], pc = X, cluster = kM$cluster, centers = kM$centers))
  }
}
