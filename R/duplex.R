#' @title DUPLEX algorithm for calibration sampling
#' @description
#' Select calibration samples from a large multivariate data using the DUPLEX
#' algorithm
#' @usage
#' duplex(X,
#'        k,
#'        metric = c("mahal", "euclid"),
#'        pc,
#'        group,
#'        .center = TRUE,
#'        .scale = FALSE)
#' @param X a numeric matrix.
#' @param k the number of calibration/validation samples.
#' @param metric the distance metric to be used: 'euclid' (Euclidean distance)
#' or 'mahal' (Mahalanobis distance, default).
#' @param pc optional. The number of Principal Components to be used to select
#' the samples. If not specified, distance are computed in the Euclidean space.
#' Alternatively, distances are computed in the principal component space and
#' `pc` is the number of principal components retained.
#' If `pc < 1`, the number of principal components kept corresponds to the
#' number
#' of components explaining at least (`pc * 100`) percent of the total variance.
#' @param group An optional `factor` (or vector that can be coerced to a factor
#' by \code{\link{as.factor}}) of length equal to nrow(X), giving the identifier
#' of related observations (e.g. samples of the same batch of measurements,
#' samples of the same origin, or of the same soil profile). When one
#' observation is
#' selected by the procedure all observations of the same group are removed
#' together and assigned to the calibration/validation sets. This allows to
#' select calibration and validation samples that are independent from each
#' other.
#' @param .center logical value indicating whether the input matrix must be
#' centered before projecting `X` onto the Principal Component space.
#' Analysis. Default set to \code{TRUE}.
#' @param .scale logical value indicating whether the input matrix must be
#' scaled before `X` onto the Principal Component space.
#' Analysis. Default set to \code{FALSE}.
#' @return a `list` with components:
#' \itemize{
#'  \item{'`model`': numeric vector giving the row indices of the input data
#'  selected for calibration}
#'  \item{'`test`': numeric vector giving the row indices of the input data
#'  selected for validation}
#'  \item{'`pc`': if the `pc` argument is specified, a numeric matrix of the
#'  scaled pc scores}
#' }
#' @references
#' Kennard, R.W., and Stone, L.A., 1969. Computer aided design of experiments.
#' Technometrics 11, 137-148.
#'
#' Snee, R.D., 1977. Validation of regression models: methods and examples.
#' Technometrics 19, 415-428.
#' @details
#' The DUPLEX algorithm is similar to the Kennard-Stone algorithm (see
#' \code{\link{kenStone}}) but allows to select both calibration and validation
#' points that are independent. Similarly to the Kennard-Stone algorithm,
#' it starts by selecting the pair of points that are the farthest apart. They
#' are assigned to the calibration sets and removed from the list of points.
#' Then, the next pair of points which are farthest apart are assigned to the
#' validation sets and removed from the list. In a third step, the procedure
#' assigns each remaining point alternatively to the calibration
#' and validation sets based on the distance to the points already selected.
#' Similarly to the Kennard-Stone algorithm, the default distance metric used
#' by the procedure is the Euclidean distance, but the Mahalanobis distance can
#' be used as well using the `pc` argument (see \code{\link{kenStone}}).
#'
#' @author Antoine Stevens & \href{https://orcid.org/0000-0002-5369-5120}{Leonardo Ramirez-Lopez}
#' @examples
#' data(NIRsoil)
#' sel <- duplex(NIRsoil$spc, k = 30, metric = "mahal", pc = .99)
#' plot(sel$pc[, 1:2], xlab = "PC1", ylab = "PC2")
#' points(sel$pc[sel$model, 1:2], pch = 19, col = 2) # points selected for calibration
#' points(sel$pc[sel$test, 1:2], pch = 18, col = 3) # points selected for validation
#' # Test on artificial data
#' X <- expand.grid(1:20, 1:20) + rnorm(1e5, 0, .1)
#' plot(X[, 1], X[, 2], xlab = "VAR1", ylab = "VAR2")
#' sel <- duplex(X, k = 25, metric = "mahal")
#' points(X[sel$model, ], pch = 19, col = 2) # points selected for calibration
#' points(X[sel$test, ], pch = 15, col = 3) # points selected for validation
#' @seealso
#' \code{\link{kenStone}}, \code{\link{honigs}}, \code{\link{shenkWest}},
#' \code{\link{naes}}
#' @export

duplex <- function(X,
                   k,
                   metric = c("mahal", "euclid"),
                   pc,
                   group,
                   .center = TRUE,
                   .scale = FALSE) {
  if (missing(k)) {
    stop("'k' must be specified")
  }
  if (ncol(X) < 2) {
    stop("'X' must have at least 2 columns")
  }
  if (k < 2) {
    stop("Invalid argument: 'k' must be higher than 2")
  }
  metric <- match.arg(metric)
  if (is.data.frame(X)) {
    x <- X <- as.matrix(X)
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
    scores <- X <- pca$x[, 1:pc, drop = F]
  }
  
  if (metric == "mahal") {
    # Project in the Mahalanobis distance space
    X <- e2m(X, sm.method = "svd")
    if (!missing(pc)) {
      scores <- X
    }
  }
  
  m <- nrow(X)
  n <- 1:nrow(X)
  half <- floor(m / 2)
  if (k > half) {
    k <- half
  }
  
  if (!missing(group)) {
    if (length(group) != nrow(X)) {
      stop("length(group) must be equal to nrow(X)")
    }
    if (!is.factor(group)) {
      group <- as.factor(group)
      warning("group has been coerced to a factor")
    }
  }
  
  # Fist two most distant points to model set
  D <- fastDist(X, X, "euclid")
  id <- c(arrayInd(which.max(D), rep(m, 2)))
  
  if (!missing(group)) {
    id <- which(group %in% group[id])
    group <- group[-id]
  }
  
  model <- n[id]
  n <- n[-id]
  
  # Another two most distant points to test set
  id <- c(arrayInd(which.max(D[, -id]), rep(m - 2, 2)))
  
  if (!missing(group)) {
    id <- which(group %in% group[id])
    group <- group[-id]
  }
  
  test <- n[id]
  n <- n[-id]
  
  ini <- i <- length(model)
  
  while (i < k) {
    # cal
    if (i == ini) {
      d <- D[model, -c(model, test), drop = FALSE]
      mins_cal <- do.call(pmin.int, lapply(seq_len(nrow(d)), function(i) d[i, ]))
    } else {
      d <- rbind(D[nid_cal, -c(model, test), drop = FALSE], mins_cal)
      mins_cal <- do.call(pmin.int, lapply(seq_len(nrow(d)), function(i) d[i, ]))
    }
    
    id <- which.max(mins_cal)
    
    if (!missing(group)) {
      id <- which(group %in% group[id])
      group <- group[-id]
    }
    
    nid_cal <- n[id]
    model <- c(model, nid_cal)
    n <- n[-id]
    
    mins_cal <- mins_cal[-id]
    if (i != ini) {
      mins_val <- mins_val[-id]
    }
    
    # test
    if (i == ini) {
      d <- D[test, -c(model, test), drop = FALSE]
      mins_val <- do.call(pmin.int, lapply(seq_len(nrow(d)), function(i) d[i, ]))
    } else {
      d <- rbind(D[nid_val, -c(model, test), drop = FALSE], mins_val)
      mins_val <- do.call(pmin.int, lapply(seq_len(nrow(d)), function(i) d[i, ]))
    }
    
    id <- which.max(mins_val)
    
    if (!missing(group)) {
      id <- which(group %in% group[id])
      group <- group[-id]
    }
    
    nid_val <- n[id]
    test <- c(test, nid_val)
    n <- n[-id]
    mins_val <- mins_val[-id]
    mins_cal <- mins_cal[-id]
    i <- length(model)
  }
  
  if (missing(pc)) {
    return(list(model = model, test = test))
  } else {
    return(list(model = model, test = test, pc = scores))
  }
}
