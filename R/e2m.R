#' @title A function for transforming a matrix from its Euclidean space to its Mahalanobis space
#' @examples
#' # test data
#' \dontrun{
#' X <- matrix(rnorm(500), ncol = 5)
#' # Normal way to compute the Mahalanobis distance
#' md1 <- sqrt(mahalanobis(X, center = colMeans(X), cov = cov(X)))
#' # Projection approach for computing the Mahalanobis distance
#' # 1. Projecting from the Euclidean to the Mahalanobis space
#' Xm <- e2m(X, sm.method = "svd")
#' # 2. Use the normal Euclidean distance on the Mahalanobis space
#' md2 <- sqrt(rowSums((sweep(Xm, 2, colMeans(Xm), "-"))^2))
#' # Plot the results of both methods
#' plot(md1, md2)
#' # Test on a real dataset
#' # Mahalanobis in the spectral space
#' data(NIRsoil)
#' X <- NIRsoil$spc
#' Xm <- e2m(X, sm.method = "svd")
#' md2 <- sqrt(rowSums((sweep(Xm, 2, colMeans(Xm), "-"))^2))
#'
#' md1 <- sqrt(mahalanobis(X, center = colMeans(X), cov = cov(X))) # does not work
#' # Mahalanobis in the PC space
#' pc <- 20
#' pca <- prcomp(X, center = TRUE, scale = TRUE)
#' X <- pca$x[, 1:pc]
#' X2 <- sweep(pca$x[, 1:pc, drop = FALSE], 2, pca$sdev[1:pc], "/")
#' md4 <- sqrt(rowSums((sweep(Xm, 2, colMeans(Xm), "-"))^2))
#' md5 <- sqrt(rowSums((sweep(X2, 2, colMeans(X2), "-"))^2))
#' md3 <- sqrt(mahalanobis(X, center = colMeans(X), cov = cov(X))) # does work
#' }
#' @keywords internal
e2m <- function(X, sm.method = c("svd", "eigen")) {
  nms <- dimnames(X)

  if (ncol(X) > nrow(X)) {
    stop("In order to project the matrix to a Mahalanobis space, the number of observations of the input matrix must greater than its number of variables")
  }

  if (ncol(X) == 1) {
    ms_x <- X / sd(X)
  } else {
    sm_method <- match.arg(sm.method)

    X <- as.matrix(X)
    vcv <- cov(X)
    sq_vcv <- sqrtSm(vcv, method = sm_method)
    sq_S <- solve(sq_vcv)
    ms_x <- X %*% sq_S
    dimnames(ms_x) <- nms
  }
  return(ms_x)
}


#' @title Square root of (square) symetric matrices
#' @keywords internal
sqrtSm <- function(X, method = c("svd", "eigen")) {
  if (!isSymmetric(X)) {
    stop("X must be a square symmetric matrix")
  }
  method <- match.arg(method)

  if (method == "svd") {
    out <- svd(X)
    D <- diag(out$d)
    U <- out$v
  }

  if (method == "eigen") {
    out <- eigen(X)
    D <- diag(out$values)
    U <- out$vectors
  }

  # if(method == 'Schur'){ require(geigen) out <- Schur(X) D <- diag(out$EValues) U <- out$Q }
  return(U %*% sqrt(D) %*% t(U))
}
