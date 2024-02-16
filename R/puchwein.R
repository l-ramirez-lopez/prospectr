#' @title Puchwein algorithm for calibration sampling
#'
#' @description
#' Select calibration samples from multivariate data using the Puchwein
#' algorithm
#' @usage
#' puchwein(X,
#'          pc = 0.95,
#'          k,
#'          min.sel,
#'          details = FALSE,
#'          .center = TRUE,
#'          .scale = FALSE)
#' @param X a matrix from which the calibration samples are to be selected
#' (optionally a data frame that can be coerced to a numerical matrix).
#' @param pc the number of principal components retained in the computation of
#' the distance in the standardized Principal Component space (Mahalanobis
#' distance).
#' If `pc < 1`, the number of principal components kept corresponds to the
#' number of components
#' explaining at least (`pc * 100`) percent of the total variance
#' (default = 0.95 as in the Puchwein paper).
#' @param k the initial limiting distance parameter, if not specified (default),
#' set to 0.2. According to Puchwein, a good starting value for the limiting
#' distance is \eqn{d_{ini} = k(p-2)} where \eqn{p} is the number of
#' principal components
#' @param min.sel minimum number of samples to select for calibration
#' (default = 5).
#' @param details logical value, if `TRUE`, adds a component in the output list
#' with the indices of the objects kept in each loop (default to `FALSE`).
#' @param .center logical value indicating whether the input matrix must be
#' centered before Principal Component.
#' Analysis. Default set to TRUE.
#' @param .scale logical value indicating whether the input matrix must be
#' scaled before Principal Component
#' Analysis. Default set to FALSE.
#' @author Antoine Stevens
#' @return a `list` with components:
#' \itemize{
#'  \item{'`model`': indices of the observations (row indices of the input
#'  data)
#'  selected for calibration}
#'  \item{'`test`': indices of the remaining observations (row indices of the
#'  input data)}
#'  \item{'`pc`': a numeric matrix of the scaled pc scores}
#'  \item{'`loop.optimal`': index of the loop producing the maximum difference
#'  between the observed and
#'  theoretical sum of leverages of the selected samples}
#'  \item{'`leverage`': data frame giving the observed and theoretical
#'  cumulative sums of leverage of the points selected in each loop}
#'  \item{'`details`': list with the indices of the observations kept in each
#'  loop}
#' }
#' @examples
#' data(NIRsoil)
#' sel <- puchwein(NIRsoil$spc, k = 0.2, pc = .99)
#' plot(sel$pc[, 1:2])
#' # points selected for calibration
#' points(NIRsoil$spc[sel$model, 1:2], col = 2, pch = 2)
#' # Leverage plot
#' opar <- par(no.readonly = TRUE)
#' par(mar = c(4, 5, 2, 2))
#' plot(sel$leverage$loop, sel$leverage$diff,
#'   type = "l",
#'   xlab = "# loops",
#'   ylab = "Difference between theoretical and \n observed sum of leverages"
#' )
#' par(opar)
#' @references
#' Puchwein, G., 1988. Selection of calibration samples for near-infrared
#' spectrometry by factor analysis of spectra. Analytical Chemystry 60, 569-573.
#'
#' Shetty, N., Rinnan, A., and Gislum, R., 2012. Selection of representative
#' calibration sample sets for near-infrared reflectance spectroscopy to predict
#' nitrogen concentration in grasses. Chemometrics and Intelligent Laboratory
#' Systems 111, 59-65.
#' @details
#' The Puchwein algorithm select samples from a data matrix by iteratively
#' eliminating similar samples using the Mahalanobis distance.
#' It starts by performing a PCA on the input matrix and extracts the score
#' matrix truncated to \eqn{A}, the number of principal components. The score
#' matrix is then normalized to unit variance and the Euclidean distance of each
#' sample to the centre of the data is computed, which is identical to the
#' Mahalanobis distance \eqn{H}. Additionally, the Mahalanobis distances between
#' samples are comptuted. The algorithm then proceeds as follows:
#'
#' \enumerate{
#'    \item Choose a initial limiting distance \eqn{d_{ini}}
#'    \item Select the sample with the highest \eqn{H} distance to the centre
#'    \item Remove all samples within the minimum distance \eqn{d_{ini}} from
#'     the sample selected in step 2
#'    \item Go back to step 2 and proceed until there are no samples/observations
#'    left in the dataset
#'    \item Go back to step 1 and increase the minimum distance by multiplying
#'    the limiting distance by the loop number
#' }
#'
#' It is not possible to obtain a pre-defined number of samples selected by the
#' method. To choose the adequate number of samples, a data frame is returned
#' by `puchwein` function (`leverage`) giving the observed and theoretical
#' cumulative sum of leverages of the points selected in each iteration. The
#' theoretical cumulative sum of leverage is computed such as each point has the
#' same leverage (the sum of leverages divided by the number of observations).
#' The loop having the largest difference between the observed and theoretical
#' sums is considered as producing the optimal selection of points (the subset
#' that best reproduces the variability of the predictor space).
#' @note The Puchwein algorithm is an iterative method and can be slow for large
#' data matrices.
#' @seealso \code{\link{kenStone}}, \code{\link{duplex}},
#' \code{\link{shenkWest}}, \code{\link{honigs}}, \code{\link{naes}}
#' @export

puchwein <- function(X,
                     pc = 0.95,
                     k = 0.2,
                     min.sel = 5,
                     details = FALSE,
                     .center = TRUE,
                     .scale = FALSE) {
  if (ncol(X) < 2) {
    stop("X must have at least 2 columns")
  }
  if (min.sel >= nrow(X)) {
    stop("min.sel must be lower than nrow(X)")
  }
  if (!is.data.frame(X)) {
    X <- as.data.frame(X)
  }

  # Compute scores from PCA
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
  X <- sweep(pca$x[, 1:pc, drop = FALSE], 2, pca$sdev[1:pc], "/") # scaling of the scores

  H <- fastDistV(X, colMeans(X), "euclid") # mahalanobis distance to the centre
  lsel <- list()
  x <- data.frame(ID = 1:nrow(X), H)
  ord <- order(H, decreasing = TRUE)
  d <- fastDist(X, X, "euclid")[ord, ord]
  x <- x[ord, ]

  d.ini <- k * max((ncol(X) - 2), 1)

  m <- 1
  repeat {
    Dm <- m * d.ini
    minD <- d[1, ] <= Dm
    sel <- x$ID[1]

    for (i in 1:nrow(x)) {
      if (!i %in% which(minD)) {
        sel <- c(sel, x$ID[i])
        minD <- minD | d[i, ] <= Dm
      }
    }
    lsel[[m]] <- sel
    if (length(sel) <= min.sel) {
      break
    }
    m <- m + 1
  }
  lsel[[length(lsel)]] <- NULL # Remove last iteration

  left <- sapply(lsel, length) # number of samples left in each loop
  sel <- nrow(x) - left
  # rem <- diff(c(0, sel)) # number of samples removed in each loop res <- data.frame(left = left, removed = rem, loop
  # = 1:m)

  x.mat <- as.matrix(X)
  L <- diag(x.mat %*% solve(t(x.mat) %*% x.mat) %*% t(x.mat)) # Leverage
  oL <- sapply(lsel, function(x) sum(L[x])) # observed leverage
  tL <- (sum(L) / length(L)) * left # theoretical leverage

  res2 <- data.frame(loop = 1:(m - 1), removed = sel, obs = oL, theor = tL, diff = oL - tL)
  loop.optimal <- which.max(oL - tL)
  model <- lsel[[loop.optimal]]

  if (details) {
    return(list(model = model, test = (1:nrow(X))[-model], pc = X, loop.optimal = loop.optimal, leverage = res2, details = lsel))
  } else {
    return(list(model = model, test = (1:nrow(X))[-model], pc = X, loop.optimal = loop.optimal, leverage = res2))
  }
}
