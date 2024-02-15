#' @title Kennard-Stone algorithm for calibration sampling
#' @description
#' \loadmathjax
#' Select calibration samples from a large multivariate data using the
#' Kennard-Stone algorithm
#' @usage
#' kenStone(X, k, metric = "mahal", pc, group,
#'          .center = TRUE, .scale = FALSE, init = NULL)
#' @param X a numeric matrix.
#' @param k number of calibration samples to be selected.
#' @param metric distance metric to be used: 'euclid' (Euclidean distance) or
#' 'mahal' (Mahalanobis distance, default).
#' @param pc optional. If not specified, distance are computed in the Euclidean
#' space. Alternatively, distance are computed
#' in the principal component score space and  `pc` is the number of principal
#' components retained.
#' If `pc < 1`, the number of principal components kept corresponds to the
#' number of components explaining at least (`pc * 100`) percent of the total
#' variance.
#' @param group An optional `factor` (or vector that can be coerced to a factor
#' by \code{\link{as.factor}}) of length equal to \code{nrow(X)}, giving the identifier
#' of related observations (e.g. samples of the same batch of measurements,
#' samples of the same origin, or of the same soil profile). Note that by using 
#' this option in some cases, the number of samples retrieved is not exactly the
#' one specified in `k` as it will depend on the groups. See details.  
#' @param .center logical value indicating whether the input matrix should be
#' centered before Principal Component Analysis. Default set to \code{TRUE}.
#' @param .scale logical value indicating whether the input matrix should be
#' scaled before Principal Component
#' Analysis. Default set to \code{FALSE}.
#' @param init (optional) a vector of integers indicating the indices of the
#' observations/rows that are to be used as observations that must be included
#' at the first iteration of the search process. Default is \code{NULL}, i.e. no
#' fixed initialization. The function will take by default the two most distant
#' observations. If the \code{group} argument is used, then all the observations
#' in the groups covered by the \code{init} observations will be also included
#' in the \code{init} subset.
#' @return a list with the following components:
#' \itemize{
#'  \item{`model`: numeric vector giving the row indices of the input data
#'  selected for calibration}
#'  \item{`test`: numeric vector giving the row indices of the remaining
#'  observations}
#'  \item{`pc`: if the `pc` argument is specified, a numeric matrix of the
#'  scaled pc scores}
#'  }
#' @references
#' Kennard, R.W., and Stone, L.A., 1969. Computer aided design of experiments.
#' Technometrics 11, 137-148.
#' @examples
#' data(NIRsoil)
#' sel <- kenStone(NIRsoil$spc, k = 30, pc = .99)
#' plot(sel$pc[, 1:2], xlab = "PC1", ylab = "PC2")
#' # points selected for calibration
#' points(sel$pc[sel$model, 1:2], pch = 19, col = 2)
#' # Test on artificial data
#' X <- expand.grid(1:20, 1:20) + rnorm(1e5, 0, .1)
#' plot(X, xlab = "VAR1", ylab = "VAR2")
#' sel <- kenStone(X, k = 25, metric = "euclid")
#' points(X[sel$model, ], pch = 19, col = 2)
#' 
#' # Using the group argument
#' library(prospectr)
#' 
#' # create groups
#' set.seed(1)
#' my_groups <- sample(1:275, nrow(NIRsoil$spc), replace = TRUE) |> as.factor()
#' 
#' # check the group size 
#' table(my_groups)
#' 
#' results_group <- kenStone(X = NIRsoil$spc, k = 2, pc = 3, group = my_groups)
#' 
#' # as the first two samples selected belong to groups
#' # which have in total more than 2 samples (k).
#' my_groups[results_group$model] |>  factor() |> table()
#' 
#' @author Antoine Stevens &
#' \href{https://orcid.org/0000-0002-5369-5120}{Leonardo Ramirez-Lopez} with
#' contributions from Thorsten Behrens and Philipp Baumann
#' @details
#' The Kennard--Stone algorithm allows to select samples with a uniform
#' distribution over the predictor space (Kennard and Stone, 1969).
#' It starts by selecting the pair of points that are the farthest apart.
#' They are assigned to the calibration set and removed from the list of points.
#' Then, the procedure assigns remaining points to the calibration set
#' by computing the distance between each unassigned points
#' \mjeqn{i_0}{i_0} and selected points \mjeqn{i}{i}
#' and finding the point for which:
#'
#' \mjdeqn{d_{selected} = \max\limits_{i_0}(\min\limits_{i}(d_{i,i_{0}}))}{d_{sel ected} = \max_{i_0}(\min_{i}(d_{i,i0}))}
#'
#' This essentially selects point \mjeqn{i_0}{i_0} which is the farthest apart from its
#' closest neighbors \mjeqn{i}{i} in the calibration set.
#' The algorithm uses the Euclidean distance to select the points. However,
#' the Mahalanobis distance can also be used. This can be achieved by performing
#' a PCA on the input data and computing the Euclidean distance on the truncated
#' score matrix according to the following definition of the Mahalanobis \mjeqn{H}{H}
#' distance:
#'
#' \mjdeqn{H_{ij}^2 = \sum_{a=1}^A (\hat t_{ia} - \hat t_{ja})^{2} / \hat \lambda_a}{H_{ij}^2 = sum_{a=1}^A (hat t_{ia} - hat t_{ja})^{2} / hat lambda_a}
#'
#' where \mjeqn{\hat t_{ia}}{hatt_{ia}} is the \mjeqn{a^{th}}{a^{th}} principal component
#' score of point \mjeqn{i}{i}, \mjeqn{\hat t_{ja}}{hatt_{ja}} is the
#' corresponding value for point \mjeqn{j}{j},
#' \mjeqn{\hat \lambda_a}{hat lambda_a} is the eigenvalue of principal
#' component \mjeqn{a}{a} and \mjeqn{A}{A} is the number of principal components
#' included in the computation.
#' 
#' When the \code{group} argument is used, the sampling is conducted in such a 
#' way that at each iteration, when a single sample is selected, this sample 
#' along with all the samples that belong to its group, are assigned to the
#' final calibration set. In this respect, at each iteration, the algorithm 
#' will select one sample (in case that sample is the only one in that group) 
#' or more to the calibration set. This also implies that the argument \code{k}
#' passed to the function will not necessary reflect the exact number of samples
#' selected. For example, if \code{k = 2} and if the first sample identified 
#' belongs to with group of 5 samples and the second one belongs to a group with 
#' 10 samples, then, the total amount of samples retrieved by the 
#' function will be 15. 
#' 
#' @seealso  \code{\link{duplex}}, \code{\link{shenkWest}}, \code{\link{naes}},
#' \code{\link{honigs}}
#' @export

kenStone <- function(X,
                     k,
                     metric = "mahal",
                     pc,
                     group,
                     .center = TRUE,
                     .scale = FALSE,
                     init = NULL) {
  if (missing(k)) {
    stop("'k' must be specified")
  }
  if (ncol(X) < 2) {
    stop("'X' must have at least 2 columns")
  }
  if (k < 2) {
    stop("Invalid argument: 'k' must be larger than 2")
  }
  metric <- match.arg(metric, c("mahal", "euclid"))
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
    scores <- X <- pca$x[, 1:pc, drop = FALSE]
  }
  
  if (metric == "mahal") {
    # Project in the Mahalanobis distance space
    X <- e2m(X, sm.method = "svd")
    if (!missing(pc)) {
      scores <- X
    }
  }
  
  m <- nrow(X)
  n_ref <- 1:m
  
  if (k >= m) {
    k <- m - 1
  }
  
  if (!missing(group)) {
    if (length(group) != nrow(X)) {
      stop("length(group) should be equal to nrow(X)")
    }
    if (!is.factor(group)) {
      group <- as.factor(group)
      warning("group has been coerced to a factor")
    }
  }
  
  distance_mat <- fastDist(X, X, "euclid")
  rm(X)
  gc()
  
  if (!is.null(init)) {
    # init is used to initialize the model set
    
    is_integer <- function(x, tol = .Machine$double.eps^0.5) abs(x - round(x)) < tol
    ## sanity checks for init
    if (!all(is_integer(init))) {
      stop("Seems like you are tryning to use non-integers in the init argument")
    }
    
    init <- unique(init)
    
    if (k < length(init)) {
      stop("Invalid argument: 'k' must be larger than the length of init")
    }
    id <- init
    
    if (length(init) == 1) {
      id <- c(id, which.max(distance_mat[, id]))
    }
  } else {
    # Fist two most distant points to initialize the model set
    id <- c(arrayInd(which.max(distance_mat), rep(m, 2)))
  }
  
  if (!missing(group)) {
    iter_group <- group
    id <- which(iter_group %in% iter_group[id])
    iter_group <- iter_group[-id]
  }
  
  model <- n_ref[id]
  n_ref <- n_ref[-id]
  ini <- i <- length(model)
  
  # browser()
  while (i < k) {
    if (i == ini) {
      distance_sub <- distance_mat[model, -model] # first loop
    } 
    else {
      distance_sub <- rbind(mins, distance_mat[nid, -model])
    }
    
    mins <- do.call(pmin.int, lapply(1:nrow(distance_sub), function(i) distance_sub[i, ]))
    # # alternatively
    # mins <- apply(distance_sub, MARGIN = 2, function(x) min(x))
    
    id <- which.max(mins)
    
    if (!missing(group)) {
      id <- which(iter_group %in% iter_group[id])
      # print(iter_group[id])
      # print(length(iter_group[id]))
      iter_group <- iter_group[-id]
    }
    
    nid <- n_ref[id]
    model <- c(model, nid)
    n_ref <- n_ref[-id]
    mins <- mins[-id]
    i <- length(model)
  }
  
  if (!missing(group)) {
    mss <- paste0(
      "\033[3m",
      "Samples selected: ", 
      length(model), 
      " from ", 
      length(unique(group[model])), 
      " groups \n",
      "\033[23m"
    )
    cat(mss)
  }
  
  if (missing(pc)) {
    return(list(model = model, test = (1:m)[-model]))
  } else {
    return(list(model = model, test = (1:m)[-model], pc = scores))
  }
}
