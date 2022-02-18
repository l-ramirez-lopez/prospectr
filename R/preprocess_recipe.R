#' @title Functions for processing spectral data
#' @aliases preprocess
#' @description
#'
#' \loadmathjax
#' The \code{preprocess_recipe} function groups a sequence of
#' preprocessing steps given by the \code{nwp_preprocess} methods while
#' \code{preprocess} is used to execute these recipes on a given dataset.
#'
#' @usage
#'
#' preprocess_recipe(...)
#'
#' preprocess(X, recipe = preprocess_recipe())
#'
#' @param ... one or more objects of class \code{nwp_preprocess} as returned by
#' the following functions (where the \code{X} argument is not passed to them):
#'
#' \itemize{
#'        \item{\code{\link{nwp_spline}}}
#'        \item{\code{\link{nwp_smooth}}}
#'        \item{\code{\link{nwp_snvt}}}
#'        \item{\code{\link{nwp_derivative}}}
#'        }
#'
#' The order in which the objects are provided represents the order in which the
#' preprocessing methods will be applied.
#'
#' @param X a numeric matrix of spectral data to be preprocessed.
#' @param recipe an object of class \code{preprocess_recipe} (as generated
#' with the \code{preprocess_recipe} function) containing the preprocessing
#' recipe, i.e. a sequence of preprocessing methods to be applied on \code{X}.
#' Optionally, an object of class \code{nwp_preprocess} can also be passed.
#' Default is a call to \code{preprocess_recipe()} without any
#' \code{nwp_preprocess} object  (i.e. no preprocessing).
#'
#' @return For \code{preprocess_recipe}, a list with the preprocessing
#' recipe. For \code{preprocess} a matrix of processed spectra.
#' @author Leonardo Ramirez-Lopez
#' @seealso \code{\link{nwp_model}}
#' @examples
#' # As a recipe
#' prepro <- preprocess_recipe(
#'   nwp_spline(min_w = 1000, max_w = 1700, resolution = 2),
#'   nwp_derivative(m = 1, half_w = 3, half_s = 5),
#'   nwp_snvt(),
#'   nwp_smooth(half_w = 5)
#' )
#'
#' data("NIRcannabis")
#' # applying the recipe to the spectrum
#' X_prepro <- preprocess(NIRcannabis$spc, recipe = prepro)
#' @export

## History:
## 25.08.2020 hello world

preprocess_recipe <- function(...) {
  prepro_dots <- list(...)
  ## names of the objects passed
  nms <- sapply(substitute(list(...))[-1], deparse)
  prepro_dots
  classes <- sapply(prepro_dots, FUN = function(X) "nwp_preprocess" %in% class(X))
  if (any(!classes)) {
    not_accepted <- nms[!classes]
    if (length(not_accepted) == 1) {
      frm <- " is"
    } else {
      frm <- " are"
    }
    stop(paste0(not_accepted, frm, " not of class 'nwp_preprocess'"))
  }
  
  pmethods <- gsub("nwp_", "", sapply(prepro_dots, FUN = function(X) X$method))
  
  if ("derivative" %in% pmethods) {
    der_order <- prepro_dots[[which(pmethods %in% "derivative")]]
    if (der_order$m == 1) {
      pmethods[which(pmethods %in% "derivative")] <- "derivative: 1st"
    }
    if (der_order$m == 2) {
      pmethods[which(pmethods %in% "derivative")] <- "derivative: 2nd"
    }
  }
  
  pr_order <- paste0(pmethods, collapse = " > ")
  
  nwp_recipe <- list(
    preprocessing_recipe = prepro_dots,
    preprocessing_order = pr_order
  )
  
  class(nwp_recipe) <- c("list", "preprocess_recipe")
  nwp_recipe
}

#' @aliases preprocess_recipe
#' @export preprocess
preprocess <- function(X, recipe = preprocess_recipe()) {
  if (!("nwp_preprocess" %in% class(recipe) | "preprocess_recipe" %in% class(recipe))) {
    stop("'recipe' must be either of class 'nwp_preprocess' or 'nwp_preprocess_recipe'")
  }
  
  if ("preprocess_recipe" %in% class(recipe)) {
    steps <- length(recipe$preprocessing_recipe)
    if (steps > 0) {
      for (i in 1:steps) {
        ith_step <- recipe$preprocessing_recipe[[i]]
        ith_method <- ith_step$method
        ith_arguments <- c(
          list(X = X),
          ith_step[!names(ith_step) %in% "method"]
        )
        X <- do.call(ith_method, ith_arguments)
      }
    }
  }
  if ("nwp_preprocess" %in% class(recipe)) {
    p_method <- recipe$method
    p_arguments <- c(
      list(X = X),
      recipe[!names(recipe) %in% "method"]
    )
    X <- do.call(p_method, p_arguments)
  }
  X
}