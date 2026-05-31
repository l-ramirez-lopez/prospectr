#' Whittaker Smoother for Baseline Correction
#'
#' This function applies the Whittaker smoother for baseline correction to a given spectrum.
#' It uses penalized least squares with second-order differences for smoothness.
#' The method is based on the paper by Eilers (2003), which describes the Whittaker smoother as
#' a method for data smoothing and baseline correction.
#'
#' @param y A numeric vector representing the input signal (e.g., a spectrum).
#' @param lambda A numeric value controlling the smoothness of the baseline. Larger values produce smoother baselines. Default is 1e6.
#' @param n_iter An integer specifying the number of iterations for convergence. Default is 10.
#'
#' @return A numeric vector representing the baseline-corrected signal.
#'
#' @references
#' Eilers, P. H. C. (2003). A perfect smoother. \emph{Analytical Chemistry}, 75(14), 3631-3636.
#'
#' @examples
#' # Generate synthetic signal with a baseline
#' set.seed(123)
#' x <- seq(1, 1000, by = 1)
#' signal <- sin(2 * pi * x / 200) + rnorm(1000, 0, 0.05)  # Simulated signal
#' baseline <- 0.005 * x  # Simulated baseline
#' y <- signal + baseline  # Combine signal and baseline
#'
#' # Apply Whittaker smoother for baseline correction
#' corrected_signal <- whittaker_smoother(y, lambda = 1e6, n_iter = 10)
#'
#' # Plot original signal and corrected signal
#' plot(x, y, type = 'l', col = 'blue', main = 'Whittaker Smoother Baseline Correction', ylab = 'Signal', xlab = 'Index')
#' lines(x, corrected_signal, col = 'red', lwd = 2)
#' legend("topright", legend = c("Original Signal", "Corrected Signal"), col = c("blue", "red"), lwd = 2)
#'
#' @export
whittaker_smoother <- function(y, lambda = 1e6, n_iter = 10) {
  L <- length(y)
  D <- diff(diag(L), differences = 2)  # Second-order difference matrix
  w <- rep(1, L)  # Initialize weights
  
  for (i in 1:n_iter) {
    # Apply penalized least squares with weighted smoothness
    W <- diag(w)  # Diagonal weight matrix
    Z <- solve(W + lambda * t(D) %*% D) %*% (w * y)  # Penalized least squares
    
    # Update weights for next iteration
    w <- ifelse(y > Z, 0.1, 1)  # Weight updates based on position relative to baseline
  }
  
  corrected_signal <- y - Z  # Subtract baseline from original signal
  return(corrected_signal)
}
