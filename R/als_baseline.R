#' Asymmetric Least Squares (ALS) Baseline Correction
#'
#' Performs Asymmetric Least Squares (ALS) baseline correction on a given signal.
#' This method is useful for removing background noise or baseline drift in spectral data.
#'
#' @param y A numeric vector representing the input signal (e.g., a spectrum).
#' @param lambda A numeric value that controls the smoothness of the estimated baseline. Larger values result in a smoother baseline. Default is 6.
#' @param p A numeric value between 0 and 1 that controls the asymmetry. Values close to 0 put more emphasis on negative residuals, and values close to 1 put more emphasis on positive residuals. Default is 0.05.
#' @param n_iter An integer representing the number of iterations for convergence. Default is 10.
#'
#' @return A numeric vector representing the estimated baseline.
#'
#' @examples
#' # Generate a synthetic signal with a baseline
#' set.seed(123)
#' x <- seq(1, 1000, by = 1)
#' signal <- sin(2 * pi * x / 200) + rnorm(1000, 0, 0.1)  # Simulated signal
#' baseline <- 0.005 * x  # Simulated baseline
#' y <- signal + baseline  # Combine signal and baseline
#'
#' # Apply Asymmetric Least Squares baseline correction
#' corrected_baseline <- als_baseline(y, lambda = 1e5, p = 0.01, n_iter = 10)
#'
#' # Plot the original signal and the corrected baseline
#' plot(x, y, type = 'l', col = 'blue', main = 'ALS Baseline Correction', ylab = 'Signal', xlab = 'Index')
#' lines(x, corrected_baseline, col = 'red', lwd = 2)
#' legend("topright", legend = c("Original Signal", "Estimated Baseline"), col = c("blue", "red"), lwd = 2)
#'
#' @export
als_baseline <- function(y, lambda = 6, p = 0.05, n_iter = 10) {
  # y: input signal (vector of data points)
  # lambda: smoothness parameter (larger values = smoother baseline)
  # p: asymmetry parameter (p < 0.5 more emphasis on negative residuals)
  # n_iter: number of iterations for convergence
  
  L <- length(y)
  D <- diff(diag(L), differences = 2)  # Second-order difference matrix
  w <- rep(1, L)  # Initialize weights
  
  for (i in 1:n_iter) {
    W <- diag(w)  # Create a diagonal matrix of weights
    Z <- solve(W + lambda * t(D) %*% D) %*% (w * y)  # Solve the linear system
    w <- p * (y > Z) + (1 - p) * (y < Z)  # Update weights based on residuals
  }
  
  return(Z)  # Return the estimated baseline
}
