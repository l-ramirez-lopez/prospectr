#' Top-Hat Filter for Baseline Correction
#'
#' This function applies a top-hat filter (rolling ball-like) to correct
#' the baseline in a signal (e.g., a spectrum).
#'
#' @param y A numeric vector representing the input signal.
#' @param window_size An integer specifying the size of the rolling window for baseline estimation.
#' @param method A character string specifying the method for smoothing ("min" for rolling minimum, "mean" for rolling mean).
#'
#' @return A numeric vector representing the baseline-corrected signal.
#'
#' @examples
#' # Generate synthetic signal with a baseline
#' set.seed(123)
#' x <- seq(1, 1000, by = 1)
#' signal <- sin(2 * pi * x / 200) + rnorm(1000, 0, 0.1)  # Simulated signal
#' baseline <- 0.005 * x  # Simulated baseline
#' y <- signal + baseline  # Combine signal and baseline
#'
#' # Apply Top-Hat filter for baseline correction
#' corrected_signal <- top_hat(y, window_size = 50, method = "min")
#'
#' # Plot original signal, baseline, and corrected signal
#' plot(x, y, type = 'l', col = 'blue', main = 'Top-Hat Filter Baseline Correction', ylab = 'Signal', xlab = 'Index')
#' lines(x, corrected_signal, col = 'red', lwd = 2)
#' legend("topright", legend = c("Original Signal", "Corrected Signal"), col = c("blue", "red"), lwd = 2)
#'
#' @export
top_hat <- function(y, window_size = 50, method = "min") {
  # Ensure window size is odd
  if (window_size %% 2 == 0) {
    window_size <- window_size + 1
  }
  
  # Calculate rolling baseline using zoo::rollapply
  if (method == "min") {
    baseline <- zoo::rollapply(y, window_size, min, fill = NA, align = "center")
  } else if (method == "mean") {
    baseline <- zoo::rollapply(y, window_size, mean, fill = NA, align = "center")
  } else {
    stop("Method must be 'min' or 'mean'")
  }
  
  # Subtract baseline from original signal
  corrected_signal <- y - baseline
  
  return(corrected_signal)
}



# #' Top-Hat Filter for Baseline Correction (Rolling Window)
# #'
# #' This function applies a top-hat filter (rolling ball-like) to correct
# #' the baseline in a signal (e.g., a spectrum) using a rolling minimum or mean.
# #'
# #' @param y A numeric vector representing the input signal.
# #' @param window_size An integer specifying the size of the rolling window for baseline estimation.
# #' @param method A character string specifying the method for smoothing ("min" for rolling minimum, "mean" for rolling mean).
# #'
# #' @return A numeric vector representing the baseline-corrected signal.
# #'
# #' @examples
# #' # Generate synthetic signal with a baseline
# #' set.seed(123)
# #' x <- seq(1, 1000, by = 1)
# #' signal <- sin(2 * pi * x / 200) + rnorm(1000, 0, 0.1)  # Simulated signal
# #' baseline <- 0.005 * x  # Simulated baseline
# #' y <- signal + baseline  # Combine signal and baseline
# #'
# #' # Apply Top-Hat filter for baseline correction
# #' corrected_signal <- top_hat_filter(y, window_size = 50, method = "min")
# #'
# #' # Plot original signal, baseline, and corrected signal
# #' plot(x, y, type = 'l', col = 'blue', main = 'Top-Hat Filter Baseline Correction', ylab = 'Signal', xlab = 'Index')
# #' lines(x, corrected_signal, col = 'red', lwd = 2)
# #' legend("topright", legend = c("Original Signal", "Corrected Signal"), col = c("blue", "red"), lwd = 2)
# #'
# #' @export
# top_hat_filter <- function(y, window_size = 50, method = "min") {
#   # Ensure window size is odd
#   if (window_size %% 2 == 0) {
#     window_size <- window_size + 1
#   }
# 
#   # Initialize baseline vector
#   L <- length(y)
#   baseline <- rep(NA, L)
# 
#   # Rolling window function for minimum or mean
#   half_window <- floor(window_size / 2)
# 
#   for (i in 1:L) {
#     # Define window range
#     start <- max(1, i - half_window)
#     end <- min(L, i + half_window)
#     window <- y[start:end]
# 
#     # Calculate rolling min or mean
#     if (method == "min") {
#       baseline[i] <- min(window)
#     } else if (method == "mean") {
#       baseline[i] <- mean(window)
#     } else {
#       stop("Method must be 'min' or 'mean'")
#     }
#   }
# 
#   # Subtract baseline from original signal
#   corrected_signal <- y - baseline
# 
#   return(corrected_signal)
# }

