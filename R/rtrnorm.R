#' Sample from a truncated normal distribution
#'
#' This function samples from a truncated normal distribution defined by the
#' mean, standard deviation, and lower and upper bounds. It uses rejection
#' sampling to ensure that the sampled values are within the specified bounds.
#' The function is vectorised, so it can handle multiple sets of parameters at
#' once.
#' @param mean A numeric vector of means for the truncated normal distribution.
#' @param sd A numeric vector of standard deviations for the truncated normal
#'   distribution.
#' @param lower A numeric vector of lower bounds for the truncated normal
#'   distribution.
#' @param upper A numeric vector of upper bounds for the truncated normal
#'   distribution.
#' @returns A numeric vector of sampled values from the truncated normal
#'   distribution.
#' @keywords internal

rtrnorm_naive <- function(mean, sd, lower, upper) {
  
  # Check that all inputs are numeric
  if (!is.numeric(mean) || !is.numeric(sd) ||
      !is.numeric(lower) || !is.numeric(upper)) {
    stop("All inputs must be numeric vectors")
  }
  
  # Check that all inputs are the same length
  n <- length(mean)
  if (length(sd) != n || length(lower) != n || length(upper) != n) {
    stop("All inputs must have the same length")
  }
  
  # Check NAs are present for all inputs in the same positions
  na_matrix <- cbind(is.na(mean), is.na(sd), is.na(lower), is.na(upper))
  
  # Rows where some but not all are NA → invalid
  inconsistent_na <- apply(na_matrix, 1, function(row) any(row) && !all(row))
  
  if (any(inconsistent_na)) {
    stop("If any value is NA, all corresponding entries (mean, sd, lower, upper) must be NA")
  }
  
  # Identify fully NA rows
  all_na <- rowSums(na_matrix) == 4
  valid  <- !all_na
  
  # Validate non-NA entries
  if (any(sd[valid] <= 0)) {
    stop("All non-NA standard deviations must be strictly positive")
  }
  
  if (any(lower[valid] > upper[valid])) {
    stop("Each 'lower' must be <= corresponding 'upper'")
  }
  
  # Output vector initialized with NA
  ret <- rep(NA_real_, n)
  
  if (!any(valid)) return(ret)
  
  # Standardised bounds for valid rows
  lower_std <- (lower[valid] - mean[valid]) / sd[valid]
  upper_std <- (upper[valid] - mean[valid]) / sd[valid]
  
  # Indices of valid rows and remaining indices for sampling
  valid_idx <- which(valid)
  remaining <- seq_along(valid_idx)
  
  # Storage for sampled standardised values
  z_ret <- numeric(length(valid_idx))
  
  # Rejection sampling
  while (length(remaining) > 0) {
    
    z <- rnorm(length(remaining))
    
    accept <- (z >= lower_std[remaining]) & (z <= upper_std[remaining])
    
    if (any(accept)) {
      idx_accept <- remaining[accept]
      z_ret[idx_accept] <- z[accept]
      
      remaining <- remaining[!accept]
    }
  }
  
  # Transform back
  ret[valid_idx] <- z_ret * sd[valid] + mean[valid]
  
  ret
}
