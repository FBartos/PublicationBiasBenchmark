#' @title Performance metrics and Monte Carlo Standard Errors
#'
#' @description
#' A comprehensive set of functions for computing performance metrics and their
#' Monte Carlo Standard Errors (MCSE) for simulation studies. All functions are
#' based on definitions from Table 3 in
#' \insertCite{siepe2024simulation;textual}{PublicationBiasBenchmark}. Also see
#' \insertCite{morris2019using;textual}{PublicationBiasBenchmark} for additional
#' details. Bias and relative bias were modified to account for possibly different
#' true values across repetitions.
#'
#' @name performance_metrics
#' @aliases bias bias_mcse relative_bias relative_bias_mcse mse mse_mcse rmse rmse_mcse
#' @aliases empirical_variance empirical_variance_mcse empirical_se empirical_se_mcse
#' @aliases coverage coverage_mcse power power_mcse mean_ci_width mean_ci_width_mcse
#' @aliases mean_generic_statistic mean_generic_statistic_mcse
#'
#' @details
#' The package provides the following performance metrics and their corresponding MCSE functions:
#'
#' \itemize{
#'   \item \code{bias(theta_hat, theta)}: Bias estimate
#'   \item \code{relative_bias(theta_hat, theta)}: Relative bias estimate
#'   \item \code{mse(theta_hat, theta)}: Mean Square Error
#'   \item \code{rmse(theta_hat, theta)}: Root Mean Square Error
#'   \item \code{empirical_variance(theta_hat)}: Empirical variance
#'   \item \code{empirical_se(theta_hat)}: Empirical standard error
#'   \item \code{coverage(ci_lower, ci_upper, theta)}: Coverage probability
#'   \item \code{power(test_rejects_h0)}: Statistical power
#'   \item \code{mean_ci_width(ci_upper, ci_lower)}: Mean confidence interval width
#'   \item \code{mean_generic_statistic(G)}: Mean of any generic statistic
#' }
#'
#' @param theta_hat Vector of parameter estimates from simulations
#' @param theta True parameter value
#' @param ci_lower Vector of lower confidence interval bounds
#' @param ci_upper Vector of upper confidence interval bounds
#' @param test_rejects_h0 Logical vector indicating whether statistical tests reject the null hypothesis
#' @param G Vector of generic statistics from simulations
#'
#' @return
#' Each metric function returns a numeric value representing the performance measure.
#' Each MCSE function returns a numeric value representing the Monte Carlo standard error.
#'
#'
#' @examples
#' # Generate some example data
#' set.seed(123)
#' theta_true <- 0.5
#' theta_estimates <- rnorm(1000, mean = theta_true, sd = 0.1)
#'
#' # Compute bias and its MCSE
#' bias_est <- bias(theta_estimates, theta_true)
#' bias_se <- bias_mcse(theta_estimates)
#'
#' # Compute MSE and its MCSE
#' mse_est <- mse(theta_estimates, theta_true)
#' mse_se <- mse_mcse(theta_estimates, theta_true)
#'
#' # Example with coverage
#' ci_lower <- theta_estimates - 1.96 * 0.1
#' ci_upper <- theta_estimates + 1.96 * 0.1
#' coverage_est <- coverage(ci_lower, ci_upper, theta_true)
#' coverage_se <- coverage_mcse(ci_lower, ci_upper, theta_true)
#'
NULL

# Helper functions for common variance calculations
# These are internal functions not exported to users

#' Calculate sample variance of estimates
#' @param theta_hat Vector of estimates
#' @return Sample variance S_theta^2
S_theta_squared <- function(theta_hat) {
  n_sim <- length(theta_hat)
  sum((theta_hat - mean(theta_hat))^2) / (n_sim - 1)
}

#' Calculate sample variance of squared errors
#' @param theta_hat Vector of estimates
#' @param theta True parameter value
#' @return Sample variance S_(theta_hat - theta)^2
S_theta_minus_theta_squared <- function(theta_hat, theta) {
  n_sim <- length(theta_hat)
  squared_errors <- (theta_hat - theta)^2
  sum((squared_errors - mean(squared_errors))^2) / (n_sim - 1)
}

#' Calculate sample variance of CI widths
#' @param ci_upper Vector of upper CI bounds
#' @param ci_lower Vector of lower CI bounds
#' @return Sample variance S_w^2
S_w_squared <- function(ci_upper, ci_lower) {
  n_sim <- length(ci_upper)
  ci_widths <- ci_upper - ci_lower
  sum((ci_widths - mean(ci_widths))^2) / (n_sim - 1)
}

#' Calculate sample variance of generic statistic
#' @param G Vector of generic statistics
#' @return Sample variance S_G^2
S_G_squared <- function(G) {
  n_sim <- length(G)
  sum((G - mean(G))^2) / (n_sim - 1)
}

#' @rdname performance_metrics
#' @export
bias <- function(theta_hat, theta) {
  # sum(theta_hat) / length(theta_hat) - theta
  # uses 'theta_hat - theta' in case theta differs across settings
  sum(theta_hat - theta) / length(theta_hat)
}

#' @rdname performance_metrics
#' @export
bias_mcse <- function(theta_hat) {
  n_sim <- length(theta_hat)
  S_theta_sq <- S_theta_squared(theta_hat)
  sqrt(S_theta_sq / n_sim)
}

#' @rdname performance_metrics
#' @export
relative_bias <- function(theta_hat, theta) {
  # Return NaN if any theta is 0 (division by zero)
  if (any(theta == 0)) {
    return(NaN)
  }
  # (sum(theta_hat) / length(theta_hat) - theta) / theta
  # uses 'theta_hat - theta' in case theta differs across settings
  sum(theta_hat - theta) / length(theta_hat) / theta
}

#' @rdname performance_metrics
#' @export
relative_bias_mcse <- function(theta_hat, theta) {
  # Return NaN if any theta is 0 (division by zero)
  if (any(theta == 0)) {
    return(NaN)
  }
  n_sim <- length(theta_hat)
  S_theta_sq <- S_theta_squared(theta_hat)
  sqrt(S_theta_sq / (theta^2 * n_sim))
}

#' @rdname performance_metrics
#' @export
mse <- function(theta_hat, theta) {
  sum((theta_hat - theta)^2) / length(theta_hat)
}

#' @rdname performance_metrics
#' @export
mse_mcse <- function(theta_hat, theta) {
  n_sim <- length(theta_hat)
  S_theta_minus_theta_sq <- S_theta_minus_theta_squared(theta_hat, theta)
  sqrt(S_theta_minus_theta_sq / n_sim)
}

#' @rdname performance_metrics
#' @export
rmse <- function(theta_hat, theta) {
  sqrt(sum((theta_hat - theta)^2) / length(theta_hat))
}

#' @rdname performance_metrics
#' @export
rmse_mcse <- function(theta_hat, theta) {
  n_sim <- length(theta_hat)
  mse_val <- mean((theta_hat - theta)^2)
  S_theta_minus_theta_sq <- S_theta_minus_theta_squared(theta_hat, theta)
  sqrt(S_theta_minus_theta_sq / (4 * mse_val * n_sim))
}

#' @rdname performance_metrics
#' @export
empirical_variance <- function(theta_hat) {
  S_theta_squared(theta_hat)
}

#' @rdname performance_metrics
#' @export
empirical_variance_mcse <- function(theta_hat) {
  n_sim <- length(theta_hat)
  S_theta_sq <- S_theta_squared(theta_hat)
  sqrt(2 * S_theta_sq^2 / (n_sim - 1))
}

#' @rdname performance_metrics
#' @export
empirical_se <- function(theta_hat) {
  sqrt(S_theta_squared(theta_hat))
}

#' @rdname performance_metrics
#' @export
empirical_se_mcse <- function(theta_hat) {
  n_sim <- length(theta_hat)
  S_theta_sq <- S_theta_squared(theta_hat)
  sqrt(S_theta_sq / (2 * (n_sim - 1)))
}

#' @rdname performance_metrics
#' @export
coverage <- function(ci_lower, ci_upper, theta) {
  ci_includes_theta <- (ci_lower <= theta) & (ci_upper >= theta)
  sum(ci_includes_theta) / length(ci_includes_theta)
}

#' @rdname performance_metrics
#' @export
coverage_mcse <- function(ci_lower, ci_upper, theta) {
  ci_includes_theta <- (ci_lower <= theta) & (ci_upper >= theta)
  n_sim <- length(ci_includes_theta)
  cov_val <- mean(ci_includes_theta)
  sqrt(cov_val * (1 - cov_val) / n_sim)
}

#' @rdname performance_metrics
#' @export
power <- function(test_rejects_h0) {
  sum(test_rejects_h0) / length(test_rejects_h0)
}

#' @rdname performance_metrics
#' @export
power_mcse <- function(test_rejects_h0) {
  n_sim <- length(test_rejects_h0)
  pow_val <- mean(test_rejects_h0)
  sqrt(pow_val * (1 - pow_val) / n_sim)
}

#' @rdname performance_metrics
#' @export
mean_ci_width <- function(ci_upper, ci_lower) {
  sum(ci_upper - ci_lower) / length(ci_upper)
}

#' @rdname performance_metrics
#' @export
mean_ci_width_mcse <- function(ci_upper, ci_lower) {
  n_sim <- length(ci_upper)
  S_w_sq <- S_w_squared(ci_upper, ci_lower)
  sqrt(S_w_sq / n_sim)
}

#' @rdname performance_metrics
#' @export
mean_generic_statistic <- function(G) {
  sum(G) / length(G)
}

#' @rdname performance_metrics
#' @export
mean_generic_statistic_mcse <- function(G) {
  n_sim <- length(G)
  S_G_sq <- S_G_squared(G)
  sqrt(S_G_sq / n_sim)
}



