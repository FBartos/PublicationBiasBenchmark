#' @title Performance Measures and Monte Carlo Standard Errors
#'
#' @description
#' A comprehensive set of functions for computing performance measures and their
#' Monte Carlo Standard Errors (MCSE) for simulation studies. All functions are
#' based on definitions from Table 3 in
#' \insertCite{siepe2024simulation;textual}{PublicationBiasBenchmark}. Winkler
#' interval score is defined in \insertCite{winkler1972decision;textual}{PublicationBiasBenchmark}.
#' Positive and negative likelihood ratios are defined in
#' \insertCite{huang2023relative;textual}{PublicationBiasBenchmark} and
#' \insertCite{deeks2004diagnostic;textual}{PublicationBiasBenchmark}. Also see
#' \insertCite{morris2019using;textual}{PublicationBiasBenchmark} for additional
#' details. Bias and relative bias were modified to account for possibly different
#' true values across repetitions.
#'
#' @name measures
#' @aliases bias bias_mcse relative_bias relative_bias_mcse mse mse_mcse rmse rmse_mcse
#' @aliases empirical_variance empirical_variance_mcse empirical_se empirical_se_mcse
#' @aliases coverage coverage_mcse mean_ci_width mean_ci_width_mcse interval_score interval_score_mcse
#' @aliases power power_mcse
#' @aliases positive_likelihood_ratio positive_likelihood_ratio_mcse
#' @aliases negative_likelihood_ratio negative_likelihood_ratio_mcse
#' @aliases mean_generic_statistic mean_generic_statistic_mcse
#'
#' @details
#' The package provides the following performance measures and their corresponding MCSE functions:
#'
#' \itemize{
#'   \item \code{bias(theta_hat, theta)}: Bias estimate
#'   \item \code{relative_bias(theta_hat, theta)}: Relative bias estimate
#'   \item \code{mse(theta_hat, theta)}: Mean Square Error
#'   \item \code{rmse(theta_hat, theta)}: Root Mean Square Error
#'   \item \code{empirical_variance(theta_hat)}: Empirical variance
#'   \item \code{empirical_se(theta_hat)}: Empirical standard error
#'   \item \code{coverage(ci_lower, ci_upper, theta)}: Coverage probability
#'   \item \code{mean_ci_width(ci_upper, ci_lower)}: Mean confidence interval width
#'   \item \code{interval_score(ci_lower, ci_upper, theta, alpha)}: interval_score
#'   \item \code{power(test_rejects_h0)}: Statistical power
#'   \item \code{positive_likelihood_ratio(tp, fp, fn, tn)}: Log positive likelihood ratio
#'   \item \code{negative_likelihood_ratio(tp, fp, fn, tn)}: Log negative likelihood ratio
#'   \item \code{mean_generic_statistic(G)}: Mean of any generic statistic
#' }
#'
#' @param theta_hat Vector of parameter estimates from simulations
#' @param theta True parameter value
#' @param ci_lower Vector of lower confidence interval bounds
#' @param ci_upper Vector of upper confidence interval bounds
#' @param test_rejects_h0 Logical vector indicating whether statistical tests reject the null hypothesis
#' @param tp Numeric with the count of true positive hypothesis tests
#' @param fp Numeric with the count of false positive hypothesis tests
#' @param tn Numeric with the count of true negative hypothesis tests
#' @param fn Numeric with the count of false negative hypothesis tests
#' @param alpha Numeric indicating the 1 - coverage level for interval_score calculation
#' @param G Vector of generic statistics from simulations
#'
#' @return
#' Each metric function returns a numeric value representing the performance measure.
#' Each MCSE function returns a numeric value representing the Monte Carlo standard error.
#'
#' @references
#' \insertAllCited{}
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
#' @keywords internal
S_theta_squared <- function(theta_hat) {
  n_sim <- length(theta_hat)
  sum((theta_hat - mean(theta_hat))^2) / (n_sim - 1)
}

#' Calculate sample variance of squared errors
#' @param theta_hat Vector of estimates
#' @param theta True parameter value
#' @return Sample variance S_(theta_hat - theta)^2
#' @keywords internal
S_theta_minus_theta_squared <- function(theta_hat, theta) {
  n_sim <- length(theta_hat)
  squared_errors <- (theta_hat - theta)^2
  sum((squared_errors - mean(squared_errors))^2) / (n_sim - 1)
}

#' Calculate sample variance of CI widths
#' @param ci_upper Vector of upper CI bounds
#' @param ci_lower Vector of lower CI bounds
#' @return Sample variance S_w^2
#' @keywords internal
S_w_squared <- function(ci_upper, ci_lower) {
  n_sim <- length(ci_upper)
  ci_widths <- ci_upper - ci_lower
  sum((ci_widths - mean(ci_widths))^2) / (n_sim - 1)
}

#' Calculate sample variance of generic statistic
#' @param G Vector of generic statistics
#' @return Sample variance S_G^2
#' @keywords internal
S_G_squared <- function(G) {
  n_sim <- length(G)
  sum((G - mean(G))^2) / (n_sim - 1)
}

#' @rdname measures
#' @export
bias <- function(theta_hat, theta) {
  # sum(theta_hat) / length(theta_hat) - theta
  # uses 'theta_hat - theta' in case theta differs across settings
  sum(theta_hat - theta) / length(theta_hat)
}

#' @rdname measures
#' @export
bias_mcse <- function(theta_hat) {
  n_sim <- length(theta_hat)
  S_theta_sq <- S_theta_squared(theta_hat)
  sqrt(S_theta_sq / n_sim)
}

#' @rdname measures
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

#' @rdname measures
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

#' @rdname measures
#' @export
mse <- function(theta_hat, theta) {
  sum((theta_hat - theta)^2) / length(theta_hat)
}

#' @rdname measures
#' @export
mse_mcse <- function(theta_hat, theta) {
  n_sim <- length(theta_hat)
  S_theta_minus_theta_sq <- S_theta_minus_theta_squared(theta_hat, theta)
  sqrt(S_theta_minus_theta_sq / n_sim)
}

#' @rdname measures
#' @export
rmse <- function(theta_hat, theta) {
  sqrt(sum((theta_hat - theta)^2) / length(theta_hat))
}

#' @rdname measures
#' @export
rmse_mcse <- function(theta_hat, theta) {
  n_sim <- length(theta_hat)
  mse_val <- mean((theta_hat - theta)^2)
  S_theta_minus_theta_sq <- S_theta_minus_theta_squared(theta_hat, theta)
  sqrt(S_theta_minus_theta_sq / (4 * mse_val * n_sim))
}

#' @rdname measures
#' @export
empirical_variance <- function(theta_hat) {
  S_theta_squared(theta_hat)
}

#' @rdname measures
#' @export
empirical_variance_mcse <- function(theta_hat) {
  n_sim <- length(theta_hat)
  S_theta_sq <- S_theta_squared(theta_hat)
  sqrt(2 * S_theta_sq^2 / (n_sim - 1))
}

#' @rdname measures
#' @export
empirical_se <- function(theta_hat) {
  sqrt(S_theta_squared(theta_hat))
}

#' @rdname measures
#' @export
empirical_se_mcse <- function(theta_hat) {
  n_sim <- length(theta_hat)
  S_theta_sq <- S_theta_squared(theta_hat)
  sqrt(S_theta_sq / (2 * (n_sim - 1)))
}

#' @rdname measures
#' @export
coverage <- function(ci_lower, ci_upper, theta) {
  ci_includes_theta <- (ci_lower <= theta) & (ci_upper >= theta)
  sum(ci_includes_theta) / length(ci_includes_theta)
}

#' @rdname measures
#' @export
coverage_mcse <- function(ci_lower, ci_upper, theta) {
  ci_includes_theta <- (ci_lower <= theta) & (ci_upper >= theta)
  n_sim <- length(ci_includes_theta)
  cov_val <- mean(ci_includes_theta)
  sqrt(cov_val * (1 - cov_val) / n_sim)
}

#' @rdname measures
#' @export
power <- function(test_rejects_h0) {
  sum(test_rejects_h0) / length(test_rejects_h0)
}

#' @rdname measures
#' @export
power_mcse <- function(test_rejects_h0) {
  n_sim <- length(test_rejects_h0)
  pow_val <- mean(test_rejects_h0)
  sqrt(pow_val * (1 - pow_val) / n_sim)
}

#' @rdname measures
#' @export
mean_ci_width <- function(ci_upper, ci_lower) {
  sum(ci_upper - ci_lower) / length(ci_upper)
}

#' @rdname measures
#' @export
mean_ci_width_mcse <- function(ci_upper, ci_lower) {
  n_sim <- length(ci_upper)
  S_w_sq <- S_w_squared(ci_upper, ci_lower)
  sqrt(S_w_sq / n_sim)
}

#' @rdname measures
#' @export
mean_generic_statistic <- function(G) {
  sum(G) / length(G)
}

#' @rdname measures
#' @export
mean_generic_statistic_mcse <- function(G) {
  n_sim <- length(G)
  S_G_sq <- S_G_squared(G)
  sqrt(S_G_sq / n_sim)
}

#' @rdname measures
#' @export
positive_likelihood_ratio <- function(tp, fp, fn, tn) {
  if(tp == 0|| fp == 0 || tn == 0 || fn == 0) {
    tp <- tp + 0.5
    fp <- fp + 0.5
    tn <- tn + 0.5
    fn <- fn + 0.5
  }
  power <- tp/(tp + fn)
  t1er <- fp/(fp + tn)
  log(power)-log(t1er)
}

#' @rdname measures
#' @export
positive_likelihood_ratio_mcse <- function(tp, fp, fn, tn) {
  if(tp == 0|| fp == 0 || tn == 0 || fn == 0) {
    tp <- tp + 0.5
    fp <- fp + 0.5
    tn <- tn + 0.5
    fn <- fn + 0.5
  }
  sqrt(1/tp - 1/(tp + fn) + 1/fp - 1/(fp + tn))
}

#' @rdname measures
#' @export
negative_likelihood_ratio <- function(tp, fp, fn, tn) {
  if(tp == 0|| fp == 0 || tn == 0 || fn == 0) {
    tp <- tp + 0.5
    fp <- fp + 0.5
    tn <- tn + 0.5
    fn <- fn + 0.5
  }
  power <- tp/(tp + fn)
  t1er <- fp/(fp + tn)
  log1p(-power)-log1p(-t1er)
}

#' @rdname measures
#' @export
negative_likelihood_ratio_mcse <- function(tp, fp, fn, tn) {
  if(tp == 0|| fp == 0 || tn == 0 || fn == 0) {
    tp <- tp + 0.5
    fp <- fp + 0.5
    tn <- tn + 0.5
    fn <- fn + 0.5
  }
  sqrt(1/tn - 1/(tp + fn) + 1/fn - 1/(fp + tn))
}

#' @rdname measures
#' @export
interval_score <- function(ci_lower, ci_upper, theta, alpha = 0.05) {

  if (length(theta) == 1) # allow for different thetas across samples
    theta <- rep(theta, length(ci_lower))

  score <- rep(NA, length(ci_lower))
  theta_lower <- theta < ci_lower
  theta_higher <- theta > ci_upper
  theta_cover <- ci_lower <= theta & theta <= ci_upper
  score[theta_lower] <- (ci_upper[theta_lower] - ci_lower[theta_lower]) + (2/alpha) * (ci_lower[theta_lower] - theta[theta_lower])
  score[theta_higher] <- (ci_upper[theta_higher] - ci_lower[theta_higher]) + (2/alpha) * (theta[theta_higher] - ci_upper[theta_higher])
  score[theta_cover] <- (ci_upper[theta_cover] - ci_lower[theta_cover])

  mean(score)
}

#' @rdname measures
#' @export
interval_score_mcse <- function(ci_lower, ci_upper, theta, alpha = 0.05) {

  if (length(theta) == 1) # allow for different thetas across samples
    theta <- rep(theta, length(ci_lower))

  score <- rep(NA, length(ci_lower))
  theta_lower <- theta < ci_lower
  theta_higher <- theta > ci_upper
  theta_cover <- ci_lower <= theta & theta <= ci_upper
  score[theta_lower] <- (ci_upper[theta_lower] - ci_lower[theta_lower]) + (2/alpha) * (ci_lower[theta_lower] - theta[theta_lower])
  score[theta_higher] <- (ci_upper[theta_higher] - ci_lower[theta_higher]) + (2/alpha) * (theta[theta_higher] - ci_upper[theta_higher])
  score[theta_cover] <- (ci_upper[theta_cover] - ci_lower[theta_cover])

  mean_generic_statistic_mcse(score)
}

#' @title Get performance measure function
#'
#' @description
#' Returns the function for computing a specific performance measure.
#' Can also be used to list available measures.
#'
#' @param measure Character string specifying the measure name. If missing, returns a vector of all available measures.
#' @param ... Additional arguments passed to methods.
#'
#' @return A function that computes the requested measure, or a character vector of measure names.
#' @export
measure <- function(measure, ...) {
  if (missing(measure)) {
    m <- as.character(utils::methods("measure"))
    m <- gsub("^measure\\.", "", m)
    return(m[m != "default"])
  }

  if (missing(...)) {
    if (is.character(measure)) {
      return(utils::getS3method("measure", measure)())
    }
  }

  if (is.character(measure)) {
    # Create a copy to avoid modifying the original object if it's not just a string
    m <- measure
    class(m) <- measure
    UseMethod("measure", m)
  } else {
    UseMethod("measure")
  }
}

#' @export
measure.bias <- function(measure, ...) bias
#' @export
measure.relative_bias <- function(measure, ...) relative_bias
#' @export
measure.mse <- function(measure, ...) mse
#' @export
measure.rmse <- function(measure, ...) rmse
#' @export
measure.empirical_variance <- function(measure, ...) empirical_variance
#' @export
measure.empirical_se <- function(measure, ...) empirical_se
#' @export
measure.coverage <- function(measure, ...) coverage
#' @export
measure.power <- function(measure, ...) power
#' @export
measure.mean_ci_width <- function(measure, ...) mean_ci_width
#' @export
measure.interval_score <- function(measure, ...) interval_score
#' @export
measure.convergence <- function(measure, ...) power
#' @export
measure.positive_likelihood_ratio <- function(measure, ...) positive_likelihood_ratio
#' @export
measure.negative_likelihood_ratio <- function(measure, ...) negative_likelihood_ratio
#' @export
measure.default <- function(measure, ...) {
  stop(paste0("Unknown measure: ", measure), call. = FALSE)
}

#' @title Get performance measure MCSE function
#'
#' @description
#' Returns the function for computing the Monte Carlo Standard Error (MCSE) of a specific performance measure.
#' Can also be used to list available measures with MCSE functions.
#'
#' @param measure Character string specifying the measure name. If missing, returns a vector of all available measures.
#' @param ... Additional arguments passed to methods.
#'
#' @return A function that computes the MCSE of the requested measure, or a character vector of measure names.
#' @export
measure_mcse <- function(measure, ...) {
  if (missing(measure)) {
    m <- as.character(utils::methods("measure_mcse"))
    m <- gsub("^measure_mcse\\.", "", m)
    return(m[m != "default"])
  }

  if (missing(...)) {
    if (is.character(measure)) {
      return(utils::getS3method("measure_mcse", measure)())
    }
  }

  if (is.character(measure)) {
    # Create a copy to avoid modifying the original object if it's not just a string
    m <- measure
    class(m) <- measure
    UseMethod("measure_mcse", m)
  } else {
    UseMethod("measure_mcse")
  }
}

#' @export
measure_mcse.bias <- function(measure, ...) bias_mcse
#' @export
measure_mcse.relative_bias <- function(measure, ...) relative_bias_mcse
#' @export
measure_mcse.mse <- function(measure, ...) mse_mcse
#' @export
measure_mcse.rmse <- function(measure, ...) rmse_mcse
#' @export
measure_mcse.empirical_variance <- function(measure, ...) empirical_variance_mcse
#' @export
measure_mcse.empirical_se <- function(measure, ...) empirical_se_mcse
#' @export
measure_mcse.coverage <- function(measure, ...) coverage_mcse
#' @export
measure_mcse.power <- function(measure, ...) power_mcse
#' @export
measure_mcse.mean_ci_width <- function(measure, ...) mean_ci_width_mcse
#' @export
measure_mcse.interval_score <- function(measure, ...) interval_score_mcse
#' @export
measure_mcse.convergence <- function(measure, ...) power_mcse
#' @export
measure_mcse.positive_likelihood_ratio <- function(measure, ...) positive_likelihood_ratio_mcse
#' @export
measure_mcse.negative_likelihood_ratio <- function(measure, ...) negative_likelihood_ratio_mcse
#' @export
measure_mcse.default <- function(measure, ...) {
  stop(paste0("Unknown measure: ", measure), call. = FALSE)
}
