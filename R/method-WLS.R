#' @title WLS (Weighted Least Squares) Method
#'
#' @author František Bartoš \email{f.bartos96@@gmail.com}
#'
#' @description
#' Implements the Weighted Least Squares method for meta-analysis.
#' WLS fits a weighted regression model with effect sizes as the outcome
#' and weights based on the inverse of the squared standard errors.
#' The intercept represents the weighted average effect size estimate.
#'
#' @param method_name Method name (automatically passed)
#' @param data Data frame with yi (effect sizes) and sei (standard errors)
#' @param settings List of method settings (no settings version are implemented)
#'
#' @return Data frame with WLS results
#'
#' @references
#'  \insertAllCited{}
#'
#' @examples
#' # Generate some example data
#' data <- data.frame(
#'   yi = c(0.2, 0.3, 0.1, 0.4, 0.25),
#'   sei = c(0.1, 0.15, 0.08, 0.12, 0.09)
#' )
#'
#' # Apply WLS method
#' result <- run_method("WLS", data)
#' print(result)
#'
#' @export
method.WLS <- function(method_name, data, settings = NULL) {

  # Fit WLS model: effect_size ~ intercept (weighted by 1/sei^2)

  # Extract data
  effect_sizes    <- data$yi
  standard_errors <- data$sei

  # Check input
  if (length(effect_sizes) < 2)
    stop("At least 2 estimates required for WLS analysis", call. = FALSE)

  # Fit weighted least squares model
  wls_model <- stats::lm(effect_sizes ~ 1, weights = 1/standard_errors^2)

  # Extract results
  coefficients    <- stats::coef(wls_model)
  se_coefficients <- summary(wls_model)$coefficients[, "Std. Error"]
  p_values        <- summary(wls_model)$coefficients[, "Pr(>|t|)"]

  # The intercept represents the weighted effect size estimate
  estimate    <- coefficients[1]
  estimate_se <- se_coefficients[1]
  estimate_p  <- p_values[1]

  # Calculate confidence interval
  estimate_lci <- estimate - 1.96 * estimate_se
  estimate_uci <- estimate + 1.96 * estimate_se

  convergence <- TRUE
  note        <- NA

  return(data.frame(
    method           = method_name,
    estimate         = estimate,
    standard_error   = estimate_se,
    ci_lower         = estimate_lci,
    ci_upper         = estimate_uci,
    p_value          = estimate_p,
    BF               = NA,
    convergence      = convergence,
    note             = note
  ))
}

#' @export
method_settings.WLS <- function(method_name) {

  settings <- list(
    "default" = list() # no available settings
  )

  return(settings)
}

#' @export
method_extra_columns.WLS <- function(method_name)
  character(0)
