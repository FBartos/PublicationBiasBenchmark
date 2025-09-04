#' @title Random Effects Meta-Analysis Method
#'
#' @description
#' Implements the publication bias-unadjusted random effects meta-analysis.
#'
#' @param method_name Method name (automatically passed)
#' @param data Data frame with yi (effect sizes) and sei (standard errors)
#' @param settings List of method settings (see Details.)
#'
#' @return Data frame with RMA results
#'
#' @details
#' The following settings are implemented \describe{
#'   \item{\code{"default"}}{Restricted Maximum Likelihood estimator
#'        (\code{method = "REML"}) with Knapp-Hartung adjustment
#'        (\code{test = "knha"}).}
#' }
#'
#'
#' @examples
#' # Generate some example data
#' data <- data.frame(
#'   yi = c(0.2, 0.3, 0.1, 0.4, 0.25),
#'   sei = c(0.1, 0.15, 0.08, 0.12, 0.09)
#' )
#'
#' # Apply PET method
#' result <- run_method("RMA", data)
#' print(result)
#'
#' @export
method.RMA <- function(method_name, data, settings) {

  # Fit RMA

  # Extract data
  effect_sizes    <- data$yi
  standard_errors <- data$sei

  # Check input
  if (length(effect_sizes) < 3)
    stop("At least 3 estimates required for RMA analysis", call. = FALSE)

  # Create a model call based on the settings
  # RMA settings contain the function call extension
  # - only data needs to be added to the call
  settings$yi  <- effect_sizes
  settings$sei <- standard_errors

  # Call the model
  rma_model <- do.call(metafor::rma.uni, settings)

  # Extract results
  estimate     <- rma_model$beta[1]
  estimate_se  <- rma_model$se[1]
  estimate_lci <- rma_model$ci.lb[1]
  estimate_uci <- rma_model$ci.ub[1]
  estimate_p   <- rma_model$pval[1]

  tau_estimate <- sqrt(rma_model$tau2)
  tau_p_value  <- rma_model$QEp
  taus <- try(stats::confint(rma_model))
  if (inherits(taus, "try-error")) {
    tau_ci_lower <- NA
    tau_ci_upper <- NA
  } else {
    tau_ci_lower <- taus$random["tau","ci.lb"]
    tau_ci_upper <- taus$random["tau","ci.ub"]
  }

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
    note             = note,
    tau_estimate     = tau_estimate,
    tau_ci_lower     = tau_ci_lower,
    tau_ci_upper     = tau_ci_upper,
    tau_p_value      = tau_p_value
  ))
}

#' @export
method_settings.RMA <- function(method_name) {

  settings <- list(
    # recommended settings according to metafor with an increased number of iterations for convergence
    "default" = list(method = "REML", test = "knha", control = list(stepadj = 0.5, maxiter = 500))
  )

  return(settings)
}

#' @export
method_extra_columns.RMA <- function(method_name)
  c("tau_estimate", "tau_ci_lower", "tau_ci_upper", "tau_p_value")
