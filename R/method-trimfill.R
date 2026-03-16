#' @title Trim-and-Fill Meta-Analysis Method
#'
#' @author František Bartoš \email{f.bartos96@@gmail.com}
#'
#' @description
#' Implements the trim-and-fill method for adjusting publication bias
#' in meta-analysis using the metafor package.
#'
#' @param method_name Method name (automatically passed)
#' @param data Data frame with yi (effect sizes) and sei (standard errors)
#' @param settings List of method settings (see Details.)
#'
#' @return Data frame with trim-and-fill results
#'
#' @details
#' The following settings are implemented \describe{
#'   \item{\code{"default"}}{Random effects model fitted with Restricted Maximum
#'        Likelihood estimator (\code{method = "REML"}) with Knapp-Hartung
#'        adjustment (\code{test = "knha"}), followed by trim-and-fill using
#'        left-side trimming (\code{side = "left"}) and L0 estimator
#'        (\code{estimator = "L0"}).}
#' }
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
#' # Apply trimfill method
#' result <- run_method("trimfill", data)
#' print(result)
#'
#' @export
method.trimfill <- function(method_name, data, settings) {

  # Extract data
  effect_sizes    <- data$yi
  standard_errors <- data$sei

  # Check input
  if (length(effect_sizes) < 3)
    stop("At least 3 estimates required for trimfill analysis", call. = FALSE)

  # Create a model call based on the RMA settings
  # RMA settings contain the function call extension
  # - only data needs to be added to the call
  rma_settings <- settings[["rma"]]
  rma_settings$yi  <- effect_sizes
  rma_settings$sei <- standard_errors

  # Fit initial RMA model
  rma_model <- do.call(metafor::rma.uni, rma_settings)

  # Apply trim-and-fill method
  trimfill_settings <- settings[["trimfill"]]
  trimfill_model    <- do.call(metafor::trimfill, c(list(x = rma_model), trimfill_settings))

  # Extract results from trim-and-fill model
  estimate     <- trimfill_model$beta[1]
  estimate_se  <- trimfill_model$se[1]
  estimate_lci <- trimfill_model$ci.lb[1]
  estimate_uci <- trimfill_model$ci.ub[1]
  estimate_p   <- trimfill_model$pval[1]

  tau_estimate <- sqrt(trimfill_model$tau2)
  tau_p_value  <- trimfill_model$QEp
  taus <- try(stats::confint(trimfill_model))
  if (inherits(taus, "try-error")) {
    tau_ci_lower <- NA
    tau_ci_upper <- NA
  } else {
    tau_ci_lower <- taus$random["tau","ci.lb"]
    tau_ci_upper <- taus$random["tau","ci.ub"]
  }

  k_missing    <- trimfill_model$k0
  k_missing_se <- trimfill_model$se.k0

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
    tau_p_value      = tau_p_value,
    k_missing        = k_missing,
    k_missing_se     = k_missing_se
  ))
}

#' @export
method_settings.trimfill <- function(method_name) {

  settings <- list(
    "default" = list(
      "rma"      = list(method = "REML", test = "knha", control = list(stepadj = 0.5, maxiter = 500)),
      "trimfill" = list(side = "left", estimator = "L0", maxiter = 500)
    )
  )

  return(settings)
}


#' @export
method_extra_columns.trimfill <- function(method_name)
  c("tau_estimate", "tau_ci_lower", "tau_ci_upper", "tau_p_value", "k_missing", "k_missing_se")

