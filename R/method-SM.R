#' @title SM (Selection Models) Method
#'
#' @author František Bartoš \email{f.bartos96@@gmail.com}
#'
#' @description
#' Implements selection models for publication bias correction in meta-analysis.
#' The method first fits a random effects meta-analysis model, then applies
#' selection modeling to adjust for publication bias using the metafor package.
#' Selection models account for the probability that studies are published
#' based on their p-values or effect sizes. See
#' \insertCite{vevea1995general;textual}{PublicationBiasBenchmark} for details.
#'
#' @param method_name Method name (automatically passed)
#' @param data Data frame with yi (effect sizes) and sei (standard errors)
#' @param settings List of method settings (see Details)
#'
#' @return Data frame with SM results
#'
#' @details
#' The following settings are implemented \describe{
#'   \item{\code{"default"} or \code{"3PSM"}}{3-parameter step function selection model with
#'        Maximum Likelihood estimator (\code{method = "ML"}) and one step
#'        at one-sided p = 0.025 (i.e., selection for significance))}
#'   \item{\code{"4PSM"}}{4-parameter step function selection model with
#'        Maximum Likelihood estimator (\code{method = "ML"}) and two steps
#'        at one-sided p = 0.025 and p = 0.50 (i.e., selection for significance
#'        and direction of the effect)}
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
#' # Apply SM method
#' result <- run_method("SM", data, "3PSM")
#' print(result)
#'
#' @export
method.SM <- function(method_name, data, settings) {

  # Extract data
  effect_sizes    <- data$yi
  standard_errors <- data$sei

  # Check input
  if (length(effect_sizes) < 3)
    stop("At least 3 estimates required for SM analysis", call. = FALSE)

  # Prepare RMA settings and add data
  rma_settings <- settings[["rma"]]
  rma_settings$yi  <- effect_sizes
  rma_settings$sei <- standard_errors

  # Fit initial RMA model
  rma_model <- do.call(metafor::rma.uni, rma_settings)

  # Apply selection model
  selmodel_settings <- settings[["selmodel"]]
  sm_model <- do.call(metafor::selmodel, c(list(x = rma_model), selmodel_settings))

  # Extract results from selection model
  estimate     <- sm_model$beta[1]
  estimate_se  <- sm_model$se[1]
  estimate_lci <- sm_model$ci.lb[1]
  estimate_uci <- sm_model$ci.ub[1]
  estimate_p   <- sm_model$pval[1]

  tau_estimate <- sqrt(sm_model$tau2)
  tau_p_value  <- sm_model$LRTp.tau2
  taus <- try(stats::confint(sm_model))
  if (inherits(taus, "try-error")) {
    tau_ci_lower <- NA
    tau_ci_upper <- NA
  } else {
    tau_ci_lower <- taus[[1]]$random["tau","ci.lb"]
    tau_ci_upper <- taus[[1]]$random["tau","ci.ub"]
  }

  bias_coefficient    <- sm_model$delta
  bias_coefficient_se <- sm_model$se.delta
  bias_p_value        <- sm_model$LRTp

  convergence <- TRUE
  note        <- NA

  return(data.frame(
    method              = method_name,
    estimate            = estimate,
    standard_error      = estimate_se,
    ci_lower            = estimate_lci,
    ci_upper            = estimate_uci,
    p_value             = estimate_p,
    BF                  = NA,
    convergence         = convergence,
    note                = note,
    tau_estimate        = tau_estimate,
    tau_ci_lower        = tau_ci_lower,
    tau_ci_upper        = tau_ci_upper,
    tau_p_value         = tau_p_value,
    bias_coefficient    = paste0(bias_coefficient, collapse = ", "),
    bias_coefficient_se = paste0(bias_coefficient_se, collapse = ", "),
    bias_p_value        = bias_p_value
  ))
}

#' @export
method_settings.SM <- function(method_name) {

  settings <- list(
    # default = SM3
    "default" = list(
      "rma"      = list(method = "ML", test = "knha", control = list(stepadj = 0.5, maxiter = 500)),
      "selmodel" = list(type = "stepfun", steps = c(0.025))
    ),
    "3PSM" = list(
      "rma"      = list(method = "ML", test = "knha", control = list(stepadj = 0.5, maxiter = 500)),
      "selmodel" = list(type = "stepfun", steps = c(0.025))
    ),
    "4PSM" = list(
      "rma"      = list(method = "ML", test = "knha", control = list(stepadj = 0.5, maxiter = 500)),
      "selmodel" = list(type = "stepfun", steps = c(0.025, 0.50))
    )
  )

  return(settings)
}

#' @export
method_extra_columns.SM <- function(method_name)
  c("tau_estimate", "tau_ci_lower", "tau_ci_upper", "tau_p_value", "bias_coefficient", "bias_coefficient_se", "bias_p_value")
