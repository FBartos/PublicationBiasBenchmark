#' @title Random Effects Meta-Analysis Method
#'
#' @description
#' Implements the publication bias-unadjusted random effects meta-analysis.
#'
#' @param method_name Method name (automatically passed)
#' @param data Data frame with yi (effect sizes) and sei (standard errors)
#' @param settings List of method settings (currently unused)
#'
#' @return Data frame with RMA results
#'
#' @details
#' The default settings uses Restricted Maximum Likelihood estimator
#' (\code{method = "REML"}) with Knapp-Hartung adjustment
#' (\code{test = "knha"}).
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
#' result <- method("RMA", data)
#' print(result)
#'
#' @export
method.RMA <- function(method_name, data, settings = NULL) {

  # Extract data
  effect_sizes    <- data$yi
  standard_errors <- data$sei

  if (length(effect_sizes) < 3) {
    return(create_empty_result(
      method_name = method_name,
      note        = "At least 2 studies required for RMA analysis",
      extra_columns = list(
        tau_estimate = NA,
        tau_ci_lower = NA,
        tau_ci_upper = NA,
        tau_p_value  = NA
      )
    ))
  }

  # Fit RMA
  result <- tryCatch({

    rma_model <- metafor::rma.uni(yi = effect_sizes, sei = standard_errors,
                                  method = "REML", test = "knha")

    # Extract results
    estimate     <- rma_model$beta[1]
    estimate_se  <- rma_model$se[1]
    estimate_lci <- rma_model$ci.lb[1]
    estimate_uci <- rma_model$ci.ub[1]
    estimate_p   <- rma_model$pval[1]

    tau_estimate <- sqrt(rma_model$tau2)
    tau_p_value  <- rma_model$QEp
    taus <- try(confint(rma_model))
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

  }, error = function(e) {

    return(create_empty_result(
      method_name = method_name,
      note        = paste("Model fitting failed:", e$message),
      extra_columns = list(
        tau_estimate = NA,
        tau_ci_lower = NA,
        tau_ci_upper = NA,
        tau_p_value  = NA
      )
    ))

  })

  return(result)
}
