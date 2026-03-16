#' @title Random Effects Meta-Analysis Method
#'
#' @author František Bartoš \email{f.bartos96@@gmail.com}
#'
#' @description
#' Implements the publication bias-unadjusted random-effects meta-analysis.
#'
#' @param method_name Method name (automatically passed)
#' @param data Data frame with yi (effect sizes) and sei (standard errors)
#' @param settings List of method settings (see Details)
#'
#' @return Data frame with RMA results
#'
#' @details
#' The following settings are implemented \describe{
#'   \item{\code{"default"}}{Restricted Maximum Likelihood estimator
#'        (\code{method = "REML"}) with Knapp-Hartung adjustment
#'        (\code{test = "knha"}) for a simple random effects meta-analysis
#'        and Restricted Maximum Likelihood estimator
#'        (\code{method = "REML"}) with t-distribution adjustment (\code{test = "t"})
#'        and cluster robust standard errors with small-sample adjustment
#'        (if converged, otherwise no small-sample adjustment or no cluster robust
#'        standard errors) for a multilevel random effects meta-analysis if
#'        \code{study_ids} is specified in the data}
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
#' # Apply RMA method
#' result <- run_method("RMA", data)
#' print(result)
#'
#' @importFrom clubSandwich vcovCR
#' @export
method.RMA <- function(method_name, data, settings) {

  # Fit RMA

  # Extract data
  effect_sizes    <- data$yi
  standard_errors <- data$sei

  # Use clustering wherever available
  if (is.null(data[["study_id"]])) {
    study_ids <- NULL
  } else {
    study_ids <- data[["study_id"]]
  }

  # Check input
  if (length(effect_sizes) < 3)
    stop("At least 3 estimates required for RMA analysis", call. = FALSE)

  # Create a model call based on the settings
  # RMA settings contain the function call extension
  # - only data needs to be added to the call

  # Dispatch single vs. multilevel settings
  if (is.null(study_ids) || length(unique(study_ids)) == nrow(data)) {

    settings$yi  <- effect_sizes
    settings$sei <- standard_errors

    settings[["test"]]     <- settings[["test.uni"]]
    settings[["test.uni"]] <- NULL
    settings[["test.mv"]]  <- NULL

    # Call the model
    rma_model <- do.call(metafor::rma.uni, settings)
    rma_est   <- rma_model

    # get tau estimates
    taus <- try(stats::confint(rma_model))
    if (inherits(taus, "try-error")) {
      tau_ci_lower <- NA
      tau_ci_upper <- NA
    } else {
      tau_ci_lower <- taus$random["tau","ci.lb"]
      tau_ci_upper <- taus$random["tau","ci.ub"]
    }

  } else {

    settings$yi  <- effect_sizes
    settings$V   <- standard_errors^2

    settings[["test"]]     <- settings[["test.mv"]]
    settings[["test.uni"]] <- NULL
    settings[["test.mv"]]  <- NULL
    settings[["dfs"]]      <- "contain"

    effect_ids <- seq_along(study_ids)
    settings$random <- ~ effect_ids | study_ids

    # Call the model
    rma_model  <- do.call(metafor::rma.mv, settings)

    # Ensure clubSandwich is available for robust SE estimation
    if (!requireNamespace("clubSandwich", quietly = TRUE))
      stop("Package 'clubSandwich' is required for cluster-robust standard errors.")

    rma_est    <- try(metafor::robust(rma_model, cluster = study_ids, clubSandwich = TRUE))
    if (inherits(rma_est, "try-error")) {
      rma_est <- try(metafor::robust(rma_model, cluster = study_ids, clubSandwich = FALSE))
    }
    if (inherits(rma_est, "try-error")) {
      rma_est <- try(metafor::robust(rma_model, cluster = study_ids, adjust = FALSE))
    }
    if (inherits(rma_est, "try-error")) {
      rma_est <- rma_model
    }

    # skip tau estimation as it often takes 30+ minutes
    # taus <- try(stats::confint(rma_model, tau2 = 1))
    # if (inherits(taus, "try-error")) {
      tau_ci_lower <- NA
      tau_ci_upper <- NA
    # } else {
    #   tau_ci_lower <- taus$random["tau","ci.lb"]
    #   tau_ci_upper <- taus$random["tau","ci.ub"]
    # }

  }


  # Extract results
  estimate     <- rma_est$beta[1]
  estimate_se  <- rma_est$se[1]
  estimate_lci <- rma_est$ci.lb[1]
  estimate_uci <- rma_est$ci.ub[1]
  estimate_p   <- rma_est$pval[1]

  tau_estimate <- sqrt(rma_model$tau2)
  tau_p_value  <- rma_model$QEp

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
    "default" = list(method = "REML", test.uni = "knha", test.mv = "t", control = list(stepadj = 0.5, maxiter = 500))
  )

  return(settings)
}

#' @export
method_extra_columns.RMA <- function(method_name)
  c("tau_estimate", "tau_ci_lower", "tau_ci_upper", "tau_p_value")
