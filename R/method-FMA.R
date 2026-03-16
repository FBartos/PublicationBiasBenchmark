#' @title Fixed Effects Meta-Analysis Method
#'
#' @author František Bartoš \email{f.bartos96@@gmail.com}
#'
#' @description
#' Implements the publication bias-unadjusted fixed effects meta-analysis.
#'
#' @param method_name Method name (automatically passed)
#' @param data Data frame with yi (effect sizes) and sei (standard errors)
#' @param settings List of method settings (see Details)
#'
#' @return Data frame with FMA results
#'
#' @details
#' The following settings are implemented \describe{
#'   \item{\code{"default"}}{T-distribution adjustment (\code{test = "t"})
#'        and cluster robust standard errors with small-sample adjustment
#'        (if converged, otherwise no small-sample adjustment or no cluster robust
#'        standard errors) for fixed effects meta-analysis if
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
#' # Apply FMA method
#' result <- run_method("FMA", data)
#' print(result)
#'
#' @importFrom clubSandwich vcovCR
#' @export
method.FMA <- function(method_name, data, settings) {

  # Fit FMA

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
  if (length(effect_sizes) < 2)
    stop("At least 2 estimates required for FMA analysis", call. = FALSE)

  # Create a model call based on the settings
  # FMA settings contain the function call extension
  # - only data needs to be added to the call

  settings$yi  <- effect_sizes
  settings$sei <- standard_errors

  # Call the model
  fma_model <- do.call(metafor::rma.uni, settings)

  # Dispatch single vs. multilevel settings
  if (is.null(study_ids) || length(unique(study_ids)) == nrow(data)) {

    fma_est   <- fma_model

  } else {

    # Ensure clubSandwich is available for robust SE estimation
    if (!requireNamespace("clubSandwich", quietly = TRUE))
      stop("Package 'clubSandwich' is required for cluster-robust standard errors.")

    fma_est    <- try(metafor::robust(fma_model, cluster = study_ids, clubSandwich = TRUE))
    if (inherits(fma_est, "try-error")) {
      fma_est <- try(metafor::robust(fma_model, cluster = study_ids, clubSandwich = FALSE))
    }
    if (inherits(fma_est, "try-error")) {
      fma_est <- try(metafor::robust(fma_model, cluster = study_ids, adjust = FALSE))
    }
    if (inherits(fma_est, "try-error")) {
      fma_est <- fma_model
    }
  }


  # Extract results
  estimate     <- fma_est$beta[1]
  estimate_se  <- fma_est$se[1]
  estimate_lci <- fma_est$ci.lb[1]
  estimate_uci <- fma_est$ci.ub[1]
  estimate_p   <- fma_est$pval[1]
  tau_p_value  <- fma_model$QEp

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
    tau_p_value      = tau_p_value
  ))
}

#' @export
method_settings.FMA <- function(method_name) {

  settings <- list(
    "default" = list(method = "FE", test = "t")
  )

  return(settings)
}

#' @export
method_extra_columns.FMA <- function(method_name)
  c("tau_p_value")
