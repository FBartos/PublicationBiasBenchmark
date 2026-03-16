#' @title WAAPWLS (Weighted Average of Adequately Powered Studies) Method
#'
#' @author František Bartoš \email{f.bartos96@@gmail.com}
#'
#' @description
#' Implements the WAAP-WLS method for meta-analysis, which combines WLS and WAAP approaches.
#' First fits a WLS model to all studies, then identifies high-powered studies based on
#' the criterion that the WLS estimate divided by 2.8 is greater than or equal to the
#' standard error. If at least 2 high-powered studies are found, uses WAAP (weighted
#' average of adequate power studies only), otherwise uses the original WLS estimate.
#' See \insertCite{stanley2017finding;textual}{PublicationBiasBenchmark} for details.
#'
#' @param method_name Method name (automatically passed)
#' @param data Data frame with yi (effect sizes) and sei (standard errors)
#' @param settings List of method settings (no settings version are implemented)
#'
#' @return Data frame with WAAPWLS results
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
#' # Apply WAAPWLS method
#' result <- run_method("WAAPWLS", data)
#' print(result)
#'
#' @export
method.WAAPWLS <- function(method_name, data, settings = NULL) {

  # Extract data
  effect_sizes    <- data$yi
  standard_errors <- data$sei

  # Check input
  if (length(effect_sizes) < 2)
    stop("At least 2 estimates required for WAAPWLS analysis", call. = FALSE)

  # First, fit WLS model to all studies
  wls_model <- stats::lm(effect_sizes ~ 1, weights = 1/standard_errors^2)

  # Identify high-powered studies
  high_powered   <- stats::coef(wls_model)[1] / 2.8 >= standard_errors
  n_high_powered <- sum(high_powered)

  # Decide which model to use based on number of high-powered studies
  if (n_high_powered >= 2) {
    # Use WAAP: fit model only to high-powered studies
    waap_model <- stats::lm(effect_sizes[high_powered] ~ 1, weights = 1/standard_errors[high_powered]^2)

    # Extract results from WAAP model
    coefficients    <- stats::coef(waap_model)
    se_coefficients <- summary(waap_model)$coefficients[, "Std. Error"]
    p_values        <- summary(waap_model)$coefficients[, "Pr(>|t|)"]
    selected_method <- "WAAP"

  } else {
    # Use WLS: extract results from original WLS model
    coefficients    <- stats::coef(wls_model)
    se_coefficients <- summary(wls_model)$coefficients[, "Std. Error"]
    p_values        <- summary(wls_model)$coefficients[, "Pr(>|t|)"]
    selected_method <- "WLS"
  }

  # Extract results
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
    note             = note,
    selected_method  = selected_method,
    n_high_powered   = n_high_powered
  ))
}

#' @export
method_settings.WAAPWLS <- function(method_name) {

  settings <- list(
    "default" = list() # no available settings
  )

  return(settings)
}

#' @export
method_extra_columns.WAAPWLS <- function(method_name)
  c("selected_method", "n_high_powered")
