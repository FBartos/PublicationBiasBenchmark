#' @title PEESE (Precision-Effect Estimate with Standard Errors) Method
#'
#' @author František Bartoš \email{f.bartos96@@gmail.com}
#'
#' @description
#' Implements the Precision-Effect Estimate with Standard Errors method for
#' publication bias correction. PEESE regresses effect sizes against standard
#' errors^2 to correct for publication bias. The intercept represents the
#' bias-corrected effect size estimate. See
#' \insertCite{stanley2014meta;textual}{PublicationBiasBenchmark} for details.
#'
#' @param method_name Method name (automatically passed)
#' @param data Data frame with yi (effect sizes) and sei (standard errors)
#' @param settings List of method settings (no settings version are implemented)
#'
#' @return Data frame with PEESE results
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
#' # Apply PEESE method
#' result <- run_method("PEESE", data)
#' print(result)
#'
#' @export
method.PEESE <- function(method_name, data, settings = NULL) {

  # Fit PEESE model: effect_size ~ intercept + slope * standard_error^2

  # Extract data
  effect_sizes    <- data$yi
  standard_errors <- data$sei

  # Check input
  if (length(effect_sizes) < 3)
    stop("At least 3 estimates required for PEESE analysis", call. = FALSE)

  if (stats::var(standard_errors) <= 0)
    stop("No variance in standard errors")

  peese_model <- stats::lm(effect_sizes ~ I(standard_errors^2), weights = 1/standard_errors^2)

  # Extract results
  coefficients    <- stats::coef(peese_model)
  se_coefficients <- summary(peese_model)$coefficients[, "Std. Error"]
  p_values        <- summary(peese_model)$coefficients[, "Pr(>|t|)"]

  # The intercept represents the bias-corrected effect size
  estimate         <- coefficients[1]
  estimate_se      <- se_coefficients[1]
  estimate_p       <- p_values[1]
  bias_coefficient <- coefficients[2]
  bias_p_value     <- p_values[2]

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
    bias_coefficient = bias_coefficient,
    bias_p_value     = bias_p_value
  ))
}

#' @export
method_settings.PEESE <- function(method_name) {

  settings <- list(
    "default" = list() # no available settings
  )

  return(settings)
}

#' @export
method_extra_columns.PEESE <- function(method_name)
  c("bias_coefficient", "bias_p_value")
