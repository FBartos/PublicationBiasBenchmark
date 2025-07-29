#' PET (Precision-Effect Test) Method
#'
#' Implements the Precision-Effect Test for publication bias correction.
#' PET regresses effect sizes against standard errors to test for and correct
#' publication bias. The intercept represents the bias-corrected effect size.
#'
#' @param method_name Method name (automatically passed)
#' @param data Data frame with yi (effect sizes) and sei (standard errors)
#' @param settings List of method settings (currently unused)
#'
#' @return Data frame with PET results
#' @export
#'
#' @examples
#' # Generate some example data
#' data <- data.frame(
#'   yi = c(0.2, 0.3, 0.1, 0.4, 0.25),
#'   sei = c(0.1, 0.15, 0.08, 0.12, 0.09)
#' )
#'
#' # Apply PET method
#' result <- method("PET", data)
#' print(result)
method.PET <- function(method_name, data, settings = NULL) {

  # Extract data
  effect_sizes    <- data$yi
  standard_errors <- data$sei

  if (length(effect_sizes) < 3) {
    return(create_empty_result(
      method_name = method_name,
      note        = "At least 3 studies required for PET analysis",
      extra_columns = list(
        bias_coefficient = NA,
        bias_p_value     = NA
      )
    ))
  }

  # Fit PET model: effect_size ~ intercept + slope * standard_error
  result <- tryCatch({

    pet_model <- stats::lm(effect_sizes ~ standard_errors, weights = 1/standard_errors^2)

    # Extract results
    coefficients    <- stats::coef(pet_model)
    se_coefficients <- summary(pet_model)$coefficients[, "Std. Error"]
    p_values        <- summary(pet_model)$coefficients[, "Pr(>|t|)"]

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

  }, error = function(e) {

    return(create_empty_result(
      method_name = method_name,
      note        = paste("Model fitting failed:", e$message),
      extra_columns = list(
        bias_coefficient = NA,
        bias_p_value     = NA
      )
    ))

  })

  return(result)
}
