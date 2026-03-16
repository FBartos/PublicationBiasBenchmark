#' @title puniform (P-Uniform) Method
#'
#' @author František Bartoš \email{f.bartos96@@gmail.com}
#'
#' @description
#' Implements the p-uniform method for publication bias detection and correction.
#' P-uniform uses the distribution of p-values from significant studies to test
#' for publication bias and estimate the effect size corrected for publication bias.
#' The method assumes that p-values follow a uniform distribution under the null
#' hypothesis of no effect, and uses this to detect and correct for bias. See
#' \insertCite{vanassen2015meta;textual}{PublicationBiasBenchmark} and
#' \insertCite{vanaert2025puniform;textual}{PublicationBiasBenchmark} for details.
#'
#' @param method_name Method name (automatically passed)
#' @param data Data frame with yi (effect sizes) and sei (standard errors)
#' @param settings List of method settings (see Details)
#'
#' @return Data frame with P-Uniform results
#'
#' @details
#' The following settings are implemented \describe{
#'   \item{\code{"default"}}{Default p-uniform analysis settings.}
#'   \item{\code{"star"}}{P-uniform star version of the method.}
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
#' # Apply puniform method
#' result <- run_method("puniform", data)
#' print(result)
#'
#' @export
method.puniform <- function(method_name, data, settings) {

  # Extract data
  effect_sizes    <- data$yi
  standard_errors <- data$sei

  # store version and remove from options list
  version <- settings[["version"]]
  settings[["version"]] <- NULL

  settings$yi <- effect_sizes
  settings$vi <- standard_errors^2

  # Check input
  if (length(effect_sizes) < 1)
    stop("At least 1 estimates is required for p-curve analysis", call. = FALSE)


  if (version == "original") {
    fit <- do.call(puniform::puniform, settings)
  } else if (version == "star") {
    fit <- do.call(puniform::puni_star, settings)
  }

  estimate     <- fit$est
  estimate_se  <- NA
  estimate_p   <- fit$pval.0

  # Calculate confidence interval
  estimate_lci <- fit$ci.lb
  estimate_uci <- fit$ci.ub

  if (version == "original") {
    tau_estimate <- NA
    tau_p_value  <- NA
    tau_ci_lower <- NA
    tau_ci_upper <- NA
    bias_p_value <- fit$pval.pb
  } else if (version == "star") {
    tau_estimate <- sqrt(fit$tau2)
    tau_p_value  <- fit$pval.het
    tau_ci_lower <- sqrt(fit$tau2.lb)
    tau_ci_upper <- sqrt(fit$tau2.ub)
    bias_p_value <- NA
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
    version          = version,
    tau_estimate     = tau_estimate,
    tau_ci_lower     = tau_ci_lower,
    tau_ci_upper     = tau_ci_upper,
    tau_p_value      = tau_p_value,
    bias_p_value     = bias_p_value

  ))
}

#' @export
method_settings.puniform <- function(method_name) {

  settings <- list(
    "default" = list(version = "original", method = "P",  side = "right"),
    "star"    = list(version = "star",     method = "ML", side = "right")
  )

  return(settings)
}

#' @export
method_extra_columns.puniform <- function(method_name)
  c("version", "tau_estimate", "tau_ci_lower", "tau_ci_upper", "tau_p_value", "bias_p_value")
