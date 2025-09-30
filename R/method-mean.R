#' @title Mean Method
#'
#' @description
#' Implements the unweighted mean method. I.e., the mean of observed effect sizes.
#' All of the computations are based on a one-sample t-test.
#'
#' @param method_name Method name (automatically passed)
#' @param data Data frame with yi (effect sizes)
#' @param settings List of method settings (see Details.)
#'
#' @return Data frame with mean results
#'
#' @details
#' The following settings are implemented \describe{
#'   \item{\code{"default"}}{No settings}
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
#' # Apply mean method
#' result <- run_method("mean", data)
#' print(result)
#'
#' @export
method.mean <- function(method_name, data, settings) {

  # Compute mean of the observed effect sizes

  # Extract data
  effect_sizes    <- data$yi

  # Check input
  if (length(effect_sizes) < 1)
    stop("At least 1 estimate required for mean", call. = FALSE)

  # Extract results
  estimate     <- mean(effect_sizes)
  estimate_se  <- sd(effect_sizes) / sqrt(length(effect_sizes))
  estimate_lci <- estimate + estimate_se * stats::qt(0.025, length(effect_sizes) - 1)
  estimate_uci <- estimate + estimate_se * stats::qt(0.975, length(effect_sizes) - 1)
  estimate_p   <- stats::pt(-abs(estimate/estimate_se), df = length(effect_sizes) - 1) * 2

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
    note             = note
  ))
}

#' @export
method_settings.mean <- function(method_name) {

  settings <- list(
    "default" = list(method = "FE", test = "t")
  )

  return(settings)
}

#' @export
method_extra_columns.mean <- function(method_name)
  c()
