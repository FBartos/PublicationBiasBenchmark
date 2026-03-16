#' @title Mean Method
#'
#' @author František Bartoš \email{f.bartos96@@gmail.com}
#'
#' @description
#' Implements the unweighted mean method. I.e., the mean of observed effect sizes.
#'
#' @param method_name Method name (automatically passed)
#' @param data Data frame with yi (effect sizes)
#' @param settings List of method settings (see Details)
#'
#' @return Data frame with mean results
#'
#' @details
#' The following settings are implemented \describe{
#'   \item{\code{"default"}}{No settings}
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
#' # Apply mean method
#' result <- run_method("mean", data)
#' print(result)
#'
#' @export
method.mean <- function(method_name, data, settings) {

  # Compute mean of the observed effect sizes

  # Extract data
  effect_sizes    <- data$yi
  standard_errors <- data$sei

  # Check input
  if (length(effect_sizes) < 1)
    stop("At least 1 estimate required for mean", call. = FALSE)

  # Extract results
  estimate     <- mean(effect_sizes)
  estimate_se  <- sqrt(sum(standard_errors^2)) / length(effect_sizes)
  estimate_lci <- estimate + estimate_se * stats::qnorm(0.025)
  estimate_uci <- estimate + estimate_se * stats::qnorm(0.975)
  estimate_p   <- stats::pnorm(-abs(estimate/estimate_se)) * 2

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
    "default" = list()
  )

  return(settings)
}

#' @export
method_extra_columns.mean <- function(method_name)
  character()
