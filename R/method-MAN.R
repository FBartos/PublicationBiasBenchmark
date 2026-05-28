#' @title Meta-Analysis of Nonaffirmative Studies (MAN) Method
#'
#' @author Frantisek Bartos \email{f.bartos96@gmail.com}
#'
#' @description
#' Implements standard meta-analysis of only the nonaffirmative studies (MAN)
#' which can serve as a sensitivity analysis for worst-case meta-analytic
#' point estimate for maximal publication bias under the selection model.
#' effects of p-hacking and publication bias in meta-analysis.
#' See \insertCite{mathur2020sensitivity;textual}{PublicationBiasBenchmark}
#' for details.
#'
#' @param method_name Method name (automatically passed)
#' @param data Data frame with yi (effect sizes) and sei (standard errors)
#' @param settings List of method settings (see Details.)
#'
#' @return Data frame with MAN results
#'
#' @details
#' The following settings are implemented \describe{
#'   \item{\code{"default"}}{MAN with affirmative results defined by
#'   positive direction \code{favor_positive = TRUE} and statistical significance
#'   \code{alpha_select = 0.05}}
#' }
#'
#' @references
#'  \insertAllCited{}
#'
#' @examples
#' \donttest{
#' # Generate some example data with at least one nonaffirmative study
#' data <- data.frame(
#'   yi = c(0.20, 0.30, 0.10, -0.05, 0.12),
#'   sei = c(0.10, 0.15, 0.08, 0.12, 0.09)
#' )
#'
#' # Apply MAN method
#' result <- run_method("MAN", data)
#' print(result)
#' }
#'
#' @export
method.MAN <- function(method_name, data, settings) {

  # Select only non-affirmative estimates
  if (settings[["favor_positive"]]) {
    data <- data[(data$yi / data$sei) < stats::qnorm(1-settings[["alpha_select"]]/2, lower.tail = TRUE),,drop = FALSE]
  } else {
    data <- data[(data$yi / data$sei) > stats::qnorm(1-settings[["alpha_select"]]/2, lower.tail = FALSE),,drop = FALSE]
  }

  # Extract data
  effect_sizes            <- data$yi
  standard_errors         <- data$sei
  sampling_variances      <- standard_errors^2
  data$sampling_variances <- sampling_variances

  # Use clustering wherever available
  if (is.null(data[["study_id"]])) {
    study_ids <- 1:nrow(data)
  } else {
    study_ids <- data[["study_id"]]
  }

  # Check input
  if (length(effect_sizes) < 3)
    stop("At least 3 estimates required for MAN analysis", call. = FALSE)

  fit <- robumeta::robu(
    formula      = effect_sizes ~ 1,
    studynum     = study_ids,
    data         = data,
    var.eff.size = sampling_variances,
    small        = TRUE
  )

  # If df < 4, do not trust the results
  convergence <- fit$dfs > 4

  # Extract results
  estimate     <- fit$reg_table[1, "b.r"]
  estimate_se  <- fit$reg_table[1, "SE"]
  estimate_lci <- fit$reg_table[1, "CI.L"]
  estimate_uci <- fit$reg_table[1, "CI.U"]
  estimate_p   <- fit$reg_table[1, "prob"]

  tau_estimate <- sqrt(fit$mod_info$tau.sq)

  k_nonaffirmative <- nrow(data)
  note             <- NA

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
    k_nonaffirmative = k_nonaffirmative
  ))
}

#' @export
method_settings.MAN <- function(method_name) {

  settings <- list(
    "default" = list(
      favor_positive = TRUE,
      alpha_select   = 0.05
    )
  )

  return(settings)
}

#' @export
method_extra_columns.MAN <- function(method_name)
  c("tau_estimate", "k_nonaffirmative")
