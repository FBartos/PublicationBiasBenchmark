#' @title Mixture Model of P-Hacking (MMPH)
#'
#' @author Frantisek Bartos \email{f.bartos96@gmail.com}
#'
#' @description
#' Implements mixture model of p-hacking as described in
#' \insertCite{moss2023modelling;textual}{PublicationBiasBenchmark}.
#'
#' The model is estimated via `publipha::phma()`.
#'
#' @param method_name Method name (automatically passed)
#' @param data Data frame with yi (effect sizes) and sei (standard errors)
#' @param settings List of method settings (see Details.)
#'
#' @return Data frame with MMPH results
#'
#' @details
#' The following settings are implemented \describe{
#'   \item{\code{"default"}}{MMPH with Stan control settings
#'   \code{control = list(adapt_delta = 0.95, max_treedepth = 15)},
#'   \code{warmup = 2000}, and \code{iter = 4000}, and
#'   \code{chains = 3}}
#' }
#'
#' @references
#'  \insertAllCited{}
#'
#' @examples
#' \donttest{
#' # Generate some example data
#' data <- data.frame(
#'   yi      = c(0.2, 0.3, 0.1, 0.4, 0.25),
#'   sei     = c(0.1, 0.15, 0.08, 0.12, 0.09),
#'   es_type = "SMD"
#' )
#'
#' # Apply MAN method
#' result <- run_method("MMPH", data)
#' print(result)
#' }
#'
#' @export
method.MMPH <- function(method_name, data, settings) {

  # Extract data
  effect_sizes       <- data$yi
  standard_errors    <- data$sei

  # Check input
  if (length(effect_sizes) < 2)
    stop("At least 2 estimates required for PHMA analysis", call. = FALSE)

  publipha_call    <- settings
  publipha_call$yi <- effect_sizes
  publipha_call$vi <- standard_errors^2
  publipha_call$refresh <- 0

  fit         <- do.call(publipha::phma, publipha_call)
  fit_summary <- rstan::summary(fit, pars = c("theta0", "tau"))$summary

  estimate_mean    <- fit_summary[rownames(fit_summary) == "theta0", "mean"]
  estimate_median  <- fit_summary[rownames(fit_summary) == "theta0", "50%"]
  estimate_lci     <- fit_summary[rownames(fit_summary) == "theta0", "2.5%"]
  estimate_uci     <- fit_summary[rownames(fit_summary) == "theta0", "97.5%"]
  estimate_n_eff   <- fit_summary[rownames(fit_summary) == "theta0", "n_eff"]
  estimate_r_hat   <- fit_summary[rownames(fit_summary) == "theta0", "Rhat"]
  tau_mean         <- fit_summary[rownames(fit_summary) == "tau", "mean"]
  tau_median       <- fit_summary[rownames(fit_summary) == "tau", "50%"]
  tau_ci_lower     <- fit_summary[rownames(fit_summary) == "tau", "2.5%"]
  tau_ci_upper     <- fit_summary[rownames(fit_summary) == "tau", "97.5%"]
  tau_n_eff        <- fit_summary[rownames(fit_summary) == "tau", "n_eff"]
  tau_r_hat        <- fit_summary[rownames(fit_summary) == "tau", "Rhat"]

  divergent_iter <- sum(rstan::get_divergent_iterations(fit))

  return(data.frame(
    method           = method_name,
    estimate         = estimate_mean,
    standard_error   = NA,
    ci_lower         = estimate_lci,
    ci_upper         = estimate_uci,
    p_value          = NA,
    BF               = NA,
    convergence      = TRUE,
    note             = NA,
    estimate_median  = estimate_median,
    estimate_n_eff   = estimate_n_eff,
    estimate_r_hat   = estimate_r_hat,
    tau_estimate     = tau_mean,
    tau_median       = tau_median,
    tau_ci_lower     = tau_ci_lower,
    tau_ci_upper     = tau_ci_upper,
    tau_n_eff        = tau_n_eff,
    tau_r_hat        = tau_r_hat,
    divergent_iter   = divergent_iter
  ))
}

#' @export
method_settings.MMPH <- function(method_name) {

  settings <- list(
    "default" = list(
      chains   = 3,
      warmup   = 2000,
      iter     = 4000,
      control  = list(adapt_delta = 0.95, max_treedepth = 15)
    )
  )

  return(settings)
}

#' @export
method_extra_columns.MMPH <- function(method_name)
  c("estimate_median", "estimate_n_eff", "estimate_r_hat",
    "tau_estimate", "tau_median", "tau_ci_lower", "tau_ci_upper", "tau_n_eff", "tau_r_hat",
    "divergent_iter"
  )
