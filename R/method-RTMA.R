#' @title Right-Truncated Meta-Analysis (RTMA) Method
#'
#' @author Frantisek Bartos \email{f.bartos96@gmail.com}
#'
#' @description
#' Implements right-truncated meta-analysis (RTMA) for correcting the joint
#' effects of p-hacking and publication bias in meta-analysis.
#' See \insertCite{mathur2024phacking;textual}{PublicationBiasBenchmark}
#' for details.
#'
#' RTMA is estimated via `phacking::phacking_meta()`.
#'
#' @param method_name Method name (automatically passed)
#' @param data Data frame with yi (effect sizes) and sei (standard errors)
#' @param settings List of method settings (see Details.)
#'
#' @return Data frame with RTMA results
#'
#' @details
#' The following settings are implemented \describe{
#'   \item{\code{"default"}}{RTMA with affirmative results defined by
#'   positive direction \code{favor_positive = TRUE} and statistical significance
#'   \code{alpha_select = 0.05}, posterior interval level
#'   \code{ci_level = 0.95}, Stan control settings
#'   \code{adapt_delta = 0.98}, \code{max_treedepth = 20},
#'   \code{parallelize = FALSE}}
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
#' # Apply RTMA method
#' result <- run_method("RTMA", data)
#' print(result)
#' }
#'
#' @export
method.RTMA <- function(method_name, data, settings) {

  # Extract data
  effect_sizes    <- data$yi
  standard_errors <- data$sei

  if (length(effect_sizes) < 1)
    stop("At least 1 estimate required for RTMA analysis", call. = FALSE)

  call_args     <- settings
  call_args$yi  <- effect_sizes
  call_args$sei <- standard_errors

  rtma_fit <- do.call(phacking::phacking_meta, call_args)

  rtma_stats <- as.data.frame(rtma_fit$stats)
  rtma_vals  <- rtma_fit$values
  rtma_post  <- rstan::extract(rtma_fit$fits)

  estimate_mode    <- rtma_stats[rtma_stats$param == "mu", "mode"]
  estimate_median  <- rtma_stats[rtma_stats$param == "mu", "median"]
  estimate_mean    <- rtma_stats[rtma_stats$param == "mu", "mean"]
  estimate_lci     <- rtma_stats[rtma_stats$param == "mu", "ci_lower"]
  estimate_uci     <- rtma_stats[rtma_stats$param == "mu", "ci_upper"]
  estimate_n_eff   <- rtma_stats[rtma_stats$param == "mu", "n_eff"]
  estimate_r_hat   <- rtma_stats[rtma_stats$param == "mu", "r_hat"]
  tau_mode         <- rtma_stats[rtma_stats$param == "tau", "mode"]
  tau_median       <- rtma_stats[rtma_stats$param == "tau", "median"]
  tau_mean         <- rtma_stats[rtma_stats$param == "tau", "mean"]
  tau_ci_lower     <- rtma_stats[rtma_stats$param == "tau", "ci_lower"]
  tau_ci_upper     <- rtma_stats[rtma_stats$param == "tau", "ci_upper"]
  tau_n_eff        <- rtma_stats[rtma_stats$param == "tau", "n_eff"]
  tau_r_hat        <- rtma_stats[rtma_stats$param == "tau", "r_hat"]

  convergence    <- isTRUE(estimate_n_eff > 500 && estimate_r_hat < 0.01)
  divergent_iter <- sum(rstan::get_divergent_iterations(rtma_fit$fits))

  return(data.frame(
    method           = method_name,
    estimate         = estimate_mode,
    standard_error   = NA,
    ci_lower         = estimate_lci,
    ci_upper         = estimate_uci,
    p_value          = NA,
    BF               = NA,
    convergence      = convergence,
    note             = NA,
    estimate_median  = estimate_median,
    estimate_mean    = estimate_mean,
    estimate_n_eff   = estimate_n_eff,
    estimate_r_hat   = estimate_r_hat,
    tau_estimate     = tau_mode,
    tau_median       = tau_median,
    tau_mean         = tau_mean,
    tau_ci_lower     = tau_ci_lower,
    tau_ci_upper     = tau_ci_upper,
    tau_n_eff        = tau_n_eff,
    tau_r_hat        = tau_r_hat,
    k_affirmative    = rtma_vals$k_affirmative,
    k_nonaffirmative = rtma_vals$k_nonaffirmative,
    optim_converged  = rtma_vals$optim_converged,
    divergent_iter   = divergent_iter
  ))
}

#' @export
method_settings.RTMA <- function(method_name) {

  # it is not possible to increase number of samples / adapt period
  # in the current version of the package

  settings <- list(
    "default" = list(
      favor_positive = TRUE,
      alpha_select   = 0.05,
      ci_level       = 0.95,
      stan_control   = list(adapt_delta = 0.98, max_treedepth = 20),
      parallelize    = FALSE
    )
  )

  return(settings)
}

#' @export
method_extra_columns.RTMA <- function(method_name)
  c("estimate_median", "estimate_mean", "estimate_n_eff", "estimate_r_hat",
    "tau_estimate", "tau_median", "tau_mean", "tau_ci_lower", "tau_ci_upper", "tau_n_eff", "tau_r_hat",
    "k_affirmative", "k_nonaffirmative", "optim_converged", "divergent_iter"
  )
