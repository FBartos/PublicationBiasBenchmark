#' @title Robust Bayesian Meta-Analysis (RoBMA) Method
#'
#' @description
#' Implements the robust Bayesian meta-analysis (RoBMA) method that uses
#' Bayesian model-averaging to combine results across several complementary
#' publication bias adjustment methods. See
#' \insertCite{maier2023robust;textual}{PublicationBiasBenchmark} and
#' \insertCite{bartos2023robust;textual}{PublicationBiasBenchmark} for
#' details.
#'
#' Note that the prior settings is dispatched based on \code{"es_type"} column attached
#' to the dataset. The resulting estimates are then summarized on the same scale
#' as was the dataset input (for \code{"r"}, heterogeneity is summarized on Fisher's z).
#'
#' \strong{Important:} This method requires JAGS (Just Another Gibbs Sampler) to be
#' installed on your system. Please download and install JAGS from
#' \url{https://mcmc-jags.sourceforge.io/} before using this method.
#'
#' @param method_name Method name (automatically passed)
#' @param data Data frame with yi (effect sizes), sei (standard errors), es_type
#' (either \code{"SMD"} for Cohen's d / Hedge's g, \code{"logOR"} for log odds
#' ratio, \code{"z"} for Fisher's z, or \code{"r"} for correlations. Defaults to
#' \code{"none"} which re-scales the default priors to unit-information width based
#' on total sample size supplied \code{"ni"}.)
#' @param settings List of method settings (see Details.)
#'
#' @return Data frame with RoBMA results
#'
#' @details
#' The following settings are implemented \describe{
#'   \item{\code{"default"}}{RoBMA-PSMA with publication bias adjustment as described in
#'   \insertCite{bartos2023robust;textual}{PublicationBiasBenchmark}.
#'   (the MCMC settings was reduced to speed-up the simulations) with the
#'   three-level specification whenever \code{"study_ids"} are supplied with the data}
#'   \item{\code{"PSMA"}}{RoBMA-PSMA with publication bias adjustment as described in
#'   \insertCite{bartos2023robust;textual}{PublicationBiasBenchmark}.
#'   (the MCMC settings was reduced to speed-up the simulations) with the
#'   three-level specification whenever \code{"study_ids"} are supplied with the data}
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
#' # Apply RoBMA method
#' result <- run_method("RoBMA", data)
#' print(result)
#' }
#' @export
method.RoBMA <- function(method_name, data, settings) {

  # Check if RoBMA and JAGS are available
  .check_robma_available(message_on_fail = TRUE, stop_on_fail = TRUE)

  # Extract data
  effect_sizes    <- data$yi
  standard_errors <- data$sei

  # Use clustering wherever available
  if (is.null(data[["study_id"]])) {
    study_ids <- NULL
  } else {
    study_ids <- data[["study_id"]]
  }

  # Specify effect sizes
  if (is.null(data[["es_type"]])) {
    es_type <- "SMD"
  } else {
    es_type <- unique(data$es_type)
    if (length(es_type) > 1)
      stop("Only one effect size es_type can be supplied.")
    if (!es_type %in% c("SMD", "logOR", "z", "r", "none"))
      stop("Effect size es_type was not recognized.")
  }

  # Check input
  if (length(effect_sizes) < 2)
    stop("At least 2 estimates required for RoBMA analysis", call. = FALSE)

  RoBMA_call <- settings
  # Dispatch the effect size types
  if (es_type == "SMD") {
    RoBMA_call$d  <- effect_sizes
    RoBMA_call$se <- standard_errors
    output_scale  <- "cohens_d"
  } else if (es_type == "logOR") {
    RoBMA_call$logOR  <- effect_sizes
    RoBMA_call$se     <- standard_errors
    output_scale      <- "logOR"
  } else if (es_type == "z") {
    RoBMA_call$z  <- effect_sizes
    RoBMA_call$se <- standard_errors
    output_scale  <- "fishers_z"
  } else if (es_type == "r") {
    RoBMA_call$r  <- effect_sizes
    RoBMA_call$se <- standard_errors
    output_scale  <- "r"
  } else if (es_type == "none") {
    # specify unit information prior scaling
    if (is.null(data$ni))
      stop("Total sample size `ni` must be specified of the `es_type` is not set (or set to `none`).")

    fit_scale     <- metafor::rma(yi = effect_sizes, sei = standard_errors, method = "FE")
    outcome_scale <- fit_scale$se * sqrt(sum(data$ni))    # prior scaling factor
    prior_scaling <- outcome_scale * RoBMA:::scale_d2z(1) # prior has a scale of 1 on Cohen's d => rescaling proportionally

    RoBMA_call$y  <- effect_sizes
    RoBMA_call$se <- standard_errors
    RoBMA_call$prior_scale    <- "none"
    RoBMA_call$priors_effect        <- RoBMA::prior(distribution = "normal",   parameters = list(mean  = 0, sd = 1 * prior_scaling))
    RoBMA_call$priors_heterogeneity <- RoBMA::prior(distribution = "invgamma", parameters = list(shape = 1, scale = 0.15 * prior_scaling))
    RoBMA_call$priors_bias          <- list(
      RoBMA::prior_weightfunction(distribution = "two.sided", parameters = list(alpha = c(1, 1),       steps = c(0.05)),             prior_weights = 1/12),
      RoBMA::prior_weightfunction(distribution = "two.sided", parameters = list(alpha = c(1, 1, 1),    steps = c(0.05, 0.1)),        prior_weights = 1/12),
      RoBMA::prior_weightfunction(distribution = "one.sided", parameters = list(alpha = c(1, 1),       steps = c(0.05)),             prior_weights = 1/12),
      RoBMA::prior_weightfunction(distribution = "one.sided", parameters = list(alpha = c(1, 1, 1),    steps = c(0.025, 0.05)),      prior_weights = 1/12),
      RoBMA::prior_weightfunction(distribution = "one.sided", parameters = list(alpha = c(1, 1, 1),    steps = c(0.05, 0.5)),        prior_weights = 1/12),
      RoBMA::prior_weightfunction(distribution = "one.sided", parameters = list(alpha = c(1, 1, 1, 1), steps = c(0.025, 0.05, 0.5)), prior_weights = 1/12),
      RoBMA::prior_PET(distribution =   "Cauchy", parameters = list(0, 1), truncation = list(0, Inf), prior_weights = 1/4),
      RoBMA::prior_PEESE(distribution = "Cauchy", parameters = list(0, 5 / prior_scaling), truncation = list(0, Inf), prior_weights = 1/4)
    )

    output_scale  <- "none"
  }

  if (is.null(study_ids) || length(unique(study_ids)) == nrow(data)) {
    RoBMA_call$study_ids <- NULL
  } else {
    RoBMA_call$study_ids <- study_ids
  }

  # Call the model
  RoBMA_model    <- do.call(RoBMA::RoBMA, RoBMA_call)
  RoBMA_summary  <- summary(RoBMA_model, output_scale = output_scale)

  # Extract results
  estimate     <- RoBMA_summary$estimates["mu", "Mean"]
  estimate_se  <- NA
  estimate_lci <- RoBMA_summary$estimates["mu", "0.025"]
  estimate_uci <- RoBMA_summary$estimates["mu", "0.975"]
  BF           <- RoBMA_summary$components["Effect", "inclusion_BF"]

  tau_estimate <- RoBMA_summary$estimates["tau", "Mean"]
  tau_ci_lower <- RoBMA_summary$estimates["tau", "0.025"]
  tau_ci_upper <- RoBMA_summary$estimates["tau", "0.975"]
  tau_BF       <- RoBMA_summary$components["Heterogeneity", "inclusion_BF"]

  bias_SM_coefficient          <- RoBMA_summary$estimates[grepl("omega", rownames(RoBMA_summary$estimates)), "Mean"]
  bias_SM_coefficient_ci_lower <- RoBMA_summary$estimates[grepl("omega", rownames(RoBMA_summary$estimates)), "0.025"]
  bias_SM_coefficient_ci_upper <- RoBMA_summary$estimates[grepl("omega", rownames(RoBMA_summary$estimates)), "0.975"]
  bias_PP_coefficient          <- RoBMA_summary$estimates[grepl("PET", rownames(RoBMA_summary$estimates)) | grepl("PEESE", rownames(RoBMA_summary$estimates)), "Mean"]
  bias_PP_coefficient_ci_lower <- RoBMA_summary$estimates[grepl("PET", rownames(RoBMA_summary$estimates)) | grepl("PEESE", rownames(RoBMA_summary$estimates)), "0.025"]
  bias_PP_coefficient_ci_upper <- RoBMA_summary$estimates[grepl("PET", rownames(RoBMA_summary$estimates)) | grepl("PEESE", rownames(RoBMA_summary$estimates)), "0.975"]
  bias_BF                      <- RoBMA_summary$components["Bias", "inclusion_BF"]

  convergence <- TRUE
  note        <- RoBMA::check_RoBMA_convergence(RoBMA_model)

  return(data.frame(
    method           = method_name,
    estimate         = estimate,
    standard_error   = estimate_se,
    ci_lower         = estimate_lci,
    ci_upper         = estimate_uci,
    p_value          = NA,
    BF               = BF,
    convergence      = convergence,
    note             = note,
    tau_estimate     = tau_estimate,
    tau_ci_lower     = tau_ci_lower,
    tau_ci_upper     = tau_ci_upper,
    tau_BF           = tau_BF,
    bias_SM_coefficient          = paste0(bias_SM_coefficient, collapse = ", "),
    bias_SM_coefficient_ci_lower = paste0(bias_SM_coefficient_ci_lower, collapse = ", "),
    bias_SM_coefficient_ci_upper = paste0(bias_SM_coefficient_ci_upper, collapse = ", "),
    bias_PP_coefficient          = paste0(bias_PP_coefficient, collapse = ", "),
    bias_PP_coefficient_ci_lower = paste0(bias_PP_coefficient_ci_lower, collapse = ", "),
    bias_PP_coefficient_ci_upper = paste0(bias_PP_coefficient_ci_upper, collapse = ", "),
    bias_BF                      = bias_BF
  ))
}

#' @export
method_settings.RoBMA <- function(method_name) {

  # Check if RoBMA and JAGS are available
  if (!.check_robma_available(message_on_fail = TRUE, stop_on_fail = FALSE)) {
    # Return minimal settings structure to pass checks
    return(list(default = list()))
  }

  settings.PSMA <- list(
    effect_direction          = "positive",
    prior_scale               = "cohens_d",
    priors_effect             = RoBMA::prior(distribution = "normal",   parameters = list(mean  = 0, sd = 1)),
    priors_heterogeneity      = RoBMA::prior(distribution = "invgamma", parameters = list(shape = 1, scale = 0.15)),
    priors_bias               = list(
      RoBMA::prior_weightfunction(distribution = "two.sided", parameters = list(alpha = c(1, 1),       steps = c(0.05)),             prior_weights = 1/12),
      RoBMA::prior_weightfunction(distribution = "two.sided", parameters = list(alpha = c(1, 1, 1),    steps = c(0.05, 0.1)),        prior_weights = 1/12),
      RoBMA::prior_weightfunction(distribution = "one.sided", parameters = list(alpha = c(1, 1),       steps = c(0.05)),             prior_weights = 1/12),
      RoBMA::prior_weightfunction(distribution = "one.sided", parameters = list(alpha = c(1, 1, 1),    steps = c(0.025, 0.05)),      prior_weights = 1/12),
      RoBMA::prior_weightfunction(distribution = "one.sided", parameters = list(alpha = c(1, 1, 1),    steps = c(0.05, 0.5)),        prior_weights = 1/12),
      RoBMA::prior_weightfunction(distribution = "one.sided", parameters = list(alpha = c(1, 1, 1, 1), steps = c(0.025, 0.05, 0.5)), prior_weights = 1/12),
      RoBMA::prior_PET(distribution =   "Cauchy", parameters = list(0, 1), truncation = list(0, Inf), prior_weights = 1/4),
      RoBMA::prior_PEESE(distribution = "Cauchy", parameters = list(0, 5), truncation = list(0, Inf), prior_weights = 1/4)
    ),
    priors_hierarchical       = RoBMA::prior("beta", parameters = list(alpha = 1, beta = 1)),
    priors_effect_null        = RoBMA::prior(distribution = "point", parameters = list(location = 0)),
    priors_heterogeneity_null = RoBMA::prior(distribution = "point", parameters = list(location = 0)),
    priors_bias_null          = RoBMA::prior_none(),
    priors_hierarchical_null  = NULL,
    algorithm  = "ss",
    chains     = 2,
    sample     = 2000,
    burnin     = 3000,
    adapt      = 2000,
    thin       = 1,
    parallel   = FALSE,
    save       = "all",
    seed       = 1
  )

  settings <- list(
    # corresponds to RoBMA PSMA settings
    "default" = settings.PSMA,
    "PSMA"    = settings.PSMA
  )

  return(settings)
}

#' @export
method_extra_columns.RoBMA <- function(method_name)
  c("tau_estimate", "tau_ci_lower", "tau_ci_upper", "tau_BF",
    "bias_SM_coefficient", "bias_SM_coefficient_ci_lower", "bias_SM_coefficient_ci_upper", "bias_PP_coefficient", "bias_PP_coefficient_ci_lower", "bias_PP_coefficient_ci_upper", "bias_BF")


# Helper function to check if RoBMA (and JAGS) is available
# Returns TRUE if available, FALSE otherwise
# Used internally and in tests
.check_robma_available <- function(message_on_fail = TRUE, stop_on_fail = FALSE) {

  # Check if RoBMA package is installed
  if (!requireNamespace("RoBMA", quietly = TRUE)) {
    if (message_on_fail) {
      msg <- "Package 'RoBMA' is required for RoBMA method. Please install it with: install.packages('RoBMA')"
      if (stop_on_fail) {
        stop(msg, call. = FALSE)
      } else {
        message(msg)
      }
    }
    return(FALSE)
  }

  # Check if JAGS is installed by trying to load the RoBMA namespace
  jags_available <- tryCatch({
    loadNamespace("RoBMA")
    TRUE
  }, error = function(e) {
    if (grepl("JAGS", e$message, ignore.case = TRUE)) {
      if (message_on_fail) {
        msg <- paste0("RoBMA requires JAGS to be installed. ",
                     "Please install JAGS from https://mcmc-jags.sourceforge.io/ ",
                     "before using the RoBMA method.")
        if (stop_on_fail) {
          stop(msg, call. = FALSE)
        } else {
          message(msg)
        }
      }
      return(FALSE)
    } else {
      if (stop_on_fail) {
        stop(e$message, call. = FALSE)
      }
      return(FALSE)
    }
  })

  return(jags_available)
}
