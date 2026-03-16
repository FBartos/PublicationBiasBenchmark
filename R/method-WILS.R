#' @title Weighted and Iterated Least Squares (WILS) Method
#'
#' @author František Bartoš \email{f.bartos96@@gmail.com}
#'
#' @description
#' Implements the weighted and iterated least squares (WILS) method for publication
#' bias correction in meta-analysis. The method is based on the idea of using excess statistical
#' significance (ESS) to identify how many underpowered studies should be removed to
#' reduce publication selection bias. See
#' \insertCite{stanley2024harnessing;textual}{PublicationBiasBenchmark} for details.
#'
#' @param method_name Method name (automatically passed)
#' @param data Data frame with yi (effect sizes) and sei (standard errors)
#' @param settings List of method settings (see Details)
#'
#' @return Data frame with WILS results
#'
#' @details
#' The WILS method has two implementation versions based on Stanley & Doucouliagos (2024).
#' The following settings are implemented \describe{
#'   \item{\code{"default"}}{The simulation version (default) uses residuals from the
#'        t ~ Precision regression for the first iteration, then switches to individual
#'        excess statistical significance (ESS) for subsequent iterations.}
#'   \item{\code{"example"}}{The example version consistently uses residuals from the
#'        t ~ Precision regression to identify studies to remove across all iterations.}
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
#' # Apply WILS method
#' result <- run_method("WILS", data)
#' print(result)
#'
#' @export
method.WILS <- function(method_name, data, settings = NULL) {

  # Extract data
  effect_sizes    <- data$yi
  standard_errors <- data$sei

  # Check input
  if (length(effect_sizes) < 2)
    stop("At least 2 estimates required for WILS analysis", call. = FALSE)

  # Apply WILS computation
  fit         <- .method_WILS_compute(y = effect_sizes, se = standard_errors, version = settings$version)
  final_model <- fit[["final_model"]]
  final_data  <- fit[["data_trimmed"]]

  # Extract results
  coefficients    <- stats::coef(final_model)
  se_coefficients <- summary(final_model)$coefficients[, "Std. Error"]
  p_values        <- summary(final_model)$coefficients[, "Pr(>|t|)"]

  # The intercept represents the weighted effect size estimate
  estimate    <- coefficients[1]
  estimate_se <- se_coefficients[1]
  estimate_p  <- p_values[1]

  # Calculate confidence interval
  estimate_lci <- estimate - 1.96 * estimate_se
  estimate_uci <- estimate + 1.96 * estimate_se

  n_removed <- nrow(data) - nrow(final_data)

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
    n_removed        = n_removed
  ))
}

#' @export
method_settings.WILS <- function(method_name) {

  settings <- list(
    "default" = list(version = "simulation"),
    "example" = list(version = "example")
  )

  return(settings)
}

#' @export
method_extra_columns.WILS <- function(method_name)
  c("n_removed")



### additional computation functions ----
# There are two WILS version in the Stanley 2024 paper
# "example" and "simulation". The key difference is how studies are dropped after the first iteration.
# Simulation (default) version uses residuals for first iteration, then switches to
# individual ESS (ESSi) for subsequent iterations. This follows the approach described
# in the paper's simulation section and footnote 15, which states:
# "After the first iteration, drop those studies with the largest ESSi"
# Example version always uses residuals for dropping studies, as per the example in the paper.
.method_WILS_compute <- function(y, se, version) {

  # Initialize data frame
  data <- data.frame(d = y, sed = se, study_id = seq_along(y))

  # Initial calculations (before while loop)
  data$Precision_sq <- 1 / data$sed^2
  data$t            <- data$d / data$sed
  data$Precision    <- 1 / data$sed

  # While loop (continue while studies were dropped)
  N0        <- Inf
  iteration <- 0
  while (N0 > nrow(data) && nrow(data) > 2) {
    N0        <- nrow(data)
    iteration <- iteration + 1

    # Determine sorting criterion based on iteration and version
    if (version == "example") {
      sort_by_ESS <- FALSE # Example version: always sort by residuals
    } else {
      sort_by_ESS <- (iteration > 1) # Simulation version: first iteration by residuals, subsequent by ESS
    }

    data <- .method_WILS_compute_iter(data, sort_by_ESS = sort_by_ESS)
  }

  # Final WLS model
  final_model <- stats::lm(d ~ 1, data = data, weights = data$Precision_sq)

  return(list(
    final_model  = final_model,
    data_trimmed = data
  ))
}

.method_WILS_compute_iter <- function(data, sort_by_ESS = FALSE) {

  # Regression: t on Precision (without constant)
  reg_t_precision <- stats::lm(t ~ 0 + Precision, data = data)
  data$Resid      <- stats::residuals(reg_t_precision)

  # Meta-analysis to get tau2
  rma_model <- metafor::rma(yi = data$d, sei = data$sed, method = "DL")
  HetVar    <- rma_model$tau2

  # Weighted regression of d on constant
  reg_d_weighted <- stats::lm(d ~ 1, data = data, weights = data$Precision_sq)
  intercept      <- stats::coef(reg_d_weighted)[1]

  # Calculate expected significance probability
  data$zz   <- (1.96 * data$sed - intercept) / sqrt(data$sed^2 + HetVar)
  data$Esig <- 1 - stats::pnorm(data$zz)

  # Statistical significance indicator
  data$SS <- ifelse(data$t > 1.96, 1, 0)

  # Excess statistical significance (individual level)
  data$ESS <- data$SS - data$Esig

  # Calculate total ESS
  ESStot <- sum(data$ESS)

  # If ESS <= 0, return data unchanged (stopping condition)
  if (ESStot <= 0) {
    return(data)
  }

  # Determine sorting criterion based on iteration
  if (sort_by_ESS) {
    # In case of simulation version, subsequent iterations sort by individual ESS and drop those with largest ESS
    data <- data[order(data$ESS), ]
  } else {
    # Always for first iteration: sort by residuals and drop those with largest residuals
    # In case of example version, all iterations sort by residuals
    data <- data[order(data$Resid), ]
  }

  data$meta_id <- seq_len(nrow(data))

  # Calculate number of studies to drop
  Nsize     <- nrow(data)
  n_to_drop <- ceiling(ESStot)
  n_to_keep <- Nsize - n_to_drop

  # Drop the excess results (those at the end after sorting)
  if (n_to_keep > 0) {
    data <- data[data$meta_id <= n_to_keep, , drop = FALSE]
  }

  return(data)
}
