#' Normal DGM (No Publication Bias)
#'
#' Generates effect sizes from a normal distribution without publication bias
#'
#' @param dgm_name DGM name (automatically passed)
#' @param settings List containing n_studies, mean_effect, heterogeneity
#'
#' @return Data frame with generated data
#' @export
dgm.no_bias <- function(dgm_name, settings) {

  # Extract settings
  n_studies     <- settings[["n_studies"]]
  mean_effect   <- settings[["mean_effect"]]
  heterogeneity <- settings[["heterogeneity"]]

  # Simulate samples sizes
  N_shape <- 2
  N_scale <- 58
  N_low   <- 25
  N_high  <- 500

  N_seq <- seq(N_low, N_high, 1)
  N_den <- stats::dnbinom(N_seq, size = N_shape, prob = 1/(N_scale+1) ) /
      (stats::pnbinom(N_high, size = N_shape, prob = 1/(N_scale+1) ) - stats::pnbinom(N_low - 1, size = N_shape, prob = 1/(N_scale+1) ))

  N  <- sample(N_seq, n_studies, TRUE, N_den)

  # Compute Cohen's d based on unit variance and equal sample size
  standard_errors <- sqrt(4/N)

  # Simulate true effect sizes
  effect_sizes <- stats::rnorm(n_studies, mean_effect, sqrt(heterogeneity^2 + standard_errors^2))

  # Create result data frame
  data <- data.frame(
    yi  = effect_sizes,
    sei = standard_errors
  )

  return(data)
}

#' @export
validate_dgm_settigns.no_bias <- function(dgm_name, settings) {

  # Check that all required settings are specified
  required_params <- c("n_studies", "mean_effect", "heterogeneity")
  missing_params <- setdiff(required_params, names(settings))
  if (length(missing_params) > 0)
    stop("Missing required settings: ", paste(missing_params, collapse = ", "))

  # Extract settings
  n_studies     <- settings[["n_studies"]]
  mean_effect   <- settings[["mean_effect"]]
  heterogeneity <- settings[["heterogeneity"]]

  # Validate settings
  if (length(n_studies) != 1 || !is.numeric(n_studies) || is.na(n_studies) || !is.wholenumber(n_studies) || n_studies < 1)
    stop("'n_studies' must be an integer larger targer than 0")
  if (length(mean_effect) != 1 || !is.numeric(mean_effect) || is.na(mean_effect))
    stop("'mean_effect' must be numeric")
  if (length(heterogeneity) != 1 || !is.numeric(heterogeneity) || is.na(heterogeneity) || heterogeneity < 0)
    stop("'heterogeneity' must be non-negative")

  return(invisible(TRUE))
}
