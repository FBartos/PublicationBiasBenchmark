#' @title Normal Unbiased Data-Generating Mechanism
#'
#' @author František Bartoš \email{f.bartos96@@gmail.com}
#'
#' @description
#' An example data-generating mechanism to simulate effect sizes without
#' publication bias.
#'
#' @param dgm_name DGM name (automatically passed)
#' @param settings List containing \describe{
#'   \item{mean_effect}{Mean effect}
#'   \item{heterogeneity}{Effect heterogeneity}
#'   \item{n_studies}{Number of effect size estimates}
#' }
#'
#' @details
#' Sample sizes of individual effect size estimates are generated from a
#' negative binomial distribution based on empirical sample size distribution
#' presented in Appendix B of
#' \insertCite{maier2023robust;textual}{PublicationBiasBenchmark}
#'
#'
#' @return Data frame with \describe{
#'   \item{yi}{effect size}
#'   \item{sei}{standard error}
#'   \item{ni}{sample size}
#'   \item{es_type}{effect size type}
#' }
#'
#' @references
#' \insertAllCited{}
#'
#' @seealso [dgm()], [validate_dgm_setting()]
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
    yi      = effect_sizes,
    sei     = standard_errors,
    ni      = N,
    es_type = "SMD"
  )

  return(data)
}

#' @export
validate_dgm_setting.no_bias <- function(dgm_name, settings) {

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

#' @export
dgm_conditions.no_bias <- function(dgm_name) {

  # generate a list of pre-specified settings
  settings <- data.frame(expand.grid(
    mean_effect    = c(0, 0.3),
    heterogeneity  = c(0, 0.15),
    n_studies      = c(10, 100)
  ))

  # attach setting id
  settings$condition_id <- 1:nrow(settings)

  return(settings)
}
