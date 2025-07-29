#' Generic data generating mechanism function
#'
#' This function provides a unified interface to various data generating
#' mechanisms for simulation studies. The specific DGM is determined by
#' the first argument.
#'
#' @param dgm_name Character string specifying the DGM type
#' @param settings List containing the required parameters for the DGM or
#' numeric settings_id
#'
#' @return A data frame containing the generated data with standardized structure
#'
#' @examples
#' dgm("Carter2019", list(mean_effect = 0, effect_heterogeneity = 0,
#'                        bias = "high", QRP = "high", n_studies = 10))
#'
#' dgm("Alinaghi2019", list(environment = "FE", mean_effect = 0))
#'
#' dgm("Stanley2017", list(environment = "Cohens_d", mean_effect = 0,
#'                         effect_heterogeneity = 0, bias = 0, n_studies = 5,
#'                         sample_sizes = c(32,64,125,250,500)))
#'
#'
#' @seealso [validate_dgm_settings()],
#' [dgm.Stanley2017()],
#' [dgm.Alinaghi2019()],
#' [dgm.Bom2019()],
#' [dgm.Carter2019()]
#' @export
dgm <- function(dgm_name, settings) {

  # Convert character to appropriate class for dispatch
  if (is.character(dgm_name)) {
    dgm_type <- structure(dgm_name, class = dgm_name)
  } else {
    dgm_type <- dgm_name
  }

  UseMethod("dgm", dgm_type)
}

#' Default DGM handler
#' @export
dgm.default <- function(dgm_name, settings) {
  available_dgms <- c(
    "no_bias",                                              # example DGM
    "Alinaghi2019", "Stanley2017", "Bom2019", "Carter2019"  # DGMs based on Hong and Reed 2021
  )
  stop("Unknown DGM type: '", class(dgm_name)[1],
       "'. Available DGMs: ", paste(available_dgms, collapse = ", "))
}

#' @title Validate DGM Settings
#'
#' @description
#' This function validates the settings provided for a given Data
#' Generating Mechanism (DGM).
#'
#' @inheritParams dgm
#'
#' @return Error or \code{TRUE} depending whether the settings are valid for
#' the specified DGM.
#'
#' @examples
#' validate_dgm_settings("Carter2019", list(mean_effect = 0,
#'                         effect_heterogeneity = 0, bias = "high",
#'                         QRP = "high", n_studies = 10))
#'
#' validate_dgm_settings("Alinaghi2019", list(environment = "FE",
#'                         mean_effect = 0))
#'
#' validate_dgm_settings("Stanley2017", list(environment = "Cohens_d",
#'                         mean_effect = 0,
#'                         effect_heterogeneity = 0, bias = 0, n_studies = 5,
#'                         sample_sizes = c(32,64,125,250,500)))
#'
#' @export
validate_dgm_settings <- function(dgm_name, settings) {

  # Convert character to appropriate class for dispatch
  if (is.character(dgm_name)) {
    dgm_type <- structure(dgm_name, class = dgm_name)
  } else {
    dgm_type <- dgm_name
  }

  UseMethod("validate_dgm_settings", dgm_type)
}
