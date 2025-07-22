#' Generic data generating mechanism function
#'
#' This function provides a unified interface to various data generating
#' mechanisms for simulation studies. The specific DGM is determined by
#' the first argument.
#'
#' @param dgm_name Character string specifying the DGM type
#' @param settings List containing the required parameters for the DGM or numeric settings_id
#'
#' @return A data frame containing the generated data with standardized structure
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
  available_dgms <- c("no_bias")
  stop("Unknown DGM type: '", class(dgm_name)[1],
       "'. Available DGMs: ", paste(available_dgms, collapse = ", "))
}

#' @export
validate_dgm_settigns <- function(dgm_name, settings) {

  # Convert character to appropriate class for dispatch
  if (is.character(dgm_name)) {
    dgm_type <- structure(dgm_name, class = dgm_name)
  } else {
    dgm_type <- dgm_name
  }

  UseMethod("validate_dgm_settigns", dgm_type)
}
