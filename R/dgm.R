#' @title Simulate From Data-Generating Mechanism
#'
#' @description
#' This function provides a unified interface to various data-generating
#' mechanisms for simulation studies. The specific DGM is determined by
#' the first argument.
#'
#' @param dgm_name Character string specifying the DGM type
#' @param settings List containing the required parameters for the DGM or
#' numeric condition_id
#'
#' @return A data frame containing the generated data with standardized structure
#'
#' @examples
#' simulate_dgm("Carter2019", list(mean_effect = 0, effect_heterogeneity = 0,
#'                        bias = "high", QRP = "high", n_studies = 10))
#'
#' simulate_dgm("Alinaghi2018", list(environment = "FE", mean_effect = 0))
#'
#' simulate_dgm("Stanley2017", list(environment = "Cohens_d", mean_effect = 0,
#'                         effect_heterogeneity = 0, bias = 0, n_studies = 5,
#'                         sample_sizes = c(32,64,125,250,500)))
#'
#'
#' @seealso [validate_dgm_setting()],
#' [dgm.Stanley2017()],
#' [dgm.Alinaghi2018()],
#' [dgm.Bom2019()],
#' [dgm.Carter2019()]
#' @export
simulate_dgm <- function(dgm_name, settings) {

  # Allow calling DGMs with pre-specified `condition_id`
  if (length(settings) == 1 && is.numeric(settings) && is.wholenumber(settings)) {
    settings <- get_dgm_condition(dgm_name, settings)
    settings <- as.list(settings)
    settings <- settings[names(settings) != "condition_id"]
  } else {
    validate_dgm_setting(dgm_name, settings)
  }

  # Call the DGM with the pre-specified settings
  dgm(dgm_name, settings)
}

#' @title DGM Method
#' @inheritParams simulate_dgm
#'
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

#' @title Default DGM handler
#' @inheritParams dgm
#'
#' @export
dgm.default <- function(dgm_name, settings) {
  available_dgms <- c(
    "no_bias",                                              # example DGM
    "Stanley2017", "Alinaghi2018", "Bom2019", "Carter2019"  # DGMs based on Hong and Reed 2021
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
#' validate_dgm_setting("Carter2019", list(mean_effect = 0,
#'                         effect_heterogeneity = 0, bias = "high",
#'                         QRP = "high", n_studies = 10))
#'
#' validate_dgm_setting("Alinaghi2018", list(environment = "FE",
#'                         mean_effect = 0))
#'
#' validate_dgm_setting("Stanley2017", list(environment = "Cohens_d",
#'                         mean_effect = 0,
#'                         effect_heterogeneity = 0, bias = 0, n_studies = 5,
#'                         sample_sizes = c(32,64,125,250,500)))
#'
#' @export
validate_dgm_setting <- function(dgm_name, settings) {

  # Convert character to appropriate class for dispatch
  if (is.character(dgm_name)) {
    dgm_type <- structure(dgm_name, class = dgm_name)
  } else {
    dgm_type <- dgm_name
  }

  UseMethod("validate_dgm_setting", dgm_type)
}

#' @title Return Pre-specified DGM Settings
#'
#' @description
#' This function returns the list of pre-specified settings for a given Data
#' Generating Mechanism (DGM).
#'
#' @inheritParams dgm
#' @param condition_id which conditions should settings be returned for.
#'
#' @return A data frame containing the pre-specified settings including a
#' `condition_id` column which maps settings id to the corresponding settings.
#'
#' @examples
#' dgm_conditions("Carter2019")
#' get_dgm_condition("Carter2019", condition_id = 1)
#'
#' dgm_conditions("Alinaghi2018")
#'
#' dgm_conditions("Stanley2017")
#'
#' @aliases dgm_conditions get_dgm_condition
#' @name dgm_conditions
NULL

#' @rdname dgm_conditions
#' @export
dgm_conditions <- function(dgm_name) {

  # Convert character to appropriate class for dispatch
  if (is.character(dgm_name)) {
    dgm_type <- structure(dgm_name, class = dgm_name)
  } else {
    dgm_type <- dgm_name
  }

  UseMethod("dgm_conditions", dgm_type)
}

#' @rdname dgm_conditions
#' @export
get_dgm_condition <- function(dgm_name, condition_id) {

  settings       <- dgm_conditions(dgm_name)
  this_condition <- settings[settings[["condition_id"]] == condition_id,,drop = FALSE]

  if (nrow(this_condition) == 0)
    stop("No matching 'condition_id' found")

  return(this_condition)
}
