#' @title  Generic method function for publication bias correction
#'
#' @description
#' This function provides a unified interface to various publication bias
#' correction methods. The specific method is determined by the first argument.
#'
#' @param method_name Character string specifying the method type
#' @param data Data frame containing yi (effect sizes) and sei (standard errors)
#' @param settings Either a character identifying a method version or list
#' containing method-specific settings. An emty input will result in running the
#' default (first implemented) version of the method.
#'
#' @return A data frame with standardized method results
#' @export
#'
#' @examples
#' # Example usage with PET method
#' data <- data.frame(
#'   yi = c(0.2, 0.3, 0.1, 0.4),
#'   sei = c(0.1, 0.15, 0.08, 0.12)
#' )
#' result <- run_method("RMA", data, "v1")
#'
#' # Example usage with PETPEESE method
#' # result <- method("PETPEESE", data)
run_method <- function(method_name, data, settings = NULL) {

  # Allow calling methods with pre-specified `settings`
  if (length(settings) == 1 && is.character(settings)) {
    settings <- get_method_setting(method_name, settings)
  } else if (length(settings) == 0) {
    settings <- get_method_setting(method_name, "default")
  }

  # Call the method with the pre-specified settings
  method(method_name, data, settings)
}

#' @title Method Method
#' @inheritParams run_method
#'
#' @export
method <- function(method_name, data, settings) {

  # Convert character to appropriate class for dispatch
  if (is.character(method_name)) {
    method <- structure(method_name, class = method_name)
  } else {
    method <- method_name
  }

  UseMethod("method", method)
}

#' @title Default method handler
#' @inheritParams method
#'
#' @export
method.default <- function(method_name, data, settings = list()) {
  available_methods <- c("PET")
  stop("Unknown method type: '", class(method_name)[1],
       "'. Available methods: ", paste(available_methods, collapse = ", "))
}


#' @title Return Pre-specified Method Settings
#'
#' @description
#' This function returns the list of pre-specified settings for a given Method
#'
#' @inheritParams dgm
#' @param version_id which method version should be used.
#'
#' @return A list containing the pre-specified settings. For most methods, the
#' list contains extension of the function call, however, a more elaborate list
#' of settings that is dispatched within the method call is possible.
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
#' @name method_settings
NULL

#' @rdname method_settings
#' @export
method_settings <- function(method_name) {

  # Convert character to appropriate class for dispatch
  if (is.character(method_name)) {
    method_type <- structure(method_name, class = method_name)
  } else {
    method_type <- method_name
  }

  UseMethod("method_settings", method_type)
}

#' @rdname method_settings
#' @export
get_method_setting <- function(method_name, version_id) {

  settings     <- method_settings(method_name)
  this_setting <- settings[[version_id]]

  if (is.null(this_setting))
    stop("No matching 'version_id' found")

  return(this_setting)
}


#' @title Create standardized empty method result for convergence failures
#'
#' @param method_name Character string of the method name
#' @param note Character string describing the failure reason
#' @param extra_columns Named list of additional columns to include
#'
#' @return Data frame with standardized empty result structure
#' @export
create_empty_result <- function(method_name, note, extra_columns = list()) {

  # Base columns that all methods should have
  base_result <- data.frame(
    method         = method_name,
    estimate       = NA,
    standard_error = NA,
    ci_lower       = NA,
    ci_upper       = NA,
    p_value        = NA,
    BF             = NA,
    convergence    = FALSE,
    note           = note
  )

  # Add any extra columns specific to certain methods
  if (length(extra_columns) > 0) {
    for (col_name in names(extra_columns)) {
      base_result[[col_name]] <- extra_columns[[col_name]]
    }
  }

  return(base_result)
}
