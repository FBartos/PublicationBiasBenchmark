#' Generic method function for publication bias correction
#'
#' This function provides a unified interface to various publication bias
#' correction methods. The specific method is determined by the first argument.
#'
#' @param method_name Character string specifying the method type
#' @param data Data frame containing yi (effect sizes) and sei (standard errors)
#' @param settings List containing method-specific settings
#' @param ... Additional arguments passed to specific methods
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
#' result <- method("PET", data)
#'
#' # Example usage with PETPEESE method
#' result <- method("PETPEESE", data)
method <- function(method_name, data, settings = list(), ...) {

  # Convert character to appropriate class for dispatch
  if (is.character(method_name)) {
    method_type <- structure(method_name, class = method_name)
  } else {
    method_type <- method_name
  }

  UseMethod("method", method_type)
}

#' Default method handler
#' @export
method.default <- function(method_name, data, settings = list(), ...) {
  available_methods <- c("PET")
  stop("Unknown method type: '", class(method_name)[1],
       "'. Available methods: ", paste(available_methods, collapse = ", "))
}


#' Create standardized empty method result for convergence failures
#'
#' @param method_name Character string of the method name
#' @param settings List of method settings to attach as attributes
#' @param note Character string describing the failure reason
#' @param data Data set passed to the method
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
