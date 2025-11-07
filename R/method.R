#' @title  Generic method function for publication bias correction
#'
#' @description
#' This function provides a unified interface to various publication bias
#' correction methods. The specific method is determined by the first argument.
#' See
#' \href{../doc/Adding_New_Methods.html}{\code{vignette("Adding_New_Methods", package = "PublicationBiasBenchmark")}}
#' for details of extending the package with new methods
#'
#' @param method_name Character string specifying the method type
#' @param data Data frame containing yi (effect sizes) and sei (standard errors)
#' @param settings Either a character identifying a method version or list
#' containing method-specific settings. An emty input will result in running the
#' default (first implemented) version of the method.
#' @param silent Logical indicating whether error messages from the method should be suppressed.
#'
#' @section Output Structure:
#' The returned data frame follows a standardized schema that downstream
#' functions rely on. All methods return the following columns:
#' \itemize{
#'   \item \code{method} (character): The name of the method used.
#'   \item \code{estimate} (numeric): The meta-analytic effect size estimate.
#'   \item \code{standard_error} (numeric): Standard error of the estimate.
#'   \item \code{ci_lower} (numeric): Lower bound of the 95% confidence interval.
#'   \item \code{ci_upper} (numeric): Upper bound of the 95% confidence interval.
#'   \item \code{p_value} (numeric): P-value for the estimate.
#'   \item \code{BF} (numeric): Bayes Factor for the estimate.
#'   \item \code{convergence} (logical): Whether the method converged successfully.
#'   \item \code{note} (character): Additional notes describing convergence issues.
#' }
#' Some methods may include additional method-specific columns beyond these
#' standard columns. Use [get_method_extra_columns()] to query which
#' additional columns a particular method returns.
#'
#' @return A data frame with standardized method results
#'
#' @examples
#' # Example usage with RMA method
#' data <- data.frame(
#'   yi = c(0.2, 0.3, 0.1, 0.4),
#'   sei = c(0.1, 0.15, 0.08, 0.12)
#' )
#' result <- run_method("RMA", data, "default")
#' @export
run_method <- function(method_name, data, settings = NULL, silent = FALSE) {

  # Allow calling methods with pre-specified `settings`
  if (length(settings) == 1 && is.character(settings)) {
    settings_name <- settings
    settings      <- get_method_setting(method_name, settings)
  } else if (length(settings) == 0) {
    settings_name <- "default"
    settings      <- get_method_setting(method_name, "default")
  } else {
    settings_name <- "<custom>"
  }

  # Call the method with the pre-specified settings
  results <- try(method(method_name, data, settings), silent = silent)

  # In case of error, return the error message and append the method specific columns
  if (inherits(results, "try-error")) {
    results <- create_empty_result(
      method_name   = method_name,
      note          = as.character(results),
      extra_columns = get_method_extra_columns(method_name)
    )
  }

  # Append the method settigns
  results$method_setting <- settings_name

  return(results)
}

#' @title Method Method
#' @description
#' S3 Method for defining methods. See [run_method()] for
#' usage and further details.
#'
#' @inheritParams run_method
#' @inheritSection run_method Output Structure
#'
#' @return A data frame with method results following the structure described
#' in the Output Structure section. This is an S3 generic method that dispatches
#' to specific method implementations based on \code{method_name}.
#'
#' @seealso [run_method()]
#' @examples
#'
#' data <- data.frame(
#'   yi = c(0.2, 0.3, 0.1, 0.4),
#'   sei = c(0.1, 0.15, 0.08, 0.12)
#' )
#' result <- run_method("RMA", data, "default")
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
#' @return Throws an error indicating the method type is unknown. This default
#' method is only called when no specific method implementation is found for the
#' given \code{method_name}.
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
#' @inheritParams method
#' @param version_id which method version should be used.
#'
#' @return A list containing the pre-specified settings. For most methods, the
#' list contains extension of the function call, however, a more elaborate list
#' of settings that is dispatched within the method call is possible.
#'
#' @examples
#' method_settings("RMA")
#' get_method_setting("RMA", version_id = "default")
#'
#' @aliases method_settings get_method_setting
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
#' @param extra_columns Character vector of additional empty columns to add to the table
#'
#' @return Data frame with standardized empty result structure
#' @export
create_empty_result <- function(method_name, note, extra_columns = NULL) {

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
  for (i in seq_along(extra_columns)) {
    base_result[[extra_columns[i]]] <- NA
  }

  return(base_result)
}


#' @title Method Extra Columns
#'
#' @description
#' Retrieves the character vector of custom columns for a given method.
#' These are method-specific columns beyond the standard columns
#' (method, estimate, standard_error, ci_lower, ci_upper, p_value, BF,
#' convergence, note) that each method returns.
#'
#' When implementing new methods, consider using standardized column names
#' for consistency: \describe{
#'   \item{Heterogeneity}{\code{tau_estimate}, \code{tau_ci_lower}, \code{tau_ci_upper},
#'         \code{tau_p_value}, \code{tau_BF}}
#'   \item{Publication Bias}{\code{bias_coefficient}, \code{bias_coefficient_se},
#'         \code{bias_p_value}, \code{bias_BF}}
#' }
#'
#' @param method_name Character string of the method name
#'
#' @return Character vector of extra column names, or empty character vector
#' if no extra columns are defined for the method
#'
#' @examples
#' # Get extra columns for PET method
#' get_method_extra_columns("PET")
#'
#' # Get extra columns for RMA method
#' get_method_extra_columns("RMA")
#'
#' @aliases method_extra_columns get_method_extra_columns
#' @name method_extra_columns
NULL

#' @rdname method_extra_columns
#' @export
get_method_extra_columns <- function(method_name) {

  # Convert character to appropriate class for dispatch
  if (is.character(method_name)) {
    method_type <- structure(method_name, class = method_name)
  } else {
    method_type <- method_name
  }

  UseMethod("method_extra_columns", method_type)
}

#' @rdname method_extra_columns
#' @export
method_extra_columns <- function(method_name) {

  # Convert character to appropriate class for dispatch
  if (is.character(method_name)) {
    method_type <- structure(method_name, class = method_name)
  } else {
    method_type <- method_name
  }

  UseMethod("method_extra_columns", method_type)
}

#' @rdname method_extra_columns
#' @export
method_extra_columns.default <- function(method_name)
  character(0)
