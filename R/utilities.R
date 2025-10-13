
#' @importFrom Rdpack reprompt
#' @keywords internal
"_PACKAGE"

### create environment to store package settings
#' @title Options for the PublicationBiasBenchmark package
#'
#' @description A placeholder object and functions for the PublicationBiasBenchmark package.
#'
#' @param name the name of the option to get the current value of - for a list of
#' available options, see details below.
#' @param ... named option(s) to change - for a list of available options, see
#' details below.
#'
#' @details
#' \describe{
#'   \item{\code{"simulation_directory"}}{Location where the benchmark data/results/measures are stored}
#'   \item{\code{"prompt_for_download"}}{Whether each file download should ask for explicit approval}
#' }
#'
#'
#' @return The current value of all available PublicationBiasBenchmark options (after applying any
#' changes specified) is returned invisibly as a named list.
#'
#' @export PublicationBiasBenchmark.options
#' @export PublicationBiasBenchmark.get_option
#' @name PublicationBiasBenchmark_options
#' @aliases PublicationBiasBenchmark_options PublicationBiasBenchmark.options PublicationBiasBenchmark.get_option
NULL

#' @rdname PublicationBiasBenchmark_options
PublicationBiasBenchmark.options    <- function(...){

  opts <- list(...)

  for(i in seq_along(opts)){

    if(!names(opts)[i] %in% names(PublicationBiasBenchmark.private))
      stop(paste("Unmatched or ambiguous option '", names(opts)[i], "'", sep=""))

    assign(names(opts)[i], opts[[i]] , envir = PublicationBiasBenchmark.private)
  }

  return(invisible(PublicationBiasBenchmark.private$options))
}

#' @rdname PublicationBiasBenchmark_options
PublicationBiasBenchmark.get_option <- function(name){

  if(length(name)!=1)
    stop("Only 1 option can be retrieved at a time")

  if(!name %in% names(PublicationBiasBenchmark.private))
    stop(paste("Unmatched or ambiguous option '", name, "'", sep=""))

  # Use eval as some defaults are put in using 'expression' to avoid evaluating at load time:
  return(eval(PublicationBiasBenchmark.private[[name]]))
}

PublicationBiasBenchmark.private <- new.env()
assign("simulation_directory", "notset", envir = PublicationBiasBenchmark.private)
assign("prompt_for_download", TRUE, envir = PublicationBiasBenchmark.private)


.onLoad <- function(libname, pkgname){

  PublicationBiasBenchmark.private$simulation_directory <- file.path(getwd(), "resources")

}

.onAttach <- function(libname, pkgname){

  packageStartupMessage(sprintf(
    "Data, results, and measures will be saved to '%1$s'.\nTo change the default location, use `PublicationBiasBenchmark.options(simulation_directory = `/path/`)`",
    PublicationBiasBenchmark.private$simulation_directory
  ))

}
