
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
#'   \item{\code{"resources_directory"}}{Location where the benchmark data/results/measures are stored}
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
assign("resources_directory",  NULL, envir = PublicationBiasBenchmark.private)
assign("prompt_for_download",  TRUE, envir = PublicationBiasBenchmark.private)


.onLoad   <- function(libname, pkgname){

  # locate the pre-downloaded results
  resources <- Sys.getenv("PublicationBiasBenchmark_RESOURCES")
  if (resources != "")
    PublicationBiasBenchmark.options(resources_directory = resources)

  # set-up OSF PAT
  try(suppressWarnings(suppressMessages(osfr::osf_auth())))
}
.onAttach <- function(libname, pkgname){

  resources <- PublicationBiasBenchmark.get_option("resources_directory")
  if (is.null(resources)) {
    packageStartupMessage(paste0(
      "This package works with precomputed data, results, and measures.\n",
      "Specify a location where those resources should be stored and accessed from by using `PublicationBiasBenchmark.options(resources_directory = '/path/')` ", 
      "or the `PublicationBiasBenchmark_RESOURCES` environmental variable."
    ))        
  } else {
    packageStartupMessage(sprintf(paste0(
      "Data, results, and measures will be stored and accessed from '%1$s'.\n", 
      "To change the default location, use `PublicationBiasBenchmark.options(resources_directory = '/path/')` ",
      "or the `PublicationBiasBenchmark_RESOURCES` environmental variable."),
      PublicationBiasBenchmark.private$resources_directory
    ))    
  }
}

.get_path <- function() {
  
  path <- PublicationBiasBenchmark.get_option("resources_directory")
  if (is.null(path))
    stop("The resources location needs to be specified via the `PublicationBiasBenchmark.options(resources_directory = '/path/')` function.", call. = FALSE)
  
  return(path)
}
