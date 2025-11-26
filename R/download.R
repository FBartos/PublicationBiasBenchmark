#' @title Download Datasets/Results/Measures of a DGM
#'
#' @description
#' This function downloads datasets/results/measures of a specified Data-Generating Mechanism (DGM)
#' from the OSF repository (\url{https://osf.io/exf3m/}). The datasets/results/measures are saved
#' to the location specified via \code{PublicationBiasBenchmark.options(resources_directory = "/path/")}.
#' To set the location permanently, specify the PublicationBiasBenchmark_RESOURCES environment
#' variable. The data are stored in dgm_name/datasets, dgm_name/results, dgm_name/measures subfolders.
#'
#' @param dgm_name Character string specifying the name of the DGM dataset to download.
#' @param overwrite Logical indicating whether to overwrite existing files.
#' Defaults to \code{FALSE}, which means only missing files will be downloaded.
#' @param progress Logical indicating whether to show progress downloading files.
#' Defaults to \code{TRUE}.
#' @param max_try Integet specifying how many times should the function attempt reconnecting to OSF upon failure.
#'
#' @return \code{TRUE} if the download was successful, otherwise an error is raised.
#'
#' @examples
#' \donttest{
#'   download_dgm_datasets("no_bias")
#' }
#'
#' @aliases download_dgm_datasets download_dgm_results download_dgm_measures
#' @name download_dgm
NULL

#' @rdname download_dgm
#' @export
download_dgm_datasets <- function(dgm_name, overwrite = FALSE, progress = TRUE, max_try = 10) {
  .download_dgm_fun(dgm_name, what = "data", overwrite = overwrite, progress = progress, max_try = max_try)
}

#' @rdname download_dgm
#' @export
download_dgm_results <- function(dgm_name, overwrite = FALSE, progress = TRUE, max_try = 10) {
  .download_dgm_fun(dgm_name, what = "results", overwrite = overwrite, progress = progress, max_try = max_try)
}

#' @rdname download_dgm
#' @export
download_dgm_measures <- function(dgm_name, overwrite = FALSE, progress = TRUE, max_try = 10) {
  .download_dgm_fun(dgm_name, what = "measures", overwrite = overwrite, progress = progress, max_try = max_try)
}


.download_dgm_fun <- function(dgm_name, what, overwrite, progress, max_try) {

  # add a warning for missing token
  if (Sys.getenv("OSF_PAT") == "")
    stop("Please set up 'OSF_PAT' environmental variable. The file download is unreliable otherwise. See '?osfr::osf_auth' for instructions.", call. = FALSE)

  path <- .get_path()

  # get link to the repository
  osf_link <- .get_osf_link(dgm_name)

  # connect to the repository
  osf_repo <- osfr::osf_retrieve_node(osf_link)

  # select the data folder
  osf_files <- osfr::osf_ls_files(osf_repo, path = what, n_max = Inf)

  ### download all datasets to the specified folder
  # check the directory name
  dgm_path <- file.path(path, dgm_name)
  if (!dir.exists(dgm_path)) {
    dir.create(dgm_path, recursive = TRUE)
  }

  # check the data folder
  data_path <- file.path(path, dgm_name, what)
  if (!dir.exists(data_path)) {
    dir.create(data_path)
  }

  # download the individual files
  if (!overwrite) {
    current_files <- list.files(data_path)
    osf_files     <- osf_files[!osf_files$name %in% current_files,]

    if(nrow(osf_files) == 0) {
      if (progress) message("All files are already downloaded.")
      return(invisible(TRUE))
    }
  }

  # Calculate the total size
  if (PublicationBiasBenchmark.get_option("prompt_for_download")) {
    size_MB <- sum(sapply(seq_len(nrow(osf_files)), function(i) osf_files$meta[[i]]$attributes$size / 1024^2))
    rl      <- readline(sprintf("You are about to download %1$i files (%2$.2f %3$s) to '%4$s'. Do you want to proceed? [Y, n]",
                                nrow(osf_files),
                                ifelse(size_MB > 1024, size_MB / 1024, size_MB),
                                ifelse(size_MB > 1024, "GB" , "MB"),
                                data_path
                                ))
    message("(Set `PublicationBiasBenchmark.options('prompt_for_download' = FALSE)` to skip this message in the future)")
    rl <- tolower(as.character(rl))
    if (!((rl == "" || substr(rl, 1, 1) == "y")))
      return(invisible(FALSE))
  }

  # download the files
  # to allow for recovery in the case of errors, delete the local files manually on overwrite
  if (overwrite) {
    unlink(data_path)
  }

  # add error catching and restart on failure
  done      <- FALSE
  iteration <- 0
  while (!done && iteration < max_try) {

    # skip files already present
    files_done  <- list.files(data_path)
    osf_files   <- osf_files[!osf_files$name %in% current_files,]

    if (length(osf_files) == 0)
      break

    done      <- try(osfr::osf_download(osf_files, path = data_path, conflicts = ifelse(overwrite, "overwrite", "skip"), progress = progress))
    done      <- !inherits(done, "try-error")
    iteration <- iteration + 1
  }

  if (iteration == max_try)
    warning("Maximum number of restarts reached. Some files might be missing.")

  return(invisible(TRUE))
}

.get_osf_link <- function(dgm_name) {
  switch(
    dgm_name,
    "no_bias"      = "https://osf.io/q8phr",
    "Alinaghi2018" = "https://osf.io/5hbm8",
    "Bom2019"      = "https://osf.io/4bcr2",
    "Carter2019"   = "https://osf.io/vcs85",
    "Stanley2017"  = "https://osf.io/fg62w"
    )
}

#' @title Retrieve a Pre-Simulated Condition and Repetition From a DGM
#'
#' @description
#' This function returns a pre-simulated dataset of a given repetition and
#' condition from a dgm. The pre-simulated datasets must be already stored
#' locally. See [download_dgm] function for more guidance.
#'
#' @inheritParams dgm
#' @inheritParams download_dgm
#' @inheritParams dgm_conditions
#' @param repetition_id Which repetition should be returned. The complete
#' condition can be returned by setting to either \code{NULL}.
#'
#' @return A data.frame
#'
#' @examples
#' \donttest{
#'   # get condition 1, repetition 1
#'   retrieve_dgm_dataset("no_bias", condition_id = 1, repetition_id = 1)
#'
#'   # get condition 1, all repetitions
#'   retrieve_dgm_dataset("no_bias", condition_id = 1)
#' }
#'
#'
#' @export
retrieve_dgm_dataset <- function(dgm_name, condition_id, repetition_id = NULL){

  if (missing(dgm_name))
    stop("'dgm_name' must be specified")
  if (missing(condition_id))
    stop("'condition_id' must be specified")

  path <- .get_path()

  # check that the directory / condition folders exist
  data_path <- file.path(path, dgm_name, "data")
  if (!dir.exists(data_path))
    stop(sprintf("Simulated datasets of the specified dgm '%1$s' cannot be locatated at the specified location '%2$s'. You might need to dowload the simulated datasets using the 'download_dgm_datasets()' function first.", dgm_name, path))

  # check the conditions exists
  this_condition <- get_dgm_condition(dgm_name, condition_id) # throws error if does not exist

  # check that the corresponding file was downloaded
  if (!file.exists(file.path(data_path, paste0(condition_id, ".csv"))))
    stop(sprintf("Simulated condition of the '%1$s' dgm cannot be locatated at the specified location '%2$s'.", condition_id, data_path))

  # load the file
  condition_file <- utils::read.csv(file = file.path(data_path, paste0(condition_id, ".csv")), header = TRUE)

  # return the complete file if repetition_id is not specified
  if (is.null(repetition_id))
    return(condition_file)

  # check that the specified repetition_id exists otherwise
  if (!any(repetition_id == unique(condition_file[["repetition_id"]])))
    stop(sprintf("The specified 'repetition_id' (%1$s) does not exist in the simulated dataset", as.character(repetition_id)))

  return(condition_file[condition_file[["repetition_id"]] == repetition_id,,drop=FALSE])
}


#' @title Retrieve a Pre-Computed Results of a Method Applied to DGM
#'
#' @description
#' This function returns a pre-computed results of a given method at a specific
#' repetition and condition from a dgm. The pre-computed results must be already stored
#' locally. See [download_dgm_results()] function for more guidance.
#'
#' @inheritParams dgm
#' @inheritParams download_dgm
#' @inheritParams dgm_conditions
#' @inheritParams retrieve_dgm_dataset
#' @param method Which method(s) should be returned. The complete results are returned by setting to \code{NULL} (default setting).
#' @param method_setting Which method setting(s) should be returned. The complete results are returned by setting to \code{NULL} (default setting).
#'
#' @return A data.frame
#'
#' @examples
#' \donttest{
#'   # get condition 1, repetition 1 for default method setting
#'   retrieve_dgm_results("no_bias", condition_id = 1, repetition_id = 1)
#'
#'   # get condition 1, all repetitions for default method setting
#'   retrieve_dgm_results("no_bias", condition_id = 1)
#' }
#'
#'
#' @export
retrieve_dgm_results <- function(dgm_name, method = NULL, method_setting = NULL, condition_id = NULL, repetition_id = NULL){

  if (missing(dgm_name))
    stop("'dgm_name' must be specified")

  path <- .get_path()

  # check that the directory / condition folders exist
  results_path <- file.path(path, dgm_name, "results")
  if (!dir.exists(results_path))
    stop(sprintf("Computed results of the specified dgm '%1$s' cannot be locatated at the specified location '%2$s'. You might need to dowload the computed results using the 'download_dgm_results()' function first.", dgm_name, path))

  # return the specific methods results or all results
  if (length(method) == 1 && length(method_setting) == 1) {

    # construct the method-method_setting filename
    method_filename <- paste0(method, "-", method_setting, ".csv")

    # check that the corresponding file was downloaded
    if (!file.exists(file.path(results_path, method_filename)))
      stop(sprintf("Computed results of the '%1$s-%2$s' method for '%3$s' dgm cannot be locatated at the specified location '%4$s'.", method, method_setting, dgm_name, results_path))

    # load the file
    results_file <- utils::read.csv(file = file.path(results_path, method_filename), header = TRUE)

  } else {

    method_results <- list.files(results_path)

    if (length(method_results) == 0)
      stop(sprintf("There are no computed results for '%1$s' dgm locatated at the specified location '%2$s'.", condition_id, results_path))

    results_file <- lapply(method_results, function(method_result) utils::read.csv(file = file.path(results_path, method_result), header = TRUE))
    results_file <- safe_rbind(results_file)

  }

  # subset by method, settings, condition, repetition if specified
  if (!is.null(method)) {
    results_file <- results_file[results_file$method %in% method, ]
  }
  if (!is.null(method_setting)) {
    results_file <- results_file[results_file$method_setting %in% method_setting, ]
  }
  if (!is.null(condition_id)) {
    results_file <- results_file[results_file$condition %in% condition_id, ]
  }
  if (!is.null(repetition_id)) {
    results_file <- results_file[results_file$repetition_id %in% repetition_id, ]
  }

  return(results_file)
}


#' @title Retrieve Pre-Computed Performance measures for a DGM
#'
#' @description
#' This function returns pre-computed performance measures for a specified
#' Data-Generating Mechanism (DGM). The pre-computed measures must be already stored
#' locally. See [download_dgm_measures()] function for more guidance.
#'
#' @inheritParams dgm
#' @inheritParams download_dgm
#' @inheritParams dgm_conditions
#' @inheritParams retrieve_dgm_results
#' @param measure Which performance measure should be returned (e.g., "bias", "mse", "coverage").
#' All measures can be returned by setting to \code{NULL}.
#' @param replacement Whether performance measures computed using replacement should be returned. Defaults to \code{FALSE}.
#'
#' @return A data.frame
#'
#' @examples
#' \donttest{
#'   # get bias measures for all methods and conditions
#'   retrieve_dgm_measures("no_bias", measure = "bias")
#'
#'   # get all measures for RMA method
#'   retrieve_dgm_measures("no_bias", method = "RMA")
#'
#'   # get MSE measures for PET method in condition 1
#'   retrieve_dgm_measures("no_bias", measure = "mse", method = "PET", condition_id = 1)
#' }
#'
#' @export
retrieve_dgm_measures <- function(dgm_name, measure = NULL, method = NULL, method_setting = NULL, condition_id = NULL, replacement = FALSE){

  if (missing(dgm_name))
    stop("'dgm_name' must be specified")

  path <- .get_path()

  # check that the directory / measures folders exist
  measures_path <- file.path(path, dgm_name, "measures")
  if (!dir.exists(measures_path))
    stop(sprintf("Computed measures of the specified dgm '%1$s' cannot be located at the specified location '%2$s'. You might need to download the computed measures using the 'download_dgm_measures()' function first.", dgm_name, path))

  # return the specific measure results or all measures
  if (length(measure) == 1) {

    # check that the corresponding file was downloaded
    file_name <- paste0(measure, if(replacement) "-replacement", ".csv")

    if (!file.exists(file.path(measures_path, file_name)))
      stop(sprintf("Computed measures '%1$s' for '%2$s' dgm cannot be located at the specified location '%3$s'.", measure, dgm_name, measures_path))

    # load the file
    measures_file <- utils::read.csv(file = file.path(measures_path, file_name), header = TRUE)

  } else {

    measure_files <- list.files(measures_path, pattern = "\\.csv$")

    # pairwise comparison must be handled manually
    if (length(measure) == 1 && measure == "pairwise") {
      measure_files <- measure_files[grepl("pairwise", measure_files)]
    } else {
      measure_files <- measure_files[!grepl("pairwise", measure_files)]
    }

    if (replacement) {
      measure_files <- measure_files[grepl("replacement", measure_files)]
    } else {
      measure_files <- measure_files[!grepl("replacement", measure_files)]
    }

    if (length(measure_files) == 0)
      stop(sprintf("There are no computed measures for '%1$s' dgm located at the specified location '%2$s'.", dgm_name, measures_path))

    measures_files <- lapply(measure_files, function(measure_file) {
      utils::read.csv(file = file.path(measures_path, measure_file), header = TRUE)
    })
    measures_file <- safe_merge(measures_files)

  }

  # subset by method, settings, condition if specified
  if (!is.null(method)) {
    measures_file <- measures_file[measures_file$method %in% method, ]
  }
  if (!is.null(method_setting)) {
    measures_file <- measures_file[measures_file$method_setting %in% method_setting, ]
  }
  if (!is.null(condition_id)) {
    measures_file <- measures_file[measures_file$condition %in% condition_id, ]
  }

  return(measures_file)
}


