#' @title Download Datasets of a DGM
#'
#' @description
#' This function downloads datasets of a specified Data Generating Mechanism (DGM).
#' All data are located at \url{https://osf.io/exf3m/}.
#'
#' @param dgm_name Character string specifying the name of the DGM dataset to download.
#' @param path Character string specifying the directory path where the dataset should
#' be saved. Defaults to the current working directory.
#' @param overwrite Logical indicating whether to overwrite existing files.
#' Defaults to \code{FALSE}, which means only missing files will be downloaded.
#'
#' @return \code{TRUE} if the download was successful, otherwise an error is raised.
#'
#' @examples
#' \dontrun{
#'   download_dgm_datasets("no_bias")
#' }
#'
#' @aliases download_dgm_datasets download_dgm_results download_dgm_metrics
#' @name download_dgm
NULL

#' @rdname download_dgm
#' @export
download_dgm_datasets <- function(dgm_name, path = getwd(), overwrite = FALSE) {
  .download_dgm_fun(dgm_name, what = "data", path = path, overwrite = overwrite)
}

#' @rdname download_dgm
#' @export
download_dgm_results <- function(dgm_name, path = getwd(), overwrite = FALSE) {
  .download_dgm_fun(dgm_name, what = "results", path = path, overwrite = overwrite)
}

#' @rdname download_dgm
#' @export
download_dgm_metrics <- function(dgm_name, path = getwd(), overwrite = FALSE) {
  .download_dgm_fun(dgm_name, what = "metrics", path = path, overwrite = overwrite)
}


.download_dgm_fun <- function(dgm_name, what, path, overwrite) {

  # get link to the repository
  osf_link <- "https://osf.io/exf3m/"

  # connect to the repository
  osf_repo <- osfr::osf_retrieve_node(osf_link)

  # select the data folder
  osf_files <- osfr::osf_ls_files(osf_repo, path = file.path(dgm_name, what))

  ### download all condition datasets to the specified folder
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
  for (i in seq_len(nrow(osf_files))) {

    # file name
    temp_fname <- osf_files$name[i]

    # save file if it does not exist or if overwrite is TRUE
    if (!file.exists(file.path(data_path, temp_fname)) || overwrite) {
      osfr::osf_download(osf_files[i, ], path = data_path)
    }
  }

  return(invisible(TRUE))
}


#' @title Retrieve a Pre-Simulated Condition and Repetition From a DGM
#'
#' @description
#' This function returns a pre-simulated dataset of a given repetition and
#' condition from a dgm. The pre-simulated datasets must be already stored
#' locally. See [download_dgm_datasets()] function for more guidance.
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
#' \dontrun{
#'   # get condition 1, repetition 1
#'   retrieve_dgm_dataset("no_bias", condition_id = 1, repetition_id = 1)
#'
#'   # get condition 1, all repetitions
#'   retrieve_dgm_dataset("no_bias", condition_id = 1)
#' }
#'
#'
#' @export
retrieve_dgm_dataset <- function(dgm_name, condition_id, repetition_id = NULL, path = getwd()){

  if (missing(dgm_name))
    stop("'dgm_name' must be specified")
  if (missing(condition_id))
    stop("'condition_id' must be specified")

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
#' @param method Which method should be returned. The complete
#' results can be returned by setting to \code{NULL}.
#'
#' @return A data.frame
#'
#' @examples
#' \dontrun{
#'   # get condition 1, repetition 1
#'   retrieve_dgm_results("no_bias", condition_id = 1, repetition_id = 1)
#'
#'   # get condition 1, all repetitions
#'   retrieve_dgm_results("no_bias", condition_id = 1)
#' }
#'
#'
#' @export
retrieve_dgm_results <- function(dgm_name, method = NULL, condition_id = NULL, repetition_id = NULL, path = getwd()){

  if (missing(dgm_name))
    stop("'dgm_name' must be specified")

  # check that the directory / condition folders exist
  results_path <- file.path(path, dgm_name, "results")
  if (!dir.exists(results_path))
    stop(sprintf("Computed results of the specified dgm '%1$s' cannot be locatated at the specified location '%2$s'. You might need to dowload the computed results using the 'download_dgm_results()' function first.", dgm_name, path))

  # return ithe specific methods results or all results
  if (!is.null(method) && length(method) == 1) {

    # check that the corresponding file was downloaded
    if (!file.exists(file.path(results_path, paste0(method, ".csv"))))
      stop(sprintf("Computed results of the '%1$s' method for '%2$s' dgm cannot be locatated at the specified location '%3$s'.", method, condition_id, results_path))

    # load the file
    results_file <- utils::read.csv(file = file.path(results_path, paste0(method, ".csv")), header = TRUE)

  } else {

    method_results <- list.files(results_path)

    if (length(method_results) == 0)
      stop(sprintf("There are no computed results for '%1$s' dgm locatated at the specified location '%2$s'.", condition_id, results_path))

    results_file <- lapply(method_results, function(method_result) utils::read.csv(file = file.path(results_path, method_result), header = TRUE))
    results_file <- save_rbind(results_file)

  }

  # subset the condition / repetition if specified
  # return the complete file if repetition_id is not specified
  if (!is.null(condition_id)) {
    results_file <- results_file[results_file$condition_id %in% condition_id, ]
  }
  if (!is.null(repetition_id)) {
    results_file <- results_file[results_file$repetition_id %in% repetition_id, ]
  }

  return(results_file)
}


#' @title Retrieve Pre-Computed Performance Metrics for a DGM
#'
#' @description
#' This function returns pre-computed performance metrics for a specified
#' Data Generating Mechanism (DGM). The pre-computed metrics must be already stored
#' locally. See [download_dgm_metrics()] function for more guidance.
#'
#' @inheritParams dgm
#' @inheritParams download_dgm
#' @inheritParams dgm_conditions
#' @inheritParams retrieve_dgm_dataset
#' @param metric Which performance metric should be returned (e.g., "bias", "mse", "coverage").
#' All metrics can be returned by setting to \code{NULL}.
#' @param method Which method should be returned. All methods can be returned by setting to \code{NULL}.
#'
#' @return A data.frame
#'
#' @examples
#' \dontrun{
#'   # get bias metrics for all methods and conditions
#'   retrieve_dgm_metrics("no_bias", metric = "bias")
#'
#'   # get all metrics for RMA method
#'   retrieve_dgm_metrics("no_bias", method = "RMA")
#'
#'   # get MSE metrics for PET method in condition 1
#'   retrieve_dgm_metrics("no_bias", metric = "mse", method = "PET", condition_id = 1)
#' }
#'
#' @export
retrieve_dgm_metrics <- function(dgm_name, metric = NULL, method = NULL, condition_id = NULL, path = getwd()){

  if (missing(dgm_name))
    stop("'dgm_name' must be specified")

  # check that the directory / metrics folders exist
  metrics_path <- file.path(path, dgm_name, "metrics")
  if (!dir.exists(metrics_path))
    stop(sprintf("Computed metrics of the specified dgm '%1$s' cannot be located at the specified location '%2$s'. You might need to download the computed metrics using the 'download_dgm_metrics()' function first.", dgm_name, path))

  # return the specific metric results or all metrics
  if (!is.null(metric) && length(metric) == 1) {

    # check that the corresponding file was downloaded
    if (!file.exists(file.path(metrics_path, paste0(metric, ".csv"))))
      stop(sprintf("Computed metrics '%1$s' for '%2$s' dgm cannot be located at the specified location '%3$s'.", metric, dgm_name, metrics_path))

    # load the file
    metrics_file <- utils::read.csv(file = file.path(metrics_path, paste0(metric, ".csv")), header = TRUE)

  } else {

    metric_files <- list.files(metrics_path, pattern = "\\.csv$")

    if (length(metric_files) == 0)
      stop(sprintf("There are no computed metrics for '%1$s' dgm located at the specified location '%2$s'.", dgm_name, metrics_path))

    metrics_files <- lapply(metric_files, function(metric_file) {
      utils::read.csv(file = file.path(metrics_path, metric_file), header = TRUE)
    })
    metrics_file <- save_merge(metrics_files)

  }

  # subset by method, condition, repetition if specified
  if (!is.null(method)) {
    metrics_file <- metrics_file[metrics_file$method %in% method, ]
  }
  if (!is.null(condition_id)) {
    metrics_file <- metrics_file[metrics_file$condition %in% condition_id, ]
  }

  return(metrics_file)
}


