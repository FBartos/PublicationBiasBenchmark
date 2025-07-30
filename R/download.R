#' @title Download Datasets of a DGM
#'
#' @description
#' This function downloads datasets of a specified Data Generating Mechanism (DGM).
#'
#' @param dgm_name Character string specifying the name of the DGM dataset to download.
#' @param path Character string specifying the directory path where the dataset should
#' be saved. Defaults to the current working directory.
#' @param add Logical indicating whether to add the downloaded data to an existing dataset.
#' Defaults to \code{FALSE}.
#'
#' @return \code{TRUE} if the download was successful, otherwise an error is raised.
#'
#' @examples
#' \dontrun{
#'   download_dgm_datasets("no_bias")
#' }
#'
#' @aliases download_dgm_datasets download_dgm_results
#' @name download_dgm
NULL

#' @rdname download_dgm
#' @export
download_dgm_datasets <- function(dgm_name, path = getwd(), add = FALSE) {
  .download_dgm_fun(dgm_name, what = "data", path = path, add = add)
}

#' @rdname download_dgm
#' @export
download_dgm_results <- function(dgm_name, path = getwd(), add = FALSE) {
  .download_dgm_fun(dgm_name, what = "results", path = path, add = add)
}

.download_dgm_fun <- function(dgm_name, what, path, add) {

  # get link to the repository
  osf_link <- dgm_repository(dgm_name)

  # connect to the repository
  osf_repo <- osfr::osf_retrieve_node(osf_link)

  # select the data folder
  osf_files <- osfr::osf_ls_files(osf_repo, path = what)

  ### download all condition datasets to the specified folder
  # check the directory name
  dgm_path <- file.path(path, dgm_name)
  if (!dir.exists(dgm_path)) {
    dir.create(dgm_path, recursive = TRUE)
  }

  # check the data folder
  data_path <- file.path(path, dgm_name, what)
  if (!add && dir.exists(data_path) && length(list.files(data_path)) != 0)
    stop(sprintf("A non-empty directory at '%1$s' already exists", data_path))
  if (!dir.exists(data_path)) {
    dir.create(data_path)
  }

  # download the individual files
  for (i in seq_len(nrow(osf_files))) {

    # file name
    temp_fname <- osf_files$name[i]

    # save file if it does not exist
    if (!file.exists(file.path(data_path, temp_fname))) {
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
#' condition can be returned by setting to either \code{FALSE}, \code{NULL} or
#' \code{NA}.
#'
#' @return A data.frame
#'
#' @examples
#' \dontrun{
#'   # get condition 1, repetition 1
#'   retrieve_dgm_dataset("no_bias", condition_id = 1, repetition_id = 1)
#'
#'   # get condition 1, all repetitions
#'   retrieve_dgm_dataset("no_bias", condition_id = 1, repetition_id = FALSE)
#' }
#'
#'
#' @export
retrieve_dgm_dataset <- function(dgm_name, condition_id, repetition_id, path = getwd()){

  if (missing(dgm_name))
    stop("'dgm_name' must be specified")
  if (missing(condition_id))
    stop("'condition_id' must be specified")
  if (missing(repetition_id))
    stop("'repetition_id' must be specified")
  # TODO: add check that the DGM exists

  # check that the directory / condition folders exist
  data_path <- file.path(path, dgm_name, "data")
  if (!dir.exists(data_path))
    stop(sprintf("Simulated datasets of the specified dgm '%1$s' cannot be locatated at the specified location '%2$s'. You might need to dowload the simulated datasets using the 'download_dgm_datasets()' function first.", dgm_name, path))

  # check the conditions exists
  this_condition <- get_dgm_condition(dgm_name, condition_id) # throws error if does not exist

  # check that the corresponding file was downloaded
  if (!file.exists(file.path(data_path, paste0(condition_id, ".csv"))))
    stop(sprintf("Simulated condition of the specified dgm '%1$s' cannot be locatated at the specified location '%2$s'.", condition_id, data_path))

  # load the file
  condition_file <- utils::read.csv(file = file.path(data_path, paste0(condition_id, ".csv")), header = TRUE)

  # return the complete file if repetition_id is not specified
  if (is.null(repetition_id) || isFALSE(repetition_id) || is.na(repetition_id))
    return(condition_file)

  # check that the specified repetition_id exists otherwise
  if (!any(repetition_id == unique(condition_file[["repetition_id"]])))
    stop(sprintf("The specified 'repetition_id' (%1$s) does not exist in the simulated dataset", as.character(repetition_id)))

  return(condition_file[condition_file[["repetition_id"]] == repetition_id,,drop=FALSE])
}
