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
#'   download_dgm_dataset("no_bias")
#' }
#'
#'
#' @export
download_dgm_dataset <- function(dgm_name, path = "", add = FALSE) {
  .download_dgm_fun(dgm_name, what = "data", path = path, add = add)
}
download_dgm_results <- function(dgm_name, path = "", add = FALSE) {
  .download_dgm_fun(dgm_name, what = "results", path = path, add = add)
}

.download_dgm_fun <- function(dgm_name, what, path = "", add = FALSE) {

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
    dir.create(dgm_path)
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


#' @export
dgm_repository <- function(dgm_name) {

  # Convert character to appropriate class for dispatch
  if (is.character(dgm_name)) {
    dgm_type <- structure(dgm_name, class = dgm_name)
  } else {
    dgm_type <- dgm_name
  }

  UseMethod("dgm_repository", dgm_type)
}
