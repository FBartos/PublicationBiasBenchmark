#' @title Upload Datasets of a DGM
#'
#' @description
#' This function uploads datasets of a specified Data-Generating Mechanism (DGM)
#' to the OSF repository at \url{https://osf.io/exf3m/}.
#'
#' This is an internal function intended for the benchmark maintainer.
#' It requires OSF repository authentication (via \code{osfr::osf_auth()})
#' and repository access.
#'
#' @param dgm_name Character string specifying the name of the DGM dataset to upload.
#' @param path Character string specifying the directory path where the datasets/results/measures
#' are located. Defaults to the location specified via
#' \code{PublicationBiasBenchmark.get_option("simulation_directory")}. The objects are read
#' from dgm_name/datasets, dgm_name/results, dgm_name/measures subfolders.
#' @param overwrite Logical indicating whether to overwrite existing files on OSF.
#' Defaults to \code{TRUE}, which overwrites files with conflicts.
#' @param progress Logical indicating whether to show progress uploading files.
#' Defaults to \code{TRUE}.
#'
#' @return \code{TRUE} if the upload was successful, otherwise an error is raised.
#'
#' @examples
#' \dontrun{
#'   upload_dgm_datasets("no_bias")
#' }
#'
#' @keywords internal
#' @aliases upload_dgm_datasets upload_dgm_results upload_dgm_measures
#' @name upload_dgm
NULL

#' @rdname upload_dgm
#' @keywords internal
upload_dgm_datasets <- function(dgm_name, path = NULL, overwrite = TRUE, progress = TRUE) {
  .upload_dgm_fun(dgm_name, what = "data", path = path, overwrite = overwrite, progress = progress)
}

#' @rdname upload_dgm
#' @keywords internal
upload_dgm_results <- function(dgm_name, path = NULL, overwrite = TRUE, progress = TRUE) {
  .upload_dgm_fun(dgm_name, what = "results", path = path, overwrite = overwrite, progress = progress)
}

#' @rdname upload_dgm
#' @keywords internal
upload_dgm_measures <- function(dgm_name, path = NULL, overwrite = TRUE, progress = TRUE) {
  .upload_dgm_fun(dgm_name, what = "measures", path = path, overwrite = overwrite, progress = progress)
}


.upload_dgm_fun <- function(dgm_name, what, path, overwrite, progress) {

  if (is.null(path))
    path <- PublicationBiasBenchmark.get_option("simulation_directory")

  # get link to the repository
  osf_link <- "https://osf.io/exf3m/"

  # connect to the repository
  osf_repo <- osfr::osf_retrieve_node(osf_link)

  # check that the remote data folder exists, create otherwise
  osf_dir <-try(osfr::osf_ls_files(osf_repo, path = file.path(dgm_name, what)), silent = TRUE)
  if (inherits(osf_dir, "try-error")) {
    osfr::osf_mkdir(osf_repo, path = file.path(dgm_name, what))
  }

  # select the remote data folder
  osf_dir <- osfr::osf_ls_files(osf_repo, path = dgm_name, type = "folder")
  osf_dir <- osfr::osf_retrieve_file(osf_dir$id[osf_dir$name == what])

  # check the local directory name
  dgm_path <- file.path(path, dgm_name)
  if (!dir.exists(dgm_path)) {
    stop(sprintf("DGM directory '%1$s' does not exist at the specified location '%2$s'.", dgm_name, path))
  }

  # check the local data folder
  data_path <- file.path(path, dgm_name, what)
  if (!dir.exists(data_path)) {
    stop(sprintf("Data folder '%1$s' does not exist for DGM '%2$s' at the specified location '%3$s'.", what, dgm_name, path))
  }

  # get list of files to upload
  local_files <- list.files(data_path, full.names = TRUE)

  if (length(local_files) == 0) {
    warning(sprintf("No files found to upload in '%1$s'.", data_path))
    return(invisible(TRUE))
  }

  # Calculate the total size
  if (PublicationBiasBenchmark.get_option("prompt_for_download")) {
    file_sizes <- file.info(local_files)$size
    size_MB <- sum(file_sizes) / 1024^2
    rl      <- readline(sprintf("You are about to upload %1$i files (%2$.2f %3$s) from '%4$s' to OSF. Do you want to proceed? [Y, n]",
                                length(local_files),
                                ifelse(size_MB > 1024, size_MB / 1024, size_MB),
                                ifelse(size_MB > 1024, "GB" , "MB"),
                                data_path
                                ))
    message("(Set `PublicationBiasBenchmark.options('prompt_for_download' = FALSE)` to skip this message in the future)")
    rl <- tolower(as.character(rl))
    if (!((rl == "" || substr(rl, 1, 1) == "y")))
      return(invisible(FALSE))
  }

  # upload the files
  # the package cannot overwrite files in subfolders
  # https://github.com/ropensci/osfr/issues/138
  # therefore we need to manually delete them first
  if (overwrite) {
    osfr::osf_rm(osf_dir, verbose = FALSE, check = FALSE)
    osfr::osf_mkdir(osf_repo, path = file.path(dgm_name, what))
    osf_dir <- osfr::osf_ls_files(osf_repo, path = dgm_name, type = "folder")
    osf_dir <- osfr::osf_retrieve_file(osf_dir$id[osf_dir$name == what])
  }
  osfr::osf_upload(osf_dir, path = local_files, progress = progress)

  return(invisible(TRUE))
}
