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
#' @param overwrite Logical indicating whether to overwrite existing files on OSF.
#' Defaults to \code{TRUE} for performance measures and \code{FALSE} for results and datasets
#' @param progress Logical indicating whether to show progress uploading files. Defaults to \code{TRUE}.
#' @param max_try Integet specifying how many times should the function attempt reconnecting to OSF upon failure.
#'
#' @return \code{TRUE} if the upload was successful, otherwise an error is raised.
#'
#' @keywords internal
#' @aliases upload_dgm_datasets upload_dgm_results upload_dgm_measures
#' @name upload_dgm
NULL

#' @rdname upload_dgm
#' @keywords internal
upload_dgm_datasets <- function(dgm_name, overwrite = FALSE, progress = TRUE, max_try = 10) {
  .upload_dgm_fun(dgm_name, what = "data", overwrite = overwrite, progress = progress, max_try = max_try)
}

#' @rdname upload_dgm
#' @keywords internal
upload_dgm_results <- function(dgm_name, overwrite = FALSE, progress = TRUE, max_try = 10) {
  .upload_dgm_fun(dgm_name, what = "results", overwrite = overwrite, progress = progress, max_try = max_try)
}

#' @rdname upload_dgm
#' @keywords internal
upload_dgm_measures <- function(dgm_name, overwrite = TRUE, progress = TRUE, max_try = 10) {
  .upload_dgm_fun(dgm_name, what = "measures", overwrite = overwrite, progress = progress, max_try = max_try)
}


.upload_dgm_fun <- function(dgm_name, what, overwrite, progress, max_try) {

  path <- PublicationBiasBenchmark.get_option("resources_directory")
  if (is.null(path))
    stop("The resources location needs to be specified via the `PublicationBiasBenchmark.get_option('resources_directory')` function.", call. = FALSE)
  
  # get link to the repository
  osf_link <- .get_osf_link(dgm_name)

  # connect to the repository
  osf_repo <- osfr::osf_retrieve_node(osf_link)

  # check that the remote data folder exists, create otherwise
  osf_dir <- osfr::osf_ls_files(osf_repo, type = "folder")
  if (sum(osf_dir$name == what) == 0) {
    osfr::osf_mkdir(osf_repo, path = what)
    osf_dir <- osfr::osf_ls_files(osf_repo, type = "folder")
  }
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
    osfr::osf_mkdir(osf_repo, path = what)
    osf_dir <- osfr::osf_ls_files(osf_repo, type = "folder")
    osf_dir <- osfr::osf_retrieve_file(osf_dir$id[osf_dir$name == what])
  }

  # add error catching and restart on failure
  done      <- FALSE
  iteration <- 0
  while (!done && iteration < max_try) {

    # skip files already present
    osf_repo    <- osfr::osf_retrieve_node(osf_link)
    files_done  <- osfr::osf_ls_files(osf_repo, path = what, n_max = Inf)
    local_files <- local_files[!basename(local_files) %in% files_done$name]

    if (length(local_files) == 0)
      break

    done      <- try(osfr::osf_upload(osf_dir, path = local_files, progress = progress))
    done      <- !inherits(done, "try-error")
    iteration <- iteration + 1
  }

  return(invisible(TRUE))
}
