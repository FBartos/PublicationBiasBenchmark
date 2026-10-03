.retrieve_local_dgm_dataset <- function(dgm_name, condition_id, repetition_id = NULL){

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
  if (length(setdiff(repetition_id, unique(condition_file[["repetition_id"]]))))
    stop(sprintf("The specified 'repetition_id' (%1$s) does not exist in the simulated dataset", as.character(repetition_id)))

  return(condition_file[condition_file[["repetition_id"]] %in% repetition_id,,drop=FALSE])
}


.retrieve_local_dgm_results <- function(dgm_name, method = NULL, method_setting = NULL, condition_id = NULL, repetition_id = NULL){

  if (missing(dgm_name))
    stop("'dgm_name' must be specified")

  path <- .get_path()

  # check that the directory / condition folders exist
  results_path <- file.path(path, dgm_name, "results")
  if (!dir.exists(results_path))
    stop(sprintf("Computed results of the specified dgm '%1$s' cannot be locatated at the specified location '%2$s'. You might need to dowload the computed results using the 'download_dgm_results()' function first.", dgm_name, path))

  files <- list.files(results_path, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
  # Inspect identifiers first so other methods' large local files stay unread.
  selected <- Filter(function(file) {
    first <- utils::read.csv(file, nrows = 1L, stringsAsFactors = FALSE)
    (is.null(method) || first$method %in% method) &&
      (is.null(method_setting) || first$method_setting %in% method_setting)
  }, files)
  if (!length(selected)) stop("No local results match the requested methods and settings.", call. = FALSE)
  results_file <- safe_rbind(lapply(selected, .read_resource_csv))
  .reject_duplicate_keys(results_file, c("method", "method_setting", "condition_id", "repetition_id"), "local result")

  # subset by method, settings, condition, repetition if specified
  if (!is.null(method)) {
    results_file <- results_file[results_file$method %in% method, ]
  }
  if (!is.null(method_setting)) {
    results_file <- results_file[results_file$method_setting %in% method_setting, ]
  }
  if (!is.null(condition_id)) {
    results_file <- results_file[results_file$condition_id %in% condition_id, ]
  }
  if (!is.null(repetition_id)) {
    results_file <- results_file[results_file$repetition_id %in% repetition_id, ]
  }

  return(results_file)
}


.retrieve_local_dgm_measures <- function(dgm_name, measure = NULL, method = NULL, method_setting = NULL, condition_id = NULL, replacement = FALSE){

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
    measures_file <- measures_file[measures_file$condition_id %in% condition_id, ]
  }

  return(measures_file)
}
