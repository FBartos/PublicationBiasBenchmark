#' @title Download Benchmark Datasets, Results, and Measures
#' @description Download immutable catalog-selected direct files or ZIP units.
#' Archives and extracted members are checked against size, SHA-256 and MD5.
#' A condition/metric filter may fetch other members of its selected unit.
#' Public downloads require no token; verified member caches avoid new downloads.
#' @param dgm_name DGM name.
#' @param overwrite Re-download selected files even if their cached copies verify.
#' @param progress Display download progress.
#' @param max_try Maximum attempts per file.
#' @param method Optional method name(s); NULL selects all available methods.
#' @param method_setting Optional method setting(s).
#' @param release Release identifier or catalog; NULL uses the package default.
#' @param condition_id Optional condition ID(s) for datasets.
#' @param measure Optional measure name(s); pairwise comparisons require "pairwise".
#' @param replacement Whether replacement measures are selected; NULL downloads both variants.
#' @return Invisible TRUE on success or FALSE when an interactive download is declined.
#' @name download_dgm
NULL

#' @rdname download_dgm
#' @export
download_dgm_datasets <- function(dgm_name, overwrite = FALSE, progress = TRUE, max_try = 10,
                                  release = NULL, condition_id = NULL) {
  .download_dgm_fun(dgm_name, "data", overwrite, progress, max_try,
                    release = release, condition_id = condition_id)
}

#' @rdname download_dgm
#' @export
download_dgm_results <- function(dgm_name, overwrite = FALSE, progress = TRUE, max_try = 10,
                                 method = NULL, method_setting = NULL, release = NULL) {
  .download_dgm_fun(dgm_name, "results", overwrite, progress, max_try, method, method_setting, release)
}

#' @rdname download_dgm
#' @export
download_dgm_measures <- function(dgm_name, overwrite = FALSE, progress = TRUE, max_try = 10,
                                  method = NULL, method_setting = NULL, release = NULL,
                                  measure = NULL, replacement = NULL) {
  .download_dgm_fun(dgm_name, "measures", overwrite, progress, max_try, method, method_setting,
                    release, measure = measure, replacement = replacement)
}

#' @rdname download_dgm
#' @export
download_dgm_metadata <- function(dgm_name, overwrite = FALSE, progress = TRUE, max_try = 10,
                                  release = NULL) {
  .download_dgm_fun(dgm_name, "metadata", overwrite, progress, max_try, release = release)
}

.download_dgm_fun <- function(dgm_name, what, overwrite, progress, max_try,
                             method = NULL, method_setting = NULL, release = NULL,
                             condition_id = NULL, measure = NULL, replacement = NULL) {
  catalog <- benchmark_catalog(release)
  kind <- if (what == "measures" && identical(measure, "pairwise")) c("measures", "pairwise") else what
  assets <- .select_assets(catalog, dgm_name, kind, method, method_setting, replacement, measure)
  if (what == "measures" && !identical(measure, "pairwise"))
    assets <- Filter(function(x) !identical(x$measure, "pairwise"), assets)
  if (!is.null(condition_id)) {
    .check_release_conditions(catalog, dgm_name, condition_id)
    assets <- Filter(function(x) any(unlist(x$condition_ids) %in% condition_id), assets)
  }
  if (!length(assets)) stop("No files match the requested selection.", call. = FALSE)
  pending <- .pending_downloads(catalog, assets, overwrite)
  if (!length(pending$assets)) {
    if (progress) message("All selected files are cached and verified.")
    return(invisible(TRUE))
  }
  if (pending$files > 0L && interactive() && PublicationBiasBenchmark.get_option("prompt_for_download")) {
    answer <- readline(sprintf("Download %d files (%.2f MB) for %s from release %s? [Y/n] ",
                                pending$files, pending$bytes/1024^2,
                                dgm_name, catalog$release))
    if (nzchar(answer) && !tolower(substr(answer, 1, 1)) %in% "y") return(invisible(FALSE))
  }
  .download_catalog_assets(catalog, assets, progress, max_try, overwrite, pending = pending)
  invisible(TRUE)
}

.check_release_conditions <- function(catalog, dgm_name, condition_id) {
  missing <- setdiff(condition_id, .catalog_conditions(catalog, dgm_name)$condition_id)
  if (length(missing)) stop("Unknown archived condition IDs: ", paste(missing, collapse = ", "), call. = FALSE)
}

.read_resource_csv <- function(path) utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)

.reject_duplicate_keys <- function(data, keys, what) {
  if (!all(keys %in% names(data))) stop("Missing ", what, " identifier columns.", call. = FALSE)
  if (anyNA(data[keys]) || anyDuplicated(data[keys]))
    stop("Missing or overlapping ", what, " keys across the selected shards.", call. = FALSE)
  data
}

.read_catalog_asset <- function(asset, path) {
  data <- .read_resource_csv(path)
  if (!is.null(asset$rows) && nrow(data) != asset$rows) stop("Archived row count differs from the catalog.", call. = FALSE)
  if (asset$kind %in% c("results", "measures") &&
      (!all(c("method", "method_setting", "condition_id") %in% names(data)) ||
       anyNA(data[c("method", "method_setting", "condition_id")]) ||
       !all(data$method == asset$method) || !all(data$method_setting == asset$method_setting)))
    stop("Archived contents do not match the declared method and setting.", call. = FALSE)
  if ("condition_id" %in% names(data) && length(setdiff(data$condition_id, unlist(asset$condition_ids))))
    stop("Archived contents include undeclared conditions.", call. = FALSE)
  data
}

.read_catalog_assets <- function(assets) {
  paths <- .cached_asset_files(assets)
  safe_rbind(lapply(seq_along(assets), function(i) .read_catalog_asset(assets[[i]], paths[i])))
}

.check_requested_coverage <- function(data, conditions = NULL, repetitions = NULL) {
  # Single pass: rows are split once per method/setting and, when repetitions
  # are requested, once per condition within it.
  for (rows in split(seq_len(nrow(data)), .method_key(data$method, data$method_setting))) {
    available <- data$condition_id[rows]
    selected <- if (is.null(conditions)) unique(available) else conditions
    if (length(setdiff(selected, available))) stop("Requested conditions are unavailable for a selected method/setting.", call. = FALSE)
    if (!is.null(repetitions)) {
      present <- split(data$repetition_id[rows], available)
      for (condition in selected)
        if (length(setdiff(repetitions, present[[as.character(condition)]])))
          stop("Requested repetitions are unavailable for a selected method/setting/condition.", call. = FALSE)
    }
  }
}

#' @title Retrieve Archived DGM Datasets
#' @description Read locally cached dataset shards using frozen condition definitions.
#' @inheritParams download_dgm
#' @param repetition_id Repetition ID(s); NULL selects all repetitions.
#' @param source "release" reads verified catalog-selected files; "local" reads
#' unpublished computation files in resources_directory/DGM.
#' @return A data frame.
#' @export
retrieve_dgm_dataset <- function(dgm_name, condition_id, repetition_id = NULL,
                                 release = NULL, source = c("release", "local")) {
  source <- match.arg(source)
  if (source == "local") return(.retrieve_local_dgm_dataset(dgm_name, condition_id, repetition_id))
  catalog <- benchmark_catalog(release)
  .check_release_conditions(catalog, dgm_name, condition_id)
  assets <- Filter(function(x) any(unlist(x$condition_ids) %in% condition_id),
                   .select_assets(catalog, dgm_name, "data"))
  if (!length(assets)) stop("No archived data for the selected conditions.", call. = FALSE)
  if (length(condition_id) != 1L) stop("Select one condition at a time when retrieving datasets.", call. = FALSE)
  .validate_plan_coverage(assets)
  data <- .read_catalog_assets(assets)
  if ("condition_id" %in% names(data)) data <- data[data$condition_id %in% condition_id, , drop = FALSE]
  keys <- intersect(c("condition_id", "repetition_id", "study_id"), names(data))
  if ("study_id" %in% keys) .reject_duplicate_keys(data, keys, "dataset")
  if (!is.null(repetition_id)) {
    if (length(setdiff(repetition_id, data$repetition_id))) stop("Requested repetitions are unavailable.", call. = FALSE)
    data <- data[data$repetition_id %in% repetition_id, , drop = FALSE]
  }
  data
}

#' @title Retrieve Archived Method Results
#' @inheritParams retrieve_dgm_dataset
#' @inheritParams download_dgm
#' @description Read and combine verified shards. Missing shards and overlapping
#' method/setting/condition/repetition keys are errors. Use source = "local" for
#' unpublished distributed computation outputs.
#' @return A data frame with the existing method-specific result columns.
#' @export
retrieve_dgm_results <- function(dgm_name, method = NULL, method_setting = NULL,
                                 condition_id = NULL, repetition_id = NULL, release = NULL,
                                 source = c("release", "local")) {
  source <- match.arg(source)
  if (source == "local") return(.retrieve_local_dgm_results(dgm_name, method, method_setting, condition_id, repetition_id))
  catalog <- benchmark_catalog(release)
  assets <- .select_assets(catalog, dgm_name, "results", method, method_setting)
  # Unknown condition IDs are rejected before any shard is read.
  if (!is.null(condition_id)) .check_release_conditions(catalog, dgm_name, condition_id)
  data <- .read_catalog_assets(assets)
  .reject_duplicate_keys(data, c("method", "method_setting", "condition_id", "repetition_id"), "result")
  .check_requested_coverage(data, condition_id, repetition_id)
  if (!is.null(condition_id)) {
    if (length(setdiff(condition_id, data$condition_id))) stop("Requested result conditions are unavailable.", call. = FALSE)
    data <- data[data$condition_id %in% condition_id, , drop = FALSE]
  }
  if (!is.null(repetition_id)) {
    if (length(setdiff(repetition_id, data$repetition_id))) stop("Requested result repetitions are unavailable.", call. = FALSE)
    data <- data[data$repetition_id %in% repetition_id, , drop = FALSE]
  }
  data
}

#' @title Retrieve Archived Performance Measures
#' @inheritParams retrieve_dgm_results
#' @inheritParams download_dgm
#' @description Read measures stored separately for each DGM, method and setting.
#' Pairwise comparisons are only read when measure = "pairwise" is requested.
#' @return A data frame of selected measures and Monte Carlo standard errors.
#' @export
retrieve_dgm_measures <- function(dgm_name, measure = NULL, method = NULL, method_setting = NULL,
                                  condition_id = NULL, replacement = FALSE, release = NULL,
                                  source = c("release", "local")) {
  source <- match.arg(source)
  if (source == "local") return(.retrieve_local_dgm_measures(dgm_name, measure, method, method_setting, condition_id, replacement))
  catalog <- benchmark_catalog(release)
  kind <- if (identical(measure, "pairwise")) c("measures", "pairwise") else "measures"
  assets <- .select_assets(catalog, dgm_name, kind, method, method_setting, replacement, measure)
  if (!identical(measure, "pairwise")) assets <- Filter(function(x) !identical(x$measure, "pairwise"), assets)
  if (!length(assets)) stop("No ordinary measures match the selection.", call. = FALSE)
  # Unknown condition IDs are rejected before any table is read.
  if (!is.null(condition_id)) .check_release_conditions(catalog, dgm_name, condition_id)
  if (is.null(measure)) {
    data <- .read_catalog_assets(assets)
  } else {
    paths <- .cached_asset_files(assets)
    data <- safe_rbind(lapply(seq_along(assets), function(i) {
      asset <- assets[[i]]; table <- .read_catalog_asset(asset, paths[i])
      if (!is.null(asset$measure_conditions)) {
        selected <- intersect(measure, c(asset$measure, unlist(asset$measures)))
        available <- unlist(lapply(selected, function(metric) {
          covered <- asset$measure_conditions[[metric]]
          if (is.null(covered)) asset$condition_ids else covered
        }))
        table <- table[table$condition_id %in% available, , drop = FALSE]
      }
      table
    }))
  }
  if (!identical(measure, "pairwise")) {
    .reject_duplicate_keys(data, c("method", "method_setting", "condition_id"), "measure")
    .check_requested_coverage(data, condition_id)
    if (!is.null(measure)) {
      required <- c("method", "method_setting", "condition_id", measure, paste0(measure, "_mcse"),
                     paste0("n_valid_", measure), paste0("replaced_", measure))
      data <- data[intersect(required, names(data))]
      if (length(measure) == 1L) {
        names(data)[names(data) == paste0("n_valid_", measure)] <- "n_valid"
        names(data)[names(data) == paste0("replaced_", measure)] <- "replaced"
      }
    }
  }
  if (!is.null(condition_id)) data <- data[data$condition_id %in% condition_id, , drop = FALSE]
  data
}
