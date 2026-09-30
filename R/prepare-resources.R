#' Prepare Local Computation Outputs for Publication
#' @description Keep distributed result shards separate and partition ordinary
#' performance measures by method and setting. Original local files are retained.
#' Pairwise comparison files are excluded; they are not ordinary method measures.
#' @param dgm_name DGM name.
#' @param kinds Local kinds to prepare: results, measures, data, or metadata.
#' @param output_directory Directory for prepared immutable measure files.
#' @param package_version Producing package version, or NULL when unknown. Supply
#' this per worker batch; it is not inferred from the current package version.
#' @param method Optional method names to include.
#' @param method_setting Optional method settings to include.
#' @return List of file descriptors for plan_benchmark_release.
#' @export
prepare_benchmark_resources <- function(dgm_name, kinds = c("results", "measures"),
                                        output_directory, package_version = NULL,
                                        method = NULL, method_setting = NULL) {
  if (!all(kinds %in% c("data", "results", "measures", "metadata"))) stop("Unsupported local resource kind.", call. = FALSE)
  root <- file.path(.get_path(), dgm_name)
  descriptors <- list()
  add <- function(asset, relative) {
    asset$id <- paste(dgm_name, asset$kind, relative, sep = "/")
    asset$filename <- paste(dgm_name, asset$kind, gsub("[/\\\\]", "__", relative), sep = "__")
    descriptors[[length(descriptors) + 1L]] <<- asset
  }
  for (kind in setdiff(kinds, "measures")) {
    directory <- file.path(root, kind)
    files <- list.files(directory, recursive = TRUE, full.names = TRUE,
                        pattern = if (kind == "metadata") NULL else "\\.csv$")
    for (path in files) {
      relative <- substring(path, nchar(directory) + 2L)
      first <- if (kind == "metadata") NULL else utils::read.csv(path, nrows = 1L, stringsAsFactors = FALSE)
      if (kind == "results" && ((!is.null(method) && !first$method %in% method) ||
                                (!is.null(method_setting) && !first$method_setting %in% method_setting))) next
      asset <- benchmark_resource(path, dgm_name, kind,
        method = if (kind == "results") first$method else NULL,
        method_setting = if (kind == "results") first$method_setting else NULL,
        condition_ids = if (kind == "data") as.integer(sub("\\.csv$", "", basename(path))) else NULL,
        package_version = package_version)
      add(asset, relative)
    }
  }
  if ("measures" %in% kinds) {
    files <- list.files(file.path(root, "measures"), pattern = "\\.csv$", full.names = TRUE)
    files <- files[!grepl("pairwise", basename(files))]
    dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)
    for (replacement in c(FALSE, TRUE)) {
      wide <- NULL; metrics <- character(); coverage <- list()
      for (path in files[grepl("replacement", basename(files)) == replacement]) {
        metric <- sub("(-replacement)?\\.csv$", "", basename(path))
        data <- .read_resource_csv(path)
        keys <- c("method", "method_setting", "condition_id")
        .reject_duplicate_keys(data, keys, "local measure")
        if (!is.null(method)) data <- data[data$method %in% method, , drop = FALSE]
        if (!is.null(method_setting)) data <- data[data$method_setting %in% method_setting, , drop = FALSE]
        if (!nrow(data)) next
        coverage[[metric]] <- split(data$condition_id, paste(data$method, data$method_setting, sep = "/"))
        auxiliary <- intersect(c("n_valid", "replaced"), names(data))
        names(data)[match(auxiliary, names(data))] <- paste0(auxiliary, "_", metric)
        wide <- if (is.null(wide)) data else merge(wide, data, by = keys, all = TRUE, sort = FALSE)
        metrics <- c(metrics, metric)
      }
      if (is.null(wide)) next
      for (group in split(wide, interaction(wide$method, wide$method_setting, drop = TRUE))) {
        temporary <- tempfile("measures-", tmpdir = output_directory, fileext = ".csv")
        utils::write.csv(group, temporary, row.names = FALSE)
        hash <- digest::digest(file = temporary, algo = "sha256", serialize = FALSE)
        label <- paste0(group$method[1], "-", group$method_setting[1], if (replacement) "-replacement")
        filename <- paste0(label, "-", hash, ".csv")
        if (!.safe_filename(filename)) { unlink(temporary); stop("Method identifiers cannot form safe filenames.", call. = FALSE) }
        path <- file.path(output_directory, filename)
        if (file.exists(path)) unlink(temporary) else if (!file.rename(temporary, path)) stop("Cannot save prepared measures.", call. = FALSE)
        asset <- benchmark_resource(path, dgm_name, "measures", group$method[1], group$method_setting[1],
          package_version = package_version, replacement = replacement, measures = metrics)
        key <- paste(group$method[1], group$method_setting[1], sep = "/")
        measure_conditions <- lapply(coverage, function(x) as.list(x[[key]]))
        asset$measures <- as.list(names(Filter(length, measure_conditions)))
        # Full coverage is the default; store only each metric's exceptions.
        asset$measure_conditions <- Filter(function(ids) !setequal(unlist(ids), unlist(asset$condition_ids)), measure_conditions)
        if (!length(asset$measure_conditions)) asset$measure_conditions <- NULL
        add(asset, paste0(label, ".csv"))
      }
    }
  }
  if (!length(descriptors)) stop("No local resources matched the selection.", call. = FALSE)
  descriptors
}
