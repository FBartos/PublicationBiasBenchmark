#' @title Inspect Benchmark Releases and Resources
#' @description Benchmark releases are complete catalogs of immutable files.
#' Public downloads do not require a Zenodo token. A package release pins its
#' default catalog; another release or a local catalog can be selected explicitly.
#' @param release Benchmark release identifier, path to a catalog JSON file, or
#' a catalog list. NULL uses the benchmark_release package option.
#' @param dgm_name Optional DGM name.
#' @param kind Optional resource kind: data, results, measures, metadata, or archive.
#' @param method Optional method name(s).
#' @param method_setting Optional method setting(s).
#' @return list_benchmark_releases returns a data frame. benchmark_catalog returns
#' a validated list; list_benchmark_resources returns a data frame of file references.
#' @name benchmark_catalog
NULL

.release_registry <- function() {
  path <- system.file("extdata", "benchmark-releases.json", package = "PublicationBiasBenchmark")
  if (!nzchar(path)) stop("The benchmark release registry is missing.", call. = FALSE)
  jsonlite::read_json(path, simplifyVector = FALSE)
}

#' @rdname benchmark_catalog
#' @export
list_benchmark_releases <- function() {
  registry <- .release_registry()
  if (!length(registry$releases)) return(data.frame(release = character(), record_id = character(),
                                                  catalog_sha256 = character()))
  do.call(rbind, lapply(registry$releases, function(x) {
    data.frame(release = x$release, record_id = as.character(x$record_id),
               catalog_sha256 = x$catalog_sha256, stringsAsFactors = FALSE)
  }))
}

.scalar_string <- function(x) is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
.safe_filename <- function(x) .scalar_string(x) && !grepl('[<>:"/\\\\|?*[:cntrl:]]', x) &&
  !grepl("[. ]$", x) && !grepl("^(CON|PRN|AUX|NUL|COM[1-9]|LPT[1-9])([.]|$)", x, ignore.case = TRUE)

.validate_catalog <- function(catalog) {
  if (!identical(as.integer(catalog$schema_version), 1L) || !.scalar_string(catalog$release))
    stop("Unsupported or invalid benchmark catalog.", call. = FALSE)
  if (!is.list(catalog$assets) || !length(catalog$assets) || !is.list(catalog$conditions))
    stop("A catalog must contain assets and frozen DGM conditions.", call. = FALSE)
  for (asset in catalog$assets) {
    required <- c("id", "dgm", "kind", "filename", "sha256", "md5", "record_id")
    if (!all(vapply(asset[required], .scalar_string, logical(1))) ||
        !.safe_filename(asset$filename) || !grepl("^[a-f0-9]{64}$", asset$sha256) ||
        !grepl("^[a-f0-9]{32}$", asset$md5) || !grepl("^[0-9]+$", asset$record_id) ||
        !asset$kind %in% c("data", "results", "measures", "metadata", "archive") ||
        !is.numeric(asset$size) || length(asset$size) != 1L || is.na(asset$size) || asset$size < 0)
      stop("Invalid benchmark asset reference.", call. = FALSE)
    if (asset$kind %in% c("results", "measures") &&
        (!.scalar_string(asset$method) || !.scalar_string(asset$method_setting)))
      stop("Results and measures must be partitioned by method.", call. = FALSE)
    if (!is.null(asset$measure_conditions) &&
        length(setdiff(unlist(asset$measure_conditions), unlist(asset$condition_ids))))
      stop("Measure coverage includes undeclared conditions.", call. = FALSE)
  }
  ids <- vapply(catalog$assets, `[[`, character(1), "id")
  if (anyDuplicated(ids)) stop("Duplicate asset IDs in the catalog.", call. = FALSE)
  for (dgm in unique(vapply(catalog$assets, `[[`, character(1), "dgm"))) {
    conditions <- .catalog_conditions(catalog, dgm)
    if (!"condition_id" %in% names(conditions) || anyNA(conditions$condition_id) ||
        anyDuplicated(conditions$condition_id))
      stop("Invalid frozen condition IDs for DGM '", dgm, "'.", call. = FALSE)
    for (asset in Filter(function(x) x$dgm == dgm, catalog$assets)) {
      if (length(setdiff(unlist(asset$condition_ids), conditions$condition_id)))
        stop("Asset references an unknown frozen condition.", call. = FALSE)
      if (asset$kind == "data" && !length(asset$condition_ids))
        stop("Dataset files must declare their conditions.", call. = FALSE)
    }
  }
  catalog
}

#' @rdname benchmark_catalog
#' @export
benchmark_catalog <- function(release = NULL) {
  if (is.null(release)) release <- PublicationBiasBenchmark.get_option("benchmark_release")
  if (is.list(release)) return(.validate_catalog(release))
  if (.scalar_string(release) && file.exists(release))
    return(.validate_catalog(jsonlite::read_json(release, simplifyVector = FALSE)))
  registry <- .release_registry()
  if (is.null(release)) release <- registry$default_release
  if (!.scalar_string(release)) stop("No published benchmark release has been configured.", call. = FALSE)
  entry <- Filter(function(x) identical(x$release, release), registry$releases)
  if (length(entry) != 1L) stop("Unknown benchmark release '", release, "'. Use a catalog file for an unlisted release.", call. = FALSE)
  entry <- entry[[1]]
  cached <- file.path(.get_path(), "releases", release, "release.json")
  .fetch_verified(.zenodo_file_url(entry$record_id, "release.json"), cached,
                  sha256 = entry$catalog_sha256, progress = FALSE)
  catalog <- .validate_catalog(jsonlite::read_json(cached, simplifyVector = FALSE))
  if (!identical(catalog$release, release)) stop("Catalog release ID does not match its registry entry.", call. = FALSE)
  catalog
}

.catalog_conditions <- function(catalog, dgm_name) {
  rows <- catalog$conditions[[dgm_name]]
  if (is.data.frame(rows)) return(rows)
  if (!length(rows)) stop("No frozen conditions for DGM '", dgm_name, "'.", call. = FALSE)
  do.call(rbind, lapply(rows, function(x) {
    x <- lapply(x, function(value) {
      if (is.null(value)) NA else if (is.list(value) || length(value) > 1L) I(list(unlist(value))) else value
    })
    as.data.frame(x, stringsAsFactors = FALSE)
  }))
}

.select_assets <- function(catalog, dgm_name = NULL, kind = NULL, method = NULL,
                           method_setting = NULL, replacement = NULL, measure = NULL) {
  selected <- Filter(function(x) {
    (is.null(dgm_name) || x$dgm %in% dgm_name) && (is.null(kind) || x$kind %in% kind) &&
      (is.null(method) || x$method %in% method) &&
      (is.null(method_setting) || x$method_setting %in% method_setting) &&
      (is.null(replacement) || identical(isTRUE(x$replacement), isTRUE(replacement))) &&
      (is.null(measure) || any(c(x$measure, unlist(x$measures)) %in% measure))
  }, catalog$assets)
  if (!length(selected)) stop("No resources match the requested DGM, kind, method, settings, or measure in release '",
                              catalog$release, "'.", call. = FALSE)
  if (!is.null(method)) {
    missing_methods <- setdiff(method, vapply(selected, function(x) x$method, character(1)))
    if (length(missing_methods)) stop("Unavailable methods: ", paste(missing_methods, collapse = ", "), call. = FALSE)
  }
  if (!is.null(method_setting)) {
    missing_settings <- setdiff(method_setting, vapply(selected, function(x) x$method_setting, character(1)))
    if (length(missing_settings)) stop("Unavailable settings: ", paste(missing_settings, collapse = ", "), call. = FALSE)
  }
  selected
}

#' @rdname benchmark_catalog
#' @export
list_benchmark_resources <- function(release = NULL, dgm_name = NULL, kind = NULL,
                                     method = NULL, method_setting = NULL) {
  catalog <- benchmark_catalog(release)
  assets <- .select_assets(catalog, dgm_name, kind, method, method_setting)
  do.call(rbind, lapply(assets, function(x) {
    data.frame(id = x$id, dgm = x$dgm, kind = x$kind,
               method = if (is.null(x$method)) "" else x$method,
               method_setting = if (is.null(x$method_setting)) "" else x$method_setting,
               filename = x$filename, size = x$size, sha256 = x$sha256,
               record_id = x$record_id,
               url = .zenodo_file_url(x$record_id, x$filename, isTRUE(catalog$sandbox)),
               package_version = if (is.null(x$package_version)) NA_character_ else x$package_version,
               stringsAsFactors = FALSE)
  }))
}

.zenodo_file_url <- function(record_id, filename, sandbox = FALSE) {
  if (!grepl("^[0-9]+$", as.character(record_id)) || !.safe_filename(filename))
    stop("Invalid Zenodo file reference.", call. = FALSE)
  sprintf("https://%s/api/records/%s/files/%s/content",
          if (sandbox) "sandbox.zenodo.org" else "zenodo.org", record_id,
          utils::URLencode(filename, reserved = TRUE))
}

.asset_cache_path <- function(asset) file.path(.get_path(), "cache", asset$sha256, asset$filename)

.file_verified <- function(path, sha256, size = NULL, md5 = NULL) {
  if (!file.exists(path) || dir.exists(path)) return(FALSE)
  if (!is.null(size) && !identical(as.numeric(file.info(path)$size), as.numeric(size))) return(FALSE)
  if (!identical(digest::digest(file = path, algo = "sha256", serialize = FALSE), sha256)) return(FALSE)
  is.null(md5) || identical(unname(tools::md5sum(path)), md5)
}

.resource_download <- function(url, destination, progress) {
  curl::curl_download(url, destination, quiet = !progress, mode = "wb")
}

.fetch_verified <- function(url, destination, sha256, size = NULL, md5 = NULL,
                            progress = TRUE, max_try = 10, overwrite = FALSE) {
  if (!is.numeric(max_try) || length(max_try) != 1L || is.na(max_try) || max_try < 1 || max_try %% 1 != 0)
    stop("max_try must be a positive integer.", call. = FALSE)
  if (!overwrite && .file_verified(destination, sha256, size, md5)) return(invisible(TRUE))
  dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
  temporary <- tempfile("transfer-", tmpdir = dirname(destination))
  on.exit(unlink(temporary), add = TRUE)
  for (attempt in seq_len(max_try)) {
    downloaded <- try(.resource_download(url, temporary, progress), silent = TRUE)
    if (!inherits(downloaded, "try-error") && .file_verified(temporary, sha256, size, md5)) {
      # The old file is retained until a replacement has passed verification.
      backup <- paste0(temporary, ".previous")
      if (file.exists(destination) && !file.rename(destination, backup)) stop("Cannot replace cached file.", call. = FALSE)
      if (!file.rename(temporary, destination)) {
        if (file.exists(backup)) file.rename(backup, destination)
        stop("Cannot install verified cached file.", call. = FALSE)
      }
      if (file.exists(backup)) unlink(backup)
      return(invisible(TRUE))
    }
    if (attempt < max_try) Sys.sleep(min(30, 2^(attempt - 1L)))
  }
  stop("Could not download and verify '", basename(destination), "' after ", max_try, " attempts.", call. = FALSE)
}

.cached_asset_files <- function(assets) {
  paths <- vapply(assets, .asset_cache_path, character(1))
  valid <- vapply(seq_along(assets), function(i) {
    x <- assets[[i]]
    .file_verified(paths[i], x$sha256, x$size, x$md5)
  }, logical(1))
  if (!all(valid)) stop("Missing or unverified resources: ", paste(basename(paths[!valid]), collapse = ", "),
                         ". Download the requested resources first.", call. = FALSE)
  paths
}

#' @title Verify Cached Benchmark Resources
#' @inheritParams benchmark_catalog
#' @return A data frame giving the verification status of selected resources.
#' @export
verify_benchmark_resources <- function(release = NULL, dgm_name = NULL, kind = NULL,
                                       method = NULL, method_setting = NULL) {
  assets <- .select_assets(benchmark_catalog(release), dgm_name, kind, method, method_setting)
  data.frame(id = vapply(assets, `[[`, character(1), "id"),
             verified = vapply(assets, function(x) .file_verified(.asset_cache_path(x), x$sha256, x$size, x$md5), logical(1)))
}
