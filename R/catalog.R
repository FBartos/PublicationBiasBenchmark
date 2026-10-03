#' @title Inspect Benchmark Releases and Resources
#' @description Benchmark releases are complete catalogs of immutable files.
#' Public downloads do not require a Zenodo token. A package release pins its
#' default catalog; another release or a local catalog can be selected explicitly.
#' Parsed catalogs are reused by content hash; cached bytes are verified before use.
#' @param release Benchmark release identifier, path to a catalog JSON file, or
#' a catalog list. NULL uses the benchmark_release package option.
#' @param dgm_name DGM name (optional when listing resources).
#' @param kind Optional resource kind: data, results, measures, pairwise, metadata, or archive.
#' @param method Optional method name(s).
#' @param method_setting Optional method setting(s).
#' @return list_benchmark_releases returns a data frame. benchmark_catalog returns
#' a validated list; list_benchmark_resources returns a data frame of file references.
#' benchmark_conditions returns the release's frozen condition data frame.
#' @name benchmark_catalog
NULL

.catalog_cache <- new.env(parent = emptyenv())
.catalog_cache$keys <- character()
.catalog_cache$values <- list()

.read_validated_catalog <- function(path, expected_sha256 = NULL) {
  bytes <- readBin(path, "raw", n = file.info(path)$size)
  sha256 <- digest::digest(bytes, algo = "sha256", serialize = FALSE)
  if (!is.null(expected_sha256) && !identical(sha256, expected_sha256))
    stop(structure(list(message = "Catalog checksum changed before parsing.", call = NULL),
                   class = c("catalog_checksum_error", "error", "condition")))
  catalog <- .catalog_cache$values[[sha256]]
  if (is.null(catalog)) {
    json <- rawToChar(bytes)
    Encoding(json) <- "UTF-8"
    catalog <- .validate_catalog(jsonlite::fromJSON(json, simplifyVector = FALSE))
    .catalog_cache$values[[sha256]] <- catalog
  }
  # Keep the two most recently used snapshots for current/previous comparisons.
  # The cache contains only objects validated from these exact hashed bytes.
  keys <- unique(c(sha256, .catalog_cache$keys))
  .catalog_cache$keys <- keys[seq_len(min(length(keys), 2L))]
  .catalog_cache$values <- .catalog_cache$values[.catalog_cache$keys]
  catalog
}

.release_registry <- function() {
  path <- system.file("extdata", "benchmark-releases.json", package = "PublicationBiasBenchmark")
  if (!nzchar(path)) stop("The benchmark release registry is missing.", call. = FALSE)
  jsonlite::read_json(path, simplifyVector = FALSE)
}

#' @rdname benchmark_catalog
#' @export
list_benchmark_releases <- function() {
  registry <- .release_registry()
  if (!length(registry$releases)) return(data.frame(release = character(), record_id = character(), doi = character(),
                                                  catalog_sha256 = character()))
  do.call(rbind, lapply(registry$releases, function(x) {
    data.frame(release = x$release, record_id = as.character(x$record_id), doi = if (is.null(x$doi)) NA_character_ else x$doi,
               catalog_sha256 = x$catalog_sha256, stringsAsFactors = FALSE)
  }))
}

.scalar_string <- function(x) is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
.safe_filename <- function(x) .scalar_string(x) && !grepl('[<>:"/\\\\|?*[:cntrl:]]', x) &&
  !grepl("[. ]$", x) && !grepl("^(CON|PRN|AUX|NUL|COM[1-9]|LPT[1-9])([.]|$)", x, ignore.case = TRUE)

.validate_catalog <- function(catalog) {
  if (length(catalog$schema_version) != 1L || !catalog$schema_version %in% c(1L, 2L) || !.scalar_string(catalog$release))
    stop("Unsupported or invalid benchmark catalog.", call. = FALSE)
  if (!is.list(catalog$assets) || !length(catalog$assets) || !is.list(catalog$conditions))
    stop("A catalog must contain assets and frozen DGM conditions.", call. = FALSE)
  for (asset in catalog$assets) {
    required <- c("id", "dgm", "kind", "filename", "sha256", "md5", "record_id")
    if (!all(vapply(asset[required], .scalar_string, logical(1))) ||
        !.safe_filename(asset$filename) || !grepl("^[a-f0-9]{64}$", asset$sha256) ||
        !grepl("^[a-f0-9]{32}$", asset$md5) || !grepl("^[0-9]+$", asset$record_id) ||
        !asset$kind %in% c("data", "results", "measures", "pairwise", "metadata", "archive") ||
        !is.numeric(asset$size) || length(asset$size) != 1L || !is.finite(asset$size) || asset$size < 0)
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
  if (catalog$schema_version == 2L) .validate_archive_catalog(catalog)
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
    return(.read_validated_catalog(release))
  registry <- .release_registry()
  if (is.null(release)) release <- registry$default_release
  if (!.scalar_string(release)) stop("No published benchmark release has been configured.", call. = FALSE)
  entry <- Filter(function(x) identical(x$release, release), registry$releases)
  if (length(entry) != 1L) stop("Unknown benchmark release '", release, "'. Use a catalog file for an unlisted release.", call. = FALSE)
  entry <- entry[[1]]
  cached <- file.path(.get_path(), "releases", release, "release.json")
  # A cached catalog is hashed once while it is read. Only a missing or changed
  # file is downloaded again, and the fresh bytes are verified by the transfer.
  catalog <- NULL
  if (file.exists(cached))
    catalog <- tryCatch(.read_validated_catalog(cached, entry$catalog_sha256),
                        catalog_checksum_error = function(error) NULL)
  if (is.null(catalog)) {
    .fetch_verified(.zenodo_file_url(entry$record_id, "release.json"), cached,
                    sha256 = entry$catalog_sha256, progress = FALSE, overwrite = TRUE)
    catalog <- .read_validated_catalog(cached, entry$catalog_sha256)
  }
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

#' @rdname benchmark_catalog
#' @export
benchmark_conditions <- function(dgm_name, release = NULL) {
  .catalog_conditions(benchmark_catalog(release), dgm_name)
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
  index <- .archive_index(catalog)
  # One archive lookup per asset; columns are built directly instead of binding
  # one data frame per asset. unlist() keeps the integer/double typing of sizes
  # that row-binding produced.
  archives <- lapply(assets, function(x) if (is.null(x$archive_id)) NULL else .catalog_archive(catalog, x$archive_id, index))
  # Optional scalar metadata may be a factor or another type in an in-memory catalog.
  text <- function(field) vapply(assets, function(x) if (is.null(x[[field]])) "" else as.character(x[[field]]), character(1))
  data.frame(id = vapply(assets, `[[`, character(1), "id"), dgm = vapply(assets, `[[`, character(1), "dgm"),
             kind = vapply(assets, `[[`, character(1), "kind"),
             method = text("method"), method_setting = text("method_setting"),
             filename = vapply(assets, `[[`, character(1), "filename"),
             size = unlist(lapply(assets, `[[`, "size")),
             sha256 = vapply(assets, `[[`, character(1), "sha256"),
             record_id = vapply(assets, `[[`, character(1), "record_id"),
             url = vapply(seq_along(assets), function(i) {
               reference <- if (is.null(archives[[i]])) assets[[i]] else archives[[i]]
               .zenodo_file_url(reference$record_id, reference$filename, isTRUE(catalog$sandbox))
             }, character(1)),
             archive_id = vapply(assets, function(x) if (is.null(x$archive_id)) NA_character_ else x$archive_id, character(1)),
             archive_filename = vapply(archives, function(a) if (is.null(a)) NA_character_ else a$filename, character(1)),
             download_size = unlist(lapply(seq_along(assets), function(i)
               if (is.null(archives[[i]])) assets[[i]]$size else archives[[i]]$size)),
             package_version = vapply(assets, function(x) if (is.null(x$package_version)) NA_character_ else as.character(x$package_version), character(1)),
             stringsAsFactors = FALSE)
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
  handle <- curl::new_handle(connecttimeout = 30, low_speed_limit = 1, low_speed_time = 180,
                             noprogress = !progress)
  response <- curl::curl_fetch_disk(url, destination, handle = handle)
  if (response$status_code >= 400L) {
    delay <- if (response$status_code == 429L) .zenodo_retry_delay(curl::parse_headers_list(response$headers), 429L, 1L) else NULL
    stop(structure(list(message = paste0("Public resource download failed (HTTP ", response$status_code, ")."),
      call = NULL, status = response$status_code, retry_delay = delay), class = c("resource_http_error", "error", "condition")))
  }
  invisible(TRUE)
}
.resource_retry_wait <- function(delay) {
  while (delay > 0) { Sys.sleep(min(60, delay)); delay <- delay - min(60, delay) }
  invisible(TRUE)
}

# libcurl error classes (see curl:::libcurl_error_codes) that a repeated request
# cannot fix: certificate/TLS verification, unsupported protocol, malformed URL
# and a failing local write.
.permanent_curl_errors <- c(
  "curl_error_peer_failed_verification", "curl_error_ssl_certproblem", "curl_error_ssl_cacert_badfile",
  "curl_error_ssl_issuer_error", "curl_error_ssl_pinnedpubkeynotmatch", "curl_error_ssl_invalidcertstatus",
  "curl_error_ssl_crl_badfile", "curl_error_unsupported_protocol", "curl_error_url_malformat",
  "curl_error_write_error")
.dns_curl_error <- "curl_error_couldnt_resolve_host"

# Classify a failed download attempt: "permanent" (stop now), "dns" (stop after
# a few attempts), "rate_limited" (wait for the server delay) or "transient"
# (back off). A transfer that completed but failed verification has no condition.
.classify_download_failure <- function(condition, attempt, retry_not_found = 0L) {
  if (inherits(condition, "resource_http_error")) {
    status <- condition$status
    if (!is.numeric(status) || length(status) != 1L || is.na(status)) return("transient")
    if (status %in% c(403L, 404L) && attempt <= retry_not_found) return("transient")
    if (status == 429L) return("rate_limited")
    if (status %in% c(408L, 425L)) return("transient")
    if ((status >= 400L && status < 500L) || status %in% c(501L, 505L)) return("permanent")
    return("transient")
  }
  if (inherits(condition, .permanent_curl_errors)) return("permanent")
  if (inherits(condition, .dns_curl_error)) return("dns")
  "transient"
}

.download_failure_message <- function(class, condition, file, url) {
  detail <- if (is.null(condition)) "" else paste0(" Underlying error: ", conditionMessage(condition))
  status <- if (inherits(condition, "resource_http_error")) condition$status else NA_integer_
  if (class == "dns")
    return(paste0("Cannot reach ", sub("^[A-Za-z][A-Za-z0-9+.-]*://([^/:?#]*).*$", "\\1", url),
                  "; check the network connection.", detail))
  if (!is.na(status) && status %in% c(404L, 410L))
    return(paste0(file, " is not available on Zenodo (HTTP ", status, "); the release may have been withdrawn ",
                  "or the catalog is outdated. Update PublicationBiasBenchmark or select another release ",
                  "with list_benchmark_releases().", detail))
  paste0("Download of '", file, "' failed with a permanent error and will not be retried.", detail)
}

# Download into a temporary file and install it only after size and hash
# verification. Permanent failures stop at once; DNS failures stop after three
# attempts; other failures back off, and rate limits wait for the server delay.
# retry_not_found: number of attempts on which HTTP 403/404 count as transient
# (a freshly published file may not be served yet); HTTP 410 is always permanent.
.fetch_verified <- function(url, destination, sha256, size = NULL, md5 = NULL,
                            progress = TRUE, max_try = 10, overwrite = FALSE, retry_not_found = 0L) {
  if (!is.numeric(max_try) || length(max_try) != 1L || is.na(max_try) || max_try < 1 || max_try %% 1 != 0)
    stop("max_try must be a positive integer.", call. = FALSE)
  if (!is.numeric(retry_not_found) || length(retry_not_found) != 1L || is.na(retry_not_found) ||
      retry_not_found < 0 || retry_not_found %% 1 != 0)
    stop("retry_not_found must be a non-negative integer.", call. = FALSE)
  if (!overwrite && .file_verified(destination, sha256, size, md5)) return(invisible(TRUE))
  dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
  temporary <- tempfile("transfer-", tmpdir = dirname(destination))
  on.exit(unlink(temporary), add = TRUE)
  condition <- NULL
  for (attempt in seq_len(max_try)) {
    downloaded <- try(.resource_download(url, temporary, progress), silent = TRUE)
    failed <- inherits(downloaded, "try-error")
    if (!failed && .file_verified(temporary, sha256, size, md5)) {
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
    condition <- if (failed) attr(downloaded, "condition") else NULL
    class <- .classify_download_failure(condition, attempt, retry_not_found)
    if (class == "permanent" || (class == "dns" && (attempt >= 3L || attempt >= max_try)))
      stop(.download_failure_message(class, condition, basename(destination), url), call. = FALSE)
    if (attempt < max_try) {
      delay <- if (class == "rate_limited" && is.numeric(condition$retry_delay)) condition$retry_delay else min(30, 2^(attempt - 1L))
      .resource_retry_wait(delay)
    }
  }
  if (inherits(condition, "resource_http_error") && isTRUE(condition$status %in% c(404L, 410L)))
    stop(.download_failure_message("permanent", condition, basename(destination), url), call. = FALSE)
  stop("Could not download and verify '", basename(destination), "' after ", max_try, " attempts",
       if (is.null(condition)) "." else paste0(" (last error: ", sub("[.]$", "", conditionMessage(condition)), ")."), call. = FALSE)
}

.cached_asset_files <- function(assets) {
  paths <- vapply(assets, .asset_cache_path, character(1))
  valid <- vapply(seq_along(assets), function(i) {
    x <- assets[[i]]
    .file_verified(paths[i], x$sha256, x$size)
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
  catalog <- benchmark_catalog(release)
  assets <- .select_assets(catalog, dgm_name, kind, method, method_setting)
  index <- .archive_index(catalog)
  data.frame(id = vapply(assets, `[[`, character(1), "id"),
             verified = vapply(assets, function(x) .file_verified(.asset_cache_path(x), x$sha256, x$size, x$md5), logical(1)),
             archive_id = vapply(assets, function(x) if (is.null(x$archive_id)) NA_character_ else x$archive_id, character(1)),
             archive_verified = vapply(assets, function(x) {
               if (is.null(x$archive_id)) return(NA)
               a <- .catalog_archive(catalog, x$archive_id, index)
               .file_verified(.archive_cache_path(a), a$sha256, a$size, a$md5)
             }, logical(1)))
}
