#' @title Describe a File for a Benchmark Release
#' @description Files remain separate for distributed jobs. Each results or
#' measures file must contain only its declared DGM, method and setting. Checksums
#' describe the exact bytes; package_version describes generation, not migration.
#' @param path Local file path.
#' @param dgm_name DGM name.
#' @param kind data, results, measures, metadata, or archive.
#' @param method Method identifier for results/measures.
#' @param method_setting Method setting identifier.
#' @param condition_ids Conditions covered by the file.
#' @param package_version Producing package version; NULL when historically unknown.
#' @param replacement Whether the file contains replacement measures.
#' @param measures Measures present in a wide measures file.
#' @param id Stable logical shard identifier; defaults to DGM/kind/basename.
#' @return A file descriptor for plan_benchmark_release.
#' @export
benchmark_resource <- function(path, dgm_name, kind, method = NULL, method_setting = NULL,
                               condition_ids = NULL, package_version = NULL, replacement = FALSE,
                               measures = NULL, id = NULL) {
  if (!file.exists(path) || dir.exists(path)) stop("Resource file does not exist.", call. = FALSE)
  if (is.null(id)) id <- paste(dgm_name, kind, basename(path), sep = "/")
  asset <- list(id = id, dgm = dgm_name, kind = kind, filename = basename(path),
                size = as.numeric(file.info(path)$size),
                sha256 = digest::digest(file = path, algo = "sha256", serialize = FALSE),
                md5 = unname(tools::md5sum(path)), record_id = "0", method = method,
                method_setting = method_setting, condition_ids = as.list(condition_ids),
                package_version = package_version, replacement = replacement,
                measures = as.list(measures), local_path = normalizePath(path, winslash = "/"))
  if (kind == "data") {
    data <- .read_resource_csv(path)
    if (!"repetition_id" %in% names(data) || !length(condition_ids))
      stop("Dataset files need repetition IDs and declared conditions.", call. = FALSE)
    if (!"condition_id" %in% names(data)) {
      if (length(condition_ids) != 1L) stop("A dataset without condition_id must cover one condition.", call. = FALSE)
      data$condition_id <- condition_ids
    }
    if (!setequal(unique(data$condition_id), condition_ids)) stop("Dataset conditions do not match their declaration.", call. = FALSE)
    asset$rows <- nrow(data)
    asset$coverage <- lapply(condition_ids, function(condition) {
      ids <- unique(data$repetition_id[data$condition_id == condition])
      list(condition_id = condition, repetitions = length(ids), ranges = .repetition_ranges(ids))
    })
  } else if (kind %in% c("results", "measures")) {
    data <- .validate_resource_rows(asset)
    asset$condition_ids <- as.list(sort(unique(data$condition_id)))
    asset$rows <- nrow(data)
    if (kind == "results") asset$coverage <- lapply(sort(unique(data$condition_id)), function(condition) {
      ids <- data$repetition_id[data$condition_id == condition]
      list(condition_id = condition, repetitions = length(ids), ranges = .repetition_ranges(ids))
    })
  }
  asset
}

.validate_resource_rows <- function(asset) {
  data <- .read_resource_csv(asset$local_path)
  if (!all(c("method", "method_setting", "condition_id") %in% names(data)) ||
      !.scalar_string(asset$method) || !.scalar_string(asset$method_setting) || !nrow(data) ||
      anyNA(data[c("method", "method_setting", "condition_id")]) ||
      !all(data$method == asset$method) || !all(data$method_setting == asset$method_setting))
    stop("Resource contents do not match their declared method and setting.", call. = FALSE)
  keys <- c("method", "method_setting", "condition_id")
  if (asset$kind == "results") keys <- c(keys, "repetition_id")
  .reject_duplicate_keys(data, keys, asset$kind)
  data
}

.repetition_ranges <- function(ids) {
  ids <- sort(unique(ids))
  if (!length(ids) || anyNA(ids) || any(ids < 1 | ids %% 1 != 0)) stop("Invalid repetition IDs.", call. = FALSE)
  starts <- c(1L, which(diff(ids) != 1) + 1L)
  ends <- c(starts[-1L] - 1L, length(ids))
  lapply(seq_along(starts), function(i) list(start = ids[starts[i]], end = ids[ends[i]]))
}

.validate_plan_coverage <- function(assets) {
  results <- Filter(function(x) x$kind %in% c("data", "results"), assets)
  groups <- split(results, vapply(results, function(x) paste(c(x$dgm, x$kind, x$method, x$method_setting), collapse = "/"), character(1)))
  for (group in groups) {
    spans <- list()
    for (asset in group) for (condition in asset$coverage) {
      if (!length(condition$ranges)) stop("Data and result shards need explicit repetition coverage.", call. = FALSE)
      for (range in condition$ranges) spans[[length(spans) + 1L]] <- data.frame(
        condition = condition$condition_id, start = range$start, end = range$end, shard = asset$id)
    }
    if (!length(spans)) stop("Data and result shards need explicit repetition coverage.", call. = FALSE)
    for (spans in split(do.call(rbind, spans), do.call(rbind, spans)$condition)) {
      spans <- spans[order(spans$start), , drop = FALSE]
      if (nrow(spans) > 1L && any(spans$start[-1L] <= cummax(spans$end)[-nrow(spans)]))
        stop("Overlapping data or result shards in the proposed release.", call. = FALSE)
    }
  }
  measures <- Filter(function(x) x$kind == "measures", assets)
  groups <- split(measures, vapply(measures, function(x)
    paste(x$dgm, x$method, x$method_setting, isTRUE(x$replacement), sep = "/"), character(1)))
  for (group in groups) {
    ids <- unlist(lapply(group, `[[`, "condition_ids"))
    if (anyDuplicated(ids)) stop("Overlapping measure shards in the proposed release.", call. = FALSE)
  }
  invisible(TRUE)
}

#' @title Plan, Stage, and Publish an Append-Only Benchmark Release
#' @description A plan combines unchanged references from a previous catalog with
#' new files. Changing an existing shard requires listing its ID in replace.
#' Only new payloads are uploaded. Publication makes the complete catalog available
#' after all component records are public and their files have been verified.
#' @param release New benchmark release identifier.
#' @param files List of descriptors returned by benchmark_resource.
#' @param conditions Named list of frozen DGM condition data frames.
#' @param previous Previous release identifier or catalog, or NULL for the baseline.
#' @param replace IDs of existing shards intentionally replaced.
#' @param metadata Zenodo-native metadata list including creators and rights (license IDs).
#' @param state_directory Directory for resumable publication state, outside Git.
#' @param max_files Maximum uploaded files per component record (at most 100).
#' @param max_bytes Maximum component size (at most 50 billion bytes).
#' @param sandbox Use sandbox.zenodo.org and a separate sandbox token.
#' @param package_version Package version assembling the release (generation stamps remain per file).
#' @param source_commit Source commit for the frozen DGM definitions.
#' @param provenance Optional source archive metadata stored in the catalog.
#' @param plan Release plan.
#' @param token Zenodo token; defaults to ZENODO_TOKEN (ZENODO_SANDBOX_TOKEN for sandbox).
#' @return plan_benchmark_release returns a plan; staging returns the staged catalog;
#' publication returns a registry entry including the catalog's version DOI and hash.
#' @name publish_benchmark_release
NULL

#' @rdname publish_benchmark_release
#' @export
plan_benchmark_release <- function(release, files, conditions = NULL, previous = NULL,
                                   replace = character(), metadata, state_directory,
                                   max_files = 100L, max_bytes = 50e9, sandbox = FALSE,
                                   package_version = as.character(utils::packageVersion("PublicationBiasBenchmark")),
                                   source_commit = NULL, provenance = NULL) {
  if (!.scalar_string(release) || !grepl("^[A-Za-z0-9._-]+$", release)) stop("Invalid release identifier.", call. = FALSE)
  if (!is.null(metadata$licenses) || !is.null(metadata$license))
    stop("Zenodo-native license metadata belongs in metadata$rights, not license or licenses.", call. = FALSE)
  if (max_files < 1 || max_files > 100 || max_files %% 1 || max_bytes <= 0 || max_bytes > 50e9)
    stop("Packing limits exceed the default Zenodo quota.", call. = FALSE)
  base <- if (is.null(previous)) NULL else benchmark_catalog(previous)
  if (anyDuplicated(vapply(files, `[[`, character(1), "id"))) stop("Duplicate input shard IDs.", call. = FALSE)
  if (!is.null(base) && identical(base$release, release)) stop("A new release needs a new identifier.", call. = FALSE)
  if (!is.null(base) && !identical(isTRUE(base$sandbox), sandbox)) stop("Cannot mix sandbox and production records.", call. = FALSE)
  assets <- if (is.null(base)) list() else base$assets
  new_files <- list()
  for (file in files) {
    old <- which(vapply(assets, function(x) identical(x$id, file$id), logical(1)))
    if (length(old)) {
      if (identical(assets[[old]]$sha256, file$sha256)) next
      if (!file$id %in% replace) stop("Explicit replacement required for shard '", file$id, "'.", call. = FALSE)
      assets <- assets[-old]
    }
    if (!.file_verified(file$local_path, file$sha256, file$size, file$md5)) stop("Unverified local file: ", file$filename, call. = FALSE)
    assets[[length(assets) + 1L]] <- file
    new_files[[length(new_files) + 1L]] <- file
  }
  if (is.null(conditions)) conditions <- base$conditions
  if (!is.null(base)) for (dgm in names(base$conditions)) {
    old <- .catalog_conditions(base, dgm)
    current <- .catalog_conditions(list(conditions = conditions), dgm)
    index <- match(old$condition_id, current$condition_id)
    if (anyNA(index) || !identical(jsonlite::toJSON(old, dataframe = "rows", digits = NA),
                                  jsonlite::toJSON(current[index, names(old), drop = FALSE], dataframe = "rows", digits = NA)))
      stop("Existing frozen condition definitions cannot change between releases.", call. = FALSE)
  }
  catalog <- list(schema_version = 1L, release = release, sandbox = sandbox,
                   previous_release = if (is.null(base)) NULL else base$release,
                   package_version = package_version,
                   source_commit = source_commit,
                   provenance = provenance,
                   conditions = conditions, assets = assets)
  .validate_catalog(catalog)
  .validate_plan_coverage(assets)
  groups <- list()
  for (group in split(new_files, vapply(new_files, function(x) paste(x$dgm, x$kind, sep = "/"), character(1)))) {
    part <- list(); bytes <- 0
    for (file in group) {
      if (file$size > max_bytes) stop("A file exceeds the configured record quota: ", file$filename, call. = FALSE)
      if (length(part) && (length(part) >= max_files || bytes + file$size > max_bytes)) {
        groups[[length(groups) + 1L]] <- part; part <- list(); bytes <- 0
      }
      part[[length(part) + 1L]] <- file; bytes <- bytes + file$size
    }
    if (length(part)) groups[[length(groups) + 1L]] <- part
  }
  if (any(vapply(groups, function(group) anyDuplicated(vapply(group, `[[`, character(1), "filename")) > 0L, logical(1))))
    stop("Component files need unique filenames. Prefix distributed shard names before staging.", call. = FALSE)
  plan <- list(catalog = catalog, groups = groups, metadata = metadata,
               state_directory = normalizePath(state_directory, winslash = "/", mustWork = FALSE), sandbox = sandbox)
  dir.create(state_directory, recursive = TRUE, showWarnings = FALSE)
  plan_path <- file.path(state_directory, "plan.rds")
  if (file.exists(plan_path)) {
    existing <- readRDS(plan_path)
    if (!identical(existing$catalog, plan$catalog)) stop("Existing publication state belongs to a different plan.", call. = FALSE)
  } else saveRDS(plan, plan_path)
  plan
}

.zenodo_base <- function(sandbox) paste0("https://", if (sandbox) "sandbox." else "", "zenodo.org/api")
.publication_token <- function(plan, token) {
  if (is.null(token)) token <- Sys.getenv(if (plan$sandbox) "ZENODO_SANDBOX_TOKEN" else "ZENODO_TOKEN")
  if (!.scalar_string(token)) stop("The required Zenodo token is not configured.", call. = FALSE)
  token
}

.zenodo_request <- function(method, path, token, sandbox = FALSE, body = NULL) {
  url <- paste0(.zenodo_base(sandbox), "/", path)
  # Record metadata has a native representation; file endpoints only accept JSON.
  accept <- if (grepl("^records(/[0-9]+(/draft)?)?$", path)) "application/vnd.inveniordm.v1+json" else "application/json"
  headers <- httr::add_headers(Authorization = paste("Bearer", token), Accept = accept)
  for (attempt in seq_len(5L)) {
    response <- httr::VERB(method, url, headers, body = body, encode = "json", httr::timeout(180))
    status <- httr::status_code(response)
    # A rate-limited mutation was rejected; other uncertain mutations use state.
    retry <- status == 429L || (method == "GET" && status %in% c(500L, 502L, 503L, 504L))
    if (!retry || attempt == 5L) break
    Sys.sleep(.zenodo_retry_delay(httr::headers(response), status, attempt))
  }
  status <- httr::status_code(response)
  if (status >= 400) {
    # Do not print request objects: they contain the authorization header.
    detail <- try(httr::content(response, as = "parsed", type = "application/json", encoding = "UTF-8"), silent = TRUE)
    message <- if (is.list(detail) && .scalar_string(detail$message)) detail$message else "request rejected"
    stop(structure(list(message = paste0("Zenodo ", method, " failed (HTTP ", status, "): ", message),
                        call = NULL, status = status, errors = if (is.list(detail)) detail$errors else NULL),
                   class = c("zenodo_http_error", "error", "condition")))
  }
  if (status == 204) return(invisible(NULL))
  httr::content(response, as = "parsed", type = "application/json", encoding = "UTF-8")
}

.zenodo_retry_delay <- function(headers, status, attempt) {
  if (status == 429L) {
    delay <- suppressWarnings(as.numeric(headers[["retry-after"]]))
    if (length(delay) == 1L && is.finite(delay) && delay >= 0) return(delay)
    reset <- suppressWarnings(as.numeric(headers[["x-ratelimit-reset"]]))
    if (length(reset) == 1L && is.finite(reset) && reset > as.numeric(Sys.time()))
      return(reset - as.numeric(Sys.time()) + 1)
    return(60)
  }
  min(30, 2^(attempt - 1L))
}

.save_publication_state <- function(plan, state) {
  path <- file.path(plan$state_directory, "state.json")
  temporary <- paste0(path, ".tmp")
  jsonlite::write_json(state, temporary, auto_unbox = TRUE, pretty = TRUE, null = "null")
  if (file.exists(path)) file.remove(path)
  if (!file.rename(temporary, path)) stop("Cannot save publication state.", call. = FALSE)
}

.publication_state <- function(plan) {
  path <- file.path(plan$state_directory, "state.json")
  if (file.exists(path)) jsonlite::read_json(path, simplifyVector = FALSE) else list(groups = list(), catalog_record = NULL)
}

.record_is_published <- function(plan, record_id, token) {
  record <- tryCatch(.zenodo_request("GET", paste0("records/", record_id), token, plan$sandbox),
                     zenodo_http_error = function(error) { if (error$status == 404L) NULL else stop(error) })
  !is.null(record) && (isTRUE(record$is_published) || identical(record$status, "published") || identical(record$state, "done"))
}

.publish_record <- function(plan, record_id, token) {
  if (!.record_is_published(plan, record_id, token))
    .zenodo_request("POST", paste0("records/", record_id, "/draft/actions/publish"), token, plan$sandbox)
  invisible(TRUE)
}

.verify_record_rights <- function(record, metadata) {
  for (expected in metadata$rights) {
    retained <- vapply(record$metadata$rights, function(right) {
      if (!is.null(expected$id)) identical(right$id, expected$id) else identical(right$title, expected$title)
    }, logical(1))
    if (!any(retained)) stop("Zenodo did not retain the requested license metadata.", call. = FALSE)
  }
  invisible(TRUE)
}

.create_component_record <- function(plan, title, description, token) {
  metadata <- plan$metadata
  if (!length(metadata$rights)) stop("Include a license in metadata$rights before staging a release.", call. = FALSE)
  # Recover a draft after a lost create response instead of creating duplicates.
  query <- utils::URLencode(paste0('metadata.title:"', title, '"'), reserved = TRUE)
  existing <- .zenodo_request("GET", paste0("user/records?q=", query, "&size=100"), token, plan$sandbox)
  matches <- Filter(function(x) identical(x$metadata$title, title), existing$hits$hits)
  if (length(matches) > 1L) stop("Multiple records match this publication batch.", call. = FALSE)
  if (length(matches)) {
    id <- as.character(matches[[1]]$id)
    record <- tryCatch(.zenodo_request("GET", paste0("records/", id, "/draft"), token, plan$sandbox),
      zenodo_http_error = function(error) {
        if (error$status == 404L) .zenodo_request("GET", paste0("records/", id), token, plan$sandbox) else stop(error)
      })
    .verify_record_rights(record, metadata)
    return(id)
  }
  metadata$title <- title; metadata$description <- description
  metadata$version <- plan$catalog$release
  metadata$resource_type <- list(id = "dataset")
  if (is.null(metadata$publisher)) metadata$publisher <- "Zenodo"
  if (is.null(metadata$publication_date)) metadata$publication_date <- as.character(Sys.Date())
  draft <- .zenodo_request("POST", "records", token, plan$sandbox,
                           list(metadata = metadata, access = list(record = "public", files = "public"),
                                files = list(enabled = TRUE)))
  .verify_record_rights(draft, metadata)
  as.character(draft$id)
}

.zenodo_upload <- function(path, record_id, filename, token, sandbox) {
  url <- paste0(.zenodo_base(sandbox), "/records/", record_id, "/draft/files/",
                 utils::URLencode(filename, reserved = TRUE), "/content")
  stream <- file(path, "rb"); on.exit(close(stream), add = TRUE)
  for (attempt in seq_len(5L)) {
    seek(stream, 0, origin = "start")
    handle <- curl::new_handle(upload = TRUE, customrequest = "PUT", infilesize = file.info(path)$size,
                               readfunction = function(n) readBin(stream, "raw", n = n),
                               connecttimeout = 30, low_speed_limit = 1, low_speed_time = 180)
    curl::handle_setheaders(handle, Authorization = paste("Bearer", token), `Content-Type` = "application/octet-stream")
    response <- curl::curl_fetch_memory(url, handle)
    if (response$status_code != 429L || attempt == 5L) break
    Sys.sleep(.zenodo_retry_delay(curl::parse_headers_list(response$headers), 429L, attempt))
  }
  if (response$status_code >= 400)
    stop(structure(list(message = paste0("Zenodo file upload failed (HTTP ", response$status_code, ")."),
                        call = NULL, status = response$status_code), class = c("zenodo_http_error", "error", "condition")))
  invisible(TRUE)
}

.stage_file <- function(plan, record_id, asset, token) {
  .stage_files(plan, record_id, list(asset), token)
}

.stage_files <- function(plan, record_id, assets, token) {
  files <- .zenodo_request("GET", paste0("records/", record_id, "/draft/files"), token, plan$sandbox)
  pending <- Filter(function(asset) {
    entry <- .zenodo_file_entry(files, asset$filename)
    if (.zenodo_entry_verified(entry, asset)) return(FALSE)
    if (!is.null(entry) && !identical(entry$status, "pending"))
      stop("Existing draft file differs; refusing to overwrite it.", call. = FALSE)
    if (!.file_verified(asset$local_path, asset$sha256, asset$size, asset$md5))
      stop("Local staged file changed.", call. = FALSE)
    TRUE
  }, assets)
  initialize <- Filter(function(asset) {
    entry <- .zenodo_file_entry(files, asset$filename)
    is.null(entry) || (!.zenodo_entry_bytes_verified(entry, asset) &&
      (!is.null(entry$size) || !is.null(entry$checksum)))
  }, pending)
  for (asset in initialize) {
    if (!is.null(.zenodo_file_entry(files, asset$filename)))
      .zenodo_request("DELETE", paste0("records/", record_id, "/draft/files/", utils::URLencode(asset$filename, reserved = TRUE)), token, plan$sandbox)
  }
  # Initialize a record's new files together; completed files remain untouched.
  if (length(initialize)) .zenodo_request("POST", paste0("records/", record_id, "/draft/files"), token, plan$sandbox,
                                         lapply(initialize, function(asset) list(key = asset$filename)))
  for (asset in pending) {
    # Empty pending keys are reusable; an upload whose response was lost can be
    # committed directly when its stored bytes already match the planned file.
    if (!.zenodo_entry_bytes_verified(.zenodo_file_entry(files, asset$filename), asset))
      .zenodo_upload(asset$local_path, record_id, asset$filename, token, plan$sandbox)
    entry <- .zenodo_request("POST", paste0("records/", record_id, "/draft/files/",
                             utils::URLencode(asset$filename, reserved = TRUE), "/commit"), token, plan$sandbox)
    if (!.zenodo_entry_verified(entry, asset))
      stop("Zenodo uploaded-file verification failed.", call. = FALSE)
    message("Staged ", paste(c(asset$dgm, asset$kind, asset$filename), collapse = "/"))
  }
  invisible(TRUE)
}

.zenodo_file_entry <- function(files, filename) {
  entry <- files$entries[[filename]]
  if (!is.null(entry)) return(entry)
  matches <- Filter(function(x) identical(x$key, filename), files$entries)
  if (length(matches) > 1L) stop("Duplicate files in the Zenodo response.", call. = FALSE)
  if (length(matches)) matches[[1]] else NULL
}

.zenodo_entry_verified <- function(entry, asset) {
  !is.null(entry) && identical(entry$status, "completed") && .zenodo_entry_bytes_verified(entry, asset)
}

.zenodo_entry_bytes_verified <- function(entry, asset) {
  !is.null(entry) && identical(entry$checksum, paste0("md5:", asset$md5)) &&
    is.numeric(entry$size) && length(entry$size) == 1L && !is.na(entry$size) && entry$size == asset$size
}

#' @rdname publish_benchmark_release
#' @export
stage_benchmark_release <- function(plan, token = NULL) {
  token <- .publication_token(plan, token)
  state <- .publication_state(plan)
  catalog <- plan$catalog
  asset_indices <- stats::setNames(seq_along(catalog$assets), vapply(catalog$assets, `[[`, character(1), "id"))
  for (i in seq_along(plan$groups)) {
    key <- as.character(i); group <- plan$groups[[i]]
    if (is.null(state$groups[[key]])) {
      title <- sprintf("PublicationBiasBenchmark %s: %s %s, part %03d", catalog$release, group[[1]]$dgm, group[[1]]$kind, i)
      id <- .create_component_record(plan, title,
             "Immutable benchmark files. Each file can be downloaded independently. File generation versions and checksums are recorded in the benchmark release catalog.", token)
      state$groups[[key]] <- list(record_id = id, published = FALSE)
      .save_publication_state(plan, state)
    }
    record <- state$groups[[key]]
    if (!isTRUE(record$published) && .record_is_published(plan, record$record_id, token)) {
      record$published <- TRUE; state$groups[[key]] <- record
      .save_publication_state(plan, state)
    }
    if (!isTRUE(record$published)) {
      .stage_files(plan, record$record_id, group, token)
    }
    for (asset in group) {
      index <- asset_indices[[asset$id]]
      catalog$assets[[index]]$record_id <- record$record_id
    }
  }
  catalog$assets <- lapply(catalog$assets, function(x) { x$local_path <- NULL; x })
  .validate_catalog(catalog)
  jsonlite::write_json(catalog, file.path(plan$state_directory, "release.json"), auto_unbox = TRUE,
                       pretty = FALSE, null = "null", digits = NA, dataframe = "rows")
  catalog
}

#' @rdname publish_benchmark_release
#' @export
verify_benchmark_release <- function(plan, token = NULL) {
  token <- .publication_token(plan, token)
  state <- .publication_state(plan)
  for (i in seq_along(plan$groups)) {
    record <- state$groups[[as.character(i)]]
    if (is.null(record)) stop("The release has not been fully staged.", call. = FALSE)
    metadata <- .zenodo_request("GET", paste0("records/", record$record_id,
      if (!isTRUE(record$published)) "/draft" else ""), token, plan$sandbox)
    .verify_record_rights(metadata, plan$metadata)
    suffix <- if (isTRUE(record$published)) "/files" else "/draft/files"
    files <- .zenodo_request("GET", paste0("records/", record$record_id, suffix), token, plan$sandbox)
    for (asset in plan$groups[[i]]) {
      entry <- .zenodo_file_entry(files, asset$filename)
      if (!.zenodo_entry_verified(entry, asset))
        stop("Incomplete or unverified staged release.", call. = FALSE)
    }
  }
  invisible(TRUE)
}

#' @rdname publish_benchmark_release
#' @export
publish_benchmark_release <- function(plan, token = NULL) {
  token <- .publication_token(plan, token)
  stage_benchmark_release(plan, token)
  verify_benchmark_release(plan, token)
  state <- .publication_state(plan)
  for (key in names(state$groups)) {
    record <- state$groups[[key]]
    if (!isTRUE(record$published)) {
      .publish_record(plan, record$record_id, token)
      state$groups[[key]]$published <- TRUE
      .save_publication_state(plan, state)
    }
  }
  # Public bytes must verify before a catalog advertises them as a usable release.
  catalog <- jsonlite::read_json(file.path(plan$state_directory, "release.json"), simplifyVector = FALSE)
  verify_dir <- file.path(plan$state_directory, "public-verification")
  for (group in plan$groups) for (asset in group) {
    reference <- Filter(function(x) identical(x$id, asset$id), catalog$assets)[[1]]
    .fetch_verified(.zenodo_file_url(reference$record_id, reference$filename, plan$sandbox),
                    file.path(verify_dir, asset$sha256), asset$sha256, asset$size, asset$md5, progress = FALSE)
  }
  if (is.null(state$catalog_record)) {
    state$catalog_record <- list(record_id = .create_component_record(plan,
      paste0("PublicationBiasBenchmark release ", catalog$release),
      "Complete catalog of this benchmark release. Unchanged assets refer to earlier immutable records; only new or corrected files are uploaded. Use the version DOI to reproduce this release.", token), published = FALSE)
    .save_publication_state(plan, state)
  }
  path <- file.path(plan$state_directory, "release.json")
  catalog_asset <- list(filename = "release.json", local_path = path, size = as.numeric(file.info(path)$size),
                         sha256 = digest::digest(file = path, algo = "sha256", serialize = FALSE), md5 = unname(tools::md5sum(path)))
  if (!isTRUE(state$catalog_record$published)) {
    if (!.record_is_published(plan, state$catalog_record$record_id, token))
      .stage_file(plan, state$catalog_record$record_id, catalog_asset, token)
    .publish_record(plan, state$catalog_record$record_id, token)
    state$catalog_record$published <- TRUE; .save_publication_state(plan, state)
  }
  record <- .zenodo_request("GET", paste0("records/", state$catalog_record$record_id), token, plan$sandbox)
  .verify_record_rights(record, plan$metadata)
  result <- list(release = catalog$release, record_id = state$catalog_record$record_id,
                  catalog_sha256 = catalog_asset$sha256,
                  doi = if (!is.null(record$pids$doi$identifier)) record$pids$doi$identifier else record$doi)
  .fetch_verified(.zenodo_file_url(result$record_id, "release.json", plan$sandbox),
                  file.path(verify_dir, "release.json"), result$catalog_sha256, catalog_asset$size, catalog_asset$md5,
                  progress = FALSE)
  jsonlite::write_json(result, file.path(plan$state_directory, "registry-entry.json"), auto_unbox = TRUE, pretty = TRUE)
  result
}
