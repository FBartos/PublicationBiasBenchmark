.zenodo_get_optional <- function(path, plan, token) {
  tryCatch(.zenodo_request("GET", path, token, plan$sandbox), zenodo_http_error = function(error) {
    if (error$status == 404L) NULL else stop(error)
  })
}
.zenodo_link_path <- function(link, sandbox) {
  prefix <- paste0(.zenodo_base(sandbox), "/")
  if (!.scalar_string(link) || !startsWith(link, prefix)) stop("Unexpected Zenodo API link.", call. = FALSE)
  substring(link, nchar(prefix) + 1L)
}
.zenodo_doi <- function(record) {
  doi <- record$pids$doi$identifier
  if (is.null(doi)) doi <- record$doi
  if (!.scalar_string(doi)) stop("Zenodo did not return a DOI.", call. = FALSE)
  doi
}
.zenodo_concept_doi <- function(record) {
  doi <- record$parent$pids$doi$identifier
  if (is.null(doi)) doi <- record$conceptdoi
  if (!.scalar_string(doi)) stop("Zenodo did not return a concept DOI.", call. = FALSE)
  doi
}
.record_metadata <- function(plan, title, description, relationships = list()) {
  metadata <- plan$metadata
  if (!length(metadata$rights)) stop("Include a license in metadata$rights before staging a release.", call. = FALSE)
  metadata$title <- title; metadata$description <- description; metadata$version <- plan$catalog$release
  tags <- unique(c(unlist(metadata$keywords), "PublicationBiasBenchmark", plan$catalog$release))
  metadata$keywords <- NULL
  metadata$subjects <- unname(c(metadata$subjects, lapply(tags, function(x) list(subject = x))))
  metadata$subjects <- metadata$subjects[!duplicated(vapply(metadata$subjects, function(x) x$subject, character(1)))]
  metadata$resource_type <- list(id = "dataset")
  if (is.null(metadata$publisher)) metadata$publisher <- "Zenodo"
  if (is.null(metadata$publication_date)) metadata$publication_date <- as.character(Sys.Date())
  related <- c(metadata$related_identifiers, relationships)
  if (length(related)) related <- related[!duplicated(vapply(related, function(x) paste(x$identifier, x$relation_type$id, sep = "/"), character(1)))]
  metadata$related_identifiers <- unname(related)
  metadata
}
.doi_relationship <- function(doi, relation) list(identifier = doi, scheme = "doi", relation_type = list(id = relation))
.update_draft_metadata <- function(plan, id, metadata, token) {
  draft <- .zenodo_request("GET", paste0("records/", id, "/draft"), token, plan$sandbox)
  # Preserve native metadata inherited from the old family, including provenance.
  # modifyList() recursively ignores replacements of unnamed JSON arrays.
  # Overlay fields explicitly so creators, rights and relationships remain exact.
  inherited_metadata <- draft$metadata
  for (field in names(metadata)) inherited_metadata[field] <- metadata[field]
  metadata <- inherited_metadata
  # Native GET responses expand controlled vocabularies with UI-only fields.
  # Submit identifiers, not those read-only expansions, when editing a record.
  if (length(metadata$rights)) metadata$rights <- lapply(metadata$rights, function(right) {
    if (!is.null(right$id)) list(id = right$id) else right
  })
  if (!is.null(metadata$resource_type$id)) metadata$resource_type <- list(id = metadata$resource_type$id)
  inherited <- draft$metadata$related_identifiers
  related <- c(inherited, metadata$related_identifiers)
  if (length(related)) {
    # A new catalog snapshot replaces old hasPart links; storage isPartOf is stable.
    if (any(vapply(metadata$related_identifiers, function(x) identical(x$relation_type$id, "haspart"), logical(1))))
      related <- c(Filter(function(x) !identical(x$relation_type$id, "haspart"), inherited), metadata$related_identifiers)
    related <- related[!duplicated(vapply(related, function(x) paste(x$identifier, x$relation_type$id, sep = "/"), character(1)))]
    metadata$related_identifiers <- unname(lapply(related, function(x) {
      if (!is.null(x$relation_type$id)) x$relation_type <- list(id = x$relation_type$id)
      x
    }))
  }
  access <- list(record = draft$access$record, files = draft$access$files)
  if (!is.null(draft$access$embargo)) {
    access$embargo <- Filter(Negate(is.null), draft$access$embargo)
  }
  result <- .zenodo_request("PUT", paste0("records/", id, "/draft"), token, plan$sandbox,
    list(metadata = metadata, access = access))
  errors <- Filter(function(x) !(identical(x$field, "files.enabled") &&
    all(unlist(x$messages) %in% "Missing uploaded files.")), result$errors)
  if (length(errors)) stop("Zenodo draft metadata validation failed: ",
    paste(vapply(errors, function(x) paste0(x$field, ": ", paste(unlist(x$messages), collapse = "; ")), character(1)), collapse = ", "), call. = FALSE)
  .verify_record_rights(result, plan$metadata)
  result
}
.family_drafts <- function(plan, family_id, token) {
  query <- utils::URLencode(paste0('parent.id:"', family_id, '"'), reserved = TRUE)
  response <- .zenodo_request("GET", paste0("user/records?q=", query, "&allversions=true&size=100"), token, plan$sandbox)
  Filter(function(x) !isTRUE(x$is_published) && !identical(x$status, "published") && !identical(x$state, "done"), response$hits$hits)
}
.ensure_family_version <- function(plan, key, base_id, title, description, metadata, token) {
  slot <- .publication_state(plan)$versions[[key]]
  if (!is.null(slot$record_id)) return(as.character(slot$record_id))
  if (is.null(base_id)) {
    id <- .create_component_record(plan, title, description, token)
  } else {
    base <- .zenodo_request("GET", paste0("records/", base_id), token, plan$sandbox)
    latest <- .zenodo_request("GET", paste0("records/", base_id, "/versions/latest"), token, plan$sandbox)
    if (!identical(as.character(latest$id), as.character(base_id))) {
      if (identical(latest$metadata$title, title) && identical(latest$metadata$version, plan$catalog$release)) id <- as.character(latest$id)
      else stop("The record family advanced beyond this plan's base version.", call. = FALSE)
    } else {
      drafts <- .family_drafts(plan, as.character(base$parent$id), token)
      if (length(drafts) > 1L) stop("Multiple drafts exist in this record family.", call. = FALSE)
      if (length(drafts)) {
        if (!isTRUE(slot$creating) && !identical(drafts[[1]]$metadata$title, title))
          stop("An unrelated new-version draft already exists; reconcile it before publishing.", call. = FALSE)
        id <- as.character(drafts[[1]]$id)
      } else {
        .update_publication_state(plan, function(state) {
          state$versions[[key]] <- list(creating = TRUE, base_id = base_id); state
        })
        draft <- .zenodo_request("POST", paste0("records/", base_id, "/versions"), token, plan$sandbox)
        id <- as.character(draft$id)
      }
    }
  }
  published <- .record_is_published(plan, id, token)
  .update_publication_state(plan, function(state) {
    state$versions[[key]] <- list(record_id = id, base_id = base_id, published = published); state
  })
  if (!isTRUE(published)) .update_draft_metadata(plan, id, metadata, token)
  id
}
.ensure_imported_files <- function(plan, key, id, base_id, archives, token) {
  if (is.null(base_id)) return(invisible(TRUE))
  files <- .zenodo_request("GET", paste0("records/", id, "/draft/files"), token, plan$sandbox)
  base_files <- .zenodo_request("GET", paste0("records/", base_id, "/files"), token, plan$sandbox)
  if (!isTRUE(.publication_state(plan)$versions[[key]]$imported)) {
    # Imported entries must match the base snapshot by key, checksum and size.
    present <- vapply(base_files$entries, function(x) {
      entry <- .zenodo_file_entry(files, x$key)
      !is.null(entry) && identical(entry$checksum, x$checksum) && identical(as.numeric(entry$size), as.numeric(x$size))
    }, logical(1))
    if (!all(present)) {
      if (length(files$entries)) stop("Incomplete or unrelated draft imports; refusing to reset uploaded files.", call. = FALSE)
      .zenodo_request("POST", paste0("records/", id, "/draft/actions/files-import"), token, plan$sandbox)
    }
    .update_publication_state(plan, function(state) { state$versions[[key]]$imported <- TRUE; state })
  }
  keep <- vapply(archives, `[[`, character(1), "filename")
  files <- .zenodo_request("GET", paste0("records/", id, "/draft/files"), token, plan$sandbox)
  for (entry in files$entries) if (!entry$key %in% keep) {
    # Delete only imported copies of files that the published base record holds.
    .delete_draft_file(plan, id, entry$key, "superseded-import", token, base_id)
    .update_publication_state(plan, function(state) {
      state$versions[[key]]$deleted <- as.list(unique(c(unlist(state$versions[[key]]$deleted), entry$key))); state
    })
  }
  invisible(TRUE)
}
# community_id is the UUID the gate resolved; it must not change between runs.
.resolve_publication_community <- function(plan, community_id) {
  state <- .publication_state(plan)
  if (!is.null(state$community_id) && !identical(state$community_id, community_id)) stop("Resolved community identity changed.", call. = FALSE)
  .update_publication_state(plan, function(state) { state$community_id <- community_id; state })
  community_id
}
.community_record_verified <- function(record, community_id) {
  community_id %in% unlist(record$parent$communities$ids) && identical(record$parent$communities$default, community_id)
}
.include_record_community <- function(plan, id, community_id, token) {
  record <- .zenodo_request("GET", paste0("records/", id), token, plan$sandbox)
  if (.community_record_verified(record, community_id)) {
    family <- as.character(record$parent$id)
    .update_publication_state(plan, function(state) {
      state$inclusions[[family]] <- list(community_id = community_id, accepted = TRUE); state
    })
    return(invisible(TRUE))
  }
  if (!community_id %in% unlist(record$parent$communities$ids)) {
    # Search record requests before submitting, including after a lost response.
    requests <- .zenodo_request("GET", paste0("records/", id, "/requests?size=100"), token, plan$sandbox)
    matches <- Filter(function(x) identical(x$type, "community-inclusion") && identical(x$receiver$community, community_id) &&
      x$status %in% c("created", "submitted", "accepted"), requests$hits$hits)
    if (length(matches) > 1L) stop("Duplicate community inclusion requests.", call. = FALSE)
    if (!length(matches)) {
      response <- .zenodo_request("POST", paste0("records/", id, "/communities"), token, plan$sandbox,
        list(communities = list(list(id = community_id, require_review = TRUE))))
      if (length(response$errors) || length(response$processed) != 1L) stop("Community inclusion was not fully submitted.", call. = FALSE)
      request <- response$processed[[1]]$request
      if (is.null(request)) request <- .zenodo_request("GET", paste0("requests/", response$processed[[1]]$request_id), token, plan$sandbox)
    } else request <- matches[[1]]
    family <- as.character(record$parent$id)
    .update_publication_state(plan, function(state) {
      state$inclusions[[family]] <- list(request_id = request$id, community_id = community_id,
                                         accepted = identical(request$status, "accepted")); state
    })
    if (!identical(request$status, "accepted")) {
      action <- request$links$actions$accept
      path <- if (.scalar_string(action)) .zenodo_link_path(action, plan$sandbox) else paste0("requests/", request$id, "/actions/accept")
      .zenodo_request("POST", path, token, plan$sandbox, list())
    }
  }
  record <- .zenodo_request("GET", paste0("records/", id), token, plan$sandbox)
  if (!community_id %in% unlist(record$parent$communities$ids)) stop("Community inclusion is not accepted.", call. = FALSE)
  if (!identical(record$parent$communities$default, community_id))
    .zenodo_request("PUT", paste0("records/", id, "/communities"), token, plan$sandbox, list(default = list(id = community_id)))
  record <- .zenodo_request("GET", paste0("records/", id), token, plan$sandbox)
  if (!.community_record_verified(record, community_id)) stop("Community branding verification failed.", call. = FALSE)
  family <- as.character(record$parent$id)
  .update_publication_state(plan, function(state) {
    state$inclusions[[family]] <- list(community_id = community_id, accepted = TRUE); state
  })
  invisible(TRUE)
}
.storage_base_id <- function(plan, dgm) {
  if (is.null(plan$previous) || plan$previous$schema_version != 2L) return(NULL)
  archives <- Filter(function(x) identical(x$dgm, dgm), plan$previous$archives)
  if (length(archives)) archives[[1]]$record_id else NULL
}
.public_catalog <- function(catalog) {
  catalog$assets <- lapply(catalog$assets, function(x) { x$local_path <- NULL; x })
  catalog$archives <- lapply(catalog$archives, function(x) { x$local_path <- NULL; x$build_fingerprint <- NULL; x })
  .validate_catalog(catalog)
}
# Catalogs are LF-terminated JSON. The installed file must parse, validate and
# carry the publication block exactly; otherwise the previous file is restored.
.write_catalog_file <- function(path, catalog) {
  expected <- catalog$publication
  .write_json_verified(path, catalog, history_dir = file.path(dirname(path), "catalog-history"),
    verify = function(installed) {
      parsed <- .parse_json_bytes(.read_bytes(installed))
      .validate_catalog(parsed)
      is.null(expected) || identical(.canonical_json(parsed$publication), .canonical_json(expected))
    })
}
.write_release_catalog <- function(plan, catalog) {
  .write_catalog_file(file.path(plan$state_directory, "release.json"), .public_catalog(catalog))
}
.reconcile_catalog_base <- function(plan, token) {
  if (is.null(plan$catalog_record_id)) return(invisible(TRUE))
  state <- .publication_state(plan)
  latest <- .zenodo_request("GET", paste0("records/", plan$catalog_record_id, "/versions/latest"), token, plan$sandbox)
  expected <- state$versions$catalog$record_id
  if (!identical(as.character(latest$id), as.character(plan$catalog_record_id)) &&
      (is.null(expected) || !identical(as.character(latest$id), as.character(expected))))
    stop("The catalog family advanced beyond this plan; reconcile it before any storage writes.", call. = FALSE)
  drafts <- .family_drafts(plan, as.character(latest$parent$id), token)
  if (length(drafts) > 1L) stop("Multiple catalog drafts require reconciliation.", call. = FALSE)
  if (length(drafts) && !identical(as.character(drafts[[1]]$id), as.character(expected)) &&
      !isTRUE(state$versions$catalog$creating) &&
      !identical(drafts[[1]]$metadata$title, paste0("PublicationBiasBenchmark release ", plan$catalog$release)))
    stop("An unrelated catalog draft exists; reconcile it before any storage writes.", call. = FALSE)
  invisible(TRUE)
}
.stage_archive_release <- function(plan, token, community_id) {
  .reconcile_catalog_base(plan, token)
  .resolve_publication_community(plan, community_id)
  catalog <- plan$catalog
  for (dgm in plan$changed_dgms) {
    key <- paste0("storage--", dgm); base_id <- .storage_base_id(plan, dgm)
    related <- if (!is.null(plan$catalog_concept_doi)) list(.doi_relationship(plan$catalog_concept_doi, "ispartof")) else list()
    title <- paste0("PublicationBiasBenchmark: ", dgm, " storage (", catalog$release, ")")
    description <- "Benchmark storage snapshot. Download datasets by DGM and results or measures by method/setting using PublicationBiasBenchmark. Cite the exact benchmark release catalog version DOI. Original source provenance and generation versions are preserved in that catalog."
    metadata <- .record_metadata(plan, title, description, related)
    metadata$subjects <- unname(c(metadata$subjects, list(list(subject = dgm), list(subject = "benchmark-storage"))))
    id <- .ensure_family_version(plan, key, base_id, title, description, metadata, token)
    archives <- Filter(function(x) identical(x$dgm, dgm), catalog$archives)
    if (!.record_is_published(plan, id, token)) {
      .update_draft_metadata(plan, id, metadata, token)
      .ensure_imported_files(plan, key, id, base_id, archives, token)
      .stage_files(plan, id, Filter(function(x) !is.null(x$local_path), archives), token)
    }
    catalog$archives <- lapply(catalog$archives, function(x) { if (x$dgm == dgm) x$record_id <- id; x })
    catalog$assets <- lapply(catalog$assets, function(x) { if (x$dgm == dgm) x$record_id <- id; x })
  }
  .write_release_catalog(plan, catalog)
  .public_catalog(catalog)
}
.verify_relationship <- function(record, doi, relation) {
  if (!any(vapply(record$metadata$related_identifiers, function(x)
    identical(x$identifier, doi) && identical(x$relation_type$id, relation), logical(1))))
    stop("Zenodo record ", record$id, " did not retain ", relation, " relationship to ", doi, ".", call. = FALSE)
  invisible(TRUE)
}
# Identifying fields of catalog entries, one string each, sorted. Sizes are
# formatted as plain numbers so that a parsed catalog and a plan agree.
.entry_signatures <- function(items, fields, members = FALSE) {
  one <- function(x) {
    values <- vapply(fields, function(field) {
      value <- unlist(x[[field]])
      if (is.numeric(value)) format(value, scientific = FALSE, trim = TRUE) else paste(as.character(value), collapse = ",")
    }, character(1))
    parts <- paste(values, collapse = "|")
    if (members) parts <- paste(parts, paste(vapply(x$members, function(m) paste(m$id, m$sha256, sep = ":"), character(1)), collapse = ","), sep = "|")
    parts
  }
  sort(vapply(items, one, character(1)))
}

# The staged release.json must describe the release and the files of this plan
# (record IDs differ between a plan and its staged catalog, nothing else may).
.check_staged_catalog <- function(plan, catalog) {
  if (!identical(catalog$release, plan$catalog$release))
    stop("The staged release.json belongs to release '", catalog$release, "', not to this plan's release '",
         plan$catalog$release, "'; use the state directory of this plan.", call. = FALSE)
  if (identical(plan$catalog$schema_version, 2L)) {
    fields <- c("id", "dgm", "filename", "sha256", "md5", "size")
    if (!identical(.entry_signatures(catalog$archives, fields, TRUE), .entry_signatures(plan$catalog$archives, fields, TRUE)))
      stop("The staged release.json has a different archive inventory than this plan; re-stage it or use the state directory of this plan.", call. = FALSE)
  } else {
    fields <- c("id", "dgm", "kind", "filename", "sha256", "md5", "size")
    if (!identical(.entry_signatures(catalog$assets, fields), .entry_signatures(plan$catalog$assets, fields)))
      stop("The staged release.json lists different files than this plan; re-stage it or use the state directory of this plan.", call. = FALSE)
  }
  invisible(TRUE)
}
# State is read and the staged catalog checked before any request is made.
.verify_archive_release <- function(plan, token = NULL) {
  token <- .publication_token(plan, token)
  .publication_state(plan, "read")
  path <- file.path(plan$state_directory, "release.json")
  if (!file.exists(path)) stop("The release has not been fully staged.", call. = FALSE)
  catalog <- benchmark_catalog(path)
  .check_staged_catalog(plan, catalog)
  for (dgm in unique(vapply(catalog$archives, `[[`, character(1), "dgm"))) {
    archives <- Filter(function(x) x$dgm == dgm, catalog$archives); id <- archives[[1]]$record_id
    published <- .record_is_published(plan, id, token)
    record <- .zenodo_request("GET", paste0("records/", id, if (!published) "/draft"), token, plan$sandbox)
    .verify_record_rights(record, plan$metadata)
    if (!is.null(plan$catalog_concept_doi)) .verify_relationship(record, plan$catalog_concept_doi, "ispartof")
    files <- .zenodo_request("GET", paste0("records/", id, if (!published) "/draft", "/files"), token, plan$sandbox)
    if (!setequal(vapply(files$entries, `[[`, character(1), "key"), vapply(archives, `[[`, character(1), "filename")) ||
        !all(vapply(archives, function(a) .zenodo_entry_verified(.zenodo_file_entry(files, a$filename), a), logical(1))))
      stop("Storage snapshot does not match the complete planned archive inventory.", call. = FALSE)
  }
  invisible(TRUE)
}
# Anonymous check that a storage record is public: records, files and no embargo.
# A just-published record may need a moment to become visible (403/404).
.verify_public_record <- function(record_id, sandbox, attempts = 6L) {
  for (attempt in seq_len(attempts)) {
    record <- tryCatch(.zenodo_request("GET", paste0("records/", record_id), NULL, sandbox),
                       zenodo_http_error = function(error) error)
    if (!inherits(record, "zenodo_http_error")) break
    if (!isTRUE(record$status %in% c(403L, 404L)) || attempt == attempts)
      stop("Record ", record_id, " is not publicly readable without a token (HTTP ", record$status, ").", call. = FALSE)
    .resource_retry_wait(min(30, 2^(attempt - 1L)))
  }
  if (!identical(record$access$record, "public") || !identical(record$access$files, "public") ||
      isTRUE(record$access$embargo$active))
    stop("Record ", record_id, " is not fully public: records and files must be public without an embargo.", call. = FALSE)
  invisible(TRUE)
}

# One anonymous request for the first byte of a public file. The body is read
# only up to `limit` bytes and the transfer is then aborted, so a server that
# ignores the Range header cannot make the verification download the file.
# Returns list(status, headers, bytes read); failures below HTTP level raise the curl error.
.resource_range_probe <- function(url, limit = 65536) {
  handle <- curl::new_handle(connecttimeout = 30, low_speed_limit = 1, low_speed_time = 180, followlocation = TRUE)
  curl::handle_setheaders(handle, Range = "bytes=0-0")
  connection <- curl::curl(url, handle = handle)
  on.exit(try(close(connection), silent = TRUE), add = TRUE)
  opened <- tryCatch(suppressWarnings({ open(connection, "rb"); TRUE }), error = function(error) FALSE)
  response <- curl::handle_data(handle)
  if (!opened && !response$status_code) {
    # No HTTP answer: repeat the connection without a body to get libcurl's classed error.
    probe <- curl::new_handle(connecttimeout = 30, nobody = TRUE, followlocation = TRUE)
    stop(tryCatch({ curl::curl_fetch_memory(url, probe); simpleError("The public file could not be opened.") },
                  error = function(error) error))
  }
  body <- if (opened) readBin(connection, "raw", limit + 1L) else raw()
  list(status = response$status_code, headers = curl::parse_headers_list(response$headers), bytes = length(body))
}

# Verify that an advertised archive is publicly served with the catalog size
# without downloading it: HTTP 206 with the total in Content-Range, or HTTP 200
# with Content-Length. When the server sends neither, the archive is fetched in
# full. 403/404 are retried briefly (visibility grace); 410 and other client
# errors are permanent; 429 waits for the server delay.
.verify_public_archive_size <- function(plan, archive, directory, max_try = 6L, retry_not_found = 5L) {
  url <- .zenodo_file_url(archive$record_id, archive$filename, plan$sandbox)
  for (attempt in seq_len(max_try)) {
    condition <- NULL
    probe <- tryCatch(.resource_range_probe(url), error = function(error) { condition <<- error; NULL })
    if (!is.null(probe) && probe$status %in% c(200L, 206L)) {
      total <- if (probe$status == 206L) {
        range <- probe$headers[["content-range"]]
        if (is.character(range) && length(range) == 1L && grepl("^bytes 0-0/[0-9]+$", range))
          as.numeric(sub("^bytes 0-0/", "", range)) else if (is.null(range)) NULL else NA_real_
      } else if (!is.null(probe$headers[["content-length"]])) suppressWarnings(as.numeric(probe$headers[["content-length"]])) else NULL
      if (is.null(total)) {
        message("The public server reported no size for ", archive$filename, "; downloading it in full to verify it.")
        .fetch_verified(url, file.path(directory, archive$filename), archive$sha256, archive$size, archive$md5,
                        progress = FALSE, max_try = max_try, overwrite = TRUE, retry_not_found = retry_not_found)
        return(invisible(TRUE))
      }
      if (is.na(total) || total != archive$size)
        stop("The public archive ", archive$filename, " does not have the catalog size (", archive$size, " bytes).", call. = FALSE)
      return(invisible(TRUE))
    }
    if (!is.null(probe)) {
      delay <- if (probe$status == 429L) .zenodo_retry_delay(probe$headers, 429L, attempt) else NULL
      condition <- structure(list(message = paste0("Public resource download failed (HTTP ", probe$status, ")."),
        call = NULL, status = probe$status, retry_delay = delay), class = c("resource_http_error", "error", "condition"))
    }
    class <- .classify_download_failure(condition, attempt, retry_not_found)
    if (class == "permanent" || (class == "dns" && (attempt >= 3L || attempt >= max_try)))
      stop(.download_failure_message(class, condition, archive$filename, url), call. = FALSE)
    if (attempt < max_try)
      .resource_retry_wait(if (class == "rate_limited" && is.numeric(condition$retry_delay)) condition$retry_delay else min(30, 2^(attempt - 1L)))
  }
  if (inherits(condition, "resource_http_error") && condition$status %in% c(404L, 410L))
    stop(.download_failure_message("permanent", condition, archive$filename, url), call. = FALSE)
  stop("Could not verify the public archive ", archive$filename, " after ", max_try, " attempts",
       if (is.null(condition)) "." else paste0(" (last error: ", sub("[.]$", "", conditionMessage(condition)), ")."), call. = FALSE)
}

# Every advertised archive is checked anonymously (record access, then size);
# only archives uploaded by this plan are downloaded and verified in full,
# including every member.
.verify_public_archives <- function(plan, catalog) {
  directory <- file.path(plan$state_directory, "public-verification"); dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  for (id in unique(vapply(catalog$archives, `[[`, character(1), "record_id")))
    .verify_public_record(id, plan$sandbox)
  uploaded <- unlist(lapply(plan$groups, function(group)
    vapply(group, function(x) paste(x$dgm, x$filename, sep = "/"), character(1))), use.names = FALSE)
  skipped <- 0L
  for (archive in catalog$archives) {
    .verify_public_archive_size(plan, archive, directory)
    if (!paste(archive$dgm, archive$filename, sep = "/") %in% uploaded) { skipped <- skipped + 1L; next }
    path <- file.path(directory, archive$filename)
    .fetch_verified(.zenodo_file_url(archive$record_id, archive$filename, plan$sandbox), path,
      archive$sha256, archive$size, archive$md5, progress = FALSE, max_try = 6L, overwrite = TRUE, retry_not_found = 5L)
    .zip_inventory(path, archive$members)
    # Verify every member, not only those needed by a sample reader selection.
    temporary <- tempfile("verify-members-", tmpdir = directory); dir.create(temporary)
    tryCatch(for (member in archive$members) {
      utils::unzip(path, files = member$filename, junkpaths = TRUE, exdir = temporary, unzip = "internal")
      extracted <- file.path(temporary, member$filename)
      if (!.file_verified(extracted, member$sha256, member$size, member$md5)) stop("Public member failed verification: ", member$filename, call. = FALSE)
      unlink(extracted)
    }, finally = unlink(temporary, recursive = TRUE))
  }
  if (skipped) message(skipped, " of ", length(catalog$archives),
    " archives were not uploaded by this plan; their public access and size were checked without downloading them.")
  invisible(TRUE)
}
# The "Release catalog family" sentence is added once, however often a
# resumed publication reaches this step (keyed on the concept DOI link).
.with_family_sentence <- function(description, concept) {
  link <- paste0("https://doi.org/", concept)
  if (isTRUE(grepl(link, description, fixed = TRUE))) return(description)
  paste0(description, ' Release catalog family: <a href="', link,
         '">PublicationBiasBenchmark releases</a>. Open the release used in your analysis and cite its exact catalog version DOI.')
}
.publish_archive_release <- function(plan, token, community_id) {
  catalog <- .stage_archive_release(plan, token, community_id)
  .verify_archive_release(plan, token)
  .resolve_publication_community(plan, community_id)
  # For a brand-new benchmark, create the catalog draft first to learn its concept
  # DOI. Consolidation instead uses the already-published baseline concept DOI.
  title <- paste0("PublicationBiasBenchmark release ", catalog$release)
  description <- "Complete cumulative benchmark release catalog. Use this version DOI for reproducible analyses; the concept DOI resolves to the latest release. Independently downloadable datasets, results and measures are listed with exact hashes and generation provenance."
  state <- .publication_state(plan)
  if (is.null(plan$catalog_record_id) && is.null(state$versions$catalog$record_id)) {
    .ensure_family_version(plan, "catalog", NULL, title, description, .record_metadata(plan, title, description), token)
    state <- .publication_state(plan)
  }
  concept <- plan$catalog_concept_doi
  if (is.null(concept)) {
    catalog_id <- state$versions$catalog$record_id
    draft <- .zenodo_get_optional(paste0("records/", catalog_id, "/draft"), plan, token)
    if (is.null(draft)) draft <- .zenodo_request("GET", paste0("records/", catalog_id), token, plan$sandbox)
    if (!length(draft$pids$doi)) {
      .zenodo_request("POST", .zenodo_link_path(draft$links$reserve_doi, plan$sandbox), token, plan$sandbox)
      draft <- .zenodo_request("GET", paste0("records/", catalog_id, "/draft"), token, plan$sandbox)
    }
    concept <- draft$parent$pids$doi$identifier
    if (is.null(concept)) {
      # Zenodo reserves the version DOI before publication but exposes the
      # parent's DOI only afterwards. Its managed DOI suffix is the parent PID.
      # Check the returned reservation scheme before deriving it, and verify
      # the actual published concept DOI below. Existing families never need this.
      prefix <- if (plan$sandbox) "10.5072/zenodo." else "10.5281/zenodo."
      if (!identical(draft$pids$doi$identifier, paste0(prefix, draft$id)) ||
          !grepl("^[0-9]+$", as.character(draft$parent$id))) stop("Unsupported managed DOI reservation scheme.", call. = FALSE)
      concept <- paste0(prefix, draft$parent$id)
    }
  }
  storage_dois <- list()
  for (dgm in unique(vapply(catalog$archives, `[[`, character(1), "dgm"))) {
    archives <- Filter(function(x) x$dgm == dgm, catalog$archives); id <- archives[[1]]$record_id
    if (!.record_is_published(plan, id, token)) {
      draft <- .zenodo_request("GET", paste0("records/", id, "/draft"), token, plan$sandbox)
      metadata <- draft$metadata
      metadata$description <- .with_family_sentence(metadata$description, concept)
      metadata$related_identifiers <- c(metadata$related_identifiers, list(.doi_relationship(concept, "ispartof")))
      .update_draft_metadata(plan, id, metadata, token)
      .publish_record(plan, id, token)
    }
    .include_record_community(plan, id, community_id, token)
    key <- paste0("storage--", dgm)
    .update_publication_state(plan, function(state) {
      state$versions[[key]]$record_id <- id; state$versions[[key]]$published <- TRUE; state
    })
    record <- .zenodo_request("GET", paste0("records/", id), token, plan$sandbox)
    .verify_record_rights(record, plan$metadata); .verify_relationship(record, concept, "ispartof")
    .update_publication_state(plan, function(state) {
      state$versions[[key]]$family_id <- as.character(record$parent$id)
      state$versions[[key]]$metadata_complete <- TRUE; state
    })
    storage_dois[[dgm]] <- .zenodo_doi(record)
  }
  .verify_public_archives(plan, catalog)
  # Include the existing catalog family before creating a new version so that
  # its membership and branding are inherited without draft community review.
  if (!is.null(plan$catalog_record_id)) .include_record_community(plan, plan$catalog_record_id, community_id, token)
  relationships <- lapply(storage_dois, .doi_relationship, relation = "haspart")
  id <- .ensure_family_version(plan, "catalog", plan$catalog_record_id, title, description,
    .record_metadata(plan, title, description, relationships), token)
  catalog$publication <- list(catalog_record_id = id, catalog_concept_doi = concept, community_id = community_id, storage_dois = storage_dois)
  .write_release_catalog(plan, catalog)
  # The verified file on disk is what gets hashed and uploaded.
  path <- file.path(plan$state_directory, "release.json")
  asset <- list(filename = "release.json", local_path = path, size = as.numeric(file.info(path)$size),
    sha256 = digest::digest(file = path, algo = "sha256", serialize = FALSE), md5 = unname(tools::md5sum(path)))
  if (!.record_is_published(plan, id, token)) {
    .update_draft_metadata(plan, id, .record_metadata(plan, title, description, relationships), token)
    .stage_file(plan, id, asset, token); .publish_record(plan, id, token)
  }
  .include_record_community(plan, id, community_id, token)
  .update_publication_state(plan, function(state) { state$versions$catalog$published <- TRUE; state })
  record <- .zenodo_request("GET", paste0("records/", id), token, plan$sandbox)
  if (!identical(.zenodo_concept_doi(record), concept)) stop("Published concept DOI differs from the catalog family.", call. = FALSE)
  .update_publication_state(plan, function(state) {
    state$versions$catalog$family_id <- as.character(record$parent$id)
    state$versions$catalog$metadata_complete <- TRUE; state
  })
  .verify_record_rights(record, plan$metadata)
  for (doi in storage_dois) .verify_relationship(record, doi, "haspart")
  result <- list(release = catalog$release, record_id = id, catalog_sha256 = asset$sha256, doi = .zenodo_doi(record), concept_doi = concept)
  .fetch_verified(.zenodo_file_url(id, "release.json", plan$sandbox), file.path(plan$state_directory, "public-verification", "release.json"),
    asset$sha256, asset$size, asset$md5, progress = FALSE, max_try = 6L, retry_not_found = 5L)
  .write_json_verified(file.path(plan$state_directory, "registry-entry.json"), result, pretty = TRUE,
                       history_dir = file.path(plan$state_directory, "catalog-history"))
  result
}
