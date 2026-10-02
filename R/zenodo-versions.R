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
  state <- .publication_state(plan)
  slot <- state$versions[[key]]
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
        state$versions[[key]] <- list(creating = TRUE, base_id = base_id)
        .save_publication_state(plan, state)
        draft <- .zenodo_request("POST", paste0("records/", base_id, "/versions"), token, plan$sandbox)
        id <- as.character(draft$id)
      }
    }
  }
  state <- .publication_state(plan)
  state$versions[[key]] <- list(record_id = id, base_id = base_id, published = .record_is_published(plan, id, token))
  .save_publication_state(plan, state)
  if (!isTRUE(state$versions[[key]]$published)) .update_draft_metadata(plan, id, metadata, token)
  id
}
.ensure_imported_files <- function(plan, key, id, base_id, archives, token) {
  if (is.null(base_id)) return(invisible(TRUE))
  state <- .publication_state(plan)
  files <- .zenodo_request("GET", paste0("records/", id, "/draft/files"), token, plan$sandbox)
  base_files <- .zenodo_request("GET", paste0("records/", base_id, "/files"), token, plan$sandbox)
  if (!isTRUE(state$versions[[key]]$imported)) {
    expected <- base_files$entries
    present <- vapply(expected, function(x) {
      entry <- .zenodo_file_entry(files, x$key)
      !is.null(entry) && identical(entry$checksum, x$checksum) && identical(as.numeric(entry$size), as.numeric(x$size))
    }, logical(1))
    if (!all(present)) {
      if (length(files$entries)) stop("Incomplete or unrelated draft imports; refusing to reset uploaded files.", call. = FALSE)
      .zenodo_request("POST", paste0("records/", id, "/draft/actions/files-import"), token, plan$sandbox)
    }
    state$versions[[key]]$imported <- TRUE; .save_publication_state(plan, state)
  }
  keep <- vapply(archives, `[[`, character(1), "filename")
  files <- .zenodo_request("GET", paste0("records/", id, "/draft/files"), token, plan$sandbox)
  for (entry in files$entries) if (!entry$key %in% keep) {
    # Delete only files imported from the base snapshot, never unknown draft data.
    if (is.null(.zenodo_file_entry(base_files, entry$key))) stop("Unexpected file in new-version draft.", call. = FALSE)
    .zenodo_request("DELETE", paste0("records/", id, "/draft/files/", utils::URLencode(entry$key, reserved = TRUE)), token, plan$sandbox)
    state$versions[[key]]$deleted <- as.list(unique(c(unlist(state$versions[[key]]$deleted), entry$key)))
    .save_publication_state(plan, state)
  }
  invisible(TRUE)
}
.resolve_publication_community <- function(plan, token) {
  state <- .publication_state(plan)
  community <- .zenodo_request("GET", paste0("communities/", utils::URLencode(plan$community, reserved = TRUE)), token, plan$sandbox)
  if (!.scalar_string(community$id)) stop("Community resolution did not return a UUID.", call. = FALSE)
  if (!is.null(state$community_id) && !identical(state$community_id, community$id)) stop("Resolved community identity changed.", call. = FALSE)
  state$community_id <- community$id; .save_publication_state(plan, state)
  community$id
}
.community_record_verified <- function(record, community_id) {
  community_id %in% unlist(record$parent$communities$ids) && identical(record$parent$communities$default, community_id)
}
.include_record_community <- function(plan, id, community_id, token) {
  record <- .zenodo_request("GET", paste0("records/", id), token, plan$sandbox)
  if (.community_record_verified(record, community_id)) {
    state <- .publication_state(plan)
    state$inclusions[[as.character(record$parent$id)]] <- list(community_id = community_id, accepted = TRUE)
    .save_publication_state(plan, state)
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
    state <- .publication_state(plan)
    family <- as.character(record$parent$id)
    state$inclusions[[family]] <- list(request_id = request$id, community_id = community_id, accepted = identical(request$status, "accepted"))
    .save_publication_state(plan, state)
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
  state <- .publication_state(plan)
  state$inclusions[[as.character(record$parent$id)]] <- list(community_id = community_id, accepted = TRUE)
  .save_publication_state(plan, state)
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
.write_release_catalog <- function(plan, catalog) {
  jsonlite::write_json(.public_catalog(catalog), file.path(plan$state_directory, "release.json"),
    auto_unbox = TRUE, pretty = FALSE, null = "null", digits = NA, dataframe = "rows")
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
.stage_archive_release <- function(plan, token = NULL) {
  token <- .publication_token(plan, token)
  .reconcile_catalog_base(plan, token)
  .resolve_publication_community(plan, token)
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
.verify_archive_release <- function(plan, token = NULL) {
  token <- .publication_token(plan, token)
  path <- file.path(plan$state_directory, "release.json")
  if (!file.exists(path)) stop("The release has not been fully staged.", call. = FALSE)
  catalog <- benchmark_catalog(path)
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
.verify_public_archives <- function(plan, catalog) {
  directory <- file.path(plan$state_directory, "public-verification"); dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  for (archive in catalog$archives) {
    path <- file.path(directory, archive$filename)
    .fetch_verified(.zenodo_file_url(archive$record_id, archive$filename, plan$sandbox), path,
      archive$sha256, archive$size, archive$md5, progress = FALSE, max_try = 3, retry_not_found = 5L)
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
  invisible(TRUE)
}
.publish_archive_release <- function(plan, token = NULL) {
  token <- .publication_token(plan, token)
  catalog <- .stage_archive_release(plan, token)
  .verify_archive_release(plan, token)
  community_id <- .resolve_publication_community(plan, token)
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
      draft <- .zenodo_request("GET", paste0("records/", state$versions$catalog$record_id, "/draft"), token, plan$sandbox)
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
      metadata$description <- paste0(metadata$description,
        ' Release catalog family: <a href="https://doi.org/', concept,
        '">PublicationBiasBenchmark releases</a>. Open the release used in your analysis and cite its exact catalog version DOI.')
      metadata$related_identifiers <- c(metadata$related_identifiers, list(.doi_relationship(concept, "ispartof")))
      .update_draft_metadata(plan, id, metadata, token)
      .publish_record(plan, id, token)
    }
    .include_record_community(plan, id, community_id, token)
    state <- .publication_state(plan)
    key <- paste0("storage--", dgm)
    state$versions[[key]]$record_id <- id; state$versions[[key]]$published <- TRUE
    .save_publication_state(plan, state)
    record <- .zenodo_request("GET", paste0("records/", id), token, plan$sandbox)
    .verify_record_rights(record, plan$metadata); .verify_relationship(record, concept, "ispartof")
    state <- .publication_state(plan)
    state$versions[[key]]$family_id <- as.character(record$parent$id)
    state$versions[[key]]$metadata_complete <- TRUE; .save_publication_state(plan, state)
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
  path <- file.path(plan$state_directory, "release.json")
  asset <- list(filename = "release.json", local_path = path, size = as.numeric(file.info(path)$size),
    sha256 = digest::digest(file = path, algo = "sha256", serialize = FALSE), md5 = unname(tools::md5sum(path)))
  if (!.record_is_published(plan, id, token)) {
    .update_draft_metadata(plan, id, .record_metadata(plan, title, description, relationships), token)
    .stage_file(plan, id, asset, token); .publish_record(plan, id, token)
  }
  .include_record_community(plan, id, community_id, token)
  state <- .publication_state(plan)
  state$versions$catalog$published <- TRUE; .save_publication_state(plan, state)
  record <- .zenodo_request("GET", paste0("records/", id), token, plan$sandbox)
  if (!identical(.zenodo_concept_doi(record), concept)) stop("Published concept DOI differs from the catalog family.", call. = FALSE)
  state <- .publication_state(plan)
  state$versions$catalog$family_id <- as.character(record$parent$id)
  state$versions$catalog$metadata_complete <- TRUE; .save_publication_state(plan, state)
  .verify_record_rights(record, plan$metadata)
  for (doi in storage_dois) .verify_relationship(record, doi, "haspart")
  result <- list(release = catalog$release, record_id = id, catalog_sha256 = asset$sha256, doi = .zenodo_doi(record), concept_doi = concept)
  .fetch_verified(.zenodo_file_url(id, "release.json", plan$sandbox), file.path(plan$state_directory, "public-verification", "release.json"),
    asset$sha256, asset$size, asset$md5, progress = FALSE, max_try = 3, retry_not_found = 5L)
  jsonlite::write_json(result, file.path(plan$state_directory, "registry-entry.json"), auto_unbox = TRUE, pretty = TRUE)
  result
}
