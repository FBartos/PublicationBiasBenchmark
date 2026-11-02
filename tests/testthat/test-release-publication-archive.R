# A small stateful Zenodo for archive publication of a first release (no earlier
# version): records with drafts and files, DOI reservation, community inclusion and
# public downloads. It lets the whole staging/publishing path run without mocking
# any publication step. `lose_publish` throws before a publish takes effect (a failed request);
# `lose_after` names events whose effect is applied before the response is lost:
# storage_publish, catalog_publish, storage_inclusion, catalog_inclusion. Uploads are snapshots: the
# bytes at upload time are checksummed and served afterwards, whatever happens to the local file.
fake_zenodo <- function(lose_publish = FALSE, lose_commit = FALSE, lose_after = character(), env = parent.frame()) {
  zenodo <- new.env()
  zenodo$records <- list(); zenodo$next_id <- 100L; zenodo$uploaded <- list(); zenodo$requests <- character()
  zenodo$upload_log <- character()
  zenodo$lose_after <- lose_after; zenodo$lost <- character()
  zenodo$inclusions <- list(); zenodo$submissions <- character()
  zenodo$lose_publish_once <- lose_publish; zenodo$publish_lost <- 0L; zenodo$lose_commit_once <- lose_commit
  record <- function(id) zenodo$records[[id]]
  view <- function(r, draft = FALSE) list(id = r$id, metadata = r$metadata, access = r$access,
    is_published = r$published,
    links = list(reserve_doi = paste0("https://zenodo.org/api/records/", r$id, "/draft/pids/doi")),
    pids = if (r$published || r$reserved) list(doi = list(identifier = paste0("10.5281/zenodo.", r$id))) else list(),
    parent = list(id = r$parent, communities = list(ids = as.list(r$communities), default = r$default),
                  pids = if (r$published) list(doi = list(identifier = paste0("10.5281/zenodo.", r$parent)))))
  kind <- function(id) if (grepl("storage", zenodo$records[[id]]$metadata$title)) "storage" else "catalog"
  lose <- function(event) if (event %in% zenodo$lose_after) {
    zenodo$lose_after <- setdiff(zenodo$lose_after, event); zenodo$lost <- c(zenodo$lost, event)
    stop("lost ", event, " response")
  }
  request_view <- function(q) list(id = q$id, type = "community-inclusion", receiver = list(community = q$community), status = q$status)
  zenodo$handler <- function(method, path, token, sandbox = FALSE, body = NULL) {
    zenodo$requests <- c(zenodo$requests, paste(method, path))
    parts <- strsplit(sub("[?].*$", "", path), "/", fixed = TRUE)[[1]]
    if (identical(parts[1], "requests") && identical(parts[3:4], c("actions", "accept"))) {
      q <- zenodo$inclusions[[parts[2]]]
      zenodo$inclusions[[parts[2]]]$status <- "accepted"
      zenodo$records[[q$record]]$communities <- unique(c(zenodo$records[[q$record]]$communities, q$community))
      return(NULL)
    }
    if (method == "GET" && startsWith(path, "user/records?")) {
      title <- sub('^.*metadata[.]title:"(.*)"&size.*$', "\\1", utils::URLdecode(path))
      hits <- Filter(function(r) identical(r$metadata$title, title), zenodo$records)
      return(list(hits = list(hits = unname(lapply(hits, view)))))
    }
    if (method == "POST" && identical(parts, "records")) {
      id <- as.character(zenodo$next_id); zenodo$next_id <- zenodo$next_id + 1L
      zenodo$records[[id]] <- list(id = id, metadata = body$metadata, access = body$access, published = FALSE, reserved = FALSE,
        files = list(), parent = as.character(900L + length(zenodo$records)), communities = character(), default = NULL, created = 0L)
      return(view(zenodo$records[[id]]))
    }
    id <- parts[2]; r <- record(id)
    if (is.null(r)) stop(zenodo_error(404L))
    rest <- parts[-(1:2)]
    if (method == "GET" && !length(rest)) {
      if (!r$published) stop(zenodo_error(404L))
      return(view(r))
    }
    if (identical(rest, "draft")) {
      if (r$published) stop(zenodo_error(404L))
      if (method == "GET") return(view(r, TRUE))
      zenodo$records[[id]]$metadata <- body$metadata; zenodo$records[[id]]$access <- body$access
      return(list(metadata = body$metadata))
    }
    if (identical(rest, c("draft", "files")) || identical(rest, "files")) {
      if (method == "GET") return(list(entries = unname(r$files)))
      for (file in body) {
        zenodo$records[[id]]$created <- zenodo$records[[id]]$created + 1L
        zenodo$records[[id]]$files[[file$key]] <- list(key = file$key, status = "pending",
          created = paste0("created-", id, "-", zenodo$records[[id]]$created))
      }
      return(list(entries = unname(zenodo$records[[id]]$files)))
    }
    if (length(rest) == 4L && identical(rest[c(1, 2, 4)], c("draft", "files", "commit"))) {
      key <- utils::URLdecode(rest[3])
      file <- zenodo$records[[id]]$files[[key]]
      bytes <- zenodo$uploaded[[paste(id, key)]]
      zenodo$records[[id]]$files[[key]] <- modifyList(file, list(status = "completed", size = length(bytes),
        checksum = paste0("md5:", digest::digest(bytes, algo = "md5", serialize = FALSE))))
      # The commit took effect, but its response is lost.
      if (zenodo$lose_commit_once) { zenodo$lose_commit_once <- FALSE; stop("lost commit response") }
      return(zenodo$records[[id]]$files[[key]])
    }
    if (identical(rest, c("draft", "pids", "doi"))) { zenodo$records[[id]]$reserved <- TRUE; return(NULL) }
    if (identical(rest, c("draft", "actions", "publish"))) {
      if (zenodo$lose_publish_once) { zenodo$lose_publish_once <- FALSE; zenodo$publish_lost <- zenodo$publish_lost + 1L; stop("lost publish response") }
      zenodo$records[[id]]$published <- TRUE
      lose(paste0(kind(id), "_publish"))
      return(NULL)
    }
    if (identical(rest, "requests"))
      return(list(hits = list(hits = unname(lapply(Filter(function(q) identical(q$record, id), zenodo$inclusions), request_view)))))
    if (identical(rest, "communities")) {
      if (method == "POST") {
        # A submission creates a request that awaits its acceptance; the record joins the community then.
        rid <- paste0("request-", length(zenodo$inclusions) + 1L)
        zenodo$inclusions[[rid]] <- list(id = rid, record = id, community = body$communities[[1]]$id, status = "submitted")
        zenodo$submissions <- c(zenodo$submissions, id)
        lose(paste0(kind(id), "_inclusion"))
        return(list(processed = list(list(request = request_view(zenodo$inclusions[[rid]])))))
      }
      zenodo$records[[id]]$default <- body$default$id
      return(NULL)
    }
    stop("Unexpected request: ", method, " ", path)
  }
  zenodo$upload <- function(path, record_id, filename, ...) {
    zenodo$upload_log <- c(zenodo$upload_log, paste(record_id, filename))
    zenodo$uploaded[[paste(record_id, filename)]] <- readBin(path, "raw", n = file.info(path)$size)
  }
  zenodo$download <- function(url, destination, progress) {
    parts <- strsplit(sub("^https://zenodo.org/api/", "", url), "/", fixed = TRUE)[[1]]
    bytes <- zenodo$uploaded[[paste(parts[2], utils::URLdecode(parts[4]))]]
    if (is.null(bytes)) stop(http_failure(404L))
    writeBin(bytes, destination)
    TRUE
  }
  zenodo$probe <- function(url, limit = 65536) {
    parts <- strsplit(sub("^https://zenodo.org/api/", "", url), "/", fixed = TRUE)[[1]]
    bytes <- zenodo$uploaded[[paste(parts[2], utils::URLdecode(parts[4]))]]
    if (is.null(bytes)) return(list(status = 404L, headers = list(), bytes = 0L))
    list(status = 206L, headers = list(`content-range` = paste0("bytes 0-0/", length(bytes))), bytes = 1L)
  }
  testthat::local_mocked_bindings(.zenodo_request = zenodo$handler, .zenodo_upload = zenodo$upload,
    .resource_download = zenodo$download, .resource_range_probe = zenodo$probe, .resource_retry_wait = function(delay) NULL, .env = env)
  zenodo
}

archive_release_plan <- function(root) {
  a <- test_resource(root, "A.csv"); b <- test_resource(root, "B.csv", method = "B")
  plan_benchmark_release("test.1", list(a, b), conditions = test_catalog(list(a))$conditions,
    metadata = list(rights = list(list(id = "cc-by-4.0"))), state_directory = file.path(root, "state"),
    community = "publicationbiasbenchmark")
}

test_that("an archive release publishes end to end inside one session and resumes after a lost response", {
  skip_if_not_installed("zip")
  root <- withr::local_tempdir(); plan <- archive_release_plan(root)
  gate <- local_mock_gate(community_id = "community-uuid")
  zenodo <- fake_zenodo(lose_publish = TRUE)
  # Staging alone leaves drafts and a staged catalog; nothing is published.
  staged <- suppressMessages(stage_benchmark_release(plan, "token"))
  expect_equal(length(gate$calls), 1L); expect_false(any(vapply(zenodo$records, `[[`, logical(1), "published")))
  expect_true(file.exists(file.path(plan$state_directory, "release.json")))
  expect_length(grep("actions/publish", zenodo$requests), 0L)
  expect_true(suppressMessages(verify_benchmark_release(plan, "token")))
  # Publishing runs staging, verification and the rest inside its single session (no second lock).
  expect_error(suppressMessages(publish_benchmark_release(plan, "token", confirm = "test.1")), "lost publish response")
  expect_false(dir.exists(file.path(plan$state_directory, ".lock")))
  result <- suppressMessages(publish_benchmark_release(plan, "token", confirm = "test.1"))
  expect_identical(zenodo$publish_lost, 1L)
  expect_identical(result$release, "test.1")
  expect_match(result$doi, "^10[.]5281/zenodo[.][0-9]+$"); expect_match(result$concept_doi, "^10[.]5281/zenodo[.]9[0-9]+$")
  published <- Filter(function(r) r$published, zenodo$records)
  expect_length(published, 2L)
  for (r in published) {
    expect_identical(r$communities, "community-uuid"); expect_identical(r$default, "community-uuid")
  }
  # The resumed run did not repeat the family sentence of the interrupted one.
  storage <- Filter(function(r) grepl("storage", r$metadata$title), zenodo$records)[[1]]
  expect_length(gregexpr("Release catalog family", storage$metadata$description, fixed = TRUE)[[1]], 1L)
  expect_match(storage$metadata$description, result$concept_doi, fixed = TRUE)
  # The catalog on disk carries the publication block and the registry entry names it.
  catalog <- benchmark_catalog(file.path(plan$state_directory, "release.json"))
  expect_identical(catalog$publication$catalog_concept_doi, result$concept_doi)
  expect_identical(catalog$publication$community_id, "community-uuid")
  expect_identical(jsonlite::read_json(file.path(plan$state_directory, "registry-entry.json"))$doi, result$doi)
  state <- .publication_state(plan)
  expect_true(.identity_equal(state$identity, plan$identity))
  expect_true(isTRUE(state$versions$catalog$published)); expect_true(isTRUE(state$versions[["storage--no_bias"]]$published))
  expect_identical(state$community_id, "community-uuid")
  expect_gte(length(list.files(file.path(plan$state_directory, "state-history"))), 5L)
  expect_gte(length(list.files(file.path(plan$state_directory, "catalog-history"))), 2L)
  # Every file written by the package ends in LF.
  expect_false(as.raw(13) %in% .read_bytes(file.path(plan$state_directory, "release.json")))
  expect_false(as.raw(13) %in% .read_bytes(file.path(plan$state_directory, "state.json")))
  # Publishing again finds everything done and returns the same entry.
  again <- suppressMessages(publish_benchmark_release(plan, "token", confirm = "test.1"))
  expect_identical(again, result)
  expect_length(Filter(function(r) r$published, zenodo$records), 2L)
  expect_length(grep("^DELETE", zenodo$requests), 0L)
  expect_false(dir.exists(file.path(plan$state_directory, ".lock")))
})

test_that("public verification during publication checks every archive without downloading unchanged ones", {
  skip_if_not_installed("zip")
  root <- withr::local_tempdir(); plan <- archive_release_plan(root)
  local_mock_gate(); zenodo <- fake_zenodo()
  downloads <- character(); fetches <- list()
  original <- zenodo$download; fetch <- .fetch_verified
  local_mocked_bindings(
    .resource_download = function(url, destination, progress) { downloads <<- c(downloads, url); original(url, destination, progress) },
    .fetch_verified = function(url, destination, sha256, size = NULL, md5 = NULL, progress = TRUE, max_try = 10, overwrite = FALSE, retry_not_found = 0L) {
      fetches[[length(fetches) + 1L]] <<- list(max_try = max_try, retry_not_found = retry_not_found)
      fetch(url, destination, sha256, size, md5, progress, max_try, overwrite, retry_not_found)
    })
  suppressMessages(publish_benchmark_release(plan, "token", confirm = "test.1"))
  # Both archives were uploaded by this plan, so each is downloaded in full once (plus release.json).
  expect_equal(sum(grepl("[.]zip/content$", downloads)), length(plan$catalog$archives))
  expect_true(any(grepl("release.json/content$", downloads)))
  # Publication-time public fetches allow five visibility retries (six attempts).
  expect_length(fetches, length(plan$catalog$archives) + 1L)
  expect_true(all(vapply(fetches, function(x) x$max_try == 6 && x$retry_not_found == 5L, logical(1))))
})

test_that("an interrupted commit resumes without uploading completed files or publishing twice", {
  skip_if_not_installed("zip")
  root <- withr::local_tempdir(); plan <- archive_release_plan(root)
  gate <- local_mock_gate()
  zenodo <- fake_zenodo(lose_commit = TRUE)
  expect_error(suppressMessages(stage_benchmark_release(plan, "token")), "lost commit response")
  expect_length(zenodo$upload_log, 1L)
  # The committed file is recognised by its stored bytes: it is neither uploaded nor committed again.
  suppressMessages(stage_benchmark_release(plan, "token"))
  expect_length(zenodo$upload_log, length(plan$catalog$archives))
  expect_false(anyDuplicated(zenodo$upload_log) > 0L)
  # Publishing runs staging and verification inside its own session: the lock taken once by
  # the session is not requested again (no deadlock).
  result <- suppressMessages(publish_benchmark_release(plan, "token", confirm = "test.1"))
  expect_length(zenodo$upload_log, length(plan$catalog$archives) + 1L)   # the archives, once, and release.json
  expect_false(anyDuplicated(zenodo$upload_log) > 0L)
  expect_identical(result$release, "test.1")
  expect_length(Filter(function(r) r$published, zenodo$records), 2L)
  records <- length(zenodo$records)
  expect_equal(suppressMessages(publish_benchmark_release(plan, "token", confirm = "test.1")), result)
  expect_length(zenodo$upload_log, length(plan$catalog$archives) + 1L)
  expect_length(zenodo$records, records)
  expect_length(grep("actions/publish", zenodo$requests), 2L)
  expect_length(gate$calls, 4L)
  expect_false(dir.exists(file.path(plan$state_directory, ".lock")))
  expect_true(file.exists(file.path(plan$state_directory, "registry-entry.json")))
})

# Everything that must hold once a release is published, however often it was interrupted.
expect_finished_release <- function(plan, zenodo, result, info = NULL) {
  published <- Filter(function(r) r$published, zenodo$records)
  expect_length(published, 2L)
  for (r in published) {
    expect_identical(r$communities, "community-uuid", info = info); expect_identical(r$default, "community-uuid", info = info)
  }
  # One publish and one inclusion request per record; the lost responses were not answered by a repetition.
  expect_length(grep("actions/publish$", zenodo$requests), 2L)
  expect_setequal(zenodo$submissions, names(published)); expect_false(anyDuplicated(zenodo$submissions) > 0L)
  expect_length(zenodo$inclusions, 2L)
  expect_true(all(vapply(zenodo$inclusions, function(q) identical(q$status, "accepted"), logical(1))))
  expect_length(grep("^DELETE", zenodo$requests), 0L)
  expect_false(anyDuplicated(zenodo$upload_log) > 0L)
  # The catalog, the registry entry and the state describe the same records.
  storage <- Filter(function(r) grepl("storage", r$metadata$title), published)[[1]]
  catalog_record <- Filter(function(r) !grepl("storage", r$metadata$title), published)[[1]]
  path <- file.path(plan$state_directory, "release.json")
  catalog <- benchmark_catalog(path)
  expect_identical(catalog$publication$catalog_record_id, catalog_record$id, info = info)
  expect_identical(catalog$publication$catalog_concept_doi, result$concept_doi, info = info)
  expect_identical(catalog$publication$storage_dois$no_bias, paste0("10.5281/zenodo.", storage$id), info = info)
  expect_identical(unique(vapply(c(catalog$archives, catalog$assets), `[[`, character(1), "record_id")), storage$id, info = info)
  entry <- jsonlite::read_json(file.path(plan$state_directory, "registry-entry.json"))
  expect_identical(entry$record_id, catalog_record$id, info = info)
  expect_identical(entry$doi, paste0("10.5281/zenodo.", catalog_record$id), info = info)
  uploaded <- zenodo$uploaded[[paste(catalog_record$id, "release.json")]]
  expect_identical(entry$catalog_sha256, digest::digest(uploaded, algo = "sha256", serialize = FALSE), info = info)
  expect_identical(result$catalog_sha256, entry$catalog_sha256, info = info)
  state <- .publication_state(plan)
  expect_identical(state$versions[["storage--no_bias"]]$record_id, storage$id, info = info)
  expect_identical(state$versions$catalog$record_id, catalog_record$id, info = info)
  expect_true(isTRUE(state$versions[["storage--no_bias"]]$published) && isTRUE(state$versions$catalog$published), info = info)
  expect_length(state$inclusions, 2L)
  expect_true(all(vapply(state$inclusions, function(x) isTRUE(x$accepted), logical(1))), info = info)
  # Nothing is left to do: publishing again changes nothing online.
  seen <- length(zenodo$requests)
  expect_identical(suppressMessages(publish_benchmark_release(plan, "token", confirm = "test.1")), result)
  expect_identical(grep("^(POST|PUT|PATCH|DELETE)", zenodo$requests[-seq_len(seen)], value = TRUE), character(), info = info)
}

test_that("a publication resumes after Zenodo applied a request whose response was lost", {
  skip_if_not_installed("zip")
  for (event in c("storage_publish", "catalog_publish", "storage_inclusion", "catalog_inclusion")) {
    root <- withr::local_tempdir(); plan <- archive_release_plan(root)
    local_mock_gate(); zenodo <- fake_zenodo(lose_after = event)
    expect_error(suppressMessages(publish_benchmark_release(plan, "token", confirm = "test.1")), paste0("lost ", event, " response"), info = event)
    expect_identical(zenodo$lost, event, info = event)
    expect_false(dir.exists(file.path(plan$state_directory, ".lock")), info = event)
    # The effect is there although the answer was lost; the resumed run finds it instead of repeating the request.
    if (grepl("publish$", event)) expect_true(any(vapply(zenodo$records, function(r) isTRUE(r$published) &&
      identical(if (grepl("storage", r$metadata$title)) "storage" else "catalog", sub("_.*$", "", event)), logical(1))), info = event)
    else expect_length(zenodo$inclusions, if (event == "storage_inclusion") 1L else 2L)
    result <- suppressMessages(publish_benchmark_release(plan, "token", confirm = "test.1"))
    expect_identical(result$release, "test.1", info = event)
    expect_finished_release(plan, zenodo, result, info = event)
  }
})

test_that("a publication whose first run was a failed request, not a lost response, resumes the same way", {
  skip_if_not_installed("zip")
  root <- withr::local_tempdir(); plan <- archive_release_plan(root)
  local_mock_gate(); zenodo <- fake_zenodo(lose_publish = TRUE)
  expect_error(suppressMessages(publish_benchmark_release(plan, "token", confirm = "test.1")), "lost publish response")
  expect_false(any(vapply(zenodo$records, `[[`, logical(1), "published")))   # nothing took effect
  result <- suppressMessages(publish_benchmark_release(plan, "token", confirm = "test.1"))
  expect_length(grep("actions/publish$", zenodo$requests), 3L)   # the failed request, then one per record
  expect_length(Filter(function(r) r$published, zenodo$records), 2L)
  expect_false(anyDuplicated(zenodo$submissions) > 0L)
})

test_that("the fake Zenodo serves the bytes that were uploaded, whatever happens to the local files afterwards", {
  skip_if_not_installed("zip")
  root <- withr::local_tempdir(); plan <- archive_release_plan(root)
  local_mock_gate(); zenodo <- fake_zenodo()
  result <- suppressMessages(publish_benchmark_release(plan, "token", confirm = "test.1"))
  storage <- Filter(function(r) grepl("storage", r$metadata$title), zenodo$records)[[1]]
  catalog_record <- Filter(function(r) !grepl("storage", r$metadata$title), zenodo$records)[[1]]
  sha256 <- function(x) if (is.raw(x)) digest::digest(x, algo = "sha256", serialize = FALSE) else digest::digest(file = x, algo = "sha256", serialize = FALSE)
  url <- function(id, filename) paste0("https://zenodo.org/api/records/", id, "/files/", filename, "/content")
  served <- function(id, filename) { destination <- withr::local_tempfile(); zenodo$download(url(id, filename), destination, FALSE); sha256(destination) }
  archive <- plan$catalog$archives[[1]]
  catalog_path <- file.path(plan$state_directory, "release.json")
  expect_identical(served(storage$id, archive$filename), archive$sha256)
  expect_identical(served(catalog_record$id, "release.json"), result$catalog_sha256)
  # The local archive and catalog are rewritten after the upload: what the fake serves does not change.
  writeBin(charToRaw("rewritten archive"), archive$local_path); writeBin(charToRaw("rewritten catalog"), catalog_path)
  expect_false(identical(sha256(archive$local_path), archive$sha256)); expect_false(identical(sha256(catalog_path), result$catalog_sha256))
  expect_identical(served(storage$id, archive$filename), archive$sha256)
  expect_identical(served(catalog_record$id, "release.json"), result$catalog_sha256)
  expect_identical(sha256(zenodo$uploaded[[paste(catalog_record$id, "release.json")]]), result$catalog_sha256)
  expect_identical(zenodo$records[[catalog_record$id]]$files[["release.json"]]$checksum,
                   paste0("md5:", digest::digest(zenodo$uploaded[[paste(catalog_record$id, "release.json")]], algo = "md5", serialize = FALSE)))
})
