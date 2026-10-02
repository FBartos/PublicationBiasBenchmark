# A small stateful Zenodo for archive publication of a first release (no earlier
# version): records with drafts and files, DOI reservation, community inclusion and
# public downloads. It lets the whole staging/publishing path run without mocking
# any publication step.
fake_zenodo <- function(lose_publish = FALSE, env = parent.frame()) {
  zenodo <- new.env()
  zenodo$records <- list(); zenodo$next_id <- 100L; zenodo$uploaded <- list(); zenodo$requests <- character()
  zenodo$lose_publish_once <- lose_publish; zenodo$publish_lost <- 0L
  record <- function(id) zenodo$records[[id]]
  view <- function(r, draft = FALSE) list(id = r$id, metadata = r$metadata, access = r$access,
    is_published = r$published,
    links = list(reserve_doi = paste0("https://zenodo.org/api/records/", r$id, "/draft/pids/doi")),
    pids = if (r$published || r$reserved) list(doi = list(identifier = paste0("10.5281/zenodo.", r$id))) else list(),
    parent = list(id = r$parent, communities = list(ids = as.list(r$communities), default = r$default),
                  pids = if (r$published) list(doi = list(identifier = paste0("10.5281/zenodo.", r$parent)))))
  zenodo$handler <- function(method, path, token, sandbox = FALSE, body = NULL) {
    zenodo$requests <- c(zenodo$requests, paste(method, path))
    parts <- strsplit(sub("[?].*$", "", path), "/", fixed = TRUE)[[1]]
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
      source <- zenodo$uploaded[[paste(id, key)]]
      zenodo$records[[id]]$files[[key]] <- modifyList(file, list(status = "completed", size = file.info(source)$size,
        checksum = paste0("md5:", unname(tools::md5sum(source)))))
      return(zenodo$records[[id]]$files[[key]])
    }
    if (identical(rest, c("draft", "pids", "doi"))) { zenodo$records[[id]]$reserved <- TRUE; return(NULL) }
    if (identical(rest, c("draft", "actions", "publish"))) {
      if (zenodo$lose_publish_once) { zenodo$lose_publish_once <- FALSE; zenodo$publish_lost <- zenodo$publish_lost + 1L; stop("lost publish response") }
      zenodo$records[[id]]$published <- TRUE
      return(NULL)
    }
    if (identical(rest, "requests")) return(list(hits = list(hits = list())))
    if (identical(rest, "communities")) {
      if (method == "POST") {
        zenodo$records[[id]]$communities <- unique(c(zenodo$records[[id]]$communities, body$communities[[1]]$id))
        return(list(processed = list(list(request = list(id = "request-1", status = "accepted")))))
      }
      zenodo$records[[id]]$default <- body$default$id
      return(NULL)
    }
    stop("Unexpected request: ", method, " ", path)
  }
  zenodo$upload <- function(path, record_id, filename, ...) zenodo$uploaded[[paste(record_id, filename)]] <- path
  zenodo$download <- function(url, destination, progress) {
    parts <- strsplit(sub("^https://zenodo.org/api/", "", url), "/", fixed = TRUE)[[1]]
    source <- zenodo$uploaded[[paste(parts[2], utils::URLdecode(parts[4]))]]
    if (is.null(source)) stop(http_failure(404L))
    file.copy(source, destination, overwrite = TRUE)
  }
  zenodo$probe <- function(url, limit = 65536) {
    parts <- strsplit(sub("^https://zenodo.org/api/", "", url), "/", fixed = TRUE)[[1]]
    source <- zenodo$uploaded[[paste(parts[2], utils::URLdecode(parts[4]))]]
    if (is.null(source)) return(list(status = 404L, headers = list(), bytes = 0L))
    list(status = 206L, headers = list(`content-range` = paste0("bytes 0-0/", file.info(source)$size)), bytes = 1L)
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
