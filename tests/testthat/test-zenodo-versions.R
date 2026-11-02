test_that("native metadata updates replace JSON arrays and preserve provenance", {
  rights <- list(list(id = "cc-by-4.0"))
  old <- list(identifier = "https://osf.io/exf3m/", scheme = "url", relation_type = list(id = "isderivedfrom"))
  relationship <- .doi_relationship("10.test/catalog", "ispartof")
  posted <- NULL
  local_mocked_bindings(.zenodo_request = function(method, path, token, sandbox = FALSE, body = NULL) {
    if (method == "GET") return(list(metadata = list(rights = rights, related_identifiers = list(old)), access = list(record = "public", files = "public")))
    posted <<- body
    list(metadata = body$metadata)
  })
  plan <- list(sandbox = TRUE, metadata = list(rights = rights))
  .update_draft_metadata(plan, "123", list(related_identifiers = list(named = relationship)), "test-token")
  expect_length(posted$metadata$related_identifiers, 2L)
  expect_null(names(posted$metadata$related_identifiers))
  expect_identical(posted$metadata$related_identifiers[[1]], old)
  expect_identical(posted$metadata$related_identifiers[[2]], relationship)
  expect_true(.verify_record_rights(list(metadata = posted$metadata), plan$metadata))
})

test_that("native imports and file deletions recover lost responses without restoring superseded files", {
  root <- withr::local_tempdir()
  plan <- list(state_directory = root, sandbox = TRUE)
  local_held_lock(plan)
  base <- list(entries = list(list(key = "keep.zip", checksum = "md5:keep", size = 1),
                             list(key = "old.zip", checksum = "md5:old", size = 2)))
  remote <- list(entries = list()); imports <- 0L; deletions <- 0L; lost_import <- TRUE; lost_delete <- TRUE
  local_mocked_bindings(.zenodo_request = function(method, path, token, sandbox = FALSE, body = NULL) {
    if (method == "GET") {
      # Record 1 is the published base; record 2 is its unpublished draft.
      if (identical(path, "records/1")) return(list(is_published = TRUE))
      if (identical(path, "records/2")) return(list(is_published = FALSE))
      return(if (identical(path, "records/1/files")) base else if (identical(path, "records/2/draft/files")) remote else
        stop("Unexpected GET ", path))
    }
    if (method == "POST") {
      imports <<- imports + 1L; remote <<- base
      if (lost_import) { lost_import <<- FALSE; stop("lost import response") }
      return(remote)
    }
    if (method == "DELETE") {
      deletions <<- deletions + 1L
      remote$entries <<- Filter(function(x) x$key != "old.zip", remote$entries)
      if (lost_delete) { lost_delete <<- FALSE; stop("lost deletion response") }
      return(NULL)
    }
    stop("Unexpected request")
  })
  archives <- list(list(filename = "keep.zip"), list(filename = "new.zip"))
  expect_error(.ensure_imported_files(plan, "storage", "2", "1", archives, "test-token"), "lost import")
  expect_error(.ensure_imported_files(plan, "storage", "2", "1", archives, "test-token"), "lost deletion")
  expect_true(.ensure_imported_files(plan, "storage", "2", "1", archives, "test-token"))
  expect_equal(imports, 1L); expect_equal(deletions, 1L)
  expect_identical(remote$entries[[1]]$key, "keep.zip")
  expect_true(.publication_state(plan)$versions$storage$imported)
})

test_that("lost version creation responses recover the same draft and reject unrelated drafts", {
  root <- withr::local_tempdir(); plan <- list(state_directory = root, sandbox = TRUE, catalog = list(release = "test.2"))
  drafts <- list(); creations <- 0L; lose <- TRUE
  local_mocked_bindings(.family_drafts = function(...) drafts, .record_is_published = function(...) FALSE,
    .update_draft_metadata = function(...) TRUE,
    .zenodo_request = function(method, path, token, sandbox = FALSE, body = NULL) {
      if (method == "GET") return(list(id = "1", parent = list(id = "100")))
      creations <<- creations + 1L
      draft <- list(id = "2", metadata = list(title = "Old title")); drafts <<- list(draft)
      if (lose) { lose <<- FALSE; stop("lost version response") }
      draft
    })
  expect_error(.ensure_family_version(plan, "storage", "1", "New title", "Description", list(), "test-token"), "lost version")
  expect_identical(.ensure_family_version(plan, "storage", "1", "New title", "Description", list(), "test-token"), "2")
  expect_equal(creations, 1L)
  other <- plan; other$state_directory <- withr::local_tempdir()
  expect_error(.ensure_family_version(other, "storage", "1", "New title", "Description", list(), "test-token"), "unrelated")
})

test_that("a version creation that Zenodo rejected is forgotten, one without an answer is kept", {
  plan <- list(state_directory = withr::local_tempdir(), sandbox = TRUE, catalog = list(release = "test.2"))
  drafts <- list(); answer <- zenodo_error(409L, "conflict")
  local_mocked_bindings(.family_drafts = function(...) drafts, .record_is_published = function(...) FALSE,
    .update_draft_metadata = function(...) TRUE,
    .zenodo_request = function(method, path, token, sandbox = FALSE, body = NULL) {
      if (method == "GET") return(list(id = "1", parent = list(id = "100")))
      stop(answer)
    })
  ensure <- function(plan) .ensure_family_version(plan, "storage", "1", "New title", "Description", list(), "test-token")
  # A conflict leaves no marker, however often the run is repeated.
  expect_error(ensure(plan), "HTTP 409")
  expect_null(.publication_state(plan)$versions$storage)
  expect_error(ensure(plan), "HTTP 409")
  expect_null(.publication_state(plan)$versions$storage)
  # Another publisher's draft appears (its title is not ours): this run does not adopt it.
  drafts <- list(list(id = "2", metadata = list(title = "Old title")))
  expect_error(ensure(plan), "An unrelated new-version draft already exists")
  expect_null(.publication_state(plan)$versions$storage)
  # No HTTP answer at all: the draft may exist, so the marker stays and the draft is recognised later.
  drafts <- list(); answer <- simpleError("connection reset")
  expect_error(ensure(plan), "connection reset")
  expect_true(.publication_state(plan)$versions$storage$creating)
  drafts <- list(list(id = "2", metadata = list(title = "Old title")))
  expect_identical(ensure(plan), "2")
})

test_that("community acceptance resumes an existing request and checks inherited branding", {
  plan <- list(state_directory = withr::local_tempdir(), sandbox = TRUE)
  community <- "community-uuid"; accepted <- FALSE; branded <- FALSE; submits <- 0L; accepts <- 0L
  local_mocked_bindings(.zenodo_request = function(method, path, token, sandbox = FALSE, body = NULL) {
    if (method == "GET" && grepl("requests", path)) return(list(hits = list(hits = list(list(id = "req", type = "community-inclusion",
      receiver = list(community = community), status = if (accepted) "accepted" else "submitted")))))
    if (method == "GET") return(list(id = "1", parent = list(id = "family", communities = list(
      ids = if (accepted) list(community) else list(), default = if (branded) community else NULL))))
    if (method == "POST" && grepl("accept", path)) {
      accepts <<- accepts + 1L; accepted <<- TRUE
      if (accepts == 1L) stop("lost acceptance response")
      return(NULL)
    }
    if (method == "PUT") { branded <<- TRUE; return(NULL) }
    submits <<- submits + 1L; stop("Unexpected duplicate submission")
  })
  expect_error(.include_record_community(plan, "1", community, "test-token"), "lost acceptance")
  expect_true(.include_record_community(plan, "1", community, "test-token"))
  expect_equal(submits, 0L); expect_equal(accepts, 1L)
  expect_true(.include_record_community(plan, "2", community, "test-token"))
  expect_equal(accepts, 1L)
  expect_error(.zenodo_link_path("https://example.test/api/action", TRUE), "Unexpected")
})

test_that("community page updates preserve policies and unrelated metadata", {
  record <- list(id = "uuid", slug = "benchmark", metadata = list(title = "Benchmark", website = "https://example.test",
      page = "<p>Old about</p>", curation_policy = "<p>Old policy</p>"),
    access = list(review_policy = "closed", member_policy = "closed", record_submission_policy = "open"))
  posted <- NULL
  backups <- withr::local_tempdir()
  local_mock_gate(community_id = "uuid")
  local_mocked_bindings(.zenodo_request = function(method, path, token, sandbox = FALSE, body = NULL) {
    if (method == "PUT") { posted <<- body; record$metadata <<- body$metadata }
    record
  })
  result <- update_benchmark_community_pages("benchmark", "<p>About</p>", "<p>Policy</p>", TRUE, "test-token",
                                             confirm = "benchmark", backup_directory = backups)
  expect_identical(posted$access, record$access)
  expect_identical(result$metadata$website, "https://example.test")
  expect_identical(result$metadata$page, "<p>About</p>")
  # The previous pages were saved before they were replaced.
  saved <- jsonlite::read_json(list.files(backups, full.names = TRUE))
  expect_identical(saved$page, "<p>Old about</p>")
  expect_identical(saved$curation_policy, "<p>Old policy</p>")
  expect_identical(saved$community_id, "uuid")
})

test_that("a community page update reports stored HTML that differs from the submitted text", {
  update <- function(about = "<p>About</p>", policy = "<p>Policy</p>", sanitize_about = identity, sanitize_policy = identity) {
    record <- list(id = "uuid", slug = "benchmark", metadata = list(title = "Benchmark", page = "<p>Old</p>", curation_policy = "<p>Old policy</p>"),
                   access = list(review_policy = "closed", member_policy = "closed"))
    local_mock_gate(community_id = "uuid")
    local_mocked_bindings(.zenodo_request = function(method, path, token, sandbox = FALSE, body = NULL) {
      if (method == "PUT") {
        record$metadata <<- body$metadata
        record$metadata$page <<- sanitize_about(body$metadata$page); record$metadata$curation_policy <<- sanitize_policy(body$metadata$curation_policy)
      }
      record
    })
    warnings <- character()
    result <- withCallingHandlers(
      update_benchmark_community_pages("benchmark", about, policy, TRUE, "test-token", confirm = "benchmark",
                                       backup_directory = withr::local_tempdir()),
      warning = function(w) { warnings <<- c(warnings, conditionMessage(w)); invokeRestart("muffleWarning") })
    list(result = result, warnings = warnings)
  }
  # The same text, also with other whitespace between the tags, is silent.
  expect_identical(update()$warnings, character())
  same <- update(about = "<p>About</p>\n<p>More  text</p>\n", policy = "<p>Policy</p>",
                 sanitize_about = function(x) "<p>About</p> <p>More text</p>")
  expect_identical(same$warnings, character()); expect_identical(same$result$metadata$page, "<p>About</p> <p>More text</p>")
  # Sanitized or unchanged old content: the update is not an error, but each differing page is reported.
  script <- update(about = "<p>About</p><script>alert(1)</script>", sanitize_about = function(x) gsub("<script>.*</script>", "", x))
  expect_length(script$warnings, 1L)
  expect_match(script$warnings, "Zenodo stored different HTML for the About page than was submitted (it may sanitize HTML); check the pages of the community 'benchmark'.", fixed = TRUE)
  expect_identical(script$result$metadata$page, "<p>About</p>")
  policy <- update(sanitize_policy = function(x) "<p>Old policy</p>")
  expect_length(policy$warnings, 1L); expect_match(policy$warnings, "for the curation policy page", fixed = TRUE)
  both <- update(sanitize_about = function(x) "<p>Different</p>", sanitize_policy = function(x) "<p>Other</p>")
  expect_length(both$warnings, 2L)
  expect_match(both$warnings[1], "About page", fixed = TRUE); expect_match(both$warnings[2], "curation policy page", fixed = TRUE)
})

test_that("catalog preflight rejects concurrent releases before storage mutations", {
  plan <- list(state_directory = withr::local_tempdir(), sandbox = TRUE, catalog_record_id = "1", catalog = list(release = "test.2"))
  latest <- list(id = "2", parent = list(id = "family")); drafts <- list()
  local_mocked_bindings(.zenodo_request = function(...) latest, .family_drafts = function(...) drafts)
  expect_error(.reconcile_catalog_base(plan, "test-token"), "before any storage writes")
  .update_publication_state(plan, function(state) { state$versions$catalog <- list(record_id = "2"); state })
  expect_true(.reconcile_catalog_base(plan, "test-token"))
  drafts <- list(list(id = "3", metadata = list(title = "Other release")))
  expect_error(.reconcile_catalog_base(plan, "test-token"), "unrelated catalog draft")
})

test_that("native tags use subjects and read-only vocabulary fields are not resubmitted", {
  plan <- list(sandbox = TRUE, catalog = list(release = "test.1"),
    metadata = list(rights = list(list(id = "cc-by-4.0", icon = "cc-by-icon")), keywords = list("legacy-tag")))
  metadata <- .record_metadata(plan, "Title", "Description")
  expect_null(metadata$keywords)
  expect_true("legacy-tag" %in% vapply(metadata$subjects, `[[`, character(1), "subject"))
  posted <- NULL
  local_mocked_bindings(.zenodo_request = function(method, path, token, sandbox = FALSE, body = NULL) {
    if (method == "GET") return(list(metadata = metadata, access = list(record = "public", files = "public",
      embargo = list(active = FALSE, reason = NULL))))
    posted <<- body
    list(metadata = body$metadata, errors = list(list(field = "files.enabled", messages = list("Missing uploaded files."))))
  })
  .update_draft_metadata(plan, "1", metadata, "test-token")
  expect_null(posted$files)
  expect_identical(posted$metadata$rights, list(list(id = "cc-by-4.0")))
  expect_null(posted$access$embargo$reason)
})
