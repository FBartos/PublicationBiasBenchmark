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
  base <- list(entries = list(list(key = "keep.zip", checksum = "md5:keep", size = 1),
                             list(key = "old.zip", checksum = "md5:old", size = 2)))
  remote <- list(entries = list()); imports <- 0L; deletions <- 0L; lost_import <- TRUE; lost_delete <- TRUE
  local_mocked_bindings(.zenodo_request = function(method, path, token, sandbox = FALSE, body = NULL) {
    if (method == "GET") return(if (identical(path, "records/1/files")) base else remote)
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
  record <- list(id = "uuid", slug = "benchmark", metadata = list(title = "Benchmark", website = "https://example.test"),
    access = list(review_policy = "closed", member_policy = "closed", record_submission_policy = "open"))
  posted <- NULL
  local_mocked_bindings(.zenodo_request = function(method, path, token, sandbox = FALSE, body = NULL) {
    if (method == "PUT") { posted <<- body; record$metadata <<- body$metadata }
    record
  })
  result <- update_benchmark_community_pages("benchmark", "<p>About</p>", "<p>Policy</p>", TRUE, "test-token")
  expect_identical(posted$access, record$access)
  expect_identical(result$metadata$website, "https://example.test")
  expect_identical(result$metadata$page, "<p>About</p>")
})

test_that("catalog preflight rejects concurrent releases before storage mutations", {
  plan <- list(state_directory = withr::local_tempdir(), sandbox = TRUE, catalog_record_id = "1", catalog = list(release = "test.2"))
  latest <- list(id = "2", parent = list(id = "family")); drafts <- list()
  local_mocked_bindings(.zenodo_request = function(...) latest, .family_drafts = function(...) drafts)
  expect_error(.reconcile_catalog_base(plan, "test-token"), "before any storage writes")
  .save_publication_state(plan, list(versions = list(catalog = list(record_id = "2"))))
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
