## Remote deletes ---------------------------------------------------------------------

# A scripted draft record: unpublished record "2" with draft files, published base "1".
draft_server <- function(entries, published = FALSE, base_files = list(), base_published = TRUE) {
  env <- new.env(); env$entries <- entries; env$requests <- list(); env$deleted <- character()
  env$handler <- function(method, path, token, sandbox = FALSE, body = NULL) {
    env$requests[[length(env$requests) + 1L]] <- paste(method, path)
    if (method == "GET" && path == "records/2") return(list(is_published = published))
    if (method == "GET" && path == "records/1") return(list(is_published = base_published))
    if (method == "GET" && path == "records/2/draft/files") return(list(entries = unname(env$entries)))
    if (method == "GET" && path == "records/1/files") return(list(entries = unname(base_files)))
    if (method == "DELETE") {
      key <- utils::URLdecode(basename(path)); env$deleted <- c(env$deleted, key); env$entries[[key]] <- NULL; return(NULL)
    }
    stop("Unexpected ", method, " ", path)
  }
  env
}
entry <- function(key, status = "pending", size = 5, checksum = "md5:aa", created = "T1")
  list(key = key, status = status, size = size, checksum = checksum, created = created)
journal <- function(plan, record, key, created = "T1") .update_publication_state(plan, function(state) {
  state$initialized[[record]][[key]] <- list(created = created, sha256 = "s", md5 = "m", size = 3); state
})
deletes <- function(server) grep("^DELETE", unlist(server$requests), value = TRUE)

test_that("a pending file that this publication state created can be reset, and the deletion is logged", {
  plan <- list(state_directory = withr::local_tempdir(), sandbox = TRUE); local_held_lock(plan)
  journal(plan, "2", "a.zip")
  server <- draft_server(list(a.zip = entry("a.zip")))
  log <- file.path(plan$state_directory, "deletions.log")
  writeLines("torn line {", log)   # the log is audit-only and never read
  local_mocked_bindings(.zenodo_request = function(method, path, ...) {
    if (method == "DELETE") expect_match(readLines(log)[2], '"event":"intent"', fixed = TRUE)   # logged before the delete
    server$handler(method, path, ...)
  })
  expect_true(.delete_draft_file(plan, "2", "a.zip", "pending-reset", "token"))
  expect_identical(deletes(server), "DELETE records/2/draft/files/a.zip")
  lines <- readLines(log)
  expect_length(lines, 3L)
  intent <- jsonlite::fromJSON(lines[2]); done <- jsonlite::fromJSON(lines[3])
  expect_identical(c(intent$event, done$event), c("intent", "done"))
  expect_identical(intent[c("record_id", "key", "reason", "status", "created", "checksum")],
                   list(record_id = "2", key = "a.zip", reason = "pending-reset", status = "pending", created = "T1", checksum = "md5:aa"))
  expect_true(all(c("time", "pid") %in% names(intent)))
})

test_that("pending resets stop for entries that are completed, empty, unjournaled or not ours", {
  plan <- list(state_directory = withr::local_tempdir(), sandbox = TRUE); local_held_lock(plan)
  attempt <- function(entry, journaled = "T1") {
    if (!is.null(journaled)) journal(plan, "2", "a.zip", journaled)
    server <- draft_server(list(a.zip = entry)); local_mocked_bindings(.zenodo_request = server$handler)
    result <- tryCatch(.delete_draft_file(plan, "2", "a.zip", "pending-reset", "token"), error = function(error) error)
    expect_length(deletes(server), 0L)
    result
  }
  expect_match(conditionMessage(attempt(entry("a.zip", status = "completed"))), "not a pending file holding uploaded bytes")
  expect_match(conditionMessage(attempt(entry("a.zip", size = NULL, checksum = NULL))), "not a pending file holding uploaded bytes")
  # Journal shapes that cannot prove ownership.
  for (case in list(list(entry("a.zip", created = "T2"), "T1"), list(entry("a.zip", created = NULL), "T1"), list(entry("a.zip"), NULL))) {
    unlink(file.path(plan$state_directory, c("state.json", "state-history")), recursive = TRUE)
    message <- conditionMessage(attempt(case[[1]], case[[2]]))
    expect_match(message, "did not create it. Nothing was deleted.", fixed = TRUE)
    expect_match(message, "record 2", fixed = TRUE); expect_match(message, "'a.zip'", fixed = TRUE)
    expect_match(message, "https://sandbox.zenodo.org/api/records/2/draft/files", fixed = TRUE)
  }
  # A published record, and a key that vanished from the draft, are never touched.
  server <- draft_server(list(a.zip = entry("a.zip")), published = TRUE); local_mocked_bindings(.zenodo_request = server$handler)
  expect_error(.delete_draft_file(plan, "2", "a.zip", "pending-reset", "token"), "already published")
  server <- draft_server(list()); local_mocked_bindings(.zenodo_request = server$handler)
  expect_error(.delete_draft_file(plan, "2", "a.zip", "pending-reset", "token"), "no longer exists")
  expect_length(deletes(server), 0L)
  expect_false(file.exists(file.path(plan$state_directory, "deletions.log")))
})

test_that("an import superseded by a newer snapshot is deleted only when the published base holds identical bytes", {
  plan <- list(state_directory = withr::local_tempdir(), sandbox = TRUE); local_held_lock(plan)
  base <- list(entry("old.zip", "completed", size = 7, checksum = "md5:old"))
  attempt <- function(draft, base_files = base, base_published = TRUE, base_id = "1") {
    server <- draft_server(list(old.zip = draft), base_files = base_files, base_published = base_published)
    local_mocked_bindings(.zenodo_request = server$handler)
    result <- tryCatch(.delete_draft_file(plan, "2", "old.zip", "superseded-import", "token", base_id), error = function(error) error)
    list(result = result, deletes = deletes(server))
  }
  ok <- attempt(entry("old.zip", "completed", size = 7, checksum = "md5:old"))
  expect_true(ok$result); expect_identical(ok$deletes, "DELETE records/2/draft/files/old.zip")
  for (refusal in list(
    attempt(entry("old.zip", "completed", size = 7, checksum = "md5:other")),
    attempt(entry("old.zip", "completed", size = 8, checksum = "md5:old")),
    attempt(entry("old.zip", "completed", size = 7, checksum = NULL)),
    attempt(entry("old.zip", "completed", size = 7, checksum = "md5:old"), base_published = FALSE),
    attempt(entry("old.zip", "completed", size = 7, checksum = "md5:old"), base_files = list()),
    attempt(entry("old.zip", "completed", size = 7, checksum = "md5:old"), base_id = NULL))) {
    expect_s3_class(refusal$result, "error"); expect_match(conditionMessage(refusal$result), "Nothing was deleted")
    expect_length(refusal$deletes, 0L)
  }
})

test_that("a delete needs the publication lock of the plan's state directory before any request", {
  plan <- list(state_directory = withr::local_tempdir(), sandbox = TRUE)
  server <- draft_server(list(a.zip = entry("a.zip")))
  local_mocked_bindings(.zenodo_request = server$handler)
  journal(plan, "2", "a.zip")
  expect_error(.delete_draft_file(plan, "2", "a.zip", "pending-reset", "token"), "hold the publication lock")
  expect_error(.delete_draft_file(list(sandbox = TRUE), "2", "a.zip", "pending-reset", "token"), "state directory")
  # A lock held by another process (a different nonce) does not count.
  .acquire_publication_lock(plan$state_directory)
  jsonlite::write_json(list(pid = 1L, host = "x", started = .utc_now(), nonce = "foreign"),
                       file.path(plan$state_directory, ".lock", "owner.json"), auto_unbox = TRUE)
  expect_error(.delete_draft_file(plan, "2", "a.zip", "pending-reset", "token"), "hold the publication lock")
  unlink(file.path(plan$state_directory, ".lock"), recursive = TRUE); rm(list = .lock_key(plan$state_directory), envir = .publication_locks)
  expect_length(server$requests, 0L)
  expect_length(deletes(server), 0L)
  expect_error(.delete_draft_file(plan, "2", "a.zip", "unlisted-reason", "token"), "should be one of")
})

# A draft record that behaves like Zenodo for file initialization, upload and commit.
fake_draft_files <- function(assets, created_in_response = TRUE) {
  env <- new.env(); env$entries <- list(); env$counter <- 0L; env$requests <- character(); env$uploads <- character()
  env$handler <- function(method, path, token, sandbox = FALSE, body = NULL) {
    env$requests <- c(env$requests, paste(method, path))
    parts <- strsplit(path, "/", fixed = TRUE)[[1]]
    if (method == "GET" && path == "records/12345") return(list(is_published = FALSE))
    if (method == "GET") return(list(entries = unname(env$entries)))
    if (method == "DELETE") { env$entries[[utils::URLdecode(parts[5])]] <- NULL; return(NULL) }
    if (endsWith(path, "/commit")) {
      key <- utils::URLdecode(parts[5]); asset <- Filter(function(x) x$filename == key, assets)[[1]]
      env$entries[[key]] <- modifyList(env$entries[[key]], list(status = "completed", size = asset$size, checksum = paste0("md5:", asset$md5)))
      return(env$entries[[key]])
    }
    created <- lapply(body, function(file) {
      env$counter <- env$counter + 1L
      env$entries[[file$key]] <- c(list(key = file$key, status = "pending"), if (created_in_response) list(created = paste0("created-", env$counter)))
      env$entries[[file$key]]
    })
    list(entries = created)
  }
  env$upload <- function(path, record_id, filename, ...) {
    env$uploads <- c(env$uploads, filename)
    asset <- Filter(function(x) x$filename == filename, assets)[[1]]
    env$entries[[filename]] <- modifyList(env$entries[[filename]], list(size = asset$size, checksum = paste0("md5:", asset$md5)))
  }
  env
}

test_that("initialization is journaled from the response before any bytes are uploaded", {
  root <- withr::local_tempdir(); assets <- lapply(1:2, function(i) test_resource(root, paste0(i, ".csv"), ids = (2 * i - 1):(2 * i)))
  plan <- list(state_directory = file.path(root, "state"), sandbox = FALSE); local_held_lock(plan)
  server <- fake_draft_files(assets)
  local_mocked_bindings(.zenodo_request = server$handler, .zenodo_upload = server$upload)
  suppressMessages(.stage_files(plan, "12345", assets, "token"))
  initialized <- .publication_state(plan)$initialized[["12345"]]
  expect_setequal(names(initialized), c("1.csv", "2.csv"))
  expect_equal(initialized[["1.csv"]], list(created = "created-1", sha256 = assets[[1]]$sha256, md5 = assets[[1]]$md5, size = assets[[1]]$size))
  expect_identical(initialized[["2.csv"]]$created, "created-2")
  # The journal is written between the initialization and the first upload.
  expect_identical(server$uploads, c("1.csv", "2.csv"))
})

test_that("a pending file holding other bytes is reset only when its creation was journaled", {
  root <- withr::local_tempdir(); asset <- test_resource(root, "a.csv")
  plan <- list(state_directory = file.path(root, "state"), sandbox = FALSE); local_held_lock(plan)
  server <- fake_draft_files(list(asset))
  local_mocked_bindings(.zenodo_request = server$handler, .zenodo_upload = server$upload)
  # Stale bytes in a pending key that this state initialized: delete, initialize again, upload the planned bytes.
  server$entries[["a.csv"]] <- entry("a.csv", size = 999, checksum = "md5:stale", created = "created-0")
  journal(plan, "12345", "a.csv", "created-0")
  suppressMessages(.stage_files(plan, "12345", list(asset), "token"))
  expect_identical(grep("^DELETE", server$requests, value = TRUE), "DELETE records/12345/draft/files/a.csv")
  expect_identical(server$entries[["a.csv"]]$checksum, paste0("md5:", asset$md5))
  expect_identical(.publication_state(plan)$initialized[["12345"]][["a.csv"]]$created, "created-1")
  # The same situation without a journal entry stops: the bytes may belong to somebody else.
  server <- fake_draft_files(list(asset))
  local_mocked_bindings(.zenodo_request = server$handler, .zenodo_upload = server$upload)
  server$entries[["a.csv"]] <- entry("a.csv", size = 999, checksum = "md5:stale", created = "created-0")
  unlink(file.path(plan$state_directory, c("state.json", "state-history")), recursive = TRUE)
  expect_error(.stage_files(plan, "12345", list(asset), "token"), "did not create it. Nothing was deleted.")
  expect_error(.stage_files(plan, "12345", list(asset), "token"), "https://zenodo.org/api/records/12345/draft/files", fixed = TRUE)
  expect_length(grep("^(DELETE|POST)", server$requests, value = TRUE), 0L)
  expect_identical(server$uploads, character())
  expect_identical(server$entries[["a.csv"]]$checksum, "md5:stale")
})

test_that("a response without creation times is not journaled, so its keys are never reset later", {
  root <- withr::local_tempdir(); asset <- test_resource(root, "a.csv")
  plan <- list(state_directory = file.path(root, "state"), sandbox = FALSE); local_held_lock(plan)
  server <- fake_draft_files(list(asset), created_in_response = FALSE)
  local_mocked_bindings(.zenodo_request = server$handler, .zenodo_upload = server$upload)
  suppressMessages(.stage_files(plan, "12345", list(asset), "token"))
  expect_null(.publication_state(plan)$initialized)
  server$entries[["a.csv"]] <- list(key = "a.csv", status = "pending", size = 999, checksum = "md5:stale")
  expect_error(.stage_files(plan, "12345", list(asset), "token"), "Nothing was deleted")
  expect_length(grep("^DELETE", server$requests, value = TRUE), 0L)
  for (shape in list(NULL, list(), "odd", list(entries = "odd"))) expect_false(.journal_initialized(plan, "12345", list(asset), shape))
  # A response that is the bare list of entries works too.
  expect_true(.journal_initialized(plan, "12345", list(asset), list(list(key = "a.csv", created = "bare"))))
  expect_identical(.publication_state(plan)$initialized[["12345"]][["a.csv"]]$created, "bare")
})

## Public verification ---------------------------------------------------------------------

public_archive <- function(size = 100, ...) modifyList(list(id = "a1", dgm = "no_bias", filename = "x.zip", record_id = "5",
  size = size, sha256 = strrep("a", 64), md5 = strrep("b", 32), members = list()), list(...))
raw_headers <- function(status, ...) {
  fields <- c(...)
  charToRaw(paste0("HTTP/1.1 ", status, " X\r\n", paste0(names(fields), ": ", fields, "\r\n", collapse = ""), "\r\n"))
}

# One scripted probe answer per request; a condition is raised, a list is returned.
scripted_probes <- function(answers, env_ = parent.frame()) {
  env <- new.env(); env$calls <- 0L; env$waits <- numeric(); env$urls <- character()
  testthat::local_mocked_bindings(
    .resource_range_probe = function(url, limit = 65536) {
      env$calls <- env$calls + 1L; env$urls <- c(env$urls, url)
      answer <- answers[[min(env$calls, length(answers))]]
      if (inherits(answer, "condition")) stop(answer)
      answer
    }, .resource_retry_wait = function(delay) env$waits <- c(env$waits, delay), .env = env_)
  env
}
status_answer <- function(status, ...) list(status = status, headers = as.list(c(...)), bytes = 0L)

test_that("a public archive is accepted from a 206 Content-Range or a 200 Content-Length without downloading it", {
  plan <- list(sandbox = FALSE, state_directory = withr::local_tempdir())
  directory <- withr::local_tempdir()
  local_mocked_bindings(.fetch_verified = function(...) stop("The archive must not be downloaded"))
  probes <- scripted_probes(list(status_answer(206L, `content-range` = "bytes 0-0/100")))
  expect_true(.verify_public_archive_size(plan, public_archive(100), directory))
  expect_identical(probes$urls, "https://zenodo.org/api/records/5/files/x.zip/content")
  probes <- scripted_probes(list(status_answer(200L, `content-length` = "100")))
  expect_true(.verify_public_archive_size(plan, public_archive(100), directory))
  # A different total is an error; so is a Content-Range that is not a one-byte range.
  probes <- scripted_probes(list(status_answer(206L, `content-range` = "bytes 0-0/99")))
  expect_error(.verify_public_archive_size(plan, public_archive(100), directory), "does not have the catalog size \\(100 bytes\\)")
  probes <- scripted_probes(list(status_answer(200L, `content-length` = "101")))
  expect_error(.verify_public_archive_size(plan, public_archive(100), directory), "does not have the catalog size")
  probes <- scripted_probes(list(status_answer(206L, `content-range` = "bytes 0-9/100")))
  expect_error(.verify_public_archive_size(plan, public_archive(100), directory), "does not have the catalog size")
})

test_that("without any size header the archive is fetched in full, with a message", {
  plan <- list(sandbox = TRUE, state_directory = withr::local_tempdir()); directory <- withr::local_tempdir()
  fetched <- list()
  local_mocked_bindings(.fetch_verified = function(url, destination, sha256, size, md5, ...) {
    fetched[[length(fetched) + 1L]] <<- list(url = url, destination = destination, size = size, args = list(...)); TRUE
  })
  for (answer in list(status_answer(206L), status_answer(200L))) {
    probes <- scripted_probes(list(answer))
    expect_message(expect_true(.verify_public_archive_size(plan, public_archive(100), directory)), "reported no size for x.zip")
  }
  expect_length(fetched, 2L)
  expect_identical(fetched[[1]]$url, "https://sandbox.zenodo.org/api/records/5/files/x.zip/content")
  expect_identical(fetched[[1]]$destination, file.path(directory, "x.zip")); expect_identical(fetched[[1]]$size, 100)
  expect_identical(fetched[[1]]$args$retry_not_found, 5L); expect_true(fetched[[1]]$args$overwrite)
})

test_that("the size probe retries a not yet visible archive five times, never a withdrawn one, and waits for rate limits", {
  plan <- list(sandbox = FALSE); directory <- withr::local_tempdir()
  for (status in c(404L, 403L)) {
    probes <- scripted_probes(list(status_answer(status)))
    expect_error(.verify_public_archive_size(plan, public_archive(), directory), if (status == 404L) "is not available on Zenodo \\(HTTP 404\\)" else "permanent error")
    expect_identical(probes$calls, 6L); expect_identical(probes$waits, c(1, 2, 4, 8, 16))
  }
  probes <- scripted_probes(c(replicate(3, status_answer(404L), simplify = FALSE), list(status_answer(206L, `content-range` = "bytes 0-0/100"))))
  expect_true(.verify_public_archive_size(plan, public_archive(), directory))
  expect_identical(probes$calls, 4L); expect_identical(probes$waits, c(1, 2, 4))
  probes <- scripted_probes(list(status_answer(410L)))
  expect_error(.verify_public_archive_size(plan, public_archive(), directory), "HTTP 410")
  expect_identical(probes$calls, 1L); expect_length(probes$waits, 0L)
  for (status in c(400L, 401L, 451L, 501L)) {
    probes <- scripted_probes(list(status_answer(status)))
    expect_error(.verify_public_archive_size(plan, public_archive(), directory), "permanent error")
    expect_identical(probes$calls, 1L)
  }
  probes <- scripted_probes(list(status_answer(429L, `retry-after` = "7"), status_answer(429L), status_answer(206L, `content-range` = "bytes 0-0/100")))
  expect_true(.verify_public_archive_size(plan, public_archive(), directory))
  expect_identical(probes$waits, c(7, 60))
  probes <- scripted_probes(c(replicate(2, status_answer(503L), simplify = FALSE), list(status_answer(200L, `content-length` = "100"))))
  expect_true(.verify_public_archive_size(plan, public_archive(), directory))
  expect_identical(probes$waits, c(1, 2))
  probes <- scripted_probes(list(status_answer(503L)))
  expect_error(.verify_public_archive_size(plan, public_archive(), directory, max_try = 3L), "after 3 attempts \\(last error: Public resource download failed \\(HTTP 503\\)\\)\\.$")
})

test_that("the size probe applies the curl error classes of the download policy", {
  plan <- list(sandbox = FALSE); directory <- withr::local_tempdir()
  probes <- scripted_probes(list(curl_failure("curl_error_peer_failed_verification", "certificate problem")))
  expect_error(.verify_public_archive_size(plan, public_archive(), directory), "certificate problem")
  expect_identical(probes$calls, 1L)
  probes <- scripted_probes(list(curl_failure("curl_error_couldnt_resolve_host", "no such host")))
  expect_error(.verify_public_archive_size(plan, public_archive(), directory), "Cannot reach zenodo.org; check the network connection.")
  expect_identical(probes$calls, 3L); expect_identical(probes$waits, c(1, 2))
  probes <- scripted_probes(list(curl_failure("curl_error_operation_timedout"), status_answer(206L, `content-range` = "bytes 0-0/100")))
  expect_true(.verify_public_archive_size(plan, public_archive(), directory))
  expect_identical(probes$waits, 1)
})

# The probe itself, with the curl connection replaced by a local file.
fake_connection <- function(body_size, status, headers, exists = TRUE) {
  path <- withr::local_tempfile(.local_envir = parent.frame())
  if (exists) writeBin(as.raw(rep(65L, body_size)), path)
  list(path = if (exists) path else file.path(dirname(path), "absent", "file"),
       mock = function(env) testthat::local_mocked_bindings(
         curl = function(url, open = "", handle) file(if (exists) path else file.path(dirname(path), "absent", "file")),
         handle_data = function(handle) list(status_code = status, headers = headers),
         .package = "curl", .env = env))
}

test_that("the range probe reads at most 64 KiB, so a server ignoring Range cannot make it download the file", {
  connection <- fake_connection(5e6, 200L, raw_headers(200, `Content-Length` = "5000000"))
  connection$mock(environment())
  answer <- .resource_range_probe("https://zenodo.org/api/records/5/files/x.zip/content")
  expect_identical(answer$status, 200L); expect_identical(answer$headers[["content-length"]], "5000000")
  expect_lte(answer$bytes, 65537L); expect_gt(answer$bytes, 0L)
  expect_identical(answer$bytes, 65537L)
  connection <- fake_connection(1, 206L, raw_headers(206, `Content-Range` = "bytes 0-0/100"))
  connection$mock(environment())
  answer <- .resource_range_probe("https://zenodo.org/x")
  expect_identical(answer$status, 206L); expect_identical(answer$headers[["content-range"]], "bytes 0-0/100"); expect_identical(answer$bytes, 1L)
  # An HTTP error is an answer, not an exception: the connection cannot be opened but a status is known.
  connection <- fake_connection(0, 404L, raw_headers(404), exists = FALSE)
  connection$mock(environment())
  answer <- .resource_range_probe("https://zenodo.org/x")
  expect_identical(answer$status, 404L); expect_identical(answer$bytes, 0L)
})

test_that("a failure before any HTTP answer raises libcurl's classed error", {
  connection <- fake_connection(0, 0L, raw_headers(0), exists = FALSE)
  connection$mock(environment())
  local_mocked_bindings(curl_fetch_memory = function(url, handle) stop(curl_failure("curl_error_couldnt_resolve_host", "no such host")), .package = "curl")
  error <- tryCatch(.resource_range_probe("https://zenodo.org/x"), error = function(error) error)
  expect_s3_class(error, "curl_error_couldnt_resolve_host")
  expect_identical(.classify_download_failure(error, 1L), "dns")
})

test_that("every storage record must be anonymously public without an embargo", {
  record <- list(access = list(record = "public", files = "public", embargo = list(active = FALSE)))
  tokens <- list()
  local_mocked_bindings(.zenodo_request = function(method, path, token, sandbox = FALSE, body = NULL) {
    tokens[[length(tokens) + 1L]] <<- list(path = path, token = token, sandbox = sandbox, method = method); record
  })
  expect_true(.verify_public_record("5", FALSE))
  expect_identical(tokens, list(list(path = "records/5", token = NULL, sandbox = FALSE, method = "GET")))
  for (restricted in list(list(record = "restricted", files = "public"), list(record = "public", files = "restricted"),
                          list(record = "public", files = "public", embargo = list(active = TRUE)), list(record = "public"))) {
    record <- list(access = restricted)
    expect_error(.verify_public_record("5", FALSE), "not fully public")
  }
  # A just published record may need a moment; anything else is an error.
  waits <- numeric(); calls <- 0L
  local_mocked_bindings(.resource_retry_wait = function(delay) waits <<- c(waits, delay),
    .zenodo_request = function(...) { calls <<- calls + 1L; if (calls < 3L) stop(zenodo_error(404L)); list(access = list(record = "public", files = "public")) })
  expect_true(.verify_public_record("5", FALSE)); expect_identical(waits, c(1, 2))
  calls <- -100L; waits <- numeric()
  expect_error(.verify_public_record("5", FALSE), "not publicly readable without a token \\(HTTP 404\\)")
  expect_length(waits, 5L)
  local_mocked_bindings(.zenodo_request = function(...) stop(zenodo_error(500L)))
  expect_error(.verify_public_record("5", FALSE), "HTTP 500")
})

test_that("only archives uploaded by the plan are downloaded; every advertised archive is checked anonymously", {
  root <- withr::local_tempdir()
  uploaded <- public_archive(id = "u", filename = "uploaded.zip", record_id = "5")
  reused <- public_archive(id = "r", filename = "reused.zip", record_id = "6", dgm = "other")
  uploaded$local_path <- file.path(root, "uploaded.zip")
  plan <- list(sandbox = FALSE, state_directory = file.path(root, "state"), groups = list(no_bias = list(uploaded), other = list()))
  catalog <- list(archives = list(uploaded, reused))
  calls <- character()
  local_mocked_bindings(
    .verify_public_record = function(record_id, sandbox) { calls <<- c(calls, paste("record", record_id)); TRUE },
    .verify_public_archive_size = function(plan, archive, directory, ...) { calls <<- c(calls, paste("size", archive$filename)); TRUE },
    .fetch_verified = function(url, destination, sha256, size, md5, progress, max_try, overwrite, retry_not_found) {
      calls <<- c(calls, paste("fetch", basename(destination), max_try, overwrite, retry_not_found)); TRUE },
    .zip_inventory = function(path, members) { calls <<- c(calls, paste("inventory", basename(path))); TRUE })
  expect_message(.verify_public_archives(plan, catalog), "1 of 2 archives were not uploaded by this plan")
  expect_identical(calls, c("record 5", "record 6", "size uploaded.zip", "fetch uploaded.zip 6 TRUE 5", "inventory uploaded.zip",
                            "size reused.zip"))
  # Nothing skipped: no message.
  plan$groups$other <- list(reused); calls <- character()
  expect_no_message(.verify_public_archives(plan, catalog))
  expect_length(grep("^fetch", calls), 2L)
})

## Community pages ---------------------------------------------------------------------------

test_that("community page updates need confirm, an owner token and a backup before any write", {
  record <- list(id = "uuid", slug = "benchmark", metadata = list(title = "T", page = "<p>old</p>", curation_policy = "<p>old policy</p>"),
                 access = list(review_policy = "closed"))
  writes <- character(); requests <- list()
  local_mocked_bindings(.zenodo_request = function(method, path, token, sandbox = FALSE, body = NULL) {
    requests[[length(requests) + 1L]] <<- paste(method, path)
    if (method != "GET") { writes <<- c(writes, paste(method, path)); if (method == "PUT") record$metadata <<- body$metadata }
    if (grepl("/members[?]", path)) return(members_page(list(member_hit(role))))
    if (grepl("^communities/[^/]+$", path) && !identical(path, "communities/uuid")) return(list(id = id))
    record
  })
  update <- function(..., sandbox = TRUE, backup = withr::local_tempdir()) update_benchmark_community_pages("benchmark",
    "<p>About</p>", "<p>Policy</p>", sandbox, "token", ..., backup_directory = backup)
  role <- "owner"; id <- "uuid"
  # confirm is checked locally, before any request.
  for (confirm in list(NULL, "other", NA_character_, c("benchmark", "benchmark"), 1)) {
    expect_error(update(confirm = confirm), "Replacing community pages is irreversible: re-run with confirm = \"benchmark\"")
  }
  expect_length(requests, 0L)
  # Only the owner may replace the pages; a manager passes the release gate but not this one.
  role <- "manager"
  expect_error(update(confirm = "benchmark"), "role 'manager'")
  role <- "reader"
  expect_error(update(confirm = "benchmark"), "required")
  expect_length(writes, 0L)
  # A foreign production community is refused before the member list is read.
  role <- "owner"; id <- "foreign-uuid"; requests <- list()
  expect_error(update(confirm = "benchmark", sandbox = FALSE), "restricted to the PublicationBiasBenchmark community")
  expect_length(writes, 0L); expect_identical(unlist(requests), "GET communities/benchmark")
  # Without a verified backup nothing is written.
  id <- "uuid"; blocker <- withr::local_tempfile(); writeLines("a file", blocker)
  expect_error(update(confirm = "benchmark", backup = file.path(blocker, "backups")), "Cannot save a backup of the current community pages.*not changed")
  expect_length(writes, 0L)
  local_mocked_bindings(.write_json_verified = function(...) stop("disk full"))
  expect_error(update(confirm = "benchmark"), "disk full")
  expect_length(writes, 0L)
})

test_that("the community page backup exists before the PUT and the default backup location is the user's data directory", {
  record <- list(id = "uuid", slug = "benchmark", metadata = list(title = "T", page = "<p>old</p>", curation_policy = "<p>old policy</p>"),
                 access = list(review_policy = "closed"))
  backups <- withr::local_tempdir(); put_saw_backup <- NULL
  local_mock_gate(community_id = "uuid")
  local_mocked_bindings(.zenodo_request = function(method, path, token, sandbox = FALSE, body = NULL) {
    if (method == "PUT") { put_saw_backup <<- length(list.files(backups)); record$metadata <<- body$metadata }
    record
  })
  update_benchmark_community_pages("benchmark", "<p>New</p>", "<p>New policy</p>", TRUE, "token", confirm = "benchmark",
                                   backup_directory = backups)
  expect_identical(put_saw_backup, 1L)
  expect_match(list.files(backups), "^community-pages-uuid-[0-9]{8}T[0-9]{6}[.][0-9]{6}[.]json$")
  expect_identical(formals(update_benchmark_community_pages)$confirm, NULL)
  expect_identical(formals(update_benchmark_community_pages)$backup_directory, NULL)
})

test_that("before R 4.0 the default backup directory is below the home folder", {
  expect_identical(.r_version(), getRversion())
  # R before 4.0 has no tools::R_user_dir(); the fallback never calls it, whatever R runs the test.
  for (version in c("3.6.3", "3.5.0")) {
    local_mocked_bindings(.r_version = function() package_version(version))
    expect_identical(.default_backup_directory(), file.path(path.expand("~"), ".PublicationBiasBenchmark"))
  }
})

test_that("from R 4.0 the default backup directory is the user's data directory for the package", {
  skip_if(getRversion() < "4.0.0", "tools::R_user_dir() needs R 4.0")
  for (version in c("4.0.0", "4.6.0")) {
    local_mocked_bindings(.r_version = function() package_version(version))
    expect_identical(.default_backup_directory(), tools::R_user_dir("PublicationBiasBenchmark", "data"))
  }
})

test_that("omitting backup_directory writes the backup to the version-dependent default", {
  record <- list(id = "uuid", slug = "benchmark", metadata = list(title = "T", page = "<p>old</p>", curation_policy = "<p>old policy</p>"),
                 access = list(review_policy = "closed"))
  fallback <- withr::local_tempdir()
  local_mock_gate(community_id = "uuid")
  local_mocked_bindings(.r_version = function() package_version("3.6.3"), .default_backup_directory = function() fallback)
  local_mocked_bindings(.zenodo_request = function(method, path, token, sandbox = FALSE, body = NULL) {
    if (method == "PUT") record$metadata <<- body$metadata
    record
  })
  update_benchmark_community_pages("benchmark", "<p>New</p>", "<p>New policy</p>", TRUE, "token", confirm = "benchmark")
  expect_length(list.files(fallback), 1L)
  expect_error(update_benchmark_community_pages("benchmark", "<p>New</p>", "<p>New policy</p>", TRUE, "token",
                 confirm = "benchmark", backup_directory = c("a", "b")), "single directory path")
})

## The catalog family sentence ----------------------------------------------------------------

test_that("the release catalog family sentence is added once per concept DOI", {
  once <- .with_family_sentence("Storage snapshot.", "10.5281/zenodo.900")
  expect_match(once, "Release catalog family: <a href=\"https://doi.org/10.5281/zenodo.900\">", fixed = TRUE)
  expect_identical(.with_family_sentence(once, "10.5281/zenodo.900"), once)
  expect_length(gregexpr("Release catalog family", once, fixed = TRUE)[[1]], 1L)
  # A different concept (another family) is a different sentence; a missing description gets one.
  expect_match(.with_family_sentence(once, "10.5281/zenodo.901"), "zenodo.901", fixed = TRUE)
  expect_match(.with_family_sentence(NULL, "10.5281/zenodo.900"), "^ Release catalog family")
  expect_match(.with_family_sentence("A link https://doi.org/10.5281/zenodo.900 inside.", "10.5281/zenodo.900"), "^A link https://doi.org/10.5281/zenodo.900 inside.$")
})

## Transport level: libcurl against a real socket server ----------------------------------------

# A base-R HTTP server (callr background process, 127.0.0.1 only for the client) that
# ignores Range and streams a body in paced chunks; it records how much it sent and
# whether the client closed the connection before the end.
range_ignoring_server <- function(port, ready, log, total) {
  socket <- serverSocket(port)
  on.exit(close(socket), add = TRUE)
  writeLines("ready", ready)
  connection <- socketAccept(socket, blocking = TRUE, open = "r+b", timeout = 60)
  on.exit(try(close(connection), silent = TRUE), add = TRUE)
  request <- raw()
  repeat {
    byte <- readBin(connection, "raw", 1L)
    if (!length(byte)) break
    request <- c(request, byte); n <- length(request)
    if (n >= 4L && identical(request[(n - 3L):n], as.raw(c(13, 10, 13, 10)))) break
  }
  header <- sprintf("HTTP/1.1 200 OK\r\nContent-Type: application/octet-stream\r\nContent-Length: %d\r\nConnection: close\r\n\r\n", total)
  writeBin(charToRaw(header), connection)
  sent <- 0
  outcome <- tryCatch({
    while (sent < total) {
      # The last chunk is capped so that exactly `total` bytes are announced and sent.
      size <- min(16384, total - sent)
      writeBin(raw(size), connection); sent <- sent + size; Sys.sleep(0.01)
      # A closed peer shows as a readable socket that yields no data.
      if (socketSelect(list(connection), FALSE, 0) && !length(readBin(connection, "raw", 1L))) stop("peer closed")
    }
    "complete"
  }, error = function(error) conditionMessage(error))
  saw_range <- grepl("\r\nrange: bytes=0-0", tolower(rawToChar(request)), fixed = TRUE)
  writeLines(c(outcome, format(sent, scientific = FALSE), as.character(saw_range)), log)
}

test_that("the range probe stops reading a 5 MB body that a real server streams although Range was sent", {
  skip_on_cran()
  skip_if_not_installed("callr")
  skip_if(getRversion() < "4.0.0", "serverSocket() needs R 4.0")
  server <- NULL
  for (attempt in 1:5) {
    port <- sample(20000:60000, 1L)
    ready <- withr::local_tempfile(); log <- withr::local_tempfile()
    server <- callr::r_bg(range_ignoring_server, args = list(port = port, ready = ready, log = log, total = 5000000L))
    for (i in 1:100) if (file.exists(ready) || !server$is_alive()) break else Sys.sleep(0.1)
    if (file.exists(ready)) break
    try(server$kill(), silent = TRUE); server <- NULL
  }
  skip_if(is.null(server), "no local port could be bound")
  withr::defer(if (server$is_alive()) server$kill())
  # libcurl sends even 127.0.0.1 requests to a configured http_proxy; keep the probe local.
  withr::local_envvar(no_proxy = "127.0.0.1")
  answer <- .resource_range_probe(sprintf("http://127.0.0.1:%d/archive.zip", port))
  # The response is 200 with the whole length announced; only the first 64 KiB are read.
  expect_identical(answer$status, 200L)
  expect_identical(answer$headers[["content-length"]], "5000000")
  expect_lte(answer$bytes, 65537L)
  expect_gt(answer$bytes, 0L)
  server$wait(30000)
  expect_false(server$is_alive())
  report <- readLines(log)
  expect_identical(report[3], "TRUE")                  # the request carried Range: bytes=0-0
  # Any outcome but "complete" (a FIN seen as end of input, or a read/write error) means the client
  # hung up before the end, after the server had sent only a small part of the body.
  expect_false(identical(report[1], "complete"))
  expect_lt(as.numeric(report[2]), 5000000)
})
