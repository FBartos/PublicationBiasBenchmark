test_that("downloads select only one DGM and method, retaining unchanged cache files", {
  root <- withr::local_tempdir(); originals <- withr::local_tempdir()
  assets <- list(test_resource(originals, "A-1.csv"), test_resource(originals, "B.csv", method = "B"),
    test_resource(originals, "data.csv", "data"), test_resource(originals, "A-measures.csv", "measures"))
  calls <- character()
  local_mocked_bindings(.get_path = function() root,
    .resource_download = function(url, destination, progress) {
      calls <<- c(calls, url)
      source <- Filter(function(a) endsWith(url, paste0(a$filename, "/content")), assets)[[1]]
      file.copy(source$local_path, destination, overwrite = TRUE)
    })
  catalog <- test_catalog(assets)
  expect_true(download_dgm_results("no_bias", method = "A", release = catalog, progress = FALSE))
  expect_length(calls, 1)
  expect_match(calls[1], "A-1.csv/content", fixed = TRUE)
  expect_false(file.exists(.asset_cache_path(assets[[2]])))
  expect_false(file.exists(.asset_cache_path(assets[[3]])))
  expect_false(file.exists(.asset_cache_path(assets[[4]])))
  expect_equal(retrieve_dgm_results("no_bias", method = "A", release = catalog)$repetition_id, 1:2)
  expect_true(download_dgm_results("no_bias", method = "A", release = catalog, progress = FALSE))
  expect_length(calls, 1)
  next_shard <- test_resource(originals, "A-2.csv", ids = 3:4)
  assets <- c(assets, list(next_shard)); updated <- test_catalog(assets, "test.2")
  download_dgm_results("no_bias", method = "A", release = updated, progress = FALSE)
  expect_length(calls, 2)
  expect_equal(retrieve_dgm_results("no_bias", method = "A", release = updated)$repetition_id, 1:4)
  expect_equal(retrieve_dgm_results("no_bias", method = "A", release = catalog)$repetition_id, 1:2)
  download_dgm_datasets("no_bias", release = catalog, progress = FALSE)
  expect_equal(nrow(retrieve_dgm_dataset("no_bias", 1, 2, release = catalog)), 2)
  download_dgm_measures("no_bias", method = "A", release = catalog, progress = FALSE)
  expect_named(retrieve_dgm_measures("no_bias", "bias", "A", release = catalog),
    c("method", "method_setting", "condition_id", "bias", "bias_mcse", "n_valid"))
  expect_error(download_dgm_results("no_bias", method = "absent", release = catalog), "No resources")
})

test_that("failed and same-size corrupt transfers never become verified resources", {
  root <- withr::local_tempdir(); source <- withr::local_tempfile()
  writeBin(charToRaw("complete"), source)
  target <- file.path(root, "resource.csv"); sha <- digest::digest(file = source, algo = "sha256")
  local_mocked_bindings(.resource_download = function(url, destination, progress) writeBin(charToRaw("corrupt!"), destination))
  expect_error(.fetch_verified("https://example.test", target, sha, size = 8, max_try = 1), "Could not download and verify")
  expect_false(file.exists(target)); expect_length(list.files(root), 0)
  file.copy(source, target)
  expect_error(.fetch_verified("https://example.test", target, sha, size = 8, max_try = 1, overwrite = TRUE), "Could not download")
  expect_true(.file_verified(target, sha, 8))
  writeBin(charToRaw("corrupt!"), target)
  expect_false(.file_verified(target, sha, 8))
  local_mocked_bindings(.resource_download = function(url, destination, progress) file.copy(source, destination, overwrite = TRUE))
  expect_true(.fetch_verified("https://example.test", target, sha, size = 8, max_try = 1))
  expect_true(.file_verified(target, sha, 8))
})

test_that("overlaps and missing coverage cannot silently produce partial results", {
  root <- withr::local_tempdir(); originals <- withr::local_tempdir()
  assets <- list(test_resource(originals, "A.csv"), test_resource(originals, "overlap.csv", ids = 2:3))
  local_mocked_bindings(.get_path = function() root)
  for (asset in assets) { path <- .asset_cache_path(asset); dir.create(dirname(path), recursive = TRUE); file.copy(asset$local_path, path) }
  expect_error(retrieve_dgm_results("no_bias", release = test_catalog(assets)), "overlapping")
  expect_error(retrieve_dgm_results("no_bias", repetition_id = 3, release = test_catalog(assets[1])), "unavailable")
  expect_error(retrieve_dgm_results("no_bias", condition_id = 2, release = test_catalog(assets[1])), "unavailable")
  unlink(.asset_cache_path(assets[[1]]))
  expect_error(retrieve_dgm_results("no_bias", release = test_catalog(assets[1])), "Missing or unverified")
})

test_that("catalog serialization preserves frozen list columns and rejects unsafe references", {
  root <- withr::local_tempdir(); asset <- test_resource(root, "A.csv")
  catalog <- test_catalog(list(asset)); catalog$conditions$no_bias$sample_sizes <- list(c(10, 20), c(40, 80))
  path <- file.path(root, "catalog.json")
  jsonlite::write_json(catalog, path, auto_unbox = TRUE, dataframe = "rows", null = "null")
  parsed <- benchmark_catalog(path)
  expect_equal(unclass(.catalog_conditions(parsed, "no_bias")$sample_sizes), list(c(10, 20), c(40, 80)))
  parsed$assets[[1]]$filename <- "../other.csv"
  expect_error(benchmark_catalog(parsed), "Invalid benchmark asset")
  expect_false(.safe_filename("C:other.csv")); expect_false(.safe_filename("NUL.csv"))
  expect_error(.zenodo_file_url("../123", "A.csv"), "Invalid Zenodo")
})

test_that("wide measures retain the original condition availability of each metric", {
  root <- withr::local_tempdir(); originals <- withr::local_tempdir()
  path <- file.path(originals, "sparse.csv")
  utils::write.csv(data.frame(method = "A", method_setting = "default", condition_id = 1:2,
    bias = c(.1, .2), bias_mcse = .01, n_valid_bias = 2,
    power = c(NA, .8), power_mcse = c(NA, .1), n_valid_power = c(NA, 2)), path, row.names = FALSE)
  asset <- benchmark_resource(path, "no_bias", "measures", "A", "default", measures = c("bias", "power"))
  asset$measure_conditions <- list(bias = list(1L, 2L), power = list(2L))
  catalog <- test_catalog(list(asset)); local_mocked_bindings(.get_path = function() root)
  target <- .asset_cache_path(asset); dir.create(dirname(target), recursive = TRUE); file.copy(path, target)
  expect_equal(retrieve_dgm_measures("no_bias", "power", release = catalog)$condition_id, 2L)
  expect_equal(retrieve_dgm_measures("no_bias", "bias", release = catalog)$condition_id, 1:2)
  expect_error(retrieve_dgm_measures("no_bias", "power", condition_id = 1, release = catalog), "unavailable")
  catalog$assets[[1]]$measure_conditions$bias <- NULL
  expect_equal(retrieve_dgm_measures("no_bias", "bias", release = catalog)$condition_id, 1:2)
  expect_equal(retrieve_dgm_measures("no_bias", "power", release = catalog)$condition_id, 2L)
})
test_that("benchmark conditions come from the selected release", {
  root <- withr::local_tempdir(); asset <- test_resource(root, "A.csv")
  catalog <- test_catalog(list(asset))
  catalog$conditions$no_bias$mean_effect <- c(10, 20)
  expect_equal(benchmark_conditions("no_bias", catalog)$mean_effect, c(10, 20))
  expect_false(identical(benchmark_conditions("no_bias", catalog), dgm_conditions("no_bias")))
})

test_that("public rate limits respect Retry-After before repairing a cached file", {
  root <- withr::local_tempdir(); source <- file.path(root, "source")
  writeBin(charToRaw("verified"), source)
  sha <- digest::digest(file = source, algo = "sha256", serialize = FALSE)
  calls <- 0L; waits <- numeric()
  local_mocked_bindings(.resource_download = function(url, destination, progress) {
    calls <<- calls + 1L
    if (calls == 1L) stop(structure(list(message = "rate limited", call = NULL, status = 429L, retry_delay = 60),
      class = c("resource_http_error", "error", "condition")))
    file.copy(source, destination)
  }, .resource_retry_wait = function(time) waits <<- c(waits, time))
  expect_true(.fetch_verified("https://example.test/file", file.path(root, "target"), sha, 8, max_try = 2L))
  expect_equal(calls, 2L); expect_equal(waits, 60)
})

http_failure <- function(status, retry_delay = NULL) structure(list(
  message = paste0("Public resource download failed (HTTP ", status, ")."), call = NULL,
  status = status, retry_delay = retry_delay), class = c("resource_http_error", "error", "condition"))
curl_failure <- function(class, message = "transfer failed") structure(list(message = message, call = NULL),
  class = c(class, "curl_error", "error", "condition"))

# Run .fetch_verified against a scripted sequence of failures; a NULL entry
# writes the verified bytes, "corrupt" writes same-size wrong bytes.
scripted_fetch <- function(failures, max_try = 10, retry_not_found = 0L,
                           url = "https://zenodo.org/api/records/1/files/A.csv/content") {
  root <- withr::local_tempdir(); source <- file.path(root, "source")
  writeBin(charToRaw("verified"), source)
  sha <- digest::digest(file = source, algo = "sha256", serialize = FALSE)
  state <- new.env(); state$calls <- 0L; state$waits <- numeric()
  target <- file.path(root, "target.csv")
  testthat::local_mocked_bindings(
    .resource_download = function(url, destination, progress) {
      state$calls <- state$calls + 1L
      failure <- if (state$calls <= length(failures)) failures[[state$calls]] else NULL
      if (identical(failure, "corrupt")) return(writeBin(charToRaw("corrupt!"), destination))
      if (!is.null(failure)) stop(failure)
      file.copy(source, destination, overwrite = TRUE)
    }, .resource_retry_wait = function(delay) state$waits <- c(state$waits, delay))
  result <- tryCatch(.fetch_verified(url, target, sha, size = 8, max_try = max_try, retry_not_found = retry_not_found),
                     error = function(error) error)
  list(calls = state$calls, waits = state$waits, result = result, installed = file.exists(target),
       verified = .file_verified(target, sha, 8))
}

test_that("a missing or withdrawn file stops after one attempt with a precise message", {
  for (status in c(404L, 410L)) {
    run <- scripted_fetch(rep(list(http_failure(status)), 10))
    expect_equal(run$calls, 1L); expect_length(run$waits, 0L)
    expect_s3_class(run$result, "error")
    expect_match(conditionMessage(run$result), sprintf("target.csv is not available on Zenodo (HTTP %d)", status), fixed = TRUE)
    expect_match(conditionMessage(run$result), "list_benchmark_releases()", fixed = TRUE)
    expect_match(conditionMessage(run$result), "Underlying error: Public resource download failed", fixed = TRUE)
    expect_false(run$installed)
  }
})

test_that("other permanent HTTP and curl failures stop after one attempt", {
  for (status in c(400L, 401L, 403L, 405L, 451L, 501L, 505L)) {
    run <- scripted_fetch(rep(list(http_failure(status)), 10))
    expect_equal(run$calls, 1L)
    expect_match(conditionMessage(run$result), "permanent error")
  }
  for (class in .permanent_curl_errors) {
    run <- scripted_fetch(rep(list(curl_failure(class, "TLS problem")), 10))
    expect_equal(run$calls, 1L)
    expect_match(conditionMessage(run$result), "TLS problem", fixed = TRUE)
  }
})

test_that("every curl error class used by the retry policy exists in the installed curl", {
  expect_true(all(c(.permanent_curl_errors, .dns_curl_error) %in% curl:::libcurl_error_codes))
})

test_that("DNS failures stop after three attempts with backoff of one and two seconds", {
  run <- scripted_fetch(rep(list(curl_failure("curl_error_couldnt_resolve_host", "Could not resolve host")), 10))
  expect_equal(run$calls, 3L); expect_equal(run$waits, c(1, 2))
  expect_match(conditionMessage(run$result), "Cannot reach zenodo.org; check the network connection.", fixed = TRUE)
  expect_match(conditionMessage(run$result), "Could not resolve host", fixed = TRUE)
  run <- scripted_fetch(rep(list(curl_failure("curl_error_couldnt_resolve_host")), 10), max_try = 2)
  expect_equal(run$calls, 2L)
  expect_match(conditionMessage(run$result), "Cannot reach zenodo.org")
  run <- scripted_fetch(list(curl_failure("curl_error_couldnt_resolve_host")))
  expect_true(run$result); expect_equal(run$calls, 2L)
})

test_that("server errors, timeouts and corrupt transfers back off and retry", {
  run <- scripted_fetch(list(http_failure(503L), http_failure(502L), http_failure(500L)))
  expect_true(run$result); expect_equal(run$calls, 4L); expect_equal(run$waits, c(1, 2, 4))
  run <- scripted_fetch(list(http_failure(408L), http_failure(425L), http_failure(504L)))
  expect_true(run$result); expect_equal(run$calls, 4L)
  run <- scripted_fetch(list("corrupt", curl_failure("curl_error_operation_timedout"), "corrupt"))
  expect_true(run$result); expect_equal(run$calls, 4L); expect_equal(run$waits, c(1, 2, 4))
  expect_true(run$verified)
  run <- scripted_fetch(rep(list(http_failure(503L)), 10), max_try = 4)
  expect_equal(run$calls, 4L); expect_equal(run$waits, c(1, 2, 4))
  expect_match(conditionMessage(run$result), "Could not download and verify 'target.csv' after 4 attempts")
  expect_match(conditionMessage(run$result), "last error: Public resource download failed (HTTP 503)", fixed = TRUE)
  run <- scripted_fetch(rep(list("corrupt"), 10), max_try = 7)
  expect_equal(run$waits, c(1, 2, 4, 8, 16, 30))
})

test_that("rate limits wait for the server delay", {
  run <- scripted_fetch(list(http_failure(429L, retry_delay = 7), http_failure(429L, retry_delay = 11)))
  expect_true(run$result); expect_equal(run$waits, c(7, 11))
  run <- scripted_fetch(list(http_failure(429L)))
  expect_true(run$result); expect_equal(run$waits, 1)
})

test_that("retry_not_found retries missing files a bounded number of times but never withdrawn ones", {
  run <- scripted_fetch(rep(list(http_failure(404L)), 3), retry_not_found = 5L)
  expect_true(run$result); expect_equal(run$calls, 4L); expect_equal(run$waits, c(1, 2, 4))
  run <- scripted_fetch(rep(list(http_failure(404L)), 20), retry_not_found = 5L)
  expect_equal(run$calls, 6L); expect_equal(run$waits, c(1, 2, 4, 8, 16))
  expect_match(conditionMessage(run$result), "is not available on Zenodo (HTTP 404)", fixed = TRUE)
  run <- scripted_fetch(list(http_failure(403L), http_failure(403L)), retry_not_found = 5L)
  expect_true(run$result); expect_equal(run$calls, 3L)
  run <- scripted_fetch(rep(list(http_failure(410L)), 20), retry_not_found = 5L)
  expect_equal(run$calls, 1L); expect_match(conditionMessage(run$result), "HTTP 410")
  run <- scripted_fetch(rep(list(http_failure(404L)), 20), retry_not_found = 5L, max_try = 3)
  expect_equal(run$calls, 3L)
  run <- scripted_fetch(rep(list(http_failure(404L)), 20))
  expect_equal(run$calls, 1L)
  expect_error(.fetch_verified("https://example.test", tempfile(), "x", retry_not_found = -1), "retry_not_found")
})

test_that("safe_rbind keeps non-empty frames and handles empty inputs", {
  a <- data.frame(x = 1:2, y = c("a", "b")); b <- data.frame(x = 3L, z = TRUE)
  empty <- a[0, ]
  expect_null(safe_rbind(list()))
  expect_identical(safe_rbind(list(empty, empty)), empty)
  expect_identical(safe_rbind(list(empty)), empty)
  expect_equal(safe_rbind(list(a, empty, b)), safe_rbind(list(a, b)))
  expect_equal(safe_rbind(list(empty, a)), a)
  expect_equal(safe_rbind(list(a, empty)), a)
  both <- safe_rbind(list(a, b))
  expect_named(both, c("x", "y", "z")); expect_equal(both$x, 1:3)
  expect_equal(both$y, c("a", "b", NA)); expect_equal(both$z, c(NA, NA, TRUE))
  expect_equal(safe_rbind(list(empty, b, empty, a))$x, c(3L, 1L, 2L))
  expect_identical(safe_rbind(list(a)), a)
})

test_that("composite method keys are injective for separators inside identifiers", {
  expect_false(.method_key("a.b", "c") == .method_key("a", "b.c"))
  expect_false(.method_key("a/b", "c") == .method_key("a", "b/c"))
  expect_false(.method_key("a-b", "c") == .method_key("a", "b-c"))
  expect_false(.method_key("1:a", "b") == .method_key("1", "a/b"))
  expect_equal(.method_key(c("A", "B"), "x"), c("1:A/x", "1:B/x"))
  keys <- .method_key(rep(c("a", "a.b", "a/b"), each = 3), rep(c("b.c", "c", "b/c"), 3))
  expect_false(anyDuplicated(keys) > 0L)
})

test_that("requested coverage is checked per method and setting even when identifiers contain separators", {
  grid <- function(method, setting, conditions, repetitions = 1:2)
    data.frame(method = method, method_setting = setting, condition_id = rep(conditions, each = length(repetitions)),
               repetition_id = repetitions)
  # With "a.b"/"c" and "a"/"b.c" merged, condition 2 would appear to be covered.
  data <- rbind(grid("a.b", "c", 1:2), grid("a", "b.c", 1L))
  expect_error(.check_requested_coverage(data, conditions = 1:2), "Requested conditions are unavailable")
  expect_silent(.check_requested_coverage(data, conditions = 1L))
  expect_silent(.check_requested_coverage(data))
  data <- rbind(grid("a/b", "c", 1:2), grid("a", "b/c", 1L))
  expect_error(.check_requested_coverage(data, conditions = 1:2), "Requested conditions are unavailable")
  # Repetitions are checked per condition within each method/setting.
  data <- rbind(grid("A", "x", 1:2), grid("B", "x", 1:2, repetitions = 1:3))
  expect_silent(.check_requested_coverage(data, 1:2, 1:2))
  expect_error(.check_requested_coverage(data, 1:2, 1:3), "Requested repetitions are unavailable")
  data$repetition_id[data$method == "B" & data$condition_id == 2 & data$repetition_id == 2] <- 4L
  expect_error(.check_requested_coverage(data, 2, 2), "Requested repetitions are unavailable")
  expect_silent(.check_requested_coverage(data, 1, 2))
})

test_that("unknown conditions are rejected before any shard is read", {
  root <- withr::local_tempdir()
  results <- test_catalog(list(test_resource(root, "A.csv")))
  measures <- test_catalog(list(test_resource(root, "M.csv", "measures")))
  local_mocked_bindings(
    .read_catalog_assets = function(...) stop("A shard was read"),
    .read_catalog_asset = function(...) stop("A shard was read"),
    .cached_asset_files = function(...) stop("A shard was read"))
  expect_error(retrieve_dgm_results("no_bias", condition_id = 99, release = results), "Unknown archived condition IDs: 99")
  expect_error(retrieve_dgm_results("no_bias", condition_id = c(1, 99), release = results), "Unknown archived condition IDs: 99")
  expect_error(retrieve_dgm_measures("no_bias", condition_id = 99, release = measures), "Unknown archived condition IDs: 99")
  expect_error(retrieve_dgm_measures("no_bias", "bias", condition_id = 99, release = measures), "Unknown archived condition IDs: 99")
})

test_that("repetitions can be requested without conditions", {
  root <- withr::local_tempdir(); originals <- withr::local_tempdir()
  assets <- list(test_resource(originals, "A.csv", ids = 1:4), test_resource(originals, "B.csv", method = "B", ids = 1:2))
  local_mocked_bindings(.get_path = function() root)
  for (asset in assets) { path <- .asset_cache_path(asset); dir.create(dirname(path), recursive = TRUE); file.copy(asset$local_path, path) }
  catalog <- test_catalog(assets)
  expect_equal(nrow(retrieve_dgm_results("no_bias", method = "A", repetition_id = 3:4, release = catalog)), 2L)
  expect_equal(nrow(retrieve_dgm_results("no_bias", repetition_id = 1:2, release = catalog)), 4L)
  expect_error(retrieve_dgm_results("no_bias", repetition_id = 3, release = catalog), "unavailable")
})
