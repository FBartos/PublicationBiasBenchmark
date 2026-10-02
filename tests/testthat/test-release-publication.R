test_that("native licenses are retained before staging and unsupported fields are rejected", {
  rights <- list(list(id = "cc-by-4.0"))
  metadata <- list(rights = rights)
  response <- list(id = "12345", metadata = list(rights = rights))
  existing <- list()
  posted <- NULL
  local_mocked_bindings(.zenodo_request = function(method, path, token, sandbox = FALSE, body = NULL) {
    if (method == "GET" && startsWith(path, "user/")) return(list(hits = list(hits = existing)))
    if (method == "GET") return(existing[[1]])
    posted <<- body
    response
  })
  plan <- list(metadata = metadata, catalog = list(release = "test.1"), sandbox = FALSE)
  expect_identical(.create_component_record(plan, "Title", "Description", "test-token"), "12345")
  expect_identical(posted$metadata$rights, rights)
  expect_null(posted$metadata$licenses)
  response$metadata$rights <- NULL
  expect_error(.create_component_record(plan, "Title", "Description", "test-token"), "did not retain")
  existing <- list(list(id = "12345", metadata = list(title = "Title")))
  expect_error(.create_component_record(plan, "Title", "Description", "test-token"), "did not retain")
  existing[[1]]$metadata$rights <- rights
  expect_identical(.create_component_record(plan, "Title", "Description", "test-token"), "12345")
  plan$metadata <- list()
  expect_error(.create_component_record(plan, "Title", "Description", "test-token"), "Include a license")
  expect_error(legacy_plan_benchmark_release("test.1", list(), metadata = list(licenses = rights),
    state_directory = withr::local_tempdir()), "metadata\\$rights")
})

test_that("release verification checks licenses in saved staged records", {
  root <- withr::local_tempdir(); asset <- test_resource(root, "A.csv")
  rights <- list(list(id = "cc-by-4.0"))
  plan <- legacy_plan_benchmark_release("test.1", list(asset), conditions = test_catalog(list(asset))$conditions,
    metadata = list(rights = rights), state_directory = file.path(root, "state"))
  record <- list(metadata = list())
  local_mocked_bindings(
    .publication_state = function(plan) list(groups = list(`1` = list(record_id = "12345", published = TRUE))),
    .zenodo_request = function(method, path, ...) {
      if (!endsWith(path, "/files")) return(record)
      list(entries = list(list(key = asset$filename, status = "completed",
        checksum = paste0("md5:", asset$md5), size = asset$size)))
    })
  expect_error(verify_benchmark_release(plan, "test-token"), "did not retain")
  record$metadata$rights <- rights
  expect_true(verify_benchmark_release(plan, "test-token"))
})

test_that("plans reuse existing bytes and require explicit corrections", {
  root <- withr::local_tempdir(); a <- test_resource(root, "A.csv")
  metadata <- list(creators = list(list(person_or_org = list(name = "Tester", type = "organizational"))))
  base <- test_catalog(list(a))
  plan <- legacy_plan_benchmark_release("test.2", list(a), previous = base, metadata = metadata,
                                 state_directory = file.path(root, "unchanged"))
  expect_length(plan$groups, 0)
  expect_identical(plan$catalog$assets[[1]]$record_id, "12345")
  corrected <- test_resource(root, "A.csv", ids = 3:4)
  expect_error(legacy_plan_benchmark_release("test.2", list(corrected), previous = base, metadata = metadata,
    state_directory = file.path(root, "correction")), "Explicit replacement")
  plan <- legacy_plan_benchmark_release("test.2", list(corrected), previous = base, replace = a$id,
    metadata = metadata, state_directory = file.path(root, "correction"))
  expect_length(plan$groups, 1)
  expect_identical(base$assets[[1]]$sha256, a$sha256)
  overlap <- test_resource(root, "other.csv", ids = 4:5)
  expect_error(legacy_plan_benchmark_release("test.3", list(corrected, overlap), conditions = base$conditions,
    metadata = metadata, state_directory = file.path(root, "overlap")), "Overlapping")
  conditions <- base$conditions; conditions$no_bias$mean_effect[1] <- 1
  expect_error(legacy_plan_benchmark_release("test.3", list(), previous = base, conditions = conditions,
    metadata = metadata, state_directory = file.path(root, "changed-conditions")), "frozen condition")
})

test_that("packing respects both file and byte quotas and rejects changed sources", {
  root <- withr::local_tempdir(); assets <- lapply(1:5, function(i) test_resource(root, paste0(i, ".csv"), ids = (2*i-1):(2*i)))
  plan <- legacy_plan_benchmark_release("test.1", assets, conditions = test_catalog(assets)$conditions,
    metadata = list(), state_directory = file.path(root, "state"), max_files = 2)
  expect_equal(lengths(plan$groups), c(2L, 2L, 1L))
  expect_error(legacy_plan_benchmark_release("test.1", assets, conditions = plan$catalog$conditions,
    metadata = list(), state_directory = file.path(root, "small"), max_bytes = 1), "file exceeds")
  writeLines("changed", assets[[1]]$local_path)
  expect_error(legacy_plan_benchmark_release("test.1", assets, conditions = plan$catalog$conditions,
    metadata = list(), state_directory = file.path(root, "changed")), "Unverified local")
})

test_that("interrupted commits resume without uploading completed files or publishing twice", {
  root <- withr::local_tempdir(); asset <- test_resource(root, "A.csv")
  plan <- legacy_plan_benchmark_release("test.1", list(asset), conditions = test_catalog(list(asset))$conditions,
    metadata = list(), state_directory = file.path(root, "state"))
  entries <- list(); remote <- list(); published <- character(); uploads <- 0L; creations <- 0L; interrupt <- TRUE
  local_mocked_bindings(
    .create_component_record = function(...) { creations <<- creations + 1L; as.character(creations) },
    .record_is_published = function(plan, record_id, token) record_id %in% published,
    .zenodo_upload = function(path, record_id, filename, ...) {
      uploads <<- uploads + 1L; remote[[paste(record_id, filename)]] <<- path
    },
    .zenodo_request = function(method, path, token, sandbox = FALSE, body = NULL) {
      record <- strsplit(path, "/", fixed = TRUE)[[1]][2]
      if (method == "GET" && endsWith(path, "/files")) return(list(entries = unname(entries[[record]])))
      if (method == "POST" && endsWith(path, "/files")) {
        filename <- body[[1]]$key; entries[[record]][[filename]] <<- list(key = filename, status = "pending"); return(NULL)
      }
      if (endsWith(path, "/commit")) {
        filename <- utils::URLdecode(strsplit(path, "/", fixed = TRUE)[[1]][5])
        source <- remote[[paste(record, filename)]]
        entry <- list(key = filename, status = "completed", checksum = paste0("md5:", unname(tools::md5sum(source))), size = file.info(source)$size)
        entries[[record]][[filename]] <<- entry
        if (interrupt) { interrupt <<- FALSE; stop("lost commit response") }
        return(entry)
      }
      if (endsWith(path, "/publish")) { published <<- c(published, record); return(NULL) }
      if (method == "GET") return(list(is_published = record %in% published, pids = list(doi = list(identifier = paste0("10.test/", record)))))
      stop("Unexpected mock request")
    },
    .resource_download = function(url, destination, progress) {
      parts <- strsplit(url, "/", fixed = TRUE)[[1]]
      file.copy(remote[[paste(parts[6], utils::URLdecode(parts[8]))]], destination, overwrite = TRUE)
    })
  expect_error(stage_benchmark_release(plan, "test-token"), "lost commit")
  expect_equal(uploads, 1L)
  suppressMessages(stage_benchmark_release(plan, "test-token"))
  expect_equal(uploads, 1L); expect_equal(creations, 1L)
  result <- suppressMessages(publish_benchmark_release(plan, "test-token"))
  expect_equal(result$doi, "10.test/2"); expect_equal(uploads, 2L)
  expect_equal(published, c("1", "2"))
  expect_equal(suppressMessages(publish_benchmark_release(plan, "test-token")), result)
  expect_equal(uploads, 2L); expect_equal(creations, 2L)
  catalog <- jsonlite::read_json(file.path(plan$state_directory, "release.json"))
  expect_null(catalog$assets[[1]]$local_path)
})

test_that("local distributed results remain separate and measures are partitioned by method", {
  root <- withr::local_tempdir(); dir.create(file.path(root, "no_bias", "results"), recursive = TRUE)
  dir.create(file.path(root, "no_bias", "measures"))
  test_resource(file.path(root, "no_bias", "results"), "A-1.csv", ids = 1:2)
  test_resource(file.path(root, "no_bias", "results"), "A-2.csv", ids = 3:4)
  measures <- data.frame(method = c("A", "B"), method_setting = "default", condition_id = 1,
    bias = c(.1, .2), bias_mcse = .01, n_valid = 4)
  utils::write.csv(measures, file.path(root, "no_bias", "measures", "bias.csv"), row.names = FALSE)
  local_mocked_bindings(.get_path = function() root)
  expect_equal(retrieve_dgm_results("no_bias", "A", source = "local")$repetition_id, 1:4)
  assets <- prepare_benchmark_resources("no_bias", output_directory = file.path(root, "prepared"), package_version = "0.4.0")
  expect_equal(sum(vapply(assets, function(x) x$kind == "results", logical(1))), 2L)
  per_method <- Filter(function(x) x$kind == "measures", assets)
  expect_length(per_method, 2)
  expect_equal(sort(vapply(per_method, `[[`, character(1), "method")), c("A", "B"))
  for (asset in per_method) expect_equal(unique(.read_resource_csv(asset$local_path)$method), asset$method)
})

test_that("batch staging resumes partial records and leaves completed files untouched", {
  root <- withr::local_tempdir()
  assets <- lapply(1:3, function(i) test_resource(root, paste0(i, ".csv"), ids = (2*i-1):(2*i)))
  plan <- list(sandbox = FALSE)
  entries <- list(); batches <- list(); uploads <- character(); deleted <- character(); reads <- 0L; interrupt <- TRUE
  local_mocked_bindings(
    .zenodo_upload = function(path, record_id, filename, ...) { uploads <<- c(uploads, filename) },
    .zenodo_request = function(method, path, token, sandbox = FALSE, body = NULL) {
      filename <- utils::URLdecode(tail(strsplit(path, "/", fixed = TRUE)[[1]], 1L))
      if (method == "GET") { reads <<- reads + 1L; return(list(entries = unname(entries))) }
      if (method == "DELETE") { deleted <<- c(deleted, filename); entries[[filename]] <<- NULL; return(NULL) }
      if (endsWith(path, "/files")) {
        batches[[length(batches) + 1L]] <<- vapply(body, `[[`, character(1), "key")
        for (file in body) entries[[file$key]] <<- list(key = file$key, status = "pending")
        return(NULL)
      }
      filename <- utils::URLdecode(strsplit(path, "/", fixed = TRUE)[[1]][5])
      asset <- Filter(function(x) x$filename == filename, assets)[[1]]
      entry <- list(key = filename, status = "completed", checksum = paste0("md5:", asset$md5), size = asset$size)
      entries[[filename]] <<- entry
      if (interrupt) { interrupt <<- FALSE; stop("lost batch commit response") }
      entry
    })
  expect_error(.stage_files(plan, "12345", assets, "test-token"), "lost batch commit")
  suppressMessages(.stage_files(plan, "12345", assets, "test-token"))
  expect_equal(reads, 2L)
  expect_equal(batches, list(c("1.csv", "2.csv", "3.csv")))
  expect_equal(uploads, c("1.csv", "2.csv", "3.csv"))
  expect_equal(deleted, character())
  entries[["2.csv"]]$checksum <- "md5:changed"
  expect_error(.stage_files(plan, "12345", assets, "test-token"), "refusing to overwrite")
  expect_equal(uploads, c("1.csv", "2.csv", "3.csv"))
  expect_equal(deleted, character())
})

test_that("explicit rate-limit responses retry rejected mutations", {
  calls <- 0L
  testthat::local_mocked_bindings(VERB = function(...) {
    calls <<- calls + 1L
    structure(list(status_code = if (calls == 1L) 429L else 201L,
      headers = structure(list(`retry-after` = "0", `content-type` = "application/json"), class = "insensitive"),
      content = charToRaw('{"id":"12345"}')), class = "response")
  }, .package = "httr")
  expect_equal(.zenodo_request("POST", "records", "test-token")$id, "12345")
  expect_equal(calls, 2L)
  expect_equal(.zenodo_retry_delay(list(), 429L, 1L), 60)
  expect_equal(.zenodo_retry_delay(list(`retry-after` = "12"), 429L, 1L), 12)
  expect_equal(.zenodo_retry_delay(list(), 503L, 4L), 8)
})

test_that("stored pending bytes can be committed without another upload", {
  root <- withr::local_tempdir()
  asset <- test_resource(root, "pending.csv")
  entry <- list(key = asset$filename, status = "pending", size = asset$size, checksum = paste0("md5:", asset$md5))
  commits <- 0L
  local_mocked_bindings(
    .zenodo_upload = function(...) stop("Unexpected re-upload"),
    .zenodo_request = function(method, path, ...) {
      if (method == "GET") return(list(entries = list(entry)))
      if (method == "POST" && endsWith(path, "/commit")) {
        commits <<- commits + 1L; entry$status <- "completed"; return(entry)
      }
      stop("Unexpected initialization or deletion")
    })
  suppressMessages(.stage_files(list(sandbox = FALSE), "12345", list(asset), "test-token"))
  expect_equal(commits, 1L)
})

test_that("native record metadata and JSON file responses are parsed consistently", {
  accepts <- character()
  testthat::local_mocked_bindings(VERB = function(method, url, config, ...) {
    accept <- unname(config$headers["Accept"])
    accepts <<- c(accepts, accept)
    structure(list(status_code = 200L,
      headers = structure(list(`content-type` = accept), class = "insensitive"),
      content = charToRaw('{"id":"12345"}')), class = "response")
  }, .package = "httr")
  expect_equal(.zenodo_request("GET", "records/12345/draft", "test-token")$id, "12345")
  expect_equal(.zenodo_request("GET", "records/12345/draft/files", "test-token")$id, "12345")
  expect_equal(accepts, c("application/vnd.inveniordm.v1+json", "application/json"))
})

test_that("prepared measures use extension-free IDs and CSV member filenames", {
  root <- withr::local_tempdir()
  results <- file.path(root, "no_bias", "results"); measures <- file.path(root, "no_bias", "measures")
  dir.create(results, recursive = TRUE); dir.create(measures)
  test_resource(results, "A-1.csv", ids = 1:2)
  for (file in c("bias.csv", "bias-replacement.csv"))
    utils::write.csv(data.frame(method = c("A", "B"), method_setting = "default", condition_id = 1,
      bias = c(.1, .2), bias_mcse = .01, n_valid = 4, replaced = if (grepl("replacement", file)) "C-default=1;" else NA),
      file.path(measures, file), row.names = FALSE)
  local_mocked_bindings(.get_path = function() root)
  assets <- prepare_benchmark_resources("no_bias", output_directory = file.path(root, "prepared"), package_version = "0.4.0")
  by_kind <- function(kind) Filter(function(x) x$kind == kind, assets)
  ids <- vapply(by_kind("measures"), `[[`, character(1), "id")
  files <- vapply(by_kind("measures"), `[[`, character(1), "filename")
  expect_setequal(ids, c("no_bias/measures/A-default", "no_bias/measures/B-default",
                         "no_bias/measures/A-default-replacement", "no_bias/measures/B-default-replacement"))
  expect_setequal(files, c("no_bias__measures__A-default.csv", "no_bias__measures__B-default.csv",
                           "no_bias__measures__A-default-replacement.csv", "no_bias__measures__B-default-replacement.csv"))
  expect_equal(files[match(c("no_bias/measures/A-default", "no_bias/measures/B-default-replacement"), ids)],
               c("no_bias__measures__A-default.csv", "no_bias__measures__B-default-replacement.csv"))
  # Other kinds keep their relative path (with extension) in the ID.
  expect_equal(by_kind("results")[[1]]$id, "no_bias/results/A-1.csv")
  expect_equal(by_kind("results")[[1]]$filename, "no_bias__results__A-1.csv")
  expect_true(all(vapply(assets, function(x) .safe_filename(x$filename), logical(1))))
})

test_that("prepared measures keep methods whose identifiers share a joined spelling apart", {
  root <- withr::local_tempdir(); measures <- file.path(root, "no_bias", "measures")
  dir.create(measures, recursive = TRUE)
  utils::write.csv(data.frame(method = c("a.b", "a"), method_setting = c("c", "b.c"), condition_id = 1,
    bias = c(.1, .2), bias_mcse = .01, n_valid = 4), file.path(measures, "bias.csv"), row.names = FALSE)
  local_mocked_bindings(.get_path = function() root)
  assets <- prepare_benchmark_resources("no_bias", kinds = "measures", output_directory = file.path(root, "prepared"))
  expect_length(assets, 2L)
  for (asset in assets) {
    table <- .read_resource_csv(asset$local_path)
    expect_equal(nrow(table), 1L)
    expect_equal(c(table$method, table$method_setting), c(asset$method, asset$method_setting))
    expect_equal(asset$id, paste0("no_bias/measures/", asset$method, "-", asset$method_setting))
    expect_equal(unlist(asset$condition_ids), 1L)
  }
  expect_setequal(vapply(assets, `[[`, character(1), "method"), c("a.b", "a"))
})

test_that("frozen conditions are checked by one helper for both plan schemas", {
  root <- withr::local_tempdir(); a <- test_resource(root, "A.csv")
  base <- test_catalog(list(a)); base$conditions$no_bias$label <- c("x", "y")
  plan <- function(conditions, archive = FALSE) plan_benchmark_release("test.2", list(), previous = base, conditions = conditions,
    metadata = list(), state_directory = withr::local_tempdir(), archive = archive, community = "test-community")
  edit <- function(change) { x <- base$conditions; x$no_bias <- change(x$no_bias); x }
  cases <- list(
    missing = list(list(), "Frozen conditions for published DGM 'no_bias' are missing."),
    other_dgm = list(list(other = base$conditions$no_bias), "Frozen conditions for published DGM 'no_bias' are missing."),
    added_row = list(edit(function(x) rbind(x, data.frame(condition_id = 3L, mean_effect = 0, label = "z"))),
                     "New conditions are not allowed for a published DGM; create a new DGM."),
    removed_row = list(edit(function(x) x[1, ]), "New conditions are not allowed for a published DGM; create a new DGM."),
    replaced_row = list(edit(function(x) { x$condition_id[2] <- 5L; x }), "New conditions are not allowed for a published DGM"),
    changed_value = list(edit(function(x) { x$mean_effect[1] <- 1; x }),
                         "Existing frozen condition definitions cannot change between releases."),
    added_column = list(edit(function(x) { x$extra <- 1; x }), "Existing frozen condition definitions cannot change between releases."),
    removed_column = list(edit(function(x) x[c("condition_id", "mean_effect")]),
                          "Existing frozen condition definitions cannot change between releases."))
  for (name in names(cases)) for (archive in c(FALSE, TRUE))
    expect_error(plan(cases[[name]][[1]], archive), cases[[name]][[2]], fixed = TRUE, info = paste(name, archive))
  expect_error(.check_frozen_conditions(base, edit(function(x) x[1, ])), "New conditions", fixed = TRUE)
  expect_true(.check_frozen_conditions(NULL, list()))
  # Reordered rows and additional DGMs are allowed.
  expect_true(.check_frozen_conditions(base, edit(function(x) x[2:1, ])))
  expect_true(.check_frozen_conditions(base, c(base$conditions, list(new_dgm = data.frame(condition_id = 1L, mean_effect = 0)))))
  expect_identical(plan(base$conditions)$catalog$conditions, base$conditions)
})
