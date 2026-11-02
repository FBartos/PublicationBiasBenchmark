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
  expect_error(plan_benchmark_release("test.1", list(), metadata = list(licenses = rights),
    state_directory = withr::local_tempdir()), "metadata\\$rights")
})

test_that("release verification checks licenses in saved staged records", {
  skip_if_not_installed("zip")
  root <- withr::local_tempdir(); asset <- test_resource(root, "A.csv")
  rights <- list(list(id = "cc-by-4.0"))
  plan <- plan_benchmark_release("test.1", list(asset), conditions = test_catalog(list(asset))$conditions,
    metadata = list(rights = rights), state_directory = file.path(root, "state"), community = "test-community")
  staged <- .public_catalog(plan$catalog)
  staged$archives <- lapply(staged$archives, function(x) { x$record_id <- "12345"; x })
  staged$assets <- lapply(staged$assets, function(x) { x$record_id <- "12345"; x })
  .write_catalog_file(file.path(plan$state_directory, "release.json"), staged)
  record <- list(metadata = list(), is_published = TRUE)
  local_mocked_bindings(
    .zenodo_request = function(method, path, ...) {
      if (!endsWith(path, "/files")) return(record)
      list(entries = lapply(plan$catalog$archives, function(archive) list(key = archive$filename, status = "completed",
        checksum = paste0("md5:", archive$md5), size = archive$size)))
    })
  expect_error(verify_benchmark_release(plan, "test-token"), "did not retain")
  record$metadata$rights <- rights
  expect_true(verify_benchmark_release(plan, "test-token"))
})

test_that("plans reuse existing bytes and require explicit corrections", {
  skip_if_not_installed("zip")
  root <- withr::local_tempdir(); a <- test_resource(root, "A.csv")
  metadata <- list(rights = list(list(id = "cc-by-4.0")))
  first <- plan_benchmark_release("test.1", list(a), conditions = test_catalog(list(a))$conditions, metadata = metadata,
    state_directory = file.path(root, "first"), community = "test-community")
  base <- .public_catalog(first$catalog)
  base$publication <- list(catalog_record_id = "123", catalog_concept_doi = "10.test/concept")
  base$archives <- lapply(base$archives, function(x) { x$record_id <- "55"; x })
  base$assets <- lapply(base$assets, function(x) { x$record_id <- "55"; x })
  plan <- plan_benchmark_release("test.2", list(a), previous = base, metadata = metadata,
    state_directory = file.path(root, "unchanged"), community = "test-community")
  expect_length(plan$groups, 0)
  expect_identical(plan$catalog$assets[[1]]$record_id, "55")
  corrected <- test_resource(root, "A.csv", ids = 3:4)
  expect_error(plan_benchmark_release("test.2", list(corrected), previous = base, metadata = metadata,
    state_directory = file.path(root, "correction"), community = "test-community"), "Explicit replacement")
  plan <- plan_benchmark_release("test.2", list(corrected), previous = base, replace = a$id,
    metadata = metadata, state_directory = file.path(root, "correction"), community = "test-community")
  expect_length(plan$groups, 1)
  expect_identical(base$assets[[1]]$sha256, a$sha256)
  overlap <- test_resource(root, "other.csv", ids = 4:5)
  expect_error(plan_benchmark_release("test.3", list(corrected, overlap), conditions = base$conditions,
    metadata = metadata, state_directory = file.path(root, "overlap"), community = "test-community"), "Overlapping")
  conditions <- base$conditions; conditions$no_bias$mean_effect[1] <- 1
  expect_error(plan_benchmark_release("test.3", list(), previous = base, conditions = conditions,
    metadata = metadata, state_directory = file.path(root, "changed-conditions"), community = "test-community"), "frozen condition")
})

test_that("planning rejects a source file that changed after it was described", {
  root <- withr::local_tempdir(); assets <- lapply(1:2, function(i) test_resource(root, paste0(i, ".csv"), ids = (2*i-1):(2*i)))
  writeLines("changed", assets[[1]]$local_path)
  expect_error(plan_benchmark_release("test.1", assets, conditions = test_catalog(assets)$conditions,
    metadata = list(), state_directory = file.path(root, "changed"), community = "test-community"), "Unverified local")
  expect_false(dir.exists(file.path(root, "changed")))
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
  plan <- list(sandbox = FALSE, state_directory = file.path(root, "state"))
  local_held_lock(plan)
  entries <- list(); batches <- list(); uploads <- character(); deleted <- character(); reads <- 0L; interrupt <- TRUE
  local_mocked_bindings(
    .zenodo_upload = function(path, record_id, filename, ...) { uploads <<- c(uploads, filename) },
    .zenodo_request = function(method, path, token, sandbox = FALSE, body = NULL) {
      filename <- utils::URLdecode(tail(strsplit(path, "/", fixed = TRUE)[[1]], 1L))
      if (method == "GET") { reads <<- reads + 1L; return(list(entries = unname(entries))) }
      if (method == "DELETE") { deleted <<- c(deleted, filename); entries[[filename]] <<- NULL; return(NULL) }
      if (endsWith(path, "/files")) {
        batches[[length(batches) + 1L]] <<- vapply(body, `[[`, character(1), "key")
        for (file in body) entries[[file$key]] <<- list(key = file$key, status = "pending", created = paste0("created-", file$key))
        return(list(entries = unname(entries[vapply(body, `[[`, character(1), "key")])))
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
  plan <- list(sandbox = FALSE, state_directory = file.path(root, "state"))
  local_held_lock(plan)
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
  suppressMessages(.stage_files(plan, "12345", list(asset), "test-token"))
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

test_that("frozen conditions are checked by one helper", {
  skip_if_not_installed("zip")
  root <- withr::local_tempdir(); a <- test_resource(root, "A.csv")
  base <- test_catalog(list(a)); base$conditions$no_bias$label <- c("x", "y")
  plan <- function(conditions) plan_benchmark_release("test.2", list(), previous = base, conditions = conditions,
    metadata = list(), state_directory = withr::local_tempdir(), community = "test-community",
    catalog_record_id = "123", catalog_concept_doi = "10.test/concept")
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
                          "Existing frozen condition definitions cannot change between releases."),
    # Without condition_id the entry redefines the conditions; it does not add new ones.
    no_condition_id = list(edit(function(x) x[setdiff(names(x), "condition_id")]),
                           "Existing frozen condition definitions cannot change between releases."),
    rows_without_condition_id = list(edit(function(x) list(list(mean_effect = 0, label = "x"), list(mean_effect = 0, label = "y"))),
                                     "Existing frozen condition definitions cannot change between releases."),
    renamed_column = list(edit(function(x) { names(x)[names(x) == "label"] <- "tag"; x }),
                          "Existing frozen condition definitions cannot change between releases."))
  for (name in names(cases))
    expect_error(plan(cases[[name]][[1]]), cases[[name]][[2]], fixed = TRUE, info = name)
  expect_error(.check_frozen_conditions(base, edit(function(x) x[1, ])), "New conditions", fixed = TRUE)
  expect_true(.check_frozen_conditions(NULL, list()))
  # Reordered rows and additional DGMs are allowed.
  expect_true(.check_frozen_conditions(base, edit(function(x) x[2:1, ])))
  expect_true(.check_frozen_conditions(base, c(base$conditions, list(new_dgm = data.frame(condition_id = 1L, mean_effect = 0)))))
  expect_identical(plan(base$conditions)$catalog$conditions, base$conditions)
})

test_that("a prepared measures file is verified before it is reused, and a corrupted one is replaced", {
  root <- withr::local_tempdir(); measures <- file.path(root, "no_bias", "measures")
  dir.create(measures, recursive = TRUE)
  utils::write.csv(data.frame(method = c("A", "B"), method_setting = "default", condition_id = 1,
    bias = c(.1, .2), bias_mcse = .01, n_valid = 4), file.path(measures, "bias.csv"), row.names = FALSE)
  local_mocked_bindings(.get_path = function() root)
  prepared <- file.path(root, "prepared")
  prepare <- function() prepare_benchmark_resources("no_bias", kinds = "measures", output_directory = prepared)
  sha256 <- function(path) digest::digest(file = path, algo = "sha256", serialize = FALSE)
  named_hash <- function(asset) sub("^.*-([a-f0-9]{64})[.]csv$", "\\1", basename(asset$local_path))
  first <- prepare()
  expect_identical(vapply(first, named_hash, character(1)), vapply(first, `[[`, character(1), "sha256"))
  # Preparing the same data again reuses the verified files without a word.
  expect_silent(again <- prepare())
  expect_identical(lapply(again, `[`, c("id", "sha256", "md5")), lapply(first, `[`, c("id", "sha256", "md5")))
  # A structurally valid table with other values under the name of the correct one is replaced.
  path <- first[[1]]$local_path; original <- .read_bytes(path)
  corrupted <- .read_resource_csv(path); corrupted$bias <- corrupted$bias + 1
  utils::write.csv(corrupted, path, row.names = FALSE)
  expect_false(identical(sha256(path), first[[1]]$sha256))
  expect_message(second <- prepare(), "A corrupted prepared file was replaced")
  expect_identical(.read_bytes(path), original)
  expect_identical(vapply(second, named_hash, character(1)), vapply(second, `[[`, character(1), "sha256"))
  expect_identical(vapply(second, `[[`, character(1), "sha256"), vapply(first, `[[`, character(1), "sha256"))
  expect_length(list.files(prepared, pattern = "^measures-"), 0L)
  # The corrupted file is not mistaken for the others: only the one that differed is reported.
  expect_silent(prepare())
})

test_that("method/setting pairs that spell the same measure label are rejected", {
  root <- withr::local_tempdir(); measures <- file.path(root, "no_bias", "measures")
  dir.create(measures, recursive = TRUE)
  local_mocked_bindings(.get_path = function() root)
  write_measures <- function(method, setting, replacement = FALSE) utils::write.csv(
    data.frame(method = method, method_setting = setting, condition_id = 1, bias = .1, bias_mcse = .01, n_valid = 4),
    file.path(measures, paste0("bias", if (replacement) "-replacement", ".csv")), row.names = FALSE)
  # "a-b" + "c" and "a" + "b-c" both give the label "a-b-c".
  utils::write.csv(data.frame(method = c("a-b", "a"), method_setting = c("c", "b-c"), condition_id = 1,
    bias = c(.1, .2), bias_mcse = .01, n_valid = 4), file.path(measures, "bias.csv"), row.names = FALSE)
  expect_error(prepare_benchmark_resources("no_bias", kinds = "measures", output_directory = file.path(root, "prepared")),
               "share the measure label 'a-b-c'")
  # Distinct labels, also across the ordinary and replacement variants, are fine.
  write_measures(c("a-b", "a"), c("c", "d"))
  utils::write.csv(data.frame(method = "a", method_setting = "d", condition_id = 1, bias = .1, bias_mcse = .01, n_valid = 4,
    replaced = "x=1;"), file.path(measures, "bias-replacement.csv"), row.names = FALSE)
  assets <- prepare_benchmark_resources("no_bias", kinds = "measures", output_directory = file.path(root, "prepared"))
  expect_setequal(vapply(assets, `[[`, character(1), "id"),
                  c("no_bias/measures/a-b-c", "no_bias/measures/a-d", "no_bias/measures/a-d-replacement"))
})
