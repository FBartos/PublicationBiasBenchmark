archive_test_plan <- function(root, assets, release = "test.1", previous = NULL, replace = character(), ...) {
  plan_benchmark_release(release, assets, previous = previous, replace = replace,
    conditions = if (is.null(previous)) list(no_bias = data.frame(condition_id = 1:2, mean_effect = 0)) else NULL,
    metadata = list(rights = list(list(id = "cc-by-4.0"))), state_directory = file.path(root, release),
    sandbox = FALSE, community = "test-community", ...)
}

test_that("archive downloads deduplicate transfers, reuse schema-1 members and repair locally", {
  skip_if_not_installed("zip")
  originals <- withr::local_tempdir(); root <- withr::local_tempdir(); state <- withr::local_tempdir()
  assets <- list(test_resource(originals, "A-1.csv", ids = 1:2), test_resource(originals, "A-2.csv", ids = 3:4),
    test_resource(originals, "B.csv", method = "B"), test_resource(originals, "data.csv", "data"))
  plan <- archive_test_plan(state, assets); catalog <- plan$catalog
  calls <- character()
  local_mocked_bindings(.get_path = function() root, .resource_download = function(url, destination, progress) {
    calls <<- c(calls, url)
    a <- Filter(function(x) endsWith(url, paste0(x$filename, "/content")), plan$catalog$archives)[[1]]
    file.copy(a$local_path, destination, overwrite = TRUE)
  })
  dir.create(dirname(.asset_cache_path(assets[[1]])), recursive = TRUE)
  file.copy(assets[[1]]$local_path, .asset_cache_path(assets[[1]]))
  expect_true(download_dgm_results("no_bias", method = "A", release = catalog, progress = FALSE))
  expect_length(calls, 1L)
  expect_equal(retrieve_dgm_results("no_bias", method = "A", release = catalog)$repetition_id, 1:4)
  expect_false(file.exists(.asset_cache_path(assets[[3]])))
  expect_false(file.exists(.asset_cache_path(assets[[4]])))
  writeBin(charToRaw("bad"), .asset_cache_path(assets[[2]]))
  expect_equal(.pending_downloads(catalog, assets = catalog$assets[1:2])$bytes, 0)
  expect_true(download_dgm_results("no_bias", method = "A", release = catalog, progress = FALSE))
  expect_length(calls, 1L)
  expect_true(all(verify_benchmark_resources(catalog, "no_bias", "results", "A")$verified))
  report <- prune_benchmark_archives(catalog)
  expect_equal(sum(report$removable), 1L)
  expect_equal(sum(prune_benchmark_archives(catalog, dry_run = FALSE)$removed), 1L)
  expect_true(download_dgm_results("no_bias", method = "A", release = catalog, progress = FALSE))
  expect_length(calls, 1L)
  expect_true(all(is.na(list_benchmark_resources(test_catalog(assets), "no_bias", "results")$archive_id)))
})

test_that("packing combines measure variants, partitions scope and preserves exact bytes", {
  skip_if_not_installed("zip")
  root <- withr::local_tempdir(); state <- withr::local_tempdir()
  a <- test_resource(root, "ordinary.csv", "measures")
  b <- test_resource(root, "replacement.csv", "measures"); b$replacement <- TRUE
  plan <- archive_test_plan(state, list(a, b))
  expect_length(plan$catalog$archives, 1L)
  expect_length(plan$catalog$archives[[1]]$members, 2L)
  expect_true(.zip_inventory(plan$catalog$archives[[1]]$local_path, plan$catalog$archives[[1]]$members))
  expect_equal(archive_test_plan(state, list(a, b)), plan)
  writeBin(charToRaw("corrupt"), plan$catalog$archives[[1]]$local_path)
  expect_error(archive_test_plan(state, list(a, b)), "Persisted archive changed")
  a$size <- 2000000001
  expect_error(.split_archive_members(list(a), 2000000000), "single member")
  a$size <- as.numeric(file.info(a$local_path)$size); a$method <- "A--B"
  expect_error(archive_test_plan(withr::local_tempdir(), list(a)), "cannot contain")
})

test_that("archive names distinguish dataset ranges and number ambiguous splits consistently", {
  skip_if_not_installed("zip")
  root <- withr::local_tempdir()
  a <- test_resource(root, "condition-1.csv", "data", condition = 1L)
  b <- test_resource(root, "condition-2.csv", "data", condition = 2L)
  data <- archive_test_plan(withr::local_tempdir(), list(a, b), max_archive_bytes = a$size)
  expect_setequal(vapply(data$catalog$archives, `[[`, character(1), "filename"),
    c("no_bias--datasets--conditions-0001-0001--test.1.zip", "no_bias--datasets--conditions-0002-0002--test.1.zip"))
  b <- test_resource(root, "condition-1-next.csv", "data", condition = 1L, ids = 3:4)
  repeated <- archive_test_plan(withr::local_tempdir(), list(a, b), max_archive_bytes = a$size)
  expect_setequal(vapply(repeated$catalog$archives, `[[`, character(1), "filename"),
    c("no_bias--datasets--conditions-0001-0001--test.1--part-001.zip",
      "no_bias--datasets--conditions-0001-0001--test.1--part-002.zip"))
  a <- test_resource(root, "A-first.csv")
  b <- test_resource(root, "A-second.csv", ids = 3:4)
  results <- archive_test_plan(withr::local_tempdir(), list(a, b), max_archive_bytes = a$size)
  expect_setequal(vapply(results$catalog$archives, `[[`, character(1), "filename"),
    c("no_bias--results--A--default--test.1--part-001.zip", "no_bias--results--A--default--test.1--part-002.zip"))
  source <- archive_test_plan(withr::local_tempdir(), list(test_resource(root, "source.csv", "archive")))
  expect_identical(source$catalog$archives[[1]]$filename, "no_bias--source-tables--test.1.zip")
})

test_that("ZIP validation rejects link types, undeclared members and truncation before extraction", {
  skip_if_not_installed("zip")
  root <- withr::local_tempdir(); a <- test_resource(root, "A.csv")
  plan <- archive_test_plan(withr::local_tempdir(), list(a)); archive <- plan$catalog$archives[[1]]
  original <- readBin(archive$local_path, "raw", n = file.info(archive$local_path)$size)
  expect_error(.zip_inventory(archive$local_path, list(modifyList(archive$members[[1]], list(filename = "other.csv")))), "ZIP")
  expect_error(.zip_inventory(archive$local_path, list(modifyList(archive$members[[1]], list(size = 1)))), "ZIP")
  signature <- as.raw(c(0x50, 0x4b, 0x01, 0x02))
  start <- which(vapply(seq_len(length(original) - 3L), function(i) identical(original[i + 0:3], signature), logical(1)))[1]
  link <- original
  # Unix S_IFLNK in the central directory's high external-attribute word.
  link[start + 40:41] <- as.raw(c(0xff, 0xa1))
  path <- file.path(root, "link.zip"); writeBin(link, path)
  expect_error(.zip_inventory(path, archive$members), "ZIP")
  writeBin(head(original, -10L), path)
  expect_error(.zip_inventory(path, archive$members), "ZIP")
})

test_that("corrections reject stale derived assets and frozen DGM growth", {
  skip_if_not_installed("zip")
  root <- withr::local_tempdir(); state <- withr::local_tempdir()
  data <- test_resource(root, "data.csv", "data")
  result <- test_resource(root, "result.csv")
  measure <- test_resource(root, "measure.csv", "measures")
  first <- archive_test_plan(state, list(data, result, measure))
  base <- .public_catalog(first$catalog)
  base$publication <- list(catalog_record_id = "123", catalog_concept_doi = "10.test/concept")
  corrected <- test_resource(root, "data.csv", "data", ids = 3:4)
  expect_error(archive_test_plan(state, list(corrected), "test.2", base, replace = data$id), "Stale derived asset")
  corrected_result <- test_resource(root, "result.csv", ids = 3:4)
  expect_error(archive_test_plan(state, list(corrected, corrected_result), "test.2", base,
    replace = c(data$id, result$id)), "Stale derived asset")
  second <- archive_test_plan(state, list(corrected, corrected_result, measure), "test.2", base,
    replace = c(data$id, result$id, measure$id))
  expect_equal(Filter(function(x) x$id == data$id, second$catalog$assets)[[1]]$sha256, corrected$sha256)
  new_data <- test_resource(root, "new-data.csv", "data", ids = 5:6)
  expect_error(archive_test_plan(state, list(new_data), "test.3", base), "New data")
  conditions <- base$conditions; conditions$no_bias <- rbind(conditions$no_bias, data.frame(condition_id = 3, mean_effect = 0))
  expect_error(plan_benchmark_release("test.4", list(), previous = base, conditions = conditions,
    metadata = list(), state_directory = file.path(state, "test.4")), "New conditions")
})

test_that("unchanged units keep archive bytes while changed DGMs bind to one version", {
  skip_if_not_installed("zip")
  root <- withr::local_tempdir(); state <- withr::local_tempdir()
  a <- test_resource(root, "A.csv"); b <- test_resource(root, "B.csv", method = "B")
  first <- archive_test_plan(state, list(a, b)); base <- .public_catalog(first$catalog)
  base$source_commit <- "historical-dgm-commit"
  base$provenance <- list(source = "https://osf.io/exf3m/", corrections = list("baseline audit"))
  base$publication <- list(catalog_record_id = "123", catalog_concept_doi = "10.test/concept")
  corrected <- test_resource(root, "A.csv", ids = 3:4)
  second <- archive_test_plan(state, list(corrected), "test.2", base, a$id)
  expect_length(second$groups$no_bias, 1L)
  expect_identical(second$catalog$source_commit, base$source_commit)
  expect_identical(second$catalog$provenance, base$provenance)
  expect_equal(second$catalog$archives[[2]]$sha256, base$archives[[2]]$sha256)
  expect_equal(unique(vapply(second$catalog$archives, `[[`, character(1), "record_id")), "0")
  expect_equal(benchmark_packing_report(second)$files, 2L)
  failed_state <- withr::local_tempdir()
  expect_error(archive_test_plan(failed_state, list(corrected, b), max_files = 1L), "packing exceeds")
  report <- read.csv(file.path(failed_state, "test.1", "packing-report.csv"))
  expect_equal(report$files, 2L); expect_equal(report$free_files, -1L)
})

test_that("condition and pairwise selection fetch only their independent archive units", {
  skip_if_not_installed("zip")
  root <- withr::local_tempdir(); originals <- withr::local_tempdir()
  first <- test_resource(originals, "condition-1.csv", "data", condition = 1L, ids = 1:20)
  second <- test_resource(originals, "condition-2.csv", "data", condition = 2L, ids = 1:20)
  ordinary <- test_resource(originals, "measure.csv", "measures")
  pair_path <- file.path(originals, "pairwise.csv")
  write.csv(data.frame(method_a = "A-default", method_b = "B-default", condition_id = 1, score = .5), pair_path, row.names = FALSE)
  pairwise <- benchmark_resource(pair_path, "no_bias", "pairwise")
  plan <- archive_test_plan(withr::local_tempdir(), list(first, second, ordinary, pairwise), max_archive_bytes = first$size)
  catalog <- plan$catalog; calls <- character()
  local_mocked_bindings(.get_path = function() root, .resource_download = function(url, destination, progress) {
    calls <<- c(calls, url)
    archive <- Filter(function(x) endsWith(url, paste0(x$filename, "/content")), catalog$archives)[[1]]
    file.copy(archive$local_path, destination)
  })
  expect_true(download_dgm_datasets("no_bias", condition_id = 1, release = catalog, progress = FALSE))
  expect_length(calls, 1L)
  expect_false(file.exists(.asset_cache_path(second)))
  expect_true(download_dgm_measures("no_bias", release = catalog, progress = FALSE))
  expect_false(file.exists(.asset_cache_path(pairwise)))
  expect_true(download_dgm_measures("no_bias", measure = "pairwise", release = catalog, progress = FALSE))
  expect_equal(retrieve_dgm_measures("no_bias", measure = "pairwise", release = catalog)$score, .5)
})

test_that("a source changing during ZIP staging fails before persistence", {
  skip_if_not_installed("zip")
  root <- withr::local_tempdir(); a <- test_resource(root, "A.csv")
  local_mocked_bindings(.archive_source = function(asset, base) {
    writeBin(charToRaw("changed source"), asset$local_path)
    asset$local_path
  })
  state <- withr::local_tempdir()
  expect_error(archive_test_plan(state, list(a)), "Source changed while staging")
  expect_length(list.files(file.path(state, "test.1", "archives"), pattern = "\\.zip$"), 0L)
})

test_that("new methods preserve old replacement tables, while added input shards invalidate aggregates", {
  skip_if_not_installed("zip")
  root <- withr::local_tempdir(); state <- withr::local_tempdir()
  a <- test_resource(root, "A.csv")
  measure <- test_resource(root, "replacement.csv", "measures"); measure$replacement <- TRUE
  first <- archive_test_plan(state, list(a, measure)); base <- .public_catalog(first$catalog)
  base$publication <- list(catalog_record_id = "123", catalog_concept_doi = "10.test/concept")
  b <- test_resource(root, "B.csv", method = "B")
  next_plan <- archive_test_plan(state, list(b), "test.2", base)
  kept <- Filter(function(x) identical(x$id, measure$id), next_plan$catalog$assets)[[1]]
  expect_identical(vapply(kept$dependencies, `[[`, character(1), "id"), a$id)
  expect_length(next_plan$groups$no_bias, 1L)
  shard <- test_resource(root, "A-next.csv", ids = 3:4)
  expect_error(archive_test_plan(state, list(shard), "test.3", base), "Stale derived asset")
  explicit <- measure; explicit$dependencies <- as.list(a$id); explicit$dependencies_explicit <- TRUE
  refreshed <- archive_test_plan(state, list(explicit), "test.4", base, replace = measure$id)
  declared <- .public_catalog(refreshed$catalog)
  declared$publication <- base$publication
  expect_error(archive_test_plan(state, list(shard), "test.5", declared), "Stale derived asset")
})

test_that("an interrupted local manifest write recovers the exact completed ZIP bytes", {
  skip_if_not_installed("zip")
  root <- withr::local_tempdir(); a <- test_resource(root, "A.csv")
  plan <- archive_test_plan(withr::local_tempdir(), list(a))
  archive <- plan$catalog$archives[[1]]
  writeBin(raw(), paste0(archive$local_path, ".json"))
  local_mocked_bindings(.archive_source = function(...) stop("Should reuse the completed ZIP"))
  recovered <- .build_release_archive(plan, archive$unit, archive$part, list(a), NULL)
  expect_identical(recovered$sha256, archive$sha256)
  expect_true(recovered$manifest_recovered)
  expect_true(.file_verified(recovered$local_path, archive$sha256, archive$size, archive$md5))
})

archive_fixture <- function(originals, state) {
  assets <- list(test_resource(originals, "A-1.csv", ids = 1:2), test_resource(originals, "A-2.csv", ids = 3:4),
    test_resource(originals, "B.csv", method = "B"), test_resource(originals, "data.csv", "data"),
    test_resource(originals, "A-measures.csv", "measures"))
  list(assets = assets, plan = archive_test_plan(state, assets))
}
fixture_downloader <- function(plan, env = parent.frame()) {
  calls <- new.env(); calls$urls <- character()
  testthat::local_mocked_bindings(.resource_download = function(url, destination, progress) {
    calls$urls <- c(calls$urls, url)
    a <- Filter(function(x) endsWith(url, paste0(x$filename, "/content")), plan$catalog$archives)[[1]]
    file.copy(a$local_path, destination, overwrite = TRUE)
  }, .env = env)
  calls
}

test_that("downloads work from an empty resources directory and leave no staging directories", {
  skip_if_not_installed("zip")
  originals <- withr::local_tempdir(); state <- withr::local_tempdir()
  fixture <- archive_fixture(originals, state); catalog <- fixture$plan$catalog
  root <- file.path(withr::local_tempdir(), "never-created")
  local_mocked_bindings(.get_path = function() root)
  calls <- fixture_downloader(fixture$plan)
  expect_false(dir.exists(root))
  expect_true(download_dgm_results("no_bias", release = catalog, progress = FALSE))
  expect_length(calls$urls, 2L)
  expect_true(all(verify_benchmark_resources(catalog, "no_bias", "results")$verified))
  expect_length(list.files(file.path(root, "cache"), pattern = "^extract-"), 0L)
  expect_equal(retrieve_dgm_results("no_bias", method = "A", release = catalog)$repetition_id, 1:4)
})

test_that("a cached archive is hashed once per download call and members are verified once", {
  skip_if_not_installed("zip")
  originals <- withr::local_tempdir(); state <- withr::local_tempdir()
  fixture <- archive_fixture(originals, state); catalog <- fixture$plan$catalog
  root <- withr::local_tempdir()
  local_mocked_bindings(.get_path = function() root)
  calls <- fixture_downloader(fixture$plan)
  expect_true(download_dgm_results("no_bias", method = "A", release = catalog, progress = FALSE))
  expect_length(calls$urls, 1L)
  archive <- Filter(function(x) x$unit == "no_bias--results--A--default", catalog$archives)[[1]]
  archive_path <- .archive_cache_path(archive)
  members <- Filter(function(x) x$archive_id == archive$id, catalog$assets)
  member_paths <- vapply(members, .asset_cache_path, character(1))
  # Damage one member and download again: the cached ZIP is reused, not fetched.
  writeBin(charToRaw("bad"), member_paths[1])
  hashed <- new.env(); hashed$paths <- character(); original <- .file_verified
  local_mocked_bindings(.file_verified = function(path, sha256, size = NULL, md5 = NULL) {
    if (file.exists(path)) hashed$paths <- c(hashed$paths, normalizePath(path, winslash = "/"))
    original(path, sha256, size, md5)
  })
  expect_true(download_dgm_results("no_bias", method = "A", release = catalog, progress = FALSE))
  expect_length(calls$urls, 1L)
  expect_equal(sum(hashed$paths == normalizePath(archive_path, winslash = "/")), 1L)
  expect_true(all(vapply(members, function(x) original(.asset_cache_path(x), x$sha256, x$size, x$md5), logical(1))))
  # Every other cached copy is hashed at most once.
  intact <- normalizePath(member_paths[-1], winslash = "/")
  expect_true(all(table(hashed$paths[hashed$paths %in% intact]) <= 1L))
  expect_length(list.files(file.path(root, "cache"), pattern = "^extract-"), 0L)
  # Nothing pending: nothing is transferred or extracted again.
  hashed$paths <- character()
  expect_true(download_dgm_results("no_bias", method = "A", release = catalog, progress = FALSE))
  expect_false(normalizePath(archive_path, winslash = "/") %in% hashed$paths)
})

test_that("a cached archive that fails extraction is fetched once and extracted again", {
  skip_if_not_installed("zip")
  originals <- withr::local_tempdir(); state <- withr::local_tempdir()
  fixture <- archive_fixture(originals, state); catalog <- fixture$plan$catalog
  root <- withr::local_tempdir()
  local_mocked_bindings(.get_path = function() root)
  calls <- fixture_downloader(fixture$plan)
  expect_true(download_dgm_results("no_bias", method = "A", release = catalog, progress = FALSE))
  archive <- Filter(function(x) x$unit == "no_bias--results--A--default", catalog$archives)[[1]]
  members <- Filter(function(x) x$archive_id == archive$id, catalog$assets)
  for (member in members) unlink(.asset_cache_path(member))
  pending <- .pending_downloads(catalog, members)
  expect_length(pending$transfers, 0L); expect_length(pending$assets, 2L)
  # The ZIP verified when pending was computed but is gone at extraction time.
  unlink(.archive_cache_path(archive))
  expect_silent(.download_catalog_assets(catalog, members, progress = FALSE, pending = pending))
  expect_length(calls$urls, 2L)
  expect_true(all(vapply(members, function(x) .file_verified(.asset_cache_path(x), x$sha256, x$size, x$md5), logical(1))))
})

test_that("an installed member is never replaced by a failed installation", {
  skip_if_not_installed("zip")
  originals <- withr::local_tempdir(); state <- withr::local_tempdir()
  fixture <- archive_fixture(originals, state); catalog <- fixture$plan$catalog
  root <- withr::local_tempdir()
  local_mocked_bindings(.get_path = function() root)
  fixture_downloader(fixture$plan)
  member <- Filter(function(x) x$kind == "data", catalog$assets)[[1]]
  # A directory occupying the destination makes the final rename fail.
  destination <- .asset_cache_path(member)
  dir.create(destination, recursive = TRUE); writeLines("keep", file.path(destination, "marker"))
  expect_error(download_dgm_datasets("no_bias", release = catalog, progress = FALSE), "Cannot install verified archive member")
  expect_true(file.exists(file.path(destination, "marker")))
  expect_length(list.files(file.path(root, "cache"), pattern = "^extract-"), 0L)
})

test_that("abandoned staging directories are removed after a day only", {
  root <- withr::local_tempdir()
  local_mocked_bindings(.get_path = function() root)
  cache <- file.path(root, "cache"); dir.create(file.path(cache, "extract-old"), recursive = TRUE)
  dir.create(file.path(cache, "extract-new")); dir.create(file.path(cache, strrep("a", 64)))
  Sys.setFileTime(file.path(cache, "extract-old"), Sys.time() - 2 * 24 * 3600)
  .remove_stale_extractions()
  expect_false(dir.exists(file.path(cache, "extract-old")))
  expect_true(dir.exists(file.path(cache, "extract-new")))
  expect_true(dir.exists(file.path(cache, strrep("a", 64))))
})

test_that("list_benchmark_resources matches the per-asset reference implementation", {
  skip_if_not_installed("zip")
  originals <- withr::local_tempdir(); state <- withr::local_tempdir()
  fixture <- archive_fixture(originals, state)
  reference <- function(catalog) {
    do.call(rbind, lapply(catalog$assets, function(x) {
      data.frame(id = x$id, dgm = x$dgm, kind = x$kind,
                 method = if (is.null(x$method)) "" else x$method,
                 method_setting = if (is.null(x$method_setting)) "" else x$method_setting,
                 filename = x$filename, size = x$size, sha256 = x$sha256, record_id = x$record_id,
                 url = .resource_reference_url(catalog, x),
                 archive_id = if (is.null(x$archive_id)) NA_character_ else x$archive_id,
                 archive_filename = if (is.null(x$archive_id)) NA_character_ else .catalog_archive(catalog, x$archive_id)$filename,
                 download_size = if (is.null(x$archive_id)) x$size else .catalog_archive(catalog, x$archive_id)$size,
                 package_version = if (is.null(x$package_version)) NA_character_ else x$package_version,
                 stringsAsFactors = FALSE)
    }))
  }
  public <- .public_catalog(fixture$plan$catalog)
  path <- file.path(state, "public.json")
  jsonlite::write_json(public, path, auto_unbox = TRUE, null = "null", digits = NA, dataframe = "rows")
  parsed <- benchmark_catalog(path)
  expect_identical(list_benchmark_resources(public), reference(public))
  expect_identical(list_benchmark_resources(path), reference(parsed))
  expect_type(list_benchmark_resources(path)$size, "integer")
  expect_type(list_benchmark_resources(public)$size, "double")
  selected <- parsed
  selected$assets <- Filter(function(x) x$kind == "results" && x$method == "A", parsed$assets)
  expect_identical(list_benchmark_resources(parsed, kind = "results", method = "A"), reference(selected))
  legacy <- test_catalog(fixture$assets)
  expect_identical(list_benchmark_resources(legacy), reference(legacy))
  expect_true(all(is.na(list_benchmark_resources(legacy)$archive_id)))
  expect_equal(list_benchmark_resources(legacy)$download_size, list_benchmark_resources(legacy)$size)
  expect_true(all(grepl("^https://zenodo.org/api/records/[0-9]+/files/.+/content$", list_benchmark_resources(public)$url)))
  expect_equal(nrow(list_benchmark_resources(public, dgm_name = "no_bias", kind = "data")), 1L)
})

test_that("catalog validation rejects bad archive references and stale or invalid computation inputs", {
  skip_if_not_installed("zip")
  originals <- withr::local_tempdir(); state <- withr::local_tempdir()
  fixture <- archive_fixture(originals, state)
  public <- .public_catalog(fixture$plan$catalog)
  expect_true(any(vapply(public$assets, function(x) length(x$dependencies) > 0L, logical(1))))
  with_dependency <- which(vapply(public$assets, function(x) x$kind == "results" && length(x$dependencies) > 0L, logical(1)))[1]
  mutate <- function(change) { x <- public; x$assets[[with_dependency]] <- change(x$assets[[with_dependency]]); x }
  expect_error(.validate_catalog(mutate(function(a) { a$dependencies[[1]]$sha256 <- strrep("0", 64); a })), "Invalid or stale computation input")
  expect_error(.validate_catalog(mutate(function(a) { a$dependencies[[1]]$id <- "missing/input"; a })), "Invalid computation input reference")
  expect_error(.validate_catalog(mutate(function(a) { a$dependencies[[1]] <- "not-a-list"; a })), "Invalid computation input reference")
  measure_input <- Filter(function(x) x$kind == "results", public$assets)[[1]]
  expect_error(.validate_catalog(mutate(function(a) { a$dependencies[[1]] <- list(id = measure_input$id, sha256 = measure_input$sha256); a })),
               "Invalid or stale computation input")
  unknown <- public; unknown$assets[[1]]$archive_id <- "no-such-archive"
  expect_error(.validate_catalog(unknown), "Unknown or duplicate archive reference")
  renamed <- public; renamed$assets[[1]]$filename <- "other.csv"
  expect_error(.validate_catalog(renamed), "differs from its archive member inventory")
  rehashed <- public; rehashed$assets <- lapply(public$assets, function(x) { x$sha256 <- strrep("1", 64); x })
  expect_error(.validate_catalog(rehashed), "differs from its archive member inventory")
  expect_identical(.catalog_archive(public, public$archives[[2]]$id), public$archives[[2]])
  expect_error(.catalog_archive(public, "absent"), "Unknown or duplicate archive reference")
})

test_that("an asset owned by two archives is rejected while planning", {
  skip_if_not_installed("zip")
  root <- withr::local_tempdir(); state <- withr::local_tempdir()
  a <- test_resource(root, "A.csv")
  first <- archive_test_plan(state, list(a)); base <- .public_catalog(first$catalog)
  twin <- base$archives[[1]]; twin$id <- paste0(twin$id, "-copy"); twin$filename <- "twin.zip"
  base$archives <- c(base$archives, list(twin))
  expect_error(archive_test_plan(state, list(a), "test.2", base), "Asset does not map to exactly one ZIP member")
  # The unduplicated catalog plans cleanly and keeps each asset's single owner.
  second <- archive_test_plan(state, list(a), "test.2", .public_catalog(first$catalog))
  expect_identical(second$catalog$assets[[1]]$archive_id, first$catalog$archives[[1]]$id)
})

test_that("plan coverage and derived inputs separate methods whose identifiers contain separators", {
  result <- function(method, setting, id) list(id = id, dgm = "no_bias", kind = "results", method = method,
    method_setting = setting, condition_ids = list(1L), sha256 = strrep("a", 64),
    coverage = list(list(condition_id = 1L, repetitions = 2L, ranges = list(list(start = 1L, end = 2L)))))
  measure <- function(method, setting, id, replacement = FALSE) list(id = id, dgm = "no_bias", kind = "measures",
    method = method, method_setting = setting, replacement = replacement, condition_ids = list(1L), sha256 = strrep("b", 64))
  expect_true(.validate_plan_coverage(list(result("a/b", "c", "r1"), result("a", "b/c", "r2"))))
  expect_true(.validate_plan_coverage(list(result("a.b", "c", "r1"), result("a", "b.c", "r2"))))
  expect_error(.validate_plan_coverage(list(result("a", "c", "r1"), result("a", "c", "r2"))), "Overlapping data or result")
  expect_true(.validate_plan_coverage(list(measure("a/b", "c", "m1"), measure("a", "b/c", "m2"))))
  expect_error(.validate_plan_coverage(list(measure("a", "b", "m1"), measure("a", "b", "m2"))), "Overlapping measure")
  expect_true(.validate_plan_coverage(list(measure("a", "b", "m1"), measure("a", "b", "m2", replacement = TRUE))))
  r1 <- result("a/b", "c", "r1"); r2 <- result("a", "b/c", "r2")
  m <- modifyList(measure("a/b", "c", "m1"), list(dependencies = list(list(id = "r1", sha256 = r1$sha256)),
                                                 dependencies_explicit = TRUE))
  inputs <- .asset_inputs(m, list(r1, r2, m))
  expect_equal(vapply(inputs, `[[`, character(1), "id"), "r1")
})
