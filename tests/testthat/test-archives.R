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
