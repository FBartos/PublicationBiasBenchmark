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
})
