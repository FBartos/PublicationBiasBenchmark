catalog_test_cache <- function() {
  cache <- new.env(parent = emptyenv())
  cache$keys <- character()
  cache$values <- list()
  cache
}

test_that("catalog caches reuse validated bytes and recheck mutable input", {
  root <- withr::local_tempdir()
  asset <- test_resource(root, "A.csv")
  catalog <- test_catalog(list(asset))
  catalog$conditions$no_bias$label <- c("Franti\u0161ek", "\u010cala")
  path <- file.path(root, "catalog.json")
  jsonlite::write_json(catalog, path, auto_unbox = TRUE, dataframe = "rows", null = "null")
  copy <- file.path(root, "same-catalog.json")
  file.copy(path, copy)
  validations <- 0L
  validate <- .validate_catalog
  local_mocked_bindings(.catalog_cache = catalog_test_cache(), .validate_catalog = function(x) {
    validations <<- validations + 1L
    validate(x)
  })
  first <- benchmark_catalog(path)
  expect_equal(.catalog_conditions(first, "no_bias")$label, c("Franti\u0161ek", "\u010cala"))
  expect_identical(benchmark_catalog(copy), first)
  expect_equal(validations, 1L)
  changed <- first
  changed$assets[[1]]$filename <- "../unsafe.csv"
  expect_error(benchmark_catalog(changed), "Invalid benchmark asset")
  expect_identical(benchmark_catalog(path), first)
  jsonlite::write_json(changed, path, auto_unbox = TRUE, null = "null")
  expect_error(benchmark_catalog(path), "Invalid benchmark asset")
})

test_that("registry pin changes reload catalogs and corrupt cached files are repaired", {
  root <- withr::local_tempdir()
  source <- withr::local_tempdir()
  asset <- test_resource(source, "A.csv")
  original <- test_catalog(list(asset))
  path <- file.path(source, "catalog.json")
  jsonlite::write_json(original, path, auto_unbox = TRUE, dataframe = "rows", null = "null")
  pin <- digest::digest(file = path, algo = "sha256", serialize = FALSE)
  release_id <- "test.1"
  downloads <- 0L
  validations <- 0L
  validate <- .validate_catalog
  local_mocked_bindings(.catalog_cache = catalog_test_cache(), .get_path = function() root,
    .release_registry = function() list(default_release = release_id,
      releases = list(list(release = release_id, record_id = "12345", catalog_sha256 = pin))),
    .resource_download = function(url, destination, progress) {
      downloads <<- downloads + 1L
      file.copy(path, destination, overwrite = TRUE)
    }, .validate_catalog = function(x) {
      validations <<- validations + 1L
      validate(x)
    })
  first <- benchmark_catalog("test.1")
  expect_identical(benchmark_catalog("test.1"), first)
  expect_equal(c(downloads, validations), c(1L, 1L))
  writeBin(charToRaw("corrupt"), file.path(root, "releases", "test.1", "release.json"))
  expect_identical(benchmark_catalog("test.1"), first)
  expect_equal(c(downloads, validations), c(2L, 1L))
  updated <- original
  updated$conditions$no_bias$mean_effect <- c(10, 20)
  jsonlite::write_json(updated, path, auto_unbox = TRUE, dataframe = "rows", null = "null")
  pin <- digest::digest(file = path, algo = "sha256", serialize = FALSE)
  expect_equal(benchmark_conditions("no_bias", "test.1")$mean_effect, c(10, 20))
  expect_equal(c(downloads, validations), c(3L, 2L))
  release_id <- "wrong-label"
  expect_error(benchmark_catalog("wrong-label"), "does not match")
})

test_that("catalog memoization retains at most two validated snapshots", {
  root <- withr::local_tempdir()
  asset <- test_resource(root, "A.csv")
  cache <- catalog_test_cache()
  local_mocked_bindings(.catalog_cache = cache)
  for (i in 1:3) {
    path <- file.path(root, paste0("catalog-", i, ".json"))
    jsonlite::write_json(test_catalog(list(asset), paste0("test.", i)), path,
      auto_unbox = TRUE, dataframe = "rows", null = "null")
    invisible(benchmark_catalog(path))
  }
  expect_length(cache$values, 2L)
  expect_length(cache$keys, 2L)
})
