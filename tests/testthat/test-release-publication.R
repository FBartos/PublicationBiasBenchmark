test_that("plans reuse existing bytes and require explicit corrections", {
  root <- withr::local_tempdir(); a <- test_resource(root, "A.csv")
  metadata <- list(creators = list(list(person_or_org = list(name = "Tester", type = "organizational"))))
  base <- test_catalog(list(a))
  plan <- plan_benchmark_release("test.2", list(a), previous = base, metadata = metadata,
                                 state_directory = file.path(root, "unchanged"))
  expect_length(plan$groups, 0)
  expect_identical(plan$catalog$assets[[1]]$record_id, "12345")
  corrected <- test_resource(root, "A.csv", ids = 3:4)
  expect_error(plan_benchmark_release("test.2", list(corrected), previous = base, metadata = metadata,
    state_directory = file.path(root, "correction")), "Explicit replacement")
  plan <- plan_benchmark_release("test.2", list(corrected), previous = base, replace = a$id,
    metadata = metadata, state_directory = file.path(root, "correction"))
  expect_length(plan$groups, 1)
  expect_identical(base$assets[[1]]$sha256, a$sha256)
  overlap <- test_resource(root, "other.csv", ids = 4:5)
  expect_error(plan_benchmark_release("test.3", list(corrected, overlap), conditions = base$conditions,
    metadata = metadata, state_directory = file.path(root, "overlap")), "Overlapping")
  conditions <- base$conditions; conditions$no_bias$mean_effect[1] <- 1
  expect_error(plan_benchmark_release("test.3", list(), previous = base, conditions = conditions,
    metadata = metadata, state_directory = file.path(root, "changed-conditions")), "frozen condition")
})

test_that("packing respects both file and byte quotas and rejects changed sources", {
  root <- withr::local_tempdir(); assets <- lapply(1:5, function(i) test_resource(root, paste0(i, ".csv"), ids = (2*i-1):(2*i)))
  plan <- plan_benchmark_release("test.1", assets, conditions = test_catalog(assets)$conditions,
    metadata = list(), state_directory = file.path(root, "state"), max_files = 2)
  expect_equal(lengths(plan$groups), c(2L, 2L, 1L))
  expect_error(plan_benchmark_release("test.1", assets, conditions = plan$catalog$conditions,
    metadata = list(), state_directory = file.path(root, "small"), max_bytes = 1), "file exceeds")
  writeLines("changed", assets[[1]]$local_path)
  expect_error(plan_benchmark_release("test.1", assets, conditions = plan$catalog$conditions,
    metadata = list(), state_directory = file.path(root, "changed")), "Unverified local")
})

test_that("interrupted commits resume without uploading completed files or publishing twice", {
  root <- withr::local_tempdir(); asset <- test_resource(root, "A.csv")
  plan <- plan_benchmark_release("test.1", list(asset), conditions = test_catalog(list(asset))$conditions,
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
