test_resource <- function(directory, filename, kind = "results", method = "A", ids = 1:2,
                          dgm = "no_bias", condition = 1L) {
  path <- file.path(directory, filename)
  data <- data.frame(method = method, method_setting = "default", condition_id = condition,
                     repetition_id = ids, estimate = seq_along(ids), stringsAsFactors = FALSE)
  if (kind == "data") data <- data.frame(repetition_id = rep(ids, each = 2), yi = 1, vi = .1)
  if (kind == "measures") data <- data.frame(method = method, method_setting = "default",
    condition_id = condition, bias = .1, bias_mcse = .01, n_valid_bias = 2L)
  utils::write.csv(data, path, row.names = FALSE)
  asset <- benchmark_resource(path, dgm, kind,
    method = if (kind == "data") NULL else method,
    method_setting = if (kind == "data") NULL else "default",
    condition_ids = condition, package_version = "0.4.0",
    measures = if (kind == "measures") "bias" else NULL)
  asset$record_id <- "12345"
  asset
}

test_catalog <- function(assets, release = "test.1") list(schema_version = 1L, release = release,
  assets = assets, conditions = list(no_bias = data.frame(condition_id = 1:2, mean_effect = 0)), sandbox = FALSE)
