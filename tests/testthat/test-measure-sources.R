source_test_results <- function(method, method_setting = "default", conditions = 1L, repetitions = 4L) {
  grid <- expand.grid(condition_id = conditions, repetition_id = seq_len(repetitions))
  data.frame(method = method, method_setting = method_setting, condition_id = grid$condition_id,
             repetition_id = grid$repetition_id, estimate = .1 * grid$repetition_id, ci_lower = -1,
             ci_upper = 1, p_value = .5, convergence = TRUE, stringsAsFactors = FALSE)
}

# Mock result reader recording the method, source and release of every call.
local_result_reader <- function(env = parent.frame()) {
  calls <- new.env(parent = emptyenv())
  calls$log <- list()
  testthat::local_mocked_bindings(
    retrieve_dgm_results = function(dgm_name, method = NULL, method_setting = NULL, condition_id = NULL,
                                    repetition_id = NULL, release = NULL, source = c("release", "local")) {
      calls$log[[length(calls$log) + 1L]] <- list(method = method, source = source, release = release)
      source_test_results(method, method_setting, conditions = c(1L, 7L, 9L))
    }, .env = env)
  calls
}
logged <- function(calls, field) {
  stats::setNames(vapply(calls$log, function(x) if (is.null(x[[field]])) NA_character_ else x[[field]], character(1)),
                  vapply(calls$log, `[[`, character(1), "method"))
}

source_test_conditions <- data.frame(condition_id = 1L, mean_effect = 0)
source_test_replacements <- list("A-default" = list(method = "B", method_setting = "default"))
source_test_single <- function(...) {
  compute_single_measure("no_bias", "bias", "A", "default", conditions = source_test_conditions,
    measure_fun = measure("bias"), measure_mcse_fun = measure_mcse("bias"), n_repetitions = 4, ...)
}
source_test_root <- function(env = parent.frame()) {
  root <- withr::local_tempdir(.local_envir = env)
  dir.create(file.path(root, "no_bias", "measures"), recursive = TRUE)
  testthat::local_mocked_bindings(.get_path = function() root, .env = env)
  root
}

test_that("measure computation reads local results by default and routes replacement sources", {
  source_test_root()
  calls <- local_result_reader()
  source_test_single(method_replacements = source_test_replacements)
  expect_equal(logged(calls, "source"), c(B = "local", A = "local"))

  calls$log <- list()
  expect_no_warning(source_test_single(method_replacements = source_test_replacements,
                                       replacement_source = "release", release = "2026.1", overwrite = TRUE))
  expect_equal(logged(calls, "source"), c(B = "release", A = "local"))
  expect_equal(logged(calls, "release"), c(B = "2026.1", A = "2026.1"))

  calls$log <- list()
  source_test_single(method_replacements = source_test_replacements, results_source = "release",
                     release = "2026.1", overwrite = TRUE)
  expect_equal(logged(calls, "source"), c(B = "release", A = "release"))

  calls$log <- list()
  source_test_single(method_replacements = source_test_replacements, results_source = "release",
                     replacement_source = "local", release = "2026.1", overwrite = TRUE)
  expect_equal(logged(calls, "source"), c(B = "local", A = "release"))
})

test_that("measure wrappers pass the sources of the method and replacement results separately", {
  source_test_root()
  calls <- local_result_reader()
  compute_measures("no_bias", "A", "default", measures = c("bias", "mse"), verbose = FALSE, conditions = source_test_conditions,
                   n_repetitions = 4, method_replacements = source_test_replacements)
  expect_equal(unname(logged(calls, "source")), rep("local", 4L))

  calls$log <- list()
  compute_measures("no_bias", "A", "default", measures = c("bias", "mse"), verbose = FALSE, conditions = source_test_conditions,
                   n_repetitions = 4, method_replacements = source_test_replacements, overwrite = TRUE,
                   replacement_source = "release", release = "2026.1")
  expect_equal(unname(logged(calls, "source")), rep(c("release", "local"), 2L))

  calls$log <- list()
  compare_measures("no_bias", c("A", "C"), c("default", "default"), verbose = FALSE, conditions = source_test_conditions,
                   n_repetitions = 4, method_replacements = list("A-default" = list(method = "B", method_setting = "default")))
  expect_true(all(logged(calls, "source") == "local"))
  expect_setequal(names(logged(calls, "source")), c("A", "B", "C"))

  calls$log <- list()
  compare_measures("no_bias", c("A", "C"), c("default", "default"), verbose = FALSE, conditions = source_test_conditions,
                   n_repetitions = 4, overwrite = TRUE, replacement_source = "release", release = "2026.1",
                   method_replacements = list("A-default" = list(method = "B", method_setting = "default")))
  expect_equal(logged(calls, "source")[["B"]], "release")
  expect_equal(unname(logged(calls, "source")[c("A", "C")]), c("local", "local"))
})

test_that("a release that no source reads is reported once", {
  source_test_root()
  calls <- local_result_reader()
  expect_warning(source_test_single(release = "2026.1"), "ignored")
  expect_equal(logged(calls, "source"), c(A = "local"))
  warnings <- character()
  withCallingHandlers(
    compute_measures("no_bias", "A", "default", measures = c("bias", "mse"), verbose = FALSE, conditions = source_test_conditions,
                     n_repetitions = 4, release = "2026.1", overwrite = TRUE),
    warning = function(w) { warnings <<- c(warnings, conditionMessage(w)); invokeRestart("muffleWarning") })
  expect_length(warnings, 1L)
  expect_match(warnings, "ignored")
  warnings <- character()
  withCallingHandlers(
    compare_measures("no_bias", c("A", "C"), c("default", "default"), verbose = FALSE, conditions = source_test_conditions,
                     n_repetitions = 4, release = "2026.1", overwrite = TRUE),
    warning = function(w) { warnings <<- c(warnings, conditionMessage(w)); invokeRestart("muffleWarning") })
  expect_length(warnings, 1L)
  expect_no_warning(source_test_single(results_source = "release", release = "2026.1", overwrite = TRUE))
})

test_that("invalid source values fail before any result is read", {
  source_test_root()
  calls <- local_result_reader()
  expect_error(source_test_single(results_source = "remote"), "should be one of")
  expect_error(source_test_single(replacement_source = "remote"), "should be one of")
  expect_error(compute_measures("no_bias", "A", "default", measures = "bias", conditions = source_test_conditions,
                                results_source = "remote"), "should be one of")
  expect_error(compute_measures("no_bias", "A", "default", measures = "bias", conditions = source_test_conditions,
                                replacement_source = "remote"), "should be one of")
  expect_error(compare_single_measure("no_bias", "estimate_comparison", c("A", "C"), c("default", "default"),
                                      conditions = source_test_conditions, results_source = "remote"), "should be one of")
  expect_error(compare_measures("no_bias", c("A", "C"), c("default", "default"), conditions = source_test_conditions,
                                replacement_source = "remote"), "should be one of")
  expect_length(calls$log, 0L)
})

test_that("omitted conditions follow the results source", {
  root <- source_test_root()
  local_result_reader()
  local_mocked_bindings(
    benchmark_catalog = function(release = NULL) list(conditions = list(no_bias = data.frame(condition_id = 7L, mean_effect = 0))),
    dgm_conditions = function(dgm_name) data.frame(condition_id = 9L, mean_effect = 0))
  compute_single_measure("no_bias", "bias", "A", "default", conditions = NULL, measure_fun = measure("bias"),
                         measure_mcse_fun = measure_mcse("bias"), n_repetitions = 4)
  output <- file.path(root, "no_bias", "measures", "bias.csv")
  expect_equal(utils::read.csv(output)$condition_id, 9L)
  compute_single_measure("no_bias", "bias", "A", "default", conditions = NULL, measure_fun = measure("bias"),
                         measure_mcse_fun = measure_mcse("bias"), n_repetitions = 4, results_source = "release",
                         overwrite = TRUE)
  expect_equal(utils::read.csv(output)$condition_id, 7L)
  # Local results with release replacements keep the package's conditions.
  compute_single_measure("no_bias", "bias", "A", "default", conditions = NULL, measure_fun = measure("bias"),
                         measure_mcse_fun = measure_mcse("bias"), n_repetitions = 4, replacement_source = "release",
                         overwrite = TRUE)
  expect_equal(utils::read.csv(output)$condition_id, 9L)
})

test_that("computing a measure creates the measures folder of a DGM that has none yet", {
  root <- withr::local_tempdir()
  local_mocked_bindings(.get_path = function() root)
  local_result_reader()
  expect_false(dir.exists(file.path(root, "no_bias", "measures")))
  expect_true(source_test_single())
  expect_true(file.exists(file.path(root, "no_bias", "measures", "bias.csv")))
  expect_equal(nrow(utils::read.csv(file.path(root, "no_bias", "measures", "bias.csv"))), 1L)
})
