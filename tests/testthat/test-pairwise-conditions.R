# Synthetic results: in condition c, method A is closer to the true effect than
# method B in the first k[c] of 4 repetitions (pair score 1) and farther in the
# rest (pair score 0), so the hand-computed score of A against B is k[c] / 4.
pairwise_test_conditions <- data.frame(condition_id = 1:3, mean_effect = c(0, 0.3, 0))
pairwise_test_k <- c(1L, 2L, 3L)

pairwise_test_results <- function(method, repetitions = 4L) {
  rows <- lapply(pairwise_test_conditions$condition_id, function(cid) {
    truth <- pairwise_test_conditions$mean_effect[cid]
    r <- seq_len(repetitions)
    distance <- if (method == "A") ifelse(r <= pairwise_test_k[cid], 0.1, 0.3) else
      if (method == "C") rep(0.05, repetitions) else rep(0.2, repetitions)
    data.frame(method = method, method_setting = "default", condition_id = cid, repetition_id = r,
               estimate = truth + distance, convergence = TRUE, stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

local_pairwise_reader <- function(env = parent.frame()) {
  root <- withr::local_tempdir(.local_envir = env)
  dir.create(file.path(root, "no_bias", "measures"), recursive = TRUE)
  testthat::local_mocked_bindings(
    .get_path = function() root,
    retrieve_dgm_results = function(dgm_name, method = NULL, method_setting = NULL, condition_id = NULL,
                                    repetition_id = NULL, release = NULL, source = c("release", "local"))
      pairwise_test_results(method),
    .env = env)
  invisible(root)
}

test_that("pairwise comparison keeps every condition for each method pair", {
  local_pairwise_reader()
  out <- compare_single_measure("no_bias", "estimate_comparison", c("A", "B"), c("default", "default"),
                                conditions = pairwise_test_conditions, n_repetitions = 4,
                                overwrite = TRUE, results_source = "local")
  expect_equal(sort(unique(out$condition_id)), 1:3)
  expect_equal(anyDuplicated(out[c("method_a", "method_b", "condition_id")]), 0L)
  expect_true(all(out$n_comparisons == 4))
  # Score of A against B, whichever way the pair is oriented in the output
  a_score <- ifelse(out$method_a == "A-default", out$score, 1 - out$score)
  by_condition <- split(a_score, out$condition_id)
  expect_equal(lengths(by_condition, use.names = FALSE), rep(unique(lengths(by_condition)), 3L))
  expect_equal(vapply(by_condition, function(x) unique(x), numeric(1), USE.NAMES = FALSE), pairwise_test_k / 4)
})

# One direction per unordered method pair, as when comparisons are added to a file
test_that("each unordered method pair is computed once per condition", {
  local_pairwise_reader()
  out <- compare_single_measure("no_bias", "estimate_comparison", c("A", "B"), c("default", "default"),
                                conditions = pairwise_test_conditions, n_repetitions = 4,
                                overwrite = TRUE, results_source = "local")
  expect_equal(nrow(out), nrow(pairwise_test_conditions))
})

pairwise_key <- function(out) paste(pmin(out$method_a, out$method_b), pmax(out$method_a, out$method_b), out$condition_id)

test_that("adding a method to an existing comparison file appends one row per new pair and condition", {
  root <- local_pairwise_reader()
  compare <- function(methods, ...) compare_single_measure("no_bias", "estimate_comparison", methods, rep("default", length(methods)),
    conditions = pairwise_test_conditions, n_repetitions = 4, results_source = "local", ...)
  first <- compare(c("A", "B"), overwrite = TRUE)
  expect_equal(nrow(first), 3L)
  utils::write.csv(first, file.path(root, "no_bias", "measures", "estimate_comparison-pairwise.csv"), row.names = FALSE)
  grown <- compare(c("A", "B", "C"))
  # Two new unordered pairs (A-C, B-C) in three conditions join the three existing rows.
  expect_equal(nrow(grown), 3L + 2L * 3L)
  expect_equal(anyDuplicated(pairwise_key(grown)), 0L)
  expect_setequal(grown$condition_id, 1:3)
  expect_true(all(grown$n_comparisons == 4))
  fresh <- grown[!pairwise_key(grown) %in% pairwise_key(first), ]
  expect_equal(nrow(fresh), 6L)
  expect_equal(sort(unique(paste(pmin(fresh$method_a, fresh$method_b), pmax(fresh$method_a, fresh$method_b)))),
               c("A-default C-default", "B-default C-default"))
  # C is always the closest method, whichever way a pair is oriented.
  c_score <- ifelse(fresh$method_a == "C-default", fresh$score, 1 - fresh$score)
  expect_true(all(c_score == 1))
  # The rows of the first run are kept unchanged.
  expect_equal(grown[pairwise_key(grown) %in% pairwise_key(first), ][order(grown$condition_id[pairwise_key(grown) %in% pairwise_key(first)]), "score"],
               first$score[order(first$condition_id)])
  # A fresh run over three methods agrees on the set of rows.
  all_fresh <- compare(c("A", "B", "C"), overwrite = TRUE)
  expect_equal(nrow(all_fresh), 9L)
  expect_setequal(pairwise_key(all_fresh), pairwise_key(grown))
})
