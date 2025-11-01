#' Compare method with a Single Measure for a DGM
#'
#' @description
#' This function provides pairwise comparison of method for Data-Generating
#' Mechanisms (DGMs). It compares method performance on a condition-by-condition
#' basis using estimates. For each pair of method, if method A has an estimate
#' closer to the true value than method B, it gets a score of 1, if further it
#' gets 0, and if equal it gets 0.5.
#'
#' @inheritParams download_dgm_datasets
#' @inheritParams compute_single_measure
#' @inheritParams compute_measures
#'
#' @return Data frame with pairwise comparison scores in long format (method_a, method_b, score)
#'
#'
#' @export
compare_single_measure <- function(dgm_name, measure_name, method, method_setting, conditions,
                                   estimate_col = "estimate", true_effect_col = "mean_effect",
                                   convergence_col = "convergence",
                                   method_replacements = NULL,
                                   n_repetitions = 1000, overwrite = FALSE, ...) {

  # Validate that method and method_setting have the same length
  if (length(method) != length(method_setting))
    stop("method and method_setting must have the same length")

  # Get DGM conditions
  if (is.null(conditions))
    conditions <- dgm_conditions(dgm_name)

  # Validate method_replacements
  if (!is.null(method_replacements)) {
    if (!is.list(method_replacements))
      stop("method_replacements must be a named list")
    # Check that each replacement contains valid method-setting combinations
    for (method_name in names(method_replacements)) {
      replacements <- method_replacements[[method_name]]
      if (is.null(replacements))
        next
      if (!is.list(replacements))
        stop(paste0("Each element in method_replacements must be a list for ", method_name))
      if (!"method" %in% names(replacements) ||
          !"method_setting" %in% names(replacements))
        stop(paste0("Each replacement must contain 'method' and 'method_setting' elements for ", method_name))
      if (length(replacements$method) != length(replacements$method_setting))
        stop(paste0("method and method_setting must have the same length for ", method_name))
    }
  }

  # Need at least 2 method for comparison
  if (length(method) < 2)
    stop("At least 2 method are required for pairwise comparison")

  # Create a file name
  file_name <- paste0(measure_name, "-pairwise", if (is.null(method_replacements)) ".csv" else "-replacement.csv")

  path <- PublicationBiasBenchmark.get_option("resources_directory")
  if (is.null(path))
    stop("The resources location needs to be specified via the `PublicationBiasBenchmark.get_option('resources_directory')` function.", call. = FALSE)
  
  output_folder <- file.path(path, dgm_name, "measures")
  output_file   <- file.path(output_folder, file_name)

  # Check if results already exist
  existing_results       <- NULL
  comparisons_to_compute <- NULL

  if (overwrite || !file.exists(output_file)) {

    # Create all possible method pairs
    method_pairs <- expand.grid(
      method_a = paste0(method, "-", method_setting),
      method_b = paste0(method, "-", method_setting),
      stringsAsFactors = FALSE
    )
    # Remove self-comparisons
    comparisons_to_compute <- method_pairs[method_pairs$method_a != method_pairs$method_b, ]

    # Remove duplicate pairs (A vs B and B vs A are the same)
    method_pairs <- method_pairs[!duplicated(t(apply(method_pairs, 1, sort))), ]

  } else {

    existing_results <- utils::read.csv(output_file)

    # Create all possible method pairs
    method_pairs <- expand.grid(
      method_a = paste0(method, "-", method_setting),
      method_b = paste0(method, "-", method_setting),
      stringsAsFactors = FALSE
    )
    # Remove self-comparisons
    method_pairs <- method_pairs[method_pairs$method_a != method_pairs$method_b, ]

    # Remove duplicate pairs (A vs B and B vs A are the same)
    method_pairs <- method_pairs[!duplicated(t(apply(method_pairs, 1, sort))), ]

    # Check existing comparisons (A vs B is same as B vs A)
    existing_pairs <- paste0(
      pmin(as.character(existing_results$method_a), as.character(existing_results$method_b)),
      "_vs_",
      pmax(as.character(existing_results$method_a), as.character(existing_results$method_b)))
    current_pairs <- paste0(
      pmin(as.character(method_pairs$method_a), as.character(method_pairs$method_b)),
      "_vs_",
      pmax(as.character(method_pairs$method_a), as.character(method_pairs$method_b)))

    # Find pairs that need to be computed
    pairs_to_compute <- which(!current_pairs %in% existing_pairs)

    if (length(pairs_to_compute) == 0) {
      # All comparisons already computed
      return(existing_results)
    }

    comparisons_to_compute <- method_pairs[pairs_to_compute, ]
  }

  # Preload all method results and apply replacements if needed
  method_results_list <- list()

  # Preload replacement method
  method_replacements_results <- list()
  if (!is.null(method_replacements)) {
    for (method_name in names(method_replacements)) {

      replacement_spec <- method_replacements[[method_name]]
      method_replacements_results[[method_name]] <- list()

      for (i in seq_along(replacement_spec$method)) {
        # Get replacement method info
        replacement_method  <- replacement_spec$method[i]
        replacement_setting <- replacement_spec$method_setting[i]
        replacement_key     <- paste0(replacement_method, "-", replacement_setting)

        method_replacements_results[[method_name]][[replacement_key]] <- retrieve_dgm_results(
          dgm_name       = dgm_name,
          method         = replacement_method,
          method_setting = replacement_setting
        )
      }
    }
  }

  for (i in seq_along(method)) {
    this_method         <- method[i]
    this_method_setting <- method_setting[i]
    method_key          <- paste0(this_method, "-", this_method_setting)

    # Retrieve the precomputed results
    method_results <- retrieve_dgm_results(
      dgm_name       = dgm_name,
      method         = this_method,
      method_setting = this_method_setting
    )

    # Check that all pre-specified columns exist
    columns_required <- c(convergence_col, estimate_col)
    if (!all(columns_required %in% names(method_results)))
      stop(sprintf("The following columns are undefined: %s",
                   paste(columns_required[!columns_required %in% names(method_results)], collapse = ", ")))

    # Apply replacements if specified
    if (!is.null(method_replacements)) {
      for (condition in conditions$condition_id) {
        condition_results <- method_results[method_results$condition_id == condition, , drop = FALSE]

        # Replace results in case of missingness
        method_name <- paste0(this_method, "-", this_method_setting)
        if (!all(condition_results[[convergence_col]]) && !is.null(method_replacements[[method_name]])) {

          # Subset converged results
          condition_results <- condition_results[condition_results[[convergence_col]], , drop = FALSE]

          # Find missing repetitions
          repetitions_all     <- 1:n_repetitions
          repetitions_missing <- repetitions_all[!repetitions_all %in% condition_results[["repetition_id"]][condition_results[[convergence_col]]]]

          # Fill in the missing repetitions
          replacement_spec <- method_replacements[[method_name]]

          for (j in seq_along(replacement_spec$method)) {
            # Break if all missing repetitions are replaced
            if (length(repetitions_missing) == 0)
              break

            # Get replacement method info
            replacement_method  <- replacement_spec$method[j]
            replacement_setting <- replacement_spec$method_setting[j]
            replacement_key     <- paste0(replacement_method, "-", replacement_setting)

            # Find missing repetitions
            temp_replacement <- method_replacements_results[[method_name]][[replacement_key]]
            temp_replacement <- temp_replacement[temp_replacement$condition_id == condition & temp_replacement[[convergence_col]], , drop = FALSE]
            temp_replacement <- temp_replacement[temp_replacement[["repetition_id"]] %in% repetitions_missing, , drop = FALSE]

            # Merge and update
            condition_results <- safe_rbind(list(condition_results, temp_replacement))
            repetitions_missing <- repetitions_all[!repetitions_all %in% condition_results[["repetition_id"]][condition_results[[convergence_col]]]]
          }

          # Update the main results
          method_results[method_results$condition_id == condition, ] <- condition_results[
            match(method_results$repetition_id[method_results$condition_id == condition], condition_results$repetition_id),
            colnames(condition_results)[colnames(condition_results) %in% colnames(method_results)]]
        }
      }
    }

    method_results_list[[method_key]] <- method_results
  }

  comparison_out <- list()

  # Perform pairwise comparisons
  for (idx in seq_len(nrow(comparisons_to_compute))) {
    method_a_key <- comparisons_to_compute$method_a[idx]
    method_b_key <- comparisons_to_compute$method_b[idx]

    # Skip if we already computed B vs A (since A vs B = B vs A)
    reverse_key <- paste0(method_b_key, "_vs_", method_a_key)
    if (reverse_key %in% names(comparison_out)) next

    method_a_results <- method_results_list[[method_a_key]]
    method_b_results <- method_results_list[[method_b_key]]

    for (condition in conditions$condition_id) {

      comparison_out[[idx]] <- data.frame(
        method_a      = method_a_key,
        method_b      = method_b_key,
        condition_id  = condition,
        score         = NA,
        n_comparisons = 0
      )

      # Get the true effect for this condition
      true_effect <- conditions[conditions$condition_id == condition, true_effect_col]

      # Filter results for this condition and converged results only
      method_a_condition <- method_a_results[method_a_results$condition_id == condition & method_a_results[[convergence_col]], , drop = FALSE]
      method_b_condition <- method_b_results[method_b_results$condition_id == condition & method_b_results[[convergence_col]], , drop = FALSE]

      # Find common repetitions (matched pairs)
      common_repetitions <- intersect(method_a_condition$repetition_id, method_b_condition$repetition_id)

      if (length(common_repetitions) == 0) {
        warning(paste("No common converged repetitions for method", method_a_key, "and", method_b_key, "in condition", condition))
        next
      }

      # Subset to common repetitions
      method_a_matched <- method_a_condition[method_a_condition$repetition_id %in% common_repetitions, ]
      method_b_matched <- method_b_condition[method_b_condition$repetition_id %in% common_repetitions, ]

      # Check order by repetition_id to ensure proper matching
      if (!all(method_a_matched$repetition_id == method_b_matched$repetition_id)) {
        method_a_matched <- method_a_matched[order(method_a_matched$repetition_id), ]
        method_b_matched <- method_b_matched[order(method_b_matched$repetition_id), ]
      }
      # Get estimates
      estimates_a <- method_a_matched[[estimate_col]]
      estimates_b <- method_b_matched[[estimate_col]]

      # Remove NA estimates
      valid_idx <- !is.na(estimates_a) & !is.na(estimates_b)
      if (sum(valid_idx) == 0) {
        warning(paste("No valid estimate pairs for method", method_a_key, "and", method_b_key, "in condition", condition))
        next
      }

      estimates_a <- estimates_a[valid_idx]
      estimates_b <- estimates_b[valid_idx]

      # Compute distances from true effect
      dist_a <- abs(estimates_a - true_effect)
      dist_b <- abs(estimates_b - true_effect)

      # Compute scores
      score <- ifelse(dist_a == dist_b, 0.5, ifelse(dist_a > dist_b, 0, 1))

      # Update output
      comparison_out[[idx]]$score         <- mean(score)
      comparison_out[[idx]]$n_comparisons <- length(score)
    }
  }

  # Merge into data.frame
  new_results <- safe_rbind(comparison_out)

  # Combine existing and new results
  if (!is.null(existing_results)) {
    new_results <- safe_rbind(list(new_results, existing_results))
  }

  return(new_results)
}

#' Compare method with Multiple Measures for a DGM
#'
#' @description
#' This is a high-level wrapper function that computes multiple pairwise comparison
#' measures for a Data-Generating Mechanism (DGM) and saves the results to CSV files.
#' It provides a clean and extensible interface for comparing method performance.
#'
#' @inheritParams download_dgm_datasets
#' @inheritParams compute_single_measure
#' @inheritParams compute_measures
#'
#' @return Invisible list of computed comparison data frames
#'
#'
#' @export
compare_measures <- function(dgm_name, method, method_setting, measures = NULL, verbose = TRUE,
                             estimate_col = "estimate", true_effect_col = "mean_effect",
                             convergence_col = "convergence",
                             method_replacements = NULL,
                             n_repetitions = 1000, overwrite = FALSE, conditions = NULL) {

  # Input validation downstream
  # Define all available comparison measures if not specified
  if (is.null(measures))
    measures <- c("estimate_comparison")

  path <- PublicationBiasBenchmark.get_option("resources_directory")
  if (is.null(path))
    stop("The resources location needs to be specified via the `PublicationBiasBenchmark.get_option('resources_directory')` function.", call. = FALSE)

  # Ensure output directory exists
  output_folder <- file.path(path, dgm_name, "measures")
  if (!dir.exists(output_folder))
    dir.create(output_folder, recursive = TRUE)

  # Compute each comparison measure
  results <- list()

  for (measure in measures) {

    # Currently only estimate_comparison is supported
    if (!measure %in% c("estimate_comparison"))
      stop(paste0("Unknown comparison measure: ", measure, ". Skipping."))

    # Specify file name
  file_name <- paste0(measure, "-pairwise", if (is.null(method_replacements)) ".csv" else "-replacement.csv")
    output_file <- file.path(output_folder, file_name)

    # If overwrite is TRUE, remove existing file to start fresh
    if (overwrite && file.exists(output_file)) {
      if (verbose)
        message("Overwriting existing ", measure, " comparison results at ", output_file)
      file.remove(output_file)
    }

    if (verbose) {
      if (file.exists(output_file) && !overwrite) {
        message("Computing missing ", measure, " comparison results...")
      } else {
        message("Computing ", measure, " comparisons...")
      }
    }

    measure_result <- compare_single_measure(
      dgm_name            = dgm_name,
      measure_name        = measure,
      method             = method,
      method_setting     = method_setting,
      conditions          = conditions,
      estimate_col        = estimate_col,
      true_effect_col     = true_effect_col,
      convergence_col     = convergence_col,
      method_replacements = method_replacements,
      n_repetitions       = n_repetitions,
      overwrite           = overwrite
    )

    # Save results (measure_result already contains combined existing + new results if applicable)
    utils::write.csv(measure_result, file = output_file, row.names = FALSE)

    if (verbose)
      message("Saved ", measure, " comparison results to ", output_file)

    results[[measure]] <- measure_result
  }

  return(invisible(results))
}
