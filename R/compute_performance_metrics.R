#' Compute Performance Metrics for a DGM
#'
#' @description
#' This function provides a modular and extensible way to compute performance
#' metrics for Data Generating Mechanisms (DGMs). It handles different types
#' of metrics and automatically determines the required arguments for each metric
#' function.
#'
#' @param dgm_name Character string specifying the DGM name
#' @param metric_name Name of the metric to compute (e.g., "bias", "mse")
#' @param methods Character vector of method names
#' @param method_settings Character vector of method settings, must be same length as methods
#' @param conditions Data frame of conditions from dgm_conditions()
#' @param results_folder Path to folder containing pre-computed results
#' @param metric_fun Function to compute the metric
#' @param metric_mcse_fun Function to compute the MCSE for the metric
#' @param power_test_type Character string specifying the test type for power computation:
#' "pvalue" (default) or "bayes_factor"
#' @param power_threshold Numeric threshold for power computation. For p-values,
#' default is 0.05 (reject if p < 0.05). For Bayes factors, default is 10
#' (reject if BF > 10, indicating strong evidence for H1)
#' @param estimate_col Character string specifying the column name containing parameter estimates. Default is "estimate"
#' @param true_effect_col Character string specifying the column name in conditions data frame containing true effect sizes. Default is "mean_effect"
#' @param ci_lower_col Character string specifying the column name containing lower confidence interval bounds. Default is "ci_lower"
#' @param ci_upper_col Character string specifying the column name containing upper confidence interval bounds. Default is "ci_upper"
#' @param pvalue_col Character string specifying the column name containing p-values. Default is "p_value"
#' @param bf_col Character string specifying the column name containing Bayes factors. Default is "BF"
#' @param convergence_col Character string specifying the column name containing convergence indicators. Default is "convergence"
#' @param overwrite Logical indicating whether to overwrite existing results. If FALSE (default), will skip computation for method-metric combinations that already exist
#' @param output_folder Path to folder where results should be saved. Only used when overwrite = FALSE to check for existing files
#' @param ... Additional arguments passed to metric functions
#'
#' @return Data frame with computed metrics and MCSEs
#'
#' @examples
#' \dontrun{
#' # Get conditions for a DGM
#' conditions <- dgm_conditions("no_bias")
#'
#' # Compute bias for specific methods
#' bias_results <- compute_single_metric(
#'   dgm_name = "no_bias",
#'   metric_name = "bias",
#'   methods = c("RMA", "PET"),
#'   method_settings = c("default", "default"),
#'   conditions = conditions,
#'   results_folder = "simulations",
#'   metric_fun = bias,
#'   metric_mcse_fun = bias_mcse
#' )
#'
#' # Compute power using Bayes factors
#' power_results <- compute_single_metric(
#'   dgm_name = "no_bias",
#'   metric_name = "power",
#'   methods = c("RMA", "PET"),
#'   method_settings = c("default", "default"),
#'   conditions = conditions,
#'   results_folder = "simulations",
#'   metric_fun = power,
#'   metric_mcse_fun = power_mcse,
#'   power_test_type = "bayes_factor",
#'   power_threshold = 6
#' )
#'
#' }
#'
#' @export
compute_single_metric <- function(dgm_name, metric_name, methods, method_settings, conditions, results_folder,
                                 metric_fun, metric_mcse_fun,
                                 power_test_type = "pvalue", power_threshold = NULL,
                                 estimate_col = "estimate", true_effect_col = "mean_effect",
                                 ci_lower_col = "ci_lower", ci_upper_col = "ci_upper",
                                 pvalue_col = "p_value", bf_col = "BF", convergence_col = "convergence",
                                 overwrite = FALSE, output_folder = NULL, ...) {

  # Validate that methods and method_settings have the same length
  if (length(methods) != length(method_settings)) {
    stop("methods and method_settings must have the same length")
  }

  # Set default threshold based on test type
  if (is.null(power_threshold)) {
    power_threshold <- if (power_test_type == "pvalue") 0.05 else 10
  }

  # Validate power test type
  if (!power_test_type %in% c("pvalue", "bayes_factor")) {
    stop("power_test_type must be either 'pvalue' or 'bayes_factor'")
  }

  # Check if results already exist (only if overwrite is FALSE and output_folder is provided)
  existing_results <- NULL
  methods_to_compute <- seq_along(methods)

  if (!overwrite && !is.null(output_folder)) {
    output_file <- file.path(output_folder, paste0(metric_name, ".csv"))
    if (file.exists(output_file)) {
      existing_results <- utils::read.csv(output_file, stringsAsFactors = FALSE)

      # Check which method-method_setting combinations already have results
      existing_combinations <- paste0(existing_results$method, "-", existing_results$method_setting)
      current_combinations <- paste0(methods, "-", method_settings)

      # Find indices of combinations that need to be computed
      methods_to_compute <- which(!current_combinations %in% existing_combinations)

      if (length(methods_to_compute) == 0) {
        # All combinations already computed, return existing results for these combinations
        return(existing_results[existing_combinations %in% current_combinations, ])
      }
    }
  }

  metric_out <- list()

  # Create dynamic column names based on metric
  metric_col_name <- metric_name
  mcse_col_name <- paste0(metric_name, "_mcse")

  for (i in methods_to_compute) {
    method <- methods[i]
    method_setting <- method_settings[i]
print(method)
print(method_setting)
    for (condition in conditions$condition_id) {

      # Retrieve the precomputed results
      method_condition_results <- retrieve_dgm_results(
        dgm_name     = dgm_name,
        method       = method,
        method_setting = method_setting,
        condition_id = condition,
        path         = results_folder
      )

      # Filter for converged results if we're not computing convergence metric
      if (metric_name != "convergence") {
        if (convergence_col %in% names(method_condition_results)) {
          method_condition_results <- method_condition_results[method_condition_results[[convergence_col]] == TRUE, , drop = FALSE]
        }

        # If no converged results remain, create NA result
        if (nrow(method_condition_results) == 0) {
          warning(paste("No converged results for method", method, "method_setting", method_setting, "condition", condition, "- setting values to NA"))
          # Create result with NAs
          result_df <- data.frame(
            method         = method,
            method_setting = method_setting,
            condition_id   = condition
          )
          result_df[[metric_col_name]] <- NA
          result_df[[mcse_col_name]] <- NA
          key <- paste0(method, "-", method_setting, "-", condition)
          metric_out[[key]] <- result_df
          next
        }
      }

      # Compute the metric based on its requirements
      key <- paste0(method, "-", method_setting, "-", condition)

      # Get the true effect for this condition
      true_effect <- conditions[conditions$condition_id == condition, true_effect_col]

      result_df <- data.frame(
        method         = method,
        method_setting = method_setting,
        condition_id   = condition
      )

      # Compute metric and MCSE based on metric type
      if (metric_name == "convergence") {
        # Convergence metric - proportion of methods that converged
        if (!convergence_col %in% names(method_condition_results)) {
          stop(paste(convergence_col, "column not found in results for convergence metric computation"))
        }
        convergence_indicator <- method_condition_results[[convergence_col]]
        result_df[[metric_col_name]] <- metric_fun(
          test_rejects_h0 = convergence_indicator
        )
        result_df[[mcse_col_name]] <- metric_mcse_fun(
          test_rejects_h0 = convergence_indicator
        )

      } else if (metric_name == "bias") {
        # Bias metric
        estimates <- method_condition_results[[estimate_col]]
        # Remove NAs
        valid_idx <- !is.na(estimates)
        if (sum(valid_idx) == 0) {
          warning(paste("No valid estimates for method", method, "method_setting", method_setting, "condition", condition, "- setting values to NA"))
          result_df[[metric_col_name]] <- NA
          result_df[[mcse_col_name]] <- NA
        } else {
          estimates <- estimates[valid_idx]

          result_df[[metric_col_name]] <- metric_fun(
            theta_hat = estimates,
            theta     = true_effect
          )
          result_df[[mcse_col_name]] <- metric_mcse_fun(
            theta_hat = estimates
          )
        }

      } else if (metric_name %in% c("relative_bias", "mse", "rmse")) {
        # Metrics where MCSE functions need both theta_hat and theta
        estimates <- method_condition_results[[estimate_col]]
        # Remove NAs
        valid_idx <- !is.na(estimates)
        if (sum(valid_idx) == 0) {
          warning(paste("No valid estimates for method", method, "method_setting", method_setting, "condition", condition, "- setting values to NA"))
          result_df[[metric_col_name]] <- NA
          result_df[[mcse_col_name]] <- NA
        } else {
          estimates <- estimates[valid_idx]

          result_df[[metric_col_name]] <- metric_fun(
            theta_hat = estimates,
            theta     = true_effect
          )
          result_df[[mcse_col_name]] <- metric_mcse_fun(
            theta_hat = estimates,
            theta     = true_effect
          )
        }

      } else if (metric_name %in% c("empirical_variance", "empirical_se")) {
        # Metrics that only need theta_hat
        estimates <- method_condition_results[[estimate_col]]
        # Remove NAs
        valid_idx <- !is.na(estimates)
        if (sum(valid_idx) == 0) {
          warning(paste("No valid estimates for method", method, "method_setting", method_setting, "condition", condition, "- setting values to NA"))
          result_df[[metric_col_name]] <- NA
          result_df[[mcse_col_name]] <- NA
        } else {
          estimates <- estimates[valid_idx]

          result_df[[metric_col_name]] <- metric_fun(
            theta_hat = estimates
          )
          result_df[[mcse_col_name]] <- metric_mcse_fun(
            theta_hat = estimates
          )
        }

      } else if (metric_name == "coverage") {
        # Coverage needs CI bounds and true effect
        ci_lower <- method_condition_results[[ci_lower_col]]
        ci_upper <- method_condition_results[[ci_upper_col]]
        # Remove NAs
        valid_idx <- !is.na(ci_lower) & !is.na(ci_upper)

        if (sum(valid_idx) == 0) {
          warning(paste("No valid confidence intervals for method", method, "method_setting", method_setting, "condition", condition, "- setting values to NA"))
          result_df[[metric_col_name]] <- NA
          result_df[[mcse_col_name]] <- NA
        } else {
          ci_lower <- ci_lower[valid_idx]
          ci_upper <- ci_upper[valid_idx]

          result_df[[metric_col_name]] <- metric_fun(
            ci_lower = ci_lower,
            ci_upper = ci_upper,
            theta    = true_effect
          )
          result_df[[mcse_col_name]] <- metric_mcse_fun(
            ci_lower = ci_lower,
            ci_upper = ci_upper,
            theta    = true_effect
          )
        }

      } else if (metric_name == "power") {
        # Power needs test rejection indicator - dispatch based on test type
        if (power_test_type == "pvalue") {
          # Use p-values for power computation
          if (!pvalue_col %in% names(method_condition_results)) {
            stop(paste(pvalue_col, "column not found in results for p-value based power computation"))
          }
          p_values <- method_condition_results[[pvalue_col]]
          # Remove NAs
          valid_idx <- !is.na(p_values)
          if (sum(valid_idx) == 0) {
            warning(paste("No valid p-values for method", method, "method_setting", method_setting, "condition", condition, "- setting values to NA"))
            result_df[[metric_col_name]] <- NA
            result_df[[mcse_col_name]] <- NA
          } else {
            test_rejects_h0 <- p_values[valid_idx] < power_threshold
            result_df[[metric_col_name]] <- metric_fun(
              test_rejects_h0 = test_rejects_h0
            )
            result_df[[mcse_col_name]] <- metric_mcse_fun(
              test_rejects_h0 = test_rejects_h0
            )
          }
        } else if (power_test_type == "bayes_factor") {
          # Use Bayes factors for power computation
          if (!bf_col %in% names(method_condition_results)) {
            stop(paste(bf_col, "column not found in results for Bayes factor based power computation"))
          }
          bf_values <- method_condition_results[[bf_col]]
          # Remove NAs
          valid_idx <- !is.na(bf_values)
          if (sum(valid_idx) == 0) {
            warning(paste("No valid Bayes factors for method", method, "method_setting", method_setting, "condition", condition, "- setting values to NA"))
            result_df[[metric_col_name]] <- NA
            result_df[[mcse_col_name]] <- NA
          } else {
            # For Bayes factors, reject H0 (favor H1) when BF > threshold
            test_rejects_h0 <- bf_values[valid_idx] > power_threshold
            result_df[[metric_col_name]] <- metric_fun(
              test_rejects_h0 = test_rejects_h0
            )
            result_df[[mcse_col_name]] <- metric_mcse_fun(
              test_rejects_h0 = test_rejects_h0
            )
          }
        }

      } else if (metric_name == "mean_ci_width") {
        # CI width needs CI bounds
        ci_lower <- method_condition_results[[ci_lower_col]]
        ci_upper <- method_condition_results[[ci_upper_col]]
        # Remove NAs
        valid_idx <- !is.na(ci_lower) & !is.na(ci_upper)
        if (sum(valid_idx) == 0) {
          warning(paste("No valid confidence intervals for method", method, "method_setting", method_setting, "condition", condition, "- setting values to NA"))
          result_df[[metric_col_name]] <- NA
          result_df[[mcse_col_name]] <- NA
        } else {
          ci_lower <- ci_lower[valid_idx]
          ci_upper <- ci_upper[valid_idx]

          result_df[[metric_col_name]] <- metric_fun(
            ci_upper = ci_upper,
            ci_lower = ci_lower
          )
          result_df[[mcse_col_name]] <- metric_mcse_fun(
            ci_upper = ci_upper,
            ci_lower = ci_lower
          )
        }
      }

      metric_out[[key]] <- result_df
    }
  }

  # Combine new results with existing ones if applicable
  new_results <- do.call(rbind, metric_out)

  if (!is.null(existing_results)) {
    # Combine existing and new results
    all_results <- rbind(existing_results, new_results)
    return(all_results)
  }

  return(new_results)
}

#' Compute Multiple Performance Metrics for a DGM
#'
#' @description
#' This is a high-level wrapper function that computes multiple performance
#' metrics for a Data Generating Mechanism (DGM) and saves the results to CSV files.
#' It provides a clean and extensible interface for computing standard simulation
#' performance metrics.
#'
#' @param dgm_name Character string specifying the DGM name
#' @param methods Character vector of method names
#' @param method_settings Character vector of method settings, must be same length as methods
#' @param results_folder Path to folder containing pre-computed results
#' @param output_folder Path to folder where results should be saved
#' @param metrics Character vector of metrics to compute. If NULL, computes all standard metrics.
#' @param verbose Logical indicating whether to print progress messages
#' @param power_test_type Character string specifying the test type for power computation:
#' "pvalue" (default) or "bayes_factor"
#' @param power_threshold Numeric threshold for power computation. For p-values,
#' default is 0.05 (reject if p < 0.05). For Bayes factors, default is 10
#' (reject if BF > 10, indicating strong evidence for H1)
#' @param estimate_col Character string specifying the column name containing parameter estimates. Default is "estimate"
#' @param true_effect_col Character string specifying the column name in conditions data frame containing true effect sizes. Default is "mean_effect"
#' @param ci_lower_col Character string specifying the column name containing lower confidence interval bounds. Default is "ci_lower"
#' @param ci_upper_col Character string specifying the column name containing upper confidence interval bounds. Default is "ci_upper"
#' @param pvalue_col Character string specifying the column name containing p-values. Default is "p_value"
#' @param bf_col Character string specifying the column name containing Bayes factors. Default is "BF"
#' @param convergence_col Character string specifying the column name containing convergence indicators. Default is "convergence"
#' @param overwrite Logical indicating whether to overwrite existing results. If FALSE (default), will skip computation for method-metric combinations that already exist
#'
#' @return Invisible list of computed metrics data frames
#'
#' @examples
#' \dontrun{
#' # Compute all standard metrics for no_bias DGM
#' results <- compute_metrics(
#'   dgm_name = "no_bias",
#'   methods = c("RMA", "PET", "PEESE", "PETPEESE"),
#'   method_settings = c("default", "default", "default", "default"),
#'   results_folder = "simulations",
#'   output_folder = "results"
#' )
#'
#' # Compute only specific metrics
#' results <- compute_metrics(
#'   dgm_name = "no_bias",
#'   methods = c("RMA", "PET"),
#'   method_settings = c("default", "default"),
#'   results_folder = "simulations",
#'   output_folder = "results",
#'   metrics = c("bias", "mse", "coverage")
#' )
#'
#' # Compute metrics with Bayes factor based power (BF > 6 for significance)
#' results <- compute_metrics(
#'   dgm_name = "no_bias",
#'   methods = c("RMA", "PET"),
#'   method_settings = c("default", "default"),
#'   results_folder = "simulations",
#'   output_folder = "results",
#'   power_test_type = "bayes_factor",
#'   power_threshold = 6
#' )
#'
#' # Compute metrics with custom p-value threshold
#' results <- compute_metrics(
#'   dgm_name = "no_bias",
#'   methods = c("RMA", "PET"),
#'   method_settings = c("default", "default"),
#'   results_folder = "simulations",
#'   output_folder = "results",
#'   power_test_type = "pvalue",
#'   power_threshold = 0.01
#' )
#'
#' # Include convergence metric
#' results <- compute_metrics(
#'   dgm_name = "no_bias",
#'   methods = c("RMA", "PET"),
#'   method_settings = c("default", "default"),
#'   results_folder = "simulations",
#'   output_folder = "results",
#'   metrics = c("bias", "mse", "coverage", "power", "convergence")
#' )
#'
#' # Use custom column names
#' results <- compute_metrics(
#'   dgm_name = "no_bias",
#'   methods = c("RMA", "PET"),
#'   method_settings = c("default", "default"),
#'   results_folder = "simulations",
#'   output_folder = "results",
#'   estimate_col = "my_estimate",
#'   true_effect_col = "true_value",
#'   ci_lower_col = "lower_ci",
#'   ci_upper_col = "upper_ci",
#'   pvalue_col = "pval",
#'   bf_col = "bayes_factor",
#'   convergence_col = "converged"
#' )
#' }
#'
#' @export
compute_metrics <- function(dgm_name, methods, method_settings, results_folder = "simulations",
                           output_folder = "results", metrics = NULL, verbose = TRUE,
                           power_test_type = "pvalue", power_threshold = NULL,
                           estimate_col = "estimate", true_effect_col = "mean_effect",
                           ci_lower_col = "ci_lower", ci_upper_col = "ci_upper",
                           pvalue_col = "p_value", bf_col = "BF", convergence_col = "convergence",
                           overwrite = FALSE) {

  # Validate that methods and method_settings have the same length
  if (length(methods) != length(method_settings)) {
    stop("methods and method_settings must have the same length")
  }

  # Get DGM conditions
  conditions <- dgm_conditions(dgm_name)

  # Define all available metrics if not specified
  if (is.null(metrics)) {
    metrics <- c("bias", "relative_bias", "mse", "rmse", "empirical_variance",
                "empirical_se", "coverage", "power", "mean_ci_width", "convergence")
  }

  # Define metric functions
  metric_functions <- list(
    bias               = list(fun = bias,               mcse_fun = bias_mcse),
    relative_bias      = list(fun = relative_bias,      mcse_fun = relative_bias_mcse),
    mse                = list(fun = mse,                mcse_fun = mse_mcse),
    rmse               = list(fun = rmse,               mcse_fun = rmse_mcse),
    empirical_variance = list(fun = empirical_variance, mcse_fun = empirical_variance_mcse),
    empirical_se       = list(fun = empirical_se,       mcse_fun = empirical_se_mcse),
    coverage           = list(fun = coverage,           mcse_fun = coverage_mcse),
    power              = list(fun = power,              mcse_fun = power_mcse),
    mean_ci_width      = list(fun = mean_ci_width,      mcse_fun = mean_ci_width_mcse),
    convergence        = list(fun = power,              mcse_fun = power_mcse)
  )

  # Ensure output directory exists
  if (!dir.exists(output_folder)) {
    dir.create(output_folder, recursive = TRUE)
  }

  # Compute each metric
  results <- list()

  for (metric in metrics) {
    if (!metric %in% names(metric_functions)) {
      warning("Unknown metric: ", metric, ". Skipping.")
      next
    }

    output_file <- file.path(output_folder, paste0(metric, ".csv"))

    # If overwrite is TRUE, remove existing file to start fresh
    if (overwrite && file.exists(output_file)) {
      if (verbose) {
        cat("Overwriting existing", metric, "results at", output_file, "\n")
      }
      file.remove(output_file)
    }

    if (verbose) {
      if (file.exists(output_file) && !overwrite) {
        cat("Computing missing", metric, "results...\n")
      } else {
        cat("Computing", metric, "...\n")
      }
    }

    metric_result <- compute_single_metric(
      dgm_name          = dgm_name,
      metric_name       = metric,
      methods           = methods,
      method_settings   = method_settings,
      conditions        = conditions,
      results_folder    = results_folder,
      metric_fun        = metric_functions[[metric]]$fun,
      metric_mcse_fun   = metric_functions[[metric]]$mcse_fun,
      power_test_type   = power_test_type,
      power_threshold   = power_threshold,
      estimate_col      = estimate_col,
      true_effect_col   = true_effect_col,
      ci_lower_col      = ci_lower_col,
      ci_upper_col      = ci_upper_col,
      pvalue_col        = pvalue_col,
      bf_col            = bf_col,
      convergence_col   = convergence_col,
      overwrite         = overwrite,
      output_folder     = output_folder
    )

    # Save results (metric_result already contains combined existing + new results if applicable)
    utils::write.csv(metric_result, file = output_file, row.names = FALSE)

    if (verbose) {
      cat("Saved", metric, "results to", output_file, "\n")
    }

    results[[metric]] <- metric_result
  }

  return(invisible(results))
}
