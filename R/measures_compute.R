#' Compute Performance Measures for a DGM
#'
#' @description
#' This function provides a modular and extensible way to compute performance
#' measures (PM) for Data Generating Mechanisms (DGMs). It handles different types
#' of measures and automatically determines the required arguments for each measure
#' function.
#'
#' @param dgm_name Character string specifying the DGM name
#' @param measure_name Name of the measure to compute (e.g., "bias", "mse")
#' @param methods Character vector of method names
#' @param method_settings Character vector of method settings, must be same length as methods
#' @param conditions Data frame of conditions from dgm_conditions()
#' @param measure_fun Function to compute the measure
#' @param measure_mcse_fun Function to compute the MCSE for the measure
#' @param power_test_type Character string specifying the test type for power computation:
#' "p_value" (default) or "bayes_factor"
#' @param power_threshold Numeric threshold for power computation. For p-values,
#' default is 0.05 (reject if p < 0.05). For Bayes factors, default is 10
#' (reject if BF > 10, indicating strong evidence for H1)
#' @param estimate_col Character string specifying the column name containing parameter estimates. Default is "estimate"
#' @param true_effect_col Character string specifying the column name in conditions data frame containing true effect sizes. Default is "mean_effect"
#' @param ci_lower_col Character string specifying the column name containing lower confidence interval bounds. Default is "ci_lower"
#' @param ci_upper_col Character string specifying the column name containing upper confidence interval bounds. Default is "ci_upper"
#' @param p_value_col Character string specifying the column name containing p-values. Default is "p_value"
#' @param bf_col Character string specifying the column name containing Bayes factors. Default is "BF"
#' @param convergence_col Character string specifying the column name containing convergence indicators. Default is "convergence"
#' @param overwrite Logical indicating whether to overwrite existing results. If FALSE (default), will skip computation for method-measure combinations that already exist
#' @param ... Additional arguments passed to measure functions
#' @inheritParams download_dgm_datasets
#'
#' @return Data frame with computed measures and MCSEs
#'
#' @examples
#' \dontrun{
#' # Get conditions for a DGM
#' conditions <- dgm_conditions("no_bias")
#'
#' # Compute bias for specific methods
#' bias_results <- compute_single_measure(
#'   dgm_name = "no_bias",
#'   measure_name = "bias",
#'   methods = c("RMA", "PET"),
#'   method_settings = c("default", "default"),
#'   conditions = conditions,
#'   results_folder = "simulations",
#'   measure_fun = bias,
#'   measure_mcse_fun = bias_mcse
#' )
#'
#' # Compute power using Bayes factors
#' power_results <- compute_single_measure(
#'   dgm_name = "no_bias",
#'   measure_name = "power",
#'   methods = c("RMA", "PET"),
#'   method_settings = c("default", "default"),
#'   conditions = conditions,
#'   results_folder = "simulations",
#'   measure_fun = power,
#'   measure_mcse_fun = power_mcse,
#'   power_test_type = "bayes_factor",
#'   power_threshold = 6
#' )
#'
#' }
#'
#' @export
compute_single_measure <- function(dgm_name, measure_name, methods, method_settings, conditions,
                                   measure_fun, measure_mcse_fun,
                                   power_test_type = "p_value", power_threshold = NULL,
                                   estimate_col = "estimate", true_effect_col = "mean_effect",
                                   ci_lower_col = "ci_lower", ci_upper_col = "ci_upper",
                                   p_value_col = "p_value", bf_col = "BF", convergence_col = "convergence",
                                   overwrite = FALSE, path = NULL, ...) {

  # Validate that methods and method_settings have the same length
  if (length(methods) != length(method_settings))
    stop("methods and method_settings must have the same length")

  # Set default threshold based on test type
  if (is.null(power_threshold))
    power_threshold <- if (power_test_type == "p_value") 0.05 else 10

  # Validate power test type
  if (!power_test_type %in% c("p_value", "bayes_factor"))
    stop("power_test_type must be either 'p_value' or 'bayes_factor'")

  # Check if results already exist
  existing_results   <- NULL
  methods_to_compute <- seq_along(methods)

  # Specify directory structures
  if (is.null(path))
    path <- PublicationBiasBenchmark.get_option("simulation_directory")
  output_folder <- file.path(path, dgm_name, "measures")

  if (!overwrite) {
    output_file <- file.path(output_folder, paste0(measure_name, ".csv"))
    if (file.exists(output_file)) {
      existing_results <- utils::read.csv(output_file, stringsAsFactors = FALSE)

      # Check which method-method_setting combinations already have results
      existing_combinations <- paste0(existing_results$method, "-", existing_results$method_setting)
      current_combinations  <- paste0(methods, "-", method_settings)

      # Find indices of combinations that need to be computed
      methods_to_compute <- which(!current_combinations %in% existing_combinations)

      if (length(methods_to_compute) == 0) {
        # All combinations already computed, return existing results for these combinations
        return(existing_results[existing_combinations %in% current_combinations, ])
      }
    }
  }

  measure_out <- list()

  # Create dynamic column names based on measure
  measure_col_name <- measure_name
  mcse_col_name    <- paste0(measure_name, "_mcse")

  for (i in methods_to_compute) {

    method         <- methods[i]
    method_setting <- method_settings[i]

    # Retrieve the precomputed results
    method_results <- retrieve_dgm_results(
      dgm_name       = dgm_name,
      method         = method,
      method_setting = method_setting,
      path           = path
    )

    for (condition in conditions$condition_id) {

      # Select the condition results
      method_condition_results <- method_results[method_results$condition_id == condition,,drop = FALSE]

      # Filter for converged results if we're not computing convergence measure
      if (measure_name != "convergence") {
        if (convergence_col %in% names(method_condition_results)) {
          method_condition_results <- method_condition_results[method_condition_results[[convergence_col]] == TRUE,,drop = FALSE]
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
          result_df[[measure_col_name]] <- NA
          result_df[[mcse_col_name]]    <- NA
          key <- paste0(method, "-", method_setting, "-", condition)
          measure_out[[key]] <- result_df
          next
        }
      }

      # Compute the measure based on its requirements
      key <- paste0(method, "-", method_setting, "-", condition)

      # Get the true effect for this condition
      true_effect <- conditions[conditions$condition_id == condition, true_effect_col]

      result_df <- data.frame(
        method         = method,
        method_setting = method_setting,
        condition_id   = condition
      )

      # Compute measure and MCSE based on measure type
      if (measure_name == "convergence") {
        # Convergence measure - proportion of methods that converged
        if (!convergence_col %in% names(method_condition_results)) {
          stop(paste(convergence_col, "column not found in results for convergence measure computation"))
        }
        convergence_indicator <- method_condition_results[[convergence_col]]
        result_df[[measure_col_name]] <- measure_fun(
          test_rejects_h0 = convergence_indicator
        )
        result_df[[mcse_col_name]] <- measure_mcse_fun(
          test_rejects_h0 = convergence_indicator
        )

      } else if (measure_name == "bias") {
        # Bias measure
        estimates <- method_condition_results[[estimate_col]]
        # Remove NAs
        valid_idx <- !is.na(estimates)
        if (sum(valid_idx) == 0) {
          warning(paste("No valid estimates for method", method, "method_setting", method_setting, "condition", condition, "- setting values to NA"))
          result_df[[measure_col_name]] <- NA
          result_df[[mcse_col_name]] <- NA
        } else {
          estimates <- estimates[valid_idx]

          result_df[[measure_col_name]] <- measure_fun(
            theta_hat = estimates,
            theta     = true_effect
          )
          result_df[[mcse_col_name]] <- measure_mcse_fun(
            theta_hat = estimates
          )
        }

      } else if (measure_name %in% c("relative_bias", "mse", "rmse")) {
        # measures where MCSE functions need both theta_hat and theta
        estimates <- method_condition_results[[estimate_col]]
        # Remove NAs
        valid_idx <- !is.na(estimates)
        if (sum(valid_idx) == 0) {
          warning(paste("No valid estimates for method", method, "method_setting", method_setting, "condition", condition, "- setting values to NA"))
          result_df[[measure_col_name]] <- NA
          result_df[[mcse_col_name]] <- NA
        } else {
          estimates <- estimates[valid_idx]

          result_df[[measure_col_name]] <- measure_fun(
            theta_hat = estimates,
            theta     = true_effect
          )
          result_df[[mcse_col_name]] <- measure_mcse_fun(
            theta_hat = estimates,
            theta     = true_effect
          )
        }

      } else if (measure_name %in% c("empirical_variance", "empirical_se")) {
        # measures that only need theta_hat
        estimates <- method_condition_results[[estimate_col]]
        # Remove NAs
        valid_idx <- !is.na(estimates)
        if (sum(valid_idx) == 0) {
          warning(paste("No valid estimates for method", method, "method_setting", method_setting, "condition", condition, "- setting values to NA"))
          result_df[[measure_col_name]] <- NA
          result_df[[mcse_col_name]] <- NA
        } else {
          estimates <- estimates[valid_idx]

          result_df[[measure_col_name]] <- measure_fun(
            theta_hat = estimates
          )
          result_df[[mcse_col_name]] <- measure_mcse_fun(
            theta_hat = estimates
          )
        }

      } else if (measure_name == "coverage") {
        # Coverage needs CI bounds and true effect
        ci_lower <- method_condition_results[[ci_lower_col]]
        ci_upper <- method_condition_results[[ci_upper_col]]
        # Remove NAs
        valid_idx <- !is.na(ci_lower) & !is.na(ci_upper)

        if (sum(valid_idx) == 0) {
          warning(paste("No valid confidence intervals for method", method, "method_setting", method_setting, "condition", condition, "- setting values to NA"))
          result_df[[measure_col_name]] <- NA
          result_df[[mcse_col_name]] <- NA
        } else {
          ci_lower <- ci_lower[valid_idx]
          ci_upper <- ci_upper[valid_idx]

          result_df[[measure_col_name]] <- measure_fun(
            ci_lower = ci_lower,
            ci_upper = ci_upper,
            theta    = true_effect
          )
          result_df[[mcse_col_name]] <- measure_mcse_fun(
            ci_lower = ci_lower,
            ci_upper = ci_upper,
            theta    = true_effect
          )
        }

      } else if (measure_name == "power") {
        # Power needs test rejection indicator - dispatch based on test type
        if (power_test_type == "p_value") {
          # Use p-values for power computation
          if (!p_value_col %in% names(method_condition_results)) {
            stop(paste(p_value_col, "column not found in results for p-value based power computation"))
          }
          p_values <- method_condition_results[[p_value_col]]
          # Remove NAs
          valid_idx <- !is.na(p_values)
          if (sum(valid_idx) == 0) {
            warning(paste("No valid p-values for method", method, "method_setting", method_setting, "condition", condition, "- setting values to NA"))
            result_df[[measure_col_name]] <- NA
            result_df[[mcse_col_name]] <- NA
          } else {
            test_rejects_h0 <- p_values[valid_idx] < power_threshold
            result_df[[measure_col_name]] <- measure_fun(
              test_rejects_h0 = test_rejects_h0
            )
            result_df[[mcse_col_name]] <- measure_mcse_fun(
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
            result_df[[measure_col_name]] <- NA
            result_df[[mcse_col_name]] <- NA
          } else {
            # For Bayes factors, reject H0 (favor H1) when BF > threshold
            test_rejects_h0 <- bf_values[valid_idx] > power_threshold
            result_df[[measure_col_name]] <- measure_fun(
              test_rejects_h0 = test_rejects_h0
            )
            result_df[[mcse_col_name]] <- measure_mcse_fun(
              test_rejects_h0 = test_rejects_h0
            )
          }
        }

      } else if (measure_name == "mean_ci_width") {
        # CI width needs CI bounds
        ci_lower <- method_condition_results[[ci_lower_col]]
        ci_upper <- method_condition_results[[ci_upper_col]]
        # Remove NAs
        valid_idx <- !is.na(ci_lower) & !is.na(ci_upper)
        if (sum(valid_idx) == 0) {
          warning(paste("No valid confidence intervals for method", method, "method_setting", method_setting, "condition", condition, "- setting values to NA"))
          result_df[[measure_col_name]] <- NA
          result_df[[mcse_col_name]] <- NA
        } else {
          ci_lower <- ci_lower[valid_idx]
          ci_upper <- ci_upper[valid_idx]

          result_df[[measure_col_name]] <- measure_fun(
            ci_upper = ci_upper,
            ci_lower = ci_lower
          )
          result_df[[mcse_col_name]] <- measure_mcse_fun(
            ci_upper = ci_upper,
            ci_lower = ci_lower
          )
        }
      }

      measure_out[[key]] <- result_df
    }
  }

  # Combine new results with existing ones if applicable
  new_results <- do.call(rbind, measure_out)

  if (!is.null(existing_results)) {
    # Combine existing and new results
    all_results <- rbind(existing_results, new_results)
    return(all_results)
  }

  return(new_results)
}

#' Compute Multiple Performance measures for a DGM
#'
#' @description
#' This is a high-level wrapper function that computes multiple performance
#' measures for a Data Generating Mechanism (DGM) and saves the results to CSV files.
#' It provides a clean and extensible interface for computing standard simulation
#' performance measures.
#'
#' @param methods Character vector of method names
#' @param method_settings Character vector of method settings, must be same length as methods
#' @param measures Character vector of measures to compute. If NULL, computes all standard measures.
#' @param verbose Logical indicating whether to print progress messages
#' @param power_test_type Character string specifying the test type for power computation:
#' "p_value" (default) or "bayes_factor"
#' @param power_threshold Numeric threshold for power computation. For p-values,
#' default is 0.05 (reject if p < 0.05). For Bayes factors, default is 10
#' (reject if BF > 10, indicating strong evidence for H1)
#' @param estimate_col Character string specifying the column name containing parameter estimates. Default is "estimate"
#' @param true_effect_col Character string specifying the column name in conditions data frame containing true effect sizes. Default is "mean_effect"
#' @param ci_lower_col Character string specifying the column name containing lower confidence interval bounds. Default is "ci_lower"
#' @param ci_upper_col Character string specifying the column name containing upper confidence interval bounds. Default is "ci_upper"
#' @param p_value_col Character string specifying the column name containing p-values. Default is "p_value"
#' @param bf_col Character string specifying the column name containing Bayes factors. Default is "BF"
#' @param convergence_col Character string specifying the column name containing convergence indicators. Default is "convergence"
#' @param overwrite Logical indicating whether to overwrite existing results. If FALSE (default), will skip computation for method-measure combinations that already exist
#' @param conditions data.frame with specification of the condition for a given condition. Defaults to FALSE, the internally stored conditions file under the DGM name is used.
#' @inheritParams download_dgm_datasets
#'
#' @return Invisible list of computed measures data frames
#'
#' @examples
#' \dontrun{
#' # Compute all standard measures for no_bias DGM
#' results <- compute_measures(
#'   dgm_name = "no_bias",
#'   methods = c("RMA", "PET", "PEESE", "PETPEESE"),
#'   method_settings = c("default", "default", "default", "default")
#' )
#'
#' # Compute only specific measures
#' results <- compute_measures(
#'   dgm_name = "no_bias",
#'   methods = c("RMA", "PET"),
#'   method_settings = c("default", "default"),
#'   measures = c("bias", "mse", "coverage")
#' )
#'
#' # Compute measures with Bayes factor based power (BF > 6 for significance)
#' results <- compute_measures(
#'   dgm_name = "no_bias",
#'   methods = c("RMA", "PET"),
#'   method_settings = c("default", "default"),
#'   power_test_type = "bayes_factor",
#'   power_threshold = 6
#' )
#'
#' # Compute measures with custom p-value threshold
#' results <- compute_measures(
#'   dgm_name = "no_bias",
#'   methods = c("RMA", "PET"),
#'   method_settings = c("default", "default"),
#'   power_test_type = "p_value",
#'   power_threshold = 0.01
#' )
#'
#' # Include convergence measure
#' results <- compute_measures(
#'   dgm_name = "no_bias",
#'   methods = c("RMA", "PET"),
#'   method_settings = c("default", "default"),
#'   measures = c("bias", "mse", "coverage", "power", "convergence")
#' )
#'
#' # Use custom column names
#' results <- compute_measures(
#'   dgm_name = "no_bias",
#'   methods = c("RMA", "PET"),
#'   method_settings = c("default", "default"),
#'   estimate_col = "my_estimate",
#'   true_effect_col = "true_value",
#'   ci_lower_col = "lower_ci",
#'   ci_upper_col = "upper_ci",
#'   p_value_col = "pval",
#'   bf_col = "bayes_factor",
#'   convergence_col = "converged"
#' )
#' }
#'
#' @export
compute_measures <- function(dgm_name, methods, method_settings, measures = NULL, verbose = TRUE,
                           power_test_type = "p_value", power_threshold = NULL,
                           estimate_col = "estimate", true_effect_col = "mean_effect",
                           ci_lower_col = "ci_lower", ci_upper_col = "ci_upper",
                           p_value_col = "p_value", bf_col = "BF", convergence_col = "convergence",
                           overwrite = FALSE, conditions = NULL, path = NULL) {

  # Validate that methods and method_settings have the same length
  if (length(methods) != length(method_settings))
    stop("methods and method_settings must have the same length")

  if (is.null(path))
    path <- PublicationBiasBenchmark.get_option("simulation_directory")

  # Get DGM conditions
  if (is.null(conditions))
    conditions <- dgm_conditions(dgm_name)

  # Define all available measures if not specified
  if (is.null(measures))
    measures <- c("bias", "relative_bias", "mse", "rmse", "empirical_variance",
                  "empirical_se", "coverage", "power", "mean_ci_width", "convergence")

  # Define measure functions
  measure_functions <- list(
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
  output_folder <- file.path(path, dgm_name, "measures")
  if (!dir.exists(output_folder))
    dir.create(output_folder, recursive = TRUE)

  # Compute each measure
  results <- list()

  for (measure in measures) {
    if (!measure %in% names(measure_functions)) {
      warning("Unknown measure: ", measure, ". Skipping.")
      next
    }

    output_file <- file.path(output_folder, paste0(measure, ".csv"))

    # If overwrite is TRUE, remove existing file to start fresh
    if (overwrite && file.exists(output_file)) {
      if (verbose) {
        cat("Overwriting existing", measure, "results at", output_file, "\n")
      }
      file.remove(output_file)
    }

    if (verbose) {
      if (file.exists(output_file) && !overwrite) {
        cat("Computing missing", measure, "results...\n")
      } else {
        cat("Computing", measure, "...\n")
      }
    }

    measure_result <- compute_single_measure(
      dgm_name          = dgm_name,
      measure_name       = measure,
      methods           = methods,
      method_settings   = method_settings,
      conditions        = conditions,
      measure_fun        = measure_functions[[measure]]$fun,
      measure_mcse_fun   = measure_functions[[measure]]$mcse_fun,
      power_test_type   = power_test_type,
      power_threshold   = power_threshold,
      estimate_col      = estimate_col,
      true_effect_col   = true_effect_col,
      ci_lower_col      = ci_lower_col,
      ci_upper_col      = ci_upper_col,
      p_value_col        = p_value_col,
      bf_col            = bf_col,
      convergence_col   = convergence_col,
      overwrite         = overwrite,
      path              = path
    )

    # Save results (measure_result already contains combined existing + new results if applicable)
    utils::write.csv(measure_result, file = output_file, row.names = FALSE)

    if (verbose) {
      cat("Saved", measure, "results to", output_file, "\n")
    }

    results[[measure]] <- measure_result
  }

  return(invisible(results))
}
