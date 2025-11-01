#' Compute Performance Measures
#'
#' @description
#' This function provides a modular and extensible way to compute performance
#' measures (PM) for Data-Generating Mechanisms (DGMs). It handles different types
#' of measures and automatically determines the required arguments for each measure
#' function.
#'
#' @param dgm_name Character string specifying the DGM name
#' @param measure_name Name of the measure to compute (e.g., "bias", "mse")
#' @param method Character vector of method names
#' @param method_setting Character vector of method settings, must be same length as method
#' @param conditions Data frame of conditions from dgm_conditions()
#' @param measure_fun Function to compute the measure
#' @param measure_mcse_fun Function to compute the MCSE for the measure
#' @param power_test_type Character vector specifying the test type for power computation:
#' "p_value" (default) or "bayes_factor" for each method. If a single value is provided, it is
#' repeated for all methods.
#' @param power_threshold_p_value Numeric threshold for power computation with p-values.
#' Default is 0.05 (reject H0 if p < 0.05).
#' @param power_threshold_bayes_factor Numeric threshold for power computation with Bayes factors.
#' Default is 10 (reject H0 if BF > 10)
#' @param estimate_col Character string specifying the column name containing parameter estimates. Default is "estimate"
#' @param true_effect_col Character string specifying the column name in conditions data frame containing true effect sizes. Default is "mean_effect"
#' @param ci_lower_col Character string specifying the column name containing lower confidence interval bounds. Default is "ci_lower"
#' @param ci_upper_col Character string specifying the column name containing upper confidence interval bounds. Default is "ci_upper"
#' @param p_value_col Character string specifying the column name containing p-values. Default is "p_value"
#' @param bf_col Character string specifying the column name containing Bayes factors. Default is "BF"
#' @param convergence_col Character string specifying the column name containing convergence indicators. Default is "convergence"
#' @param method_replacements Named list of replacement method specifications. Each element should be named
#' with the "method-method_setting" combination (e.g., "RMA-default") and contain a named list with:
#' \itemize{
#'   \item{\code{method}: Character vector of replacement method names}
#'   \item{\code{method_setting}: Character vector of replacement method settings (same length as methods)}
#'   \item{\code{power_test_type}: Optional character vector of power test types for each replacement method (same length as methods). If not specified, uses the main power_test_type parameter}
#' }
#' If multiple elements are specified within the vectors, these replacements are applied consecutively
#' in case the previous replacements also failed to converge.
#' Defaults to \code{NULL}, i.e., omitting repetitions without converged results on method-by-method basis.
#' @param n_repetitions Number of repetitions in each condition. Necessary method replacement. Defaults to \code{1000}.
#' @param overwrite Logical indicating whether to overwrite existing results. If FALSE (default), will skip computation for method-measure combinations that already exist
#' @param ... Additional arguments passed to measure functions
#'
#' @return TRUE upon successfully computation of the results file
#'
#' @export
compute_single_measure <- function(dgm_name, measure_name, method, method_setting, conditions,
                                   measure_fun, measure_mcse_fun,
                                   power_test_type = "p_value",
                                   estimate_col = "estimate", true_effect_col = "mean_effect",
                                   ci_lower_col = "ci_lower", ci_upper_col = "ci_upper",
                                   p_value_col = "p_value", bf_col = "BF", convergence_col = "convergence",
                                   power_threshold_p_value = 0.05, power_threshold_bayes_factor = 10,
                                   method_replacements = NULL, n_repetitions = 1000,
                                   overwrite = FALSE, ...) {

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
        stop(paste0("Each replacement must contain 'method' and 'method_setting' elements for", method_name))
      if (length(replacements$method) != length(replacements$method_setting))
        stop(paste0("method and method_setting must have the same length for ", method_name))
    }
  }

  # Validate power test type
  if (!all(power_test_type %in% c("p_value", "bayes_factor")))
    stop("power_test_type must be either 'p_value' or 'bayes_factor'")
  if (length(power_test_type) != 1 && length(power_test_type) != length(method))
    stop("power_test_type must be either a single value or have the same length as method")
  if (length(power_test_type) == 1) {
    power_test_type <- rep(power_test_type, length(method))
  }

  # Check if results already exist
  existing_results   <- NULL
  methods_to_compute <- seq_along(method)

  # Create a file name
  file_name <- paste0(measure_name, if (is.null(method_replacements) || length(method_replacements) == 0) ".csv" else "-replacement.csv")

  path <- .get_path()
  
  output_folder <- file.path(path, dgm_name, "measures")
  output_file   <- file.path(output_folder, file_name)

  if (!overwrite && file.exists(output_file)) {
    # Load the existing file and only attach results for the missing selected methods if overwrite is FALSE
    existing_results <- utils::read.csv(output_file)

    # Check which method-method_setting combinations already have results
    existing_combinations <- unique(paste0(existing_results$method, "-", existing_results$method_setting))
    current_combinations  <- paste0(method, "-", method_setting)

    # Find indices of combinations that need to be computed
    methods_to_compute <- which(!current_combinations %in% existing_combinations)

    if (length(methods_to_compute) == 0) {
      # All combinations already
      return(invisible(TRUE))
    }
  } else if (overwrite && file.exists(output_file)) {
    # Load the existing file and overwrite the results for the selected methods if overwrite is TRUE
    existing_results <- utils::read.csv(output_file)

    # Remove results for methods to be computed
    current_combinations  <- paste0(method, "-", method_setting)
    existing_results      <- existing_results[!paste0(existing_results$method, "-", existing_results$method_setting) %in% current_combinations,]

  } else {
    existing_results <- NULL
  }

  measure_out <- list()

  # Create dynamic column names based on measure
  measure_col_name <- measure_name
  mcse_col_name    <- paste0(measure_name, "_mcse")

  # Preload replacement methods
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

        # Precompute H0 rejection
        if (measure_name %in% c("power", "positive_likelihood_ratio", "negative_likelihood_ratio")) {
          if ("power_test_type" %in% names(replacement_spec)) {
            if (length(replacement_spec$power_test_type) == 1) {
              replacement_power_test_type <- replacement_spec$power_test_type[1]
            } else {
              replacement_power_test_type <- replacement_spec$power_test_type[i]
            }
          } else {
            replacement_power_test_type <- power_test_type
          }

          if (replacement_power_test_type == "p_value") {
            test_statistic <- method_replacements_results[[method_name]][[replacement_key]][[p_value_col]]
            reject_h0      <- test_statistic < power_threshold_p_value
          } else if (replacement_power_test_type == "bayes_factor") {
            test_statistic <- method_replacements_results[[method_name]][[replacement_key]][[bf_col]]
            reject_h0      <- test_statistic > power_threshold_bayes_factor
          } else
            stop(paste0("power_test_type must be either 'p_value' or 'bayes_factor' for replacement method ", replacement_key))

          method_replacements_results[[method_name]][[replacement_key]][["h0_rejected"]] <- reject_h0
        }
      }
    }
  }

  for (i in methods_to_compute) {

    this_method         <- method[i]
    this_method_setting <- method_setting[i]

    # Retrieve the precomputed results
    method_results <- retrieve_dgm_results(
      dgm_name       = dgm_name,
      method         = this_method,
      method_setting = this_method_setting
    )

    # Check that all pre-specified columns exist
    columns_required <- c(
      convergence_col, estimate_col, ci_lower_col, ci_upper_col,
      switch(power_test_type[i],
             "p_value"      = p_value_col,
             "bayes_factor" = bf_col)
    )
    if (!all(columns_required %in% names(method_results)))
      stop(sprintf("The following columns are undefined: %1$s,", columns_required[!columns_required %in% names(method_results)]))

    # Precompute H0 rejection
    # this needs to be done before merging potential method replacement because they
    # might use different power_test_type and power_threshold values
    if (measure_name %in% c("power", "positive_likelihood_ratio", "negative_likelihood_ratio")) {
      if (power_test_type[i] == "p_value") {
        test_statistic <- method_results[[p_value_col]]
        reject_h0      <- test_statistic < power_threshold_p_value
      } else if (power_test_type[i] == "bayes_factor") {
        test_statistic <- method_results[[bf_col]]
        reject_h0      <- test_statistic > power_threshold_bayes_factor
      }

      method_results[["h0_rejected"]] <- reject_h0
    }

    for (condition in conditions$condition_id) {

      # Select the condition results
      method_condition_results <- method_results[method_results$condition_id == condition,,drop = FALSE]

      # Select the matching null condition when likelihood ratio is requested
      if (measure_name %in% c("positive_likelihood_ratio", "negative_likelihood_ratio")) {

        this_condition <- conditions[conditions$condition_id == condition,,drop=FALSE]

        # Do not compute for true null hypothesis
        if (this_condition[[true_effect_col]] == 0)
          next

        # Find the matching null hypothesis
        this_null_condition <- this_condition
        this_null_condition[[true_effect_col]] <- 0
        null_conditions        <- conditions[conditions[[true_effect_col]] == 0,,drop=FALSE]
        this_null_condition_id <- NA
        colnames_to_check      <- colnames(null_conditions)[colnames(null_conditions) != "condition_id"]
        for (j in 1:nrow(null_conditions)) {
          if (isTRUE(all.equal(unlist(unname(null_conditions[j,colnames_to_check])), unlist(unname(this_null_condition[,colnames_to_check]))))) {
            this_null_condition_id <- null_conditions[j,"condition_id"]
          }
        }

        if (is.na(this_null_condition_id))
          stop("The matching null hypothesis condition was not found")

        # select the null condition results
        method_condition_results_null <- method_results[method_results$condition_id == this_null_condition_id,,drop = FALSE]

        # Replace results in case of missingness
        method_name <- paste0(this_method, "-", this_method_setting)
        if (!all(method_condition_results_null[[convergence_col]]) && !is.null(method_replacements)) {
          method_condition_results_null <- method_condition_results_replacement(
            method_condition_results    = method_condition_results_null,
            method_name                 = method_name,
            method_replacements         = method_replacements,
            n_repetitions               = n_repetitions,
            condition                   = this_null_condition_id,
            convergence_col             = convergence_col,
            estimate_col                = estimate_col,
            ci_lower_col                = ci_lower_col,
            ci_upper_col                = ci_upper_col,
            method_replacements_results = method_replacements_results,
            measure_name                = measure_name
          )
        }

      }

      # Replace results in case of missingness
      replaced    <- FALSE
      method_name <- paste0(this_method, "-", this_method_setting)
      if (!all(method_condition_results[[convergence_col]]) && !is.null(method_replacements)) {
        method_condition_results <- method_condition_results_replacement(
          method_condition_results    = method_condition_results,
          method_name                 = method_name,
          method_replacements         = method_replacements,
          n_repetitions               = n_repetitions,
          condition                   = condition,
          convergence_col             = convergence_col,
          estimate_col                = estimate_col,
          ci_lower_col                = ci_lower_col,
          ci_upper_col                = ci_upper_col,
          method_replacements_results = method_replacements_results,
          measure_name                 = measure_name
        )
        replaced <- attr(method_condition_results, "replaced")
      }

      # Create result holder
      key       <- paste0(this_method, "-", this_method_setting, "-", condition)
      result_df <- data.frame(
        method         = this_method,
        method_setting = this_method_setting,
        condition_id   = condition
      )

      if (!isFALSE(replaced))
        result_df[["replaced"]] <- replaced

      # Filter for converged results if we're not computing convergence measure
      if (measure_name != "convergence") {
        method_condition_results <- method_condition_results[method_condition_results[[convergence_col]],,drop = FALSE]

        # If no converged results remain, create NA result
        if (nrow(method_condition_results) == 0) {
          warning(paste("No converged results for method", this_method, "method_setting", this_method_setting, "condition", condition, "- setting values to NA"))
          result_df[[measure_col_name]] <- NA
          result_df[[mcse_col_name]]    <- NA
          result_df[["n_valid"]]        <- 0
          measure_out[[key]] <- result_df
          next
        }
      }

      # Get the true effect for this condition
      true_effect <- conditions[conditions$condition_id == condition, true_effect_col]

      # Compute measure and MCSE based on measure type
      if (measure_name == "convergence") {

        convergence_indicator <- method_condition_results[[convergence_col]]
        result_df[[measure_col_name]] <- measure_fun(test_rejects_h0 = convergence_indicator)
        result_df[[mcse_col_name]]    <- measure_mcse_fun(test_rejects_h0 = convergence_indicator)
        result_df[["n_valid"]]        <- length(convergence_indicator)

      } else if (measure_name %in% c("bias", "relative_bias", "mse", "rmse", "empirical_variance", "empirical_se")) {

        estimates <- method_condition_results[[estimate_col]]
        valid_idx <- !is.na(estimates)

        if (sum(valid_idx) == 0) {

          warning(paste("No valid estimates for method", this_method, "method_setting", this_method_setting, "condition", condition, "- setting values to NA"), immediate. = TRUE)
          result_df[[measure_col_name]] <- NA
          result_df[[mcse_col_name]]    <- NA

        } else if (measure_name == "bias") {

          estimates <- estimates[valid_idx]
          result_df[[measure_col_name]] <- measure_fun(theta_hat = estimates, theta = true_effect)
          result_df[[mcse_col_name]]    <- measure_mcse_fun(theta_hat = estimates)

        } else if (measure_name %in% c("relative_bias", "mse", "rmse")) {

          estimates <- estimates[valid_idx]
          result_df[[measure_col_name]] <- measure_fun(theta_hat = estimates, theta = true_effect)
          result_df[[mcse_col_name]]    <- measure_mcse_fun(theta_hat = estimates, theta = true_effect)

        } else if (measure_name %in% c("empirical_variance", "empirical_se")) {

          estimates <- estimates[valid_idx]
          result_df[[measure_col_name]] <- measure_fun(theta_hat = estimates)
          result_df[[mcse_col_name]]    <- measure_mcse_fun(theta_hat = estimates)

        }

        result_df[["n_valid"]] <- sum(valid_idx)

      } else if (measure_name %in% c("coverage", "mean_ci_width", "interval_score")) {

        ci_lower  <- method_condition_results[[ci_lower_col]]
        ci_upper  <- method_condition_results[[ci_upper_col]]
        valid_idx <- !is.na(ci_lower) & !is.na(ci_upper)

        if (sum(valid_idx) == 0) {

          warning(paste("No valid confidence intervals for method", this_method, "method_setting", this_method_setting, "condition", condition, "- setting values to NA"), immediate. = TRUE)
          result_df[[measure_col_name]] <- NA
          result_df[[mcse_col_name]]    <- NA

        } else if (measure_name %in% c("coverage", "interval_score")) {

          ci_lower <- ci_lower[valid_idx]
          ci_upper <- ci_upper[valid_idx]
          result_df[[measure_col_name]] <- measure_fun(ci_lower = ci_lower, ci_upper = ci_upper, theta = true_effect)
          result_df[[mcse_col_name]]    <- measure_mcse_fun(ci_lower = ci_lower, ci_upper = ci_upper, theta = true_effect)

        } else if (measure_name == "mean_ci_width") {

          ci_lower <- ci_lower[valid_idx]
          ci_upper <- ci_upper[valid_idx]
          result_df[[measure_col_name]] <- measure_fun(ci_lower = ci_lower, ci_upper = ci_upper)
          result_df[[mcse_col_name]]    <- measure_mcse_fun(ci_lower = ci_lower, ci_upper = ci_upper)

        }

        interval_score

        result_df[["n_valid"]] <- sum(valid_idx)

      } else if (measure_name == "power") {

        test_rejects_h0 <- method_condition_results[["h0_rejected"]]
        valid_idx       <- !is.na(test_rejects_h0)

        if (sum(valid_idx) == 0) {

          warning(paste("No valid h0 rejection indicators for method", this_method, "method_setting", this_method_setting, "condition", condition, "- setting values to NA"), immediate. = TRUE)
          result_df[[measure_col_name]] <- NA
          result_df[[mcse_col_name]]    <- NA

        } else {

          test_rejects_h0 <- test_rejects_h0[valid_idx]
          result_df[[measure_col_name]] <- measure_fun(test_rejects_h0 = test_rejects_h0)
          result_df[[mcse_col_name]]    <- measure_mcse_fun(test_rejects_h0 = test_rejects_h0)

        }

        result_df[["n_valid"]] <- sum(valid_idx)

      } else if (measure_name %in% c("positive_likelihood_ratio", "negative_likelihood_ratio")) {

        test_rejects_h0_null <- method_condition_results_null[["h0_rejected"]]
        test_rejects_h0_alt  <- method_condition_results[["h0_rejected"]]
        valid_idx_null <- !is.na(test_rejects_h0_null)
        valid_idx_alt  <- !is.na(test_rejects_h0_alt)

        if (sum(valid_idx_null) == 0 || sum(valid_idx_alt) == 0) {

          warning(paste("No valid h0 rejection indicators for method", this_method, "method_setting", this_method_setting, "condition", condition, "- setting values to NA"), immediate. = TRUE)
          result_df[[measure_col_name]] <- NA
          result_df[[mcse_col_name]]    <- NA

        } else {

          test_rejects_h0_null <- test_rejects_h0_null[valid_idx_null]
          test_rejects_h0_alt  <- test_rejects_h0_alt[valid_idx_alt]
          result_df[[measure_col_name]] <- measure_fun(tp = sum(test_rejects_h0_alt), fp = sum(test_rejects_h0_null), fn = sum(!test_rejects_h0_alt), tn = sum(!test_rejects_h0_null))
          result_df[[mcse_col_name]]    <- measure_mcse_fun(tp = sum(test_rejects_h0_alt), fp = sum(test_rejects_h0_null), fn = sum(!test_rejects_h0_alt), tn = sum(!test_rejects_h0_null))

        }

        result_df[["n_valid"]] <- sum(c(valid_idx_null, valid_idx_alt))

      }

      measure_out[[key]] <- result_df
    }
  }

  # Merge into data.frame
  new_results <- safe_rbind(measure_out)

  # Combine existing and new results
  if (!is.null(existing_results)) {
    new_results <- safe_rbind(list(new_results, existing_results))
  }

  # Save results
  utils::write.csv(new_results, file = output_file, row.names = FALSE)

  return(invisible(TRUE))
}

method_condition_results_replacement <- function(method_condition_results, method_name,
                                                 method_replacements, n_repetitions,
                                                 condition, convergence_col, estimate_col, ci_lower_col, ci_upper_col,
                                                 method_replacements_results, measure_name) {

  if (is.null(method_replacements[[method_name]]))
    stop(paste0("No method replacements specified for method-method_setting combination ", method_name))

  # Subset converged results
  method_condition_results <- method_condition_results[method_condition_results[[convergence_col]],,drop = FALSE]

  # Remove results with missing critical columns (i.e., NAs despite convergence)
  if (measure_name %in% c("convergence", "bias", "relative_bias", "mse", "rmse", "empirical_variance", "empirical_se")) {
    method_condition_results <- method_condition_results[!is.na(method_condition_results[[estimate_col]]),,drop = FALSE]
  } else if (measure_name %in% c("coverage", "mean_ci_width", "interval_score")) {
    method_condition_results <- method_condition_results[!is.na(method_condition_results[[ci_lower_col]]),,drop = FALSE]
    method_condition_results <- method_condition_results[!is.na(method_condition_results[[ci_upper_col]]),,drop = FALSE]
  } else if (measure_name %in% c("power", "positive_likelihood_ratio", "negative_likelihood_ratio")) {
    method_condition_results <- method_condition_results[!is.na(method_condition_results[["h0_rejected"]]),,drop = FALSE]
  }

  # Find missing repetitions
  repetitions_all     <- 1:n_repetitions
  repetitions_missing <- repetitions_all[!repetitions_all %in% method_condition_results[["repetition_id"]][method_condition_results[[convergence_col]]]]

  # Fill in the missing repetitions
  replaced         <- NULL
  replacement_spec <- method_replacements[[method_name]]

  for (j in seq_along(replacement_spec$method)){

    # Break if all missing repetitions are replaced
    if (length(repetitions_missing) == 0)
      break

    # Get replacement method info
    replacement_method  <- replacement_spec$method[j]
    replacement_setting <- replacement_spec$method_setting[j]
    replacement_key     <- paste0(replacement_method, "-", replacement_setting)

    # Find missing repetitions
    temp_replacement <- method_replacements_results[[method_name]][[replacement_key]]
    temp_replacement <- temp_replacement[temp_replacement$condition_id == condition & temp_replacement[[convergence_col]],,drop=FALSE]
    temp_replacement <- temp_replacement[temp_replacement[["repetition_id"]] %in% repetitions_missing,,drop=FALSE]

    # Remove results with missing critical columns (i.e., NAs despite convergence)
    if (measure_name %in% c("convergence", "bias", "relative_bias", "mse", "rmse", "empirical_variance", "empirical_se")) {
      temp_replacement <- temp_replacement[!is.na(temp_replacement[[estimate_col]]),,drop = FALSE]
    } else if (measure_name %in% c("coverage", "mean_ci_width", "interval_score")) {
      temp_replacement <- temp_replacement[!is.na(temp_replacement[[ci_lower_col]]),,drop = FALSE]
      temp_replacement <- temp_replacement[!is.na(temp_replacement[[ci_upper_col]]),,drop = FALSE]
    } else if (measure_name %in% c("power", "positive_likelihood_ratio", "negative_likelihood_ratio")) {
      temp_replacement <- temp_replacement[!is.na(temp_replacement[["h0_rejected"]]),,drop = FALSE]
    }

    # Store information about replacement
    replaced <- paste0(replaced, paste0(paste0(replacement_key,"=",nrow(temp_replacement))), sep = ";")

    # Merge and update
    method_condition_results <- safe_rbind(list(method_condition_results, temp_replacement))
    repetitions_missing      <- repetitions_all[!repetitions_all %in% method_condition_results[["repetition_id"]][method_condition_results[[convergence_col]]]]
  }

  # store the replacement information
  attr(method_condition_results, "replaced") <- replaced

  return(method_condition_results)
}

#' Compute Multiple Performance measures for a DGM
#'
#' @description
#' This is a high-level wrapper function that computes multiple performance
#' measures for a Data-Generating Mechanism (DGM) and saves the results to CSV files.
#' It provides a clean and extensible interface for computing standard simulation
#' performance measures.
#'
#' @param measures Character vector of measures to compute. If NULL, computes all standard measures.
#' @param verbose Print detailed progress of the calculation.
#' @inheritParams compute_single_measure
#' @inheritParams download_dgm_datasets
#'
#' @return TRUE upon successfully computation of the results file
#'
#' @examples
#' \donttest{
#' # Download DGM results
#' dgm_name <- "no_bias"
#' download_dgm_results(dgm_name)
#'
#' # Basic usage
#' compute_measures(
#'   dgm_name        = dgm_name,
#'   method          = c("mean", "RMA", "PET"),
#'   method_setting  = c("default", "default", "default"),
#'   measures        = c("bias", "mse", "coverage")
#' )
#'
#' # With method replacements for non-converged results
#' method_replacements <- list(
#'   "RMA-default" = list(method = "FMA", method_setting = "default"),
#'   "PET-default" = list(method = c("WLS", "FMA"),
#'                        method_setting = c("default", "default"))
#' )
#'
#' compute_measures(
#'   dgm_name            = dgm_name,
#'   method              = c("RMA", "PET"),
#'   method_setting      = c("default", "default"),
#'   method_replacements = method_replacements,
#'   measures            = c("bias", "mse")
#' )
#' }
#'
#' @export
compute_measures <- function(dgm_name, method, method_setting, measures = NULL, verbose = TRUE,
                             power_test_type = "p_value",
                             power_threshold_p_value = 0.05, power_threshold_bayes_factor = 10,
                             estimate_col = "estimate", true_effect_col = "mean_effect",
                             ci_lower_col = "ci_lower", ci_upper_col = "ci_upper",
                             p_value_col = "p_value", bf_col = "BF", convergence_col = "convergence",
                             method_replacements = NULL, n_repetitions = 1000,
                             overwrite = FALSE, conditions = NULL) {

  # Define all available measures if not specified
  if (is.null(measures))
    measures <- c("bias", "relative_bias", "mse", "rmse", "empirical_variance",
                  "empirical_se", "coverage", "power", "mean_ci_width", "interval_score", "convergence",
                  "positive_likelihood_ratio", "negative_likelihood_ratio")

  # Define measure functions
  measure_functions <- list(
    bias                        = list(fun = bias,                      mcse_fun = bias_mcse),
    relative_bias               = list(fun = relative_bias,             mcse_fun = relative_bias_mcse),
    mse                         = list(fun = mse,                       mcse_fun = mse_mcse),
    rmse                        = list(fun = rmse,                      mcse_fun = rmse_mcse),
    empirical_variance          = list(fun = empirical_variance,        mcse_fun = empirical_variance_mcse),
    empirical_se                = list(fun = empirical_se,              mcse_fun = empirical_se_mcse),
    coverage                    = list(fun = coverage,                  mcse_fun = coverage_mcse),
    mean_ci_width               = list(fun = mean_ci_width,             mcse_fun = mean_ci_width_mcse),
    interval_score              = list(fun = interval_score,            mcse_fun = interval_score_mcse),
    power                       = list(fun = power,                     mcse_fun = power_mcse),
    convergence                 = list(fun = power,                     mcse_fun = power_mcse),
    positive_likelihood_ratio   = list(fun = positive_likelihood_ratio, mcse_fun = positive_likelihood_ratio_mcse),
    negative_likelihood_ratio   = list(fun = negative_likelihood_ratio, mcse_fun = negative_likelihood_ratio_mcse)
  )

  # Compute each measure
  for (measure in measures) {

    if (!measure %in% names(measure_functions))
      stop(paste0("Unknown measure: ", measure, ". Skipping."))

    if (verbose)
      message("Computing ", measure, "...")

    compute_single_measure(
      dgm_name                  = dgm_name,
      measure_name              = measure,
      method                    = method,
      method_setting            = method_setting,
      conditions                = conditions,
      measure_fun               = measure_functions[[measure]]$fun,
      measure_mcse_fun          = measure_functions[[measure]]$mcse_fun,
      power_test_type           = power_test_type,
      power_threshold_p_value   = power_threshold_p_value,
      power_threshold_bayes_factor = power_threshold_bayes_factor,
      estimate_col              = estimate_col,
      true_effect_col           = true_effect_col,
      ci_lower_col              = ci_lower_col,
      ci_upper_col              = ci_upper_col,
      p_value_col               = p_value_col,
      bf_col                    = bf_col,
      convergence_col           = convergence_col,
      method_replacements       = method_replacements,
      n_repetitions             = n_repetitions,
      overwrite                 = overwrite
    )

    if (verbose)
      message("Saved ", measure)
  }

  return(invisible(TRUE))
}
