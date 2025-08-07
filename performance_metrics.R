### no_bias DGM performance metrics computation script ----
# This is the scripts to compute the performance metrics for the "no_bias" DGM
# Author: Frantisek Bartos
# Date: 07/08/2025
library(PublicationBiasBenchmark)

# dgm and folder settigns
dgm_name       <- "no_bias"
results_folder <- "simulations"
output_folder  <- "results"

# collect the pre-simulated results
download_dgm_results(dgm_name, path = results_folder)

# compute all of the metrics for the following methods
methods <- c("RMA", "PET", "PEESE", "PETPEESE")

# get a list of the dgm conditions (also available in the repository metadata/dgm-conditions.csv file
conditions <- dgm_conditions("no_bias")

bias_out <- list()
for(method in methods){
  for(condition in conditions$condition_id){

    # retrieve the precomputed results
    method_condition_results <- retrieve_dgm_results(dgm_name, method = method, condition_id = condition, path = results_folder)
    bias_out[[paste0(method, "-", condition)]] <- data.frame(
      method    = method,
      version   = unique(method_condition_results$version_id),
      condition = condition,
      bias = bias(
        theta_hat = method_condition_results[["estimate"]],
        theta     = conditions[conditions$condition_id == condition, "mean_effect"]
      ),
      bias_mcse = bias_mcse(
        theta_hat = method_condition_results[["estimate"]]
      )
    )
  }
}
bias_out <- do.call(rbind, bias_out)
write.csv(bias_out, file = file.path(output_folder, "bias.csv"), row.names = FALSE)

# Compute relative bias
relative_bias_out <- list()
for(method in methods){
  for(condition in conditions$condition_id){

    # retrieve the precomputed results
    method_condition_results <- retrieve_dgm_results(dgm_name, method = method, condition_id = condition, path = results_folder)
    relative_bias_out[[paste0(method, "-", condition)]] <- data.frame(
      method    = method,
      version   = unique(method_condition_results$version_id),
      condition = condition,
      relative_bias = relative_bias(
        theta_hat = method_condition_results[["estimate"]],
        theta     = conditions[conditions$condition_id == condition, "mean_effect"]
      ),
      relative_bias_mcse = relative_bias_mcse(
        theta_hat = method_condition_results[["estimate"]],
        theta     = conditions[conditions$condition_id == condition, "mean_effect"]
      )
    )
  }
}
relative_bias_out <- do.call(rbind, relative_bias_out)
write.csv(relative_bias_out, file = file.path(output_folder, "relative_bias.csv"), row.names = FALSE)

# Compute MSE
mse_out <- list()
for(method in methods){
  for(condition in conditions$condition_id){

    # retrieve the precomputed results
    method_condition_results <- retrieve_dgm_results(dgm_name, method = method, condition_id = condition, path = results_folder)
    mse_out[[paste0(method, "-", condition)]] <- data.frame(
      method    = method,
      version   = unique(method_condition_results$version_id),
      condition = condition,
      mse = mse(
        theta_hat = method_condition_results[["estimate"]],
        theta     = conditions[conditions$condition_id == condition, "mean_effect"]
      ),
      mse_mcse = mse_mcse(
        theta_hat = method_condition_results[["estimate"]],
        theta     = conditions[conditions$condition_id == condition, "mean_effect"]
      )
    )
  }
}
mse_out <- do.call(rbind, mse_out)
write.csv(mse_out, file = file.path(output_folder, "mse.csv"), row.names = FALSE)

# Compute RMSE
rmse_out <- list()
for(method in methods){
  for(condition in conditions$condition_id){

    # retrieve the precomputed results
    method_condition_results <- retrieve_dgm_results(dgm_name, method = method, condition_id = condition, path = results_folder)
    rmse_out[[paste0(method, "-", condition)]] <- data.frame(
      method    = method,
      version   = unique(method_condition_results$version_id),
      condition = condition,
      rmse = rmse(
        theta_hat = method_condition_results[["estimate"]],
        theta     = conditions[conditions$condition_id == condition, "mean_effect"]
      ),
      rmse_mcse = rmse_mcse(
        theta_hat = method_condition_results[["estimate"]],
        theta     = conditions[conditions$condition_id == condition, "mean_effect"]
      )
    )
  }
}
rmse_out <- do.call(rbind, rmse_out)
write.csv(rmse_out, file = file.path(output_folder, "rmse.csv"), row.names = FALSE)

# Compute empirical variance
empirical_variance_out <- list()
for(method in methods){
  for(condition in conditions$condition_id){

    # retrieve the precomputed results
    method_condition_results <- retrieve_dgm_results(dgm_name, method = method, condition_id = condition, path = results_folder)
    empirical_variance_out[[paste0(method, "-", condition)]] <- data.frame(
      method    = method,
      version   = unique(method_condition_results$version_id),
      condition = condition,
      empirical_variance = empirical_variance(
        theta_hat = method_condition_results[["estimate"]]
      ),
      empirical_variance_mcse = empirical_variance_mcse(
        theta_hat = method_condition_results[["estimate"]]
      )
    )
  }
}
empirical_variance_out <- do.call(rbind, empirical_variance_out)
write.csv(empirical_variance_out, file = file.path(output_folder, "empirical_variance.csv"), row.names = FALSE)

# Compute empirical standard error
empirical_se_out <- list()
for(method in methods){
  for(condition in conditions$condition_id){

    # retrieve the precomputed results
    method_condition_results <- retrieve_dgm_results(dgm_name, method = method, condition_id = condition, path = results_folder)
    empirical_se_out[[paste0(method, "-", condition)]] <- data.frame(
      method    = method,
      version   = unique(method_condition_results$version_id),
      condition = condition,
      empirical_se = empirical_se(
        theta_hat = method_condition_results[["estimate"]]
      ),
      empirical_se_mcse = empirical_se_mcse(
        theta_hat = method_condition_results[["estimate"]]
      )
    )
  }
}
empirical_se_out <- do.call(rbind, empirical_se_out)
write.csv(empirical_se_out, file = file.path(output_folder, "empirical_se.csv"), row.names = FALSE)

# Compute coverage
coverage_out <- list()
for(method in methods){
  for(condition in conditions$condition_id){

    # retrieve the precomputed results
    method_condition_results <- retrieve_dgm_results(dgm_name, method = method, condition_id = condition, path = results_folder)
    coverage_out[[paste0(method, "-", condition)]] <- data.frame(
      method    = method,
      version   = unique(method_condition_results$version_id),
      condition = condition,
      coverage = coverage(
        ci_lower = method_condition_results[["ci_lower"]],
        ci_upper = method_condition_results[["ci_upper"]],
        theta    = conditions[conditions$condition_id == condition, "mean_effect"]
      ),
      coverage_mcse = coverage_mcse(
        ci_lower = method_condition_results[["ci_lower"]],
        ci_upper = method_condition_results[["ci_upper"]],
        theta    = conditions[conditions$condition_id == condition, "mean_effect"]
      )
    )
  }
}
coverage_out <- do.call(rbind, coverage_out)
write.csv(coverage_out, file = file.path(output_folder, "coverage.csv"), row.names = FALSE)

# Compute power
power_out <- list()
for(method in methods){
  for(condition in conditions$condition_id){

    # retrieve the precomputed results
    method_condition_results <- retrieve_dgm_results(dgm_name, method = method, condition_id = condition, path = results_folder)
    theta_hat <- method_condition_results[["estimate"]]
    ci_lower  <- method_condition_results[["ci_lower"]]
    ci_upper  <- method_condition_results[["ci_upper"]]

    power_out[[paste0(method, "-", condition)]] <- data.frame(
      method    = method,
      version   = unique(method_condition_results$version_id),
      condition = condition,
      power = power(
        test_rejects_h0 = method_condition_results[["p_value"]] < 0.05
      ),
      power_mcse = power_mcse(
        test_rejects_h0 = method_condition_results[["p_value"]] < 0.05
      )
    )
  }
}
power_out <- do.call(rbind, power_out)
write.csv(power_out, file = file.path(output_folder, "power.csv"), row.names = FALSE)

# Compute mean CI width
mean_ci_width_out <- list()
for(method in methods){
  for(condition in conditions$condition_id){

    # retrieve the precomputed results
    method_condition_results <- retrieve_dgm_results(dgm_name, method = method, condition_id = condition, path = results_folder)
    mean_ci_width_out[[paste0(method, "-", condition)]] <- data.frame(
      method    = method,
      version   = unique(method_condition_results$version_id),
      condition = condition,
      mean_ci_width = mean_ci_width(
        ci_upper = method_condition_results[["ci_upper"]],
        ci_lower = method_condition_results[["ci_lower"]]
      ),
      mean_ci_width_mcse = mean_ci_width_mcse(
        ci_upper = method_condition_results[["ci_upper"]],
        ci_lower = method_condition_results[["ci_lower"]]
      )
    )
  }
}
mean_ci_width_out <- do.call(rbind, mean_ci_width_out)
write.csv(mean_ci_width_out, file = file.path(output_folder, "mean_ci_width.csv"), row.names = FALSE)
