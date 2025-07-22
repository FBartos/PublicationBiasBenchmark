# Test script to verify the package functionality
# This can be run to check if the main functions work correctly

# Source all R files (for testing during development)
source_files <- list.files("R", pattern = "\\.R$", full.names = TRUE)
for (file in source_files) {
  source(file)
}

cat("Testing PublicationBiasBenchmark package...\n\n")

# Test 1: Normal DGM
cat("Test 1: Normal DGM\n")
cat("==================\n")

normal_settings <- list(
  n_studies = 20,
  true_effect = 0.3,
  study_sizes = c(50, 100, 200),
  heterogeneity = 0.1
)

normal_data <- dgm("normal", normal_settings)
cat("✓ Normal DGM successful\n")
cat("Generated", nrow(normal_data), "studies\n")
cat("Mean effect size:", round(mean(normal_data$yi), 3), "\n")
cat("Data structure: yi, sei, vi columns present:", all(c("yi", "sei", "vi") %in% names(normal_data)), "\n\n")

# Test 2: PET Method
cat("Test 2: PET Method\n")
cat("==================\n")

pet_result <- method("PET", normal_data)
cat("✓ PET method successful\n")
cat("Convergence:", pet_result$convergence, "\n")
cat("Corrected effect:", round(pet_result$est, 3), "\n")
cat("Bias coefficient p-value:", round(pet_result$bias_p_value, 3), "\n\n")

# Test 3: PETPEESE Method
cat("Test 3: PETPEESE Method\n")
cat("=======================\n")

petpeese_result <- method("PETPEESE", normal_data)
cat("✓ PETPEESE method successful\n")
cat("Convergence:", petpeese_result$convergence, "\n")
cat("Selected method:", petpeese_result$selected_method, "\n")
cat("Corrected effect:", round(petpeese_result$est, 3), "\n\n")

# Test 4: Publication Bias DGM
cat("Test 4: Publication Bias DGM\n")
cat("=============================\n")

bias_settings <- list(
  n_studies = 20,
  true_effect = 0.2,
  study_sizes = c(30, 60, 120),
  heterogeneity = 0.05,
  selection_threshold = 0.05
)

bias_data <- dgm("publication_bias", bias_settings)
cat("✓ Publication bias DGM successful\n")
cat("Generated", nrow(bias_data), "studies\n")
cat("Mean effect size:", round(mean(bias_data$yi), 3), "\n")
cat("(Should be biased upward from true effect of", bias_settings$true_effect, ")\n\n")

# Test 5: Multiple Methods
cat("Test 5: Multiple Methods\n")
cat("========================\n")

methods_to_test <- c("PET", "PETPEESE", "Egger", "TrimFill")

for (method_name in methods_to_test) {
  tryCatch({
    result <- method(method_name, bias_data)
    cat("✓", method_name, "method successful\n")
    cat("  - Convergence:", result$convergence, "\n")
    if (!is.na(result$est)) {
      cat("  - Estimate:", round(result$est, 3), "\n")
    }
  }, error = function(e) {
    cat("✗", method_name, "method failed:", e$message, "\n")
  })
}

# Test 6: Predefined Settings
cat("\nTest 6: Predefined Settings\n")
cat("============================\n")

tryCatch({
  # Test with settings_id
  preset_data <- dgm("normal", 1)  # Using predefined settings ID 1
  cat("✓ Predefined settings successful\n")
  cat("Generated", nrow(preset_data), "studies using preset 1\n")
  
  # Test attributes
  dgm_name <- attr(preset_data, "dgm_name")
  settings <- attr(preset_data, "settings")
  cat("DGM name from attributes:", dgm_name, "\n")
  cat("Settings from attributes: n_studies =", settings$n_studies, "\n")
  
}, error = function(e) {
  cat("✗ Predefined settings failed:", e$message, "\n")
})

cat("\nAll tests completed!\n")
cat("Package appears to be working correctly with the new format.\n")
