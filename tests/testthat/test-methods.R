context("Basic tests of methods")


# Methods to skip for computational resource reasons
# These methods are computationally intensive and would slow down the test suite
SKIP_METHODS_COMPUTATIONAL <- c(
  "RoBMA"   # Bayesian MCMC sampling - computationally intensive
)

test_that("All method implementations have required S3 methods", {

  # Get all S3 methods for the method generic function
  method_methods <- methods("method")

  # Extract method names (e.g., "method.PET" -> "PET")
  method_names <- sub("^method\\.", "", method_methods)

  # Filter out "default" since it's a fallback, not an actual method
  method_names <- method_names[!method_names %in% c("default", "skeleton")]

  # Expect at least some methods to be found
  expect_true(length(method_names) > 0,
              info = "No method implementation files found")

  # Get all S3 methods for the necessary functions
  method_settings_methods      <- methods("method_settings")
  method_extra_columns_methods <- methods("method_extra_columns")

  # Test each method
  for (method_name in method_names) {

    # Test 1: method_settings.{METHOD_NAME} method exists
    settings_method_name <- paste0("method_settings.", method_name)
    expect_true(
      settings_method_name %in% method_settings_methods,
      info = paste0("Method '", settings_method_name, "' does not exist")
    )

    # Test 2: method_extra_columns.{METHOD_NAME} method exists
    extra_cols_method_name <- paste0("method_extra_columns.", method_name)
    expect_true(
      extra_cols_method_name %in% method_extra_columns_methods,
      info = paste0("Method '", extra_cols_method_name, "' does not exist")
    )
  }
})


test_that("Method settings functions return valid lists", {

  # Get all S3 methods for the method generic function
  method_methods <- methods("method")
  method_names <- sub("^method\\.", "", method_methods)
  method_names <- method_names[!method_names %in% c("default", "skeleton")]

  # Skip RoBMA because of possibly missing JAGS
  method_names <- method_names[!method_names %in% "RoBMA"]

  for (method_name in method_names) {

    # Skip RoBMA if JAGS is not available
    if (method_name == "RoBMA") {
      if (!PublicationBiasBenchmark:::.check_robma_available(message_on_fail = FALSE, stop_on_fail = FALSE)) {
        skip("RoBMA requires JAGS to be installed")
      }
    }

    # Get settings
    settings <- method_settings(method_name)

    # Test that settings is a list
    # method_settings('{method_name}') should return a list
    expect_type(settings, "list")

    # Test that settings has a "default" entry
    expect_true(
      "default" %in% names(settings),
      info = paste0("method_settings('", method_name, "') does not have a 'default' setting")
    )

    # Test that there is at least one setting
    expect_true(
      length(settings) > 0,
      info = paste0("method_settings('", method_name, "') returns empty list")
    )
  }
})


test_that("Method extra columns functions return valid character vectors", {

  # Get all S3 methods for the method generic function
  method_methods <- methods("method")
  method_names <- sub("^method\\.", "", method_methods)
  method_names <- method_names[!method_names %in% c("default", "skeleton")]

  # Skip RoBMA because of possibly missing JAGS
  method_names <- method_names[!method_names %in% "RoBMA"]

  for (method_name in method_names) {

    # Get extra columns
    extra_cols <- method_extra_columns(method_name)

    # Test that extra_cols is a character vector
    # method_extra_columns('{method_name}') should return a character vector
    expect_type(extra_cols, c("character"))
  }
})


test_that("Method run_method works with default settings", {

  # Create simple test data
  test_data <- data.frame(
    yi = c(0.2, 0.3, 0.1, 0.4, 0.25),
    sei = c(0.1, 0.15, 0.08, 0.12, 0.11),
    ni = c(50, 60, 70, 55, 65)
  )

  # Get all S3 methods for the method generic function
  method_methods <- methods("method")
  method_names <- sub("^method\\.", "", method_methods)
  method_names <- method_names[!method_names %in% c("default", "skeleton")]

  # Skip computationally intensive methods
  method_names <- method_names[!method_names %in% SKIP_METHODS_COMPUTATIONAL]

  for (method_name in method_names) {

    # Test that run_method works with default settings
    result <- suppressWarnings(run_method(method_name, test_data, "default"))

    # Test that result is a data frame
    expect_s3_class(
      result,
      "data.frame"
    )

    # Test that result has required columns
    required_cols <- c("method", "estimate", "standard_error", "ci_lower",
                      "ci_upper", "p_value", "BF", "convergence", "note")

    for (col in required_cols) {
      expect_true(
        col %in% names(result),
        info = paste0("run_method('", method_name, "') result missing '", col, "' column")
      )
    }

    # Test that result has exactly one row
    expect_equal(
      nrow(result),
      1,
      info = paste0("run_method('", method_name, "') result does not have exactly one row")
    )

    # Test that method column contains the method name
    expect_equal(
      result$method,
      method_name,
      info = paste0("run_method('", method_name, "') result 'method' column does not match method name")
    )

    # Test that convergence is logical
    # result$convergence should be logical
    expect_type(result$convergence, "logical")
  }
})


test_that("Method extra columns are present in run_method output", {

  # Create simple test data
  test_data <- data.frame(
    yi = c(0.2, 0.3, 0.1, 0.4, 0.25),
    sei = c(0.1, 0.15, 0.08, 0.12, 0.11),
    ni = c(50, 60, 70, 55, 65)
  )

  # Get all S3 methods for the method generic function
  method_methods <- methods("method")
  method_names <- sub("^method\\.", "", method_methods)
  method_names <- method_names[!method_names %in% c("default", "skeleton")]

  # Skip computationally intensive methods
  method_names <- method_names[!method_names %in% SKIP_METHODS_COMPUTATIONAL]

  for (method_name in method_names) {

    # Get extra columns for this method
    extra_cols <- method_extra_columns(method_name)

    # Run method
    result <- suppressWarnings(run_method(method_name, test_data, "default"))

    # Test that all extra columns are present in the result
    for (col in extra_cols) {
      expect_true(
        col %in% names(result),
        info = paste0("run_method('", method_name, "') result missing extra column '", col,
                     "' declared in method_extra_columns")
      )
    }
  }
})


test_that("Method run_method handles errors gracefully", {

  # Create invalid test data (too few observations)
  invalid_data <- data.frame(
    yi = c(),
    sei = c(),
    ni = c()
  )

  # Get all S3 methods for the method generic function
  method_methods <- methods("method")
  method_names   <- sub("^method\\.", "", method_methods)
  method_names   <- method_names[!method_names %in% c("default", "skeleton")]

  # Skip computationally intensive methods
  method_names <- method_names[!method_names %in% SKIP_METHODS_COMPUTATIONAL]

  for (method_name in method_names) {

    # Test that run_method returns a result (not an error)
    result <- run_method(method_name, invalid_data, "default", silent = TRUE)

    # Test that result is a data frame even on error
    expect_s3_class(
      result,
      "data.frame"
    )    # Test that convergence is FALSE when there's an error
    expect_false(
      result$convergence,
      info = paste0("run_method('", method_name, "') convergence should be FALSE for invalid data")
    )

    # Test that note contains some information about the error
    expect_true(
      !is.na(result$note) && nchar(result$note) > 0,
      info = paste0("run_method('", method_name, "') note should contain error information")
    )
  }
})
