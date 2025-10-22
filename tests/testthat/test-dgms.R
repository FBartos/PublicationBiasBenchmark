context("Basic tests of DGMs")


test_that("All DGM implementations have required S3 methods", {

  # Get all S3 methods for the dgm generic function
  dgm_methods <- methods("dgm")

  # Extract DGM names (e.g., "dgm.Carter2019" -> "Carter2019")
  dgm_names <- sub("^dgm\\.", "", dgm_methods)

  # Filter out "default" since it's a fallback, not an actual DGM
  dgm_names <- dgm_names[dgm_names != "default"]

  # Expect at least some DGMs to be found
  expect_true(length(dgm_names) > 0,  info = "No DGM implementation files found")

  # Get all S3 emthods for the neccessary functions
  validate_dgm_setting_methods <- methods("validate_dgm_setting")
  dgm_conditions_methods       <- methods("dgm_conditions")

  # Test each DGM
  for (dgm_name in dgm_names) {

    # Test 1: validate_dgm_setting.{DGM_NAME} method exists
    validate_method_name <- paste0("validate_dgm_setting.", dgm_name)
    expect_true(
      validate_method_name %in% validate_dgm_setting_methods,
      info = paste0("Method '", validate_method_name, "' does not exist")
    )

    # Test 2: dgm_conditions.{DGM_NAME} method exists
    conditions_method_name <- paste0("dgm_conditions.", dgm_name)
    expect_true(
      conditions_method_name %in% dgm_conditions_methods,
      info = paste0("Method '", conditions_method_name, "' does not exist")
    )
  }
})


test_that("DGM conditions functions return valid data frames", {

  # Get all S3 methods for the dgm generic function
  dgm_methods <- methods("dgm")
  dgm_names <- sub("^dgm\\.", "", dgm_methods)
  dgm_names <- dgm_names[dgm_names != "default"]

  for (dgm_name in dgm_names) {

    # Get conditions
    conditions <- dgm_conditions(dgm_name)

    # Test that conditions is a data frame
    expect_s3_class(
      conditions,
      "data.frame",
    )

    # Test that conditions has a condition_id column
    expect_true(
      "condition_id" %in% names(conditions),
      info = paste0("dgm_conditions('", dgm_name, "') does not have a 'condition_id' column")
    )

    # Test that condition_id values are unique
    expect_equal(
      length(unique(conditions$condition_id)),
      nrow(conditions),
      info = paste0("dgm_conditions('", dgm_name, "') has duplicate condition_id values")
    )

    # Test that there is at least one condition
    expect_true(
      nrow(conditions) > 0,
      info = paste0("dgm_conditions('", dgm_name, "') returns empty data frame")
    )
  }
})


test_that("DGM simulate_dgm works with condition_id", {

  # Get all S3 methods for the dgm generic function
  dgm_methods <- methods("dgm")
  dgm_names <- sub("^dgm\\.", "", dgm_methods)
  dgm_names <- dgm_names[dgm_names != "default"]

  for (dgm_name in dgm_names) {

    # Get first condition
    conditions <- dgm_conditions(dgm_name)
    first_condition_id <- conditions$condition_id[1]

    # Test that simulate_dgm works with condition_id
    result <- simulate_dgm(dgm_name, first_condition_id)

    # Test that result is a data frame
    expect_s3_class(
      result,
      "data.frame"
    )

    # Test that result has required columns
    expect_true(
      "yi" %in% names(result),
      info = paste0("simulate_dgm('", dgm_name, "', ", first_condition_id,
                   ") result missing 'yi' column")
    )

    expect_true(
      "sei" %in% names(result),
      info = paste0("simulate_dgm('", dgm_name, "', ", first_condition_id,
                   ") result missing 'sei' column")
    )

    expect_true(
      "ni" %in% names(result),
      info = paste0("simulate_dgm('", dgm_name, "', ", first_condition_id,
                   ") result missing 'ni' column")
    )

    expect_true(
      "es_type" %in% names(result),
      info = paste0("simulate_dgm('", dgm_name, "', ", first_condition_id,
                   ") result missing 'es_type' column")
    )

    # Test that result has at least one row
    expect_true(
      nrow(result) > 0,
      info = paste0("simulate_dgm('", dgm_name, "', ", first_condition_id,
                   ") returns empty data frame")
    )
  }
})


test_that("DGM validation functions work correctly", {

  # Get all S3 methods for the dgm generic function
  dgm_methods <- methods("dgm")
  dgm_names <- sub("^dgm\\.", "", dgm_methods)
  dgm_names <- dgm_names[dgm_names != "default"]

  for (dgm_name in dgm_names) {

    # Get first condition to extract valid settings
    conditions      <- dgm_conditions(dgm_name)
    first_condition <- as.list(conditions[1, ])
    first_condition$condition_id <- NULL  # Remove condition_id

    # Test that validation passes for valid settings
    expect_true(
      validate_dgm_setting(dgm_name, first_condition),
      info = paste0("validate_dgm_setting('", dgm_name,
                   "') fails for valid settings from dgm_conditions")
    )

    # Test that validation fails for empty settings
    expect_error(
      validate_dgm_setting(dgm_name, list()),
      info = paste0("validate_dgm_setting('", dgm_name,
                   "') does not error on empty settings")
    )
  }
})
