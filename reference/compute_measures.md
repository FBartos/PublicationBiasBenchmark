# Compute Multiple Performance measures for a DGM

This is a high-level wrapper function that computes multiple performance
measures for a Data-Generating Mechanism (DGM) and saves the results to
CSV files. It provides a clean and extensible interface for computing
standard simulation performance measures.

## Usage

``` r
compute_measures(
  dgm_name,
  method,
  method_setting,
  measures = NULL,
  verbose = TRUE,
  power_test_type = "p_value",
  power_threshold_p_value = 0.05,
  power_threshold_bayes_factor = 10,
  estimate_col = "estimate",
  true_effect_col = "mean_effect",
  ci_lower_col = "ci_lower",
  ci_upper_col = "ci_upper",
  p_value_col = "p_value",
  bf_col = "BF",
  convergence_col = "convergence",
  method_replacements = NULL,
  n_repetitions = 1000,
  overwrite = FALSE,
  conditions = NULL,
  results_source = c("local", "release"),
  release = NULL,
  replacement_source = NULL
)
```

## Arguments

- dgm_name:

  Character string specifying the DGM name

- method:

  Character vector of method names

- method_setting:

  Character vector of method settings, must be same length as method

- measures:

  Character vector of measures to compute. If NULL, computes all
  standard measures.

- verbose:

  Print detailed progress of the calculation.

- power_test_type:

  Character vector specifying the test type for power computation:
  "p_value" (default), "bayes_factor", or "ci" for each method. The "ci"
  option is intended for methods reporting only confidence intervals and
  rejects H0 if the confidence interval excludes zero. If a single value
  is provided, it is repeated for all methods.

- power_threshold_p_value:

  Numeric threshold for power computation with p-values. Default is 0.05
  (reject H0 if p \< 0.05).

- power_threshold_bayes_factor:

  Numeric threshold for power computation with Bayes factors. Default is
  10 (reject H0 if BF \> 10)

- estimate_col:

  Character string specifying the column name containing parameter
  estimates. Default is "estimate"

- true_effect_col:

  Character string specifying the column name in conditions data frame
  containing true effect sizes. Default is "mean_effect"

- ci_lower_col:

  Character string specifying the column name containing lower
  confidence interval bounds. Default is "ci_lower"

- ci_upper_col:

  Character string specifying the column name containing upper
  confidence interval bounds. Default is "ci_upper"

- p_value_col:

  Character string specifying the column name containing p-values.
  Default is "p_value"

- bf_col:

  Character string specifying the column name containing Bayes factors.
  Default is "BF"

- convergence_col:

  Character string specifying the column name containing convergence
  indicators. Default is "convergence"

- method_replacements:

  Named list of replacement method specifications. Each element should
  be named with the "method-method_setting" combination (e.g.,
  "RMA-default") and contain a named list with:

  - `method`: Character vector of replacement method names

  - `method_setting`: Character vector of replacement method settings
    (same length as methods)

  - `power_test_type`: Optional character vector of power test types for
    each replacement method (same length as methods). If not specified,
    uses the main power_test_type parameter

  If multiple elements are specified within the vectors, these
  replacements are applied consecutively in case the previous
  replacements also failed to converge. Defaults to `NULL`, i.e.,
  omitting repetitions without converged results on method-by-method
  basis.

- n_repetitions:

  Number of repetitions in each condition. Used to verify that the
  results contain all repetitions (an error is thrown if any repetition
  is missing) and for method replacement. Defaults to `1000`.

- overwrite:

  Logical indicating whether to overwrite existing results. If FALSE
  (default), will skip computation for method-measure combinations that
  already exist

- conditions:

  Data frame of conditions from dgm_conditions(). If NULL, the selected
  release's frozen conditions are used when `results_source = "release"`
  and `dgm_conditions(dgm_name)` otherwise. With local results and
  release replacements the package's conditions are used, and the
  release replacement results must cover each of them (missing
  repetitions are an error).

- results_source:

  Where the results of the computed methods come from: "local" (default)
  reads unpublished outputs under resources_directory/DGM/results;
  "release" reads verified results of a published benchmark release (see
  download_dgm_results()).

- release:

  Benchmark release identifier or catalog, passed to the result reader
  whenever either source is "release". It is ignored, with a warning,
  when both `results_source` and `replacement_source` are "local".

- replacement_source:

  Where the results of the replacement methods in `method_replacements`
  come from: "local" or "release". NULL (default) uses `results_source`.
  Use `replacement_source = "release"` to replace a new method's
  failures with published results of established methods.

## Value

TRUE upon successfully computation of the results file

## Examples

``` r
if (FALSE) { # \dontrun{
# Download DGM results
# Public downloads require no token.
dgm_name <- "no_bias"
download_dgm_results(dgm_name)

# Basic usage; results_source = "release" reads the downloaded published
# results, the default "local" reads unpublished outputs from the resources directory
compute_measures(
  dgm_name        = dgm_name,
  method          = c("mean", "RMA", "PET"),
  method_setting  = c("default", "default", "default"),
  measures        = c("bias", "mse", "coverage"),
  results_source  = "release"
)

# With method replacements for non-converged results
method_replacements <- list(
  "RMA-default" = list(method = "FMA", method_setting = "default"),
  "PET-default" = list(method = c("WLS", "FMA"),
                       method_setting = c("default", "default"))
)

compute_measures(
  dgm_name            = dgm_name,
  method              = c("RMA", "PET"),
  method_setting      = c("default", "default"),
  method_replacements = method_replacements,
  measures            = c("bias", "mse"),
  results_source      = "release"
)
} # }
```
