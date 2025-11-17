# Compare method with a Single Measure for a DGM

This function provides pairwise comparison of method for Data-Generating
Mechanisms (DGMs). It compares method performance on a
condition-by-condition basis using estimates. For each pair of method,
if method A has an estimate closer to the true value than method B, it
gets a score of 1, if further it gets 0, and if equal it gets 0.5.

## Usage

``` r
compare_single_measure(
  dgm_name,
  measure_name,
  method,
  method_setting,
  conditions,
  estimate_col = "estimate",
  true_effect_col = "mean_effect",
  convergence_col = "convergence",
  method_replacements = NULL,
  n_repetitions = 1000,
  overwrite = FALSE,
  ...
)
```

## Arguments

- dgm_name:

  Character string specifying the name of the DGM dataset to download.

- measure_name:

  Name of the measure to compute (e.g., "bias", "mse")

- method:

  Character vector of method names

- method_setting:

  Character vector of method settings, must be same length as method

- conditions:

  Data frame of conditions from dgm_conditions()

- estimate_col:

  Character string specifying the column name containing parameter
  estimates. Default is "estimate"

- true_effect_col:

  Character string specifying the column name in conditions data frame
  containing true effect sizes. Default is "mean_effect"

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

  Number of repetitions in each condition. Necessary method replacement.
  Defaults to `1000`.

- overwrite:

  Logical indicating whether to overwrite existing files. Defaults to
  `FALSE`, which means only missing files will be downloaded.

- ...:

  Additional arguments passed to measure functions

## Value

Data frame with pairwise comparison scores in long format (method_a,
method_b, score)
