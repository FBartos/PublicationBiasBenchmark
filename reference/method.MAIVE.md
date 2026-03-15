# MAIVE: Meta-Analysis Instrumental Variable Estimator

Implements the MAIVE method for publication bias correction using
instrumental variable estimation with variance instrumentation. MAIVE
addresses spurious precision in meta-analysis by instrumenting standard
errors with inverse sample sizes, providing consistent estimates even
when precision is manipulated through p-hacking (Irsova et al. 2025) .

The method implements several estimators:

- PET (Precision-Effect Test): Linear precision-effect model

- PEESE (Precision-Effect Estimate with Standard Error): Quadratic model

- PET-PEESE: Conditional selection based on PET significance

- EK (Endogenous Kink): Flexible bias function with kink point

- WAIVE: Robust variant with outlier downweighting

## Usage

``` r
# S3 method for class 'MAIVE'
method(method_name, data, settings)
```

## Arguments

- method_name:

  Method identifier (automatically passed by framework)

- data:

  Data frame with yi (effect sizes), sei (standard errors), ni (sample
  sizes), and optionally study_id for clustering

- settings:

  List of method settings from method_settings.MAIVE()

## Value

Single-row data frame with standardized output columns:

- method:

  Method identifier

- estimate:

  Meta-analytic effect size estimate

- standard_error:

  Standard error of estimate

- ci_lower, ci_upper:

  95% confidence interval bounds (Anderson-Rubin if available)

- p_value:

  Two-tailed p-value

- BF:

  Bayes factor (NA for MAIVE)

- convergence:

  Logical convergence indicator

- note:

  Error messages if any

- first_stage_f:

  First-stage F-statistic for instrument strength

- hausman_stat:

  Hausman test statistic comparing IV vs OLS

- bias_p_value:

  P-value for publication bias test

- used_ar_ci:

  Whether Anderson-Rubin CI was used

- ar_ci_available:

  Whether AR CI was computed successfully

- instrument_strength:

  Instrument strength classification returned by MAIVE or derived from
  the first-stage F-statistic

## Details

MAIVE uses inverse sample sizes (1/N) as instruments for variances
(SE^2) in the first stage, then uses the instrumented variances in
second-stage PET/PEESE models. This approach provides consistent
estimation when precision is endogenous due to p-hacking or selective
reporting.

The Anderson-Rubin confidence interval is robust to weak instruments and
is automatically computed for unweighted IV estimators when feasible (n
\< 5000). For weighted estimators or large samples, standard CIs are
used. PublicationBiasBenchmark targets MAIVE 0.2.4 and persists upstream
MAIVE warnings in the standardized `note` column.

WAIVE extends MAIVE by downweighting: (1) negative residuals (spurious
precision) using exponential decay, and (2) extreme residuals (\|z\| \>
2) as potential outliers. This provides additional robustness against
publication bias and outliers. When IV is used and the first-stage
F-statistic is numeric and below 10, PublicationBiasBenchmark blanks the
standardized inferential outputs (`estimate`, `standard_error`,
`ci_lower`, `ci_upper`, `p_value`) to `NA` while keeping
`convergence = TRUE`. These rows remain available as diagnostics and are
treated as missing estimates in downstream performance summaries rather
than as convergence failures.

Available settings (see method_settings.MAIVE()):

- default:

  PET-PEESE with IV, unweighted, levels first-stage

- PET:

  PET with IV, unweighted

- PEESE:

  PEESE with IV, unweighted

- EK:

  Endogenous Kink model with IV

- weighted:

  PET-PEESE with MAIVE-adjusted weighting

- WAIVE:

  Robust WAIVE variant with outlier downweighting

- log_first_stage:

  PET-PEESE with log-linear first stage

- no_IV:

  Standard PET-PEESE without instrumentation (baseline)

## References

Irsova Z, Bom PR, Havranek T, Rachinger H (2025). “Spurious precision in
meta-analysis of observational research.” *Nature Communications*,
**16**, 8454.
[doi:10.1038/s41467-025-63261-0](https://doi.org/10.1038/s41467-025-63261-0)
.

## See also

[`run_method()`](https://fbartos.github.io/PublicationBiasBenchmark/reference/run_method.md),
[`method_settings()`](https://fbartos.github.io/PublicationBiasBenchmark/reference/method_settings.md),
[`method_extra_columns()`](https://fbartos.github.io/PublicationBiasBenchmark/reference/method_extra_columns.md)

## Author

Petr Cala <cala.p@seznam.cz>

## Examples

``` r
if (FALSE) { # \dontrun{
# Generate test data
data <- simulate_dgm("Stanley2017", condition_id = 1)

# Apply default MAIVE (PET-PEESE with IV)
result <- run_method("MAIVE", data, "default")

# Apply WAIVE variant
result_waive <- run_method("MAIVE", data, "WAIVE")

# Apply weighted MAIVE
result_weighted <- run_method("MAIVE", data, "weighted")

# View available configurations
method_settings("MAIVE")
} # }
```
