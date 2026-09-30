# Generic method function for publication bias correction

This function provides a unified interface to various publication bias
correction methods. The specific method is determined by the first
argument. See
[`vignette("Adding_New_Methods", package = "PublicationBiasBenchmark")`](https://fbartos.github.io/PublicationBiasBenchmark/doc/Adding_New_Methods.md)
for details of extending the package with new methods

## Usage

``` r
run_method(
  method_name,
  data,
  settings = NULL,
  silent = FALSE,
  fit_limit = NULL
)
```

## Arguments

- method_name:

  Character string specifying the method type

- data:

  Data frame containing yi (effect sizes) and sei (standard errors)

- settings:

  Either a character identifying a method version or list containing
  method-specific settings. An emty input will result in running the
  default (first implemented) version of the method.

- silent:

  Logical indicating whether error messages from the method should be
  suppressed.

- fit_limit:

  Optional numeric giving the maximum time, in minutes, that the method
  is allowed to run. `NULL` (the default) or a non-finite value imposes
  no limit. When the limit is exceeded, the fit is aborted and a failure
  result with `convergence = FALSE` and
  `note = "time limit exceeded with <fit_limit> minutes"` is returned.
  See the Time Limits section for the mechanism and its platform
  differences.

## Value

A data frame with standardized method results

## Time Limits

Methods that spend their time inside compiled sampling code (`RoBMA` via
JAGS, `RTMA` and `MMPH` via Stan) do not return to R's evaluator and
therefore cannot be stopped by R's own elapsed-time limit. `fit_limit`
is consequently enforced by evaluating the method in a separate R
process
([callr::r_session](https://callr.r-lib.org/reference/r_session.html))
that is killed once the limit passes, which stops a fit regardless of
what it is executing, on every platform.

That process is started on the first limited fit and reused by the
following ones, adding roughly 0.05 seconds per fit; it is discarded and
replaced whenever a fit is killed or the process dies. A fit that
exceeds the limit therefore leaves no work behind, but also keeps
nothing from the fits before it.

The worker has its own random number stream, which is seeded from the
calling session for every fit. Results of methods that use randomness
stay reproducible from the calling session's seed, but differ from those
obtained without a limit.

## Output Structure

The returned data frame follows a standardized schema that downstream
functions rely on. All methods return the following columns:

- `method` (character): The name of the method used.

- `estimate` (numeric): The meta-analytic effect size estimate.

- `standard_error` (numeric): Standard error of the estimate.

- `ci_lower` (numeric): Lower bound of the 95% confidence interval.

- `ci_upper` (numeric): Upper bound of the 95% confidence interval.

- `p_value` (numeric): P-value for the estimate.

- `BF` (numeric): Bayes Factor for the estimate.

- `convergence` (logical): Whether the method converged successfully.

- `note` (character): Additional notes describing convergence issues.

Some methods may include additional method-specific columns beyond these
standard columns. Use
[`get_method_extra_columns()`](https://fbartos.github.io/PublicationBiasBenchmark/reference/method_extra_columns.md)
to query which additional columns a particular method returns.

## Examples

``` r
# Example usage with RMA method
data <- data.frame(
  yi = c(0.2, 0.3, 0.1, 0.4),
  sei = c(0.1, 0.15, 0.08, 0.12)
)
result <- run_method("RMA", data, "default")
```
