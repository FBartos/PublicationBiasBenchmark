# Method Method

S3 Method for defining methods. See
[`run_method()`](https://fbartos.github.io/PublicationBiasBenchmark/reference/run_method.md)
for usage and further details.

## Usage

``` r
method(method_name, data, settings)
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

## Value

A data frame with method results following the structure described in
the Output Structure section. This is an S3 generic method that
dispatches to specific method implementations based on `method_name`.

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

## See also

[`run_method()`](https://fbartos.github.io/PublicationBiasBenchmark/reference/run_method.md)

## Examples

``` r
data <- data.frame(
  yi = c(0.2, 0.3, 0.1, 0.4),
  sei = c(0.1, 0.15, 0.08, 0.12)
)
result <- run_method("RMA", data, "default")
```
