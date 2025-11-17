# Weighted and Iterated Least Squares (WILS) Method

Implements the weighted and iterated least squares (WILS) method for
publication bias correction in meta-analysis. The method is based on the
idea of using excess statistical significance (ESS) to identify how many
underpowered studies should be removed to reduce publication selection
bias. See Stanley and Doucouliagos (2024) for details.

## Usage

``` r
# S3 method for class 'WILS'
method(method_name, data, settings = NULL)
```

## Arguments

- method_name:

  Method name (automatically passed)

- data:

  Data frame with yi (effect sizes) and sei (standard errors)

- settings:

  List of method settings (see Details)

## Value

Data frame with WILS results

## Details

The WILS method has two implementation versions based on Stanley &
Doucouliagos (2024). The following settings are implemented

- `"default"`:

  The simulation version (default) uses residuals from the t ~ Precision
  regression for the first iteration, then switches to individual excess
  statistical significance (ESS) for subsequent iterations.

- `"example"`:

  The example version consistently uses residuals from the t ~ Precision
  regression to identify studies to remove across all iterations.

## References

Stanley TD, Doucouliagos H (2024). “Harnessing the power of excess
statistical significance: Weighted and iterative least squares.”
*Psychological Methods*, **29**(2), 407–420.
[doi:10.1037/met0000502](https://doi.org/10.1037/met0000502) .

## Examples

``` r
# Generate some example data
data <- data.frame(
  yi = c(0.2, 0.3, 0.1, 0.4, 0.25),
  sei = c(0.1, 0.15, 0.08, 0.12, 0.09)
)

# Apply WILS method
result <- run_method("WILS", data)
print(result)
#>             method  estimate standard_error   ci_lower  ci_upper   p_value BF
#> (Intercept)   WILS 0.1662069     0.07448276 0.02022069 0.3121931 0.2682079 NA
#>             convergence note n_removed method_setting
#> (Intercept)        TRUE   NA         3        default
```
