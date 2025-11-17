# PEESE (Precision-Effect Estimate with Standard Errors) Method

Implements the Precision-Effect Estimate with Standard Errors method for
publication bias correction. PEESE regresses effect sizes against
standard errors^2 to correct for publication bias. The intercept
represents the bias-corrected effect size estimate. See Stanley and
Doucouliagos (2014) for details.

## Usage

``` r
# S3 method for class 'PEESE'
method(method_name, data, settings = NULL)
```

## Arguments

- method_name:

  Method name (automatically passed)

- data:

  Data frame with yi (effect sizes) and sei (standard errors)

- settings:

  List of method settings (no settings version are implemented)

## Value

Data frame with PEESE results

## References

Stanley TD, Doucouliagos H (2014). “Meta-regression approximations to
reduce publication selection bias.” *Research Synthesis Methods*,
**5**(1), 60–78.
[doi:10.1002/jrsm.1095](https://doi.org/10.1002/jrsm.1095) .

## Examples

``` r
# Generate some example data
data <- data.frame(
  yi = c(0.2, 0.3, 0.1, 0.4, 0.25),
  sei = c(0.1, 0.15, 0.08, 0.12, 0.09)
)

# Apply PEESE method
result <- run_method("PEESE", data)
print(result)
#>             method   estimate standard_error   ci_lower  ci_upper   p_value BF
#> (Intercept)  PEESE 0.06720823     0.09919638 -0.1272167 0.2616331 0.5466436 NA
#>             convergence note bias_coefficient bias_p_value method_setting
#> (Intercept)        TRUE   NA         14.88532    0.1927957        default
```
