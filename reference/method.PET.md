# PET (Precision-Effect Test) Method

Implements the Precision-Effect Test for publication bias correction.
PET regresses effect sizes against standard errors to test for and
correct publication bias. The intercept represents the bias-corrected
effect size estimate. See Stanley and Doucouliagos (2014) for details.

## Usage

``` r
# S3 method for class 'PET'
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

Data frame with PET results

## References

Stanley TD, Doucouliagos H (2014). “Meta-regression approximations to
reduce publication selection bias.” *Research Synthesis Methods*,
**5**(1), 60–78.
[doi:10.1002/jrsm.1095](https://doi.org/10.1002/jrsm.1095) .

## Author

František Bartoš <f.bartos96@gmail.com>

## Examples

``` r
# Generate some example data
data <- data.frame(
  yi = c(0.2, 0.3, 0.1, 0.4, 0.25),
  sei = c(0.1, 0.15, 0.08, 0.12, 0.09)
)

# Apply PET method
result <- run_method("PET", data)
print(result)
#>             method   estimate standard_error  ci_lower  ci_upper   p_value BF
#> (Intercept)    PET -0.1360294      0.1863442 -0.501264 0.2292052 0.5182334 NA
#>             convergence note bias_coefficient bias_p_value method_setting
#> (Intercept)        TRUE   NA          3.59473     0.147487        default
```
