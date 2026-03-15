# PET-PEESE (Precision-Effect Test and Precision-Effect Estimate with Standard Errors) Method

Implements the Precision-Effect Test and Precision-Effect Estimate with
Standard Errors (PET-PEESE) regresses effect sizes against standard
errors^2 to correct for publication bias. The intercept represents the
bias-corrected effect size estimate. See Stanley and Doucouliagos (2014)
for details.

## Usage

``` r
# S3 method for class 'PETPEESE'
method(method_name, data, settings)
```

## Arguments

- method_name:

  Method name (automatically passed)

- data:

  Data frame with yi (effect sizes) and sei (standard errors)

- settings:

  List of method settings (see Details)

## Value

Data frame with PET-PEESE results

## Details

The following settings are implemented

- `"default"`:

  (`conditional_alpha = 0.10`) determines whether to use PET (PET's
  effect is not significant at alpha = 0.10 or PEESE estimate (PET's
  effect is significant at alpha = 0.10)

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

# Apply PETPEESE method
result <- run_method("PETPEESE", data)
print(result)
#>               method   estimate standard_error  ci_lower  ci_upper   p_value BF
#> (Intercept) PETPEESE -0.1360294      0.1863442 -0.501264 0.2292052 0.5182334 NA
#>             convergence note bias_coefficient bias_p_value selected_method
#> (Intercept)        TRUE   NA          3.59473     0.147487             PET
#>             method_setting
#> (Intercept)        default
```
