# Trim-and-Fill Meta-Analysis Method

Implements the trim-and-fill method for adjusting publication bias in
meta-analysis using the metafor package.

## Usage

``` r
# S3 method for class 'trimfill'
method(method_name, data, settings)
```

## Arguments

- method_name:

  Method name (automatically passed)

- data:

  Data frame with yi (effect sizes) and sei (standard errors)

- settings:

  List of method settings (see Details.)

## Value

Data frame with trim-and-fill results

## Details

The following settings are implemented

- `"default"`:

  Random effects model fitted with Restricted Maximum Likelihood
  estimator (`method = "REML"`) with Knapp-Hartung adjustment
  (`test = "knha"`), followed by trim-and-fill using left-side trimming
  (`side = "left"`) and L0 estimator (`estimator = "L0"`).

## References

There are no references for Rd macro `\insertAllCites` on this help
page.

## Examples

``` r
# Generate some example data
data <- data.frame(
  yi = c(0.2, 0.3, 0.1, 0.4, 0.25),
  sei = c(0.1, 0.15, 0.08, 0.12, 0.09)
)

# Apply trimfill method
result <- run_method("trimfill", data)
print(result)
#>     method  estimate standard_error   ci_lower  ci_upper      p_value BF
#> 1 trimfill 0.1774672     0.05366124 0.07229309 0.2826413 0.0009424154 NA
#>   convergence note tau_estimate tau_ci_lower tau_ci_upper tau_p_value k_missing
#> 1        TRUE   NA    0.0877971            0    0.3178726   0.1226888         2
#>   k_missing_se method_setting
#> 1     1.602467        default
```
