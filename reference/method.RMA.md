# Random Effects Meta-Analysis Method

Implements the publication bias-unadjusted random-effects meta-analysis.

## Usage

``` r
# S3 method for class 'RMA'
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

Data frame with RMA results

## Details

The following settings are implemented

- `"default"`:

  Restricted Maximum Likelihood estimator (`method = "REML"`) with
  Knapp-Hartung adjustment (`test = "knha"`) for a simple random effects
  meta-analysis and Restricted Maximum Likelihood estimator
  (`method = "REML"`) with t-distribution adjustment (`test = "t"`) and
  cluster robust standard errors with small-sample adjustment (if
  converged, otherwise no small-sample adjustment or no cluster robust
  standard errors) for a multilevel random effects meta-analysis if
  `study_ids` is specified in the data

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

# Apply RMA method
result <- run_method("RMA", data)
print(result)
#>   method  estimate standard_error   ci_lower  ci_upper    p_value BF
#> 1    RMA 0.2255651     0.05033069 0.08582468 0.3653055 0.01097584 NA
#>   convergence note tau_estimate tau_ci_lower tau_ci_upper tau_p_value
#> 1        TRUE   NA   0.05721499            0    0.3038259   0.2941821
#>   method_setting
#> 1        default
```
