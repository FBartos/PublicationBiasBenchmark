# Fixed Effects Meta-Analysis Method

Implements the publication bias-unadjusted fixed effects meta-analysis.

## Usage

``` r
# S3 method for class 'FMA'
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

Data frame with FMA results

## Details

The following settings are implemented

- `"default"`:

  T-distribution adjustment (`test = "t"`) and cluster robust standard
  errors with small-sample adjustment (if converged, otherwise no
  small-sample adjustment or no cluster robust standard errors) for
  fixed effects meta-analysis if `study_ids` is specified in the data

## References

There are no references for Rd macro `\insertAllCites` on this help
page.

## Author

František Bartoš <f.bartos96@gmail.com>

## Examples

``` r
# Generate some example data
data <- data.frame(
  yi = c(0.2, 0.3, 0.1, 0.4, 0.25),
  sei = c(0.1, 0.15, 0.08, 0.12, 0.09)
)

# Apply FMA method
result <- run_method("FMA", data)
print(result)
#>   method  estimate standard_error   ci_lower  ci_upper     p_value BF
#> 1    FMA 0.2179928     0.04501055 0.09302349 0.3429621 0.008380921 NA
#>   convergence note tau_p_value method_setting
#> 1        TRUE   NA   0.2941821        default
```
