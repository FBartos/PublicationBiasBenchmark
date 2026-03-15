# WLS (Weighted Least Squares) Method

Implements the Weighted Least Squares method for meta-analysis. WLS fits
a weighted regression model with effect sizes as the outcome and weights
based on the inverse of the squared standard errors. The intercept
represents the weighted average effect size estimate.

## Usage

``` r
# S3 method for class 'WLS'
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

Data frame with WLS results

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

# Apply WLS method
result <- run_method("WLS", data)
print(result)
#>             method  estimate standard_error  ci_lower  ci_upper    p_value BF
#> (Intercept)    WLS 0.2179928     0.04998789 0.1200165 0.3159691 0.01205352 NA
#>             convergence note method_setting
#> (Intercept)        TRUE   NA        default
```
