# WAAPWLS (Weighted Average of Adequately Powered Studies) Method

Implements the WAAP-WLS method for meta-analysis, which combines WLS and
WAAP approaches. First fits a WLS model to all studies, then identifies
high-powered studies based on the criterion that the WLS estimate
divided by 2.8 is greater than or equal to the standard error. If at
least 2 high-powered studies are found, uses WAAP (weighted average of
adequate power studies only), otherwise uses the original WLS estimate.
See Stanley et al. (2017) for details.

## Usage

``` r
# S3 method for class 'WAAPWLS'
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

Data frame with WAAPWLS results

## References

Stanley TD, Doucouliagos H, Ioannidis JP (2017). “Finding the power to
reduce publication bias.” *Statistics in Medicine*, **36**(10),
1580-1598. [doi:10.1002/sim.7228](https://doi.org/10.1002/sim.7228) .

## Author

František Bartoš <f.bartos96@gmail.com>

## Examples

``` r
# Generate some example data
data <- data.frame(
  yi = c(0.2, 0.3, 0.1, 0.4, 0.25),
  sei = c(0.1, 0.15, 0.08, 0.12, 0.09)
)

# Apply WAAPWLS method
result <- run_method("WAAPWLS", data)
print(result)
#>              method  estimate standard_error  ci_lower  ci_upper    p_value BF
#> (Intercept) WAAPWLS 0.2179928     0.04998789 0.1200165 0.3159691 0.01205352 NA
#>             convergence note selected_method n_high_powered method_setting
#> (Intercept)        TRUE   NA             WLS              0        default
```
