# SM (Selection Models) Method

Implements selection models for publication bias correction in
meta-analysis. The method first fits a random effects meta-analysis
model, then applies selection modeling to adjust for publication bias
using the metafor package. Selection models account for the probability
that studies are published based on their p-values or effect sizes. See
Vevea and Hedges (1995) for details.

## Usage

``` r
# S3 method for class 'SM'
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

Data frame with SM results

## Details

The following settings are implemented

- `"default"` or `"3PSM"`:

  3-parameter step function selection model with Maximum Likelihood
  estimator (`method = "ML"`) and one step at one-sided p = 0.025 (i.e.,
  selection for significance))

- `"4PSM"`:

  4-parameter step function selection model with Maximum Likelihood
  estimator (`method = "ML"`) and two steps at one-sided p = 0.025 and p
  = 0.50 (i.e., selection for significance and direction of the effect)

## References

Vevea JL, Hedges LV (1995). “A general linear model for estimating
effect size in the presence of publication bias.” *Psychometrika*,
**60**(3), 419–435.
[doi:10.1007/BF02294384](https://doi.org/10.1007/BF02294384) .

## Examples

``` r
# Generate some example data
data <- data.frame(
  yi = c(0.2, 0.3, 0.1, 0.4, 0.25),
  sei = c(0.1, 0.15, 0.08, 0.12, 0.09)
)

# Apply SM method
result <- run_method("SM", data, "3PSM")
#> Error : Optimizer (optim) did not achieve convergence (convergence = 1).
print(result)
#>   method estimate standard_error ci_lower ci_upper p_value BF convergence
#> 1     SM       NA             NA       NA       NA      NA NA       FALSE
#>                                                                         note
#> 1 Error : Optimizer (optim) did not achieve convergence (convergence = 1).\n
#>   tau_estimate tau_ci_lower tau_ci_upper tau_p_value bias_coefficient
#> 1           NA           NA           NA          NA               NA
#>   bias_coefficient_se bias_p_value method_setting
#> 1                  NA           NA           3PSM
```
