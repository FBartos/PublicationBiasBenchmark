# Endogenous Kink Method

Implements the endogenous kink (EK) method proposed by Bom and Rachinger
for publication bias correction in meta-analysis. This method modifies
the PET-PEESE approach by incorporating a non-linear relationship
between publication bias and standard errors through a kinked regression
specification. The method recognizes that when the true effect is
non-zero, there is minimal publication selection when standard errors
are very small (since most estimates are significant), but selection
increases as standard errors grow. The kink point is endogenously
determined using a two-step procedure based on the confidence interval
of the initial effect estimate. See Bom and Rachinger (2019) for
details.

## Usage

``` r
# S3 method for class 'EK'
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

Data frame with EK results

## References

Bom PR, Rachinger H (2019). “A kinked meta-regression model for
publication bias correction.” *Research Synthesis Methods*, **10**(4),
497-514. [doi:10.1002/jrsm.1352](https://doi.org/10.1002/jrsm.1352) .

## Author

František Bartoš <f.bartos96@gmail.com>

## Examples

``` r
# Generate some example data
data <- data.frame(
  yi = c(0.2, 0.3, 0.1, 0.4, 0.25),
  sei = c(0.1, 0.15, 0.08, 0.12, 0.09)
)

# Apply EK method
result <- run_method("EK", data)
print(result)
#>   method   estimate standard_error   ci_lower ci_upper   p_value BF convergence
#> 1     EK -0.1360294      0.1863442 -0.7290598 0.457001 0.5182334 NA        TRUE
#>   note method_setting
#> 1   NA        default
```
