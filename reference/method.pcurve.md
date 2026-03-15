# pcurve (P-Curve) Method

Implements the p-Curve method which analyzes the distribution of
p-values from significant studies to assess whether the significant
findings reflect true effects or QRP/publication bias. The method also
provides tests for the evidential value, lack of evidential value, and
p-hacking. See Simonsohn et al. (2014) for details.

The current implementation does not provide a test against the null
hypothsis of no effect and does not produce confidence intervals of the
estimate.

## Usage

``` r
# S3 method for class 'pcurve'
method(method_name, data, settings)
```

## Arguments

- method_name:

  Method name (automatically passed)

- data:

  Data frame with yi (effect sizes), sei (standard errors), and ni
  (sample sizes wherever available, otherwise set to Inf)

- settings:

  List of method settings (see Details)

## Value

Data frame with P-Curve results

## Details

The following settings are implemented

- `"default"`:

  no options

## References

Simonsohn U, Nelson LD, Simmons JP (2014). “p-curve and effect size:
Correcting for publication bias using only significant results.”
*Perspectives on Psychological Science*, **9**(6), 666–681.
[doi:10.1177/1745691614553988](https://doi.org/10.1177/1745691614553988)
.

## Author

František Bartoš <f.bartos96@gmail.com>

## Examples

``` r
# Generate some example data
data <- data.frame(
  yi = c(0.2, 0.3, 0.1, 0.4, 0.25),
  sei = c(0.1, 0.15, 0.08, 0.12, 0.09)
)

# Apply pcurve method
result <- run_method("pcurve", data)
#> Warning: ties should not be present for the one-sample Kolmogorov-Smirnov test
#> Warning: ties should not be present for the one-sample Kolmogorov-Smirnov test
#> Warning: ties should not be present for the one-sample Kolmogorov-Smirnov test
#> Warning: ties should not be present for the one-sample Kolmogorov-Smirnov test
#> Warning: ties should not be present for the one-sample Kolmogorov-Smirnov test
#> Warning: ties should not be present for the one-sample Kolmogorov-Smirnov test
#> Warning: ties should not be present for the one-sample Kolmogorov-Smirnov test
#> Warning: ties should not be present for the one-sample Kolmogorov-Smirnov test
#> Warning: ties should not be present for the one-sample Kolmogorov-Smirnov test
#> Warning: ties should not be present for the one-sample Kolmogorov-Smirnov test
#> Warning: ties should not be present for the one-sample Kolmogorov-Smirnov test
#> Warning: ties should not be present for the one-sample Kolmogorov-Smirnov test
#> Warning: ties should not be present for the one-sample Kolmogorov-Smirnov test
#> Warning: ties should not be present for the one-sample Kolmogorov-Smirnov test
#> Warning: ties should not be present for the one-sample Kolmogorov-Smirnov test
#> Warning: ties should not be present for the one-sample Kolmogorov-Smirnov test
#> Warning: ties should not be present for the one-sample Kolmogorov-Smirnov test
#> Warning: ties should not be present for the one-sample Kolmogorov-Smirnov test
#> Warning: ties should not be present for the one-sample Kolmogorov-Smirnov test
#> Warning: ties should not be present for the one-sample Kolmogorov-Smirnov test
#> Warning: ties should not be present for the one-sample Kolmogorov-Smirnov test
#> Warning: ties should not be present for the one-sample Kolmogorov-Smirnov test
#> Warning: ties should not be present for the one-sample Kolmogorov-Smirnov test
#> Warning: ties should not be present for the one-sample Kolmogorov-Smirnov test
print(result)
#>   method estimate standard_error ci_lower ci_upper p_value BF convergence note
#> 1 pcurve  3.99994             NA       NA       NA      NA NA        TRUE   NA
#>   p_value_evidence p_value_lack p_value_hack method_setting
#> 1        0.3699365    0.6300635    0.1962818        default
```
