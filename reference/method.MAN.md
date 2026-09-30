# Meta-Analysis of Nonaffirmative Studies (MAN) Method

Implements standard meta-analysis of only the nonaffirmative studies
(MAN) which can serve as a sensitivity analysis for worst-case
meta-analytic point estimate for maximal publication bias under the
selection model. effects of p-hacking and publication bias in
meta-analysis. See Mathur and VanderWeele (2020) for details.

## Usage

``` r
# S3 method for class 'MAN'
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

Data frame with MAN results

## Details

The following settings are implemented

- `"default"`:

  MAN with affirmative results defined by positive direction
  `favor_positive = TRUE` and statistical significance
  `alpha_select = 0.05`

## References

Mathur MB, VanderWeele TJ (2020). “Sensitivity analysis for publication
bias in meta-analyses.” *Journal of the Royal Statistical Society Series
C: Applied Statistics*, **69**(5), 1091–1119.
[doi:10.1111/rssc.12440](https://doi.org/10.1111/rssc.12440) .

## Author

Frantisek Bartos <f.bartos96@gmail.com>

## Examples

``` r
# \donttest{
# Generate some example data with at least one nonaffirmative study
data <- data.frame(
  yi = c(0.20, 0.30, 0.10, -0.05, 0.12),
  sei = c(0.10, 0.15, 0.08, 0.12, 0.09)
)

# Apply MAN method
result <- run_method("MAN", data)
print(result)
#>   method   estimate standard_error    ci_lower  ci_upper   p_value BF
#> 1    MAN 0.07723757     0.03661896 -0.09989418 0.2543693 0.1844874 NA
#>   convergence note tau_estimate k_nonaffirmative method_setting
#> 1       FALSE   NA            0                3        default
# }
```
