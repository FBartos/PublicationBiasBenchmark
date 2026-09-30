# Mixture Model of P-Hacking (MMPH)

Implements mixture model of p-hacking as described in Moss and De Bin
(2023) .

The model is estimated via
[`publipha::phma()`](https://rdrr.io/pkg/publipha/man/ma.html).

## Usage

``` r
# S3 method for class 'MMPH'
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

Data frame with MMPH results

## Details

The following settings are implemented

- `"default"`:

  MMPH with Stan control settings
  `control = list(adapt_delta = 0.95, max_treedepth = 15)`,
  `warmup = 2000`, and `iter = 4000`, and `chains = 3`, and convergence
  thresholds `max_r_hat = 1.05` and `min_ess = 300`

A fit is considered converged if the R-hat of the effect size is below
`max_r_hat` and its effective sample size is above `min_ess`. Both
thresholds must be specified with custom settings.

## References

Moss J, De Bin R (2023). “Modelling publication bias and p-hacking.”
*Biometrics*, **79**(1), 319–331.
[doi:10.1111/biom.13560](https://doi.org/10.1111/biom.13560) .

## Author

Frantisek Bartos <f.bartos96@gmail.com>

## Examples

``` r
# \donttest{
# Generate some example data
data <- data.frame(
  yi      = c(0.2, 0.3, 0.1, 0.4, 0.25),
  sei     = c(0.1, 0.15, 0.08, 0.12, 0.09),
  es_type = "SMD"
)

# Apply MAN method
result <- run_method("MMPH", data)
#> Warning: There were 939 divergent transitions after warmup. See
#> https://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
#> to find out why this is a problem and how to eliminate them.
#> Warning: Examine the pairs() plot to diagnose sampling problems
#> Warning: Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#bulk-ess
#> Warning: Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#tail-ess
print(result)
#>   method   estimate standard_error   ci_lower  ci_upper p_value BF convergence
#> 1   MMPH 0.09266129             NA -0.3357053 0.3904664      NA NA        TRUE
#>   note estimate_median estimate_n_eff estimate_r_hat tau_estimate tau_median
#> 1   NA       0.1148324        1068.03       1.001839    0.2483575  0.1871512
#>   tau_ci_lower tau_ci_upper tau_n_eff tau_r_hat divergent_iter method_setting
#> 1   0.01594945    0.8111914  608.3729  1.007081            939        default
# }
```
