# Right-Truncated Meta-Analysis (RTMA) Method

Implements right-truncated meta-analysis (RTMA) for correcting the joint
effects of p-hacking and publication bias in meta-analysis. See Mathur
(2024) for details.

RTMA is estimated via
[`phacking::phacking_meta()`](https://mathurlabstanford.github.io/phacking/reference/phacking_meta.html).

## Usage

``` r
# S3 method for class 'RTMA'
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

Data frame with RTMA results

## Details

The following settings are implemented

- `"default"`:

  RTMA with affirmative results defined by positive direction
  `favor_positive = TRUE` and statistical significance
  `alpha_select = 0.05`, posterior interval level `ci_level = 0.95`,
  Stan control settings `adapt_delta = 0.98`, `max_treedepth = 20`,
  `parallelize = FALSE`, and convergence thresholds `max_r_hat = 1.01`
  and `min_ess = 500`

- `"relaxed"`:

  RTMA with affirmative results defined by positive direction
  `favor_positive = TRUE` and statistical significance
  `alpha_select = 0.05`, posterior interval level `ci_level = 0.95`,
  relaxed Stan control settings `adapt_delta = 0.95`,
  `max_treedepth = 15`, `parallelize = FALSE`, and relaxed convergence
  thresholds `max_r_hat = 1.05` and `min_ess = 300`

A fit is considered converged if the R-hat of the effect size is below
`max_r_hat` and its effective sample size is above `min_ess`. Both
thresholds must be specified with custom settings.

## References

Mathur MB (2024). “P-hacking in meta-analyses: A formalization and new
meta-analytic methods.” *Research Synthesis Methods*, **15**(3),
483–499. [doi:10.1002/jrsm.1701](https://doi.org/10.1002/jrsm.1701) .

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

# Apply RTMA method
result <- run_method("RTMA", data)
#> 
#> SAMPLING FOR MODEL 'phacking_rtma' NOW (CHAIN 1).
#> Chain 1: 
#> Chain 1: Gradient evaluation took 4.3e-05 seconds
#> Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 0.43 seconds.
#> Chain 1: Adjust your expectations accordingly!
#> Chain 1: 
#> Chain 1: 
#> Chain 1: Iteration:    1 / 2000 [  0%]  (Warmup)
#> Chain 1: Iteration:  200 / 2000 [ 10%]  (Warmup)
#> Chain 1: Iteration:  400 / 2000 [ 20%]  (Warmup)
#> Chain 1: Iteration:  600 / 2000 [ 30%]  (Warmup)
#> Chain 1: Iteration:  800 / 2000 [ 40%]  (Warmup)
#> Chain 1: Iteration: 1000 / 2000 [ 50%]  (Warmup)
#> Chain 1: Iteration: 1001 / 2000 [ 50%]  (Sampling)
#> Chain 1: Iteration: 1200 / 2000 [ 60%]  (Sampling)
#> Chain 1: Iteration: 1400 / 2000 [ 70%]  (Sampling)
#> Chain 1: Iteration: 1600 / 2000 [ 80%]  (Sampling)
#> Chain 1: Iteration: 1800 / 2000 [ 90%]  (Sampling)
#> Chain 1: Iteration: 2000 / 2000 [100%]  (Sampling)
#> Chain 1: 
#> Chain 1:  Elapsed Time: 0.123 seconds (Warm-up)
#> Chain 1:                0.042 seconds (Sampling)
#> Chain 1:                0.165 seconds (Total)
#> Chain 1: 
#> 
#> SAMPLING FOR MODEL 'phacking_rtma' NOW (CHAIN 2).
#> Chain 2: 
#> Chain 2: Gradient evaluation took 1.2e-05 seconds
#> Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 0.12 seconds.
#> Chain 2: Adjust your expectations accordingly!
#> Chain 2: 
#> Chain 2: 
#> Chain 2: Iteration:    1 / 2000 [  0%]  (Warmup)
#> Chain 2: Iteration:  200 / 2000 [ 10%]  (Warmup)
#> Chain 2: Iteration:  400 / 2000 [ 20%]  (Warmup)
#> Chain 2: Iteration:  600 / 2000 [ 30%]  (Warmup)
#> Chain 2: Iteration:  800 / 2000 [ 40%]  (Warmup)
#> Chain 2: Iteration: 1000 / 2000 [ 50%]  (Warmup)
#> Chain 2: Iteration: 1001 / 2000 [ 50%]  (Sampling)
#> Chain 2: Iteration: 1200 / 2000 [ 60%]  (Sampling)
#> Chain 2: Iteration: 1400 / 2000 [ 70%]  (Sampling)
#> Chain 2: Iteration: 1600 / 2000 [ 80%]  (Sampling)
#> Chain 2: Iteration: 1800 / 2000 [ 90%]  (Sampling)
#> Chain 2: Iteration: 2000 / 2000 [100%]  (Sampling)
#> Chain 2: 
#> Chain 2:  Elapsed Time: 0.06 seconds (Warm-up)
#> Chain 2:                0.05 seconds (Sampling)
#> Chain 2:                0.11 seconds (Total)
#> Chain 2: 
#> 
#> SAMPLING FOR MODEL 'phacking_rtma' NOW (CHAIN 3).
#> Chain 3: 
#> Chain 3: Gradient evaluation took 1.3e-05 seconds
#> Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 0.13 seconds.
#> Chain 3: Adjust your expectations accordingly!
#> Chain 3: 
#> Chain 3: 
#> Chain 3: Iteration:    1 / 2000 [  0%]  (Warmup)
#> Chain 3: Iteration:  200 / 2000 [ 10%]  (Warmup)
#> Chain 3: Iteration:  400 / 2000 [ 20%]  (Warmup)
#> Chain 3: Iteration:  600 / 2000 [ 30%]  (Warmup)
#> Chain 3: Iteration:  800 / 2000 [ 40%]  (Warmup)
#> Chain 3: Iteration: 1000 / 2000 [ 50%]  (Warmup)
#> Chain 3: Iteration: 1001 / 2000 [ 50%]  (Sampling)
#> Chain 3: Iteration: 1200 / 2000 [ 60%]  (Sampling)
#> Chain 3: Iteration: 1400 / 2000 [ 70%]  (Sampling)
#> Chain 3: Iteration: 1600 / 2000 [ 80%]  (Sampling)
#> Chain 3: Iteration: 1800 / 2000 [ 90%]  (Sampling)
#> Chain 3: Iteration: 2000 / 2000 [100%]  (Sampling)
#> Chain 3: 
#> Chain 3:  Elapsed Time: 0.065 seconds (Warm-up)
#> Chain 3:                0.071 seconds (Sampling)
#> Chain 3:                0.136 seconds (Total)
#> Chain 3: 
#> 
#> SAMPLING FOR MODEL 'phacking_rtma' NOW (CHAIN 4).
#> Chain 4: 
#> Chain 4: Gradient evaluation took 1.3e-05 seconds
#> Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 0.13 seconds.
#> Chain 4: Adjust your expectations accordingly!
#> Chain 4: 
#> Chain 4: 
#> Chain 4: Iteration:    1 / 2000 [  0%]  (Warmup)
#> Chain 4: Iteration:  200 / 2000 [ 10%]  (Warmup)
#> Chain 4: Iteration:  400 / 2000 [ 20%]  (Warmup)
#> Chain 4: Iteration:  600 / 2000 [ 30%]  (Warmup)
#> Chain 4: Iteration:  800 / 2000 [ 40%]  (Warmup)
#> Chain 4: Iteration: 1000 / 2000 [ 50%]  (Warmup)
#> Chain 4: Iteration: 1001 / 2000 [ 50%]  (Sampling)
#> Chain 4: Iteration: 1200 / 2000 [ 60%]  (Sampling)
#> Chain 4: Iteration: 1400 / 2000 [ 70%]  (Sampling)
#> Chain 4: Iteration: 1600 / 2000 [ 80%]  (Sampling)
#> Chain 4: Iteration: 1800 / 2000 [ 90%]  (Sampling)
#> Chain 4: Iteration: 2000 / 2000 [100%]  (Sampling)
#> Chain 4: 
#> Chain 4:  Elapsed Time: 0.317 seconds (Warm-up)
#> Chain 4:                0.08 seconds (Sampling)
#> Chain 4:                0.397 seconds (Total)
#> Chain 4: 
#> Warning: There were 42 divergent transitions after warmup. See
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
#>   method   estimate standard_error    ci_lower ci_upper p_value BF convergence
#> 1   RTMA 0.08532409             NA -0.07998049 2.071974      NA NA       FALSE
#>   note estimate_median estimate_mean estimate_n_eff estimate_r_hat tau_estimate
#> 1   NA       0.1381143     0.2952222       104.4241       1.036616   0.04624282
#>   tau_median  tau_mean tau_ci_lower tau_ci_upper tau_n_eff tau_r_hat
#> 1  0.1081534 0.1734836   0.01173193    0.7560038  134.5846  1.030427
#>   k_affirmative k_nonaffirmative optim_converged divergent_iter method_setting
#> 1             2                3            TRUE             42        default
# }
```
