# Robust Bayesian Meta-Analysis (RoBMA) Method

Implements the robust Bayesian meta-analysis (RoBMA) method that uses
Bayesian model-averaging to combine results across several complementary
publication bias adjustment methods. See Maier et al. (2023) and Bartoš
et al. (2023) for details.

Note that the prior settings is dispatched based on `"es_type"` column
attached to the dataset. The resulting estimates are then summarized on
the same scale as was the dataset input (for `"r"`, heterogeneity is
summarized on Fisher's z).

**Important:** This method requires JAGS (Just Another Gibbs Sampler) to
be installed on your system. Please download and install JAGS from
<https://mcmc-jags.sourceforge.io/> before using this method.

## Usage

``` r
# S3 method for class 'RoBMA'
method(method_name, data, settings)
```

## Arguments

- method_name:

  Method name (automatically passed)

- data:

  Data frame with yi (effect sizes), sei (standard errors), es_type
  (either `"SMD"` for Cohen's d / Hedge's g, `"logOR"` for log odds
  ratio, `"z"` for Fisher's z, or `"r"` for correlations. Defaults to
  `"none"` which re-scales the default priors to unit-information width
  based on total sample size supplied `"ni"`.)

- settings:

  List of method settings (see Details.)

## Value

Data frame with RoBMA results

## Details

The following settings are implemented

- `"default"`:

  RoBMA-PSMA with publication bias adjustment as described in Bartoš et
  al. (2023) . (the MCMC settings was reduced to speed-up the
  simulations) with the three-level specification whenever `"study_ids"`
  are supplied with the data

- `"PSMA"`:

  RoBMA-PSMA with publication bias adjustment as described in Bartoš et
  al. (2023) . (the MCMC settings was reduced to speed-up the
  simulations) with the three-level specification whenever `"study_ids"`
  are supplied with the data

## References

Bartoš F, Maier M, Wagenmakers E, Doucouliagos H, Stanley TD (2023).
“Robust Bayesian meta-analysis: Model-averaging across complementary
publication bias adjustment methods.” *Research Synthesis Methods*,
**14**(1), 99–116.
[doi:10.1002/jrsm.1594](https://doi.org/10.1002/jrsm.1594) .  
  
Maier M, Bartoš F, Wagenmakers E (2023). “Robust Bayesian meta-analysis:
Addressing publication bias with model-averaging.” *Psychological
Methods*, **28**(1), 107-122.
[doi:10.1037/met0000405](https://doi.org/10.1037/met0000405) .

## Examples

``` r
# \donttest{
# Generate some example data
data <- data.frame(
  yi      = c(0.2, 0.3, 0.1, 0.4, 0.25),
  sei     = c(0.1, 0.15, 0.08, 0.12, 0.09),
  es_type = "SMD"
)

# Apply RoBMA method
result <- run_method("RoBMA", data)
#> Loading required namespace: runjags
#> Loading required namespace: mvtnorm
print(result)
#>   method   estimate standard_error ci_lower  ci_upper p_value        BF
#> 1  RoBMA 0.07245329             NA        0 0.2827614      NA 0.7532874
#>   convergence note tau_estimate tau_ci_lower tau_ci_upper    tau_BF
#> 1        TRUE TRUE   0.04087817            0     0.252459 0.4609204
#>                                                                               bias_SM_coefficient
#> 1 1, 0.91543706489259, 0.831569767534035, 0.804442561500065, 0.814358728230785, 0.847369579189188
#>                                                                           bias_SM_coefficient_ci_lower
#> 1 1, 0.189202106868124, 0.0413680937549003, 0.0211984484761842, 0.0217220354856666, 0.0223131588475772
#>   bias_SM_coefficient_ci_upper                 bias_PP_coefficient
#> 1             1, 1, 1, 1, 1, 1 0.695002655675012, 3.60584982099692
#>   bias_PP_coefficient_ci_lower      bias_PP_coefficient_ci_upper  bias_BF
#> 1                         0, 0 2.9168083740665, 22.8263307590521 5.263982
#>   method_setting
#> 1        default
# }
```
