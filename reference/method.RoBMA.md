# Robust Bayesian Meta-Analysis (RoBMA) Method

Implements the robust Bayesian meta-analysis (RoBMA) method that uses
Bayesian model-averaging to combine results across several complementary
publication bias adjustment methods. See Maier et al. (2023) and Bartoš
et al. (2023) for details. If `"study_id"` column is included in the
data input, the method uses multilevel parameterization as described in
Bartoš et al. (2025) .

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

Bartoš F, Maier M, Wagenmakers E (2025). “Robust Bayesian multilevel
meta-analysis: Adjusting for publication bias in the presence of
dependent effect sizes.” *ArXiV Preprint*.
[doi:10.31234/osf.io/9tgp2_v1](https://doi.org/10.31234/osf.io/9tgp2_v1)
.  
  
Bartoš F, Maier M, Wagenmakers E, Doucouliagos H, Stanley TD (2023).
“Robust Bayesian meta-analysis: Model-averaging across complementary
publication bias adjustment methods.” *Research Synthesis Methods*,
**14**(1), 99–116.
[doi:10.1002/jrsm.1594](https://doi.org/10.1002/jrsm.1594) .  
  
Maier M, Bartoš F, Wagenmakers E (2023). “Robust Bayesian meta-analysis:
Addressing publication bias with model-averaging.” *Psychological
Methods*, **28**(1), 107-122.
[doi:10.1037/met0000405](https://doi.org/10.1037/met0000405) .

## Author

František Bartoš <f.bartos96@gmail.com>

## Examples

``` r
# \donttest{
# Generate some example data
data <- data.frame(
  yi      = c(0.2, 0.3, 0.1, 0.4, 0.25),
  sei     = c(0.1, 0.15, 0.08, 0.12, 0.09),
  es_type = "SMD"
)

# Apply RoBMA method (requires RoBMA 3.6.1 version of the package)
#result <- run_method("RoBMA", data)
#print(result)
# }
```
