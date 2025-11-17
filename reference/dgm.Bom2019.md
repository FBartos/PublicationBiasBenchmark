# Bom and Rachinger (2019) Data-Generating Mechanism

Simulates univariate regression environments to estimate the effect of
X1 on Y (parameter alpha1). Effect heterogeneity is introduced via an
omitted variable (X2) correlated with X1, whose coefficient (alpha2) is
randomly distributed with mean zero and variance sigma2_h.

The description and code is based on Hong and Reed (2021) . The
data-generating mechanism was introduced in Bom and Rachinger (2019) .

## Usage

``` r
# S3 method for class 'Bom2019'
dgm(dgm_name, settings)
```

## Arguments

- dgm_name:

  DGM name (automatically passed)

- settings:

  List containing

  mean_effect

  :   Mean effect

  effect_heterogeneity

  :   Mean effect heterogeneity

  bias

  :   Proportion of studies affected by publication bias

  n_studies

  :   Number of effect size estimates

  sample_sizes

  :   Sample sizes of the effect size estimates. A vector of sample
      sizes needs to be supplied. The sample sizes in the vector are
      sequentially reused until all effect size estimates are generated.

## Value

Data frame with

- yi:

  effect size

- sei:

  standard error

- ni:

  sample size

- es_type:

  effect size type

## Details

This function simulates univariate regression environments, focusing on
estimating the effect of a variable X1 on a dependent variable Y,
represented by the parameter alpha1. The simulation introduces variation
in the standard errors of estimated effects by allowing sample sizes to
differ across primary studies. Effect heterogeneity is modeled through
an omitted variable (X2) that is correlated with X1, where the
coefficient on the omitted variable, alpha2, is randomly distributed
across studies with mean zero and variance sigma2_h.

Publication selection is modeled in two regimes: (1) no selection, and
(2) 50% selection. Under 50% selection, each estimate has a 50% chance
of being evaluated for inclusion. If selected, only positive and
statistically significant estimates are published; otherwise, new
estimates are generated until this criterion is met. This process
continues until the meta-analyst’s sample reaches its predetermined
size.

## References

Bom PR, Rachinger H (2019). “A kinked meta-regression model for
publication bias correction.” *Research Synthesis Methods*, **10**(4),
497-514. [doi:10.1002/jrsm.1352](https://doi.org/10.1002/jrsm.1352) .  
  
Hong S, Reed WR (2021). “Using Monte Carlo experiments to select
meta-analytic estimators.” *Research Synthesis Methods*, **12**(2),
192-215. [doi:10.1002/jrsm.1467](https://doi.org/10.1002/jrsm.1467) .

## See also

[`dgm()`](https://fbartos.github.io/PublicationBiasBenchmark/reference/dgm.md),
[`validate_dgm_setting()`](https://fbartos.github.io/PublicationBiasBenchmark/reference/validate_dgm_setting.md)
