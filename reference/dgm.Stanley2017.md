# Stanley, Doucouliagos, and Ioannidis (2017) Data-Generating Mechanism

Simulates two scenarios for meta-analysis studies investigating the
effect of a treatment in: (1) Log Odds Ratio scenario, where the outcome
is binary and effect heterogeneity is controlled by a random component,
and (2) Cohen's d scenario, where the outcome is continuous and effect
heterogeneity is introduced through a random component. Both scenarios
allow for varying sample sizes and publication selection regimes,
affecting the inclusion of study estimates based on their statistical
significance and sign.

The description and code is based on Hong and Reed (2021) . The
data-generating mechanism was introduced in Stanley et al. (2017) .

## Usage

``` r
# S3 method for class 'Stanley2017'
dgm(dgm_name, settings)
```

## Arguments

- dgm_name:

  DGM name (automatically passed)

- settings:

  List containing

  environment

  :   Type of the simulation environment. One of `"logOR"` or `"SMD"`.

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

This function simulates two meta-analysis scenarios to evaluate the
effect of a binary treatment variable (`treat = {0, 1}`) on study
outcomes, incorporating both effect heterogeneity and publication
selection mechanisms.

In the Log Odds Ratio (`"logOR"`) scenario, primary studies assess the
impact of treatment on a binary success indicator (`Y = 1`). The control
group has a fixed 10% probability of success, while the treatment
group's probability is increased by a fixed effect and a mean-zero
random component, whose variance (sigma2_h) controls effect
heterogeneity. Each study estimates a logistic regression, with the
coefficient on `treat` (alpha1) as the effect of interest. Study sample
sizes vary, resulting in different standard errors for estimated
effects.

In the Cohen's d (`"SMD"`) scenario, the outcome variable is continuous.
The treatment effect is modeled as a fixed effect (alpha1) plus a random
component (variance sigma2_h). Each study computes Cohen's d, the
standardized mean difference between treatment and control groups. Study
sample sizes vary, affecting the standard errors of d.

Publication selection is modeled in two regimes: (1) no selection, and
(2) 50% selection. Under 50% selection, each estimate has a 50% chance
of being evaluated for inclusion. If selected, only positive and
statistically significant estimates are published; otherwise, new
estimates are generated until this criterion is met. This process
continues until the meta-analyst’s sample reaches its predetermined
size.

## References

Hong S, Reed WR (2021). “Using Monte Carlo experiments to select
meta-analytic estimators.” *Research Synthesis Methods*, **12**(2),
192-215. [doi:10.1002/jrsm.1467](https://doi.org/10.1002/jrsm.1467) .  
  
Stanley TD, Doucouliagos H, Ioannidis JP (2017). “Finding the power to
reduce publication bias.” *Statistics in Medicine*, **36**(10),
1580-1598. [doi:10.1002/sim.7228](https://doi.org/10.1002/sim.7228) .

## See also

[`dgm()`](https://fbartos.github.io/PublicationBiasBenchmark/reference/dgm.md),
[`validate_dgm_setting()`](https://fbartos.github.io/PublicationBiasBenchmark/reference/validate_dgm_setting.md)

## Author

František Bartoš <f.bartos96@gmail.com> (adapted from Hong and Reed
2021)
