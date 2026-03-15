# Alinaghi and Reed (2018) Data-Generating Mechanism

This data-generating mechanism simulates univariate regression studies
where a variable X affects a continuous outcome Y. Each study estimates
the coefficient of X, which consists of a fixed component (alpha1)
representing the overall mean effect, and a random component that varies
across studies but is constant within each study. In the "Random
Effects" environment (`"RE"`), each study produces one estimate, and the
population effect differs across studies. In the "Panel Random Effects"
environment (`"PRE"`), each study has 10 estimates, modeling the common
scenario where multiple estimates per study are available, with
publication selection targeting the study rather than individual
estimates.

The description and code is based on Hong and Reed (2021) . The
data-generating mechanism was introduced in Alinaghi and Reed (2018) .

## Usage

``` r
# S3 method for class 'Alinaghi2018'
dgm(dgm_name, settings)
```

## Arguments

- dgm_name:

  DGM name (automatically passed)

- settings:

  List containing

  environment

  :   Type of the simulation environment. One of `"FE"`, `"RE"`, or
      `"PRE"`.

  mean_effect

  :   Mean effect

  bias

  :   Type of publication bias. One of `"none"`, `"positive"`, or
      `"significant"`.

## Value

Data frame with

- yi:

  effect size

- sei:

  standard error

- ni:

  sample size

- study_id:

  study identifier

- es_type:

  effect size type

## Details

This data-generating mechanism is based on Alinaghi & Reed (2018), who
study univariate regression models where a variable X affects a
continuous variable Y. The parameter of interest is the coefficient on
X. In the "Random Effects" environment (`"RE"`), each study produces one
estimate, and the population effect differs across studies. The
coefficient on X equals a fixed component (alpha1) plus a random
component that is fixed within a study but varies across studies. The
overall mean effect of X on Y is given by alpha1. In the "Panel Random
Effects" environment (`"PRE"`), each study has 10 estimates, modeling
the common scenario where multiple estimates per study are available. In
this environment, effect estimates and standard errors are simulated to
be more similar within studies than across studies, and publication
selection targets the study rather than individual estimates (a study
must have at least 7 out of 10 estimates that are significant or
correctly signed.).

A distinctive feature of Alinaghi & Reed's experiments is that the
number of effect size estimates is fixed before publication selection,
making the meta-analyst's sample size endogenous and affected by the
effect size. Large population effects are subject to less publication
selection, as most estimates satisfy the selection criteria (statistical
significance or correct sign). The sample size of all primary studies is
fixed at 100 observations. (Neither the number of estimates nor the
sample size of primary studies can be changed in the current
implementation of the function.)

Another feature is the separation of statistical significance and sign
of the estimated effect as criteria for selection.
Significant/correctly-signed estimates are always "published," while
insignificant/wrong-signed estimates have only a 10% chance of being
published. This allows for different and sometimes conflicting
consequences for estimator performance.

## References

Alinaghi N, Reed WR (2018). “Meta-analysis and publication bias: How
well does the FAT-PET-PEESE procedure work?” *Research Synthesis
Methods*, **9**(2), 285-311.
[doi:10.1002/jrsm.1298](https://doi.org/10.1002/jrsm.1298) .  
  
Hong S, Reed WR (2021). “Using Monte Carlo experiments to select
meta-analytic estimators.” *Research Synthesis Methods*, **12**(2),
192-215. [doi:10.1002/jrsm.1467](https://doi.org/10.1002/jrsm.1467) .

## See also

[`dgm()`](https://fbartos.github.io/PublicationBiasBenchmark/reference/dgm.md),
[`validate_dgm_setting()`](https://fbartos.github.io/PublicationBiasBenchmark/reference/validate_dgm_setting.md)

## Author

František Bartoš <f.bartos96@gmail.com> (adapted from Hong and Reed
2021)
