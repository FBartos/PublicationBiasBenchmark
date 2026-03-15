# Normal Unbiased Data-Generating Mechanism

An example data-generating mechanism to simulate effect sizes without
publication bias.

## Usage

``` r
# S3 method for class 'no_bias'
dgm(dgm_name, settings)
```

## Arguments

- dgm_name:

  DGM name (automatically passed)

- settings:

  List containing

  mean_effect

  :   Mean effect

  heterogeneity

  :   Effect heterogeneity

  n_studies

  :   Number of effect size estimates

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

Sample sizes of individual effect size estimates are generated from a
negative binomial distribution based on empirical sample size
distribution presented in Appendix B of Maier et al. (2023)

## References

Maier M, Bartoš F, Wagenmakers E (2023). “Robust Bayesian meta-analysis:
Addressing publication bias with model-averaging.” *Psychological
Methods*, **28**(1), 107-122.
[doi:10.1037/met0000405](https://doi.org/10.1037/met0000405) .

## See also

[`dgm()`](https://fbartos.github.io/PublicationBiasBenchmark/reference/dgm.md),
[`validate_dgm_setting()`](https://fbartos.github.io/PublicationBiasBenchmark/reference/validate_dgm_setting.md)

## Author

František Bartoš <f.bartos96@gmail.com>
