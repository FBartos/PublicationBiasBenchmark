# Simulate From Data-Generating Mechanism

This function provides a unified interface to various data-generating
mechanisms for simulation studies. The specific DGM is determined by the
first argument. See
[[`vignette("Adding_New_DGMs", package = "PublicationBiasBenchmark")`](https://fbartos.github.io/PublicationBiasBenchmark/articles/Adding_New_DGMs.md)](https://fbartos.github.io/PublicationBiasBenchmark/doc/Adding_New_DGMs.md)
for details of extending the package with new DGMs.

## Usage

``` r
simulate_dgm(dgm_name, settings)
```

## Arguments

- dgm_name:

  Character string specifying the DGM type

- settings:

  List containing the required parameters for the DGM or numeric
  condition_id

## Value

A data frame containing the generated data with standardized structure

## Output Structure

The returned data frame follows a standardized schema that downstream
functions rely on. Across the currently implemented DGMs, the following
columns are used:

- `yi` (numeric): The effect size estimate.

- `sei` (numeric): Standard error of `yi`.

- `ni` (integer): Total sample size for the estimate (e.g., sum over
  groups where applicable).

- `es_type` (character): Effect size type, used to disambiguate the
  scale of `yi`. Currently used values are `"SMD"` (standardized mean
  difference / Cohen's d), `"logOR"` (log odds ratio), and `"none"`
  (unspecified generic continuous coefficient).

- `study_id` (integer/character, optional): Identifier of the primary
  study/cluster when a DGM yields multiple estimates per study (e.g.,
  Alinaghi2018, PRE). If absent, each row is treated as an independent
  study.

## See also

[`validate_dgm_setting()`](https://fbartos.github.io/PublicationBiasBenchmark/reference/validate_dgm_setting.md),
[`dgm.Stanley2017()`](https://fbartos.github.io/PublicationBiasBenchmark/reference/dgm.Stanley2017.md),
[`dgm.Alinaghi2018()`](https://fbartos.github.io/PublicationBiasBenchmark/reference/dgm.Alinaghi2018.md),
[`dgm.Bom2019()`](https://fbartos.github.io/PublicationBiasBenchmark/reference/dgm.Bom2019.md),
[`dgm.Carter2019()`](https://fbartos.github.io/PublicationBiasBenchmark/reference/dgm.Carter2019.md)

## Examples

``` r

simulate_dgm("Carter2019", 1)
#>              yi       sei  ni es_type
#> 1   0.007637488 0.4472152  20     SMD
#> 2   0.179673222 0.1829422 120     SMD
#> 3   0.093579427 0.4266347  22     SMD
#> 4   0.057188571 0.3923124  26     SMD
#> 5  -0.052892856 0.4472918  20     SMD
#> 6   0.040592673 0.1482651 182     SMD
#> 7   0.147353405 0.2890666  48     SMD
#> 8   0.029867798 0.0889158 506     SMD
#> 9  -0.064986328 0.3163112  40     SMD
#> 10 -0.129336528 0.3089292  42     SMD

simulate_dgm("Carter2019", list(mean_effect = 0, effect_heterogeneity = 0,
                       bias = "high", QRP = "high", n_studies = 10))
#>            yi       sei  ni es_type
#> 1  1.00556181 0.3781218  32     SMD
#> 2  0.08461967 0.2086119  92     SMD
#> 3  0.36956727 0.1594920 160     SMD
#> 4  1.12632130 0.5165476  18     SMD
#> 5  0.78400899 0.3294138  40     SMD
#> 6  0.08363017 0.2295215  76     SMD
#> 7  0.51244062 0.2191765  86     SMD
#> 8  0.99811889 0.3668715  34     SMD
#> 9  0.89897768 0.3733193  32     SMD
#> 10 0.54598650 0.2149387  90     SMD

simulate_dgm("Stanley2017", list(environment = "SMD", mean_effect = 0,
                        effect_heterogeneity = 0, bias = 0, n_studies = 5,
                        sample_sizes = c(32,64,125,250,500)))
#>             yi        sei   ni es_type
#> 1  0.360739876 0.25202513   64     SMD
#> 2 -0.688925385 0.18194498  128     SMD
#> 3 -0.194041430 0.12678842  250     SMD
#> 4  0.197469917 0.08966044  500     SMD
#> 5 -0.001791946 0.06324557 1000     SMD

```
