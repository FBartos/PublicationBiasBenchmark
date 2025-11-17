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
#>             yi       sei  ni es_type
#> 1  -0.36100456 0.3681106  30     SMD
#> 2  -0.32997749 0.2643940  58     SMD
#> 3   0.70533775 0.4858412  18     SMD
#> 4  -0.25994330 0.1255268 256     SMD
#> 5   0.02979852 0.1474501 184     SMD
#> 6   0.16323063 0.4721889  18     SMD
#> 7  -0.28402439 0.2639336  58     SMD
#> 8  -0.04741783 0.3015537  44     SMD
#> 9  -0.16656330 0.2239942  80     SMD
#> 10  0.29194225 0.3102463  42     SMD

simulate_dgm("Carter2019", list(mean_effect = 0, effect_heterogeneity = 0,
                       bias = "high", QRP = "high", n_studies = 10))
#>            yi       sei  ni es_type
#> 1  0.78360290 0.3686681  32     SMD
#> 2  0.04228118 0.2182435  84     SMD
#> 3  0.47544911 0.2255153  81     SMD
#> 4  0.90066316 0.3178459  44     SMD
#> 5  0.72976778 0.3455482  36     SMD
#> 6  0.69439468 0.3116263  44     SMD
#> 7  1.18205518 0.5541036  16     SMD
#> 8  0.13864104 0.1049557 364     SMD
#> 9  0.80602118 0.4029696  27     SMD
#> 10 0.51675737 0.2121197  92     SMD

simulate_dgm("Stanley2017", list(environment = "SMD", mean_effect = 0,
                        effect_heterogeneity = 0, bias = 0, n_studies = 5,
                        sample_sizes = c(32,64,125,250,500)))
#>            yi        sei   ni es_type
#> 1  0.27645111 0.25119131   64     SMD
#> 2 -0.14731625 0.17701631  128     SMD
#> 3  0.25515327 0.12700475  250     SMD
#> 4 -0.07596428 0.08947497  500     SMD
#> 5 -0.03670826 0.06325088 1000     SMD

```
