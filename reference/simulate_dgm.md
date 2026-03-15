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
#> 1  -0.13686539 0.1827878 120     SMD
#> 2  -0.20110752 0.1659390 146     SMD
#> 3   0.29723153 0.3448859  34     SMD
#> 4   0.06484761 0.1226601 266     SMD
#> 5   0.09594375 0.4266467  22     SMD
#> 6  -0.64207603 0.4022117  26     SMD
#> 7   0.67866778 0.3336518  38     SMD
#> 8  -0.03098970 0.2041364  96     SMD
#> 9   0.03861937 0.1118138 320     SMD
#> 10  0.27300882 0.2117983  90     SMD

simulate_dgm("Carter2019", list(mean_effect = 0, effect_heterogeneity = 0,
                       bias = "high", QRP = "high", n_studies = 10))
#>           yi       sei  ni es_type
#> 1  1.0203888 0.3730957  33     SMD
#> 2  0.3100572 0.2308651  76     SMD
#> 3  0.9735686 0.4108831  27     SMD
#> 4  0.6891020 0.2978578  48     SMD
#> 5  0.6640099 0.3141860  43     SMD
#> 6  0.3232743 0.1617205 155     SMD
#> 7  0.2855561 0.1407540 204     SMD
#> 8  0.2916250 0.1411467 203     SMD
#> 9  0.6891704 0.2758268  56     SMD
#> 10 0.2571214 0.1217753 272     SMD

simulate_dgm("Stanley2017", list(environment = "SMD", mean_effect = 0,
                        effect_heterogeneity = 0, bias = 0, n_studies = 5,
                        sample_sizes = c(32,64,125,250,500)))
#>            yi        sei   ni es_type
#> 1  0.15507196 0.25037546   64     SMD
#> 2  0.14590373 0.17701174  128     SMD
#> 3  0.27332351 0.12708033  250     SMD
#> 4 -0.04239066 0.08945276  500     SMD
#> 5  0.11838589 0.06330093 1000     SMD

```
