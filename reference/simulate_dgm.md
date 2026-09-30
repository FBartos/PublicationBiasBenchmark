# Simulate From Data-Generating Mechanism

This function provides a unified interface to various data-generating
mechanisms for simulation studies. The specific DGM is determined by the
first argument. See
[`vignette("Adding_New_DGMs", package = "PublicationBiasBenchmark")`](https://fbartos.github.io/PublicationBiasBenchmark/doc/Adding_New_DGMs.md)
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
#> 1   0.25755982 0.2961040  46     SMD
#> 2  -0.05721959 0.2540522  62     SMD
#> 3   0.09504104 0.3537529  32     SMD
#> 4  -0.17554980 0.1964935 104     SMD
#> 5   0.30564833 0.2120459  90     SMD
#> 6   0.31696664 0.3803304  28     SMD
#> 7  -0.09158760 0.1451713 190     SMD
#> 8  -0.06960250 0.2020917  98     SMD
#> 9  -0.61713792 0.4577352  20     SMD
#> 10 -0.14039321 0.1783935 126     SMD

simulate_dgm("Carter2019", list(mean_effect = 0, effect_heterogeneity = 0,
                       bias = "high", QRP = "high", n_studies = 10))
#>            yi       sei  ni es_type
#> 1  0.67550295 0.2944954  49     SMD
#> 2  0.30153726 0.1341038 225     SMD
#> 3  1.66030655 0.6246537  15     SMD
#> 4  0.61978188 0.2486399  68     SMD
#> 5  0.47103362 0.2312475  77     SMD
#> 6  0.03423383 0.2085304  92     SMD
#> 7  0.22264315 0.2091884  92     SMD
#> 8  0.33214673 0.1655597 148     SMD
#> 9  0.60491268 0.2814724  53     SMD
#> 10 0.44040718 0.1985723 104     SMD

simulate_dgm("Stanley2017", list(environment = "SMD", mean_effect = 0,
                        effect_heterogeneity = 0, bias = 0, n_studies = 5,
                        sample_sizes = c(32,64,125,250,500)))
#>            yi        sei   ni es_type
#> 1  0.38370008 0.25228991   64     SMD
#> 2 -0.22342854 0.17732738  128     SMD
#> 3 -0.10876961 0.12658460  250     SMD
#> 4  0.20951019 0.08968776  500     SMD
#> 5  0.08014665 0.06327094 1000     SMD

```
