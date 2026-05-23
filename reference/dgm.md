# DGM Method

S3 Method for defining data-generating mechanisms. See
[`simulate_dgm()`](https://fbartos.github.io/PublicationBiasBenchmark/reference/simulate_dgm.md)
for usage and further details.

## Usage

``` r
dgm(dgm_name, settings)
```

## Arguments

- dgm_name:

  Character string specifying the DGM type

- settings:

  List containing the required parameters for the DGM or numeric
  condition_id

## Value

A data frame with simulated data following the structure described in
the Output Structure section. This is an S3 generic method that
dispatches to specific DGM implementations based on `dgm_name`.

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

[`simulate_dgm()`](https://fbartos.github.io/PublicationBiasBenchmark/reference/simulate_dgm.md)

## Examples

``` r

simulate_dgm("Carter2019", 1)
#>             yi        sei  ni es_type
#> 1   0.02957667 0.15340138 170     SMD
#> 2   0.65188090 0.26065904  62     SMD
#> 3   0.43794482 0.36949956  30     SMD
#> 4  -0.06340306 0.19616540 104     SMD
#> 5   0.12392935 0.18587350 116     SMD
#> 6  -0.37170132 0.36828797  30     SMD
#> 7   0.01948841 0.19245466 108     SMD
#> 8  -0.21635366 0.35458622  32     SMD
#> 9  -0.01977538 0.09853534 412     SMD
#> 10 -0.17787392 0.25870896  60     SMD
```
