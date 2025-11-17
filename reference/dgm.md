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
#>             yi        sei   ni es_type
#> 1  -0.10800241 0.28888551   48     SMD
#> 2  -0.06930436 0.39235000   26     SMD
#> 3   0.49314901 0.38366643   28     SMD
#> 4   0.04244850 0.05976816 1120     SMD
#> 5   0.39060875 0.47587858   18     SMD
#> 6  -0.18115375 0.50102447   16     SMD
#> 7  -0.03658874 0.23572198   72     SMD
#> 8   0.25387417 0.23342995   74     SMD
#> 9   0.19210061 0.26321786   58     SMD
#> 10  0.88012881 0.60465647   12     SMD
```
