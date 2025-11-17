# AK Method

Implements the Andrews & Kasy (AK) method for publication bias
correction in meta-analysis. The AK method categorizes estimated effects
into groups with different probabilities of being published. AK1 uses
symmetric selection grouping estimates into significant (\|t\| \>= 1.96)
and insignificant (\|t\| \< 1.96) estimates. AK2 uses asymmetric
selection with four groups based on both significance and sign: highly
significant positive/negative effects and marginally significant
positive/negative effects, each with different publication
probabilities. See Andrews and Kasy (2019) for details.

## Usage

``` r
# S3 method for class 'AK'
method(method_name, data, settings)
```

## Arguments

- method_name:

  Method name (automatically passed)

- data:

  Data frame with yi (effect sizes), sei (standard errors), and study_id
  (for clustering wherever available)

- settings:

  List of method settings (see Details)

## Value

Data frame with AK results

## Details

The following settings are implemented

- `"default"`:

  Uses AK1 estimator (symmetric selection)

- `"AK1"`:

  Symmetric selection model grouping estimates into significant (\|t\|
  \>= 1.96) and insignificant (\|t\| \< 1.96) categories with relative
  publication probabilities of 1 and p1 respectively.

- `"AK2"`:

  Asymmetric selection model with four groups based on t-statistics: (a)
  t \>= 1.96, (b) t \< -1.96, (c) -1.96 \<= t \< 0, and (d) 0 \<= t \<
  1.96, with relative publication probabilities of 1, p1, p2, and p3
  respectively.

## References

Andrews I, Kasy M (2019). “Identification of and correction for
publication bias.” *American Economic Review*, **109**(8), 2766–2794.
[doi:10.1257/aer.20180310](https://doi.org/10.1257/aer.20180310) .

## Examples

``` r
# Generate some example data
data <- data.frame(
  yi = c(0.2, 0.3, 0.1, 0.4, 0.25),
  sei = c(0.1, 0.15, 0.08, 0.12, 0.09)
)

# Apply AK method
result <- run_method("AK", data, "default")
#> Warning: NaNs produced
print(result)
#>   method  estimate standard_error   ci_lower  ci_upper    p_value BF
#> 1     AK 0.1239951     0.07280359 -0.1892534 0.4372437 0.09217477 NA
#>   convergence note tau_estimate      tau2_se   bias_coefficient
#> 1        TRUE   NA            0 1.084652e-10 0.0685673164368531
#>   bias_coefficient_se version method_setting
#> 1   0.110251775397694     AK1        default
```
