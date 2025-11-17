# puniform (P-Uniform) Method

Implements the p-uniform method for publication bias detection and
correction. P-uniform uses the distribution of p-values from significant
studies to test for publication bias and estimate the effect size
corrected for publication bias. The method assumes that p-values follow
a uniform distribution under the null hypothesis of no effect, and uses
this to detect and correct for bias. See van Assen et al. (2015) and van
Aert and van Assen (2025) for details.

## Usage

``` r
# S3 method for class 'puniform'
method(method_name, data, settings)
```

## Arguments

- method_name:

  Method name (automatically passed)

- data:

  Data frame with yi (effect sizes) and sei (standard errors)

- settings:

  List of method settings (see Details)

## Value

Data frame with P-Uniform results

## Details

The following settings are implemented

- `"default"`:

  Default p-uniform analysis settings.

- `"star"`:

  P-uniform star version of the method.

## References

van Aert RCM, van Assen MALM (2025). “Correcting for publication bias in
a meta-analysis with the p-uniform\* method.” *Psychonomic Bulletin &
Review*. <https://osf.io/preprints/metaarxiv/zqjr9/>.  
  
van Assen MALM, van Aert RCM, Wicherts JM (2015). “Meta-analysis using
effect size distributions of only statistically significant studies.”
*Psychological Methods*, **20**(3), 293–309.
[doi:10.1037/met0000025](https://doi.org/10.1037/met0000025) .

## Examples

``` r
# Generate some example data
data <- data.frame(
  yi = c(0.2, 0.3, 0.1, 0.4, 0.25),
  sei = c(0.1, 0.15, 0.08, 0.12, 0.09)
)

# Apply puniform method
result <- run_method("puniform", data)
print(result)
#>     method   estimate standard_error  ci_lower  ci_upper   p_value BF
#> 1 puniform 0.03038879             NA -2.250357 0.3192963 0.4631793 NA
#>   convergence note  version tau_estimate tau_ci_lower tau_ci_upper tau_p_value
#> 1        TRUE   NA original           NA           NA           NA          NA
#>   bias_p_value method_setting
#> 1    0.1467798        default
```
