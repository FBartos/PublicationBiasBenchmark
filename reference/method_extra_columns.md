# Method Extra Columns

Retrieves the character vector of custom columns for a given method.
These are method-specific columns beyond the standard columns (method,
estimate, standard_error, ci_lower, ci_upper, p_value, BF, convergence,
note) that each method returns.

When implementing new methods, consider using standardized column names
for consistency:

- Heterogeneity:

  `tau_estimate`, `tau_ci_lower`, `tau_ci_upper`, `tau_p_value`,
  `tau_BF`

- Publication Bias:

  `bias_coefficient`, `bias_coefficient_se`, `bias_p_value`, `bias_BF`

## Usage

``` r
get_method_extra_columns(method_name)

method_extra_columns(method_name)

# Default S3 method
method_extra_columns(method_name)
```

## Arguments

- method_name:

  Character string of the method name

## Value

Character vector of extra column names, or empty character vector if no
extra columns are defined for the method

## Examples

``` r
# Get extra columns for PET method
get_method_extra_columns("PET")
#> [1] "bias_coefficient" "bias_p_value"    

# Get extra columns for RMA method
get_method_extra_columns("RMA")
#> [1] "tau_estimate" "tau_ci_lower" "tau_ci_upper" "tau_p_value" 
```
