# Mean Method

Implements the unweighted mean method. I.e., the mean of observed effect
sizes.

## Usage

``` r
# S3 method for class 'mean'
method(method_name, data, settings)
```

## Arguments

- method_name:

  Method name (automatically passed)

- data:

  Data frame with yi (effect sizes)

- settings:

  List of method settings (see Details)

## Value

Data frame with mean results

## Details

The following settings are implemented

- `"default"`:

  No settings

## References

There are no references for Rd macro `\insertAllCites` on this help
page.

## Author

František Bartoš <f.bartos96@gmail.com>

## Examples

``` r
# Generate some example data
data <- data.frame(
  yi = c(0.2, 0.3, 0.1, 0.4, 0.25),
  sei = c(0.1, 0.15, 0.08, 0.12, 0.09)
)

# Apply mean method
result <- run_method("mean", data)
print(result)
#>   method estimate standard_error ci_lower ci_upper      p_value BF convergence
#> 1   mean     0.25     0.04955805 0.152868 0.347132 4.544962e-07 NA        TRUE
#>   note method_setting
#> 1   NA        default
```
