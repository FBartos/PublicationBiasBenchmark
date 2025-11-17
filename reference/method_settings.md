# Return Pre-specified Method Settings

This function returns the list of pre-specified settings for a given
Method

## Usage

``` r
method_settings(method_name)

get_method_setting(method_name, version_id)
```

## Arguments

- method_name:

  Character string specifying the method type

- version_id:

  which method version should be used.

## Value

A list containing the pre-specified settings. For most methods, the list
contains extension of the function call, however, a more elaborate list
of settings that is dispatched within the method call is possible.

## Examples

``` r
method_settings("RMA")
#> $default
#> $default$method
#> [1] "REML"
#> 
#> $default$test.uni
#> [1] "knha"
#> 
#> $default$test.mv
#> [1] "t"
#> 
#> $default$control
#> $default$control$stepadj
#> [1] 0.5
#> 
#> $default$control$maxiter
#> [1] 500
#> 
#> 
#> 
get_method_setting("RMA", version_id = "default")
#> $method
#> [1] "REML"
#> 
#> $test.uni
#> [1] "knha"
#> 
#> $test.mv
#> [1] "t"
#> 
#> $control
#> $control$stepadj
#> [1] 0.5
#> 
#> $control$maxiter
#> [1] 500
#> 
#> 
```
