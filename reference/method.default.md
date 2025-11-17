# Default method handler

Default method handler

## Usage

``` r
# Default S3 method
method(method_name, data, settings = list())
```

## Arguments

- method_name:

  Character string specifying the method type

- data:

  Data frame containing yi (effect sizes) and sei (standard errors)

- settings:

  Either a character identifying a method version or list containing
  method-specific settings. An emty input will result in running the
  default (first implemented) version of the method.

## Value

Throws an error indicating the method type is unknown. This default
method is only called when no specific method implementation is found
for the given `method_name`.
