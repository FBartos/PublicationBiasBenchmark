# Get performance measure function

Returns the function for computing a specific performance measure. Can
also be used to list available measures.

## Usage

``` r
measure(measure, ...)
```

## Arguments

- measure:

  Character string specifying the measure name. If missing, returns a
  vector of all available measures.

- ...:

  Additional arguments passed to methods.

## Value

A function that computes the requested measure, or a character vector of
measure names.
