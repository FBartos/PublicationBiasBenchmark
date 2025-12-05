# Get performance measure MCSE function

Returns the function for computing the Monte Carlo Standard Error (MCSE)
of a specific performance measure. Can also be used to list available
measures with MCSE functions.

## Usage

``` r
measure_mcse(measure, ...)
```

## Arguments

- measure:

  Character string specifying the measure name. If missing, returns a
  vector of all available measures.

- ...:

  Additional arguments passed to methods.

## Value

A function that computes the MCSE of the requested measure, or a
character vector of measure names.
