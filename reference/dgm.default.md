# Default DGM handler

Default DGM handler

## Usage

``` r
# Default S3 method
dgm(dgm_name, settings)
```

## Arguments

- dgm_name:

  Character string specifying the DGM type

- settings:

  List containing the required parameters for the DGM or numeric
  condition_id

## Value

Throws an error indicating the DGM type is unknown. This default method
is only called when no specific DGM implementation is found for the
given `dgm_name`.
