# Retrieve Pre-Computed Performance measures for a DGM

This function returns pre-computed performance measures for a specified
Data-Generating Mechanism (DGM). The pre-computed measures must be
already stored locally. See
[`download_dgm_measures()`](https://fbartos.github.io/PublicationBiasBenchmark/reference/download_dgm.md)
function for more guidance.

## Usage

``` r
retrieve_dgm_measures(
  dgm_name,
  measure = NULL,
  method = NULL,
  method_setting = NULL,
  condition_id = NULL,
  replacement = FALSE
)
```

## Arguments

- dgm_name:

  Character string specifying the DGM type

- measure:

  Which performance measure should be returned (e.g., "bias", "mse",
  "coverage"). All measures can be returned by setting to `NULL`.

- method:

  Which method(s) should be returned. The complete results are returned
  by setting to `NULL` (default setting).

- method_setting:

  Which method setting(s) should be returned. The complete results are
  returned by setting to `NULL` (default setting).

- condition_id:

  which conditions should settings be returned for.

- replacement:

  Whether performance measures computed using replacement should be
  returned. Defaults to `FALSE`.

## Value

A data.frame

## Examples

``` r
# \donttest{
  # get bias measures for all methods and conditions
  retrieve_dgm_measures("no_bias", measure = "bias")
#> Error: The resources location needs to be specified via the `PublicationBiasBenchmark.options(resources_directory = '/path/')` function.

  # get all measures for RMA method
  retrieve_dgm_measures("no_bias", method = "RMA")
#> Error: The resources location needs to be specified via the `PublicationBiasBenchmark.options(resources_directory = '/path/')` function.

  # get MSE measures for PET method in condition 1
  retrieve_dgm_measures("no_bias", measure = "mse", method = "PET", condition_id = 1)
#> Error: The resources location needs to be specified via the `PublicationBiasBenchmark.options(resources_directory = '/path/')` function.
# }
```
