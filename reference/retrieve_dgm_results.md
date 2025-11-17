# Retrieve a Pre-Computed Results of a Method Applied to DGM

This function returns a pre-computed results of a given method at a
specific repetition and condition from a dgm. The pre-computed results
must be already stored locally. See
[`download_dgm_results()`](https://fbartos.github.io/PublicationBiasBenchmark/reference/download_dgm.md)
function for more guidance.

## Usage

``` r
retrieve_dgm_results(
  dgm_name,
  method = NULL,
  method_setting = NULL,
  condition_id = NULL,
  repetition_id = NULL
)
```

## Arguments

- dgm_name:

  Character string specifying the DGM type

- method:

  Which method(s) should be returned. The complete results are returned
  by setting to `NULL` (default setting).

- method_setting:

  Which method setting(s) should be returned. The complete results are
  returned by setting to `NULL` (default setting).

- condition_id:

  which conditions should settings be returned for.

- repetition_id:

  Which repetition should be returned. The complete condition can be
  returned by setting to either `NULL`.

## Value

A data.frame

## Examples

``` r
# \donttest{
  # get condition 1, repetition 1 for default method setting
  retrieve_dgm_results("no_bias", condition_id = 1, repetition_id = 1)
#> Error: The resources location needs to be specified via the `PublicationBiasBenchmark.options(resources_directory = '/path/')` function.

  # get condition 1, all repetitions for default method setting
  retrieve_dgm_results("no_bias", condition_id = 1)
#> Error: The resources location needs to be specified via the `PublicationBiasBenchmark.options(resources_directory = '/path/')` function.
# }

```
