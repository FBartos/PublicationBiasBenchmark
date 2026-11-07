# Retrieve a Pre-Simulated Condition and Repetition From a DGM

This function returns a pre-simulated dataset of a given repetition and
condition from a dgm. The pre-simulated datasets must be already stored
locally. See
[download_dgm](https://fbartos.github.io/PublicationBiasBenchmark/reference/download_dgm.md)
function for more guidance.

## Usage

``` r
retrieve_dgm_dataset(dgm_name, condition_id, repetition_id = NULL)
```

## Arguments

- dgm_name:

  Character string specifying the DGM type

- condition_id:

  which conditions should settings be returned for.

- repetition_id:

  Which repetition should be returned. The complete condition can be
  returned by setting to either `NULL`.

## Value

A data.frame

## Examples

``` r
if (FALSE) { # \dontrun{
  # get condition 1, repetition 1
  retrieve_dgm_dataset("no_bias", condition_id = 1, repetition_id = 1)

  # get condition 1, all repetitions
  retrieve_dgm_dataset("no_bias", condition_id = 1)
} # }

```
