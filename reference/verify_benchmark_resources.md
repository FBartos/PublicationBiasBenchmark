# Verify Cached Benchmark Resources

Verify Cached Benchmark Resources

## Usage

``` r
verify_benchmark_resources(
  release = NULL,
  dgm_name = NULL,
  kind = NULL,
  method = NULL,
  method_setting = NULL
)
```

## Arguments

- release:

  Benchmark release identifier, path to a catalog JSON file, or a
  catalog list. NULL uses the benchmark_release package option.

- dgm_name:

  DGM name (optional when listing resources).

- kind:

  Optional resource kind: data, results, measures, pairwise, metadata,
  or archive.

- method:

  Optional method name(s).

- method_setting:

  Optional method setting(s).

## Value

A data frame giving the verification status of selected resources.
