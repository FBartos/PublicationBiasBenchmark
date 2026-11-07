# Remove Cached Benchmark Archives

Remove ZIP copies whose selected catalog members all have verified
extracted copies. Logical member files and historical catalogs are
retained.

## Usage

``` r
prune_benchmark_archives(release = NULL, dgm_name = NULL, dry_run = TRUE)
```

## Arguments

- release:

  Benchmark release identifier, path to a catalog JSON file, or a
  catalog list. NULL uses the benchmark_release package option.

- dgm_name:

  DGM name (optional when listing resources).

- dry_run:

  Report removable archives without deleting them.

## Value

A data frame with archive IDs, paths, byte sizes and removal status.
