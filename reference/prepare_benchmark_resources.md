# Prepare Local Computation Outputs for Publication

Keep distributed result shards separate and partition ordinary
performance measures by method and setting. Original local files are
retained. Pairwise comparison files are excluded; they are not ordinary
method measures.

## Usage

``` r
prepare_benchmark_resources(
  dgm_name,
  kinds = c("results", "measures"),
  output_directory,
  package_version = NULL,
  method = NULL,
  method_setting = NULL
)
```

## Arguments

- dgm_name:

  DGM name.

- kinds:

  Local kinds to prepare: results, measures, data, or metadata.

- output_directory:

  Directory for prepared immutable measure files.

- package_version:

  Producing package version, or NULL when unknown. Supply this per
  worker batch; it is not inferred from the current package version.

- method:

  Optional method names to include.

- method_setting:

  Optional method settings to include.

## Value

List of file descriptors for plan_benchmark_release.
