# Describe a File for a Benchmark Release

Files remain separate for distributed jobs. Each results or measures
file must contain only its declared DGM, method and setting. Checksums
describe the exact bytes; package_version describes generation, not
migration.

## Usage

``` r
benchmark_resource(
  path,
  dgm_name,
  kind,
  method = NULL,
  method_setting = NULL,
  condition_ids = NULL,
  package_version = NULL,
  replacement = FALSE,
  measures = NULL,
  id = NULL,
  dependencies = NULL
)
```

## Arguments

- path:

  Local file path.

- dgm_name:

  DGM name.

- kind:

  data, results, measures, pairwise, metadata, or archive.

- method:

  Method identifier for results/measures.

- method_setting:

  Method setting identifier.

- condition_ids:

  Conditions covered by the file.

- package_version:

  Producing package version; NULL when historically unknown.

- replacement:

  Whether the file contains replacement measures.

- measures:

  Measures present in a wide measures file.

- id:

  Stable logical shard identifier; defaults to DGM/kind/basename.

- dependencies:

  Optional IDs of input assets used to compute this asset.

## Value

A file descriptor for plan_benchmark_release.
