# Retrieve Archived Method Results

Read and combine verified shards. Missing shards and overlapping
method/setting/condition/repetition keys are errors. Use source =
"local" for unpublished distributed computation outputs.

## Usage

``` r
retrieve_dgm_results(
  dgm_name,
  method = NULL,
  method_setting = NULL,
  condition_id = NULL,
  repetition_id = NULL,
  release = NULL,
  source = c("release", "local")
)
```

## Arguments

- dgm_name:

  DGM name.

- method:

  Optional method name(s); NULL selects all available methods.

- method_setting:

  Optional method setting(s).

- condition_id:

  Optional condition ID(s) for datasets.

- repetition_id:

  Repetition ID(s); NULL selects all repetitions.

- release:

  Release identifier or catalog; NULL uses the package default.

- source:

  "release" reads verified catalog-selected files; "local" reads
  unpublished computation files in resources_directory/DGM.

## Value

A data frame with the existing method-specific result columns.
