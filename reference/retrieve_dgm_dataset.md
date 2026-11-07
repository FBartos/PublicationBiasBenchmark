# Retrieve Archived DGM Datasets

Read locally cached dataset shards using frozen condition definitions.

## Usage

``` r
retrieve_dgm_dataset(
  dgm_name,
  condition_id,
  repetition_id = NULL,
  release = NULL,
  source = c("release", "local")
)
```

## Arguments

- dgm_name:

  DGM name.

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

A data frame.
