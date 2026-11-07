# Download Benchmark Datasets, Results, and Measures

Download immutable catalog-selected direct files or ZIP units. Archives
and extracted members are checked against size, SHA-256 and MD5. A
condition/metric filter may fetch other members of its selected unit.
Public downloads require no token; verified member caches avoid new
downloads.

## Usage

``` r
download_dgm_datasets(
  dgm_name,
  overwrite = FALSE,
  progress = TRUE,
  max_try = 10,
  release = NULL,
  condition_id = NULL
)

download_dgm_results(
  dgm_name,
  overwrite = FALSE,
  progress = TRUE,
  max_try = 10,
  method = NULL,
  method_setting = NULL,
  release = NULL
)

download_dgm_measures(
  dgm_name,
  overwrite = FALSE,
  progress = TRUE,
  max_try = 10,
  method = NULL,
  method_setting = NULL,
  release = NULL,
  measure = NULL,
  replacement = NULL
)

download_dgm_metadata(
  dgm_name,
  overwrite = FALSE,
  progress = TRUE,
  max_try = 10,
  release = NULL
)
```

## Arguments

- dgm_name:

  DGM name.

- overwrite:

  Re-download selected files even if their cached copies verify.

- progress:

  Display download progress.

- max_try:

  Maximum attempts per file.

- release:

  Release identifier or catalog; NULL uses the package default.

- condition_id:

  Optional condition ID(s) for datasets.

- method:

  Optional method name(s); NULL selects all available methods.

- method_setting:

  Optional method setting(s).

- measure:

  Optional measure name(s); pairwise comparisons require "pairwise".

- replacement:

  Whether replacement measures are selected; NULL downloads both
  variants.

## Value

Invisible TRUE on success or FALSE when an interactive download is
declined.
