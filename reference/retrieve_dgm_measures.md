# Retrieve Archived Performance Measures

Read measures stored separately for each DGM, method and setting.
Pairwise comparisons are only read when measure = "pairwise" is
requested.

## Usage

``` r
retrieve_dgm_measures(
  dgm_name,
  measure = NULL,
  method = NULL,
  method_setting = NULL,
  condition_id = NULL,
  replacement = FALSE,
  release = NULL,
  source = c("release", "local")
)
```

## Arguments

- dgm_name:

  DGM name.

- measure:

  Optional measure name(s); pairwise comparisons require "pairwise".

- method:

  Optional method name(s); NULL selects all available methods.

- method_setting:

  Optional method setting(s).

- condition_id:

  Optional condition ID(s) for datasets.

- replacement:

  Whether replacement measures are selected; NULL downloads both
  variants.

- release:

  Release identifier or catalog; NULL uses the package default.

- source:

  "release" reads verified catalog-selected files; "local" reads
  unpublished computation files in resources_directory/DGM.

## Value

A data frame of selected measures and Monte Carlo standard errors.
