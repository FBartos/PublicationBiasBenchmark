# Changelog

## PublicationBiasBenchmark 0.4.0

### Features

- The benchmark data moved from OSF to
  [Zenodo](https://zenodo.org/communities/publicationbiasbenchmark/) and
  are published in versioned releases. Each package version uses a
  default release;
  [`list_benchmark_releases()`](https://fbartos.github.io/PublicationBiasBenchmark/reference/benchmark_catalog.md)
  lists the available releases and the `release` argument of the
  download and retrieve functions selects one. See the new “Releases”
  article.
- The default release is
  [2026.2](https://doi.org/10.5281/zenodo.23121978). It corrects 11
  replacement-measure tables of
  [2026.1](https://doi.org/10.5281/zenodo.23070787) (AK, MAN, MMPH and
  RTMA) whose values were missing because of the `safe_rbind()` bug
  fixed below; all other files are identical, and 2026.1 remains
  available.
- Release 2026.1 contains every original OSF file and recomputes no
  results. Its condition tables repair a truncated Bom2019 file and
  misassigned Carter2019 metadata, and four WILS example files whose
  `method_setting` was mislabelled `default` have corrected copies (the
  originals are kept).
- Results and measures can be downloaded for a single method
  (`method = "RMA"`). Downloads are verified against checksums and
  cached; files shared by several releases are downloaded only once, and
  [`prune_benchmark_archives()`](https://fbartos.github.io/PublicationBiasBenchmark/reference/prune_benchmark_archives.md)
  frees disk space.
- New functions
  [`benchmark_conditions()`](https://fbartos.github.io/PublicationBiasBenchmark/reference/benchmark_catalog.md),
  [`benchmark_catalog()`](https://fbartos.github.io/PublicationBiasBenchmark/reference/benchmark_catalog.md),
  [`list_benchmark_releases()`](https://fbartos.github.io/PublicationBiasBenchmark/reference/benchmark_catalog.md),
  [`list_benchmark_resources()`](https://fbartos.github.io/PublicationBiasBenchmark/reference/benchmark_catalog.md),
  [`verify_benchmark_resources()`](https://fbartos.github.io/PublicationBiasBenchmark/reference/verify_benchmark_resources.md),
  [`download_dgm_metadata()`](https://fbartos.github.io/PublicationBiasBenchmark/reference/download_dgm.md)
  and
  [`prune_benchmark_archives()`](https://fbartos.github.io/PublicationBiasBenchmark/reference/prune_benchmark_archives.md).
- [`retrieve_dgm_dataset()`](https://fbartos.github.io/PublicationBiasBenchmark/reference/retrieve_dgm_dataset.md),
  [`retrieve_dgm_results()`](https://fbartos.github.io/PublicationBiasBenchmark/reference/retrieve_dgm_results.md)
  and
  [`retrieve_dgm_measures()`](https://fbartos.github.io/PublicationBiasBenchmark/reference/retrieve_dgm_measures.md)
  read your own unpublished files with `source = "local"`.
- [`compute_measures()`](https://fbartos.github.io/PublicationBiasBenchmark/reference/compute_measures.md),
  [`compare_measures()`](https://fbartos.github.io/PublicationBiasBenchmark/reference/compare_measures.md)
  and their single-measure versions gain `results_source`,
  `replacement_source` and `release`: your method’s results are read
  locally by default, and `replacement_source = "release"` takes the
  results of the replacement methods from the published benchmark.

### Changes

- Publishing releases is maintainer-only: the publication functions are
  no longer exported (see `.github/RELEASING.md` in the GitHub
  repository).
- Downloads no longer retry errors that cannot succeed, such as a
  missing file.

### Fixes

- Replacement measures could be `NA` in conditions where the replaced
  method had no valid runs, because the internal `safe_rbind()` dropped
  the replacement rows.
- Pairwise comparisons
  ([`compare_single_measure()`](https://fbartos.github.io/PublicationBiasBenchmark/reference/compare_single_measure.md),
  [`compare_measures()`](https://fbartos.github.io/PublicationBiasBenchmark/reference/compare_measures.md))
  kept only the last condition of every method pair; they now give one
  row per method pair and condition. Recompute existing pairwise files
  with `overwrite = TRUE`.
- [`compute_single_measure()`](https://fbartos.github.io/PublicationBiasBenchmark/reference/compute_single_measure.md)
  creates the DGM’s `measures` folder when it does not exist.
- Methods whose names or settings contain separators such as `.` or `/`
  are no longer merged.

## PublicationBiasBenchmark 0.3.0

CRAN release: 2026-09-30

### Features

- Added RTMA method
- Added MAN method
- Added MMPH method
- Added `fit_limit` argument to
  [`run_method()`](https://fbartos.github.io/PublicationBiasBenchmark/reference/run_method.md)
  that aborts a fit exceeding the given number of minutes and returns a
  standard failure result with
  `note = "time limit exceeded with <fit_limit> minutes"`. The fit runs
  in a reused background R process so that methods sitting in compiled
  sampling code (RoBMA, RTMA, MMPH) can be stopped as well.

## PublicationBiasBenchmark 0.2.1

CRAN release: 2026-05-23

### Fixes

- Fix RoBMA and BayesTools version

## PublicationBiasBenchmark 0.2.0

CRAN release: 2026-03-16

### Features

- Added MAIVE method (by Petr Čala)

## PublicationBiasBenchmark 0.1.3

CRAN release: 2025-12-05

### Features

- Added
  [`measure()`](https://fbartos.github.io/PublicationBiasBenchmark/reference/measure.md)
  function to list available performance measures (renamed from
  [`measures()`](https://fbartos.github.io/PublicationBiasBenchmark/reference/measures.md)).
- Added
  [`measure_mcse()`](https://fbartos.github.io/PublicationBiasBenchmark/reference/measure_mcse.md)
  function to list available performance measure MCSE functions.
- Implemented S3 methods for
  [`measure()`](https://fbartos.github.io/PublicationBiasBenchmark/reference/measure.md)
  and
  [`measure_mcse()`](https://fbartos.github.io/PublicationBiasBenchmark/reference/measure_mcse.md)
  to retrieve specific functions (e.g., `measure("bias")`,
  `measure_mcse("bias")`).
- Updated
  [`method()`](https://fbartos.github.io/PublicationBiasBenchmark/reference/method.md)
  and
  [`dgm()`](https://fbartos.github.io/PublicationBiasBenchmark/reference/dgm.md)
  to list available methods and DGMs when called without arguments.
- Updated
  [`method()`](https://fbartos.github.io/PublicationBiasBenchmark/reference/method.md)
  and
  [`dgm()`](https://fbartos.github.io/PublicationBiasBenchmark/reference/dgm.md)
  to return the corresponding function when called with a single
  argument (e.g., `method("RMA")`).
- [`measure()`](https://fbartos.github.io/PublicationBiasBenchmark/reference/measure.md),
  [`measure_mcse()`](https://fbartos.github.io/PublicationBiasBenchmark/reference/measure_mcse.md),
  [`method()`](https://fbartos.github.io/PublicationBiasBenchmark/reference/method.md),
  and
  [`dgm()`](https://fbartos.github.io/PublicationBiasBenchmark/reference/dgm.md)
  now dynamically retrieve available options using
  [`methods()`](https://rdrr.io/r/utils/methods.html).

## PublicationBiasBenchmark 0.1.2

CRAN release: 2025-11-26

### Fixes

- Vignette updates
- Stop download if OSF_PAT is missing (due to errors in the osfr
  package)

## PublicationBiasBenchmark 0.1.1

CRAN release: 2025-11-17

### Fixes

- Documentation updates

## PublicationBiasBenchmark 0.1.0

CRAN release: 2025-11-05

Initial CRAN submission.
