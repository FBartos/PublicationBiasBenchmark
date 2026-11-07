# PublicationBiasBenchmark 0.5.0.9000
## Features
 - The benchmark data moved from OSF to [Zenodo](https://zenodo.org/communities/publicationbiasbenchmark/) and are published in versioned releases. Each package version uses a default release; `list_benchmark_releases()` lists the available releases and the `release` argument of the download and retrieve functions selects one. See the new "Releases" article.
 - The default release is [2026.2](https://doi.org/10.5281/zenodo.23121978). It corrects 11 replacement-measure tables of [2026.1](https://doi.org/10.5281/zenodo.23070787) (AK, MAN, MMPH and RTMA) whose values were missing because of the `safe_rbind()` bug fixed below; all other files are identical, and 2026.1 remains available.
 - Release 2026.1 contains every original OSF file and recomputes no results. Its condition tables repair a truncated Bom2019 file and misassigned Carter2019 metadata, and four WILS example files whose `method_setting` was mislabelled `default` have corrected copies (the originals are kept).
 - Results and measures can be downloaded for a single method (`method = "RMA"`). Downloads are verified against checksums and cached; files shared by several releases are downloaded only once, and `prune_benchmark_archives()` frees disk space.
 - New functions `benchmark_conditions()`, `benchmark_catalog()`, `list_benchmark_releases()`, `list_benchmark_resources()`, `verify_benchmark_resources()`, `download_dgm_metadata()` and `prune_benchmark_archives()`.
 - `retrieve_dgm_dataset()`, `retrieve_dgm_results()` and `retrieve_dgm_measures()` read your own unpublished files with `source = "local"`.
 - `compute_measures()`, `compare_measures()` and their single-measure versions gain `results_source`, `replacement_source` and `release`: your method's results are read locally by default, and `replacement_source = "release"` takes the results of the replacement methods from the published benchmark.

## Changes
 - Publishing releases is maintainer-only: the publication functions are no longer exported (see `.github/RELEASING.md` in the GitHub repository).
 - Downloads no longer retry errors that cannot succeed, such as a missing file.

## Fixes
 - Replacement measures could be `NA` in conditions where the replaced method had no valid runs, because the internal `safe_rbind()` dropped the replacement rows.
 - Pairwise comparisons (`compare_single_measure()`, `compare_measures()`) kept only the last condition of every method pair; they now give one row per method pair and condition. Recompute existing pairwise files with `overwrite = TRUE`.
 - `compute_single_measure()` creates the DGM's `measures` folder when it does not exist.
 - Methods whose names or settings contain separators such as `.` or `/` are no longer merged.

# PublicationBiasBenchmark 0.3.0
## Features
 - Added RTMA method
 - Added MAN method
 - Added MMPH method 
 - Added `fit_limit` argument to `run_method()` that aborts a fit exceeding the
   given number of minutes and returns a standard failure result with
   `note = "time limit exceeded with <fit_limit> minutes"`. The fit runs in a
   reused background R process so that methods sitting in compiled sampling
   code (RoBMA, RTMA, MMPH) can be stopped as well.
 
# PublicationBiasBenchmark 0.2.1
## Fixes
 - Fix RoBMA and BayesTools version

# PublicationBiasBenchmark 0.2.0
## Features
 - Added MAIVE method (by Petr Čala)

# PublicationBiasBenchmark 0.1.3
## Features
 - Added `measure()` function to list available performance measures (renamed from `measures()`).
 - Added `measure_mcse()` function to list available performance measure MCSE functions.
 - Implemented S3 methods for `measure()` and `measure_mcse()` to retrieve specific functions (e.g., `measure("bias")`, `measure_mcse("bias")`).
 - Updated `method()` and `dgm()` to list available methods and DGMs when called without arguments.
 - Updated `method()` and `dgm()` to return the corresponding function when called with a single argument (e.g., `method("RMA")`).
 - `measure()`, `measure_mcse()`, `method()`, and `dgm()` now dynamically retrieve available options using `methods()`.

# PublicationBiasBenchmark 0.1.2
## Fixes
 - Vignette updates
 - Stop download if OSF_PAT is missing (due to errors in the osfr package)
 
# PublicationBiasBenchmark 0.1.1
## Fixes
 - Documentation updates

# PublicationBiasBenchmark 0.1.0
Initial CRAN submission.
