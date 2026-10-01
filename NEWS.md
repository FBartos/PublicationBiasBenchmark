# PublicationBiasBenchmark 0.5.0.9000
## Features
 - Added schema-2 ZIP catalogs with member-first cache reuse, portable extraction checks and archive cache pruning; schema-1 releases remain supported.
 - Added complete download-unit packing, persisted ZIP builds and per-DGM quota reports.
 - Added native storage/catalog versioning, file import and superseded ZIP removal, resumable community inclusion and DOI relationships.
 - Reject frozen-DGM growth and stale results/measures after input corrections; preserve per-worker producing versions.
 - The verified 2026.1 release remains the package default pending production consolidation and approval.

# PublicationBiasBenchmark 0.4.0
## Features
 - Migrated benchmark storage to append-only Zenodo records and complete release catalogs.
 - Added separate downloads by DGM and method, checksum verification, shared caching, and explicit release selection.
 - Added resumable release planning, batch staging, verification, and publication for distributed shards, including Zenodo rate-limit handling.
 - Preserved source provenance and partitioned performance measures by method and setting.
 - Added explicit local result access for unpublished computations.

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
