# PublicationBiasBenchmark 0.5.0.9000
## Features
 - Added schema-2 ZIP catalogs with member-first cache reuse, portable extraction checks and archive cache pruning; schema-1 releases remain supported.
 - Added complete download-unit packing, persisted ZIP builds and per-DGM quota reports.
 - Added native storage/catalog versioning, file import and superseded ZIP removal, resumable community inclusion and DOI relationships.
 - Reject frozen-DGM growth and stale results/measures after input corrections; preserve per-worker producing versions.
 - Consolidated release 2026.1 into five DGM storage records with 266 independent ZIPs, verified the replacement catalog checksum and withdrew the 40 superseded storage records.
 - Clarified ZIP names with dataset condition ranges and original source tables; number all ambiguous split parts consistently from 001.
 - Cache parsed, validated catalogs by their exact SHA-256 while continuing to verify cached bytes before use.
 - Build pull request websites with read-only permissions and publish only from a separate trusted deployment job.
 - Generate the README package citation from CITATION so its authors, year and version stay synchronized.

## Changes
 - `compute_single_measure()`, `compute_measures()`, `compare_single_measure()` and `compare_measures()` read local results by default again (`results_source = "local"`, as before release catalogs); pass `results_source = "release"` to use published results. The new `replacement_source` argument selects where the results of replacement methods come from (default: `results_source`), for example `replacement_source = "release"` to replace a new method's failures with published results of established methods. A `release` is ignored, with a warning, when both sources are local.
 - Publication is maintainer-only. `plan_benchmark_release()`, `stage_benchmark_release()`, `verify_benchmark_release()`, `publish_benchmark_release()`, `benchmark_packing_report()`, `update_benchmark_community_pages()`, `prepare_benchmark_resources()` and `benchmark_resource()` are no longer exported (call them as `PublicationBiasBenchmark:::name()`); no exported function can change anything on Zenodo. The maintainer workflow moved from the vignette to `RELEASING.md` in the GitHub repository.
 - Staging, publishing and community-page updates first check, before any remote write, that the token's account is an owner or manager (owner for community pages) of the community; production is restricted to the PublicationBiasBenchmark community. Publishing needs `confirm = "<release>"`, and `update_benchmark_community_pages()` needs `confirm = "<community>"` and saves a verified backup of the current pages first. The check protects this package's entry points; it is not a security boundary for Zenodo.
 - Publication state is protected: planning, staging and publishing take an exclusive lock on the state directory (a stale `.lock` is reported, never removed automatically); state is bound to its plan by an identity (plans and states written by earlier versions must be re-planned in a new state directory); `state.json`, `release.json`, `registry-entry.json` and `plan.rds` are written verified, the first three with a never-deleted history (`state-history/`, `catalog-history/`); every update re-reads the state, so no update is lost.
 - A publication session recomputes the plan's identity (release, community, environment, the id, name, hashes and size of every upload entry, the storage base record of each changed DGM and the earlier catalog's schema, record selectors, metadata, limits; identity format version 2) and refuses a plan that was edited after planning; `verify_benchmark_release()` reads the state first and rejects a staged `release.json` of another release or with a different archive or member descriptor (id, name, size, SHA-256, MD5). A verified write that fails verification restores and re-checks the previous bytes, and reports the file that remains if that fails. The default backup directory of `update_benchmark_community_pages()` is `tools::R_user_dir()` on R 4.0 or later and `~/.PublicationBiasBenchmark` on older R versions.
 - Online deletions go through one guarded helper: only pending draft files that the publication state created and imports provably identical to the published base version are deleted, and every attempt is logged in `deletions.log`. A pending file holding data that cannot be attributed stops the run instead of being deleted.
 - Public verification after publishing checks every storage record anonymously (public records and files, no embargo) and every advertised archive with a one-byte ranged request; only archives uploaded by the plan are downloaded and verified member by member. Not yet visible files are retried five times.
 - Downloads no longer retry permanent errors: HTTP 4xx (except 408, 425 and 429), HTTP 501 and 505, TLS/certificate, protocol and local-write errors stop at once; DNS failures stop after three attempts; a missing or withdrawn file reports that the release may have been withdrawn or the catalog is outdated. Rate limits wait for the server delay and other failures back off.
 - Catalogs and publication state written by this version are LF-terminated JSON on every platform (release 2026.1 is untouched). Finish an in-flight publication with the package version that started it: this version refuses plans and states written by earlier versions (re-plan in a new state directory), unpublished drafts of an interrupted earlier-version run can be discarded in the Zenodo web interface before re-planning, and already published storage versions cannot be changed (a run interrupted after storage publication needs manual reconciliation). A CRLF `release.json` that an interrupted earlier-version publish uploaded to the catalog record's draft makes publishing stop at "Existing draft file differs"; remove it from the draft in the Zenodo web interface (the package never deletes completed files), then stage, verify and publish again. See `RELEASING.md`.
 - Legacy plans (`archive = FALSE`) now reject added condition rows for a published DGM, like archive plans; frozen conditions are checked once for both plan schemas, and legacy plans store the `community` they publish to.
 - Prepared measures use the logical IDs `<dgm>/measures/<label>` of release 2026.1 (no `.csv`) with member files named `<dgm>__measures__<label>.csv`; method/setting pairs that spell the same label are rejected.
 - Cached archives and members are verified by size and SHA-256 once per download call; archive members are staged in the resource cache and installed by renaming, so a failed installation leaves the previous copy in place.

## Fixes
 - Replacement measures computed with earlier versions could be `NA` (`n_valid` 0) in conditions where the replaced method had no valid runs; the internal `safe_rbind()` discarded the replacement rows in that case and is fixed.
 - `compute_single_measure()` creates the DGM's `measures` folder when it does not exist.
 - Pairwise comparisons (`compare_single_measure()`, `compare_measures()`) kept only the last condition of every method pair, and fresh runs computed both orientations of each pair; they now give one row per unordered method pair and condition, as runs that add methods to an existing file already did. Recompute existing pairwise files with `overwrite = TRUE`.
 - Methods whose names or settings contain separators such as `.` or `/` are no longer merged in coverage checks, release planning and measure preparation; unknown condition IDs are rejected before any shard is read.
 - The release catalog family sentence of a storage record's description is added once, however often a publication is resumed.
 - Catalog validation and `list_benchmark_resources()` use indexed lookups instead of repeated scans.

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
