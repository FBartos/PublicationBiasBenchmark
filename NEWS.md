# PublicationBiasBenchmark 0.5.0.9000
## Features
 - Default benchmark release is now [2026.2](https://doi.org/10.5281/zenodo.23121978). It replaces the 11 replacement-measure tables of 2026.1 (AK-AK2, MAN-default and MMPH-default in three DGMs each, RTMA-relaxed in two) whose 8,674 values were NA because of the `safe_rbind()` defect; the other 2,205 assets are identical, and 2026.1 remains available with `release = "2026.1"`.
 - Added schema-2 ZIP catalogs with member-first cache reuse, portable extraction checks and archive cache pruning; schema-1 releases remain supported.
 - Added complete download-unit packing, persisted ZIP builds and per-DGM quota reports.
 - Added native storage/catalog versioning, file import and superseded ZIP removal, resumable community inclusion and DOI relationships.
 - Reject frozen-DGM growth and stale results/measures after input corrections; preserve per-worker producing versions.
 - Consolidated release 2026.1 into five DGM storage records with 266 independent ZIPs, verified the replacement catalog checksum and withdrew the 40 superseded storage records.
 - Release 2026.1 keeps every original OSF file and source hash and recomputes no numerical results. Frozen conditions repair a truncated Bom2019 CSV and misassigned Carter2019 metadata; four WILS example files whose `method_setting` was mislabelled `default` have explicit corrected copies (every numerical token preserved, originals archived). The original mixed-method measure tables stay in the source archive; per-method tables are the reader interface.
 - Clarified ZIP names with dataset condition ranges and original source tables; number all ambiguous split parts consistently from 001.
 - Cache parsed, validated catalogs by their exact SHA-256 while continuing to verify cached bytes before use.
 - Build pull request websites with read-only permissions and publish only from a separate trusted deployment job.
 - Generate the README package citation from CITATION so its authors, year and version stay synchronized.

## Changes
 - `compute_single_measure()`, `compute_measures()`, `compare_single_measure()` and `compare_measures()` read local results by default again (`results_source = "local"`, as before release catalogs); pass `results_source = "release"` to use published results. The new `replacement_source` argument selects where the results of replacement methods come from (default: `results_source`), for example `replacement_source = "release"` to replace a new method's failures with published results of established methods. A `release` is ignored, with a warning, when both sources are local.
 - Publication is maintainer-only. `plan_benchmark_release()`, `stage_benchmark_release()`, `verify_benchmark_release()`, `publish_benchmark_release()`, `benchmark_packing_report()`, `update_benchmark_community_pages()`, `prepare_benchmark_resources()` and `benchmark_resource()` are no longer exported (call them as `PublicationBiasBenchmark:::name()`); no exported function can change anything on Zenodo. The maintainer workflow moved from the vignette to `RELEASING.md` in the GitHub repository.
 - Staging, publishing and community-page updates first check, before any remote write, that the token's account is an owner or manager (owner for community pages) of the community; production is restricted to the PublicationBiasBenchmark community. Publishing needs `confirm = "<release>"`, and `update_benchmark_community_pages()` needs `confirm = "<community>"` and saves a verified backup of the current pages first. The check protects this package's entry points; it is not a security boundary for Zenodo.
 - `update_benchmark_community_pages()` compares the stored About and curation policy text with the submitted HTML (whitespace collapsed) after the update and warns when Zenodo stored different HTML, which it may do when it sanitizes HTML.
 - Publication state is protected: planning, staging and publishing take an exclusive lock on the state directory (a stale `.lock` is reported, never removed automatically); state is bound to its plan by an identity (plans and states written by earlier versions must be re-planned in a new state directory); `state.json`, `release.json`, `registry-entry.json` and `plan.rds` are written verified, the first three with a never-deleted history (`state-history/`, `catalog-history/`); every update re-reads the state, so no update is lost.
 - A publication session refuses a plan whose state directory or archive files no longer exist (move the folder back or re-plan) instead of creating an empty state directory; only planning creates one.
 - The history of `state.json`, `release.json` and `registry-entry.json` keeps installed versions only: a new version is added after it was installed and verified, so a failed replacement leaves nothing in the history (a failing history copy at that point warns and the next write adds the version).
 - A publication session recomputes the plan's identity (release, community, environment, the id, name, hashes and size of every upload entry, the storage base record of each changed DGM and the earlier catalog's schema, record selectors, metadata, limits; identity format version 2) and refuses a plan that was edited after planning; `verify_benchmark_release()` reads the state first and rejects a staged `release.json` of another release, one that differs from the plan's catalog in any condition, asset, archive or member descriptor (record IDs, local paths, build fingerprints and the publication block are excluded), and one whose record IDs do not match the state. A verified write that fails verification restores and re-checks the previous bytes, and reports the file that remains if that fails. The default backup directory of `update_benchmark_community_pages()` is `tools::R_user_dir()` on R 4.0 or later and `~/.PublicationBiasBenchmark` on older R versions.
 - Online deletions go through one guarded helper: only imports provably identical to the published base version are deleted, and every attempt is logged in `deletions.log`. Pending draft files are never deleted automatically: a pending file holding bytes that do not match the planned file stops staging before any file of that record is initialized or uploaded (the draft's metadata may already have been updated and superseded imports removed); the message points to the draft in the Zenodo web interface, where the file can be deleted before re-running; empty pending files are reused and pending files with matching bytes are committed without another upload. Publication states no longer record file initializations (an `initialized` entry in an existing state is ignored).
 - A new-version request that Zenodo refuses with a client error (4xx) is remembered as rejected in the publication state: a draft that appears in the record family afterwards is never adopted, whatever its title, and the run stops for manual reconciliation. After a gateway error (5xx) or no answer the request may have created the draft, so the `creating` marker stays and the draft is adopted when the run resumes.
 - Public verification after publishing checks every storage record anonymously (public records and files, no embargo) and every advertised archive with a one-byte ranged request; only archives uploaded by the plan are downloaded and verified member by member. Not yet visible files are retried five times.
 - Downloads no longer retry permanent errors: HTTP 4xx (except 408, 425 and 429), HTTP 501 and 505, TLS/certificate, protocol and local-write errors stop at once; DNS failures stop after three attempts; a missing or withdrawn file reports that the release may have been withdrawn or the catalog is outdated. Rate limits wait for the server delay and other failures back off.
 - Catalogs and publication state written by this version are LF-terminated JSON on every platform (release 2026.1 is untouched). Finish an in-flight publication with the package version that started it: this version refuses plans and states written by earlier versions (re-plan in a new state directory), unpublished drafts of an interrupted earlier-version run can be discarded in the Zenodo web interface before re-planning, and already published storage versions cannot be changed (a run interrupted after storage publication needs manual reconciliation). The catalog file is uploaded only when publishing, after the storage versions were published, so a `release.json` (CRLF when written on Windows) that an interrupted earlier-version publish left in the catalog draft is that case: finish that release with the earlier version. See `RELEASING.md`.
 - The direct-file (schema 1) publisher was removed; releases are always published as ZIP archives (schema 2). Schema-1 catalogs remain readable.
 - Prepared measures use the logical IDs `<dgm>/measures/<label>` of release 2026.1 (no `.csv`) with member files named `<dgm>__measures__<label>.csv`; method/setting pairs that spell the same label are rejected.
 - Cached archives and members are verified by size and SHA-256 once per download call; archive members are staged in the resource cache and installed by renaming, so a failed installation leaves the previous copy in place.
 - The results articles respect a configured resources directory (option or `PublicationBiasBenchmark_RESOURCES`), resolve one benchmark release for all downloads and conditions, and print the release, its DOI and catalog SHA-256 in the session information.

## Fixes
 - Replacement measures computed with earlier versions could be `NA` (`n_valid` 0) in conditions where the replaced method had no valid runs; the internal `safe_rbind()` discarded the replacement rows in that case and is fixed.
 - `compute_single_measure()` creates the DGM's `measures` folder when it does not exist.
 - Pairwise comparisons (`compare_single_measure()`, `compare_measures()`) kept only the last condition of every method pair, and fresh runs computed both orientations of each pair; they now give one row per unordered method pair and condition, as runs that add methods to an existing file already did. Recompute existing pairwise files with `overwrite = TRUE`.
 - Methods whose names or settings contain separators such as `.` or `/` are no longer merged in coverage checks, release planning and measure preparation; unknown condition IDs are rejected before any shard is read.
 - The release catalog family sentence of a storage record's description is added once, however often a publication is resumed.
 - Catalog validation and `list_benchmark_resources()` use indexed lookups instead of repeated scans.
 - `prepare_benchmark_resources()` verifies an existing hash-named prepared measures file against its name before reusing it and replaces a corrupted one (with a message) instead of describing its bytes as the measures.

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
