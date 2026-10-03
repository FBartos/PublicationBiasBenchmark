# Releasing benchmark data

This file is for the package maintainers. Readers of the benchmark only need the
user vignette "Benchmark Releases"; nothing here is part of the installed package.

Publication functions are **internal**: they are not exported, so no exported
function can create, change, delete or publish anything on Zenodo. Call them with
the `PublicationBiasBenchmark:::` prefix, from a checkout or an installed copy:

| Function | Purpose |
| --- | --- |
| `benchmark_resource()` | describe one worker file (hashes, coverage) |
| `prepare_benchmark_resources()` | discover local shards, split local measures per method |
| `plan_benchmark_release()` | build a release plan and its archives (state directory) |
| `benchmark_packing_report()` | per-DGM file counts, sizes and quota |
| `stage_benchmark_release()` | create/update drafts and upload archives |
| `verify_benchmark_release()` | read-only check of the staged drafts |
| `publish_benchmark_release()` | publish, include in the community, verify publicly |
| `update_benchmark_community_pages()` | replace the community's About and curation pages |

## What the gate protects

`stage_benchmark_release()`, `publish_benchmark_release()` and
`update_benchmark_community_pages()` ask Zenodo for the community members before
any request that writes. The token's account must be

- an `owner` or `manager` of the community for staging and publishing;
- the `owner` for community pages (Zenodo allows only owners to update a community).

On production the community must be the PublicationBiasBenchmark community
(`415b2de6-b6f9-444d-9109-d74752e20cd0`); the sandbox accepts any community the
token maintains. The gate fails closed: a missing token, an unknown community, a
token for the other environment (sandbox vs production), a non-member, another
role or any unexpected answer stops the call before anything is written.

The gate protects this package's entry points. It is **not a security boundary for
Zenodo itself**: whoever holds a token can call the Zenodo API directly. Keep the
tokens private (`ZENODO_TOKEN` for production, `ZENODO_SANDBOX_TOKEN` for the
sandbox; they are read from the environment and never stored in plans or state).

Publishing is irreversible and therefore also needs `confirm = "<release>"`, the
plan's release identifier repeated by hand. `verify_benchmark_release()` is
read-only and never needs the gate.

## Workflow

1. **Workers** write unpublished files beneath `resources_directory/DGM/results`.
   A CSV shard contains one method and setting and has unique condition/repetition
   keys. Workers must use disjoint keys and report the package version that
   actually generated each batch.
2. **Describe the files.** Describe worker outputs one by one, or discover them:

   ```r
   files <- list(
     PublicationBiasBenchmark:::benchmark_resource("worker-01.csv", "no_bias", "results",
       method = "myMethod", method_setting = "default", package_version = "0.4.0",
       id = "no_bias/results/myMethod-default/batch-01"))
   files <- PublicationBiasBenchmark:::prepare_benchmark_resources("no_bias",
     kinds = c("results", "measures"), output_directory = "resources/prepared",
     package_version = "0.4.0")
   ```

   Measures are published per method and setting with the logical ID
   `<dgm>/measures/<label>` (no `.csv`) and the member file
   `<dgm>__measures__<label>.csv`. Two method/setting pairs that spell the same
   label are rejected.
3. **Plan.** The plan builds and persists the ZIP archives and the state directory.

   ```r
   metadata <- list(
     creators = list(list(person_or_org = list(type = "personal",
       given_name = "Your", family_name = "Name"))),
     rights = list(list(id = "cc-by-4.0")))
   plan <- PublicationBiasBenchmark:::plan_benchmark_release(
     "my-next-release", files, previous = "2026.2", metadata = metadata,
     state_directory = "resources/publication/my-next-release",
     catalog_record_id = "23121978", catalog_concept_doi = "10.5281/zenodo.23070786")
   PublicationBiasBenchmark:::benchmark_packing_report(plan)
   ```

   Review the packing report, `asset-audit.csv`, the proposed metadata and sandbox
   evidence before anything is published.
4. **Stage, verify, publish.**

   ```r
   PublicationBiasBenchmark:::stage_benchmark_release(plan)
   PublicationBiasBenchmark:::verify_benchmark_release(plan)
   entry <- PublicationBiasBenchmark:::publish_benchmark_release(plan, confirm = "my-next-release")
   ```

5. **Register.** After verified publication and approval, add `entry` to
   `inst/extdata/benchmark-releases.json` and set `default_release` for the next
   package release. A catalog file can also be used directly with
   `release = "/path/to/release.json"`. Concurrent publishers must use distinct
   release identifiers; only one maintainer publishes a given plan.

Use `ZENODO_SANDBOX_TOKEN` with `sandbox = TRUE` and a dedicated sandbox community
for integration tests; the production community does not exist there.

## What staging and publishing do

Archive bytes are built once with the suggested `zip` package and persisted with
their hashes for retries. Only flat ASCII regular-file members using store or
deflate compression are supported; the reader checks the ZIP central directory,
including link attributes, before extracting. A new DGM storage version imports
the previous version's files, removes superseded unit ZIPs from the new draft, and
uploads complete replacement ZIPs. Imported ZIPs count toward the 100-file and
50,000,000,000-byte default limits. Packing fails instead of creating arbitrary
extra record families. `archive = FALSE` retains the legacy direct-file publisher.

The catalog is published only after verification:

- every storage record is checked **anonymously** (no token): records and files
  must be public and not under embargo;
- every advertised archive is checked with an anonymous one-byte ranged request
  (`Range: bytes=0-0`; the body read is capped at 64 KiB) against the catalog size;
  a server that reports no size makes the check download that archive in full;
- archives uploaded by this plan are downloaded in full and every member is verified;
  unchanged archives are not downloaded again;
- native rights, DOI relationships and accepted community inclusion are verified.

Freshly published files can take a moment to appear: 403 and 404 answers are
retried five times; 410 (withdrawn) and other client errors are not retried.

The publisher submits published records and accepts its own inclusion requests with
the community owner's token and preserves the community review policy. Subsequent
storage and catalog versions inherit the family's community membership and
branding, which are checked on every release. Catalog publication continues the
existing native version family with a new DOI.

A correction to an existing logical shard requires its ID in `replace`. The new
catalog selects corrected bytes; earlier catalogs retain the old references. Frozen
conditions and dataset coverage cannot grow for an existing DGM (plans of both
schemas reject added, removed or changed condition rows); add a new DGM identifier
for new conditions or datasets. Corrections of existing dataset assets are allowed
explicitly, but affected results and derived measures must be recomputed, supplied
and listed in `replace` in the same release. Result corrections or additions
invalidate affected ordinary, replacement and pairwise measures. The publisher
records input asset IDs/hashes and rejects stale derived assets. Provide actual
input IDs through `benchmark_resource(dependencies = ...)` when known; otherwise
coverage and method metadata conservatively infer dependencies. Metadata and source
archive assets can be explicitly corrected, but new assets of these kinds require a
new DGM. Generation versions remain attached to members; the package version
building a ZIP is recorded separately.

## The state directory

Keep the state directory until public verification and registry pinning finish. It
allows an interrupted run to resume without replacing completed files; running the
same plan again recovers lost responses and never duplicates records or uploads.

| Path | Content |
| --- | --- |
| `plan.rds` | the plan, with its identity |
| `state.json` | what has been created online (records, versions, community, journal) |
| `state-history/` | every earlier `state.json`, never deleted |
| `release.json` | the staged or final catalog |
| `catalog-history/` | every earlier `release.json` and `registry-entry.json`, never deleted |
| `registry-entry.json` | the result of publishing |
| `archives/`, `public-verification/` | built ZIPs and public verification copies |
| `packing-report.csv`, `asset-audit.csv` | review material |
| `deletions.log` | audit log of online deletions (never read by the package) |
| `.lock/` | the lock of a running session |

Files are written next to their destination, checked byte for byte and only then
renamed into place; `state.json`, `release.json` and `registry-entry.json` also keep
a copy of every version in the history directories above (`plan.rds`, the lock's
`owner.json` and community-page backups are verified but have no history). A failed
check restores the previous bytes and checks them again; if restoring fails, the
error says which file holds the unverified new bytes and where the previous bytes
are. Catalogs and state are LF-terminated UTF-8 JSON on every platform. Release
2026.1 is pinned by its checksum and is never rewritten.

A plan must not be edited after planning: every session recomputes the plan's
identity (release, community, environment, the id, name, hashes and size of every
upload entry, the storage base record of each changed DGM and the earlier catalog's
schema, record selectors, metadata, packing limits) and refuses a modified plan
before any request. `verify_benchmark_release()` reads the state first and also
stops when a staged `release.json` belongs to another release or has a different
archive or member descriptor (id, name, size, SHA-256, MD5).

### Releases interrupted by an earlier package version

Finish an in-flight publication with the package version that started it. This
version refuses plans and states written by earlier versions ("This plan was
created by an earlier package version; re-plan in a new state directory"), so do
not upgrade the package in the middle of a release. If a release was interrupted
by an earlier version and has to be continued with this one, the plan is rebuilt in
a new state directory and what exists online decides what is possible:

- **Unpublished drafts** of the interrupted run (storage version drafts and the
  catalog draft) can be discarded in the Zenodo web interface before re-planning in
  a new state directory; the new plan then creates its own drafts. The package
  never deletes completed draft files itself.
- **Published storage versions cannot be changed.** A run interrupted after its
  storage versions were published needs manual reconciliation: the ZIPs of a
  re-planned release are rebuilt and will not match the published ones, so the new
  plan cannot adopt them. Decide how to proceed before re-planning (for example by
  finishing that release with the older package version).
- **CRLF `release.json`.** Staging never uploads the catalog file.
  `publish_benchmark_release()` uploads `release.json` to the catalog draft only
  after every storage version of the release has been published and verified
  publicly, in this version and in earlier ones. A completed `release.json` in the
  catalog draft of an interrupted earlier-version run (with CRLF line ends when that
  version wrote it on Windows; this version writes LF bytes) therefore means that
  the run had already published its storage versions: it is the case above. Finish
  that release with the package version that started it. A plan re-planned with this
  version cannot adopt the published storage versions: its rebuilt ZIPs differ from
  them, so the storage check of `verify_benchmark_release()` and of publishing stops
  the re-planned release before its catalog is uploaded. Removing the catalog file
  from the draft does not change that.

Do not edit files in the state directory.

### Lock

A session owns its state directory exclusively through the directory `.lock`
(planning, staging and publishing all take it). `.lock/owner.json` names the
process id, host and start time. If a session crashed or was interrupted hard, the
next run stops with "The publication state directory is locked" and prints the
owner and the exact path. Remove the lock **only if that session is not running**:

1. check on the named host that no R session with that process id is publishing;
2. delete the directory printed in the message (`.../.lock`);
3. run the same call again; it resumes.

The package never removes a lock it did not create, and a session releases its own
lock only while `owner.json` still carries its random nonce.

### State recovery

If `state.json` is unreadable, invalid, or missing while `state-history/` exists, the
run stops before any request and states that nothing was changed online. The message
names the newest history file. Check that file and copy it to `state.json`, then run
again. A state written for another plan, or by a package version before plan
identities existed, is rejected: re-plan in a new state directory. A plan saved by
an older version must be re-planned the same way.

### Online deletions

All deletions go through one guarded helper. A draft file is deleted only when it is
provably redundant: a pending file that this state created (its creation time was
journaled from the initialization response) and whose bytes differ from the plan, or
an imported copy of a file the published base version holds with identical key,
checksum and size. Every attempt is appended to `deletions.log` (`intent` before,
`done` after). If a pending file holding bytes cannot be attributed to this state,
the run stops, names the record, the key and the draft files URL, and deletes
nothing: inspect the draft on Zenodo, remove the file there if it is safe, and run
again.

## Community pages

`update_benchmark_community_pages(community, about, curation_policy, sandbox,
confirm = community)` replaces the About and curation pages. It needs the owner's
token and `confirm` equal to the community, and it writes the previous pages to
`backup_directory` (default `tools::R_user_dir("PublicationBiasBenchmark", "data")` on
R 4.0 or later and `~/.PublicationBiasBenchmark` on older R versions) as a verified
JSON file before the request; if that fails nothing is changed. Review
and approve the HTML first. The community's access and review policies are preserved.

## Older scripts

The scripts under `resources/zenodo-consolidation/` predate `confirm`, the gate and
the internal status of these functions. Do not reuse them as they are; adapt them to
the calls above first.
