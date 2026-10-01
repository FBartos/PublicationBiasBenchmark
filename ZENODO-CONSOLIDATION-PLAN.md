# Zenodo community and storage consolidation plan

Date: 2026-10-01, revised the same day after review. Status: implementation on
`codex/zenodo-consolidation`; production checkpoints remain pending.

## Objective and scope

Give PublicationBiasBenchmark a permanent Zenodo community home, a clear release
history and a small number of meaningful storage records. Publish cumulative
releases by adding only new or corrected download units. Preserve all existing
results, their provenance and the ability to select an exact historical release.

This is a handoff for an agent on another machine. Implement the package reader,
publisher, documentation and production consolidation described below. Adding
this plan did not change published records or settings. One-off migration
scripts, credentials, downloaded data, ZIPs and publication state must stay
outside Git. Reusable package functions and their tests belong in the
repository. Production records mint permanent DOIs that cannot be deleted, so
stop for the user's explicit approval at both checkpoints in the execution order.

## Current state and portable starting point

- Repository: <https://github.com/FBartos/PublicationBiasBenchmark>.
- Branch `codex/zenodo-migration` holds this plan; its last code commit is
  `bf1022a`.
- PR <https://github.com/FBartos/PublicationBiasBenchmark/pull/10> publishes and
  pins release `2026.1`. It was still open at the implementation check. The
  consolidation branch uses that verified code as its baseline and keeps new
  work out of PR 10. Reconcile the new PR's base after PR 10 is merged.
- Baseline package version: `0.4.0`; implementation development version:
  `0.5.0.9000`; default benchmark release remains `2026.1` pending verification
  and approval of a production consolidation release.
- Catalog: <https://zenodo.org/records/23070787>, version DOI
  `10.5281/zenodo.23070787`, concept DOI `10.5281/zenodo.23070786`.
- Public catalog file:
  <https://zenodo.org/records/23070787/files/release.json?download=1>.
- Catalog SHA-256:
  `40e72cdf642a023956e53c3e58117ba6c23840e56f5f7d394cf8d47360f1e07f`.
- Registry: `inst/extdata/benchmark-releases.json`.
- The catalog references 2,216 payload files across 40 storage records, about
  28.098 GB uncompressed. There is also one catalog record: 41 records in total,
  each with a version DOI and a concept DOI. DOIs are assigned to records, not
  each file.
- All 1,965 original OSF files are preserved, alongside canonical metadata and
  derived per-method measures. Public bytes were verified against sizes, SHA-256
  and MD5; all 41 records have CC BY 4.0 recorded in native `metadata.rights`.
- The five DGMs are `no_bias`, `Alinaghi2018`, `Bom2019`, `Carter2019` and
  `Stanley2017`.
- In `2026.1`, results are one CSV per DGM/method/setting (largest 362 MB),
  measures are one CSV per DGM/method/setting/variant, and no pairwise measure
  assets exist.
- The records currently link to OSF provenance, but lack explicit release/storage
  `isPartOf` and `hasPart` relationships and community membership.
- The current publisher (`R/upload.R`) creates a new catalog record for every
  release and new component records for every batch. It has no native
  versioning, file import, file deletion or community support yet.

Use the verified public `2026.1` catalog and its file references as the source
inventory. It is available through `benchmark_catalog("2026.1")`; every resource
kind, including `metadata` and `archive`, must be considered. Do not depend on
ignored migration files from the original machine, and do not repeat simulations
or require OSF uploads. If a referenced public file is unavailable or fails its
hash, stop that migration step and report the exact affected asset.

The original machine has optional local audit/state files beneath
`resources/migration/`, including `publication/plan.rds` and `publication/state.json`.
They are not tracked and are not prerequisites for this handoff.

## Required user experience

Users must be able to download these independently:

| Selection | Maximum scope of the payload downloaded |
| --- | --- |
| Simulated datasets for one DGM | That DGM's datasets |
| Results for one DGM and one method | That DGM/method's results |
| Measures for one DGM and one method | That DGM/method's measures |

Sharing a Zenodo record must not force downloading its other files. Never put
different methods' ordinary results or measures into the same download archive.
Keep datasets, results and measures in separate archives. A method's ordinary
and replacement measures share one archive. No pairwise tables exist in `2026.1`;
keep the existing filter so that future pairwise tables remain an explicit
separate selection and are never pulled into ordinary per-method measure
downloads.

Dataset downloads are rare, so coarse dataset archives are accepted. A condition
filter fetches the whole data archive containing that condition, which may be
all of a DGM's datasets. Document this honestly.

Preserve the public `download_dgm_*()` and `retrieve_dgm_*()` interfaces and
existing selection arguments. The package handles archive downloads, validation,
extraction and combining worker shards. Users do not manage record IDs or ZIP
contents. A condition/measure filter may fetch other conditions/measure columns
inside the selected archive. It must still satisfy the three maximum download
scopes above.

## Target organization and record versioning

Use three layers:

1. **Community:** the permanent project home, navigation and curation.
2. **Release catalog record family:** one native Zenodo version per benchmark
   release. The version DOI identifies an exact release; the concept DOI links
   to the latest catalog. Cite the exact version in reproducible analyses.
3. **DGM storage record families:** one record family per DGM, holding one
   independently downloadable ZIP per download unit. All resource kinds for a
   DGM share its storage record; the ZIP boundaries enforce download scope.

### DGM lifecycle

A DGM's conditions, datasets, metadata and source archive are fixed when the DGM
is first published. New or changed conditions define a new DGM with its own
storage family; the publisher rejects new conditions or new data assets for a
published DGM. Within a DGM, only method results and measures change: new
methods or settings, explicit corrections and, later, pairwise tables. An
explicit correction of a published dataset remains possible through the
replacement mechanism and republishes the affected data archive. Reject a
correction that retains stale dependent results or ordinary/replacement/pairwise
measures. Record input IDs/hashes; affected derived assets must be recomputed,
supplied and explicitly replaced in the same release.

### Download units

Each storage version contains exactly one ZIP per download unit:

- `data`: contiguous condition ranges, split only as needed to keep each archive
  within 2,000,000,000 bytes of uncompressed members. R's internal `unzip` supports larger
  archives only partially (see `?unzip`).
- `results`: one per method/setting.
- `measures`: one per method/setting, holding both ordinary and replacement
  variants.
- `pairwise`: one per DGM, once pairwise tables are published.
- `metadata`: one per DGM.
- `archive` (original source tables): one per DGM.

Only a unit that exceeds the 2 GB cap is split, into ordered parts.
Split at member boundaries. Reject a single oversized member with an informative
error; splitting its containing unit cannot make that member fit. Keep established
dataset chunk membership where possible, reusing unchanged chunks during corrections.

Estimated from the `2026.1` catalog, with data chunks in parentheses:

| DGM | ZIPs |
| --- | --- |
| `no_bias` | 46 (1) |
| `Alinaghi2018` | 54 (2) |
| `Bom2019` | 55 (3) |
| `Carter2019` | 57 (5) |
| `Stanley2017` | 54 (2) |

Each record keeps 43-54 free file slots, roughly 20 more method/settings per DGM
at two ZIPs each. The largest DGM, `Carter2019`, holds about 12.4 GB
uncompressed: 8.66 GB of datasets and 3.60 GB of results. The `archive` kind is
only 0.28 GB across all DGMs. Recalculate exact archive counts and compressed
sizes during implementation; these estimates do not promise unlimited growth.

### Versioning

For an update, reuse unchanged storage versions. When a DGM changes:

1. Create a native new version of its storage record.
2. Import the previous version's files. InvenioRDM's files-import links all files
   of the previous version without duplicating storage.
3. Delete the ZIPs of the units being replaced from the new draft.
4. Upload one new complete ZIP per added or changed unit.

A changed unit's new ZIP contains its unchanged members byte for byte plus the
new or corrected members, so file counts stay bounded. A published ZIP is never
edited or reused under the same name, and earlier versions keep their original
contents. The new catalog selects the corrected logical assets; old catalogs keep
their original references. Readers follow the catalog, not record file listings.

Each catalog uses exactly one storage version per DGM. Every archive descriptor
for a DGM references that version's record ID, including archives first
uploaded to an earlier version; imported bytes and hashes are identical. This
keeps `hasPart` exact and makes the snapshot each release uses unambiguous.

### Catalog family and historical records

Continue the existing catalog family rooted at record `23070787` using native
versioning (`POST /records/{id}/versions`), not the current publisher's
new-record path. The first consolidated catalog is a **new benchmark release**
with a new release identifier and version DOI. Retain `2026.1` and its hash
unchanged in the package registry. Inspect existing remote versions/drafts
before choosing the next release identifier; `2026.2` is a candidate, not a
reserved identifier. Future catalogs are complete cumulative snapshots even
though payload uploads contain only changed units.

The current 40 storage records and their DOIs remain historical. Consolidation
creates a new active layout; it cannot erase previously minted identifiers or
make global Zenodo search contain only six records. Do not delete, withdraw,
restrict or overwrite the existing baseline to improve search appearance.
Consolidation re-uploads every `2026.1` payload, compressed, into the new
families, which stay on Zenodo permanently alongside the historical records.

### Quotas

Default limits remain 100 files and 50,000,000,000 bytes per record version.
Count physical ZIP files, not their logical members. Imported files count toward
each new version's limits. Check limits before any upload, and fail with a
useful packing report when the chosen layout no longer fits. Request an
appropriate quota/use-case agreement from Zenodo for substantial growth; do not
silently create many arbitrary records to bypass quotas. If expansion requires
additional record families, for example separate results and measures families
for a DGM, define meaningful collections and document the agreed layout. The
community itself provides no extra storage allowance.

## Archive boundaries and version stamps

Group assets into the download units above. Do not combine CSV worker shards
into one method-wide CSV. Store the original individual CSV files as flat ZIP
members named by their logical filename, retaining their exact bytes and
existing method version fields. Distributed shard filenames must be unique
within their unit; prefix them before staging, as the publisher already
requires. Keep source archives and metadata separate from normal downloads.

Use predictable unique ZIP names, for example:

```text
Carter2019--data--c0001-0180--2026.2.zip
Carter2019--results--RMA--default--2026.2.zip
Carter2019--measures--RMA--default--2026.2.zip
Carter2019--pairwise--2026.3.zip
Carter2019--metadata--2026.2.zip
Carter2019--archive--2026.2.zip
```

The suffix names the release that first published that revision of the unit.
Append `--part-002` and so on when a unit is split. Condition ranges and
releases are illustrative. Reject method or setting identifiers containing `--`.
Record packaging version and batch metadata in the catalog's archive
descriptors, not in filenames.

The packaging version means the package assembling the archive, not the version
that generated every member. Preserve the existing per-asset producing
`package_version`, including `NULL` where unknown. Do not invent historical
generation versions, relabel them as `0.4.0`, or assume distributed jobs all ran
the same version. Newly computed workers should provide their actual producing
package versions. Release identity, packaging version and per-worker
generation/method versions are distinct metadata.

Build each archive once with the `zip` package (in Suggests, used only by the
publisher). Persist it with its size and hashes in publication state, and verify
it before every upload or retry instead of rebuilding it; do not rely on
deterministic rebuilds. Do not add per-worker README files or redundant
provenance bundles. The release catalog and existing CSV provenance are
sufficient.

## Catalog schema and package reader changes

Introduce a versioned catalog extension, preferably schema version 2, while
retaining support for current schema version 1 and its direct CSV downloads.
An old release remains usable explicitly after the default changes.

Keep logical `assets` and their IDs, coverage, row counts, hashes, method fields,
replacement/measure metadata and generation provenance. Add a physical archive
table, with one descriptor per ZIP: archive ID, exact record/version ID, filename,
SHA-256, MD5, byte size and packaging/batch metadata. An archive-backed logical
asset references an archive ID; its member name equals its logical filename, and
it keeps its own original size and hashes. Do not confuse a member hash with a
ZIP hash. Retain a complete member inventory for each referenced archive,
including members no longer selected after a logical correction, so reused ZIPs
still validate. Keep this information in `release.json`; no second downloadable
manifest is required. Validate all references and coverage before staging.

Reader behavior:

1. Load the selected, hash-pinned catalog; select logical assets first.
2. Check verified member caches first. Resolve and deduplicate only the archives
   needed for missing or corrupt members, and download each once, verifying its
   size, SHA-256 and MD5.
3. Extract with base R (`utils::unzip(unzip = "internal")`; no new Imports).
   Inspect the ZIP central directory (base R's listing does not expose file types)
   and reject unsafe or undeclared names, links,
   duplicates and collisions. Extract needed members one at a time with
   `junkpaths = TRUE` into a fresh temporary directory. Never allow an archive
   member to write outside the intended cache directory.
   Support flat ASCII regular-file members with store/deflate compression in
   single-disk, non-ZIP64 archives; reject unsupported formats before extraction.
4. Verify each extracted member against its catalog size and hashes, then move it
   atomically to the existing `cache/<sha256>/<filename>` path used for schema-1
   files. Existing `2026.1` caches are then reused without new downloads, and
   flat paths avoid Windows path-length problems.
5. Re-extract from a verified cached archive when possible. Do not include
   unrelated cached files by scanning directories, and do not equate a cached ZIP
   with verified extracted members. Provide a way to remove cached archives whose
   members are verified, because keeping both roughly doubles dataset disk use.

Preserve existing duplicate-key, coverage, row-count and frozen-condition checks.
Keep local unpublished computation readers working. Download progress/prompts
must report actual pending ZIP bytes without counting a shared archive once per
member. Resource listing and cache verification should make archive/member
relationships understandable. Use portable ZIP handling on Windows and Linux;
do not depend on undocumented remote ZIP-member extraction or HTTP range access.

## Community integration and DOI relationships

Community URL: <https://zenodo.org/communities/publicationbiasbenchmark/>.
Slug: `publicationbiasbenchmark`.
UUID: `415b2de6-b6f9-444d-9109-d74752e20cd0`.

At the 2026-10-01 read-only check, the community was public and empty, membership
was by invitation, submissions were open, and all submissions required review
(`review_policy = closed`). The configured production token belonged to its
owner. Recheck current remote state rather than assuming these settings persist.
Configure credentials on the other machine as `ZENODO_TOKEN` in the user-level
`~/.Renviron` or the ignored project `.Renviron`. Never store them in any other
file inside the repository, and never commit or print them. Sandbox uses a
separate `ZENODO_SANDBOX_TOKEN`.

Add reusable publisher support for the community. Resolve its slug to the UUID
and persist that resolved identity in publication state. On the live Zenodo API,
the members endpoint required the UUID even though upstream documentation also
advertised slug support. Follow actual returned links and response schemas.

Include the existing catalog family and the consolidated DGM storage families in
the community, and use PublicationBiasBenchmark as their branded community. Keep
the historical storage records accessible through the old catalog; there is no
need to populate the community with every historical storage part immediately.
Use consistent titles and keywords identifying DGM, release and storage role.
Native version families should provide coherent history instead of one unrelated
record family per update.

InvenioRDM stores community membership and the default (branding) community on
the parent record. Inclusion should therefore be a one-time action per record
family, about six requests in total, which later versions inherit. Verify this
in the sandbox, track inclusion per family in publication state, and verify the
membership of every version a release uses.

Publish storage records first, then request inclusion of the published records.
Do not submit drafts for community review: under `review_policy = closed` that
blocks publication until the request is accepted. Keep `review_policy = closed`,
so outside submissions are still reviewed. The publisher accepts its own
inclusion requests with the owner's token. Do not change community settings
without the user's approval. Submission, acceptance and branding must be
explicit, resumable publisher operations; do not generate curator comments or
send invitations as part of migration.

Give every storage version an `isPartOf` relationship to the catalog concept DOI
`10.5281/zenodo.23070786`. Give each catalog version `hasPart` relationships to
the exact storage-version DOIs it uses. The concept link never changes, so
reused storage versions need no metadata edits in later releases. The catalog
DOI also need not be reserved early, because storage versions publish before the
catalog. Zenodo links versions within a family natively. Preserve OSF
`isDerivedFrom` provenance and all other existing metadata. These are metadata
relationships, not a way to assign the catalog's DOI to separate storage records.

The About page should explain the three download units and link the latest
catalog, exact-release citation instructions, package website and documentation.
The curation policy should state that official benchmark releases require
validated contents and provenance. Record pages should point users to the common
release citation even though storage records have their own DOIs.

Community search and the concept DOI are navigation aids. Package readers use
the registry's exact catalog record and checksum, never community search order
or an unpinned "latest" result. Public downloads need no token or membership.

## Publisher changes and production execution

Extend the existing planning, staging, verification and publication functions.
Add native new-version creation, files-import and draft file deletion. Recover
an existing new-version draft instead of creating another. Track all of the
following in local resumable state:

- record family IDs and version IDs;
- imported and deleted files;
- persisted archives and their hashes, and completed archive uploads;
- community inclusion requests and acceptance per family;
- metadata completion.

Recover lost responses by checking remote state before creating duplicate
versions, records or requests. Retain existing rate-limit handling and native
license validation (`metadata.rights`, not legacy `license`/`licenses`). Keep
public record/version identifiers in the catalog; never store credentials there
or in publication state.

Execute in this order:

1. Implement and unit-test the reader/publisher changes with small fixtures,
   without remote writes.
2. Run a sandbox publication end to end in a dedicated sandbox test community,
   which the production UUID does not identify. Cover:
   - a baseline with at least two DGM families and a catalog family;
   - a second release that adds one method unit and corrects another;
   - native versioning, files-import and removal of the superseded ZIP;
   - community inclusion and acceptance, and inherited membership/branding on a
     new version;
   - relationships;
   - recovery after interrupted upload, publish, import and inclusion;
   - anonymous reads from a clean cache.
3. Run a production dry run with read-only remote access:
   - Reconcile remote catalog versions/drafts and community state, and propose
     the release identifier.
   - Verify the source catalog hash and build the complete source inventory from
     public references.
   - Download and verify source assets, then build and persist the archives
     without rewriting CSVs.
   - Produce a local audit mapping every old asset to its archive member and
     hash.
   - Produce a packing report with each DGM record's ZIP count, compressed and
     uncompressed bytes, free quota and the total upload volume.
4. **Checkpoint 1: user approval.** Report the sandbox evidence, packing report,
   proposed release identifier, titles and metadata. Make no production write
   until the user explicitly approves.
5. Create the DGM storage families, upload the archives and verify their
   metadata and contents. Publish them, then verify anonymous public downloads
   and every logical member's original hashes.
6. Request and accept community inclusion of the published storage families
   and the existing catalog family, and set branding. Confirm actual accepted
   membership, not merely a successful submission response. Confirm native
   rights and `isPartOf` relationships. Retry incomplete steps from saved state.
7. Create the new catalog version in the family rooted at `23070787`, with its
   `hasPart` relationships. Publish it only after the payload and community
   checks pass. Verify its public bytes/hash, rights, relationships and
   membership. Run fresh anonymous package downloads from the new catalog in an
   isolated cache.
8. **Checkpoint 2: user approval.** Report the release DOI/hash and verification
   evidence. Only after approval, add the verified registry entry, retain the old
   entry and change the package default. Update release/package metadata and
   documentation to the actual published identifiers and versions.

Publishing payloads before the catalog is a recoverable intermediate state. A
failed inclusion or verification must not advertise an incomplete new default.
Do not overwrite an existing shard implicitly: preserve the current explicit
replacement mechanism for logical corrections, including dependent measures.

## Code and documentation entry points

| Path | Work |
| --- | --- |
| `R/catalog.R` | Schema validation, archive references, listing, member-first verified caching and extraction |
| `R/download.R` | Archive-aware selection/downloads and unchanged retrieval interfaces |
| `R/upload.R` | Unit ZIP planning and persistence, DGM families, native catalog/storage versions, files-import and deletion, relationships, community workflow, frozen-DGM checks and state recovery |
| `R/prepare-resources.R` | Preserve distributed shard preparation and provenance handling |
| `inst/extdata/benchmark-releases.json` | Preserve 2026.1 and pin the verified new release |
| `tests/testthat/test-downloads.R` | Reader, selection, integrity, cache reuse and backwards compatibility tests |
| `tests/testthat/test-release-publication.R` | Packing, unit replacement, version import, metadata and recovery tests |
| `vignettes/Benchmark_Releases.Rmd` | Replace the current raw-file/many-record explanation with the agreed layout |
| `vignettes/Using_Presimulated_Datasets.Rmd`, `vignettes/Using_Precomputed_Results.Rmd`, `vignettes/Using_Precomputed_Measures.Rmd` | Explain independent download units and filtering behavior |
| `README.Rmd`, generated `README.md`, `NEWS.md`, `DESCRIPTION` (`zip` in Suggests), `man/` | Keep usage, release links, package version and generated reference docs synchronized |

Check the actual source tree before adding helpers; avoid duplicating existing
coverage, cache or publication logic. Keep migration-only scripts outside Git.

## Acceptance and verification

- Both the old schema-1 `2026.1` catalog and the new archive-backed catalog work.
- Each of the three download selections fetches only the allowed DGM/kind/method
  archives. A dataset condition filter fetches only the data archives containing
  that condition. Settings/replacement filters work, and pairwise tables stay a
  separate selection.
- Distributed CSVs remain separate, and retrieval matches baseline values,
  coverage, row counts and method versions. All 1,965 original OSF files and all
  existing logical assets map to byte-identical verified members.
- Every record passes actual file-count/byte quotas, native license validation,
  public archive hash verification and member hash verification.
- A small second-release test:
  - adds one method unit and corrects another;
  - reuses unchanged files without uploading their contents again;
  - removes the superseded ZIP from the new version only, and preserves all old
    snapshots;
  - rejects an unintended overlap or replacement, and new conditions or data for
    a published DGM.
- Retries after partial upload, publish, version import, file deletion or
  community inclusion recover the same state without duplicate
  records/versions/requests. Failures do not update the default registry entry.
- Corrupt/truncated ZIPs, missing or corrupt members, unsafe paths and duplicate
  member destinations fail safely. Shared archive downloads are deduplicated.
  Cached-member repairs, reuse of existing `2026.1` caches and cross-release
  cache reuse work on Windows and Linux.
- Public record pages show consistent community branding, useful titles and
  release relationships. The About page explains where to start and how to cite.
- Both user checkpoints were passed with explicit approval.
- Run the relevant package tests, R CMD check and a package website build with
  the result articles, then confirm existing CI checks. Use small fixtures for
  routine automated tests; production audit covers the full migrated inventory.
- Review the final Git diff/build contents for credentials, one-off scripts,
  source data, archives and state files. Report the final release DOI/hash,
  storage-family inventory, community status and verification evidence.

## Verified primary references

Recheck these and live API capabilities during implementation:

- [Communities and roles](https://help.zenodo.org/docs/communities/about-communities/)
- [Adding already published records](https://help.zenodo.org/docs/share/submit-to-community/)
- [Submission review and policies](https://help.zenodo.org/docs/communities/review-submissions/)
- [Community About/curation pages](https://help.zenodo.org/docs/communities/manage-community-settings/add-pages/)
- [Record branding and curation](https://help.zenodo.org/docs/communities/curate/)
- [File limits and ZIP recommendations](https://help.zenodo.org/docs/deposit/manage-files/)
- [Version DOIs, concept DOIs and storage reuse](https://zenodo.org/help/versioning)
- [Storage quotas and fair usage](https://support.zenodo.org/help/en-gb/1-upload-deposit/80-what-are-the-size-limitations-of-zenodo)
- [Native drafts/records/version/file APIs](https://raw.githubusercontent.com/inveniosoftware/docs-invenio-rdm/master/docs/reference/rest_api_drafts_records.md),
  including files-import (`POST /api/records/{id}/draft/actions/files-import`)
- [Native community API](https://raw.githubusercontent.com/inveniosoftware/docs-invenio-rdm/master/docs/reference/rest_api_communities.md),
  including `review_policy` values
- [Native membership API](https://raw.githubusercontent.com/inveniosoftware/docs-invenio-rdm/master/docs/reference/rest_api_members.md)
- [Native request actions](https://raw.githubusercontent.com/inveniosoftware/docs-invenio-rdm/master/docs/reference/rest_api_requests.md)
- R `?unzip` for the internal method's archive-size limits

Upstream InvenioRDM documentation can differ from the deployed Zenodo version.
Verify request/response formats in the sandbox and use returned API links;
do not silently fall back to legacy metadata that the native API ignores.
