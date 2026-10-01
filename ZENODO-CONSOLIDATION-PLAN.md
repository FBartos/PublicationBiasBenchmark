# Zenodo community and storage consolidation plan

Date: 2026-10-01. Status: agreed direction, awaiting implementation.

## Objective and scope

Give PublicationBiasBenchmark a permanent Zenodo community home, a clear release
history and a small number of meaningful storage records. Publish cumulative
releases by adding only new or corrected batches. Preserve all existing results,
their provenance and the ability to select an exact historical release.

This is a handoff for an agent on another machine. Implement the package reader,
publisher, documentation and production consolidation described below. This
commit only records the plan; it does not change published records or settings.
One-off migration scripts, credentials, downloaded data, ZIPs and publication
state must stay outside Git. Reusable package functions and their tests belong
in the repository.

## Current state and portable starting point

- Repository: <https://github.com/FBartos/PublicationBiasBenchmark>.
- Branch: `codex/zenodo-migration`; implementation baseline: commit `bf1022a`.
- Existing PR: <https://github.com/FBartos/PublicationBiasBenchmark/pull/10>.
  Continue this branch/PR unless the user directs otherwise.
- Package version: `0.4.0`; default benchmark release: `2026.1`.
- Catalog: <https://zenodo.org/records/23070787>, version DOI
  `10.5281/zenodo.23070787`, concept DOI `10.5281/zenodo.23070786`.
- Public catalog file:
  <https://zenodo.org/records/23070787/files/release.json?download=1>.
- Catalog SHA-256:
  `40e72cdf642a023956e53c3e58117ba6c23840e56f5f7d394cf8d47360f1e07f`.
- Registry: `inst/extdata/benchmark-releases.json`.
- The catalog references 2,216 payload files across 40 storage records, about
  28.098 GB. There is also one catalog record: 41 records in total, each with a
  version DOI and a concept DOI. DOIs are assigned to records, not each file.
- All 1,965 original OSF files are preserved, alongside canonical metadata and
  derived per-method measures. Public bytes were verified against sizes, SHA-256
  and MD5; all 41 records have CC BY 4.0 recorded in native `metadata.rights`.
- The five DGMs are `no_bias`, `Alinaghi2018`, `Bom2019`, `Carter2019` and
  `Stanley2017`.
- The records currently link to OSF provenance, but lack explicit release/storage
  `isPartOf` and `hasPart` relationships and community membership.

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
Keep datasets, results and measures in separate archives. Finer partitions by
method setting, replacement variant or computation batch are acceptable.
Pairwise comparison tables remain an explicit separate selection and must not
be pulled into ordinary per-method measure downloads.

Preserve the public `download_dgm_*()` and `retrieve_dgm_*()` interfaces and
existing selection arguments. The package handles archive downloads, validation,
extraction and combining worker shards. Users do not manage record IDs or ZIP
contents. A condition/measure filter may fetch other conditions/measure columns
inside the selected archive; document this honestly. It must still satisfy the
three maximum download scopes above.

## Target organization and record versioning

Use three layers:

1. **Community:** the permanent project home, navigation and curation.
2. **Release catalog record family:** one native Zenodo version per benchmark
   release. The version DOI identifies an exact release; the concept DOI links
   to the latest catalog. Cite the exact version in reproducible analyses.
3. **DGM storage record families:** a baseline record per DGM containing multiple
   independently downloadable ZIPs. All resource kinds for a DGM can share its
   storage record; the ZIP boundaries enforce download scope.

The current baseline can provisionally fit in five DGM storage records plus a
catalog record. A previous packing estimate, including separate metadata and
original-source archives, gave 67 ZIP entries for `no_bias` and 78 for each other
DGM. The largest DGM held about 12.4 GB of source assets. Recalculate exact archive
counts and compressed byte sizes during implementation; these are estimates,
not a promise that five records will accommodate unlimited future growth.

For an update, reuse unchanged record versions. When a DGM changes, create a
native new version of its storage record, import the unchanged files using
Zenodo's file-import mechanism and upload only new immutable batch ZIPs. A
previous ZIP is never edited to insert new worker files. Earlier published
versions retain their original contents. A correction goes into a new archive;
the new catalog explicitly selects the corrected logical assets while old
catalogs keep their original references. Unselected older archives may remain
in the new storage snapshot; readers must follow the catalog, not file listings.

Continue the existing catalog family rooted at record `23070787` using native
versioning. The first consolidated catalog is a **new benchmark release** with
a new release identifier and version DOI. Retain `2026.1` and its hash unchanged
in the package registry. Inspect existing remote versions/drafts before choosing
the next release identifier; `2026.2` is a candidate, not a reserved identifier.
Future catalogs are complete cumulative snapshots even though payload uploads
contain only diffs.

The current 40 storage records and their DOIs remain historical. Consolidation
creates a new active layout; it cannot erase previously minted identifiers or
make global Zenodo search contain only six records. Do not delete, withdraw,
restrict or overwrite the existing baseline to improve search appearance.

Default limits remain 100 uploaded files and 50,000,000,000 bytes per record.
Count physical ZIP files, not their logical members. Check limits before any
upload. Native new versions do not reset the logical snapshot's limits. Fail
with a useful packing report when the chosen layout no longer fits. Request an
appropriate quota/use-case agreement from Zenodo for substantial growth; do not
silently create many arbitrary records to bypass quotas. If expansion requires
additional record families, define meaningful collections and document the
agreed layout. The community itself provides no extra storage allowance.

## Archive boundaries and version stamps

Group new assets by DGM, kind, method/setting where applicable, replacement
variant where applicable, and immutable publication batch. Do not combine CSV
worker shards into one method-wide CSV. Put the original individual CSV files
inside the appropriate ZIP, retaining their exact bytes and existing method
version fields. Keep source archives and metadata separate from normal downloads.

Use predictable unique ZIP names, for example:

```text
Carter2019--data--release-2026.2--packager-0.4.1--batch-001.zip
Carter2019--results--RMA--default--release-2026.2--packager-0.4.1--batch-001.zip
Carter2019--measures--RMA--default--ordinary--release-2026.2--packager-0.4.1--batch-001.zip
```

These versions are illustrative. `packager` means the package assembling the
archive, not the version that generated every historical member. Preserve the
existing per-asset producing `package_version`, including `NULL` where unknown.
Do not invent historical generation versions, relabel them as `0.4.0`, or
assume distributed jobs all ran the same version. Newly computed workers should
provide their actual producing package versions. Release identity, packaging
version and per-worker generation/method versions are distinct metadata.

Use stable member names/paths that avoid collisions between distributed outputs.
Ensure retries use identical staged ZIP bytes: either build deterministically
or persist and verify the completed archive rather than rebuilding it differently.
Do not add per-worker README files or redundant provenance bundles. The release
catalog and existing CSV provenance are sufficient.

## Catalog schema and package reader changes

Introduce a versioned catalog extension, preferably schema version 2, while
retaining support for current schema version 1 and its direct CSV downloads.
An old release remains usable explicitly after the default changes.

Keep logical `assets` and their IDs, coverage, row counts, hashes, method fields,
replacement/measure metadata and generation provenance. Add a physical archive
table, with one descriptor per ZIP: archive ID, exact record/version ID, filename,
SHA-256, MD5, byte size and packaging/batch metadata. An archive-backed logical
asset references an archive ID and a safe relative member path while retaining
its own original size and hashes. Do not confuse a member hash with a ZIP hash.
Retain a complete member inventory for each referenced archive, including members
no longer selected after a logical correction, so reused ZIPs still validate.
Keep this information in `release.json`; no second downloadable manifest is
required. Validate all references and coverage before staging.

Reader behavior:

1. Load the selected, hash-pinned catalog; select logical assets first.
2. Resolve and deduplicate only the archives needed by that selection. Download
   each missing/corrupt archive once, verifying its size, SHA-256 and MD5.
3. Validate the member list and extract needed members into a temporary location.
   Reject unsafe paths, links, collisions and undeclared contents. Never allow
   an archive member to write outside the intended cache directory.
4. Verify each extracted logical file against its catalog size and hashes before
   making it available to retrieval functions. Commit cache files atomically.
5. Reuse verified member caches across releases and between direct-file and
   archive-backed assets when their identities match. Re-extract from a verified
   cached archive when possible. Do not include unrelated cached files by scanning
   directories, and do not equate a cached ZIP with verified extracted members.

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
Credentials must be configured separately on the other machine as `ZENODO_TOKEN`;
never commit or print them. Sandbox uses a separate `ZENODO_SANDBOX_TOKEN`.

Add reusable publisher support for the community. Resolve its slug to the UUID
and persist that resolved identity in publication state. On the live Zenodo API,
the members endpoint required the UUID even though upstream documentation also
advertised slug support. Follow actual returned links and response schemas.

Include the existing catalog and the consolidated active storage records in the
community, and use PublicationBiasBenchmark as their branded community. Keep the
historical storage records accessible through the old catalog; there is no need
to populate the community with every historical storage part immediately.
Use consistent titles and keywords identifying DGM, release and storage role.
Native version families should provide coherent history instead of one unrelated
record family per update. Verify how Zenodo carries community membership/branding
across versions and enforce it for every published version used by a release.

Give storage metadata `isPartOf` relationships to the appropriate release catalog
DOI, and catalog metadata `hasPart` relationships to its exact storage-version
DOIs. Preserve OSF `isDerivedFrom` provenance and all other existing metadata.
Reused storage versions can participate in multiple catalogs; new links must not
remove older valid relationships. These are metadata relationships, not a way
to assign the catalog's DOI to separate storage records. Reserve the new catalog
DOI early enough to establish links, but publish the catalog last.

The About page should explain the three download units and link the latest
catalog, exact-release citation instructions, package website and documentation.
The curation policy should state that official benchmark releases require
validated contents and provenance. Record pages should point users to the common
release citation even though storage records have their own DOIs.

Keep review for outside submissions. For trusted maintainers, either automate
inclusion and acceptance using the owner's permissions under the current policy,
or use Zenodo's policy allowing curators/managers/owners to submit without review.
Avoid broad automatic acceptance of ordinary community members. Submission,
acceptance and branding must be explicit, resumable publisher operations; do not
generate curator comments or send invitations as part of migration.

Community search and the concept DOI are navigation aids. Package readers use
the registry's exact catalog record and checksum, never community search order
or an unpinned "latest" result. Public downloads need no token or membership.

## Publisher changes and production execution

Extend the existing planning, staging, verification and publication functions.
Track record family IDs, version IDs, imported existing files, completed archive
uploads, the reserved catalog DOI, community inclusion requests/acceptance and
metadata completion in local resumable state. Recover lost responses by checking
remote state before creating duplicate versions, records or requests. Retain
existing rate-limit handling and native license validation (`metadata.rights`,
not legacy `license`/`licenses`). Keep public record/version identifiers in the
catalog; never store credentials there or in publication state.

Execute in this order:

1. Reconcile remote catalog versions/drafts and verify the source catalog hash.
   Build the complete source inventory from public references.
2. Download and verify source assets. Build the archives without rewriting CSVs.
   Produce a local audit mapping every old asset to its archive member and hash,
   and a packing report demonstrating limits for each proposed DGM record.
3. Implement and test the reader/publisher changes and a small sandbox publication
   covering baseline, diff update, native version import and community inclusion.
4. Reserve the next catalog version DOI. Stage the consolidated DGM records;
   verify uploaded archive metadata and contents. Publish payloads and verify
   anonymous public downloads and every logical member's original hashes.
5. Complete community inclusion/branding, native rights and DOI relationships for
   all participating records. Confirm actual accepted membership, not merely a
   successful submission response. Retry incomplete steps from saved state.
6. Publish the new catalog only after required payload and community checks pass.
   Verify its public bytes/hash, rights, relationships and membership. Run fresh
   anonymous package downloads from the new catalog in an isolated cache.
7. Add the verified catalog registry entry, retain the old entry, and change the
   package default only after verification. Update release/package metadata and
   documentation to the actual published identifiers and versions.

Publishing payloads before the catalog is a recoverable intermediate state. A
failed inclusion or verification must not advertise an incomplete new default.
Do not overwrite an existing shard implicitly: preserve the current explicit
replacement mechanism for logical corrections, including dependent measures.

## Code and documentation entry points

| Path | Work |
| --- | --- |
| `R/catalog.R` | Schema validation, archive references, listing, verified caching and extraction |
| `R/download.R` | Archive-aware selection/downloads and unchanged retrieval interfaces |
| `R/upload.R` | ZIP planning, DGM families, native versions/import, relationships, community workflow and state recovery |
| `R/prepare-resources.R` | Preserve distributed shard preparation and provenance handling |
| `inst/extdata/benchmark-releases.json` | Preserve 2026.1 and pin the verified new release |
| `tests/testthat/test-downloads.R` | Reader, selection, integrity and backwards compatibility tests |
| `tests/testthat/test-release-publication.R` | Packing, diff publication, version import, metadata and recovery tests |
| `vignettes/Benchmark_Releases.Rmd` | Replace the current raw-file/many-record explanation with the agreed layout |
| `vignettes/Using_Presimulated_Datasets.Rmd`, `vignettes/Using_Precomputed_Results.Rmd`, `vignettes/Using_Precomputed_Measures.Rmd` | Explain independent download units and filtering behavior |
| `README.Rmd`, generated `README.md`, `NEWS.md`, `DESCRIPTION`, `man/` | Keep usage, release links, package version and generated reference docs synchronized |

Check the actual source tree before adding helpers; avoid duplicating existing
coverage, cache or publication logic. Keep migration-only scripts outside Git.

## Acceptance and verification

- Both the old schema-1 `2026.1` catalog and the new archive-backed catalog work.
- Each of the three download selections fetches only the allowed DGM/kind/method
  archives; settings/replacement filters and explicit pairwise selection work.
- Distributed CSVs remain separate, and retrieval matches baseline values,
  coverage, row counts and method versions. All 1,965 original OSF files and all
  existing logical assets map to byte-identical verified members.
- Every record passes actual file-count/byte quotas, native license validation,
  public archive hash verification and member hash verification.
- A small second-release test adds one new batch, reuses unchanged files without
  uploading their contents again, preserves all old snapshots, and rejects an
  unintended overlap or replacement. Frozen existing conditions stay unchanged.
- Retries after partial upload, publish, version import or community inclusion
  recover the same state without duplicate records/requests. Failures do not
  update the default registry entry.
- Corrupt/truncated ZIPs, missing or corrupt members, unsafe paths and duplicate
  member destinations fail safely. Shared archive downloads are deduplicated;
  cached-member repairs and cross-release cache reuse work on Windows and Linux.
- Public record pages show consistent community branding, useful titles and
  release relationships. The About page explains where to start and how to cite.
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
- [Native drafts/records/version/file APIs](https://raw.githubusercontent.com/inveniosoftware/docs-invenio-rdm/master/docs/reference/rest_api_drafts_records.md)
- [Native community API](https://raw.githubusercontent.com/inveniosoftware/docs-invenio-rdm/master/docs/reference/rest_api_communities.md)
- [Native membership API](https://raw.githubusercontent.com/inveniosoftware/docs-invenio-rdm/master/docs/reference/rest_api_members.md)
- [Native request actions](https://raw.githubusercontent.com/inveniosoftware/docs-invenio-rdm/master/docs/reference/rest_api_requests.md)

Upstream InvenioRDM documentation can differ from the deployed Zenodo version.
Verify request/response formats in the sandbox and use returned API links;
do not silently fall back to legacy metadata that the native API ignores.
