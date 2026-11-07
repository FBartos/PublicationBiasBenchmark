# Plan, Stage, and Publish a Cumulative Benchmark Release

A plan combines unchanged references from a previous catalog with new
files. Changing an existing shard requires listing its ID in replace.
Archive publication builds complete changed download units, imports
unchanged ZIPs into native storage versions and continues the native
catalog family. Publication makes the complete catalog available after
anonymous access and size checks of every advertised archive,
payload/member checks of the archives it uploaded, and native rights,
DOI relationship and accepted community checks have passed.

## Usage

``` r
plan_benchmark_release(
  release,
  files,
  conditions = NULL,
  previous = NULL,
  replace = character(),
  metadata,
  state_directory,
  max_files = 100L,
  max_bytes = 5e+10,
  sandbox = FALSE,
  package_version = as.character(utils::packageVersion("PublicationBiasBenchmark")),
  source_commit = NULL,
  provenance = NULL,
  community = "publicationbiasbenchmark",
  catalog_record_id = NULL,
  catalog_concept_doi = NULL,
  max_archive_bytes = .archive_byte_limit
)

stage_benchmark_release(plan, token = NULL)

verify_benchmark_release(plan, token = NULL)

publish_benchmark_release(plan, token = NULL, confirm = NULL)
```

## Arguments

- release:

  New benchmark release identifier.

- files:

  List of descriptors returned by benchmark_resource.

- conditions:

  Named list of frozen DGM condition data frames.

- previous:

  Previous release identifier or catalog, or NULL for the baseline.

- replace:

  IDs of existing shards intentionally replaced.

- metadata:

  Zenodo-native metadata list including creators and rights (license
  IDs).

- state_directory:

  Directory for resumable publication state, outside Git.

- max_files:

  Maximum uploaded files per component record (at most 100).

- max_bytes:

  Maximum component size (at most 50 billion bytes).

- sandbox:

  Use sandbox.zenodo.org and a separate sandbox token.

- package_version:

  Package version assembling the release (generation stamps remain per
  file).

- source_commit:

  Source commit for the frozen DGM definitions.

- provenance:

  Optional source archive metadata stored in the catalog.

- community:

  Community slug or UUID. Required for archive publication.

- catalog_record_id:

  Existing catalog family version ID, required to consolidate a schema-1
  release.

- catalog_concept_doi:

  Existing catalog family concept DOI.

- max_archive_bytes:

  Uncompressed ZIP cap, at most 2,000,000,000 bytes.

- plan:

  Release plan.

- token:

  Zenodo token; defaults to ZENODO_TOKEN (ZENODO_SANDBOX_TOKEN for
  sandbox).

- confirm:

  Required to publish: the release identifier of the plan, repeated
  explicitly. Publishing is irreversible.

## Value

plan_benchmark_release returns a plan; staging returns the staged
catalog; publication returns a registry entry including the catalog's
version DOI and hash.

## Details

These functions are maintainer-only and not exported; call them as
`PublicationBiasBenchmark:::plan_benchmark_release()` (see
`.github/RELEASING.md` in the GitHub repository). Staging and publishing
first check that the token's account is an owner or manager of the
benchmark community (the gate), take an exclusive lock on the state
directory and bind the state to the plan. The gate protects this
package's entry points; it is not a security boundary for Zenodo itself.
