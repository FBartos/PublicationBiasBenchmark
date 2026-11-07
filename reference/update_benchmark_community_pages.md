# Update a Benchmark Community's About and Curation Pages

Explicitly update the community's HTML pages while preserving its
identity, other metadata, submission policies and membership settings.
This is separate from release publication; review and approve production
page content before calling it. Sandbox uses a separate account and
token. The function is maintainer-only and not exported
(`PublicationBiasBenchmark:::update_benchmark_community_pages()`). It
requires `confirm`, checks that the token's account owns the community,
and saves the current pages to `backup_directory` before replacing them.
After the update the stored text of both pages is compared with the
submitted HTML (whitespace collapsed); a difference gives a warning,
because Zenodo may sanitize HTML.

## Usage

``` r
update_benchmark_community_pages(
  community,
  about,
  curation_policy,
  sandbox = FALSE,
  token = NULL,
  confirm = NULL,
  backup_directory = NULL
)
```

## Arguments

- community:

  Community slug or UUID.

- about:

  HTML for the About page.

- curation_policy:

  HTML for the curation policy page.

- sandbox:

  Use sandbox.zenodo.org.

- token:

  Zenodo token; defaults to the matching environment variable.

- confirm:

  Required: the `community` value, repeated explicitly.

- backup_directory:

  Directory that receives a verified JSON copy of the current pages
  before they are replaced. NULL (default) uses the user's data
  directory for the package
  (`tools::R_user_dir("PublicationBiasBenchmark", "data")` on R 4.0 or
  later, `~/.PublicationBiasBenchmark` on older R versions).

## Value

The verified updated community metadata, invisibly.
