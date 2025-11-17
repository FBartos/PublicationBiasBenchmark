# Upload Datasets of a DGM

This function uploads datasets of a specified Data-Generating Mechanism
(DGM) to the OSF repository at <https://osf.io/exf3m/>.

This is an internal function intended for the benchmark maintainer. It
requires OSF repository authentication (via
[`osfr::osf_auth()`](https://docs.ropensci.org/osfr/reference/osf_auth.html))
and repository access.

## Usage

``` r
upload_dgm_datasets(dgm_name, overwrite = FALSE, progress = TRUE, max_try = 10)

upload_dgm_results(dgm_name, overwrite = FALSE, progress = TRUE, max_try = 10)

upload_dgm_measures(dgm_name, overwrite = TRUE, progress = TRUE, max_try = 10)
```

## Arguments

- dgm_name:

  Character string specifying the name of the DGM dataset to upload.

- overwrite:

  Logical indicating whether to overwrite existing files on OSF.
  Defaults to `TRUE` for performance measures and `FALSE` for results
  and datasets

- progress:

  Logical indicating whether to show progress uploading files. Defaults
  to `TRUE`.

- max_try:

  Integet specifying how many times should the function attempt
  reconnecting to OSF upon failure.

## Value

`TRUE` if the upload was successful, otherwise an error is raised.
