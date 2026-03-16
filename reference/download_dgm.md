# Download Datasets/Results/Measures of a DGM

This function downloads datasets/results/measures of a specified
Data-Generating Mechanism (DGM) from the OSF repository
(<https://osf.io/exf3m/>). The datasets/results/measures are saved to
the location specified via
`PublicationBiasBenchmark.options(resources_directory = "/path/")`. To
set the location permanently, specify the
PublicationBiasBenchmark_RESOURCES environment variable. The data are
stored in dgm_name/datasets, dgm_name/results, dgm_name/measures
subfolders.

## Usage

``` r
download_dgm_datasets(
  dgm_name,
  overwrite = FALSE,
  progress = TRUE,
  max_try = 10
)

download_dgm_results(
  dgm_name,
  overwrite = FALSE,
  progress = TRUE,
  max_try = 10
)

download_dgm_measures(
  dgm_name,
  overwrite = FALSE,
  progress = TRUE,
  max_try = 10
)
```

## Arguments

- dgm_name:

  Character string specifying the name of the DGM dataset to download.

- overwrite:

  Logical indicating whether to overwrite existing files. Defaults to
  `FALSE`, which means only missing files will be downloaded.

- progress:

  Logical indicating whether to show progress downloading files.
  Defaults to `TRUE`.

- max_try:

  Integet specifying how many times should the function attempt
  reconnecting to OSF upon failure.

## Value

`TRUE` if the download was successful, otherwise an error is raised.

## Examples

``` r
if (FALSE) { # \dontrun{
  download_dgm_datasets("no_bias")
} # }
```
