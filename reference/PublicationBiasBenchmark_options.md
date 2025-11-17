# Options for the PublicationBiasBenchmark package

A placeholder object and functions for the PublicationBiasBenchmark
package.

## Usage

``` r
PublicationBiasBenchmark.options(...)

PublicationBiasBenchmark.get_option(name)
```

## Arguments

- ...:

  named option(s) to change - for a list of available options, see
  details below.

- name:

  the name of the option to get the current value of - for a list of
  available options, see details below.

## Value

The current value of all available PublicationBiasBenchmark options
(after applying any changes specified) is returned invisibly as a named
list.

## Details

- `"resources_directory"`:

  Location where the benchmark data/results/measures are stored

- `"prompt_for_download"`:

  Whether each file download should ask for explicit approval
