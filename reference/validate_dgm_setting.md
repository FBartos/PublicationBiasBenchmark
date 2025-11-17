# Validate DGM Settings

This function validates the settings provided for a given Data
Generating Mechanism (DGM).

## Usage

``` r
validate_dgm_setting(dgm_name, settings)
```

## Arguments

- dgm_name:

  Character string specifying the DGM type

- settings:

  List containing the required parameters for the DGM or numeric
  condition_id

## Value

Error or `TRUE` depending whether the settings are valid for the
specified DGM.

## Examples

``` r
validate_dgm_setting("Carter2019", list(mean_effect = 0,
                        effect_heterogeneity = 0, bias = "high",
                        QRP = "high", n_studies = 10))

validate_dgm_setting("Alinaghi2018", list(environment = "FE",
                        mean_effect = 0, bias = "positive"))

validate_dgm_setting("Stanley2017", list(environment = "SMD",
                        mean_effect = 0,
                        effect_heterogeneity = 0, bias = 0, n_studies = 5,
                        sample_sizes = c(32,64,125,250,500)))
```
