# Return Pre-specified DGM Settings

This function returns the list of pre-specified settings for a given
Data Generating Mechanism (DGM).

## Usage

``` r
dgm_conditions(dgm_name)

get_dgm_condition(dgm_name, condition_id)
```

## Arguments

- dgm_name:

  Character string specifying the DGM type

- condition_id:

  which conditions should settings be returned for.

## Value

A data frame containing the pre-specified settings including a
`condition_id` column which maps settings id to the corresponding
settings.

## Examples

``` r
head(dgm_conditions("Carter2019"))
#>   n_studies mean_effect  QRP bias effect_heterogeneity condition_id
#> 1        10         0.0 none none                    0            1
#> 2        30         0.0 none none                    0            2
#> 3        60         0.0 none none                    0            3
#> 4       100         0.0 none none                    0            4
#> 5        10         0.2 none none                    0            5
#> 6        30         0.2 none none                    0            6
get_dgm_condition("Carter2019", condition_id = 1)
#>   n_studies mean_effect  QRP bias effect_heterogeneity condition_id
#> 1        10           0 none none                    0            1

head(dgm_conditions("Alinaghi2018"))
#>   environment mean_effect bias condition_id
#> 1          RE         0.0 none            1
#> 2         PRE         0.0 none            2
#> 3          FE         0.0 none            3
#> 4          RE         0.5 none            4
#> 5         PRE         0.5 none            5
#> 6          FE         0.5 none            6

head(dgm_conditions("Stanley2017"))
#>   mean_effect effect_heterogeneity bias n_studies environment
#> 1         0.0               0.0000    0         5         SMD
#> 2         0.5               0.0000    0         5         SMD
#> 3         0.0               0.0625    0         5         SMD
#> 4         0.5               0.0625    0         5         SMD
#> 5         0.0               0.1250    0         5         SMD
#> 6         0.5               0.1250    0         5         SMD
#>            sample_sizes condition_id
#> 1 32, 64, 125, 250, 500            1
#> 2 32, 64, 125, 250, 500            2
#> 3 32, 64, 125, 250, 500            3
#> 4 32, 64, 125, 250, 500            4
#> 5 32, 64, 125, 250, 500            5
#> 6 32, 64, 125, 250, 500            6
```
