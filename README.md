
<!-- README.md is generated from README.Rmd. Please edit that file -->

# PublicationBiasBenchmark

An R package for benchmarking publication bias correction methods
through simulation studies.

## Installation

``` r
# Install from GitHub
devtools::install_github("FBartos/PublicationBiasBenchmark")
```

## Usage

### Basic Example

``` r
library(PublicationBiasBenchmark)

# define sim setting
sim_settings <- list(
  n_studies     = 100,
  mean_effect   = 0.3,
  heterogeneity = 0.1
)

# check whether it is feasible
# (defined outside of the function - not to decrease performance during simulation)
validate_dgm_settigns("no_bias", sim_settings)

# simulate the data
df <- dgm("no_bias", sim_settings)

# fit a method
method("PET", df)
```

### Key Functions

- `validate_dgm_settigns()`: Validates data generating mechanism
  settings before simulation
- `dgm()`: Generates simulated data according to specified bias model
  and settings
- `method()`: Applies publication bias correction methods to simulated
  data

### Available DGM Models

- `"no_bias"`: Generates data without publication bias (a test
  simulation)
- …

### Available Methods

- `"PET"`: Publication bias correction using PET (Precision-Effect Test)
- …

## Documentation

For detailed documentation of all functions and parameters, use:

``` r
?dgm
?method
?validate_dgm_settigns
```
