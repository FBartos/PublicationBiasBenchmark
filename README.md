README
================

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
validate_dgm_setting("no_bias", sim_settings)

# simulate the data
df <- simulate_dgm("no_bias", sim_settings)

# fit a method
method("PET", df)
```

### Using Pre-Simulated Datasets

``` r
library(PublicationBiasBenchmark)

# download the pre-simulated datasets to "sims" folder
download_folder <- file.path(getwd(), "sims")
download_dgm_datasets("no_bias", path = download_folder)

# retrieve first repetition of first condition from the downloaded datasets
retrieve_dgm_dataset("no_bias", condition_id = 1, repetition_id = 1, path = download_folder)
```

### Key Functions

- `simulate_dgm()`: Generates simulated data according to specified data
  generating model and settings.
- `dgm_settings()`: Lists prespecified settings of the data generating
  mechanism.
- `validate_dgm_setting()`: Validates setting of the data generating
  mechanism.
- `download_dgm_datasets()`: Downloads pre-simulated datasets from the
  DGM’s OSF repository.
- `retrieve_dgm_dataset()`: Retrieves the condition and repetition of
  the pre-simulated dataset downloaded from the DGM’s OSF repository.

### Available DGM Models

- `"no_bias"`: Generates data without publication bias (a test
  simulation)
- `"Stanley2017"`: Stanley et al. (2017)
- `"Alinaghi2018"`: Alinaghi & Reed (2018)
- `"Bom2019"`: Bom & Rachinger (2019)
- `"Carter2019"`: Carter et al. (2019)

### Available Methods

- `"RMA"`: Random effects meta-analysis
- `"PET"`: Precision-Effect Test (PET) publication bias adjustment
- …

### DGM OSF Repositories

All DGM are linked to OSF repositories containing the following
elements:

- `data` : folder containing by-condition simulated datasets for all
  repetitions
- `results` : folder containing by-method results for all conditions \*
  repetitions
- `metadata` : folder containing the following information:
  - `conditions.csv` : file mapping of all conditions and the
    corresponding settings
  - `data_generation.R` : file with code and reproducibility details for
    exact reproduction of the pre-simulated datasets
  - `methods.R` : file with code and reproducibility details for exact
    reproduction of the by method results

Link to the OSF repository can be accessed via the `dgm_repository()`
function.

### References

<div id="refs" class="references csl-bib-body hanging-indent"
entry-spacing="0" line-spacing="2">

<div id="ref-alinaghi2018meta" class="csl-entry">

Alinaghi, N., & Reed, W. R. (2018). Meta-analysis and publication bias:
How well does the FAT-PET-PEESE procedure work? *Research Synthesis
Methods*, *9*(2), 285–311. <https://doi.org/10.1002/jrsm.1298>

</div>

<div id="ref-bom2019kinked" class="csl-entry">

Bom, P. R., & Rachinger, H. (2019). A kinked meta-regression model for
publication bias correction. *Research Synthesis Methods*, *10*(4),
497–514. <https://doi.org/10.1002/jrsm.1352>

</div>

<div id="ref-carter2019correcting" class="csl-entry">

Carter, E. C., Schönbrodt, F. D., Gervais, W. M., & Hilgard, J. (2019).
Correcting for bias in psychology: A comparison of meta-analytic
methods. *Advances in Methods and Practices in Psychological Science*,
*2*(2), 115–144. <https://doi.org/10.1177/2515245919847196>

</div>

<div id="ref-stanley2017finding" class="csl-entry">

Stanley, T. D., Doucouliagos, H., & Ioannidis, J. P. (2017). Finding the
power to reduce publication bias. *Statistics in Medicine*, *36*(10),
1580–1598. <https://doi.org/10.1002/sim.7228>

</div>

</div>
