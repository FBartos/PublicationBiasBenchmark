README
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![CRAN
check](https://github.com/FBartos/PublicationBiasBenchmark/workflows/R-CMD-check/badge.svg)](https://github.com/FBartos/PublicationBiasBenchmark/actions)
[![R
tests](https://github.com/FBartos/PublicationBiasBenchmark/workflows/test-package/badge.svg)](https://github.com/FBartos/PublicationBiasBenchmark/actions)
[![Benchmark
Site](https://github.com/FBartos/PublicationBiasBenchmark/workflows/pkgdown/badge.svg)](https://github.com/FBartos/PublicationBiasBenchmark/actions)
[![CRAN
status](https://www.r-pkg.org/badges/version/PublicationBiasBenchmark)](https://CRAN.R-project.org/package=PublicationBiasBenchmark)
<!-- badges: end -->

# PublicationBiasBenchmark

**PublicationBiasBenchmark** is an R package for benchmarking
publication bias correction methods through simulation studies. It
provides:  
- Predefined data-generating mechanisms from the literature  
- Functions for running meta-analytic methods on simulated data  
- Pre-simulated datasets and pre-computed results for reproducible
benchmarks  
- Tools for visualizing and comparing method performance

All datasets and results are hosted on OSF:
<https://doi.org/10.17605/OSF.IO/EXF3M>

For the methodology of living synthetic benchmarks please cite:

> Bartoš, F., Pawel, S., & Siepe, B. S. (2025). Living synthetic
> benchmarks: A neutral and cumulative framework for simulation studies.
> *arXiv Preprint*. <https://doi.org/10.48550/arXiv.2510.19489>

For the publication bias benchmark R package please cite:

> Bartoš, F., Pawel, S., & Siepe, B. S. (2025).
> PublicationBiasBenchmark: Benchmark for publication bias correction
> methods (version 0.1.0).
> <https://github.com/FBartos/PublicationBiasBenchmark>

Overviews of the benchmark results are available as articles on the
package website:

- [Overall
  Results](https://fbartos.github.io/PublicationBiasBenchmark/articles/Results.html)
- [Stanley
  (2017)](https://fbartos.github.io/PublicationBiasBenchmark/articles/Results_Stanley2017.html)
- [Alinaghi
  (2018)](https://fbartos.github.io/PublicationBiasBenchmark/articles/Results_Alinaghi2018.html)
- [Bom
  (2019)](https://fbartos.github.io/PublicationBiasBenchmark/articles/Results_Bom2019.html)
- [Carter
  (2019)](https://fbartos.github.io/PublicationBiasBenchmark/articles/Results_Carter2019.html)

Contributor guidelines for extending the package with data-generating
mechanisms, methods, and results are available at:

- [How to add a new data-generating
  mechanism](https://fbartos.github.io/PublicationBiasBenchmark/articles/Adding_New_DGMs.html)
- [How to add a new
  method](https://fbartos.github.io/PublicationBiasBenchmark/articles/Adding_New_Methods.html)
- [How to compute method
  results](https://fbartos.github.io/PublicationBiasBenchmark/articles/Computing_Method_Results.html)
- [How to compute method
  measures](https://fbartos.github.io/PublicationBiasBenchmark/articles/Computing_Method_Measures.html)

Illustrations of how to use the precomputed datasets, results, and
measures are available at:

- [How to use presimulated
  datasets](https://fbartos.github.io/PublicationBiasBenchmark/articles/Using_Presimulated_Datasets.html)
- [How to use precomputed
  results](https://fbartos.github.io/PublicationBiasBenchmark/articles/Using_Precomputed_Results.html)
- [How to use precomputed
  measures](https://fbartos.github.io/PublicationBiasBenchmark/articles/Using_Precomputed_Measures.html)

The rest of this file overviews the main features of the package.

## Installation

Install the released CRAN version

``` r
install.packages("PublicationBiasBenchmark")
```

Install latest development version from GitHub

``` r
remotes::install_github("FBartos/PublicationBiasBenchmark")
```

### Versions

Addition or modification of a method or data-generating mechanisms is
always reflected in a minor version update. Minor changes to
infrastructure etc are reflected in patch updates.

## Usage

``` r
library(PublicationBiasBenchmark)
```

### Simulating From Existing Data-Generating Mechanisms

``` r
# Obtain a data.frame with pre-defined conditions
dgm_conditions("Stanley2017")

# simulate the data from the second condition
df <- simulate_dgm("Stanley2017", 2)

# fit a method
run_method("RMA", df)
```

### Using Pre-Simulated Datasets

``` r
# download the pre-simulated datasets
# (the intended location for storing the package resources needs to be specified)
PublicationBiasBenchmark.options(resources_directory = "/path/to/files")
download_dgm_datasets("no_bias")

# retrieve first repetition of first condition from the downloaded datasets
retrieve_dgm_dataset("no_bias", condition_id = 1, repetition_id = 1)
```

### Using Pre-Computed Results

``` r
# download the pre-computed results
download_dgm_results("no_bias")

# retrieve results the first repetition of first condition of RMA from the downloaded results
retrieve_dgm_results("no_bias", method = "RMA", condition_id = 1, repetition_id = 1)

# retrieve all results across all conditions and repetitions
retrieve_dgm_results("no_bias")
```

### Using Pre-Computed Measures

``` r
# download the pre-computed measures
download_dgm_measures("no_bias")

# retrieve measures of bias the first condition of RMA from the downloaded results
retrieve_dgm_measures("no_bias", measure = "bias", method = "RMA", condition_id = 1)

# retrieve all measures across all conditions and measures
retrieve_dgm_measures("no_bias")
```

### Simulating From an Existing DGM With Custom Settings

``` r
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
run_method("RMA", df)
```

### Key Functions

#### Data-Generating Mechanisms

- `simulate_dgm()`: Generates simulated data according to specified
  data-generating mechanism and settings.
- `dgm_conditions()`: Lists prespecified conditions of the
  data-generating mechanism.
- `validate_dgm_setting()`: Validates (custom) setting of the
  data-generating mechanism.
- `download_dgm_datasets()`: Downloads pre-simulated datasets from the
  OSF repository.
- `retrieve_dgm_dataset()`: Retrieves the pre-simulated dataset of a
  given condition and repetition from downloaded from the pre-downloaded
  OSF repository.

#### Method Estimation And Results

- `run_method()`: Estimates method on a supplied data according to the
  specified settings.
- `method_settings()`: Lists prespecified settings of the method.
- `download_dgm_results()`: Downloads pre-computed results from the OSF
  repository.
- `retrieve_dgm_results()`: Retrieves the pre-computed results of a
  given method, condition, and repetition from the pre-downloaded OSF
  repository.

#### Performance measures And Results

- `bias()`, `bias_mcse()`, etc.: Functions to compute performance
  measures and their Monte Carlo standard errors.
- `download_dgm_measures()`: Downloads pre-computed performance measures
  from the OSF repository.
- `retrieve_dgm_measures()`: Retrieves the pre-computed performance
  measures of a given method, condition, and repetition from the
  pre-downloaded OSF repository.

### Available Data-Generating Mechanisms

See `methods("dgm")` for the full list:

- `"no_bias"`: Generates data without publication bias (a test
  simulation)
- `"Stanley2017"`: Stanley et al. (2017)
- `"Alinaghi2018"`: Alinaghi & Reed (2018)
- `"Bom2019"`: Bom & Rachinger (2019)
- `"Carter2019"`: Carter et al. (2019)

### Available Methods

See `methods("method")` for the full list:

- `"mean"`: Mean effects size
- `"FMA"`: Fixed effects meta-analysis
- `"RMA"`: Random effects meta-analysis
- `"WLS"`: Weighted Least Squares
- `"trimfill"`: Trim-and-Fill (Duval & Tweedie, 2000)
- `"WAAPWLS"`: Weighted Least Squares - Weighted Average of Adequately
  Power Studies (Stanley et al., 2017)
- `"WILS"`: Weighted and Iterated Least Squares (Stanley & Doucouliagos,
  2024)
- `"PET"`: Precision-Effect Test (PET) publication bias adjustment
  (Stanley & Doucouliagos, 2014)
- `"PEESE"`: Precision-Effect Estimate with Standard Errors (PEESE)
  publication bias adjustment (Stanley & Doucouliagos, 2014)
- `"PETPEESE"`: Precision-Effect Test and Precision-Effect Estimate with
  Standard Errors (PET-PEESE) publication bias adjustment (Stanley &
  Doucouliagos, 2014)
- `"EK"`: Endogenous Kink (Bom & Rachinger, 2019)
- `"SM"`: Selection Models (3PSM, 4PSM) (Vevea & Hedges, 1995)
- `"pcurve"`: P-curve (Simonsohn et al., 2014)
- `"puniform"`: P-uniform (van Assen et al., 2015) and P-uniform\* (van
  Aert & van Assen, 2025)
- `"AK"`: Andrews & Kasy selection models (AK1, AK2) (Andrews & Kasy,
  2019)
- `"RoBMA"`: Robust Bayesian Meta-Analysis (Bartoš et al., 2023)

### Available Performance Measures

See `?measures` for the full list of performance measures and their
Monte Carlo standard errors/

### DGM OSF Repositories

All DGMs are linked to the OSF repository (<https://osf.io/exf3m/>) and
contain the following elements:

- `data` : folder containing by-condition simulated datasets for all
  repetitions
- `results` : folder containing by-method results for all conditions \*
  repetitions
- `measures` : folder containing by-measure performance for all methods
  \* conditions
- `metadata` : folder containing the following information:
  - `dgm-conditions.csv` : file mapping of all conditions and the
    corresponding settings
  - `dgm-generation.R` : file with code for exact reproduction of the
    pre-simulated datasets
  - `dgm-sessionInfo.txt`: file with reproducibility details for the
    pre-simulated datasets
  - `dgm-session.log`: file with reproducibility details for the
    pre-simulated datasets (based on sessioninfo package)
  - `results.R` : file with code for exact reproduction of the by method
    results (might be method / method groups specific)
  - `results-sessionInfo.txt`: file with reproducibility details for the
    precomputed results (might be method / method groups specific)
  - `pm-computation.R` : file with code for computation of performance
    measures

### References

<div id="refs" class="references csl-bib-body hanging-indent"
entry-spacing="0" line-spacing="2">

<div id="ref-alinaghi2018meta" class="csl-entry">

Alinaghi, N., & Reed, W. R. (2018). Meta-analysis and publication bias:
How well does the FAT-PET-PEESE procedure work? *Research Synthesis
Methods*, *9*(2), 285–311. <https://doi.org/10.1002/jrsm.1298>

</div>

<div id="ref-andrews2019identification" class="csl-entry">

Andrews, I., & Kasy, M. (2019). Identification of and correction for
publication bias. *American Economic Review*, *109*(8), 2766–2794.
<https://doi.org/10.1257/aer.20180310>

</div>

<div id="ref-bartos2023robust" class="csl-entry">

Bartoš, F., Maier, M., Wagenmakers, E.-J., Doucouliagos, H., & Stanley,
T. (2023). Robust bayesian meta-analysis: Model-averaging across
complementary publication bias adjustment methods. *Research Synthesis
Methods*, *14*(1), 99–116. <https://doi.org/10.1002/jrsm.1594>

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

<div id="ref-duval2000trim" class="csl-entry">

Duval, S. J., & Tweedie, R. L. (2000). Trim and fill: A simple
funnel-plot-based method of testing and adjusting for publication bias
in meta-analysis. *Biometrics*, *56*(2), 455–463.
<https://doi.org/10.1111/j.0006-341X.2000.00455.x>

</div>

<div id="ref-simonsohn2014pcurve" class="csl-entry">

Simonsohn, U., Nelson, L. D., & Simmons, J. P. (2014). P-curve and
effect size: Correcting for publication bias using only significant
results. *Perspectives on Psychological Science*, *9*(6), 666–681.
<https://doi.org/10.1177/1745691614553988>

</div>

<div id="ref-stanley2014meta" class="csl-entry">

Stanley, T. D., & Doucouliagos, H. (2014). Meta-regression
approximations to reduce publication selection bias. *Research Synthesis
Methods*, *5*(1), 60–78. <https://doi.org/10.1002/jrsm.1095>

</div>

<div id="ref-stanley2024harnessing" class="csl-entry">

Stanley, T. D., & Doucouliagos, H. (2024). Harnessing the power of
excess statistical significance: Weighted and iterative least squares.
*Psychological Methods*, *29*(2), 407–420.
<https://doi.org/10.1037/met0000502>

</div>

<div id="ref-stanley2017finding" class="csl-entry">

Stanley, T. D., Doucouliagos, H., & Ioannidis, J. P. (2017). Finding the
power to reduce publication bias. *Statistics in Medicine*, *36*(10),
1580–1598. <https://doi.org/10.1002/sim.7228>

</div>

<div id="ref-vanaert2025puniform" class="csl-entry">

van Aert, R. C. M., & van Assen, M. A. L. M. (2025). Correcting for
publication bias in a meta-analysis with the p-uniform\* method.
*Psychonomic Bulletin & Review*.
<https://osf.io/preprints/metaarxiv/zqjr9/>

</div>

<div id="ref-vanassen2015meta" class="csl-entry">

van Assen, M. A. L. M., van Aert, R. C. M., & Wicherts, J. M. (2015).
Meta-analysis using effect size distributions of only statistically
significant studies. *Psychological Methods*, *20*(3), 293–309.
<https://doi.org/10.1037/met0000025>

</div>

<div id="ref-vevea1995general" class="csl-entry">

Vevea, J. L., & Hedges, L. V. (1995). A general linear model for
estimating effect size in the presence of publication bias.
*Psychometrika*, *60*(3), 419–435. <https://doi.org/10.1007/BF02294384>

</div>

</div>
