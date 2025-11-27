# 0.1.3
## New Features
 - Added `measure()` function to list available performance measures (renamed from `measures()`).
 - Added `measure_mcse()` function to list available performance measure MCSE functions.
 - Implemented S3 methods for `measure()` and `measure_mcse()` to retrieve specific functions (e.g., `measure("bias")`, `measure_mcse("bias")`).
 - Updated `method()` and `dgm()` to list available methods and DGMs when called without arguments.
 - Updated `method()` and `dgm()` to return the corresponding function when called with a single argument (e.g., `method("RMA")`).
 - `measure()`, `measure_mcse()`, `method()`, and `dgm()` now dynamically retrieve available options using `methods()`.

# 0.1.2
## Fixes
 - Vignette updates
 - Stop download if OSF_PAT is missing (due to errors in the osfr package)
 
# 0.1.1
## Fixes
 - Documentation updates

# 0.1.0
Initial CRAN submission.
