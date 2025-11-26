# 0.1.3
## New Features
 - Added `measures()` function to list available performance measures.
 - Implemented S3 methods for `measures()` to retrieve specific measure functions (e.g., `measures("bias")`).
 - Updated `method()` and `dgm()` to list available methods and DGMs when called without arguments.
 - Updated `method()` and `dgm()` to return the corresponding function when called with a single argument (e.g., `method("RMA")`).
 - `measures()`, `method()`, and `dgm()` now dynamically retrieve available options using `methods()`.

# 0.1.2
## Fixes
 - Vignette updates
 - Stop download if OSF_PAT is missing (due to errors in the osfr package)
 
# 0.1.1
## Fixes
 - Documentation updates

# 0.1.0
Initial CRAN submission.
