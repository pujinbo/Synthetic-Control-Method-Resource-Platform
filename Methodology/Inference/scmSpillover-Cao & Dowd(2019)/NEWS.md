# scmSpillover 0.1.1

## New Features

* Added comprehensive unit tests using `testthat` framework
* Added `generate_test_data()` function for simulation studies

## Improvements

* Improved documentation with roxygen2 comments for all exported functions
* Added input validation and informative error messages
* Enhanced numerical stability in `scm()` and `sp_estimation()` with regularization

## Bug Fixes

* Fixed weight rearrangement issue in `scm_batch()` when processing non-first units

---
 
# scmSpillover 0.1.0

*Initial release*
 
## Core Functions

* `run_scm_spillover()`: Main function for SCM analysis with spillover effects
* `scm()`: Basic synthetic control method for single treated unit
* `scm_batch()`: Batch SCM estimation for all units
* `sp_estimation()`: Spillover-adjusted treatment effect estimation
* `vanilla_scm()`: Standard SCM without spillover adjustment for comparison

## Inference

* `sp_andrews_te()`: Andrews-type test for treatment effects with confidence intervals
* `sp_andrews_spillover()`: Test for presence of spillover effects

## Visualization

* `plot_effects()`: Plot treatment effects with confidence intervals
* `plot_all()`: Generate comprehensive visualization suite
* `qplot_effects()`, `qplot_compare()`, `qplot_series()`, `qplot_ci()`: Quick plotting functions
* `save_all_plots()`: Export all plots to files

## Methods

* `print.scm_spillover()`: Print method for results summary
* `summary.scm_spillover()`: Detailed summary with period-by-period results
* `plot.scm_spillover()`: S3 plot method

## Data

* `generate_test_data()`: Generate simulated panel data for testing


