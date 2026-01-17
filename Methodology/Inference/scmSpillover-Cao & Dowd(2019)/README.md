# scmSpillover - Synthetic Control Method with Spillover Effects R Package

## Introduction

`scmSpillover` is an R package that implements the Synthetic Control Method with Spillover Effects. This package is based on the methodology of Cao and Dowd (2019), addressing the limitation of traditional synthetic control methods that ignore interdependence between units.

### Key Features

- **Spillover Effects Modeling**: Accounts for indirect impacts of treated units on other units
- **Comparative Analysis**: Provides results for both traditional SCM and spillover-adjusted SCM simultaneously
- **Visualization Tools**: Built-in professional chart generation capabilities
- **Flexible and General**: Applicable to any panel data, not limited to specific domains

## Installation

### Install from Local Source
```r
# After downloading the source code
install.packages("path/to/scmSpillover_0.1.1.tar.gz", repos = NULL, type = "source")
```

## Quick Start

```r
library(scmSpillover)

# View basic package information
packageDescription("scmSpillover")

# List all functions in the package
ls("package:scmSpillover")
```

### Basic Example

```r
library(scmSpillover)
packageDescription("scmSpillover")

# Load example data
cigs_path <- system.file("extdata", "cigs.rda", package = "scmSpillover")
load(cigs_path)

help(package = "scmSpillover")

result <- run_scm_spillover(
  data = cigs,
  treatment_start = 20,
  affected_units = c(1, 5, 6, 9, 15, 23, 24, 25, 29, 34, 35)
)

# View results
print(result)
summary(result)

# Generate visualizations
# Plot with confidence bands.For other available plotting options, please refer to the visualization section.
qplot_ci(result, start_year = 1989, unit_name = "California")
```

## Core Functions

### 1. Main Analysis Function

#### `run_scm_spillover()`
Performs complete SCM analysis with spillover effects

**Parameters:**
- `data`: Panel data matrix (periods x units)
- `treatment_start`: Treatment start period (integer)
- `treated_unit`: Column index of treated unit (default = 1)
- `affected_units`: Vector of indices for all affected units
- `verbose`: Whether to display progress information

**Returns:**
- `spillover_effects`: Treatment effects adjusted for spillovers
- `vanilla_effects`: Traditional SCM treatment effects
- `ci_lower`, `ci_upper`: 95% confidence intervals
- `synthetic`: Synthetic control unit values

### 2. Visualization Functions

#### Quick Plotting Functions 

`scmSpillover` now provides a comprehensive set of quick plotting functions for immediate visualization:

##### `qplot_ci()` - Effects with Confidence Intervals
```r
# Quick plot with confidence bands
qplot_ci(result, start_year = 1989, unit_name = "California")
```

##### `qplot_point()` - Clean Point Estimates
```r
# Plot without confidence bands for cleaner visualization
qplot_point(result, start_year = 1989)
```

##### `qplot_compare()` - Method Comparison
```r
# Compare spillover-adjusted vs standard SCM
qplot_compare(result, start_year = 1989)
```

##### `qplot_series()` - Time Series
```r
# Show actual vs synthetic control over time
qplot_series(result, start_year = 1989, treatment_label = "Prop 99")
```

##### `qplot_all()` - All Plots in Sequence
```r
# Generate all three main plots with one command
qplot_all(result, 
          start_year = 1989,
          show_ci = TRUE)  # Set FALSE for no confidence bands

# Non-interactive version (no pause between plots)
qplot_all(result, start_year = 1989, pause = FALSE)
```

##### `save_all_plots()` - Export All Visualizations
```r
# Save all plot variants to files
save_all_plots(result,
               start_year = 1989,
               prefix = "california_analysis",
               path = "figures/",
               width = 10,
               height = 6,
               dpi = 300)
# This creates:
# - california_analysis_effects_ci.png
# - california_analysis_effects_point.png
# - california_analysis_method_comparison.png
# - california_analysis_time_series.png
```

#### Traditional Plotting Functions

##### `plot_all()` - Interactive Analysis Suite
Generates three comprehensive charts with pause between each:

```r
plots <- plot_all(
  result,                    
  start_year = 1989,        
  unit_name = "California",     
  outcome_label = "Cigarette Sales (packs per capita)",
  treatment_label = "Proposition 99",
  show_ci = TRUE  # New parameter to control confidence bands
)
```

This generates:
1. **Method Comparison**: Spillover-adjusted vs Standard SCM effects
2. **Treatment Effects with CI**: Point estimates with 95% confidence intervals (now without error bars, only ribbon)
3. **Time Series**: Actual values vs synthetic control

##### `plot_effects()` - Customizable Effect Plot
```r
# Basic effect plot with confidence intervals
p1 <- plot_effects(result, 
                   start_year = 1989,
                   show_ci = TRUE,
                   show_vanilla = FALSE)

# Compare with vanilla SCM
p2 <- plot_effects(result,
                   start_year = 1989,
                   show_ci = TRUE,
                   show_vanilla = TRUE)  # Adds dashed line for standard SCM

# Clean version without CI
p3 <- plot_effects(result,
                   start_year = 1989,
                   show_ci = FALSE,
                   show_vanilla = FALSE)
```

```

#### Customization Options

All plotting functions support these common parameters:
- `start_year`: First year of treatment (for x-axis labeling)
- `unit_name`: Name of treated unit for titles
- `outcome_label`: Y-axis label
- `treatment_label`: Label for treatment intervention (in time series plots)
- `show_ci`: Whether to display confidence intervals (where applicable)

The plots use a consistent color scheme:
- **Spillover-adjusted effects**: Blue (#0072B2)
- **Standard SCM**: Orange (#D55E00)
- **Actual values**: Yellow-Orange (#E69F00)
- **Zero reference line**: Red (dashed)

### 3. Low-level Functions

#### Core Estimation Functions
- `sp_estimation()`: Spillover-adjusted estimation
- `sp_andrews_te()`: Andrews treatment effect test
- `sp_andrews_spillover()`: Spillover effect test

#### Traditional SCM Functions
- `scm()`: Synthetic control for single unit
- `scm_batch()`: Batch computation for all units
- `vanilla_scm()`: Standard SCM (for comparison)

## Practical Application Cases

### Case 1: Policy Evaluation (California Cigarette Tax)

```r
# Load cigarette sales data
cigs_path <- system.file("extdata", "cigs.rda", package = "scmSpillover")
load(cigs_path)

# Analyze Proposition 99 impact
result_cigs <- run_scm_spillover(
  data = cigs,
  treatment_start = 20,  # 1989
  treated_unit = 1,       # California
  affected_units = c(1, 5, 6, 9, 15, 23, 24, 25, 29, 34, 35)
)

# Visualize results
plots <- plot_all(
  result_cigs,
  start_year = 1989,
  unit_name = "California",
  outcome_label = "Per Capita Cigarette Sales (packs)",
  treatment_label = "Proposition 99"
)

# Extract key results
cat("Average Treatment Effect (with spillovers):", mean(result_cigs$spillover_effects), "\n")
cat("Average Treatment Effect (traditional):", mean(result_cigs$vanilla_effects), "\n")
```

### Case 2: Testing with Simulated Data

```r
# Generate test data
test_data <- generate_test_data(
  n_units = 20,
  n_periods = 30,
  treatment_start = 20,
  effect_size = -15
)

# Run analysis
result_test <- run_scm_spillover(
  data = test_data,
  treatment_start = 20,
  affected_units = c(1, 2, 3)  # Assume spillovers to units 2 and 3
)

plots <- plot_all(result_test, 
                  start_year = 2001,
                  unit_name = "Treatment Group",
                  outcome_label = "Outcome Variable")
```

## Methodological Notes

### Importance of Spillover Effects

Traditional SCM assumes SUTVA (Stable Unit Treatment Value Assumption), meaning one unit's treatment does not affect other units. However, in many practical situations, this assumption does not hold:

- **Economic Policy**: Policies in one region may affect neighboring regions
- **Public Health**: Vaccination has herd immunity effects
- **Educational Reform**: Demonstration schools may influence surrounding schools

### This Package's Solution

1. **Model Spillover Structure**: Define which units may be affected through matrix A
2. **Adjust Estimation Method**: Correct bias in traditional SCM
3. **Statistical Inference**: Provide hypothesis testing that accounts for spillovers

## Output Interpretation

### Result Object Structure

```r
result$spillover_effects  # Spillover-adjusted effects by period
result$vanilla_effects    # Traditional SCM effects
result$ci_lower          # Lower confidence interval bound
result$ci_upper          # Upper confidence interval bound
result$synthetic         # Synthetic control values
```

### Effect Interpretation

- **Negative Effects**: Treatment reduced the outcome variable (e.g., cigarette tax reduces sales)
- **Positive Effects**: Treatment increased the outcome variable (e.g., training increases productivity)
- **Confidence Interval Excluding 0**: Effect is statistically significant

## Common Issues

## Citation

If you use this package in your research, please cite:

```
Cao, J. and Dowd, C. (2019). "Estimation and Inference for Synthetic Control Methods with Spillover Effects." Working Paper.
```

## License

MIT License

## Contact and Support

- Report issues: fu.zhanc@northeastern.edu
- Contact authors: Jianfei Cao <j.cao@northeastern.edu> and Connor Dowd <cd@codowd.com>

## Changelog

### v0.1.1 (2025-12)
- Added comprehensive unit tests with testthat
- Added `generate_test_data()` function for simulation studies
- Improved documentation
- Fixed non-ASCII character issues

### v0.1.0 (2025-11)
- Initial release
- Implemented core SCM spillover effect estimation
- Added visualization functionality
- Support for general panel data
- Added multiple new plotting options

---

**Note**: This package is under active development. Contributions and suggestions are welcome!
