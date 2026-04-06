# bufferedThresholdPanel — Installation Guide

## Overview

**bufferedThresholdPanel** is a pure-R package.  It has **no C++ code** and
requires **no compiler toolchain** (no Rtools, no Xcode, no GCC).  Installation
is the same on every platform: Windows, macOS, and Linux.

---

## Requirements

| Component | Minimum |
|-----------|---------|
| R version | 4.1.0   |
| RAM       | 2 GB    |
| Disk      | 100 MB  |

No external libraries, no compiler, no Python.

---

## Installation

### From CRAN (once published — recommended)

```r
install.packages("bufferedThresholdPanel")
```

### From GitHub (development version)

```r
# Install remotes if needed
install.packages("remotes")

remotes::install_github("MessaoudZouikri/bufferedThresholdPanel")
```

### From a local clone

```r
# After cloning the repository
setwd("path/to/bufferedThresholdPanel")
install.packages(".", type = "source", repos = NULL)
```

---

## Dependencies

All dependencies are pure-R packages and are installed automatically.

**Required** (installed automatically with the package):

| Package      | Version  | Purpose                         |
|--------------|----------|---------------------------------|
| future       | ≥ 1.33.0 | Parallel bootstrap              |
| furrr        | ≥ 0.3.1  | Parallel map over bootstrap reps|
| ggplot2      | ≥ 3.4.0  | Diagnostic plots                |
| gt           | ≥ 0.10.0 | Publication-ready tables        |
| dplyr        | ≥ 1.1.0  | Data manipulation               |
| tibble       | ≥ 3.2.0  | Tidy data frames                |
| broom        | ≥ 1.0.5  | tidy/glance/augment methods     |
| parallelly   | ≥ 1.36.0 | CPU core detection              |

**Optional** (install separately as needed):

| Package   | Purpose                                            |
|-----------|----------------------------------------------------|
| shiny     | Interactive Shiny app (`inst/app/app.R`)           |
| bslib     | Shiny app theme                                    |
| plotly    | Interactive plots in Shiny                         |
| DT        | Interactive tables in Shiny                        |
| readxl    | Load `.xlsx` data in Shiny                         |
| readr     | Load `.csv` data in Shiny                          |
| writexl   | Export results from Shiny                          |
| tidyr     | `pivot_longer` in Shiny coefficient table          |
| knitr     | Compile the vignette                               |
| rmarkdown | Compile the vignette                               |
| testthat  | Run the test suite                                 |

Install optional packages with:

```r
install.packages(c("shiny", "bslib", "plotly", "DT",
                   "readxl", "readr", "writexl", "tidyr"))
```

---

## Verify Installation

```r
library(bufferedThresholdPanel)

# Check version
packageVersion("bufferedThresholdPanel")

# Load the built-in dataset
data(panel_data)
cat("Obs:", nrow(panel_data), " | Countries:", length(unique(panel_data$country)), "\n")

# Quick estimation (2-regime BTPD)
fit <- bptr(
  growthRate ~ eci + initialGDP + fdiGDP + capFormGDP +
               inflation + popGrowth + indVAGDP + tradeOpenness,
  data     = panel_data,
  id       = "countryId",
  time     = "year",
  q        = "oilRentGDP",
  n_thresh = 1,
  buffer   = TRUE
)
print(fit)
```

---

## Running the Shiny App

After installing the optional Shiny packages:

```r
library(bufferedThresholdPanel)

# Launch the interactive explorer
bptr_shiny()
```

Or run the standalone app directly:

```r
shiny::runApp(system.file("app", package = "bufferedThresholdPanel"))
```

---

## Troubleshooting

### "there is no package called 'bufferedThresholdPanel'"

The package is not yet on CRAN.  Install from GitHub:

```r
remotes::install_github("MessaoudZouikri/bufferedThresholdPanel")
```

### "package 'remotes' is not available"

```r
install.packages("remotes")
```

### Parallel bootstrap is slow

The bootstrap loop uses `future`/`furrr` for parallelism.  If no workers are
detected, it falls back to sequential execution.  Speed is proportional to the
number of physical CPU cores.  On a dual-core machine, 299 bootstrap
replications for the 2-regime model typically take 1–3 minutes.

Reduce replications for exploratory work:

```r
boot <- bptr_bootstrap(fit, n_boot = 99)   # quick check
boot <- bptr_bootstrap(fit, n_boot = 499)  # for publication
```

### "package 'gt' is not available for this version of R"

Update R to ≥ 4.1.0, then reinstall gt:

```r
install.packages("gt")
```

---

## Support

- **GitHub Issues**: <https://github.com/MessaoudZouikri/bufferedThresholdPanel/issues>
- **Authors**: Zouikri, Hamdi, Souam (EconomiX-CNRS, Paris Nanterre University)

---

**Package**: bufferedThresholdPanel v0.1.0  
**R Minimum**: 4.1.0  
**Last Updated**: 2025
