# bufferedThresholdPanel

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/bufferedThresholdPanel)](https://CRAN.R-project.org/package=bufferedThresholdPanel)
[![R-CMD-check](https://github.com/MessaoudZouikri/bufferedThresholdPanel/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/MessaoudZouikri/bufferedThresholdPanel/actions)
[![Codecov test coverage](https://codecov.io/gh/MessaoudZouikri/bufferedThresholdPanel/branch/main/graph/badge.svg)](https://app.codecov.io/gh/MessaoudZouikri/bufferedThresholdPanel?branch=main)
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
<!-- badges: end -->

## Overview

**bufferedThresholdPanel** estimates panel data threshold regression models
with individual fixed effects.  It implements two complementary models:

- **Panel Threshold Regression (PTR)** — abrupt regime transitions following
  Hansen (1999, 2000)
- **Buffered Panel Threshold Data (BTPD)** — smooth, hysteresis-driven
  transitions via buffer zones, introduced by Belarbi et al. (2021)

In the BTPD model, a buffer zone $(r_L, r_U)$ sits between two regimes.
When the threshold variable falls inside the buffer zone, the regime indicator
retains its previous value rather than jumping immediately.  This path-dependent
mechanism — known as *hysteresis* and formalised in the time-series context by
Li et al. (2015) — captures real-world phenomena such as institutional quality
traps and natural-resource dependence, where regime transitions are gradual
rather than instantaneous.

## Key Features

- **Two- and three-regime models** with automatic grid search for optimal
  buffer zones
- **Sequential bootstrap tests** — $F_{1,2}$ (linearity vs 2-regime) and
  $F_{2,3}$ (2-regime vs 3-regime) following the procedure of Belarbi et al.
  (2021), with correct bootstrap null in each step
- **Parallel bootstrap** via `future`/`furrr` for confidence intervals
- **Publication-ready tables** — AER and Journal of Econometrics styles via `gt`
- **Full `broom` integration** — `tidy()`, `glance()`, `augment()`
- **Interactive Shiny app** for exploratory analysis and result export
- **Built-in dataset** — 92-country panel (2002–2016) from Hamdi et al. (2025)

## Installation

```r
# From CRAN (once published)
install.packages("bufferedThresholdPanel")

# Development version from GitHub
remotes::install_github("MessaoudZouikri/bufferedThresholdPanel")
```

No compiler required — the package is pure R.

## Quick Start

```r
library(bufferedThresholdPanel)

# Load the built-in balanced panel dataset
# 92 countries, 15 years (2002-2016), 1380 observations
data(panel_data)
head(panel_data)
```

### Sequential regime test

The empirical application estimates two models: in **Model I** `oilRentGDP`
is the threshold variable and `rle` is a predictor; in **Model II** the roles
are reversed.

```r
# Model I — oil dependence as threshold, Rule of Law as predictor
result_I <- bptr_test_seq(
  growthRate ~ rle + eci + initialGDP + fdiGDP + capFormGDP +
               inflation + popGrowth + indVAGDP + tradeOpenness,
  data        = panel_data,
  id          = "countryId",
  time        = "year",
  q           = "oilRentGDP",    # threshold variable
  buffer      = TRUE,
  n_boot      = 299,
  grid_size_3 = 50,
  alpha       = 0.10
)
print(result_I)

# Model II — Rule of Law as threshold, oil dependence as predictor
result_II <- bptr_test_seq(
  growthRate ~ oilRentGDP + eci + initialGDP + fdiGDP + capFormGDP +
               inflation + popGrowth + indVAGDP + tradeOpenness,
  data        = panel_data,
  id          = "countryId",
  time        = "year",
  q           = "rle",           # threshold variable
  buffer      = TRUE,
  n_boot      = 299,
  grid_size_3 = 50,
  alpha       = 0.10
)
print(result_II)
```

### Direct model estimation

```r
# Model I — two-regime BTPD, oilRentGDP as threshold
fit2 <- bptr(
  growthRate ~ rle + eci + initialGDP + fdiGDP + capFormGDP +
               inflation + popGrowth + indVAGDP + tradeOpenness,
  data     = panel_data,
  id       = "countryId",
  time     = "year",
  q        = "oilRentGDP",    # threshold variable
  n_thresh = 1,        # 1 threshold → 2 regimes
  buffer   = TRUE,     # hysteresis transition
  se_type  = "HC3"
)
print(fit2)

# Model II — two-regime BTPD, rle as threshold
fit2_II <- bptr(
  growthRate ~ oilRentGDP + eci + initialGDP + fdiGDP + capFormGDP +
               inflation + popGrowth + indVAGDP + tradeOpenness,
  data     = panel_data,
  id       = "countryId",
  time     = "year",
  q        = "rle",           # threshold variable
  n_thresh = 1,
  buffer   = TRUE,
  se_type  = "HC3"
)
print(fit2_II)

# Model I — three-regime BTPD, oilRentGDP as threshold
fit3 <- bptr(
  growthRate ~ rle + eci + initialGDP + fdiGDP + capFormGDP +
               inflation + popGrowth + indVAGDP + tradeOpenness,
  data        = panel_data,
  id          = "countryId",
  time        = "year",
  q           = "oilRentGDP",    # threshold variable
  n_thresh    = 2,        # 2 thresholds → 3 regimes
  buffer      = TRUE,
  grid_size_3 = 50        # 4-D grid search resolution
)
summary(fit3)
```

### Bootstrap confidence intervals

```r
boot <- bptr_bootstrap(fit2, n_boot = 299)
print(boot)
```

### Tidy output

```r
library(broom)

tidy(fit3, conf.int = TRUE)   # coefficient table with CIs
glance(fit3)                  # model-level fit statistics
augment(fit3)                 # add fitted values and regimes to data
threshold_tidy(fit3)          # threshold estimates and CIs
```

### Publication-ready tables

```r
# AER style
bptr_table(fit3, style = "AER",
           title = "Growth, Oil Dependence and Economic Complexity")

# LaTeX export
bptr_latex(fit3, file = "table1.tex")
```

### Diagnostic plots

```r
plot(fit3, which = 1)   # residuals vs fitted, coloured by regime
plot(fit3, which = 2)   # threshold variable with buffer-zone boundaries
```

### Interactive Shiny app

```r
bptr_shiny()
```

## The Model

### Two-regime BTPD

$$Y_{it} = \mu_i + X_{it}\beta_1 \mathbf{1}(R_{it}=1) + X_{it}\beta_2 \mathbf{1}(R_{it}=2) + \varepsilon_{it}$$

$$R_{it} = \begin{cases}
1 & q_{it} \le r_L \\
R_{i,t-1} & r_L < q_{it} \le r_U \quad \text{(buffer zone)} \\
2 & q_{it} > r_U
\end{cases}$$

### Three-regime BTPD

Two buffer zones $(r_{L,1}, r_{U,1})$ and $(r_{L,2}, r_{U,2})$ with
$r_{L,1} \le r_{U,1} < r_{L,2} \le r_{U,2}$.  Observations in each buffer
zone retain their previous regime, producing smooth, history-dependent
transitions between all three regimes.

### Estimation

Individual fixed effects are removed by within-group demeaning.  The optimal
buffer-zone parameters minimise the concentrated sum of squared residuals over
a candidate grid:
- Two-regime BTPD: 2-D grid search, $O(G^2)$
- Three-regime BTPD: 4-D grid search, $O(G^4/24)$ — use `grid_size_3 ≤ 50`

### Sequential tests

The package implements the full sequential testing procedure of Belarbi et al.
(2021), Section 2.3.  Because the threshold parameter is not identified under
the null of linearity, the test statistic does not follow a standard
distribution — a problem first analysed by Davies (1987) — and p-values are
obtained by bootstrap.  The critical distinction between the two steps is that
the $F_{2,3}$ bootstrap draws residuals from the **two-regime model** (not from
the linear null), which is the null hypothesis of the second step.

## Built-in Dataset

`panel_data` is a balanced panel of 92 countries observed from 2002 to 2016:

| Column         | Description                              |
|----------------|------------------------------------------|
| `countryId`    | Numeric country ID (1–92)                |
| `country`      | Country name                             |
| `year`         | Year (2002–2016)                         |
| `growthRate`   | Annual GDP growth rate (log difference)  |
| `oilRentGDP`   | Oil rents as % of GDP (threshold var. Model I; predictor Model II) |
| `rle`          | Rule of Law index (threshold var. Model II; predictor Model I)     |
| `initialGDP`   | Log initial GDP per capita               |
| `eci`          | Economic Complexity Index                |
| `fdiGDP`       | FDI inflows as % of GDP                  |
| `capFormGDP`   | Gross fixed capital formation as % GDP   |
| `inflation`    | Inflation rate (GDP deflator)            |
| `popGrowth`    | Population growth rate                   |
| `indVAGDP`     | Industrial value added as % GDP          |
| `tradeOpenness`| Trade openness (exports + imports / GDP) |

Sources: World Bank WDI; Harvard CID Atlas of Economic Complexity.

## References

Belarbi, Y., Hamdi, F., Khalfi, A., and Souam, S. (2021).
Growth, institutions and oil dependence: A buffered threshold panel approach.
*Economic Modelling*, 99, 105477.
<https://doi.org/10.1016/j.econmod.2021.02.018>

Davies, R. B. (1987). 
Hypothesis testing when a nuisance parameter is present only under the alternative. 
*Biometrika*, 74(1), 33–43. 
<https://doi.org/10.1093/biomet/74.1.33>

Hamdi, F., Souam, S., and Zouikri, M. (2025).
The heterogeneous effect of economic complexity on growth and human development:
New empirical evidence using buffered panel threshold regression.
*The World Economy*, 48, 2561–2592.
<https://doi.org/10.1111/twec.70023>

Hansen, B. E. (1999).
Threshold effects in non-dynamic panels: estimation, testing, and inference.
*Journal of Econometrics*, 93(2), 345–368.
<https://doi.org/10.1016/S0304-4076(99)00025-1>

Hansen, B. E. (2000).
Sample splitting and threshold estimation.
*Econometrica*, 68(3), 575–603.
<https://doi.org/10.2307/2669382>

Li, G., Guan, B., Li, W., & Yu, P. L. H. (2015). 
Hysteretic autoregressive time series models. 
*Biometrika*, 102(3), 717–723. 
<https://doi.org/10.1093/biomet/asv017>

## License

GPL (≥ 3) — see [LICENSE](LICENSE)

## Authors

- **Faycal Hamdi** — RECITS Laboratory, USTHB, Algiers
- **Said Souam** — EconomiX-CNRS, Université Paris Nanterre
- **Messaoud Zouikri** (maintainer) — EconomiX-CNRS, Université Paris Nanterre