# bufferedThresholdPanel

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/bufferedThresholdPanel)](https://CRAN.R-project.org/package=bufferedThresholdPanel)
[![R-CMD-check](https://github.com/MessaoudZouikri/bufferedThresholdPanel/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/MessaoudZouikri/bufferedThresholdPanel/actions)
[![Codecov test coverage](https://codecov.io/gh/MessaoudZouikri/bufferedThresholdPanel/branch/master/graph/badge.svg)](https://app.codecov.io/gh/MessaoudZouikri/bufferedThresholdPanel?branch=master)
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

### Testing procedure — choosing the number of regimes

Before estimating the model, you need to determine whether the data support
one, two, or three regimes.  The package implements the **sequential bootstrap
testing procedure** of Belarbi et al. (2021), Section 2.3.  Two tests are
run in order:

| Test | Null H₀ | Alternative H₁ | Bootstrap null |
|------|---------|----------------|----------------|
| **F₁,₂** | Linear model (no threshold) | 2-regime BTPD | Residuals from the linear model |
| **F₂,₃** | 2-regime BTPD | 3-regime BTPD | Residuals from the **2-regime** model |

The key distinction: `bptr_test_23()` resamples residuals from the
**two-regime model** — not from the linear null — because that is the null
hypothesis of the second step.  `bptr_test_seq()` runs both steps
automatically and selects the preferred model.

---

#### Step 1 — Test linearity against a 2-regime BTPD model (F₁,₂)

`bptr_test()` tests H₀: linear panel model against H₁: 2-regime BTPD.
Bootstrap p-values are used because the threshold parameter is unidentified
under the null (Davies, 1987).

```r
# Model I — is there a threshold in oilRentGDP?
test12_I <- bptr_test(
  growthRate ~ rle + eci + initialGDP + fdiGDP + capFormGDP +
               inflation + popGrowth + indVAGDP + tradeOpenness,
  data      = panel_data,
  id        = "countryId",
  time      = "year",
  q         = "oilRentGDP",   # threshold variable to test
  buffer    = TRUE,            # test BTPD (set FALSE for classical PTR)
  n_boot    = 299,
  seed      = 2025
)
print(test12_I)
# Sup-LR statistic : 55.52
# Bootstrap p-value: 0.002  *** → reject linearity

# Model II — is there a threshold in rle?
test12_II <- bptr_test(
  growthRate ~ oilRentGDP + eci + initialGDP + fdiGDP + capFormGDP +
               inflation + popGrowth + indVAGDP + tradeOpenness,
  data      = panel_data,
  id        = "countryId",
  time      = "year",
  q         = "rle",
  buffer    = TRUE,
  n_boot    = 299,
  seed      = 2025
)
print(test12_II)
# Sup-LR statistic : 52.65
# Bootstrap p-value: 0.000  *** → reject linearity
```

If the p-value exceeds your significance level (e.g. 0.05), stop here —
the data support a linear model.  If linearity is rejected, proceed to
Step 2.

---

#### Step 2 — Test 2-regime against 3-regime BTPD (F₂,₃)

`bptr_test_23()` takes the **fitted 2-regime model** (from `test12_I$model`)
as its first argument.  It resamples bootstrap residuals from that 2-regime
model — not from the linear null — which is the correct null for this step.

```r
# Model I — does oilRentGDP support 3 regimes?
test23_I <- bptr_test_23(
  fit_2reg    = test12_I$model,   # fitted 2-regime model from Step 1
  n_boot      = 299,
  grid_size_3 = 50,               # 4-D grid resolution for 3-regime search
  seed        = 2025
)
print(test23_I)
# H0 thresholds (2-regime): 0.1490  0.9584
# H1 thresholds (3-regime): -1.0506  0.0720  0.1490  0.9584
# Sup-LR statistic : 29.25
# Bootstrap p-value: 0.002  *** → 3-regime model preferred

# Model II — does rle support 3 regimes?
test23_II <- bptr_test_23(
  fit_2reg    = test12_II$model,
  n_boot      = 299,
  grid_size_3 = 50,
  seed        = 2025
)
print(test23_II)
# Bootstrap p-value: 0.000  *** → 3-regime model preferred
```

---

#### Step 3 — Estimate the selected model

Both tests rejected their respective nulls, so the three-regime BTPD model
is preferred.  Retrieve it directly from the test object or re-estimate with
a finer grid:

```r
# Option A — use the model already fitted inside the test object (fast)
fit3_I  <- test23_I$model_3reg
fit3_II <- test23_II$model_3reg
summary(fit3_I)

# Option B — re-estimate with a finer grid for publication results
fit3_I <- bptr(
  growthRate ~ rle + eci + initialGDP + fdiGDP + capFormGDP +
               inflation + popGrowth + indVAGDP + tradeOpenness,
  data        = panel_data,
  id          = "countryId",
  time        = "year",
  q           = "oilRentGDP",
  n_thresh    = 2,          # 2 thresholds → 3 regimes
  buffer      = TRUE,
  grid_size_3 = 80,         # finer grid for final results
  se_type     = "HC3"
)
summary(fit3_I)
```

---

#### Automated shortcut — run all steps at once

If you do not need to inspect intermediate results, `bptr_test_seq()` runs
Steps 1–3 automatically and prints progress at each decision point:

```r
# Model I — full sequential procedure
result_I <- bptr_test_seq(
  growthRate ~ rle + eci + initialGDP + fdiGDP + capFormGDP +
               inflation + popGrowth + indVAGDP + tradeOpenness,
  data        = panel_data,
  id          = "countryId",
  time        = "year",
  q           = "oilRentGDP",
  buffer      = TRUE,
  n_boot      = 299,
  grid_size_3 = 50,
  alpha       = 0.10,
  seed        = 2025
)
print(result_I)
# $n_regimes_selected  → 1, 2, or 3
# $final_model         → the recommended fitted bptr object

# Model II
result_II <- bptr_test_seq(
  growthRate ~ oilRentGDP + eci + initialGDP + fdiGDP + capFormGDP +
               inflation + popGrowth + indVAGDP + tradeOpenness,
  data        = panel_data,
  id          = "countryId",
  time        = "year",
  q           = "rle",
  buffer      = TRUE,
  n_boot      = 299,
  grid_size_3 = 50,
  alpha       = 0.10,
  seed        = 2025
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

| Column          | Description                                                         |
|-----------------|---------------------------------------------------------------------|
| `countryId`     | Numeric country ID (1–92)                                           |
| `country`       | Country name                                                        |
| `year`          | Year (2002–2016)                                                    |
| `growthRate`    | Annual GDP growth rate (log difference)                             |
| `oilRentGDP`    | Oil rents as % of GDP (threshold var. Model I; predictor Model II)  |
| `rle`           | Rule of Law index (threshold var. Model II; predictor Model I)      |
| `initialGDP`    | Log initial GDP per capita                                          |
| `eci`           | Economic Complexity Index                                           |
| `fdiGDP`        | FDI inflows as % of GDP                                             |
| `capFormGDP`    | Gross fixed capital formation as % GDP                              |
| `inflation`     | Inflation rate (GDP deflator)                                       |
| `popGrowth`     | Population growth rate                                              |
| `indVAGDP`      | Industrial value added as % GDP                                     |
| `tradeOpenness` | Trade openness (exports + imports / GDP)                            |

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
<https://doi.org/10.1111/1468-0262.00124>

Li, G., Guan, B., Li, W., & Yu, P. L. H. (2015).
Hysteretic autoregressive time series models.
*Biometrika*, 102(3), 717–723.
<https://doi.org/10.1093/biomet/asv017>

## Citation

If you use **bufferedThresholdPanel** in your research, please cite:

**The R package:**
```bibtex
@Manual{bufferedThresholdPanel,
  title  = {{bufferedThresholdPanel}: Buffered Panel Threshold Regression
             with Hysteresis},
  author = {Hamdi, Faycal and Souam, Said and Zouikri, Messaoud},
  year   = {2025},
  note   = {R package version 0.1.0},
  url    = {https://github.com/MessaoudZouikri/bufferedThresholdPanel}
}
```

**The empirical application paper:**
```bibtex
@Article{hsz2025,
  author  = {Hamdi, Faycal and Souam, Said and Zouikri, Messaoud},
  title   = {The heterogeneous effect of economic complexity on growth and
             human development: {N}ew empirical evidence using buffered
             panel threshold regression},
  journal = {The World Economy},
  year    = {2025},
  volume  = {48},
  pages   = {2561--2592},
  doi     = {10.1111/twec.70023}
}
```

**The methodology paper:**
```bibtex
@Article{beletal2021,
  author  = {Belarbi, Yacine and Hamdi, Faycal and Khalfi, Abderaouf
             and Souam, Said},
  title   = {Growth, institutions and oil dependence: {A} buffered
             threshold panel approach},
  journal = {Economic Modelling},
  year    = {2021},
  volume  = {99},
  pages   = {105477},
  doi     = {10.1016/j.econmod.2021.02.018}
}
```

To get the citation directly in R:
```r
citation("bufferedThresholdPanel")
```

## License

GPL (≥ 3) — see [LICENSE](LICENSE)

## Authors

- **Faycal Hamdi** — RECITS Laboratory, USTHB, Algiers
- **Said Souam** — EconomiX-CNRS, Université Paris Nanterre
- **Messaoud Zouikri** (**maintainer**) — EconomiX-CNRS, Université Paris Nanterre

---

## Feedback, Bug Reports & Suggestions

Your feedback helps make **bufferedThresholdPanel** better for the entire
research community. We welcome feedback from users. Whether you have found a bug, 
have a question about the methodology, or would like to suggest an improvement, 
we would be pleased to hear from you.

**Please reach out at:**
📧 [bufferedThresholdPanel@proton.me](mailto:bufferedThresholdPanel@proton.me)

When reporting a **bug**, please include:

- Your R version (`R.version.string`)
- Your operating system
- A minimal reproducible example (the smallest code that triggers the issue)
- The full error message or unexpected output

When suggesting an **improvement or new feature**, please describe:

- The use case or research context that motivates it
- How you would expect it to behave

For **methodological questions** related to the BTPD model of
Belarbi et al. (2021) or the sequential testing procedure, feel free
to ask as well — we are happy to clarify.

