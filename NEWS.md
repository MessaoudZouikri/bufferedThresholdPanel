# bufferedThresholdPanel 0.1.0

## Initial release

* Implements the Buffered Panel Threshold Data (BTPD) model of
  Belarbi et al. (2021) for panel data with individual fixed effects.
* Supports two-regime models (one buffer zone) and three-regime models
  (two buffer zones) with hysteresis-driven regime transitions.
* Implements the classical Panel Threshold Regression (PTR) of
  Hansen (1999, 2000) as a special case.
* Sequential bootstrap tests for the number of regimes:
  F1,2 (linearity vs 2-regime) and F2,3 (2-regime vs 3-regime).
* Parallel bootstrap confidence intervals via `future`/`furrr`.
* Publication-ready tables in AER and Journal of Econometrics styles
  via `bptr_table()`, `bptr_latex()`, and `bptr_kable()`.
* Full `broom` integration: `tidy()`, `glance()`, `augment()`.
* Interactive Shiny explorer via `bptr_shiny()`.
* Built-in dataset `panel_data`: 92 countries, 2002–2016, used in
  Hamdi et al. (2025) <doi:10.1111/twec.70023>.
