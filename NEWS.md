# bufferedThresholdPanel 0.1.2

## Breaking change

* **`trim` default corrected from 0.10 to 0.15** in `bptr()`, `bptr_test()`,
  and `bptr_test_seq()`.  Both Hansen (1999) and Hansen (2000) specify
  π₀ = 0.15 (the [15%, 85%] quantile range); the previous 0.10 was a
  miscalibration with no paper support.  Threshold estimates and bootstrap
  p-values from code that relied on the old default will differ numerically;
  pass `trim = 0.10` explicitly to reproduce prior results.

## Reliability and correctness improvements

* **`removeFE()` vectorised**: replaced an O(N·N·T) per-unit for-loop with
  `ave()` (O(N·T)).  Names are now stripped from the output to prevent silent
  name-mismatch errors in downstream operations.  A warning is issued for
  singleton units (T = 1) that demean to zero.
* **`concentratedOLS()` / `concentratedOLS3()` underdetermined guard**: the
  regime-size threshold was corrected from `<= n_vars` to `< n_vars`, matching
  the grid-search guard.  Exactly-identified regimes (obs == regressors) now
  proceed to OLS instead of being silently zeroed-out.  A `warning()` call
  (previously absent) is now issued for genuinely underdetermined regimes.
* **`validatePanel()` extended checks**: NA detection now covers all column
  types (not only numeric).  A new check warns when data are not sorted by
  time within units, which is critical for hysteresis regime assignment.
* **`tidy.bptr()` NaN/Inf guard**: `statistic` and `p.value` are set to
  `NA_real_` when a regime's standard error is zero or non-finite, preventing
  silent propagation of `NaN`/`Inf` into downstream tables.
* **`bptr_bootstrap()` 3-regime guard**: calling the function on a 3-regime
  model now raises an informative error rather than silently producing
  incomplete output (missing `beta3`).
* **Bootstrap index precomputation**: `bptr_test()`, `bptr_test_23()`, and
  `bptr_bootstrap()` now build the unit→row index map once before the
  bootstrap loop (`split(seq_along(id), id)`), eliminating O(N²·T) per-
  iteration `which()` scans.
* **`bptr()` input validation**: `id`, `time`, and `q` are now checked to be
  single character strings; `grid_size` and `grid_size_3` are validated as
  positive integers when provided.
* **Grammar**: `print.bptr_test()` and `print.bptr_test23()` now print
  "Not significant" (previously "No significant").

## Shiny app fixes

* **Regime Analysis tab** no longer crashes with a row-count mismatch error
  when the dataset contains missing values (which `model.frame()` drops during
  fitting, making `model()$n_obs < nrow(data)`).  All three outputs now slice
  data vectors to exactly `model()$n_obs` rows before combining with model
  output.  The Regime Composition table now also includes the dependent
  variable column.
* **Robust SE type description** is now a persistent, selection-aware help text
  below the SE type selector.  Selecting HC0, HC1, HC2, or HC3 immediately
  shows a plain-language description of that estimator, replacing a static
  tooltip that always described HC3 regardless of selection.

## Model comparison

* **`glance()` now returns `bic_approx`** alongside the existing `aic_approx`.
  BIC penalises model complexity more heavily than AIC for panels with more
  than ~7 observations, making it preferable when the goal is selecting the
  correct number of regimes rather than predictive accuracy.  No additional
  computation: both metrics reuse the already-available `ssr`, `n_obs`, and
  parameter count.

## Performance and new features

* **Rcpp-accelerated buffer indicator functions**: `buildBufferIndicators()`
  and `buildBufferIndicators3()` now use compiled C++ code via Rcpp, yielding
  2–3× speedups for the grid search in BTPD models (the key computational
  bottleneck).  Results are bit-identical to the previous pure-R
  implementation (all 528 tests pass unchanged).
* **`grid_size` parameter in `bptr_bootstrap()`**: New `grid_size` argument
  allows users to cap the threshold grid during bootstrap replications.
  `grid_size = NULL` (default) uses the exhaustive grid from the original
  fit for maximum precision.  Pass `grid_size = 50` for fast approximate
  CIs during exploration (e.g. ~0.8 minutes vs ~37 hours for rle with
  92×15 panel and 4 workers).

# bufferedThresholdPanel 0.1.1

## Bug fixes and methodological improvements

* **First-observation tiebreaker** (`buildBufferIndicators3`): At
  exact equidistance between buffer-zone boundaries the tie now resolves
  to the upper regime, matching the strict less-than (`<`) rule of the
  original Matlab implementation.
* **Parallel workers in testthat**: `bptr_bootstrap()` now defaults to
  `workers = 1L` when `TESTTHAT` is set, preventing multisession-launch
  failures during `devtools::test()` (where the package is loaded via
  `load_all()` rather than installed).
* **Grid-size cap warning**: The `"grid_size > 50 capped"` warning now
  fires only for 3-regime BTPD models (`n_thresh == 2`), where the 4-D
  grid search makes the cap meaningful.
* **Exhaustive grid default**: The default `grid_size = NULL` always
  uses all unique observed values of the threshold variable (Hansen 1999)
  rather than a sampled subset, guaranteeing the global SSR minimum.
* **Robust Vcov memory relief**: `robustVcov()` uses element-wise
  multiplication (`omega * X`) instead of constructing the full n x n
  diagonal matrix, eliminating a memory blowup in large panels.
* **set.seed hygiene**: `bptr_test()`, `bptr_test_23()`, and
  `bptr_bootstrap()` now save and restore `.Random.seed` via
  `on.exit()`, eliminating side effects on the user's RNG state.
* **regime_table ordering**: `tapply()` now uses sorted factor levels,
  ensuring deterministic column and row order in regime tables.
* **se_type validation**: Added explicit `stop()` for invalid `se_type`
  values.
* **Dead branch removed**: The unreachable `length(g_vec) == 2L` path in
  `computeSSR()` under non-buffer mode has been removed.

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
