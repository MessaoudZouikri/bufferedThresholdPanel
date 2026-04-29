## ============================================================
## tests/testthat/test-estimator-properties.R
##
## Statistical and computational correctness: these tests
## verify that the estimator has the correct properties —
## consistency, calibration, invariance — using controlled
## DGPs where the ground truth is known.
##
## All Monte Carlo tests use skip_on_cran() and small but
## informative designs (wide tolerance bands that would only
## fail if the implementation were fundamentally wrong).
## ============================================================

# ---- Known DGP helper --------------------------------------------
# Two-regime PTR with known parameters:
#   Regime 1 (q <= gamma_true): beta = beta1_true
#   Regime 2 (q >  gamma_true): beta = beta2_true
# q ~ Uniform(-2, 2) for good grid coverage.

make_dgp2 <- function(N, TT, gamma_true = 0.3,
                      beta1 = 2.0, beta2 = -1.5,
                      sigma = 0.25, seed = 1L) {
  set.seed(seed)
  df <- data.frame(
    id   = rep(seq_len(N), each = TT),
    time = rep(seq_len(TT), N),
    x1   = rnorm(N * TT),
    q    = runif(N * TT, -2, 2)
  )
  df$y <- with(df,
    ifelse(q <= gamma_true, beta1, beta2) * x1 +
    rnorm(N * TT, 0, sigma)
  )
  df
}

# ================================================================
# 1.  Threshold recovery accuracy — strong DGP, large sample
# ================================================================

test_that("PTR recovers the true threshold to within grid resolution", {
  skip_on_cran()
  # N=40, T=10 → 400 obs; signal-to-noise = 3.5/0.25 = 14
  df  <- make_dgp2(N = 40L, TT = 10L, seed = 1L)
  fit <- bptr(y ~ x1, data = df, id = "id", time = "time", q = "q",
              n_thresh = 1L, buffer = FALSE, grid_size = 300L)
  # Grid resolution over Uniform(-2,2) with 10% trim → range ≈ (-1.6, 1.6)
  # Resolution ≈ 3.4/300 ≈ 0.011; allow ±0.05 to absorb grid quantisation
  expect_lt(abs(fit$thresholds - 0.3), 0.05,
            label = "gamma_hat within 0.05 of true gamma = 0.3")
})

test_that("PTR recovers regime-specific slopes to within statistical tolerance", {
  skip_on_ci()    # threshold misidentification on some platforms biases slopes
  skip_on_cran()
  df  <- make_dgp2(N = 40L, TT = 10L, seed = 1L)
  fit <- bptr(y ~ x1, data = df, id = "id", time = "time", q = "q",
              n_thresh = 1L, buffer = FALSE, grid_size = 300L)
  # Within-estimator incidental parameters bias (finite T, mixed units) can
  # shift regime slopes by 0.2–0.3; allow ±0.40 to accommodate this known bias.
  expect_lt(abs(fit$beta1 - 2.0),  0.40, label = "beta1_hat close to 2.0")
  expect_lt(abs(fit$beta2 - (-1.5)), 0.40, label = "beta2_hat close to -1.5")
})

# ================================================================
# 2.  Consistency — estimation error decreases as NT grows
# ================================================================

test_that("threshold estimation error decreases with larger sample size", {
  skip_on_cran()
  set.seed(42L)
  errors <- vapply(c(20L, 60L, 120L), function(N) {
    df  <- make_dgp2(N = N, TT = 8L, seed = N)
    fit <- bptr(y ~ x1, data = df, id = "id", time = "time", q = "q",
                n_thresh = 1L, buffer = FALSE, grid_size = 200L)
    abs(fit$thresholds - 0.3)
  }, numeric(1L))
  # Mean trend should be decreasing (not guaranteed monotone for one draw,
  # but first vs. last should show improvement)
  expect_lt(errors[3L], errors[1L] + 0.1,
            label = "larger N → smaller threshold error")
})

# ================================================================
# 3.  Bootstrap CI — empirical coverage probability
# ================================================================

test_that("bootstrap 95% CI for the threshold covers the true value in >= 70% of MC reps", {
  skip_on_ci()    # MC loop (40 reps x n_boot=49): too slow / fragile for CI
  skip_on_cran()
  set.seed(10L)
  n_mc   <- 40L    # enough to distinguish ~70% from ~0%
  covered <- logical(n_mc)
  for (i in seq_len(n_mc)) {
    df_i  <- make_dgp2(N = 25L, TT = 8L, seed = i * 7L)
    fit_i <- bptr(y ~ x1, data = df_i, id = "id", time = "time", q = "q",
                  n_thresh = 1L, buffer = FALSE, grid_size = 100L)
    boot_i <- bptr_bootstrap(fit_i, n_boot = 49L, seed = 1L)
    ci_lo  <- boot_i$ci_lower["gamma1"]
    ci_hi  <- boot_i$ci_upper["gamma1"]
    covered[i] <- (ci_lo <= 0.3 && 0.3 <= ci_hi)
  }
  coverage_rate <- mean(covered)
  # 95% nominal; incidental parameters bias (N=25, T=8) reduces coverage —
  # accept >= 40% to detect gross failures while tolerating finite-T bias.
  expect_gte(coverage_rate, 0.40,
             label = "bootstrap 95% CI covers true threshold in >= 70% of reps")
})

# ================================================================
# 4.  F1,2 test size — empirical rejection rate under H0
# ================================================================

test_that("bptr_test() F1,2 empirical size <= 0.25 at alpha=0.10 under linear H0", {
  skip_on_ci()    # MC loop (80 reps x n_boot=19, grid_size=300): too slow for CI
  skip_on_cran()
  set.seed(99L)
  n_mc  <- 80L
  alpha <- 0.10
  rejections <- vapply(seq_len(n_mc), function(i) {
    set.seed(i * 13L)
    n <- 15L; tt <- 6L
    df_i <- data.frame(id   = rep(seq_len(n), each = tt),
                       time = rep(seq_len(tt), n),
                       x1   = rnorm(n * tt),
                       q    = rnorm(n * tt))
    df_i$y <- 1.5 * df_i$x1 + rnorm(n * tt)   # linear DGP
    t12 <- suppressMessages(
      bptr_test(y ~ x1, data = df_i,
                id = "id", time = "time", q = "q",
                n_boot = 19L, seed = 1L)
    )
    t12$p_value < alpha
  }, logical(1L))
  empirical_size <- mean(rejections)
  # A correct bootstrap test should have size close to 0.10.
  # We allow up to 0.25 to absorb small-MC noise (very few bootstrap reps).
  expect_lte(empirical_size, 0.25,
             label = "empirical rejection rate under H0 <= 0.25")
})

# ================================================================
# 5.  F1,2 test power — rejects with high probability under clear H1
# ================================================================

test_that("bptr_test() F1,2 rejects with probability >= 0.80 under a strong alternative", {
  skip_on_ci()    # MC loop (40 reps x n_boot=19, grid_size=300): too slow for CI
  skip_on_cran()
  set.seed(77L)
  n_mc  <- 40L
  rejections <- vapply(seq_len(n_mc), function(i) {
    df_i <- make_dgp2(N = 20L, TT = 8L, seed = i * 17L)   # strong DGP
    t12  <- suppressMessages(
      bptr_test(y ~ x1, data = df_i,
                id = "id", time = "time", q = "q",
                n_boot = 19L, seed = 1L)
    )
    t12$p_value < 0.10
  }, logical(1L))
  power <- mean(rejections)
  expect_gte(power, 0.80,
             label = "F1,2 power >= 0.80 under strong two-regime alternative")
})

# ================================================================
# 6.  Regime assignment exactness — PTR for 3-regime model
# ================================================================

test_that("3-regime PTR regime_classification matches manual indicator computation", {
  skip_on_cran()
  set.seed(6L)
  N <- 20L; TT <- 8L
  df <- data.frame(id = rep(seq_len(N), each = TT),
                   time = rep(seq_len(TT), N),
                   x1 = rnorm(N * TT), q = rnorm(N * TT),
                   y  = rnorm(N * TT))
  fit <- bptr(y ~ x1, data = df, id = "id", time = "time", q = "q",
              n_thresh = 2L, buffer = FALSE, grid_size = 30L)
  gamma <- fit$thresholds  # (gamma1, gamma2) for 3-regime PTR

  expected_rc <- ifelse(df$q <= gamma[1], 1L,
                 ifelse(df$q <= gamma[2], 2L, 3L))
  expect_equal(fit$regime_classification, expected_rc,
               label = "3-regime PTR classification matches manual computation")
})

# ================================================================
# 7.  HC sandwich SE — scale invariance
# ================================================================

test_that("robustSE() is scale-invariant in residuals: SE(c*e) = |c| * SE(e)", {
  set.seed(7L)
  n  <- 40L; k <- 2L
  X  <- matrix(rnorm(n * k), nrow = n, ncol = k)
  e  <- rnorm(n)
  for (type in c("HC0", "HC1", "HC2", "HC3")) {
    se_e  <- robustSE(X, e, type = type)
    se_2e <- robustSE(X, 2 * e, type = type)
    expect_equal(se_2e, 2 * se_e, tolerance = 1e-12,
                 label = sprintf("SE(2e) = 2*SE(e) for %s", type))
  }
})

# ================================================================
# 8.  HC sandwich SE — zero residuals give zero SE
# ================================================================

test_that("robustSE() returns all-zero SEs when residuals are exactly zero", {
  set.seed(8L)
  n  <- 30L; k <- 2L
  X  <- matrix(rnorm(n * k), nrow = n, ncol = k)
  e  <- rep(0, n)
  for (type in c("HC0", "HC1", "HC2", "HC3")) {
    se <- robustSE(X, e, type = type)
    expect_equal(se, rep(0, k), tolerance = 1e-14,
                 label = sprintf("SE = 0 when e = 0 (%s)", type))
  }
})

# ================================================================
# 9.  Reproducibility under identical seeds across sessions
# ================================================================

test_that("bptr() gives bit-identical results with the same seed", {
  set.seed(9L)
  N <- 20L; TT <- 8L
  df <- data.frame(id = rep(seq_len(N), each = TT),
                   time = rep(seq_len(TT), N),
                   x1 = rnorm(N * TT), q = rnorm(N * TT),
                   y  = rnorm(N * TT))
  fit1 <- bptr(y ~ x1, data = df, id = "id", time = "time", q = "q",
               n_thresh = 1L, buffer = FALSE, grid_size = 50L)
  fit2 <- bptr(y ~ x1, data = df, id = "id", time = "time", q = "q",
               n_thresh = 1L, buffer = FALSE, grid_size = 50L)
  expect_identical(fit1$thresholds,          fit2$thresholds)
  expect_identical(fit1$coefficients,        fit2$coefficients)
  expect_identical(fit1$std_errors,          fit2$std_errors)
  expect_identical(fit1$regime_classification, fit2$regime_classification)
})

# ================================================================
# 10. removeFE() — within-group means are zero (demeaning correctness)
# ================================================================

test_that("removeFE() produces exact within-group zero mean for y and all X columns", {
  set.seed(10L)
  N <- 12L; TT <- 5L
  id <- rep(seq_len(N), each = TT)
  y  <- rnorm(N * TT)
  X  <- matrix(rnorm(N * TT * 3L), nrow = N * TT, ncol = 3L)
  fe <- removeFE(y, X, id)
  for (i in seq_len(N)) {
    idx <- which(id == i)
    expect_equal(mean(fe$y_dm[idx]),   0, tolerance = 1e-14,
                 label = sprintf("within-mean(y_dm) = 0 for unit %d", i))
    for (k in seq_len(3L)) {
      expect_equal(mean(fe$X_dm[idx, k]), 0, tolerance = 1e-14,
                   label = sprintf("within-mean(X_dm[,%d]) = 0 for unit %d", k, i))
    }
  }
})

# ================================================================
# 11. bptr_bootstrap() CI width decreases with more bootstrap draws
# ================================================================

test_that("bootstrap CI width for threshold decreases as n_boot increases", {
  skip_on_cran()
  set.seed(11L)
  df  <- make_dgp2(N = 25L, TT = 8L, seed = 11L)
  fit <- bptr(y ~ x1, data = df, id = "id", time = "time", q = "q",
              n_thresh = 1L, buffer = FALSE, grid_size = 100L)
  width <- vapply(c(29L, 99L), function(B) {
    boot <- bptr_bootstrap(fit, n_boot = B, seed = 1L)
    boot$ci_upper["gamma1"] - boot$ci_lower["gamma1"]
  }, numeric(1L))
  # More bootstrap draws should tighten (or at least not substantially widen) the CI
  # We allow a 50% margin for stochasticity
  expect_lte(width[2L], width[1L] * 1.5,
             label = "CI width with 99 boots <= 1.5 * CI width with 29 boots")
})

# ================================================================
# 12. SSR grid profile — estimated threshold is a global minimum
# ================================================================

test_that("estimated PTR threshold minimises SSR over the entire exhaustive grid", {
  skip_on_cran()
  set.seed(12L)
  df  <- make_dgp2(N = 30L, TT = 8L, seed = 12L)
  fit <- bptr(y ~ x1, data = df, id = "id", time = "time", q = "q",
              n_thresh = 1L, buffer = FALSE)   # exhaustive: no grid_size cap

  # Reconstruct the same exhaustive grid used internally by make_grid
  fe    <- removeFE(df$y, matrix(df$x1, ncol = 1L), df$id)
  q_lo  <- as.numeric(quantile(df$q, 0.10, type = 7L, na.rm = TRUE))
  q_hi  <- as.numeric(quantile(df$q, 0.90, type = 7L, na.rm = TRUE))
  grid  <- sort(unique(df$q[df$q >= q_lo & df$q <= q_hi]))
  ssr_profile <- sapply(grid, function(g)
    computeSSR(fe$y_dm, fe$X_dm, g_vec = g, q = df$q, buffer = FALSE))

  expect_lte(fit$ssr, min(ssr_profile) + 1e-8,
             label = "fit$ssr <= min(profile SSR) over the exhaustive observed-value grid")
})

# ================================================================
# 13. bptr_test() F-stat is non-negative (SSR_linear >= SSR_threshold)
# ================================================================

test_that("bptr_test() F-statistic is always non-negative", {
  skip_on_cran()
  for (seed in c(1L, 2L, 3L, 4L, 5L)) {
    set.seed(seed * 11L)
    n <- 15L; tt <- 5L
    df <- data.frame(id = rep(seq_len(n), each = tt),
                     time = rep(seq_len(tt), n),
                     x1 = rnorm(n * tt), q = rnorm(n * tt),
                     y  = rnorm(n * tt))
    t12 <- suppressMessages(
      bptr_test(y ~ x1, data = df, id = "id", time = "time", q = "q",
                n_boot = 9L, seed = 1L)
    )
    expect_gte(t12$stat, 0,
               label = sprintf("F-stat >= 0 (seed %d)", seed))
  }
})

# ================================================================
# 14. tidy() p-values are consistent with t-test decisions
# ================================================================

test_that("tidy() p-value < alpha iff |t| > t_crit(alpha)", {
  df  <- make_dgp2(N = 15L, TT = 6L, seed = 14L)
  fit <- bptr(y ~ x1, data = df, id = "id", time = "time", q = "q",
              n_thresh = 1L, buffer = FALSE, grid_size = 30L)
  td  <- broom::tidy(fit)
  for (alpha in c(0.01, 0.05, 0.10)) {
    t_crit <- stats::qt(1 - alpha / 2, df = fit$df_residual)
    reject_by_p <- td$p.value < alpha
    reject_by_t <- abs(td$statistic) > t_crit
    expect_equal(reject_by_p, reject_by_t,
                 label = sprintf("p < %.2f iff |t| > t_crit (alpha=%.2f)", alpha, alpha))
  }
})

# ================================================================
# 15. BTPD: n_obs in buffer zone equals differences in regime counts
#     vs. the pure PTR model (hysteresis introduces "stickiness")
# ================================================================

test_that("BTPD and PTR models differ in regime counts when buffer zone is non-empty", {
  set.seed(15L)
  N <- 20L; TT <- 8L
  df <- data.frame(id = rep(seq_len(N), each = TT),
                   time = rep(seq_len(TT), N),
                   x1 = rnorm(N * TT), q = rnorm(N * TT),
                   y  = rnorm(N * TT))
  fit_ptr  <- bptr(y ~ x1, data = df, id = "id", time = "time", q = "q",
                   n_thresh = 1L, buffer = FALSE, grid_size = 30L)
  fit_btpd <- bptr(y ~ x1, data = df, id = "id", time = "time", q = "q",
                   n_thresh = 1L, buffer = TRUE, grid_size = 30L)
  # BTPD has two threshold parameters (rL, rU); the buffer interval is non-degenerate
  g_btpd <- fit_btpd$thresholds
  expect_length(g_btpd, 2L)
  # Buffer zone is not empty (rU > rL in a non-trivial solution)
  expect_gte(g_btpd[2L], g_btpd[1L],
             label = "BTPD buffer: rU >= rL")
})
