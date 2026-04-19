## ============================================================
## tests/testthat/test-mathematical-identities.R
##
## Exact arithmetic checks: every assertion here is a
## mathematical theorem, not an empirical approximation.
## All tests are deterministic and fast.
## ============================================================

library(broom)

# ---- Shared fixtures -----------------------------------------------

make_fit <- function(seed = 1L, buffer = FALSE, n_thresh = 1L,
                     N = 15L, TT = 6L, grid_size = 30L,
                     grid_size_3 = 5L) {
  set.seed(seed)
  df <- data.frame(
    id   = rep(seq_len(N), each = TT),
    time = rep(seq_len(TT), N),
    x1   = rnorm(N * TT),
    x2   = rnorm(N * TT),
    q    = rnorm(N * TT)
  )
  df$y <- with(df,
    1.5 * x1 - 0.6 * x2 +
    (q > 0.2) * (-2.0 * x1 + 0.8 * x2) + rnorm(N * TT, 0, 0.4)
  )
  list(
    fit = bptr(y ~ x1 + x2, data = df, id = "id", time = "time", q = "q",
               n_thresh = n_thresh, buffer = buffer,
               grid_size = grid_size, grid_size_3 = grid_size_3),
    df  = df
  )
}

# ================================================================
# 1.  tidy() — exact t-statistic formula: t = coef / se
# ================================================================

test_that("tidy() t-statistic equals coef / std.error to machine precision", {
  obj <- make_fit(seed = 1L)
  td  <- tidy(obj$fit)
  expect_equal(td$statistic, td$estimate / td$std.error,
               tolerance = 1e-12, label = "t = coef / se")
})

# ================================================================
# 2.  tidy() — exact p-value formula: p = 2 * pt(-|t|, df)
# ================================================================

test_that("tidy() p-value equals 2 * pt(-|t|, df_residual) to machine precision", {
  obj <- make_fit(seed = 2L)
  td  <- tidy(obj$fit)
  df_r <- obj$fit$df_residual
  expected_p <- 2 * stats::pt(-abs(td$statistic), df = df_r)
  expect_equal(td$p.value, expected_p,
               tolerance = 1e-12, label = "p = 2 * pt(-|t|, df)")
})

# ================================================================
# 3.  tidy() — exact confidence interval formula
# ================================================================

test_that("tidy() confidence intervals exactly equal coef ± t_crit * se", {
  obj <- make_fit(seed = 3L)
  fit <- obj$fit
  td  <- tidy(fit, conf.int = TRUE, conf.level = 0.95)
  alpha  <- 0.05
  t_crit <- stats::qt(1 - alpha / 2, df = fit$df_residual)
  expect_equal(td$conf.low,  td$estimate - t_crit * td$std.error,
               tolerance = 1e-12, label = "conf.low = estimate - t_crit*se")
  expect_equal(td$conf.high, td$estimate + t_crit * td$std.error,
               tolerance = 1e-12, label = "conf.high = estimate + t_crit*se")
})

test_that("tidy() confidence intervals respect a non-default conf.level", {
  obj <- make_fit(seed = 3L)
  fit <- obj$fit
  for (cl in c(0.90, 0.99)) {
    td    <- tidy(fit, conf.int = TRUE, conf.level = cl)
    alpha <- 1 - cl
    t_crit <- stats::qt(1 - alpha / 2, df = fit$df_residual)
    expect_equal(td$conf.low,  td$estimate - t_crit * td$std.error,
                 tolerance = 1e-12,
                 label = sprintf("conf.low correct at level %.2f", cl))
    expect_equal(td$conf.high, td$estimate + t_crit * td$std.error,
                 tolerance = 1e-12,
                 label = sprintf("conf.high correct at level %.2f", cl))
  }
})

# ================================================================
# 4.  glance() — sigma = sqrt(SSR / df_residual)
# ================================================================

test_that("glance() sigma equals sqrt(SSR / df_residual) exactly", {
  obj <- make_fit(seed = 4L)
  fit <- obj$fit
  gl  <- glance(fit)
  expected_sigma <- sqrt(fit$ssr / max(1, fit$df_residual))
  expect_equal(gl$sigma, expected_sigma, tolerance = 1e-12,
               label = "sigma = sqrt(SSR / df)")
})

# ================================================================
# 5.  glance() — AIC formula: n * log(SSR/n) + 2 * K
# ================================================================

test_that("glance() aic_approx matches n * log(SSR/n) + 2*K exactly", {
  obj <- make_fit(seed = 5L)
  fit <- obj$fit
  gl  <- glance(fit)
  K <- length(fit$thresholds) + length(as.vector(fit$coefficients))
  expected_aic <- fit$n_obs * log(fit$ssr / fit$n_obs) + 2 * K
  expect_equal(gl$aic_approx, expected_aic, tolerance = 1e-10,
               label = "AIC = n*log(SSR/n) + 2K")
})

# ================================================================
# 6.  vcov() — diagonal equals squared standard errors
# ================================================================

test_that("vcov() diagonal entries equal std_errors^2 exactly", {
  obj    <- make_fit(seed = 6L)
  fit    <- obj$fit
  V      <- vcov(fit)
  se_vec <- as.vector(fit$std_errors)
  expect_equal(unname(diag(V)), se_vec^2, tolerance = 1e-12,
               label = "diag(vcov) = se^2")
})

test_that("vcov() is symmetric", {
  obj <- make_fit(seed = 6L)
  V   <- vcov(obj$fit)
  expect_equal(V, t(V), tolerance = 1e-12, label = "vcov is symmetric")
})

# ================================================================
# 7.  threshold_tidy() — regime-size counts match indicator sums
# ================================================================

test_that("threshold_tidy() n_regime1 and n_regime2 match regime_classification sums", {
  obj <- make_fit(seed = 7L)
  fit <- obj$fit
  tt  <- threshold_tidy(fit)
  rc  <- fit$regime_classification
  for (i in seq_len(nrow(tt))) {
    expect_equal(tt$n_regime1[i], sum(rc <= i, na.rm = TRUE),
                 label = sprintf("n_regime1[%d] = sum(rc <= %d)", i, i))
    expect_equal(tt$n_regime2[i], sum(rc > i, na.rm = TRUE),
                 label = sprintf("n_regime2[%d] = sum(rc > %d)", i, i))
  }
})

# ================================================================
# 8.  computeSSR() cross-validation against concentratedOLS()
# ================================================================

test_that("computeSSR() equals sum(concentratedOLS()$resid^2) for multiple thresholds", {
  set.seed(8L)
  n   <- 40L
  X   <- matrix(rnorm(n * 2L), nrow = n, ncol = 2L)
  y   <- rnorm(n)
  q   <- rnorm(n)
  for (g in c(-1.0, -0.5, 0.0, 0.5, 1.0)) {
    ind1 <- buildIndicators(q, g); ind2 <- 1 - ind1
    manual <- sum(concentratedOLS(y, X, ind1, ind2)$resid^2)
    auto   <- computeSSR(y, X, g_vec = g, q_dm = q, buffer = FALSE)
    expect_equal(auto, manual, tolerance = 1e-12,
                 label = sprintf("computeSSR == manual at gamma = %.1f", g))
  }
})

# ================================================================
# 9.  concentratedOLS() — fitted values equal X_dm * beta per regime
# ================================================================

test_that("concentratedOLS() fitted values equal X_dm * beta within each regime", {
  set.seed(9L)
  n    <- 50L
  X    <- matrix(rnorm(n * 2L), nrow = n, ncol = 2L)
  y    <- X %*% c(2, -1) + rnorm(n, 0, 0.5)
  ind1 <- as.numeric(seq_len(n) <= 25L); ind2 <- 1 - ind1
  ols  <- concentratedOLS(y, X, ind1, ind2)

  idx1 <- which(ind1 == 1); idx2 <- which(ind2 == 1)
  expect_equal(ols$fitted[idx1],
               as.vector(X[idx1, , drop = FALSE] %*% ols$beta1),
               tolerance = 1e-12, label = "fitted[r1] = X_dm[r1] * beta1")
  expect_equal(ols$fitted[idx2],
               as.vector(X[idx2, , drop = FALSE] %*% ols$beta2),
               tolerance = 1e-12, label = "fitted[r2] = X_dm[r2] * beta2")
})

# ================================================================
# 10. removeFE() — idempotence: applying twice gives the same result
# ================================================================

test_that("removeFE() is idempotent: applying twice equals applying once", {
  set.seed(10L)
  n  <- 40L; id <- rep(seq_len(8L), each = 5L)
  y  <- rnorm(n)
  X  <- matrix(rnorm(n * 2L), nrow = n, ncol = 2L)
  fe1 <- removeFE(y, X, id)
  fe2 <- removeFE(fe1$y_dm, fe1$X_dm, id)
  expect_equal(fe2$y_dm, fe1$y_dm, tolerance = 1e-12,
               label = "y_dm idempotent")
  expect_equal(fe2$X_dm, fe1$X_dm, tolerance = 1e-12,
               label = "X_dm idempotent")
})

# ================================================================
# 11. PTR regime assignment — exact correspondence with indicator
# ================================================================

test_that("PTR regime_classification exactly matches q_dm <= gamma", {
  set.seed(11L)
  N <- 15L; TT <- 6L
  df <- data.frame(id = rep(seq_len(N), each = TT),
                   time = rep(seq_len(TT), N),
                   x1 = rnorm(N * TT), q = rnorm(N * TT),
                   y  = rnorm(N * TT))
  fit   <- bptr(y ~ x1, data = df, id = "id", time = "time", q = "q",
                n_thresh = 1L, buffer = FALSE, grid_size = 30L)
  gamma <- fit$thresholds  # single value for PTR
  q_dm  <- removeFE(df$q, matrix(1, nrow = nrow(df)), df$id)$y_dm
  rc    <- fit$regime_classification
  expect_true(all(rc[q_dm <= gamma] == 1L),
              label = "q_dm <= gamma → regime 1")
  expect_true(all(rc[q_dm  > gamma] == 2L),
              label = "q_dm > gamma → regime 2")
})

# ================================================================
# 12. OLS normal equations — X'e = 0 within each regime (3-regime PTR)
# ================================================================

test_that("OLS normal equations hold in each regime of a 3-regime PTR model", {
  skip_on_cran()
  set.seed(12L)
  N <- 20L; TT <- 8L
  df <- data.frame(id = rep(seq_len(N), each = TT),
                   time = rep(seq_len(TT), N),
                   x1 = rnorm(N * TT), q = rnorm(N * TT),
                   y  = rnorm(N * TT))
  fit <- bptr(y ~ x1, data = df, id = "id", time = "time", q = "q",
              n_thresh = 2L, buffer = FALSE, grid_size = 30L)
  fe   <- removeFE(df$y, matrix(df$x1, ncol = 1L), df$id)
  X_dm <- fe$X_dm
  rc   <- fit$regime_classification
  for (r in 1:3) {
    idx <- which(rc == r)
    if (length(idx) < 2L) next
    xe <- as.numeric(crossprod(X_dm[idx, , drop = FALSE], fit$residuals[idx]))
    expect_equal(xe, rep(0, ncol(X_dm)), tolerance = 1e-6,
                 label = sprintf("X'e = 0 in regime %d", r))
  }
})

# ================================================================
# 13. robustSE() HC0 — exact sandwich formula verification
# ================================================================

test_that("robustSE() HC0 matches the sandwich formula exactly", {
  set.seed(13L)
  n  <- 30L; k <- 2L
  X  <- matrix(rnorm(n * k), nrow = n, ncol = k)
  e  <- rnorm(n)
  se_func <- robustSE(X, e, type = "HC0")

  XTX_inv <- solve(crossprod(X))
  omega    <- e^2
  V_manual <- XTX_inv %*% crossprod(X, diag(omega) %*% X) %*% XTX_inv
  se_manual <- sqrt(diag(V_manual))
  expect_equal(se_func, se_manual, tolerance = 1e-12,
               label = "HC0 sandwich matches manual formula")
})

# ================================================================
# 14. robustSE() HC1 = HC0 * sqrt(n / (n - k))  [exact]
# ================================================================

test_that("robustSE() HC1 equals HC0 * sqrt(n / (n - k)) exactly", {
  set.seed(14L)
  n  <- 40L; k <- 2L
  X  <- matrix(rnorm(n * k), nrow = n, ncol = k)
  e  <- rnorm(n)
  se0 <- robustSE(X, e, type = "HC0")
  se1 <- robustSE(X, e, type = "HC1")
  expect_equal(se1, se0 * sqrt(n / (n - k)), tolerance = 1e-12,
               label = "HC1 = HC0 * sqrt(n/(n-k))")
})

# ================================================================
# 15. robustSE() HC3 >= HC2 >= HC0  [analytical: leverage corrections]
# ================================================================

test_that("robustSE() satisfies HC3 >= HC2 >= HC0 for each coefficient", {
  for (seed in seq_len(10L)) {
    set.seed(seed * 31L)
    n  <- 30L; k <- 2L
    X  <- matrix(rnorm(n * k), nrow = n, ncol = k)
    e  <- rnorm(n)
    se0 <- robustSE(X, e, type = "HC0")
    se2 <- robustSE(X, e, type = "HC2")
    se3 <- robustSE(X, e, type = "HC3")
    expect_true(all(se2 >= se0 - 1e-12),
                label = sprintf("HC2 >= HC0 (seed %d)", seed))
    expect_true(all(se3 >= se2 - 1e-12),
                label = sprintf("HC3 >= HC2 (seed %d)", seed))
  }
})

# ================================================================
# 16. Within R² nesting: threshold model R² >= linear model R²
# ================================================================

test_that("within-R² of threshold model >= within-R² of linear model", {
  set.seed(16L)
  N <- 20L; TT <- 8L
  df <- data.frame(id = rep(seq_len(N), each = TT),
                   time = rep(seq_len(TT), N),
                   x1 = rnorm(N * TT), q = rnorm(N * TT),
                   y  = rnorm(N * TT))
  fe    <- removeFE(df$y, matrix(df$x1, ncol = 1L), df$id)
  b0    <- solve(crossprod(fe$X_dm)) %*% crossprod(fe$X_dm, fe$y_dm)
  ssr_lin <- sum((fe$y_dm - fe$X_dm %*% b0)^2)
  tss_w   <- sum((fe$y_dm - mean(fe$y_dm))^2)
  r2_lin  <- 1 - ssr_lin / tss_w

  for (buf in c(FALSE, TRUE)) {
    fit  <- bptr(y ~ x1, data = df, id = "id", time = "time", q = "q",
                 n_thresh = 1L, buffer = buf, grid_size = 30L)
    r2_t <- 1 - fit$ssr / fit$tss_within
    expect_gte(r2_t, r2_lin - 1e-8,
               label = sprintf("R²_threshold >= R²_linear (buffer=%s)", buf))
  }
})

# ================================================================
# 17. augment() column identity with internal fit fields
# ================================================================

test_that("augment() .fitted, .resid, .regime exactly match internal fit fields", {
  obj <- make_fit(seed = 17L)
  fit <- obj$fit
  aug <- augment(fit)
  expect_equal(aug$.fitted, fit$fitted_values, tolerance = 1e-12,
               label = ".fitted == fitted_values")
  expect_equal(aug$.resid,  fit$residuals,     tolerance = 1e-12,
               label = ".resid == residuals")
  expect_equal(aug$.regime, fit$regime_classification,
               label = ".regime == regime_classification")
})

# ================================================================
# 18. buildBufferIndicators3() — transition rules verified deterministically
# ================================================================

test_that("buildBufferIndicators3() obeys all five transition rules exactly", {
  rL1 <- -0.5; rU1 <- -0.1; rL2 <- 0.1; rU2 <- 0.5
  id  <- rep(1L, 8L)
  #         clear R1   buf1    clear R2   buf2     clear R3   buf2    clear R2   buf1
  q   <- c(-1.0,  -0.3,   0.0,     0.3,     1.0,    0.3,     0.0,    -0.3)
  d   <- buildBufferIndicators3(q, rL1, rU1, rL2, rU2, id)
  expect_equal(d[1L], 1L, label = "q < rL1 → regime 1")
  expect_equal(d[2L], 1L, label = "q in buf1, came from R1 → stay R1")
  expect_equal(d[3L], 2L, label = "q in (rU1, rL2) → regime 2")
  expect_equal(d[4L], 2L, label = "q in buf2, came from R2 → stay R2")
  expect_equal(d[5L], 3L, label = "q > rU2 → regime 3")
  expect_equal(d[6L], 3L, label = "q in buf2, came from R3 → stay R3")
  expect_equal(d[7L], 2L, label = "q in (rU1, rL2) → regime 2")
  expect_equal(d[8L], 2L, label = "q in buf1, came from R2 → go to R2")
})

test_that("buildBufferIndicators3() initialises first obs deterministically", {
  rL1 <- -0.5; rU1 <- -0.1; rL2 <- 0.1; rU2 <- 0.5
  # First obs of each unit: assign to lower regime of its zone
  ids <- c(1L, 2L, 3L, 4L)
  qs  <- c(-0.3,   # buf1 first obs → lower regime = R1
            0.0,   # clear R2 first obs → R2
            0.3,   # buf2 first obs → lower regime = R2
            1.0)   # clear R3 → R3
  d <- buildBufferIndicators3(qs, rL1, rU1, rL2, rU2, ids)
  expect_equal(d[1L], 1L, label = "buf1 first obs → R1")
  expect_equal(d[2L], 2L, label = "clear R2 first obs → R2")
  expect_equal(d[3L], 2L, label = "buf2 first obs → R2")
  expect_equal(d[4L], 3L, label = "clear R3 first obs → R3")
})

# ================================================================
# 19. BTPD buffer zone — obs inside buffer retain previous regime
# ================================================================

test_that("BTPD buffer zone: observations inside (g1, g2] keep previous regime", {
  set.seed(19L)
  N <- 10L; TT <- 8L
  df <- data.frame(id = rep(seq_len(N), each = TT),
                   time = rep(seq_len(TT), N),
                   x1 = rnorm(N * TT), q = rnorm(N * TT),
                   y  = rnorm(N * TT))
  fit  <- bptr(y ~ x1, data = df, id = "id", time = "time", q = "q",
               n_thresh = 1L, buffer = TRUE, grid_size = 30L)
  g1   <- fit$thresholds[1]; g2 <- fit$thresholds[2]
  q_dm <- removeFE(df$q, matrix(1, nrow = nrow(df)), df$id)$y_dm
  rc   <- fit$regime_classification
  in_buf <- which(q_dm > g1 & q_dm <= g2)
  # For each buffer-zone observation (after the first in its unit),
  # regime must equal the previous observation's regime within the same unit
  for (pos in in_buf) {
    unit_idx <- which(df$id == df$id[pos])
    unit_pos <- which(unit_idx == pos)
    if (unit_pos == 1L) next   # first obs: initialisation rule, skip
    prev_pos <- unit_idx[unit_pos - 1L]
    expect_equal(rc[pos], rc[prev_pos],
                 label = sprintf("buffer obs at row %d retains regime %d",
                                 pos, rc[prev_pos]))
  }
})

# ================================================================
# 20. df_residual: exact formula n_obs - n_regimes*n_vars - n_groups
# ================================================================

test_that("df_residual satisfies n_obs - n_regimes*n_vars - n_groups for PTR and BTPD", {
  for (buf in c(FALSE, TRUE)) {
    obj <- make_fit(seed = 20L, buffer = buf)
    fit <- obj$fit
    expected_df <- fit$n_obs -
      fit$n_regimes * length(fit$var_names) -
      fit$n_groups
    expect_equal(fit$df_residual, expected_df,
                 label = sprintf("df_residual formula (buffer=%s)", buf))
  }
})
