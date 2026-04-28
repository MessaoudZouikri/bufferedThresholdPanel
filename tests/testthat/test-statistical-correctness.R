## ============================================================
## tests/testthat/test-statistical-correctness.R
## Numerical accuracy and statistical correctness tests
## ============================================================

# ---- Shared fixture: well-separated DGP with known threshold ----
make_dgp <- function(N = 30, TT = 10, gamma_true = 0.3, seed = 42L) {
  set.seed(seed)
  df <- data.frame(
    id   = rep(seq_len(N), each = TT),
    time = rep(seq_len(TT), N),
    x1   = rnorm(N * TT),
    q    = runif(N * TT, -2, 2)  # uniform ensures clean grid coverage
  )
  df$y <- with(df,
    2.0 * x1 +
    (q > gamma_true) * (-3.5 * x1) +  # large shift → easy threshold recovery
    rnorm(N * TT, 0, 0.25)
  )
  df
}

df_dgp <- make_dgp()

# ================================================================
# 1. Grid-search optimality: estimated SSR <= SSR at the true threshold
# ================================================================

test_that("PTR grid search SSR is <= SSR evaluated at the true threshold", {
  # grid_size > N*T ensures make_grid() collapses to all distinct observed
  # values (via unique()), so the grid covers every interval including the
  # one that contains the true threshold 0.3.
  fit <- bptr(y ~ x1, data = df_dgp,
              id = "id", time = "time", q = "q",
              n_thresh = 1, buffer = FALSE, grid_size = 500)

  fe    <- removeFE(df_dgp$y, matrix(df_dgp$x1, ncol = 1L), df_dgp$id)
  ssr_true <- computeSSR(fe$y_dm, fe$X_dm, g_vec = 0.3, q = df_dgp$q,
                          buffer = FALSE)

  expect_lte(fit$ssr, ssr_true + 1e-8)
})

# ================================================================
# 2. Within-R² lies in [0, 1] for PTR and BTPD
# ================================================================

test_that("Within R-squared is in [0, 1] for all model types", {
  for (buf in c(FALSE, TRUE)) {
    fit <- bptr(y ~ x1, data = df_dgp,
                id = "id", time = "time", q = "q",
                n_thresh = 1, buffer = buf, grid_size = 100)
    r2 <- 1 - fit$ssr / fit$tss_within
    expect_gte(r2, 0 - 1e-10,
               label = sprintf("R2 >= 0 (buffer=%s)", buf))
    expect_lte(r2, 1 + 1e-10,
               label = sprintf("R2 <= 1 (buffer=%s)", buf))
  }
})

# ================================================================
# 3. Threshold model SSR <= linear model SSR (threshold can only help)
# ================================================================

test_that("Threshold model SSR does not exceed linear model SSR", {
  fe    <- removeFE(df_dgp$y, matrix(df_dgp$x1, ncol = 1L), df_dgp$id)
  b0    <- solve(crossprod(fe$X_dm)) %*% crossprod(fe$X_dm, fe$y_dm)
  ssr_lin <- sum((fe$y_dm - fe$X_dm %*% b0)^2)

  fit <- bptr(y ~ x1, data = df_dgp,
              id = "id", time = "time", q = "q",
              n_thresh = 1, buffer = FALSE, grid_size = 100)

  expect_lte(fit$ssr, ssr_lin + 1e-6)
})

# ================================================================
# 4. Both PTR and BTPD SSR lie below the linear model SSR
# ================================================================

test_that("Both PTR and BTPD SSR are below the linear model SSR", {
  fe    <- removeFE(df_dgp$y, matrix(df_dgp$x1, ncol = 1L), df_dgp$id)
  b0    <- solve(crossprod(fe$X_dm)) %*% crossprod(fe$X_dm, fe$y_dm)
  ssr_lin <- sum((fe$y_dm - fe$X_dm %*% b0)^2)

  for (buf in c(FALSE, TRUE)) {
    fit <- bptr(y ~ x1, data = df_dgp,
                id = "id", time = "time", q = "q",
                n_thresh = 1, buffer = buf, grid_size = 100)
    expect_lte(fit$ssr, ssr_lin + 1e-6,
               label = sprintf("SSR < linear SSR (buffer=%s)", buf))
  }
})

# ================================================================
# 5. fitted + residuals = within-demeaned y (accounting identity)
# ================================================================

test_that("fitted + residuals equal within-demeaned y", {
  fit <- bptr(y ~ x1, data = df_dgp,
              id = "id", time = "time", q = "q",
              n_thresh = 1, buffer = FALSE, grid_size = 50)

  fe   <- removeFE(df_dgp$y, matrix(df_dgp$x1, ncol = 1L), df_dgp$id)
  expect_equal(fit$fitted_values + fit$residuals, fe$y_dm, tolerance = 1e-10)
})

# ================================================================
# 6. OLS normal equations hold within each regime (X'e = 0)
# ================================================================

test_that("OLS normal equations hold within each regime (X'e = 0)", {
  fit <- bptr(y ~ x1, data = df_dgp,
              id = "id", time = "time", q = "q",
              n_thresh = 1, buffer = FALSE, grid_size = 50)

  fe   <- removeFE(df_dgp$y, matrix(df_dgp$x1, ncol = 1L), df_dgp$id)
  X_dm <- fe$X_dm
  rc   <- fit$regime_classification

  for (r in 1:2) {
    idx <- which(rc == r)
    if (length(idx) < 2L) next
    xe  <- as.numeric(crossprod(X_dm[idx, , drop = FALSE], fit$residuals[idx]))
    expect_equal(xe, rep(0, ncol(X_dm)), tolerance = 1e-6,
                 label = sprintf("X'e = 0 in regime %d", r))
  }
})

# ================================================================
# 7. Regime observation counts sum to n_obs for all model variants
# ================================================================

test_that("n_obs_regime sums to n_obs for all n_thresh / buffer combinations", {
  for (n_t in c(1L, 2L)) {
    for (buf in c(FALSE, TRUE)) {
      fit <- bptr(y ~ x1, data = df_dgp,
                  id = "id", time = "time", q = "q",
                  n_thresh = n_t, buffer = buf,
                  grid_size = 30L, grid_size_3 = 5L)
      expect_equal(sum(fit$n_obs_regime), fit$n_obs,
                   label = sprintf("n_thresh=%d buffer=%s", n_t, buf))
      expect_equal(fit$n_obs, nrow(df_dgp))
    }
  }
})

# ================================================================
# 8. df_residual formula: n_obs - n_regimes*n_vars - n_groups
# ================================================================

test_that("df_residual matches formula n_obs - n_regimes*n_vars - n_groups", {
  fit <- bptr(y ~ x1, data = df_dgp,
              id = "id", time = "time", q = "q",
              n_thresh = 1L, buffer = FALSE, grid_size = 50L)

  expected_df <- nrow(df_dgp) -
    fit$n_regimes * length(fit$var_names) -
    fit$n_groups
  expect_equal(fit$df_residual, expected_df)
})

# ================================================================
# 9. SSR field equals sum(residuals^2)
# ================================================================

test_that("SSR field matches sum of squared residuals", {
  for (buf in c(FALSE, TRUE)) {
    fit <- bptr(y ~ x1, data = df_dgp,
                id = "id", time = "time", q = "q",
                n_thresh = 1L, buffer = buf, grid_size = 50L)
    expect_equal(fit$ssr, sum(fit$residuals^2), tolerance = 1e-10,
                 label = sprintf("SSR check (buffer=%s)", buf))
  }
})

# ================================================================
# 10. glance() r.squared matches 1 - SSR/TSS
# ================================================================

test_that("glance() r.squared matches 1 - SSR / tss_within", {
  fit <- bptr(y ~ x1, data = df_dgp,
              id = "id", time = "time", q = "q",
              n_thresh = 1L, buffer = FALSE, grid_size = 50L)
  gl  <- broom::glance(fit)
  expect_equal(gl$r.squared, 1 - fit$ssr / fit$tss_within, tolerance = 1e-10)
})

# ================================================================
# 11. Coefficients and std errors have correct dimensions and signs
# ================================================================

test_that("Coefficients and SE matrices have correct dimensions", {
  fit <- bptr(y ~ x1, data = df_dgp,
              id = "id", time = "time", q = "q",
              n_thresh = 1L, buffer = FALSE, grid_size = 50L)

  expect_equal(dim(fit$coefficients), c(1L, 2L))   # 1 var × 2 regimes
  expect_equal(dim(fit$std_errors),   c(1L, 2L))
  expect_true(all(fit$std_errors > 0))
})

# ================================================================
# 12. tidy() p-values are in [0, 1]
# ================================================================

test_that("tidy() p.values are in [0, 1]", {
  fit <- bptr(y ~ x1, data = df_dgp,
              id = "id", time = "time", q = "q",
              n_thresh = 1L, buffer = FALSE, grid_size = 50L)
  td  <- broom::tidy(fit)
  expect_true(all(td$p.value >= 0 & td$p.value <= 1))
})

# ================================================================
# 13. tidy(conf.int = TRUE) produces conf.low <= estimate <= conf.high
# ================================================================

test_that("tidy() confidence intervals bracket the point estimate", {
  fit <- bptr(y ~ x1, data = df_dgp,
              id = "id", time = "time", q = "q",
              n_thresh = 1L, buffer = FALSE, grid_size = 50L)
  td  <- broom::tidy(fit, conf.int = TRUE)
  expect_true(all(td$conf.low  <= td$estimate + 1e-10))
  expect_true(all(td$conf.high >= td$estimate - 1e-10))
})

# ================================================================
# 14. 3-regime model: regime classification in {1, 2, 3}
# ================================================================

test_that("3-regime model produces regime classifications in {1, 2, 3}", {
  skip_on_cran()
  fit <- bptr(y ~ x1, data = df_dgp,
              id = "id", time = "time", q = "q",
              n_thresh = 2L, buffer = FALSE, grid_size = 50L)
  expect_true(all(fit$regime_classification %in% 1:3))
  expect_equal(fit$n_regimes, 3L)
})
