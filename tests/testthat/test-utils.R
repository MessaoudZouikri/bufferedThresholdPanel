## ============================================================
## tests/testthat/test-utils.R
## Unit tests for low-level utility functions
## ============================================================

# ================================================================
# removeFE()
# ================================================================

test_that("removeFE() produces within-group means of zero", {
  set.seed(1L)
  n_obs <- 50L
  id    <- rep(seq_len(10L), each = 5L)
  y     <- rnorm(n_obs)
  X     <- matrix(rnorm(n_obs * 2L), nrow = n_obs, ncol = 2L)

  fe <- removeFE(y, X, id)

  for (i in seq_len(10L)) {
    idx <- which(id == i)
    expect_equal(mean(fe$y_dm[idx]), 0, tolerance = 1e-12,
                 label = sprintf("within-mean(y_dm) = 0 for unit %d", i))
    for (k in seq_len(2L)) {
      expect_equal(mean(fe$X_dm[idx, k]), 0, tolerance = 1e-12,
                   label = sprintf("within-mean(X_dm[,%d]) = 0 for unit %d", k, i))
    }
  }
})

test_that("removeFE() preserves dimensions", {
  set.seed(2L)
  n_obs <- 30L; id <- rep(seq_len(5L), each = 6L)
  y <- rnorm(n_obs)
  X <- matrix(rnorm(n_obs * 3L), nrow = n_obs, ncol = 3L)
  fe <- removeFE(y, X, id)
  expect_equal(length(fe$y_dm), n_obs)
  expect_equal(dim(fe$X_dm), c(n_obs, 3L))
})

test_that("removeFE() preserves column names of X", {
  set.seed(3L)
  n_obs <- 20L; id <- rep(seq_len(4L), each = 5L)
  y <- rnorm(n_obs)
  X <- matrix(rnorm(n_obs * 2L), nrow = n_obs,
               dimnames = list(NULL, c("alpha", "beta")))
  fe <- removeFE(y, X, id)
  expect_equal(colnames(fe$X_dm), c("alpha", "beta"))
})

test_that("removeFE() accepts a vector X and converts it to a matrix", {
  set.seed(4L)
  n_obs <- 20L; id <- rep(seq_len(4L), each = 5L)
  y     <- rnorm(n_obs)
  x_vec <- rnorm(n_obs)
  fe    <- removeFE(y, x_vec, id)
  expect_equal(length(fe$y_dm), n_obs)
  expect_true(is.matrix(fe$X_dm))
  expect_equal(dim(fe$X_dm), c(n_obs, 1L))
})

test_that("removeFE() output is a list with y_dm and X_dm", {
  set.seed(5L)
  n_obs <- 15L; id <- rep(seq_len(3L), each = 5L)
  y <- rnorm(n_obs); X <- matrix(rnorm(n_obs), ncol = 1L)
  fe <- removeFE(y, X, id)
  expect_named(fe, c("y_dm", "X_dm"))
})

# ================================================================
# buildIndicators()
# ================================================================

test_that("buildIndicators() returns a binary (0/1) vector", {
  q <- seq(-3, 3, length.out = 60)
  d <- buildIndicators(q, gamma = 0)
  expect_true(all(d %in% c(0, 1)))
})

test_that("buildIndicators() maps q <= gamma to 1 and q > gamma to 0", {
  q     <- seq(-3, 3, length.out = 61)
  gamma <- 0.5
  d     <- buildIndicators(q, gamma)
  expect_true(all(d[q <= gamma] == 1L))
  expect_true(all(d[q > gamma]  == 0L))
})

test_that("buildIndicators() handles scalar q correctly", {
  expect_equal(buildIndicators(-1, gamma = 0), 1)
  expect_equal(buildIndicators( 1, gamma = 0), 0)
  expect_equal(buildIndicators( 0, gamma = 0), 1)  # q == gamma → indicator = 1
})

# ================================================================
# buildBufferIndicators()
# ================================================================

test_that("buildBufferIndicators() returns only 0s and 1s", {
  q  <- c(-2, -1, -0.1, 0.1, 1, 2)
  id <- rep(1L, 6L)
  d  <- buildBufferIndicators(q, g1 = -0.3, g2 = 0.3, prev_d = NULL, id = id)
  expect_true(all(d %in% c(0L, 1L)))
})

test_that("buildBufferIndicators() correctly enforces hysteresis in buffer zone", {
  # Sequence: R1 → jumps to R2 (q>g2) → enters buffer zone → should stay in R2
  id <- rep(1L, 5L)
  q  <- c(-1.0, 1.0, 0.0, 0.0, -1.0)  # -1:R1, 1:R2, 0:buffer, 0:buffer, -1:R1
  d  <- buildBufferIndicators(q, g1 = -0.5, g2 = 0.5, prev_d = NULL, id = id)

  expect_equal(d[1L], 0L)  # q < g1 → regime 1
  expect_equal(d[2L], 1L)  # q > g2 → regime 2
  expect_equal(d[3L], 1L)  # buffer, came from R2 → stay in R2
  expect_equal(d[4L], 1L)  # buffer, came from R2 → stay in R2
  expect_equal(d[5L], 0L)  # q < g1 → regime 1
})

test_that("buildBufferIndicators() resets hysteresis state at each new unit", {
  id <- c(1L, 1L, 2L, 2L)
  q  <- c(1.0, 0.0, 0.0, 1.0)
  g1 <- -0.5; g2 <- 0.5
  d  <- buildBufferIndicators(q, g1 = g1, g2 = g2, prev_d = NULL, id = id)

  # Unit 1: q[1]=1>g2→R2, q[2]=0∈buffer came from R2 → R2
  expect_equal(d[1L], 1L)
  expect_equal(d[2L], 1L)
  # Unit 2 starts fresh: q[3]=0∈buffer, first obs, q<=g2 → R1
  expect_equal(d[3L], 0L)
  # Unit 2: q[4]=1>g2 → R2
  expect_equal(d[4L], 1L)
})

test_that("buildBufferIndicators() output has the same length as q", {
  q  <- rnorm(100L); id <- rep(seq_len(10L), each = 10L)
  d  <- buildBufferIndicators(q, g1 = -0.5, g2 = 0.5, prev_d = NULL, id = id)
  expect_equal(length(d), 100L)
})

# ================================================================
# buildBufferIndicators3()
# ================================================================

test_that("buildBufferIndicators3() returns only 1L, 2L, 3L", {
  set.seed(10L)
  q  <- rnorm(60L); id <- rep(seq_len(6L), each = 10L)
  d  <- buildBufferIndicators3(q, rL1 = -0.5, rU1 = -0.1,
                                rL2 = 0.1,  rU2 = 0.5,  id = id)
  expect_true(all(d %in% c(1L, 2L, 3L)))
})

test_that("buildBufferIndicators3() clear-regime observations are deterministic", {
  # Single-unit sequence: clearly in R1 or R3
  id <- rep(1L, 4L)
  q  <- c(-3.0, 3.0, -3.0, 3.0)
  d  <- buildBufferIndicators3(q, rL1 = -0.5, rU1 = -0.1,
                                rL2 = 0.1,  rU2 = 0.5,  id = id)
  expect_equal(d[1L], 1L)  # q << rL1 → regime 1
  expect_equal(d[2L], 3L)  # q >> rU2 → regime 3
})

test_that("buildBufferIndicators3() middle zone (rU1 < q <= rL2) gives regime 2", {
  id <- rep(1L, 3L)
  q  <- c(-3.0, 0.0, 0.0)   # R1 → clearly R2 → clearly R2
  d  <- buildBufferIndicators3(q, rL1 = -0.5, rU1 = -0.1,
                                rL2 = 0.1,  rU2 = 0.5,  id = id)
  expect_equal(d[2L], 2L)
  expect_equal(d[3L], 2L)
})

test_that("buildBufferIndicators3() has correct output length", {
  q  <- rnorm(40L); id <- rep(seq_len(8L), each = 5L)
  d  <- buildBufferIndicators3(q, -0.5, -0.1, 0.1, 0.5, id)
  expect_equal(length(d), 40L)
})

# ================================================================
# concentratedOLS()
# ================================================================

test_that("concentratedOLS() satisfies OLS normal equations within each regime", {
  set.seed(5L)
  n   <- 50L
  X   <- matrix(rnorm(n * 2L), nrow = n, ncol = 2L,
                dimnames = list(NULL, c("x1", "x2")))
  y   <- X %*% c(2.0, -1.0) + rnorm(n, 0, 0.3)
  ind1 <- as.numeric(seq_len(n) <= 25L)
  ind2 <- 1 - ind1

  ols <- concentratedOLS(y, X, ind1, ind2)

  xe1 <- as.numeric(crossprod(X[ind1 == 1L, ], ols$resid[ind1 == 1L]))
  xe2 <- as.numeric(crossprod(X[ind2 == 1L, ], ols$resid[ind2 == 1L]))
  expect_equal(xe1, c(0, 0), tolerance = 1e-8, label = "X'e = 0 in regime 1")
  expect_equal(xe2, c(0, 0), tolerance = 1e-8, label = "X'e = 0 in regime 2")
})

test_that("concentratedOLS() returns correct named components", {
  set.seed(6L)
  n    <- 30L
  X    <- matrix(rnorm(n), nrow = n, ncol = 1L)
  y    <- rnorm(n)
  ind1 <- as.numeric(seq_len(n) <= 15L)
  ind2 <- 1 - ind1
  ols  <- concentratedOLS(y, X, ind1, ind2)

  expect_named(ols, c("beta1", "beta2", "fitted", "resid", "sigma2"))
  expect_equal(length(ols$fitted), n)
  expect_equal(length(ols$resid),  n)
  expect_true(ols$sigma2 >= 0)
})

test_that("concentratedOLS() fitted + resid = y", {
  set.seed(7L)
  n    <- 40L
  X    <- matrix(rnorm(n * 2L), nrow = n, ncol = 2L)
  y    <- rnorm(n)
  ind1 <- as.numeric(seq_len(n) <= 20L)
  ind2 <- 1 - ind1
  ols  <- concentratedOLS(y, X, ind1, ind2)
  expect_equal(ols$fitted + ols$resid, y, tolerance = 1e-10)
})

# ================================================================
# concentratedOLS3()
# ================================================================

test_that("concentratedOLS3() returns correct named components including beta3", {
  set.seed(8L)
  n    <- 60L
  X    <- matrix(rnorm(n * 2L), nrow = n, ncol = 2L)
  y    <- rnorm(n)
  ind1 <- as.numeric(seq_len(n) <= 20L)
  ind2 <- as.numeric(seq_len(n) > 20L & seq_len(n) <= 40L)
  ind3 <- as.numeric(seq_len(n) > 40L)
  ols3 <- concentratedOLS3(y, X, ind1, ind2, ind3)

  expect_named(ols3, c("beta1", "beta2", "beta3", "fitted", "resid", "sigma2"))
  expect_equal(length(ols3$beta1), 2L)
  expect_equal(length(ols3$beta2), 2L)
  expect_equal(length(ols3$beta3), 2L)
  expect_equal(length(ols3$fitted), n)
  expect_true(ols3$sigma2 >= 0)
})

test_that("concentratedOLS3() fitted + resid = y", {
  set.seed(9L)
  n    <- 45L
  X    <- matrix(rnorm(n), nrow = n, ncol = 1L)
  y    <- rnorm(n)
  ind1 <- as.numeric(seq_len(n) <= 15L)
  ind2 <- as.numeric(seq_len(n) > 15L & seq_len(n) <= 30L)
  ind3 <- as.numeric(seq_len(n) > 30L)
  ols3 <- concentratedOLS3(y, X, ind1, ind2, ind3)
  expect_equal(ols3$fitted + ols3$resid, y, tolerance = 1e-10)
})

# ================================================================
# robustSE()
# ================================================================

test_that("robustSE() returns positive finite SEs for all HC types", {
  set.seed(11L)
  n <- 30L
  X <- matrix(rnorm(n * 2L), nrow = n, ncol = 2L)
  r <- rnorm(n)
  for (type in c("HC0", "HC1", "HC2", "HC3")) {
    se <- robustSE(X, r, type = type)
    expect_equal(length(se), 2L, label = sprintf("length (type=%s)", type))
    expect_true(all(is.finite(se)), label = sprintf("finite (type=%s)", type))
    expect_true(all(se > 0), label = sprintf("positive (type=%s)", type))
  }
})

test_that("robustSE() corrections are non-decreasing: HC0 <= HC1 <= HC2 <= HC3", {
  set.seed(12L)
  n  <- 50L
  X  <- matrix(rnorm(n), nrow = n, ncol = 1L)
  r  <- rnorm(n)
  se <- vapply(c("HC0", "HC1", "HC2", "HC3"),
               function(t) robustSE(X, r, t), numeric(1L))
  expect_lte(se["HC0"], se["HC1"] + 1e-12)
  expect_lte(se["HC1"], se["HC2"] + 1e-12)
  expect_lte(se["HC2"], se["HC3"] + 1e-12)
})

test_that("robustSE() accepts a numeric vector X and converts it", {
  set.seed(13L)
  n <- 20L; x <- rnorm(n); r <- rnorm(n)
  se <- robustSE(x, r)
  expect_equal(length(se), 1L)
  expect_true(is.finite(se) && se > 0)
})

test_that("robustSE() returns 0-vector when X or resid is empty", {
  X0 <- matrix(numeric(0L), nrow = 0L, ncol = 2L)
  r0 <- numeric(0L)
  se <- robustSE(X0, r0)
  expect_equal(length(se), 2L)
  expect_true(all(se == 0))
})

test_that("robustSE() returns Inf when system is underdetermined (n <= k)", {
  X <- matrix(c(1, 2, 3, 4), nrow = 2L, ncol = 2L)
  r <- c(0.1, -0.1)
  se <- robustSE(X, r)
  expect_true(all(is.infinite(se)))
})

# ================================================================
# computeSSR()
# ================================================================

test_that("computeSSR() returns a finite non-negative scalar", {
  set.seed(14L)
  n <- 40L
  X <- matrix(rnorm(n * 2L), nrow = n, ncol = 2L)
  y <- rnorm(n)
  q <- rnorm(n)
  s <- computeSSR(y, X, g_vec = 0.0, q_dm = q, buffer = FALSE)
  expect_equal(length(s), 1L)
  expect_true(is.finite(s))
  expect_gte(s, 0)
})

test_that("computeSSR() returns Inf when a regime has fewer obs than regressors", {
  n <- 10L
  X <- matrix(rnorm(n * 3L), nrow = n, ncol = 3L)  # 3 regressors
  y <- rnorm(n)
  # All q values << 0 → all obs in regime 1, regime 2 empty → Inf
  q <- rep(-10, n)
  s <- computeSSR(y, X, g_vec = 0.0, q_dm = q, buffer = FALSE)
  expect_equal(s, Inf)
})

test_that("computeSSR() result is consistent with manual concentratedOLS()", {
  set.seed(15L)
  n    <- 30L
  X    <- matrix(rnorm(n), nrow = n, ncol = 1L)
  y    <- rnorm(n)
  q    <- rnorm(n)
  g    <- 0.2
  ind1 <- as.numeric(q <= g); ind2 <- 1 - ind1
  manual_ssr <- sum(concentratedOLS(y, X, ind1, ind2)$resid^2)
  auto_ssr   <- computeSSR(y, X, g_vec = g, q_dm = q, buffer = FALSE)
  expect_equal(auto_ssr, manual_ssr, tolerance = 1e-10)
})

# ================================================================
# validatePanel()
# ================================================================

test_that("validatePanel() returns correct structure", {
  df <- data.frame(id = rep(1:5, each = 4), time = rep(1:4, 5))
  info <- validatePanel(df, "id", "time")
  expect_named(info, c("n_units", "n_periods", "n_obs", "balanced", "has_missing"))
  expect_equal(info$n_units,   5L)
  expect_equal(info$n_periods, 4L)
  expect_equal(info$n_obs,    20L)
  expect_true(info$balanced)
  expect_false(info$has_missing)
})

test_that("validatePanel() detects unbalanced panel", {
  df_unbal <- data.frame(
    id   = c(1L, 1L, 1L, 2L, 2L),
    time = c(1L, 2L, 3L, 1L, 2L)
  )
  info <- validatePanel(df_unbal, "id", "time")
  expect_false(info$balanced)
  expect_equal(info$n_units, 2L)
  expect_equal(info$n_obs,   5L)
})
