## ============================================================
## tests/testthat/test-bptr.R
## Smoke tests for bufferedThresholdPanel
## ============================================================

# ---- Shared test fixture -----------------------------------------------
make_panel <- function(N = 20, TT = 8, seed = 1L) {
  set.seed(seed)
  df <- data.frame(
    id   = rep(seq_len(N), each = TT),
    time = rep(seq_len(TT), N),
    x1   = rnorm(N * TT),
    x2   = rnorm(N * TT),
    q    = rnorm(N * TT)
  )
  df$y <- with(df,
    1.5 * x1 - 0.4 * x2 +
    (q > 0.3) * (-1.0 * x1 + 0.6 * x2) +
    rnorm(N * TT, 0, 0.5)
  )
  df
}

df <- make_panel()

# ================================================================
# 1. bptr() — basic structure
# ================================================================

test_that("bptr() returns correct class and basic structure", {
  fit <- bptr(y ~ x1 + x2, data = df,
              id = "id", time = "time", q = "q",
              n_thresh = 1, buffer = TRUE)

  expect_s3_class(fit, "bptr")
  expect_equal(fit$n_regimes, 2L)
  expect_true(fit$buffer)
  expect_length(fit$thresholds, 2L)           # rL and rU
  expect_length(fit$residuals, nrow(df))
  expect_length(fit$fitted_values, nrow(df))
  expect_true(is.numeric(fit$ssr))
  expect_true(fit$ssr > 0)
  expect_equal(sum(fit$n_obs_regime), fit$n_obs)
})

test_that("bptr() PTR (buffer=FALSE) returns single threshold", {
  fit <- bptr(y ~ x1 + x2, data = df,
              id = "id", time = "time", q = "q",
              n_thresh = 1, buffer = FALSE)

  expect_s3_class(fit, "bptr")
  expect_false(fit$buffer)
  expect_length(fit$thresholds, 1L)           # single gamma
})

test_that("bptr() 3-regime BTPD returns 4 threshold values", {
  fit <- bptr(y ~ x1 + x2, data = df,
              id = "id", time = "time", q = "q",
              n_thresh = 2, buffer = TRUE, grid_size_3 = 8L)

  expect_s3_class(fit, "bptr")
  expect_equal(fit$n_regimes, 3L)
  expect_length(fit$thresholds, 4L)           # rL1, rU1, rL2, rU2
  expect_equal(sum(fit$n_obs_regime), fit$n_obs)
})

test_that("bptr() coefficient matrix has correct dimensions", {
  fit <- bptr(y ~ x1 + x2, data = df,
              id = "id", time = "time", q = "q",
              n_thresh = 1, buffer = TRUE)

  cm <- fit$coefficients
  expect_true(is.matrix(cm))
  expect_equal(nrow(cm), 2L)                  # x1, x2
  expect_equal(ncol(cm), 2L)                  # regime 1, regime 2
})

test_that("bptr() respects se_type argument", {
  for (se in c("HC0", "HC1", "HC2", "HC3")) {
    fit <- bptr(y ~ x1 + x2, data = df,
                id = "id", time = "time", q = "q",
                n_thresh = 1, buffer = TRUE, se_type = se)
    expect_equal(fit$se_type, se)
    expect_true(all(fit$std_errors > 0))
  }
})

# ================================================================
# 2. S3 extractors
# ================================================================

test_that("coef(), fitted(), residuals(), nobs() work correctly", {
  fit <- bptr(y ~ x1 + x2, data = df,
              id = "id", time = "time", q = "q",
              n_thresh = 1, buffer = TRUE)

  expect_identical(coef(fit), fit$coefficients)
  expect_identical(fitted(fit), fit$fitted_values)
  expect_identical(residuals(fit), fit$residuals)
  expect_identical(nobs(fit), fit$n_obs)
})

test_that("vcov() returns a square symmetric matrix", {
  fit <- bptr(y ~ x1 + x2, data = df,
              id = "id", time = "time", q = "q",
              n_thresh = 1, buffer = TRUE)

  V <- vcov(fit)
  expect_true(is.matrix(V))
  expect_equal(nrow(V), ncol(V))
  expect_equal(V, t(V))
})

test_that("predict() in-sample equals fitted values", {
  fit <- bptr(y ~ x1 + x2, data = df,
              id = "id", time = "time", q = "q",
              n_thresh = 1, buffer = TRUE)

  expect_equal(predict(fit), fit$fitted_values)
})

test_that("print() and summary() run without error", {
  fit <- bptr(y ~ x1 + x2, data = df,
              id = "id", time = "time", q = "q",
              n_thresh = 1, buffer = TRUE)

  expect_output(print(fit),   "Buffered Panel Threshold")
  expect_output(print(summary(fit)), "Buffered Panel Threshold")
})

test_that("plot() returns a ggplot object invisibly", {
  fit <- bptr(y ~ x1 + x2, data = df,
              id = "id", time = "time", q = "q",
              n_thresh = 1, buffer = TRUE)

  p <- plot(fit, which = 1)
  expect_s3_class(p, "ggplot")
})

# ================================================================
# 3. broom integration
# ================================================================

test_that("tidy() returns a tibble with expected columns", {
  fit <- bptr(y ~ x1 + x2, data = df,
              id = "id", time = "time", q = "q",
              n_thresh = 1, buffer = TRUE)

  td <- broom::tidy(fit)
  expect_s3_class(td, "tbl_df")
  expect_true(all(c("term", "estimate", "std.error",
                    "statistic", "p.value", "regime") %in% names(td)))
  expect_equal(nrow(td), 4L)                  # 2 vars × 2 regimes
})

test_that("glance() returns a one-row tibble", {
  fit <- bptr(y ~ x1 + x2, data = df,
              id = "id", time = "time", q = "q",
              n_thresh = 1, buffer = TRUE)

  gl <- broom::glance(fit)
  expect_s3_class(gl, "tbl_df")
  expect_equal(nrow(gl), 1L)
  expect_true("r.squared" %in% names(gl))
})

test_that("augment() returns correct number of rows", {
  fit <- bptr(y ~ x1 + x2, data = df,
              id = "id", time = "time", q = "q",
              n_thresh = 1, buffer = TRUE)

  aug <- broom::augment(fit)
  expect_equal(nrow(aug), nrow(df))
  expect_true(".fitted" %in% names(aug))
  expect_true(".resid"  %in% names(aug))
  expect_true(".regime" %in% names(aug))
})

test_that("threshold_tidy() returns a data frame with threshold estimates", {
  fit <- bptr(y ~ x1 + x2, data = df,
              id = "id", time = "time", q = "q",
              n_thresh = 1, buffer = TRUE)

  tt <- threshold_tidy(fit)
  expect_true(is.data.frame(tt))
  expect_true("estimate" %in% names(tt))
  expect_equal(nrow(tt), 2L)                  # rL, rU
})

# ================================================================
# 4. bptr_test() — F1,2 (fast with n_boot = 19)
# ================================================================

test_that("bptr_test() returns correct class and components", {
  t12 <- bptr_test(y ~ x1 + x2, data = df,
                   id = "id", time = "time", q = "q",
                   buffer = TRUE, n_boot = 19L, seed = 42L)

  expect_s3_class(t12, "bptr_test")
  expect_true(is.numeric(t12$stat))
  expect_true(t12$p_value >= 0 && t12$p_value <= 1)
  expect_true(t12$n_boot > 0)
  expect_length(t12$threshold, 2L)
  expect_output(print(t12), "F1,2")
})

# ================================================================
# 5. bptr_bootstrap() — residual bootstrap (fast with n_boot = 19)
# ================================================================

test_that("bptr_bootstrap() returns correct class and CI structure", {
  fit  <- bptr(y ~ x1 + x2, data = df,
               id = "id", time = "time", q = "q",
               n_thresh = 1, buffer = TRUE)
  boot <- bptr_bootstrap(fit, n_boot = 19L, seed = 42L)

  expect_s3_class(boot, "bptr_bootstrap")
  expect_true(is.matrix(boot$gamma_boot))
  expect_true(boot$n_boot > 0)
  expect_named(boot$ci_lower)
  expect_named(boot$ci_upper)
  expect_true(all(boot$ci_lower <= boot$ci_upper))
  expect_output(print(boot), "Bootstrap")
})

# ================================================================
# 6. buildBufferIndicators() and buildBufferIndicators3()
# ================================================================

test_that("buildBufferIndicators() returns only 0s and 1s", {
  q  <- c(-2, -0.5, 0, 0.5, 1, 2)
  id <- rep(1L, 6L)
  d  <- buildBufferIndicators(q, g1 = -0.3, g2 = 0.3, prev_d = NULL, id = id)
  expect_true(all(d %in% c(0, 1)))
})

test_that("buildBufferIndicators3() returns only 1L, 2L, 3L", {
  set.seed(5)
  q  <- rnorm(40)
  id <- rep(seq_len(5L), each = 8L)
  d  <- buildBufferIndicators3(q, rL1 = -0.5, rU1 = -0.1,
                                rL2 = 0.1,  rU2 = 0.5,  id = id)
  expect_true(all(d %in% c(1L, 2L, 3L)))
  expect_equal(length(d), 40L)
})

# ================================================================
# 7. panel_data dataset
# ================================================================

test_that("panel_data has correct structure", {
  data("panel_data", package = "bufferedThresholdPanel")

  expect_equal(nrow(panel_data), 1380L)
  expect_equal(ncol(panel_data), 14L)
  expect_true("rle"          %in% names(panel_data))
  expect_true("oilRentGDP"   %in% names(panel_data))
  expect_equal(names(panel_data)[5], "oilRentGDP")
  expect_equal(names(panel_data)[6], "rle")
  expect_false(anyNA(panel_data))
  expect_equal(length(unique(panel_data$country)), 92L)
  expect_equal(length(unique(panel_data$year)),    15L)
  expect_true(is.integer(panel_data$countryId))
  expect_true(is.integer(panel_data$year))
})

test_that("panel_data rle is in [0, 1]", {
  data("panel_data", package = "bufferedThresholdPanel")
  expect_true(all(panel_data$rle >= 0 & panel_data$rle <= 1))
})

# ================================================================
# 8. Input validation
# ================================================================

test_that("bptr() stops on missing id column", {
  expect_error(
    bptr(y ~ x1, data = df, id = "no_such_col",
         time = "time", q = "q"),
    regexp = "not found"
  )
})

test_that("bptr() stops on missing q column", {
  expect_error(
    bptr(y ~ x1, data = df, id = "id",
         time = "time", q = "no_such_q"),
    regexp = "not found"
  )
})
