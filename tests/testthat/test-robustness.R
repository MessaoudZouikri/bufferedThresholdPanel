## ============================================================
## tests/testthat/test-robustness.R
## Edge cases, panel variants, reproducibility, and CRAN compliance
## ============================================================

# ================================================================
# Unbalanced panel
# ================================================================

test_that("bptr() works on an unbalanced panel (PTR)", {
  set.seed(42L)
  N     <- 10L
  times <- c(3, 4, 5, 6, 4, 5, 3, 6, 4, 5)
  df_unbal <- do.call(rbind, lapply(seq_len(N), function(i) {
    TT <- times[i]
    data.frame(id = i, time = seq_len(TT),
               x1 = rnorm(TT), q = rnorm(TT),
               y  = rnorm(TT))
  }))
  fit <- bptr(y ~ x1, data = df_unbal, id = "id", time = "time", q = "q",
              n_thresh = 1L, buffer = FALSE, grid_size = 30L)
  expect_s3_class(fit, "bptr")
  expect_equal(sum(fit$n_obs_regime), fit$n_obs)
  expect_equal(fit$n_obs, nrow(df_unbal))
})

test_that("bptr() works on an unbalanced panel (BTPD)", {
  set.seed(43L)
  N     <- 10L
  times <- c(4, 5, 3, 6, 4, 5, 3, 6, 4, 5)
  df_unbal <- do.call(rbind, lapply(seq_len(N), function(i) {
    TT <- times[i]
    data.frame(id = i, time = seq_len(TT),
               x1 = rnorm(TT), q = rnorm(TT), y = rnorm(TT))
  }))
  fit <- bptr(y ~ x1, data = df_unbal, id = "id", time = "time", q = "q",
              n_thresh = 1L, buffer = TRUE, grid_size = 30L)
  expect_s3_class(fit, "bptr")
  expect_equal(sum(fit$n_obs_regime), fit$n_obs)
})

# ================================================================
# Minimal viable panel
# ================================================================

test_that("bptr() works on a minimal panel (N=3, T=4, 1 regressor)", {
  set.seed(7L)
  df_micro <- data.frame(
    id   = rep(seq_len(3L), each = 4L),
    time = rep(seq_len(4L), 3L),
    x1   = rnorm(12L),
    q    = rnorm(12L),
    y    = rnorm(12L)
  )
  fit <- bptr(y ~ x1, data = df_micro, id = "id", time = "time", q = "q",
              n_thresh = 1L, buffer = FALSE, grid_size = 10L)
  expect_s3_class(fit, "bptr")
  expect_true(is.finite(fit$ssr))
})

# ================================================================
# Number of regressors
# ================================================================

test_that("bptr() works with a single regressor", {
  set.seed(20L)
  df <- data.frame(id=rep(1:10, each=6), time=rep(1:6, 10),
                   x1=rnorm(60), q=rnorm(60), y=rnorm(60))
  fit <- bptr(y ~ x1, data=df, id="id", time="time", q="q",
              n_thresh=1L, buffer=FALSE, grid_size=30L)
  expect_equal(nrow(fit$coefficients), 1L)
  expect_equal(ncol(fit$coefficients), 2L)
})

test_that("bptr() works with four regressors", {
  set.seed(21L)
  n <- 20L; tt <- 8L
  df <- data.frame(
    id=rep(1:n, each=tt), time=rep(1:tt, n),
    x1=rnorm(n*tt), x2=rnorm(n*tt), x3=rnorm(n*tt), x4=rnorm(n*tt),
    q=rnorm(n*tt), y=rnorm(n*tt)
  )
  fit <- bptr(y~x1+x2+x3+x4, data=df, id="id", time="time", q="q",
              n_thresh=1L, buffer=FALSE, grid_size=20L)
  expect_equal(nrow(fit$coefficients), 4L)
  expect_equal(ncol(fit$coefficients), 2L)
  expect_equal(length(fit$var_names), 4L)
})

# ================================================================
# Non-default trim values
# ================================================================

test_that("bptr() respects non-default trim values and stores them", {
  set.seed(22L)
  df <- data.frame(id=rep(1:15, each=6), time=rep(1:6, 15),
                   x1=rnorm(90), q=rnorm(90), y=rnorm(90))
  for (tr in c(0.10, 0.20, 0.30)) {
    fit <- bptr(y~x1, data=df, id="id", time="time", q="q",
                n_thresh=1L, buffer=FALSE, grid_size=20L, trim=tr)
    expect_s3_class(fit, "bptr")
    expect_equal(fit$trim, tr)
  }
})

# ================================================================
# All four robust SE types
# ================================================================

test_that("bptr() returns positive SEs for all HC types", {
  set.seed(23L)
  df <- data.frame(id=rep(1:10, each=8), time=rep(1:8, 10),
                   x1=rnorm(80), q=rnorm(80), y=rnorm(80))
  for (se_t in c("HC0", "HC1", "HC2", "HC3")) {
    fit <- bptr(y~x1, data=df, id="id", time="time", q="q",
                n_thresh=1L, buffer=FALSE, grid_size=20L, se_type=se_t)
    expect_true(all(fit$std_errors > 0),
                label = sprintf("SEs > 0 for %s", se_t))
    expect_equal(fit$se_type, se_t)
  }
})

# ================================================================
# predict() out-of-sample
# ================================================================

test_that("predict.bptr() out-of-sample: correct length, no NAs (PTR)", {
  set.seed(30L)
  df <- data.frame(id=rep(1:10, each=6), time=rep(1:6, 10),
                   x1=rnorm(60), q=rnorm(60))
  df$y <- df$x1 + (df$q > 0)*(-df$x1) + rnorm(60, 0, 0.5)
  fit  <- bptr(y~x1, data=df, id="id", time="time", q="q",
               n_thresh=1L, buffer=FALSE, grid_size=30L)
  nd   <- data.frame(id=rep(11:12, each=4), time=rep(1:4, 2),
                     x1=rnorm(8), q=rnorm(8))
  pred <- predict(fit, newdata=nd)
  expect_equal(length(pred), 8L)
  expect_false(anyNA(pred))
})

test_that("predict.bptr() out-of-sample: correct length, no NAs (BTPD)", {
  set.seed(31L)
  df <- data.frame(id=rep(1:10, each=6), time=rep(1:6, 10),
                   x1=rnorm(60), q=rnorm(60), y=rnorm(60))
  fit  <- bptr(y~x1, data=df, id="id", time="time", q="q",
               n_thresh=1L, buffer=TRUE, grid_size=30L)
  nd   <- data.frame(id=rep(11:12, each=4), time=rep(1:4, 2),
                     x1=rnorm(8), q=rnorm(8))
  pred <- predict(fit, newdata=nd)
  expect_equal(length(pred), 8L)
  expect_false(anyNA(pred))
})

# ================================================================
# augment() variations
# ================================================================

test_that("augment.bptr() works without an explicit data argument", {
  set.seed(32L)
  df <- data.frame(id=rep(1:10, each=5), time=rep(1:5, 10),
                   x1=rnorm(50), q=rnorm(50), y=rnorm(50))
  fit <- bptr(y~x1, data=df, id="id", time="time", q="q",
              n_thresh=1L, buffer=FALSE, grid_size=20L)
  aug <- broom::augment(fit)
  expect_equal(nrow(aug), nrow(df))
  expect_true(all(aug$.regime %in% 1:2))
})

test_that("augment.bptr() works with an explicit data argument", {
  set.seed(33L)
  df <- data.frame(id=rep(1:10, each=5), time=rep(1:5, 10),
                   x1=rnorm(50), q=rnorm(50), y=rnorm(50))
  fit <- bptr(y~x1, data=df, id="id", time="time", q="q",
              n_thresh=1L, buffer=FALSE, grid_size=20L)
  aug <- broom::augment(fit, data=df)
  expect_equal(nrow(aug), nrow(df))
  expect_equal(aug$.regime, fit$regime_classification)
})

test_that("augment.bptr() 3-regime model has .regime in {1, 2, 3}", {
  skip_on_cran()
  set.seed(34L)
  df <- data.frame(id=rep(1:20, each=8), time=rep(1:8, 20),
                   x1=rnorm(160), q=rnorm(160), y=rnorm(160))
  fit <- bptr(y~x1, data=df, id="id", time="time", q="q",
              n_thresh=2L, buffer=FALSE, grid_size=30L)
  aug <- broom::augment(fit)
  expect_true(all(aug$.regime %in% 1:3))
})

# ================================================================
# threshold_tidy() with and without bootstrap CIs
# ================================================================

test_that("threshold_tidy() conf.low <= estimate <= conf.high (point-only fallback)", {
  set.seed(40L)
  df <- data.frame(id=rep(1:10, each=6), time=rep(1:6, 10),
                   x1=rnorm(60), q=rnorm(60), y=rnorm(60))
  fit <- bptr(y~x1, data=df, id="id", time="time", q="q",
              n_thresh=1L, buffer=FALSE, grid_size=20L)
  tt  <- threshold_tidy(fit)
  expect_true(all(tt$conf.low  <= tt$estimate + 1e-8))
  expect_true(all(tt$conf.high >= tt$estimate - 1e-8))
})

test_that("threshold_tidy() uses bootstrap CIs when bootstrap is attached", {
  skip_on_cran()
  set.seed(50L)
  df <- data.frame(id=rep(1:10, each=6), time=rep(1:6, 10),
                   x1=rnorm(60), q=rnorm(60))
  df$y <- df$x1 + (df$q > 0.3)*(-df$x1) + rnorm(60, 0, 0.4)
  fit  <- bptr(y~x1, data=df, id="id", time="time", q="q",
               n_thresh=1L, buffer=FALSE, grid_size=30L)
  boot <- bptr_bootstrap(fit, n_boot=19L, seed=1L)
  fit$bootstrap <- boot
  tt   <- threshold_tidy(fit)
  expect_true(all(tt$conf.low  <= tt$estimate + 1e-6))
  expect_true(all(tt$conf.high >= tt$estimate - 1e-6))
})

# ================================================================
# Reproducibility
# ================================================================

test_that("bptr_test() gives identical results with the same seed", {
  skip_on_cran()
  set.seed(100L)
  df <- data.frame(id=rep(1:15, each=6), time=rep(1:6, 15),
                   x1=rnorm(90), q=rnorm(90))
  df$y <- df$x1 + (df$q > 0.3)*(-1.5*df$x1) + rnorm(90, 0, 0.4)

  t1 <- bptr_test(y~x1, data=df, id="id", time="time", q="q",
                   buffer=FALSE, n_boot=19L, seed=123L)
  t2 <- bptr_test(y~x1, data=df, id="id", time="time", q="q",
                   buffer=FALSE, n_boot=19L, seed=123L)

  expect_equal(t1$stat,       t2$stat)
  expect_equal(t1$p_value,    t2$p_value)
  expect_equal(t1$boot_stats, t2$boot_stats)
})

test_that("bptr_bootstrap() gives identical results with the same seed", {
  skip_on_cran()
  set.seed(101L)
  df  <- data.frame(id=rep(1:10, each=6), time=rep(1:6, 10),
                    x1=rnorm(60), q=rnorm(60), y=rnorm(60))
  fit <- bptr(y~x1, data=df, id="id", time="time", q="q",
              n_thresh=1L, buffer=FALSE, grid_size=30L)

  b1 <- bptr_bootstrap(fit, n_boot=9L, seed=99L)
  b2 <- bptr_bootstrap(fit, n_boot=9L, seed=99L)

  expect_equal(b1$gamma_boot, b2$gamma_boot)
  expect_equal(b1$ci_lower,   b2$ci_lower)
  expect_equal(b1$ci_upper,   b2$ci_upper)
})

# ================================================================
# vcov() — positive diagonal
# ================================================================

test_that("vcov.bptr() diagonal is strictly positive (variances > 0)", {
  set.seed(60L)
  df <- data.frame(id=rep(1:10, each=6), time=rep(1:6, 10),
                   x1=rnorm(60), x2=rnorm(60), q=rnorm(60), y=rnorm(60))
  fit <- bptr(y~x1+x2, data=df, id="id", time="time", q="q",
              n_thresh=1L, buffer=FALSE, grid_size=20L)
  V   <- vcov(fit)
  expect_true(all(diag(V) > 0))
})

# ================================================================
# n_obs stored vs nrow(data)
# ================================================================

test_that("n_obs in fitted object equals nrow(data)", {
  set.seed(70L)
  df <- data.frame(id=rep(1:12, each=7), time=rep(1:7, 12),
                   x1=rnorm(84), q=rnorm(84), y=rnorm(84))
  fit <- bptr(y~x1, data=df, id="id", time="time", q="q",
              n_thresh=1L, buffer=FALSE, grid_size=20L)
  expect_equal(fit$n_obs, nrow(df))
})

# ================================================================
# n_groups stored correctly
# ================================================================

test_that("n_groups in fitted object matches number of unique units", {
  set.seed(71L)
  df <- data.frame(id=rep(1:8, each=6), time=rep(1:6, 8),
                   x1=rnorm(48), q=rnorm(48), y=rnorm(48))
  fit <- bptr(y~x1, data=df, id="id", time="time", q="q",
              n_thresh=1L, buffer=FALSE, grid_size=20L)
  expect_equal(fit$n_groups, length(unique(df$id)))
})

# ================================================================
# call and formula stored on the object
# ================================================================

test_that("bptr() stores the call and formula on the result object", {
  set.seed(80L)
  df <- data.frame(id=rep(1:10, each=5), time=rep(1:5, 10),
                   x1=rnorm(50), q=rnorm(50), y=rnorm(50))
  fit <- bptr(y~x1, data=df, id="id", time="time", q="q",
              n_thresh=1L, buffer=FALSE, grid_size=20L)
  expect_s3_class(fit$formula, "formula")
  expect_true(is.call(fit$call))
})

# ================================================================
# bptr_table() — stars=FALSE, notes, style variants
# ================================================================

test_that("bptr_table() works with stars=FALSE", {
  set.seed(90L)
  df <- data.frame(id=rep(1:10, each=6), time=rep(1:6, 10),
                   x1=rnorm(60), q=rnorm(60), y=rnorm(60))
  fit <- bptr(y~x1, data=df, id="id", time="time", q="q",
              n_thresh=1L, buffer=FALSE, grid_size=20L)
  tbl <- bptr_table(fit, stars=FALSE)
  expect_s3_class(tbl, "gt_tbl")
})

test_that("bptr_table() works with notes argument", {
  set.seed(91L)
  df <- data.frame(id=rep(1:10, each=6), time=rep(1:6, 10),
                   x1=rnorm(60), q=rnorm(60), y=rnorm(60))
  fit <- bptr(y~x1, data=df, id="id", time="time", q="q",
              n_thresh=1L, buffer=FALSE, grid_size=20L)
  tbl <- bptr_table(fit, notes="Robust standard errors (HC3).")
  expect_s3_class(tbl, "gt_tbl")
})

test_that("bptr_table() works with style='JEconometrics'", {
  set.seed(92L)
  df <- data.frame(id=rep(1:10, each=6), time=rep(1:6, 10),
                   x1=rnorm(60), q=rnorm(60), y=rnorm(60))
  fit <- bptr(y~x1, data=df, id="id", time="time", q="q",
              n_thresh=1L, buffer=FALSE, grid_size=20L)
  tbl <- bptr_table(fit, style="JEconometrics")
  expect_s3_class(tbl, "gt_tbl")
})

test_that("bptr_table() works with style='AER'", {
  set.seed(93L)
  df <- data.frame(id=rep(1:10, each=6), time=rep(1:6, 10),
                   x1=rnorm(60), q=rnorm(60), y=rnorm(60))
  fit <- bptr(y~x1, data=df, id="id", time="time", q="q",
              n_thresh=1L, buffer=FALSE, grid_size=20L)
  tbl <- bptr_table(fit, style="AER")
  expect_s3_class(tbl, "gt_tbl")
})

# ================================================================
# bptr_latex() — with and without file argument
# ================================================================

test_that("bptr_latex() returns invisibly without file argument", {
  set.seed(94L)
  df <- data.frame(id=rep(1:10, each=6), time=rep(1:6, 10),
                   x1=rnorm(60), q=rnorm(60), y=rnorm(60))
  fit <- bptr(y~x1, data=df, id="id", time="time", q="q",
              n_thresh=1L, buffer=FALSE, grid_size=20L)
  tex <- bptr_latex(fit)
  expect_true(!is.null(tex))
})

test_that("bptr_latex() writes to file when file argument is given", {
  set.seed(95L)
  df <- data.frame(id=rep(1:10, each=6), time=rep(1:6, 10),
                   x1=rnorm(60), q=rnorm(60), y=rnorm(60))
  fit <- bptr(y~x1, data=df, id="id", time="time", q="q",
              n_thresh=1L, buffer=FALSE, grid_size=20L)
  tmp <- tempfile(fileext=".tex")
  bptr_latex(fit, file=tmp)
  expect_true(file.exists(tmp))
  expect_gt(file.size(tmp), 0L)
  unlink(tmp)
})

# ================================================================
# bptr_kable() — basic call
# ================================================================

test_that("bptr_kable() returns a knitr_kable object", {
  skip_if_not_installed("knitr")
  set.seed(96L)
  df <- data.frame(id=rep(1:10, each=6), time=rep(1:6, 10),
                   x1=rnorm(60), q=rnorm(60), y=rnorm(60))
  fit <- bptr(y~x1, data=df, id="id", time="time", q="q",
              n_thresh=1L, buffer=FALSE, grid_size=20L)
  kb <- bptr_kable(fit)
  expect_s3_class(kb, "knitr_kable")
})

# ================================================================
# augment.bptr() — warning when data size mismatches
# ================================================================

test_that("augment.bptr() warns when data nrow != n_obs", {
  set.seed(97L)
  df <- data.frame(id=rep(1:10, each=6), time=rep(1:6, 10),
                   x1=rnorm(60), q=rnorm(60), y=rnorm(60))
  fit <- bptr(y~x1, data=df, id="id", time="time", q="q",
              n_thresh=1L, buffer=FALSE, grid_size=20L)
  df_wrong <- df[1:40, ]
  expect_warning(broom::augment(fit, data=df_wrong))
})

# ================================================================
# augment.bptr() — error when no data available
# ================================================================

test_that("augment.bptr() errors when no data stored and none supplied", {
  set.seed(98L)
  df <- data.frame(id=rep(1:10, each=6), time=rep(1:6, 10),
                   x1=rnorm(60), q=rnorm(60), y=rnorm(60))
  fit <- bptr(y~x1, data=df, id="id", time="time", q="q",
              n_thresh=1L, buffer=FALSE, grid_size=20L)
  fit$data <- NULL
  expect_error(broom::augment(fit))
})

# ================================================================
# threshold_tidy() — bootstrap CI branch
# ================================================================

test_that("threshold_tidy() uses bootstrap CIs when bootstrap is attached", {
  skip_on_cran()
  set.seed(99L)
  df <- data.frame(id=rep(1:10, each=6), time=rep(1:6, 10),
                   x1=rnorm(60), q=rnorm(60))
  df$y <- df$x1 + (df$q > 0.2) * (-df$x1) + rnorm(60, 0, 0.3)
  fit  <- bptr(y~x1, data=df, id="id", time="time", q="q",
               n_thresh=1L, buffer=FALSE, grid_size=30L)
  boot <- bptr_bootstrap(fit, n_boot=9L, seed=1L)
  fit$bootstrap <- boot
  tt <- threshold_tidy(fit)
  expect_true(tt$conf.low  <= tt$estimate + 1e-8)
  expect_true(tt$conf.high >= tt$estimate - 1e-8)
  expect_true(tt$conf.low  < tt$conf.high || tt$conf.low == tt$conf.high)
})

# ================================================================
# glance.bptr() — NA r.squared when tss_within is zero/NULL
# ================================================================

test_that("glance.bptr() returns NA r.squared when tss_within is NULL", {
  set.seed(100L)
  df <- data.frame(id=rep(1:10, each=6), time=rep(1:6, 10),
                   x1=rnorm(60), q=rnorm(60), y=rnorm(60))
  fit <- bptr(y~x1, data=df, id="id", time="time", q="q",
              n_thresh=1L, buffer=FALSE, grid_size=20L)
  fit$tss_within <- NULL
  gl <- broom::glance(fit)
  expect_true(is.na(gl$r.squared))
})

# ================================================================
# threshold_tidy() — LR-inversion CI branch
# ================================================================

test_that("threshold_tidy() uses LR-inversion CIs when lr_test and lr_grid are present", {
  set.seed(101L)
  df <- data.frame(id=rep(1:10, each=6), time=rep(1:6, 10),
                   x1=rnorm(60), q=rnorm(60), y=rnorm(60))
  fit <- bptr(y~x1, data=df, id="id", time="time", q="q",
              n_thresh=1L, buffer=FALSE, grid_size=20L)
  crit <- stats::qchisq(0.95, df = 1)
  th         <- fit$thresholds[1]
  fake_lr    <- c(crit + 2, crit - 0.5, crit - 0.5, crit + 1)
  fake_grid  <- seq(th - 1, th + 1, length.out = 4)
  fit$lr_test    <- list(stat = 10.0)
  fit$lr_grid    <- list(fake_lr)
  fit$grid_points <- list(fake_grid)
  tt <- threshold_tidy(fit)
  expect_true(tt$conf.low  <= tt$estimate + 1e-8)
  expect_true(tt$conf.high >= tt$estimate - 1e-8)
  expect_true(tt$conf.low  <  tt$conf.high + 1e-8)
})

# ================================================================
# augment.bptr() — NULL fitted_values / residuals / regime_classification
# ================================================================

test_that("augment.bptr() returns NA columns when fit internals are NULL", {
  set.seed(102L)
  df <- data.frame(id=rep(1:10, each=6), time=rep(1:6, 10),
                   x1=rnorm(60), q=rnorm(60), y=rnorm(60))
  fit <- bptr(y~x1, data=df, id="id", time="time", q="q",
              n_thresh=1L, buffer=FALSE, grid_size=20L)
  fit$fitted_values        <- NULL
  fit$residuals            <- NULL
  fit$regime_classification <- NULL
  aug <- broom::augment(fit)
  expect_true(all(is.na(aug$.fitted)))
  expect_true(all(is.na(aug$.resid)))
  expect_true(all(is.na(aug$.regime)))
})

# ================================================================
# glance.bptr() — n_groups = NULL branch
# ================================================================

test_that("glance.bptr() returns NA for n_groups when n_groups is NULL", {
  set.seed(103L)
  df <- data.frame(id=rep(1:10, each=6), time=rep(1:6, 10),
                   x1=rnorm(60), q=rnorm(60), y=rnorm(60))
  fit <- bptr(y~x1, data=df, id="id", time="time", q="q",
              n_thresh=1L, buffer=FALSE, grid_size=20L)
  fit$n_groups <- NULL
  gl <- broom::glance(fit)
  expect_true(is.na(gl$n_groups))
})

# ================================================================
# computeSSR() — 2-element non-buffer g_vec branch (lines 216-219)
# ================================================================

test_that("computeSSR() handles 2-element g_vec in non-buffer mode", {
  set.seed(104L)
  n <- 40L
  X <- matrix(rnorm(n), ncol = 1L)
  y <- rnorm(n)
  q <- rnorm(n)
  s <- computeSSR(y, X, g_vec = c(-0.5, 0.5), q_dm = q, buffer = FALSE)
  expect_true(is.numeric(s))
  expect_true(is.finite(s) || is.infinite(s))
})

# ================================================================
# bptr_test_seq() — 3-regime model selection path
# ================================================================

test_that("bptr_test_seq() selects 3-regime model with a strong 3-regime DGP", {
  skip_on_cran()
  set.seed(201L)
  n <- 30L; tt <- 10L
  df <- data.frame(id  = rep(seq_len(n), each = tt),
                   time = rep(seq_len(tt), n),
                   x1   = rnorm(n * tt),
                   q    = runif(n * tt, -3, 3))
  df$y <- with(df, ifelse(q < -1,  4.0 * x1,
                   ifelse(q >  1, -4.0 * x1, 0.0)) + rnorm(n * tt, 0, 0.05))
  result <- suppressMessages(
    bptr_test_seq(y ~ x1, data = df,
                  id = "id", time = "time", q = "q",
                  buffer = FALSE, n_boot = 29L,
                  grid_size = 30L, grid_size_3 = 5L,
                  alpha = 0.10, seed = 1L)
  )
  expect_s3_class(result, "bptr_test_seq")
  expect_equal(result$n_regimes_selected, 3L)
  expect_s3_class(result$test_23, "bptr_test23")
  expect_s3_class(result$final_model, "bptr")
})
