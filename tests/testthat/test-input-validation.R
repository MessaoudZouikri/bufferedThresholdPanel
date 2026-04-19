## ============================================================
## tests/testthat/test-input-validation.R
## All error and warning paths for exported functions
## ============================================================

make_minimal <- function(N = 10, TT = 5, seed = 1L) {
  set.seed(seed)
  data.frame(
    id   = rep(seq_len(N), each = TT),
    time = rep(seq_len(TT), N),
    x1   = rnorm(N * TT),
    q    = rnorm(N * TT),
    y    = rnorm(N * TT)
  )
}

df_min <- make_minimal()

# ================================================================
# bptr() — formula argument
# ================================================================

test_that("bptr() errors when formula is a string, not a formula object", {
  expect_error(
    bptr("y ~ x1", data = df_min, id = "id", time = "time", q = "q"),
    "'formula' must be a formula"
  )
})

test_that("bptr() errors when formula is a list", {
  expect_error(
    bptr(list(y ~ x1), data = df_min, id = "id", time = "time", q = "q"),
    "'formula' must be a formula"
  )
})

# ================================================================
# bptr() — data argument
# ================================================================

test_that("bptr() errors when data is a list", {
  expect_error(
    bptr(y ~ x1, data = as.list(df_min), id = "id", time = "time", q = "q"),
    "'data' must be a data.frame"
  )
})

test_that("bptr() errors when data is a matrix", {
  expect_error(
    bptr(y ~ x1, data = as.matrix(df_min), id = "id", time = "time", q = "q"),
    "'data' must be a data.frame"
  )
})

# ================================================================
# bptr() — n_thresh argument
# ================================================================

test_that("bptr() errors when n_thresh = 0", {
  expect_error(
    bptr(y ~ x1, data = df_min, id = "id", time = "time", q = "q", n_thresh = 0),
    "'n_thresh' must be 1 or 2"
  )
})

test_that("bptr() errors when n_thresh = 3", {
  expect_error(
    bptr(y ~ x1, data = df_min, id = "id", time = "time", q = "q", n_thresh = 3),
    "'n_thresh' must be 1 or 2"
  )
})

test_that("bptr() errors when n_thresh is non-integer numeric", {
  expect_error(
    bptr(y ~ x1, data = df_min, id = "id", time = "time", q = "q", n_thresh = 1.5),
    "'n_thresh' must be 1 or 2"
  )
})

# ================================================================
# bptr() — trim argument
# ================================================================

test_that("bptr() errors when trim = 0 (boundary)", {
  expect_error(
    bptr(y ~ x1, data = df_min, id = "id", time = "time", q = "q", trim = 0),
    "'trim' must be between 0 and 0.5"
  )
})

test_that("bptr() errors when trim = 0.5 (boundary)", {
  expect_error(
    bptr(y ~ x1, data = df_min, id = "id", time = "time", q = "q", trim = 0.5),
    "'trim' must be between 0 and 0.5"
  )
})

test_that("bptr() errors when trim is negative", {
  expect_error(
    bptr(y ~ x1, data = df_min, id = "id", time = "time", q = "q", trim = -0.1),
    "'trim' must be between 0 and 0.5"
  )
})

test_that("bptr() errors when trim > 0.5", {
  expect_error(
    bptr(y ~ x1, data = df_min, id = "id", time = "time", q = "q", trim = 0.6),
    "'trim' must be between 0 and 0.5"
  )
})

# ================================================================
# bptr() — column existence
# ================================================================

test_that("bptr() errors when id column is absent from data", {
  expect_error(
    bptr(y ~ x1, data = df_min, id = "not_there", time = "time", q = "q"),
    "not found"
  )
})

test_that("bptr() errors when time column is absent from data", {
  expect_error(
    bptr(y ~ x1, data = df_min, id = "id", time = "not_there", q = "q"),
    "not found"
  )
})

test_that("bptr() errors when q column is absent from data", {
  expect_error(
    bptr(y ~ x1, data = df_min, id = "id", time = "time", q = "not_there"),
    "not found in data"
  )
})

# ================================================================
# bptr() — no regressors
# ================================================================

test_that("bptr() errors when formula resolves to zero regressors after intercept removal", {
  expect_error(
    bptr(y ~ 1, data = df_min, id = "id", time = "time", q = "q"),
    "No explanatory variables"
  )
})

# ================================================================
# bptr() — 3-regime BTPD grid_size_3 too small
# ================================================================

test_that("bptr() errors when grid_size_3 < 4 for 3-regime BTPD", {
  expect_error(
    bptr(y ~ x1, data = df_min, id = "id", time = "time", q = "q",
         n_thresh = 2L, buffer = TRUE, grid_size_3 = 3L),
    "grid_size_3 must be >= 4"
  )
})

# ================================================================
# bptr_test_23() — input validation
# ================================================================

test_that("bptr_test_23() errors when argument is not a bptr object", {
  expect_error(bptr_test_23(list(n_thresh = 1L)), "must be a bptr object")
})

test_that("bptr_test_23() errors when bptr object has n_thresh = 2", {
  fit3 <- bptr(y ~ x1, data = df_min, id = "id", time = "time", q = "q",
               n_thresh = 2L, buffer = FALSE, grid_size = 10L, grid_size_3 = 4L)
  expect_error(bptr_test_23(fit3), "must be a bptr object with n_thresh = 1")
})

# ================================================================
# bptr_bootstrap() — input validation
# ================================================================

test_that("bptr_bootstrap() errors when x is not a bptr object", {
  expect_error(bptr_bootstrap(list(n_thresh = 1L)), "must be of class 'bptr'")
})

test_that("bptr_bootstrap() errors when x is a plain list", {
  expect_error(bptr_bootstrap(list()), "must be of class 'bptr'")
})

# ================================================================
# predict.bptr() — newdata validation
# ================================================================

test_that("predict.bptr() errors when threshold variable is absent from newdata", {
  fit <- bptr(y ~ x1, data = df_min, id = "id", time = "time", q = "q",
              n_thresh = 1L, buffer = FALSE, grid_size = 20L)
  bad <- df_min[, setdiff(names(df_min), "q")]
  expect_error(predict(fit, newdata = bad), "not found in newdata")
})

# ================================================================
# validatePanel() — input validation
# ================================================================

test_that("validatePanel() errors when id column is absent", {
  expect_error(validatePanel(df_min, "no_id", "time"), "not found in data")
})

test_that("validatePanel() errors when time column is absent", {
  expect_error(validatePanel(df_min, "id", "no_time"), "not found in data")
})

test_that("validatePanel() warns when data contains NA values", {
  df_na       <- df_min
  df_na$x1[1] <- NA_real_
  expect_warning(validatePanel(df_na, "id", "time"), "Missing values")
})

test_that("validatePanel() returns correct counts for a balanced panel", {
  info <- validatePanel(df_min, "id", "time")
  expect_equal(info$n_units,   10L)
  expect_equal(info$n_periods,  5L)
  expect_equal(info$n_obs,     50L)
  expect_true(info$balanced)
  expect_false(info$has_missing)
})

test_that("validatePanel() detects unbalanced panel (balanced = FALSE)", {
  df_unbal <- data.frame(
    id   = c(1L, 1L, 1L, 2L, 2L),
    time = c(1L, 2L, 3L, 1L, 2L)
  )
  info <- validatePanel(df_unbal, "id", "time")
  expect_false(info$balanced)
  expect_equal(info$n_units, 2L)
  expect_equal(info$n_obs,   5L)
})

# ================================================================
# robustSE() — edge cases
# ================================================================

test_that("robustSE() returns 0 when X or resid is empty", {
  X0 <- matrix(numeric(0L), nrow = 0L, ncol = 1L)
  r0 <- numeric(0L)
  se <- robustSE(X0, r0)
  expect_equal(length(se), 1L)
  expect_equal(se, 0)
})

test_that("robustSE() returns Inf when n <= k (underdetermined system)", {
  X <- matrix(c(1, 2, 3, 4), nrow = 2L, ncol = 2L)
  r <- c(0.1, -0.1)
  se <- robustSE(X, r)
  expect_true(all(is.infinite(se)))
})

# ================================================================
# augment.bptr() — data mismatch warning
# ================================================================

test_that("augment.bptr() warns when provided data has wrong row count", {
  fit <- bptr(y ~ x1, data = df_min, id = "id", time = "time", q = "q",
              n_thresh = 1L, buffer = FALSE, grid_size = 20L)
  expect_warning(broom::augment(fit, data = df_min[1:10, ]), "rows")
})

# ================================================================
# bptr_shiny() — graceful error when shiny is absent
# ================================================================

test_that("bptr_shiny() errors informatively when shiny is not installed", {
  skip_if(requireNamespace("shiny", quietly = TRUE), "shiny is installed")
  expect_error(bptr_shiny(), "shiny")
})

# ================================================================
# bptr_kable() — graceful error when knitr is absent
# ================================================================

test_that("bptr_kable() errors informatively when knitr is not installed", {
  skip_if(requireNamespace("knitr", quietly = TRUE), "knitr is installed")
  fit <- bptr(y ~ x1, data = df_min, id = "id", time = "time", q = "q",
              n_thresh = 1L, buffer = FALSE, grid_size = 20L)
  expect_error(bptr_kable(fit), "knitr")
})
