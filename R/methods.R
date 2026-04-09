# =============================================================================
# S3 methods and inference functions for bufferedThresholdPanel
# =============================================================================

# --------------------------------------------------------------------------- #
#  print.bptr                                                                  #
# --------------------------------------------------------------------------- #

#' @rdname bptr
#' @param x A \code{bptr} object (for \code{print}, \code{plot} and other S3 methods).
#' @param object A \code{bptr} object (for \code{summary}, \code{coef},
#'   \code{fitted}, \code{residuals}, \code{nobs}, \code{vcov} and \code{predict}).
#' @param digits Number of significant digits (default 4)
#' @param ... Additional arguments (currently ignored).
#' @method print bptr
#' @export
print.bptr <- function(x, digits = 4, ...) {
  cat("\n--- Buffered Panel Threshold Regression ---\n")
  cat(sprintf("Call: %s\n\n", deparse(x$call)))
  cat(sprintf("Model  : %s  |  Regimes: %d  |  SE: %s\n",
              if (x$buffer) "BTPD (buffered)" else "PTR (standard)",
              x$n_regimes, x$se_type))
  cat(sprintf("Panel  : N=%d, T=%d, NT=%d\n",
              x$n_groups, x$panel_info$n_periods, x$n_obs))
  cat(sprintf("gamma  : %s\n", paste(round(x$thresholds, digits), collapse = "  ")))
  cat(sprintf("Regime sizes: %s\n", paste(x$n_obs_regime, collapse = " / ")))
  cat(sprintf("SSR    : %.6f  |  Within R2: %.4f\n",
              x$ssr, 1 - x$ssr / x$tss_within))

  cm <- x$coefficients; sm <- x$std_errors; df_r <- x$df_residual
  t_mat <- cm / sm
  p_mat <- 2 * stats::pt(-abs(t_mat), df = df_r)

  for (r in seq_len(ncol(cm))) {
    cat(sprintf("\n  Regime %d  (n = %d):\n", r, x$n_obs_regime[r]))
    out <- data.frame(
      Estimate  = round(cm[, r], digits),
      Std.Error = round(sm[, r], digits),
      t.value   = round(t_mat[, r], digits),
      "Pr(>|t|)" = round(p_mat[, r], digits),
      check.names = FALSE)
    print(out)
  }
  invisible(x)
}

# --------------------------------------------------------------------------- #
#  summary.bptr / print.summary.bptr                                          #
# --------------------------------------------------------------------------- #

#' @rdname bptr
#' @export
summary.bptr <- function(object, ...) {
  cm <- object$coefficients; sm <- object$std_errors; df_r <- object$df_residual
  t_m <- cm / sm; p_m <- 2 * stats::pt(-abs(t_m), df = df_r)
  sf  <- function(p) ifelse(p < .01, "***", ifelse(p < .05, "**",
                            ifelse(p < .10, "*", "")))
  tbls <- lapply(seq_len(ncol(cm)), function(r) {
    data.frame(Estimate = cm[, r], Std.Error = sm[, r],
               t.value = t_m[, r], "Pr(>|t|)" = p_m[, r],
               sig = sf(p_m[, r]),
               check.names = FALSE, stringsAsFactors = FALSE)
  })
  names(tbls) <- paste0("regime", seq_along(tbls))
  structure(
    list(call = object$call,
         model_type = if (object$buffer) "BTPD" else "PTR",
         n_regimes = object$n_regimes, thresholds = object$thresholds,
         n_obs = object$n_obs, n_groups = object$n_groups,
         n_obs_regime = object$n_obs_regime, df_residual = df_r,
         ssr = object$ssr, sigma = sqrt(object$ssr / max(1, df_r)),
         r2_within = 1 - object$ssr / object$tss_within,
         coef_tables = tbls, se_type = object$se_type,
         panel_info = object$panel_info),
    class = "summary.bptr")
}

#' @rdname bptr
#' @export
print.summary.bptr <- function(x, digits = 4, ...) {
  cat("\n=== Buffered Panel Threshold Regression ===\n")
  cat(sprintf("Call: %s\n\n", deparse(x$call)))
  cat(sprintf("Model: %s | Regimes: %d | SE: %s\n",
              x$model_type, x$n_regimes, x$se_type))
  cat(sprintf("Panel: N=%d, T=%d, NT=%d\n",
              x$n_groups, x$panel_info$n_periods, x$n_obs))
  cat(sprintf("Thresholds: %s\n",
              paste(round(x$thresholds, digits), collapse = "  ")))
  cat(sprintf("Residual SE: %.4f | Within R2: %.4f | SSR: %.4f | df: %d\n\n",
              x$sigma, x$r2_within, x$ssr, x$df_residual))
  for (nm in names(x$coef_tables)) {
    r <- as.integer(sub("regime", "", nm))
    cat(sprintf("--- Regime %d  (n = %d) ---\n", r, x$n_obs_regime[r]))
    tbl <- x$coef_tables[[nm]]; tbl[, 1:4] <- round(tbl[, 1:4], digits)
    print(tbl, quote = FALSE); cat("\n")
  }
  cat("Signif: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n")
  invisible(x)
}

# --------------------------------------------------------------------------- #
#  Standard S3 extractors                                                      #
# --------------------------------------------------------------------------- #

#' @rdname bptr
#' @export
coef.bptr <- function(object, ...) object$coefficients

#' @rdname bptr
#' @export
fitted.bptr <- function(object, ...) object$fitted_values

#' @rdname bptr
#' @export
residuals.bptr <- function(object, ...) object$residuals

#' @rdname bptr
#' @importFrom stats nobs
#' @method nobs bptr
#' @export
nobs.bptr <- function(object, ...) object$n_obs

#' Variance-Covariance Matrix of a BPTR Model
#'
#' Returns a block-diagonal matrix built from the per-regime sandwich
#' standard errors. Off-diagonal blocks (cross-regime covariances) are
#' set to zero.
#'
#' @rdname bptr
#' @export
vcov.bptr <- function(object, ...) {
  sm <- object$std_errors
  v  <- diag(as.vector(sm)^2)
  rn <- as.vector(outer(rownames(sm),
                         paste0("regime", seq_len(ncol(sm))),
                         paste, sep = ":"))
  rownames(v) <- colnames(v) <- rn
  v
}

#' @rdname bptr
#' @param newdata An optional data frame for out-of-sample prediction.
#'   Must contain the threshold variable and all regressors.
#' @export
predict.bptr <- function(object, newdata = NULL, ...) {
  if (is.null(newdata)) return(object$fitted_values)
  if (!object$q_name %in% names(newdata))
    stop(paste("Threshold variable", object$q_name, "not found in newdata"))
  X <- stats::model.matrix(object$formula, newdata)
  if ("(Intercept)" %in% colnames(X)) X <- X[, -1, drop = FALSE]
  q_new <- newdata[[object$q_name]]
  gamma <- object$thresholds
  pred  <- numeric(nrow(X))
  if (object$n_regimes == 2L) {
    ind1 <- as.numeric(q_new <= gamma[1]); ind2 <- 1 - ind1
    if (sum(ind1) > 0) pred[ind1 == 1] <- X[ind1 == 1, , drop = FALSE] %*% object$beta1
    if (sum(ind2) > 0) pred[ind2 == 1] <- X[ind2 == 1, , drop = FALSE] %*% object$beta2
  } else {
    if (!object$buffer) {
      ind1 <- as.numeric(q_new <= gamma[1])
      ind2 <- as.numeric(q_new > gamma[1] & q_new <= gamma[2])
      ind3 <- as.numeric(q_new > gamma[2])
    } else {
      d    <- buildBufferIndicators3(q_new, gamma[1], gamma[2], gamma[3], gamma[4],
                                     newdata[[object$id]])
      ind1 <- as.numeric(d == 1L); ind2 <- as.numeric(d == 2L)
      ind3 <- as.numeric(d == 3L)
    }
    if (sum(ind1) > 0) pred[ind1 == 1] <- X[ind1 == 1, , drop = FALSE] %*% object$beta1
    if (sum(ind2) > 0) pred[ind2 == 1] <- X[ind2 == 1, , drop = FALSE] %*% object$beta2
    if (sum(ind3) > 0) pred[ind3 == 1] <- X[ind3 == 1, , drop = FALSE] %*% object$beta3
  }
  pred
}

#' @rdname bptr
#' @param which Integer vector: \code{1} (residuals vs fitted) and/or
#'   \code{2} (threshold variable with regime boundaries). Default \code{1:2}.
#' @importFrom ggplot2 ggplot aes geom_point geom_hline labs theme_bw
#' @export
plot.bptr <- function(x, which = 1:2, ...) {
  rc <- factor(x$regime_classification)
  last_p <- NULL
  if (1 %in% which) {
    df1 <- data.frame(fitted = x$fitted_values, resid = x$residuals, regime = rc)
    p1  <- ggplot2::ggplot(df1, ggplot2::aes(x = fitted, y = resid, colour = regime)) +
      ggplot2::geom_point(alpha = 0.4, size = 0.9) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
      ggplot2::labs(title = "Residuals vs Fitted",
                    x = "Fitted values", y = "Residuals", colour = "Regime") +
      ggplot2::theme_bw()
    print(p1); last_p <- p1
  }
  if (2 %in% which) {
    df2 <- data.frame(obs = seq_len(nrow(x$data)),
                      q   = x$data[[x$q_name]], regime = rc)
    p2  <- ggplot2::ggplot(df2, ggplot2::aes(x = obs, y = q, colour = regime)) +
      ggplot2::geom_point(alpha = 0.4, size = 0.9)
    for (thr in x$thresholds)
      p2 <- p2 + ggplot2::geom_hline(yintercept = thr,
                                      linetype = "dashed", colour = "black")
    p2 <- p2 +
      ggplot2::labs(title = "Threshold Variable with Regime Classification",
                    x = "Observation index", y = x$q_name, colour = "Regime") +
      ggplot2::theme_bw()
    print(p2); last_p <- p2
  }
  invisible(last_p)
}

# --------------------------------------------------------------------------- #
#  bptr_test: F1,2 — linearity vs 2-regime                                   #
# --------------------------------------------------------------------------- #

#' Sup-LR Test for Threshold Existence (F1,2)
#'
#' Tests the null hypothesis of a linear panel model against the alternative of
#' a two-regime buffered (or standard) threshold panel model, using the
#' bootstrap procedure of Belarbi et al. (2021), Algorithm 1.  The bootstrap
#' draws residuals from the \emph{linear} null model.
#'
#' The test statistic is
#' \deqn{F_{1,2} = \frac{S_1 - S_2(\hat\gamma_2)}{\hat\sigma^2_2(\hat\gamma_2)}}
#' where \eqn{S_1} is the within-demeaned OLS residual sum of squares and
#' \eqn{S_2} is the minimum SSR of the two-regime model over the threshold grid.
#'
#' @param formula A model formula (same syntax as \code{\link{bptr}})
#' @param data Panel data frame
#' @param id Character. Cross-sectional identifier column
#' @param time Character. Time identifier column
#' @param q Character. Threshold variable column
#' @param buffer Logical. Use buffered (BTPD) transitions? (default \code{FALSE})
#' @param n_boot Integer. Number of bootstrap replications (default 299)
#' @param trim Numeric. Trimming fraction for threshold grid (default 0.15)
#' @param grid_size Integer. Grid resolution for threshold search (default 300)
#' @param seed Integer. Random seed for reproducibility (default 42).
#'   Note: this call sets the global RNG state via \code{set.seed()}.
#' @return An object of class \code{"bptr_test"} with components:
#'   \code{stat} (observed F1,2), \code{p_value} (bootstrap p-value),
#'   \code{boot_stats} (bootstrap replications), \code{n_boot},
#'   \code{threshold} (estimated threshold), \code{model} (fitted 2-regime
#'   \code{bptr} object)
#' @references
#'   Belarbi, Y., Hamdi, F., Khalfi, A., and Souam, S. (2021).
#'   Growth, institutions and oil dependence: A buffered threshold panel approach.
#'   \emph{Economic Modelling}, 99, 105477.
#'   \doi{10.1016/j.econmod.2021.02.018}
#' @importFrom stats quantile
#' @export
#' @examples
#' \donttest{
#' set.seed(1)
#' n <- 30; tt <- 8
#' df <- data.frame(id = rep(1:n, each = tt), time = rep(1:tt, n),
#'                  x1 = rnorm(n * tt), q = rnorm(n * tt))
#' df$y <- 1.5 * df$x1 + (df$q > 0.4) * (-1.0 * df$x1) + rnorm(n * tt, 0, 0.5)
#' test12 <- bptr_test(y ~ x1, data = df, id = "id", time = "time",
#'                     q = "q", n_boot = 99)
#' print(test12)
#' }
bptr_test <- function(formula, data, id, time, q,
                      buffer = FALSE, n_boot = 299,
                      trim = 0.15, grid_size = 300, seed = 42) {
  set.seed(seed)
  validatePanel(data, id, time)

  mf <- stats::model.frame(formula, data)
  y  <- stats::model.response(mf)
  X  <- stats::model.matrix(formula, data)
  if ("(Intercept)" %in% colnames(X)) X <- X[, -1, drop = FALSE]

  fe     <- removeFE(y, X, data[[id]])
  y_dm   <- fe$y_dm; X_dm <- fe$X_dm
  b0     <- solve(crossprod(X_dm)) %*% crossprod(X_dm, y_dm)
  resid0 <- y_dm - X_dm %*% b0
  S1     <- sum(resid0^2)

  fit_h1 <- bptr(formula = formula, data = data, id = id, time = time, q = q,
                 n_thresh = 1, buffer = buffer, trim = trim, grid_size = grid_size)
  S2    <- fit_h1$ssr
  F_obs <- (S1 - S2) / (S2 / max(1, fit_h1$df_residual))

  id_vec <- data[[id]]; uniq <- unique(id_vec)
  fv0    <- as.vector(X_dm %*% b0)
  dep    <- as.character(formula[[2]])

  boot_stats <- vapply(seq_len(n_boot), function(b) {
    rs <- numeric(length(resid0))
    for (i in uniq) {
      idx <- which(id_vec == i)
      rs[idx] <- sample(resid0[idx], replace = TRUE)
    }
    db <- data; db[[dep]] <- fv0 + rs
    tryCatch({
      mf_b  <- stats::model.frame(formula, db)
      y_b   <- stats::model.response(mf_b)
      X_b   <- stats::model.matrix(formula, db)
      if ("(Intercept)" %in% colnames(X_b)) X_b <- X_b[, -1, drop = FALSE]
      fe_b  <- removeFE(y_b, X_b, db[[id]])
      b0b   <- solve(crossprod(fe_b$X_dm)) %*% crossprod(fe_b$X_dm, fe_b$y_dm)
      S1b   <- sum((fe_b$y_dm - fe_b$X_dm %*% b0b)^2)
      fb    <- bptr(formula = formula, data = db, id = id, time = time, q = q,
                    n_thresh = 1, buffer = buffer, trim = trim, grid_size = grid_size)
      (S1b - fb$ssr) / (fb$ssr / max(1, fb$df_residual))
    }, error = function(e) NA_real_)
  }, numeric(1))

  boot_stats <- boot_stats[!is.na(boot_stats)]
  result <- list(stat = F_obs, p_value = mean(boot_stats >= F_obs),
                 boot_stats = boot_stats, n_boot = length(boot_stats),
                 threshold = fit_h1$thresholds, model = fit_h1,
                 test_label = "F1,2: linearity vs 2-regime")
  class(result) <- "bptr_test"
  result
}

#' @rdname bptr_test
#' @param x A \code{bptr_test} object.
#' @param ... Additional arguments (currently ignored).
#' @method print bptr_test
#' @export
print.bptr_test <- function(x, ...) {
  cat(sprintf("\n--- %s ---\n", x$test_label))
  cat(sprintf("Sup-LR statistic : %.4f\n", x$stat))
  cat(sprintf("Bootstrap p-value: %.4f  (B = %d)\n", x$p_value, x$n_boot))
  cat(sprintf("Threshold(s)     : %s\n",
              paste(round(x$threshold, 4), collapse = "  ")))
  sig <- if (x$p_value < .01) "***" else if (x$p_value < .05) "**" else
         if (x$p_value < .10) "*" else ""
  cat(sprintf("Conclusion: %s threshold effect %s\n\n",
              if (x$p_value < .10) "Significant" else "No significant", sig))
  invisible(x)
}

# --------------------------------------------------------------------------- #
#  bptr_test_23: F2,3 — 2-regime vs 3-regime                                 #
# --------------------------------------------------------------------------- #

#' Sup-LR Test of 2-Regime Against 3-Regime BTPD (F2,3)
#'
#' Tests H0: two-regime model against H1: three-regime model using the
#' bootstrap procedure of Belarbi et al. (2021), Section 2.3.  The critical
#' distinction from \code{\link{bptr_test}} is that bootstrap residuals are
#' drawn from the \emph{two-regime} model (the alternative of the preceding
#' F1,2 test, which becomes the null here), not from the linear model.
#'
#' The test statistic is
#' \deqn{F_{2,3} = \frac{S_2(\hat\gamma_2) - S_3(\hat\gamma_3)}{\hat\sigma^2_3(\hat\gamma_3)}}
#'
#' @param fit_2reg A fitted \code{bptr} object with \code{n_thresh = 1}
#'   representing the two-regime model (H0 of this test)
#' @param n_boot Integer. Number of bootstrap replications (default 299)
#' @param grid_size_3 Integer. Grid resolution for the 3-regime BTPD 4-D
#'   search (default 50)
#' @param seed Integer. Random seed (default 42).
#'   Note: sets global RNG state via \code{set.seed()}.
#' @param workers Unused; kept for API consistency with \code{bptr_bootstrap}
#' @return An object of class \code{"bptr_test23"} with components:
#'   \code{stat}, \code{p_value}, \code{boot_stats}, \code{n_boot},
#'   \code{thresholds_2}, \code{thresholds_3}, \code{model_2reg},
#'   \code{model_3reg}
#' @references
#'   Belarbi, Y., Hamdi, F., Khalfi, A., and Souam, S. (2021).
#'   Growth, institutions and oil dependence: A buffered threshold panel approach.
#'   \emph{Economic Modelling}, 99, 105477.
#'   \doi{10.1016/j.econmod.2021.02.018}
#' @export
#' @examples
#' \donttest{
#' set.seed(2)
#' n <- 30; tt <- 8
#' df <- data.frame(id = rep(1:n, each = tt), time = rep(1:tt, n),
#'                  x1 = rnorm(n * tt), q = rnorm(n * tt))
#' df$y <- 1.5 * df$x1 + (df$q < -0.5) * 0.8 * df$x1 +
#'         (df$q > 0.5) * (-1.2 * df$x1) + rnorm(n * tt, 0, 0.5)
#' fit2 <- bptr(y ~ x1, data = df, id = "id", time = "time",
#'              q = "q", n_thresh = 1, buffer = TRUE)
#' test23 <- bptr_test_23(fit2, n_boot = 49, grid_size_3 = 20)
#' print(test23)
#' }
bptr_test_23 <- function(fit_2reg, n_boot = 299, grid_size_3 = 50L,
                          seed = 42L, workers = NULL) {
  if (!inherits(fit_2reg, "bptr") || fit_2reg$n_thresh != 1L)
    stop("'fit_2reg' must be a bptr object with n_thresh = 1")
  set.seed(seed)

  fit_3reg <- bptr(
    formula = fit_2reg$formula, data = fit_2reg$data,
    id = fit_2reg$id, time = fit_2reg$time, q = fit_2reg$q_name,
    n_thresh = 2L, buffer = fit_2reg$buffer, trim = fit_2reg$trim,
    grid_size_3 = grid_size_3, se_type = fit_2reg$se_type)

  S2     <- fit_2reg$ssr; S3 <- fit_3reg$ssr
  F_obs  <- (S2 - S3) / (S3 / max(1, fit_3reg$df_residual))

  # Bootstrap under H0 = 2-regime model (Belarbi 2021, Section 2.3)
  fv2    <- fit_2reg$fitted_values
  rs2    <- fit_2reg$residuals
  id_vec <- fit_2reg$data[[fit_2reg$id]]
  uniq   <- unique(id_vec)
  dep    <- as.character(fit_2reg$formula[[2]])

  boot_stats <- vapply(seq_len(n_boot), function(b) {
    rs <- numeric(length(rs2))
    for (i in uniq) {
      idx <- which(id_vec == i)
      rs[idx] <- sample(rs2[idx], replace = TRUE)
    }
    db <- fit_2reg$data; db[[dep]] <- fv2 + rs
    tryCatch({
      fb2 <- bptr(formula = fit_2reg$formula, data = db,
                  id = fit_2reg$id, time = fit_2reg$time, q = fit_2reg$q_name,
                  n_thresh = 1L, buffer = fit_2reg$buffer, trim = fit_2reg$trim,
                  grid_size = 300L, se_type = fit_2reg$se_type)
      fb3 <- bptr(formula = fit_2reg$formula, data = db,
                  id = fit_2reg$id, time = fit_2reg$time, q = fit_2reg$q_name,
                  n_thresh = 2L, buffer = fit_2reg$buffer, trim = fit_2reg$trim,
                  grid_size_3 = grid_size_3, se_type = fit_2reg$se_type)
      (fb2$ssr - fb3$ssr) / (fb3$ssr / max(1, fb3$df_residual))
    }, error = function(e) NA_real_)
  }, numeric(1))

  boot_stats <- boot_stats[!is.na(boot_stats)]
  n_ok <- length(boot_stats)
  if (n_ok == 0) stop("All bootstrap replications failed.")
  if (n_ok < n_boot)
    warning(sprintf("%d of %d bootstrap replications failed.",
                    n_boot - n_ok, n_boot))

  result <- list(
    stat         = F_obs,
    p_value      = mean(boot_stats >= F_obs),
    boot_stats   = boot_stats,
    n_boot       = n_ok,
    thresholds_2 = fit_2reg$thresholds,
    thresholds_3 = fit_3reg$thresholds,
    model_2reg   = fit_2reg,
    model_3reg   = fit_3reg,
    test_label   = "F2,3: 2-regime vs 3-regime"
  )
  class(result) <- "bptr_test23"
  result
}

#' @rdname bptr_test_23
#' @param x A \code{bptr_test23} object.
#' @param ... Additional arguments (currently ignored).
#' @method print bptr_test23
#' @export
print.bptr_test23 <- function(x, ...) {
  cat(sprintf("\n--- %s ---\n", x$test_label))
  cat(sprintf("H0 thresholds (2-regime): %s\n",
              paste(round(x$thresholds_2, 4), collapse = "  ")))
  cat(sprintf("H1 thresholds (3-regime): %s\n",
              paste(round(x$thresholds_3, 4), collapse = "  ")))
  cat(sprintf("Sup-LR statistic : %.4f\n", x$stat))
  cat(sprintf("Bootstrap p-value: %.4f  (B = %d)\n", x$p_value, x$n_boot))
  sig <- if (x$p_value < .01) "***" else if (x$p_value < .05) "**" else
         if (x$p_value < .10) "*" else ""
  cat(sprintf("Conclusion: %s evidence for 3-regime model %s\n\n",
              if (x$p_value < .10) "Significant" else "No significant", sig))
  invisible(x)
}

# --------------------------------------------------------------------------- #
#  bptr_test_seq: full sequential test F1,2 → F2,3                           #
# --------------------------------------------------------------------------- #

#' Sequential Regime Test for BTPD/PTR Models
#'
#' Implements the complete sequential testing strategy of Belarbi et al. (2021),
#' Section 2.3:
#' \enumerate{
#'   \item Test linearity vs two-regime model (F1,2); bootstrap under linear H0.
#'   \item If F1,2 is significant at \code{alpha}, test two-regime vs
#'     three-regime model (F2,3); bootstrap under two-regime H0.
#'   \item The number of regimes is selected based on the sequential p-values.
#' }
#' Progress is reported via \code{\link{message}} so it can be suppressed with
#' \code{suppressMessages()}.  The final model (the one selected by the
#' sequential procedure) is returned as \code{$final_model}.
#'
#' @param formula A model formula
#' @param data Panel data frame
#' @param id Character. Cross-sectional identifier column
#' @param time Character. Time identifier column
#' @param q Character. Threshold variable column
#' @param buffer Logical. Use buffered (BTPD) model? (default \code{FALSE})
#' @param n_boot Integer. Bootstrap replications for each test (default 299)
#' @param trim Numeric. Trimming fraction (default 0.15)
#' @param grid_size Integer. Grid resolution for the 2-regime search
#'   (default 300)
#' @param grid_size_3 Integer. Grid resolution for the 3-regime BTPD 4-D
#'   search (default 50)
#' @param alpha Numeric. Significance level for the sequential decision rule
#'   (default 0.10)
#' @param seed Integer. Random seed (default 42)
#' @return An object of class \code{"bptr_test_seq"} with components:
#'   \code{test_12} (F1,2 result), \code{test_23} (\code{NULL} if F1,2 not
#'   rejected), \code{final_model} (selected \code{bptr} object),
#'   \code{n_regimes_selected}, \code{alpha}
#' @references
#'   Belarbi, Y., Hamdi, F., Khalfi, A., and Souam, S. (2021).
#'   Growth, institutions and oil dependence: A buffered threshold panel approach.
#'   \emph{Economic Modelling}, 99, 105477.
#'   \doi{10.1016/j.econmod.2021.02.018}
#' @export
#' @examples
#' \donttest{
#' set.seed(3)
#' n <- 30; tt <- 8
#' df <- data.frame(id = rep(1:n, each = tt), time = rep(1:tt, n),
#'                  x1 = rnorm(n * tt), q = rnorm(n * tt))
#' df$y <- 1.5 * df$x1 + (df$q < -0.5) * 0.8 * df$x1 +
#'         (df$q > 0.5) * (-1.2 * df$x1) + rnorm(n * tt, 0, 0.5)
#' result <- bptr_test_seq(y ~ x1, data = df, id = "id", time = "time",
#'                         q = "q", buffer = TRUE,
#'                         n_boot = 49, grid_size_3 = 20)
#' print(result)
#' }
bptr_test_seq <- function(formula, data, id, time, q,
                           buffer      = FALSE,
                           n_boot      = 299,
                           trim        = 0.15,
                           grid_size   = 300,
                           grid_size_3 = 50L,
                           alpha       = 0.10,
                           seed        = 42) {

  message("Step 1: Testing linearity vs 2-regime model (F1,2)...")
  test_12 <- bptr_test(formula = formula, data = data, id = id, time = time,
                        q = q, buffer = buffer, n_boot = n_boot, trim = trim,
                        grid_size = grid_size, seed = seed)

  test_23 <- NULL; n_reg_sel <- 1L; final_model <- test_12$model

  if (test_12$p_value >= alpha) {
    message(sprintf("  F1,2 p = %.4f >= %.2f: linearity not rejected. Final: linear model.",
                    test_12$p_value, alpha))
  } else {
    message(sprintf("  F1,2 p = %.4f < %.2f: linearity rejected.",
                    test_12$p_value, alpha))
    message("Step 2: Testing 2-regime vs 3-regime model (F2,3)...")

    test_23 <- bptr_test_23(fit_2reg = test_12$model, n_boot = n_boot,
                             grid_size_3 = grid_size_3, seed = seed)

    if (test_23$p_value >= alpha) {
      message(sprintf("  F2,3 p = %.4f >= %.2f: 2-regime model selected.",
                      test_23$p_value, alpha))
      final_model <- test_12$model; n_reg_sel <- 2L
    } else {
      message(sprintf("  F2,3 p = %.4f < %.2f: 3-regime model selected.",
                      test_23$p_value, alpha))
      final_model <- test_23$model_3reg; n_reg_sel <- 3L
    }
  }

  result <- list(test_12 = test_12, test_23 = test_23,
                 final_model = final_model,
                 n_regimes_selected = n_reg_sel, alpha = alpha)
  class(result) <- "bptr_test_seq"
  result
}

#' @rdname bptr_test_seq
#' @param x A \code{bptr_test_seq} object.
#' @param ... Additional arguments (currently ignored).
#' @method print bptr_test_seq
#' @export
print.bptr_test_seq <- function(x, ...) {
  cat("\n=== Sequential Regime Test (Belarbi et al. 2021) ===\n\n")
  print(x$test_12)
  if (!is.null(x$test_23)) print(x$test_23)
  cat(sprintf("==> Selected model: %d regime(s)  (alpha = %.2f)\n\n",
              x$n_regimes_selected, x$alpha))
  if (!is.null(x$final_model)) { cat("Final model:\n"); print(x$final_model) }
  invisible(x)
}

# --------------------------------------------------------------------------- #
#  bptr_bootstrap                                                              #
# --------------------------------------------------------------------------- #

#' Residual Bootstrap for 2-Regime BPTR Models
#'
#' Draws bootstrap confidence intervals for the threshold parameter and
#' regime-specific slope coefficients by resampling within-unit residuals
#' from the fitted two-regime model (Hansen 1999; Belarbi et al. 2021).
#'
#' @param x A fitted \code{bptr} object with \code{n_thresh = 1}
#' @param n_boot Integer. Bootstrap replications (default 299)
#' @param workers Integer or \code{NULL}. Parallel workers for
#'   \code{future}/\code{furrr} (default: \code{availableCores() - 1})
#' @param seed Integer. Random seed (default 42).
#'   Note: sets global RNG state via \code{set.seed()}.
#' @return An object of class \code{"bptr_bootstrap"} with components:
#'   \code{gamma_boot}, \code{beta1_boot}, \code{beta2_boot},
#'   \code{ci_lower}, \code{ci_upper} (95\% percentile intervals),
#'   \code{n_boot}, \code{method}, \code{original}
#' @param ... Additional arguments (currently ignored).
#' @references
#'   Hansen, B. E. (1999). Threshold effects in non-dynamic panels: estimation,
#'   testing, and inference. \emph{Journal of Econometrics}, 93(2), 345--368.
#'   \doi{10.1016/S0304-4076(99)00025-1}
#' @importFrom future plan sequential multisession
#' @importFrom furrr future_map furrr_options
#' @importFrom parallelly availableCores
#' @importFrom stats quantile
#' @export
#' @examples
#' \donttest{
#' set.seed(4)
#' n <- 30; tt <- 8
#' df <- data.frame(id = rep(1:n, each = tt), time = rep(1:tt, n),
#'                  x1 = rnorm(n * tt), q = rnorm(n * tt))
#' df$y <- 1.5 * df$x1 + (df$q > 0.3) * (-0.9 * df$x1) + rnorm(n * tt, 0, 0.5)
#' fit  <- bptr(y ~ x1, data = df, id = "id", time = "time",
#'              q = "q", n_thresh = 1, buffer = TRUE)
#' boot <- bptr_bootstrap(fit, n_boot = 49)
#' print(boot)
#' }
bptr_bootstrap <- function(x, n_boot = 299, workers = NULL, seed = 42) {
  if (!inherits(x, "bptr")) stop("Object must be of class 'bptr'")
  set.seed(seed)
  w <- if (is.null(workers)) max(1L, parallelly::availableCores() - 1L) else
    max(1L, as.integer(workers))
  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)
  if (w > 1L) future::plan(future::multisession, workers = w) else
    future::plan(future::sequential)

  fv    <- x$fitted_values; rs <- x$residuals
  id_v  <- x$data[[x$id]]; uniq <- unique(id_v)
  dep   <- as.character(x$formula[[2]])

  one_boot <- function(b) {
    rs_s <- numeric(length(rs))
    for (i in uniq) {
      idx <- which(id_v == i)
      rs_s[idx] <- sample(rs[idx], replace = TRUE)
    }
    db <- x$data; db[[dep]] <- fv + rs_s
    tryCatch(
      { fb <- bptr(formula = x$formula, data = db, id = x$id, time = x$time,
                   q = x$q_name, n_thresh = x$n_thresh, buffer = x$buffer,
                   trim = x$trim, se_type = x$se_type)
        list(gamma = fb$gamma, beta1 = fb$beta1, beta2 = fb$beta2) },
      error = function(e) NULL)
  }

  bl   <- furrr::future_map(.x = seq_len(n_boot), .f = one_boot,
                             .options = furrr::furrr_options(seed = seed))
  bl   <- Filter(Negate(is.null), bl); n_ok <- length(bl)
  if (n_ok == 0) stop("All bootstrap replications failed.")
  if (n_ok < n_boot)
    warning(sprintf("%d replications failed.", n_boot - n_ok))

  gb <- do.call(rbind, lapply(bl, `[[`, "gamma"))
  b1 <- do.call(rbind, lapply(bl, `[[`, "beta1"))
  b2 <- do.call(rbind, lapply(bl, `[[`, "beta2"))
  if (!is.matrix(gb)) gb <- matrix(gb, ncol = 1)
  colnames(gb) <- paste0("gamma", seq_len(ncol(gb)))
  colnames(b1) <- colnames(b2) <- x$var_names

  all_b <- cbind(gb, b1, b2)
  ci_lo <- apply(all_b, 2, stats::quantile, 0.025, na.rm = TRUE)
  ci_hi <- apply(all_b, 2, stats::quantile, 0.975, na.rm = TRUE)
  pn    <- c(colnames(gb),
             paste0("regime1_", x$var_names),
             paste0("regime2_", x$var_names))
  names(ci_lo) <- names(ci_hi) <- pn

  result <- list(gamma_boot = gb, beta1_boot = b1, beta2_boot = b2,
                 ci_lower = ci_lo, ci_upper = ci_hi, n_boot = n_ok,
                 method = "residual bootstrap (within-unit resampling)",
                 original = x)
  class(result) <- "bptr_bootstrap"
  result
}

#' @rdname bptr_bootstrap
#' @method print bptr_bootstrap
#' @export
print.bptr_bootstrap <- function(x, ...) {
  cat("\n--- Bootstrap Inference for BPTR ---\n")
  cat(sprintf("Method: %s\n", x$method))
  cat(sprintf("Replications: %d\n\n95%% Confidence Intervals:\n", x$n_boot))
  print(data.frame(lower = round(x$ci_lower, 4),
                   upper = round(x$ci_upper, 4)))
  invisible(x)
}