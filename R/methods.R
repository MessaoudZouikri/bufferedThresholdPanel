# =============================================================================
# S3 methods and inference functions for bufferedThresholdPanel
# =============================================================================

# --------------------------------------------------------------------------- #
#  print.bptr                                                                  #
# --------------------------------------------------------------------------- #

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

#' @export
summary.bptr <- function(object, ...) {
  cm <- object$coefficients; sm <- object$std_errors; df_r <- object$df_residual
  t_m <- cm / sm; p_m <- 2 * stats::pt(-abs(t_m), df = df_r)
  sf  <- function(p) ifelse(p<.01,"***",ifelse(p<.05,"**",ifelse(p<.10,"*","")))
  tbls <- lapply(seq_len(ncol(cm)), function(r) {
    data.frame(Estimate=cm[,r], Std.Error=sm[,r],
               t.value=t_m[,r], "Pr(>|t|)"=p_m[,r], sig=sf(p_m[,r]),
               check.names=FALSE, stringsAsFactors=FALSE)
  })
  names(tbls) <- paste0("regime", seq_along(tbls))
  structure(
    list(call=object$call, model_type=if(object$buffer)"BTPD"else"PTR",
         n_regimes=object$n_regimes, thresholds=object$thresholds,
         n_obs=object$n_obs, n_groups=object$n_groups,
         n_obs_regime=object$n_obs_regime, df_residual=df_r,
         ssr=object$ssr, sigma=sqrt(object$ssr/max(1,df_r)),
         r2_within=1-object$ssr/object$tss_within,
         coef_tables=tbls, se_type=object$se_type, panel_info=object$panel_info),
    class = "summary.bptr")
}

#' @export
print.summary.bptr <- function(x, digits = 4, ...) {
  cat("\n=== Buffered Panel Threshold Regression ===\n")
  cat(sprintf("Call: %s\n\n", deparse(x$call)))
  cat(sprintf("Model: %s | Regimes: %d | SE: %s\n",
              x$model_type, x$n_regimes, x$se_type))
  cat(sprintf("Panel: N=%d, T=%d, NT=%d\n",
              x$n_groups, x$panel_info$n_periods, x$n_obs))
  cat(sprintf("Thresholds: %s\n", paste(round(x$thresholds,digits),collapse="  ")))
  cat(sprintf("Residual SE: %.4f | Within R2: %.4f | SSR: %.4f | df: %d\n\n",
              x$sigma, x$r2_within, x$ssr, x$df_residual))
  for (nm in names(x$coef_tables)) {
    r <- as.integer(sub("regime","",nm))
    cat(sprintf("--- Regime %d  (n = %d) ---\n", r, x$n_obs_regime[r]))
    tbl <- x$coef_tables[[nm]]; tbl[,1:4] <- round(tbl[,1:4], digits)
    print(tbl, quote=FALSE); cat("\n")
  }
  cat("Signif: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n")
  invisible(x)
}



# --------------------------------------------------------------------------- #
#  augment.bptr                                                                #
# --------------------------------------------------------------------------- #

#' Augment a \code{bptr} object
#'
#' Adds fitted values, residuals, and regime classification to the original
#' data frame.
#'
#' @param x    A fitted \code{bptr} object.
#' @param data Optionally, override the data frame stored in \code{x$data}.
#' @param ...  Unused; present for S3 compatibility.
#'
#' @return A \code{\link[tibble]{tibble}} containing all columns of the
#'   original data plus:
#'   \describe{
#'     \item{\code{.fitted}}{Fitted values.}
#'     \item{\code{.resid}}{Residuals.}
#'     \item{\code{.regime}}{Integer regime label (1, 2, or 3).}
#'   }
#' @importFrom broom augment
#' @export
augment.bptr <- function(x, data = NULL, ...) {

  if (is.null(data)) {
    if (is.null(x$data))
      stop(
        "No data stored in the `bptr` object. ",
        "Supply it explicitly: augment(fit, data = your_df)",
        call. = FALSE
      )
    data <- x$data
  }

  data$.fitted <- as.numeric(x$fitted_values)
  data$.resid  <- as.numeric(x$residuals)
  data$.regime <- as.integer(x$regime_classification)

  tibble::as_tibble(data)
}


# --------------------------------------------------------------------------- #
#  Standard S3 extractors                                                      #
# --------------------------------------------------------------------------- #

#' @importFrom stats nobs
#' @export
coef.bptr      <- function(object, ...) object$coefficients
#' @export
fitted.bptr    <- function(object, ...) object$fitted_values
#' @export
residuals.bptr <- function(object, ...) object$residuals
#' @export
nobs.bptr      <- function(object, ...) object$n_obs

#' @export
vcov.bptr <- function(object, ...) {
  sm <- object$std_errors
  v  <- diag(as.vector(sm)^2)
  rn <- as.vector(outer(rownames(sm), paste0("regime", seq_len(ncol(sm))), paste, sep=":"))
  rownames(v) <- colnames(v) <- rn; v
}

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
    if (length(which(ind1==1))>0) pred[ind1==1] <- X[ind1==1,,drop=FALSE] %*% object$beta1
    if (length(which(ind2==1))>0) pred[ind2==1] <- X[ind2==1,,drop=FALSE] %*% object$beta2
  } else {
    if (!object$buffer) {
      ind1 <- as.numeric(q_new<=gamma[1])
      ind2 <- as.numeric(q_new>gamma[1] & q_new<=gamma[2])
      ind3 <- as.numeric(q_new>gamma[2])
    } else {
      d <- buildBufferIndicators3(q_new, gamma[1], gamma[2], gamma[3], gamma[4],
                                  newdata[[object$id]])
      ind1 <- as.numeric(d==1L); ind2 <- as.numeric(d==2L); ind3 <- as.numeric(d==3L)
    }
    if (sum(ind1)>0) pred[ind1==1] <- X[ind1==1,,drop=FALSE] %*% object$beta1
    if (sum(ind2)>0) pred[ind2==1] <- X[ind2==1,,drop=FALSE] %*% object$beta2
    if (sum(ind3)>0) pred[ind3==1] <- X[ind3==1,,drop=FALSE] %*% object$beta3
  }
  pred
}

# --------------------------------------------------------------------------- #
#  plot.bptr                                                                   #
#  .data$ pronouns silence R CMD check "no visible binding" notes for         #
#  column names used inside ggplot2::aes().                                    #
# --------------------------------------------------------------------------- #

#' @importFrom ggplot2 ggplot aes geom_point geom_hline labs theme_bw
#' @importFrom rlang .data
#' @export
plot.bptr <- function(x, which = 1:2, ...) {
  rc     <- factor(x$regime_classification)
  last_p <- NULL

  if (1 %in% which) {
    df1 <- data.frame(fitted = x$fitted_values,
                      resid  = x$residuals,
                      regime = rc)
    p1 <- ggplot2::ggplot(df1,
                          ggplot2::aes(x      = .data$fitted,
                                       y      = .data$resid,
                                       colour = .data$regime)) +
      ggplot2::geom_point(alpha = 0.4, size = 0.9) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed",
                          colour = "grey40") +
      ggplot2::labs(title = "Residuals vs Fitted",
                    x = "Fitted", y = "Residuals") +
      ggplot2::theme_bw()
    print(p1); last_p <- p1
  }

  if (2 %in% which) {
    df2 <- data.frame(obs    = seq_along(x$data[[x$q_name]]),
                      q      = x$data[[x$q_name]],
                      regime = rc)
    p2 <- ggplot2::ggplot(df2,
                          ggplot2::aes(x      = .data$obs,
                                       y      = .data$q,
                                       colour = .data$regime)) +
      ggplot2::geom_point(alpha = 0.4, size = 0.9)
    for (thr in x$thresholds)
      p2 <- p2 + ggplot2::geom_hline(yintercept = thr,
                                      linetype = "dashed",
                                      colour = "black")
    p2 <- p2 +
      ggplot2::labs(title = "Threshold Variable / Regime Classification",
                    x = "Observation", y = x$q_name) +
      ggplot2::theme_bw()
    print(p2); last_p <- p2
  }
  invisible(last_p)
}

# --------------------------------------------------------------------------- #
#  bptr_test: F1,2 \u2014 linearity vs 2-regime                                   #
# --------------------------------------------------------------------------- #

#' Sup-LR Test for Threshold Existence (F1,2)
#'
#' Tests H0: linear panel model against H1: 2-regime BTPD/PTR using the
#' bootstrap procedure of Belarbi et al. (2021) Algorithm 1 (bootstrap
#' residuals from the linear null).
#'
#' @param formula Model formula
#' @param data Panel data frame
#' @param id Cross-sectional identifier column
#' @param time Time identifier column
#' @param q Threshold variable column
#' @param buffer Logical. Buffered model?
#' @param n_boot Integer. Bootstrap replications (default 299)
#' @param trim Numeric. Trimming fraction (default 0.15)
#' @param grid_size Integer. Grid resolution (default 300)
#' @param seed Integer. Random seed (default 42)
#' @return Object of class \code{"bptr_test"}
#' @importFrom stats quantile
#' @export
bptr_test <- function(formula, data, id, time, q,
                      buffer=FALSE, n_boot=299, trim=0.15, grid_size=300, seed=42) {
  set.seed(seed)
  panel_info <- validatePanel(data, id, time)

  mf <- stats::model.frame(formula, data)
  y  <- stats::model.response(mf)
  X  <- stats::model.matrix(formula, data)
  if ("(Intercept)" %in% colnames(X)) X <- X[,-1,drop=FALSE]

  fe     <- removeFE(y, X, data[[id]])
  y_dm   <- fe$y_dm; X_dm <- fe$X_dm
  b0     <- solve(crossprod(X_dm)) %*% crossprod(X_dm, y_dm)
  resid0 <- y_dm - X_dm %*% b0
  S1     <- sum(resid0^2)

  fit_h1 <- bptr(formula=formula, data=data, id=id, time=time, q=q,
                 n_thresh=1, buffer=buffer, trim=trim, grid_size=grid_size)
  S2     <- fit_h1$ssr
  F_obs  <- (S1 - S2) / (S2 / max(1, fit_h1$df_residual))

  id_vec <- data[[id]]; uniq <- unique(id_vec); fv0 <- as.vector(X_dm %*% b0)
  dep    <- as.character(formula[[2]])

  boot_stats <- vapply(seq_len(n_boot), function(b) {
    rs <- numeric(length(resid0))
    for (i in uniq) { idx <- which(id_vec==i); rs[idx] <- sample(resid0[idx], replace=TRUE) }
    db <- data; db[[dep]] <- fv0 + rs
    tryCatch({
      fe_b  <- removeFE(stats::model.response(stats::model.frame(formula,db)),
                        { Xb <- stats::model.matrix(formula,db);
                          if("(Intercept)"%in%colnames(Xb)) Xb <- Xb[,-1,drop=FALSE]; Xb },
                        db[[id]])
      b0b   <- solve(crossprod(fe_b$X_dm)) %*% crossprod(fe_b$X_dm, fe_b$y_dm)
      S1b   <- sum((fe_b$y_dm - fe_b$X_dm %*% b0b)^2)
      fb    <- bptr(formula=formula, data=db, id=id, time=time, q=q,
                    n_thresh=1, buffer=buffer, trim=trim, grid_size=grid_size)
      (S1b - fb$ssr) / (fb$ssr / max(1, fb$df_residual))
    }, error=function(e) NA_real_)
  }, numeric(1))

  boot_stats <- boot_stats[!is.na(boot_stats)]
  result <- list(stat=F_obs, p_value=mean(boot_stats >= F_obs),
                 boot_stats=boot_stats, n_boot=length(boot_stats),
                 threshold=fit_h1$thresholds, model=fit_h1,
                 test_label="F1,2: linearity vs 2-regime")
  class(result) <- "bptr_test"
  result
}

#' @export
print.bptr_test <- function(x, ...) {
  cat(sprintf("\n--- %s ---\n", x$test_label))
  cat(sprintf("Sup-LR statistic : %.4f\n", x$stat))
  cat(sprintf("Bootstrap p-value: %.4f  (B = %d)\n", x$p_value, x$n_boot))
  cat(sprintf("Threshold(s)     : %s\n", paste(round(x$threshold,4),collapse="  ")))
  sig <- if(x$p_value<.01)"***" else if(x$p_value<.05)"**" else if(x$p_value<.10)"*" else ""
  cat(sprintf("Conclusion: %s threshold effect %s\n\n",
              if(x$p_value<.10)"Significant" else "No significant", sig))
  invisible(x)
}

# --------------------------------------------------------------------------- #
#  bptr_test_23: F2,3 \u2014 2-regime vs 3-regime                                 #
# --------------------------------------------------------------------------- #

#' Sup-LR Test of 2-Regime Against 3-Regime BTPD (F2,3)
#'
#' Tests H0: 2-regime BTPD/PTR against H1: 3-regime BTPD/PTR.
#'
#' The bootstrap critical values are obtained by resampling residuals from the
#' \emph{2-regime} model (the alternative of the previous F1,2 test, which
#' becomes the null here).  This is the key distinction from the F1,2 test and
#' follows Belarbi et al. (2021) Section 2.3: \emph{"the bootstrap errors used
#' are no longer those obtained under the null hypothesis but under the
#' alternative hypothesis"}.
#'
#' @param fit_2reg A fitted \code{bptr} object with \code{n_thresh = 1} (the
#'   2-regime model that forms H0 of this test)
#' @param n_boot Integer. Bootstrap replications (default 299)
#' @param grid_size_3 Integer. Grid resolution for 3-regime search (default 50)
#' @param seed Integer. Random seed (default 42)
#' @param workers Integer or NULL. Parallel workers (not used; kept for API
#'   consistency)
#' @return Object of class \code{"bptr_test23"} with fields: \code{stat},
#'   \code{p_value}, \code{boot_stats}, \code{n_boot}, \code{thresholds_2},
#'   \code{thresholds_3}, \code{model_2reg}, \code{model_3reg}
#' @export
bptr_test_23 <- function(fit_2reg, n_boot = 299, grid_size_3 = 50L, seed = 42L,
                         workers = NULL) {

  if (!inherits(fit_2reg, "bptr") || fit_2reg$n_thresh != 1L)
    stop("'fit_2reg' must be a bptr object with n_thresh = 1")

  set.seed(seed)

  # ---- Fit 3-regime model under H1 --------------------------------------- #
  fit_3reg <- bptr(
    formula     = fit_2reg$formula,
    data        = fit_2reg$data,
    id          = fit_2reg$id,
    time        = fit_2reg$time,
    q           = fit_2reg$q_name,
    n_thresh    = 2L,
    buffer      = fit_2reg$buffer,
    trim        = fit_2reg$trim,
    grid_size_3 = grid_size_3,
    se_type     = fit_2reg$se_type
  )

  S2     <- fit_2reg$ssr
  S3     <- fit_3reg$ssr
  sig2_3 <- S3 / max(1, fit_3reg$df_residual)
  F_obs  <- (S2 - S3) / sig2_3

  # ---- Bootstrap under H0 (2-regime model) -------------------------------- #
  # Per Belarbi et al. (2021) Section 2.3: resample residuals from the 2-regime
  # model (NOT from the linear model used in the F1,2 test).
  fitted_2    <- fit_2reg$fitted_values
  residuals_2 <- fit_2reg$residuals
  id_vec      <- fit_2reg$data[[fit_2reg$id]]
  uniq_ids    <- unique(id_vec)
  formula_    <- fit_2reg$formula
  data_orig   <- fit_2reg$data
  dep         <- as.character(formula_[[2]])

  boot_stats <- vapply(seq_len(n_boot), function(b) {
    # Resample within-unit residuals from the 2-regime model
    rs <- numeric(length(residuals_2))
    for (i in uniq_ids) {
      idx <- which(id_vec == i)
      rs[idx] <- sample(residuals_2[idx], size = length(idx), replace = TRUE)
    }
    db <- data_orig; db[[dep]] <- fitted_2 + rs

    tryCatch({
      # Refit 2-regime model on bootstrap sample
      fb2 <- bptr(formula=formula_, data=db, id=fit_2reg$id, time=fit_2reg$time,
                  q=fit_2reg$q_name, n_thresh=1L, buffer=fit_2reg$buffer,
                  trim=fit_2reg$trim, grid_size=300L, se_type=fit_2reg$se_type)
      # Refit 3-regime model on bootstrap sample
      fb3 <- bptr(formula=formula_, data=db, id=fit_2reg$id, time=fit_2reg$time,
                  q=fit_2reg$q_name, n_thresh=2L, buffer=fit_2reg$buffer,
                  trim=fit_2reg$trim, grid_size_3=grid_size_3, se_type=fit_2reg$se_type)
      (fb2$ssr - fb3$ssr) / (fb3$ssr / max(1, fb3$df_residual))
    }, error = function(e) NA_real_)
  }, numeric(1))

  boot_stats <- boot_stats[!is.na(boot_stats)]
  n_ok       <- length(boot_stats)
  if (n_ok == 0) stop("All bootstrap replications failed. Check data or reduce grid_size_3.")
  if (n_ok < n_boot)
    warning(sprintf("%d of %d bootstrap replications failed and were discarded.",
                    n_boot - n_ok, n_boot))

  result <- list(
    stat          = F_obs,
    p_value       = mean(boot_stats >= F_obs),
    boot_stats    = boot_stats,
    n_boot        = n_ok,
    thresholds_2  = fit_2reg$thresholds,
    thresholds_3  = fit_3reg$thresholds,
    model_2reg    = fit_2reg,
    model_3reg    = fit_3reg,
    test_label    = "F2,3: 2-regime vs 3-regime"
  )
  class(result) <- "bptr_test23"
  result
}

#' @export
print.bptr_test23 <- function(x, ...) {
  cat(sprintf("\n--- %s ---\n", x$test_label))
  cat(sprintf("H0 thresholds (2-regime): %s\n",
              paste(round(x$thresholds_2, 4), collapse = "  ")))
  cat(sprintf("H1 thresholds (3-regime): %s\n",
              paste(round(x$thresholds_3, 4), collapse = "  ")))
  cat(sprintf("Sup-LR statistic : %.4f\n", x$stat))
  cat(sprintf("Bootstrap p-value: %.4f  (B = %d)\n", x$p_value, x$n_boot))
  sig <- if(x$p_value<.01)"***" else if(x$p_value<.05)"**" else if(x$p_value<.10)"*" else ""
  cat(sprintf("Conclusion: %s evidence for 3-regime model %s\n\n",
              if(x$p_value<.10)"Significant" else "No significant", sig))
  invisible(x)
}

# --------------------------------------------------------------------------- #
#  bptr_test_seq: Full sequential test F1,2 then F2,3                        #
# --------------------------------------------------------------------------- #

#' Sequential Regime Test for BTPD/PTR Models
#'
#' Implements the full sequential testing strategy of Belarbi et al. (2021),
#' Section 2.3:
#' \enumerate{
#'   \item Test linearity vs 2-regime model (F1,2, bootstrap under linear H0)
#'   \item If F1,2 is significant at \code{alpha}, test 2-regime vs 3-regime
#'     (F2,3, bootstrap under 2-regime H0)
#'   \item Select the preferred number of regimes based on sequential p-values
#' }
#' The final recommended model is returned as \code{$final_model}.
#'
#' @param formula Model formula
#' @param data Panel data frame
#' @param id Cross-sectional identifier column
#' @param time Time identifier column
#' @param q Threshold variable column
#' @param buffer Logical. Buffered model?
#' @param n_boot Integer. Bootstrap replications for each test (default 299)
#' @param trim Numeric. Trimming fraction (default 0.15)
#' @param grid_size Integer. Grid resolution for 2-regime search (default 300)
#' @param grid_size_3 Integer. Grid resolution for 3-regime BTPD search
#'   (default 50)
#' @param alpha Numeric. Significance level for the sequential decision rule
#'   (default 0.10)
#' @param seed Integer. Random seed (default 42)
#' @return Object of class \code{"bptr_test_seq"} with fields:
#'   \code{test_12}, \code{test_23} (NULL if F1,2 not significant),
#'   \code{final_model}, \code{n_regimes_selected}, \code{alpha}
#' @export
bptr_test_seq <- function(formula, data, id, time, q,
                          buffer      = FALSE,
                          n_boot      = 299,
                          trim        = 0.15,
                          grid_size   = 300,
                          grid_size_3 = 50L,
                          alpha       = 0.10,
                          seed        = 42) {

  cat("Step 1: Testing linearity vs 2-regime model (F1,2)...\n")
  test_12 <- bptr_test(formula=formula, data=data, id=id, time=time, q=q,
                       buffer=buffer, n_boot=n_boot, trim=trim,
                       grid_size=grid_size, seed=seed)

  test_23       <- NULL
  n_reg_sel     <- 1L
  final_model   <- NULL   # linear model not stored in bptr_test

  if (test_12$p_value >= alpha) {
    cat(sprintf("  F1,2 p = %.4f >= %.2f: accept linearity. Stopping.\n\n",
                test_12$p_value, alpha))
    final_model <- test_12$model   # 2-regime is best we have
    n_reg_sel   <- 1L              # but linear is preferred -- flag for user
  } else {
    cat(sprintf("  F1,2 p = %.4f < %.2f: linearity rejected.\n", test_12$p_value, alpha))
    cat("Step 2: Testing 2-regime vs 3-regime model (F2,3)...\n")

    test_23 <- bptr_test_23(fit_2reg=test_12$model, n_boot=n_boot,
                             grid_size_3=grid_size_3, seed=seed)

    if (test_23$p_value >= alpha) {
      cat(sprintf("  F2,3 p = %.4f >= %.2f: 2-regime model selected.\n\n",
                  test_23$p_value, alpha))
      final_model <- test_12$model
      n_reg_sel   <- 2L
    } else {
      cat(sprintf("  F2,3 p = %.4f < %.2f: 3-regime model selected.\n\n",
                  test_23$p_value, alpha))
      final_model <- test_23$model_3reg
      n_reg_sel   <- 3L
    }
  }

  result <- list(
    test_12            = test_12,
    test_23            = test_23,
    final_model        = final_model,
    n_regimes_selected = n_reg_sel,
    alpha              = alpha
  )
  class(result) <- "bptr_test_seq"
  result
}

#' @export
print.bptr_test_seq <- function(x, ...) {
  cat("\n========= Sequential Regime Test (Belarbi et al. 2021) =========\n\n")
  print(x$test_12)
  if (!is.null(x$test_23)) print(x$test_23)
  cat(sprintf("==> Selected model: %d regime(s)  (alpha = %.2f)\n\n",
              x$n_regimes_selected, x$alpha))
  if (!is.null(x$final_model)) {
    cat("Final model summary:\n")
    print(x$final_model)
  }
  invisible(x)
}

# --------------------------------------------------------------------------- #
#  bptr_bootstrap: residual bootstrap CI for 2-regime model                   #
# --------------------------------------------------------------------------- #

#' Bootstrap Inference for 2-Regime BPTR Models
#'
#' @param x A fitted \code{bptr} object with \code{n_thresh = 1}
#' @param n_boot Integer. Bootstrap replications (default 299)
#' @param workers Integer or NULL. Parallel workers
#' @param seed Integer. Random seed (default 42)
#' @return Object of class \code{"bptr_bootstrap"}
#' @importFrom future plan sequential multisession
#' @importFrom furrr future_map furrr_options
#' @importFrom parallelly availableCores
#' @importFrom stats quantile
#' @export
bptr_bootstrap <- function(x, n_boot = 299, workers = NULL, seed = 42) {
  if (!inherits(x, "bptr")) stop("Object must be of class 'bptr'")
  set.seed(seed)
  w <- if (is.null(workers)) max(1L, parallelly::availableCores()-1L) else max(1L, as.integer(workers))
  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)
  if (w > 1L) future::plan(future::multisession, workers=w) else future::plan(future::sequential)

  fv <- x$fitted_values; rs <- x$residuals; id_vec <- x$data[[x$id]]
  uniq <- unique(id_vec); dep <- as.character(x$formula[[2]])

  one_boot <- function(b) {
    rs_s <- numeric(length(rs))
    for (i in uniq) { idx <- which(id_vec==i); rs_s[idx] <- sample(rs[idx],replace=TRUE) }
    db <- x$data; db[[dep]] <- fv + rs_s
    tryCatch({
      fb <- bptr(formula=x$formula, data=db, id=x$id, time=x$time, q=x$q_name,
                 n_thresh=x$n_thresh, buffer=x$buffer, trim=x$trim, se_type=x$se_type)
      list(gamma=fb$gamma, beta1=fb$beta1, beta2=fb$beta2)
    }, error=function(e) NULL)
  }

  bl <- furrr::future_map(.x=seq_len(n_boot), .f=one_boot,
                          .options=furrr::furrr_options(seed=seed))
  bl <- Filter(Negate(is.null), bl); n_ok <- length(bl)
  if (n_ok==0) stop("All bootstrap replications failed.")
  if (n_ok<n_boot) warning(sprintf("%d replications failed.", n_boot-n_ok))

  gb <- do.call(rbind, lapply(bl, `[[`, "gamma"))
  b1 <- do.call(rbind, lapply(bl, `[[`, "beta1"))
  b2 <- do.call(rbind, lapply(bl, `[[`, "beta2"))
  if (!is.matrix(gb)) gb <- matrix(gb, ncol=1)
  colnames(gb) <- paste0("gamma", seq_len(ncol(gb)))
  colnames(b1) <- colnames(b2) <- x$var_names

  all_b <- cbind(gb, b1, b2)
  ci_lo <- apply(all_b, 2, quantile, 0.025, na.rm=TRUE)
  ci_hi <- apply(all_b, 2, quantile, 0.975, na.rm=TRUE)
  pn    <- c(colnames(gb), paste0("regime1_",x$var_names), paste0("regime2_",x$var_names))
  names(ci_lo) <- names(ci_hi) <- pn

  result <- list(gamma_boot=gb, beta1_boot=b1, beta2_boot=b2,
                 ci_lower=ci_lo, ci_upper=ci_hi, n_boot=n_ok,
                 method="residual bootstrap (within-unit resampling)", original=x)
  class(result) <- "bptr_bootstrap"
  result
}

#' @export
print.bptr_bootstrap <- function(x, ...) {
  cat("\n--- Bootstrap Inference for BPTR ---\n")
  cat(sprintf("Method: %s\n", x$method))
  cat(sprintf("Replications: %d\n\n95%% Confidence Intervals:\n", x$n_boot))
  print(data.frame(lower=round(x$ci_lower,4), upper=round(x$ci_upper,4)))
  invisible(x)
}