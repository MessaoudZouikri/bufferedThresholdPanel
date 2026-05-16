#' Tidy a Panel Threshold Model
#'
#' @description
#' Extract coefficient estimates and statistics from a panel threshold model
#' in a tidy format suitable for downstream analysis.
#'
#' @param x A \code{bptr} object from fitting a panel threshold model
#' @param conf.int Logical indicating whether to include confidence intervals
#' @param conf.level Confidence level for intervals (default 0.95)
#' @param ... Additional arguments (not currently used)
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{regime}{Integer indicating regime number}
#'   \item{term}{Character vector of variable names}
#'   \item{estimate}{Estimated coefficients}
#'   \item{std.error}{Standard errors}
#'   \item{statistic}{t-statistics; \code{NA} when the standard error is zero
#'     or non-finite (degenerate regime with too few observations)}
#'   \item{p.value}{p-values for two-sided tests; \code{NA} when
#'     \code{statistic} is \code{NA}}
#'   \item{conf.low, conf.high}{Confidence interval bounds (if conf.int = TRUE)}
#' }
#'
#' @examples
#' set.seed(1)
#' n <- 10; tt <- 6
#' df <- data.frame(id = rep(1:n, each = tt), time = rep(1:tt, n),
#'                  x1 = rnorm(n * tt), q = rnorm(n * tt))
#' df$y <- 1.5 * df$x1 + (df$q > 0) * (-2 * df$x1) + rnorm(n * tt, 0, 0.5)
#' m <- bptr(y ~ x1, data = df, id = "id", time = "time", q = "q",
#'           n_thresh = 1, grid_size = 30)
#' broom::tidy(m)
#' broom::tidy(m, conf.int = TRUE)
#' @importFrom broom tidy
#' @importFrom tibble tibble
#' @importFrom stats qt pt
#' @method tidy bptr
#' @export
tidy.bptr <- function(x, conf.int = FALSE, conf.level = 0.95, ...) {

  coefs <- x$coefficients   # matrix: rows = variables, cols = regimes
  ses   <- x$std_errors     # same shape
  df_r  <- x$df_residual

  n_regimes      <- ncol(coefs)
  variable_names <- rownames(coefs)

  coef_vec <- as.vector(coefs)
  se_vec   <- as.vector(ses)
  # Guard against zero or non-finite SEs (degenerate regimes) — produce NA
  # rather than Inf/NaN which would propagate silently into p-values.
  t_vals <- ifelse(is.finite(se_vec) & se_vec > 0,
                   coef_vec / se_vec,
                   NA_real_)

  result <- tibble::tibble(
    regime    = rep(seq_len(n_regimes), each = length(variable_names)),
    term      = rep(variable_names, times = n_regimes),
    estimate  = coef_vec,
    std.error = se_vec,
    statistic = t_vals,
    p.value   = ifelse(!is.na(t_vals),
                       2 * stats::pt(-abs(t_vals), df = df_r),
                       NA_real_)
  )

  if (conf.int) {
    alpha  <- 1 - conf.level
    t_crit <- stats::qt(1 - alpha / 2, df = df_r)
    result$conf.low  <- result$estimate - t_crit * result$std.error
    result$conf.high <- result$estimate + t_crit * result$std.error
  }

  result
}

#' Extract Threshold Estimates with Confidence Intervals
#'
#' @description
#' Extract threshold parameter estimates together with confidence intervals.
#' Priority order: LR-inversion (when available on the model object), then
#' bootstrap percentile CIs (via \code{boot} argument or \code{x$bootstrap}),
#' then a point-only fallback.
#'
#' @param x A \code{bptr} object
#' @param conf.level Confidence level (default 0.95)
#' @param boot An optional \code{bptr_bootstrap} object returned by
#'   \code{\link{bptr_bootstrap}}. When supplied, bootstrap percentile CIs are
#'   computed from it. Takes priority over \code{x$bootstrap} if both are
#'   present.
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{threshold_id}{Integer threshold identifier}
#'   \item{estimate}{Threshold estimate}
#'   \item{conf.low}{Lower confidence bound (equals \code{estimate} when no CI
#'     information is available)}
#'   \item{conf.high}{Upper confidence bound (equals \code{estimate} when no CI
#'     information is available)}
#'   \item{n_regime1}{Observations in regime 1 (or lower regimes)}
#'   \item{n_regime2}{Observations in regime 2 (or higher regimes)}
#' }
#'
#' @examples
#' set.seed(1)
#' n <- 10; tt <- 6
#' df <- data.frame(id = rep(1:n, each = tt), time = rep(1:tt, n),
#'                  x1 = rnorm(n * tt), q = rnorm(n * tt))
#' df$y <- 1.5 * df$x1 + (df$q > 0) * (-2 * df$x1) + rnorm(n * tt, 0, 0.5)
#' m <- bptr(y ~ x1, data = df, id = "id", time = "time", q = "q",
#'           n_thresh = 1, grid_size = 30)
#' threshold_tidy(m)
#' @importFrom tibble tibble
#' @importFrom stats quantile qchisq
#' @export
threshold_tidy <- function(x, conf.level = 0.95, boot = NULL) {

  if (!is.null(boot) && !inherits(boot, "bptr_bootstrap"))
    stop("'boot' must be a 'bptr_bootstrap' object returned by bptr_bootstrap().")

  thresh  <- x$thresholds
  n_t     <- length(thresh)
  alpha   <- 1 - conf.level

  result <- tibble::tibble(
    threshold_id = seq_len(n_t),
    estimate     = thresh,
    conf.low     = thresh,
    conf.high    = thresh
  )

  # TODO (deferred): LR-inversion confidence intervals — Hansen (1999, Sec. 3).
  # bptr() would need to store the SSR profile over the threshold grid:
  #   x$lr_grid[[i]]     : LR statistics at each grid point for threshold i
  #   x$grid_points[[i]] : corresponding threshold candidates
  # Confidence set: { gamma : LR(gamma) <= qchisq(conf.level, df = 1) }.
  # The grid loop in bptr() already evaluates SSR at every candidate —
  # retaining those values is the only missing step.
  if (!is.null(x$lr_test) && !is.null(x$lr_grid)) {
    crit <- stats::qchisq(conf.level, df = 1)
    for (i in seq_len(n_t)) {
      lr_vals  <- x$lr_grid[[i]]
      grd_pts  <- x$grid_points[[i]]
      in_set   <- lr_vals <= crit
      if (any(in_set)) {
        result$conf.low[i]  <- min(grd_pts[in_set])
        result$conf.high[i] <- max(grd_pts[in_set])
      }
    }

  # Bootstrap percentile confidence intervals.
  # Supply a bptr_bootstrap object via `boot`, or attach one to the model as
  # fit$bootstrap <- boot before calling threshold_tidy().
  } else {
    boot_src <- if (!is.null(boot)) boot else x$bootstrap
    if (!is.null(boot_src) && !is.null(boot_src$gamma_boot)) {
      boot_g <- boot_src$gamma_boot
      if (!is.matrix(boot_g)) boot_g <- matrix(boot_g, ncol = 1)
      for (i in seq_len(n_t)) {
        bv                  <- boot_g[, min(i, ncol(boot_g))]
        result$conf.low[i]  <- stats::quantile(bv, alpha / 2,     na.rm = TRUE)
        result$conf.high[i] <- stats::quantile(bv, 1 - alpha / 2, na.rm = TRUE)
      }
    }
  }

  rc <- x$regime_classification
  result$n_regime1 <- vapply(seq_len(n_t), function(i)
    sum(rc <= i,  na.rm = TRUE), integer(1L))
  result$n_regime2 <- vapply(seq_len(n_t), function(i)
    sum(rc >  i,  na.rm = TRUE), integer(1L))

  result
}

#' Glance at a Panel Threshold Model
#'
#' @description
#' Extract model-level fit statistics from a \code{bptr} object.
#'
#' @param x A \code{bptr} object
#' @param ... Additional arguments (not currently used)
#'
#' @return A one-row tibble with columns:
#' \describe{
#'   \item{n_obs}{Number of observations}
#'   \item{n_groups}{Number of cross-sectional units}
#'   \item{n_thresholds}{Number of threshold parameters}
#'   \item{model_type}{Descriptor string}
#'   \item{ssr}{Sum of squared residuals}
#'   \item{sigma}{Residual standard error}
#'   \item{aic_approx}{Approximate AIC}
#'   \item{bic_approx}{Approximate BIC}
#'   \item{r.squared}{Within R-squared (fraction of within-group variance explained)}
#' }
#'
#' @details
#' \strong{Why "approx"?}  Both information criteria use the concentrated
#' log-likelihood \eqn{n \cdot \log(\text{SSR}/n)} rather than the full
#' Gaussian log-likelihood \eqn{-\frac{n}{2}\log(2\pi\hat\sigma^2) -
#' \frac{n}{2}}.  The additive constants \eqn{n(\log(2\pi)+1)} are the same
#' for every model on a given dataset and cancel in comparisons, but they
#' make the absolute values differ from those returned by R's \code{AIC()} or
#' \code{BIC()} on likelihood-based objects.
#'
#' Additionally, the \eqn{k} parameter count includes threshold parameters and
#' slope coefficients only; the \eqn{N} individual fixed-effect intercepts
#' removed by within-demeaning are excluded.  This is standard practice for
#' fixed-effects panel estimators (the incidental-parameters problem makes
#' treating them as free parameters inappropriate), but it means the values are
#' only comparable across models estimated on the same dataset with the same
#' fixed-effects structure.
#'
#' @examples
#' set.seed(1)
#' n <- 10; tt <- 6
#' df <- data.frame(id = rep(1:n, each = tt), time = rep(1:tt, n),
#'                  x1 = rnorm(n * tt), q = rnorm(n * tt))
#' df$y <- 1.5 * df$x1 + (df$q > 0) * (-2 * df$x1) + rnorm(n * tt, 0, 0.5)
#' m <- bptr(y ~ x1, data = df, id = "id", time = "time", q = "q",
#'           n_thresh = 1, grid_size = 30)
#' broom::glance(m)
#' @importFrom broom glance
#' @importFrom tibble tibble
#' @method glance bptr
#' @export
glance.bptr <- function(x, ...) {

  n_params   <- length(x$thresholds) + length(as.vector(x$coefficients))
  aic_approx <- x$n_obs * log(x$ssr / x$n_obs) + 2 * n_params
  bic_approx <- x$n_obs * log(x$ssr / x$n_obs) + log(x$n_obs) * n_params

  r.squared <- if (!is.null(x$tss_within) && x$tss_within > 0) {
    1 - x$ssr / x$tss_within
  } else {
    NA_real_
  }

  tibble::tibble(
    n_obs        = x$n_obs,
    n_groups     = if (!is.null(x$n_groups)) x$n_groups else NA_integer_,
    n_thresholds = length(x$thresholds),
    model_type   = paste0(length(x$thresholds), "-threshold",
                          if (x$buffer) " (buffered)" else ""),
    ssr          = x$ssr,
    sigma        = sqrt(x$ssr / max(1, x$df_residual)),
    aic_approx   = aic_approx,
    bic_approx   = bic_approx,
    r.squared    = r.squared
  )
}

#' Augment Data with Panel Threshold Model Results
#'
#' @description
#' Add fitted values, residuals, and regime classifications to the original data.
#'
#' @param x A \code{bptr} object
#' @param data Original data frame; if \code{NULL} the data stored in \code{x}
#'   is used
#' @param ... Additional arguments (not currently used)
#'
#' @return The input data returned as a \code{\link[tibble]{tibble}} with three
#'   additional columns: \code{.fitted} (within-demeaned fitted values),
#'   \code{.resid} (within-demeaned residuals), and \code{.regime} (integer
#'   regime assignment, 1-indexed).  If \code{data} has more rows than
#'   \code{x$n_obs} (e.g. due to NA dropping during model fitting), only the
#'   first \code{x$n_obs} rows receive model output; the remainder carry
#'   \code{NA}.
#'
#' @examples
#' set.seed(1)
#' n <- 10; tt <- 6
#' df <- data.frame(id = rep(1:n, each = tt), time = rep(1:tt, n),
#'                  x1 = rnorm(n * tt), q = rnorm(n * tt))
#' df$y <- 1.5 * df$x1 + (df$q > 0) * (-2 * df$x1) + rnorm(n * tt, 0, 0.5)
#' m <- bptr(y ~ x1, data = df, id = "id", time = "time", q = "q",
#'           n_thresh = 1, grid_size = 30)
#' head(broom::augment(m))
#' @importFrom broom augment
#' @importFrom tibble as_tibble
#' @method augment bptr
#' @export
augment.bptr <- function(x, data = NULL, ...) {

  if (is.null(data)) {
    if (!is.null(x$data)) {
      data <- x$data
    } else {
      stop("No data provided and cannot extract from model object.")
    }
  }

  data <- tibble::as_tibble(data)

  if (nrow(data) != x$n_obs) {
    warning(sprintf(
      "Data has %d rows but model has %d observations.",
      nrow(data), x$n_obs
    ))
  }

  n_rows <- min(nrow(data), x$n_obs)

  # Use 'fitted_values' and 'regime_classification' (aligned with bptr object)
  data$.fitted <- if (!is.null(x$fitted_values)) {
    x$fitted_values[seq_len(n_rows)]
  } else {
    rep(NA_real_, n_rows)
  }

  data$.resid <- if (!is.null(x$residuals)) {
    x$residuals[seq_len(n_rows)]
  } else {
    rep(NA_real_, n_rows)
  }

  data$.regime <- if (!is.null(x$regime_classification)) {
    x$regime_classification[seq_len(n_rows)]
  } else {
    rep(NA_integer_, n_rows)
  }

  data
}