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
#'   \item{statistic}{t-statistics}
#'   \item{p.value}{p-values for two-sided tests}
#'   \item{conf.low, conf.high}{Confidence interval bounds (if conf.int = TRUE)}
#' }
#'
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

  result <- tibble::tibble(
    regime    = rep(seq_len(n_regimes), each = length(variable_names)),
    term      = rep(variable_names, times = n_regimes),
    estimate  = as.vector(coefs),
    std.error = as.vector(ses),
    statistic = as.vector(coefs) / as.vector(ses),
    p.value   = 2 * stats::pt(-abs(as.vector(coefs) / as.vector(ses)), df = df_r)
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
#' When LR-inversion information is present on the model object it is used;
#' otherwise bootstrap quantiles are used if available; finally a point-only
#' fallback is returned.
#'
#' @param x A \code{bptr} object
#' @param conf.level Confidence level (default 0.95)
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{threshold_id}{Integer threshold identifier}
#'   \item{estimate}{Threshold estimate}
#'   \item{conf.low}{Lower confidence bound}
#'   \item{conf.high}{Upper confidence bound}
#'   \item{n_regime1}{Observations in regime 1 (or lower regimes)}
#'   \item{n_regime2}{Observations in regime 2 (or higher regimes)}
#' }
#'
#' @importFrom tibble tibble
#' @importFrom stats quantile qchisq
#' @export
threshold_tidy <- function(x, conf.level = 0.95) {

  # Use 'thresholds' field (aligned with bptr result object)
  thresh  <- x$thresholds
  n_t     <- length(thresh)
  alpha   <- 1 - conf.level

  result <- tibble::tibble(
    threshold_id = seq_len(n_t),
    estimate     = thresh,
    conf.low     = thresh,    # will be overwritten if CI info available
    conf.high    = thresh
  )

  # LR-inversion confidence intervals
  if (!is.null(x$lr_test) && !is.null(x$lr_grid)) {
    crit <- stats::qchisq(conf.level, df = 1)
    for (i in seq_len(n_t)) {
      lr_vals   <- x$lr_grid[[i]]
      grd_pts   <- x$grid_points[[i]]
      in_set    <- lr_vals <= crit
      if (any(in_set)) {
        result$conf.low[i]  <- min(grd_pts[in_set])
        result$conf.high[i] <- max(grd_pts[in_set])
      }
    }

  # Bootstrap percentile confidence intervals
  } else if (!is.null(x$bootstrap) && !is.null(x$bootstrap$gamma_boot)) {
    boot_g <- x$bootstrap$gamma_boot
    if (!is.matrix(boot_g)) boot_g <- matrix(boot_g, ncol = 1)
    for (i in seq_len(n_t)) {
      bv                  <- boot_g[, min(i, ncol(boot_g))]
      result$conf.low[i]  <- stats::quantile(bv, alpha / 2,     na.rm = TRUE)
      result$conf.high[i] <- stats::quantile(bv, 1 - alpha / 2, na.rm = TRUE)
    }
  }
  # else: conf.low / conf.high stay at point estimate (no CI info)

  # Regime sizes
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
#'   \item{r.squared}{Within R-squared (fraction of within-group variance explained)}
#' }
#'
#' @importFrom broom glance
#' @importFrom tibble tibble
#' @method glance bptr
#' @export
glance.bptr <- function(x, ...) {

  n_params   <- length(x$thresholds) + length(as.vector(x$coefficients))
  aic_approx <- x$n_obs * log(x$ssr / x$n_obs) + 2 * n_params

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
#' @return The data with additional columns \code{.fitted}, \code{.resid},
#'   and \code{.regime}
#'
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