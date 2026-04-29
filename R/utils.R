# =============================================================================
# Utility functions for bufferedThresholdPanel
# =============================================================================

#' Remove Fixed Effects via Within-Group Demeaning
#' @param y Numeric vector of dependent variable
#' @param X Numeric matrix of explanatory variables
#' @param id Vector of cross-sectional identifiers
#' @return List with demeaned y and X matrices
#' @export
removeFE <- function(y, X, id) {
  if (is.vector(X)) X <- matrix(X, ncol = 1)
  unique_ids <- unique(id)
  n_vars <- ncol(X); n_obs <- length(y)
  y_dm <- numeric(n_obs)
  X_dm <- matrix(0, nrow = n_obs, ncol = n_vars)
  n_singleton <- 0L
  for (i in unique_ids) {
    idx <- which(id == i)
    if (length(idx) > 1) {
      y_dm[idx]   <- y[idx] - mean(y[idx], na.rm = TRUE)
      X_dm[idx, ] <- sweep(X[idx, , drop = FALSE], 2,
                           colMeans(X[idx, , drop = FALSE], na.rm = TRUE), "-")
    } else {
      n_singleton <- n_singleton + 1L
    }
  }
  if (n_singleton > 0L)
    warning(sprintf(
      "%d singleton unit(s) with T=1 found; their observations are demeaned to zero and contribute nothing to estimation.",
      n_singleton))
  colnames(X_dm) <- colnames(X)
  list(y_dm = y_dm, X_dm = X_dm)
}

#' Build Threshold Indicators
#' @param q Numeric vector of threshold variable
#' @param gamma Threshold value
#' @return Numeric vector of indicators (1 if q <= gamma, 0 otherwise)
#' @export
buildIndicators <- function(q, gamma) as.numeric(q <= gamma)

# --------------------------------------------------------------------------- #
#  2-Regime Buffer Indicators                                                  #
# --------------------------------------------------------------------------- #

#' Build 2-Regime Buffer Indicators with Hysteresis
#'
#' Transition rule (Belarbi et al. 2021, eq. 1):
#' \itemize{
#'   \item q <= g1: regime 1
#'   \item g1 < q <= g2 (buffer zone): keep previous regime
#'   \item q > g2: regime 2
#' }
#' For the first observation of each unit, regime is determined by q > g2.
#'
#' @param q Numeric vector of threshold variable (original scale, not demeaned)
#' @param g1 Lower buffer boundary (rL)
#' @param g2 Upper buffer boundary (rU)
#' @param prev_d Unused legacy argument (kept for API compatibility)
#' @param id Panel identifiers
#' @return Numeric vector: 0 = regime 1, 1 = regime 2
#' @export
buildBufferIndicators <- function(q, g1, g2, prev_d, id) {
  n_obs <- length(q); d <- numeric(n_obs)
  for (i in unique(id)) {
    idx <- sort(which(id == i))
    for (t in seq_along(idx)) {
      q_it <- q[idx[t]]
      if (t == 1L) {
        d[idx[t]] <- as.numeric(q_it > g2)
      } else {
        d_prev <- d[idx[t - 1L]]
        d[idx[t]] <- if (q_it > g2) 1 else if (q_it < g1) 0 else d_prev
      }
    }
  }
  d
}

# --------------------------------------------------------------------------- #
#  3-Regime Buffer Indicators                                                  #
# --------------------------------------------------------------------------- #

#' Build 3-Regime Buffer Indicators with Hysteresis
#'
#' Implements the K=3 transition mechanism of Belarbi et al. (2021), Section 2.1.
#' The four buffer-zone parameters satisfy rL1 <= rU1 < rL2 <= rU2.
#'
#' Five regions of the threshold variable:
#' \enumerate{
#'   \item q <= rL1 : clearly regime 1
#'   \item rL1 < q <= rU1 (buffer zone 1): if previous regime = 1, stay in 1;
#'     if previous regime > 1, go to 2
#'   \item rU1 < q <= rL2 : clearly regime 2
#'   \item rL2 < q <= rU2 (buffer zone 2): if previous regime <= 2, stay in 2;
#'     if previous regime = 3, stay in 3
#'   \item q > rU2 : clearly regime 3
#' }
#' First observation in a buffer zone is assigned to the lower regime of that zone.
#'
#' @param q Numeric vector of threshold variable (original scale, not demeaned)
#' @param rL1 Lower boundary of buffer zone 1
#' @param rU1 Upper boundary of buffer zone 1
#' @param rL2 Lower boundary of buffer zone 2 (must be > rU1)
#' @param rU2 Upper boundary of buffer zone 2
#' @param id Panel identifiers
#' @return Integer vector with values 1L, 2L, or 3L
#' @export
buildBufferIndicators3 <- function(q, rL1, rU1, rL2, rU2, id) {
  n_obs <- length(q); d <- integer(n_obs)
  for (i in unique(id)) {
    idx <- sort(which(id == i))
    for (t in seq_along(idx)) {
      q_it <- q[idx[t]]
      if (t == 1L) {
        # Deterministic initialisation: assign to lower regime of buffer zone
        d[idx[t]] <- if (q_it <= rU1) 1L else if (q_it <= rU2) 2L else 3L
      } else {
        d_prev <- d[idx[t - 1L]]
        d[idx[t]] <- if (q_it <= rL1) {
          1L
        } else if (q_it <= rU1) {
          # Buffer zone 1: stay in 1 if came from 1, go to 2 otherwise
          if (d_prev <= 1L) 1L else 2L
        } else if (q_it <= rL2) {
          2L
        } else if (q_it <= rU2) {
          # Buffer zone 2: stay in 2 if came from 1 or 2, stay in 3 if came from 3
          if (d_prev <= 2L) 2L else 3L
        } else {
          3L
        }
      }
    }
  }
  d
}

# --------------------------------------------------------------------------- #
#  Concentrated OLS (2-regime)                                                 #
# --------------------------------------------------------------------------- #

#' Concentrated OLS for 2-Regime Threshold Regression
#' @param y_dm Demeaned dependent variable
#' @param X_dm Demeaned explanatory variables matrix
#' @param ind1 Indicator vector for regime 1
#' @param ind2 Indicator vector for regime 2
#' @return List with beta1, beta2, fitted, resid, sigma2
#' @export
concentratedOLS <- function(y_dm, X_dm, ind1, ind2) {
  n_obs <- length(y_dm); n_vars <- ncol(X_dm)
  ols_r <- function(idx) {
    if (length(idx) <= n_vars) return(rep(0, n_vars))
    XtX <- crossprod(X_dm[idx, , drop = FALSE])
    tryCatch(
      as.vector(solve(XtX) %*% crossprod(X_dm[idx, , drop = FALSE], y_dm[idx])),
      error = function(e) rep(0, n_vars)
    )
  }
  idx1 <- which(ind1 == 1); idx2 <- which(ind2 == 1)
  beta1 <- ols_r(idx1); beta2 <- ols_r(idx2)
  fv <- numeric(n_obs)
  if (length(idx1) > 0) fv[idx1] <- X_dm[idx1, , drop = FALSE] %*% beta1
  if (length(idx2) > 0) fv[idx2] <- X_dm[idx2, , drop = FALSE] %*% beta2
  resid <- y_dm - fv
  list(beta1 = beta1, beta2 = beta2, fitted = fv, resid = resid,
       sigma2 = sum(resid^2) / max(1L, n_obs - 2L * n_vars))
}

# --------------------------------------------------------------------------- #
#  Concentrated OLS (3-regime)                                                 #
# --------------------------------------------------------------------------- #

#' Concentrated OLS for 3-Regime Threshold Regression
#'
#' Estimates three sets of slope coefficients (beta1, beta2, beta3) by
#' ordinary least squares applied to each regime's sub-sample.
#'
#' @param y_dm Demeaned dependent variable
#' @param X_dm Demeaned explanatory variables matrix
#' @param ind1 Indicator vector for regime 1 (integer or numeric, 1 = in regime)
#' @param ind2 Indicator vector for regime 2
#' @param ind3 Indicator vector for regime 3
#' @return List with beta1, beta2, beta3, fitted, resid, sigma2
#' @export
concentratedOLS3 <- function(y_dm, X_dm, ind1, ind2, ind3) {
  n_obs <- length(y_dm); n_vars <- ncol(X_dm)
  ols_r <- function(idx) {
    if (length(idx) <= n_vars) return(rep(0, n_vars))
    XtX <- crossprod(X_dm[idx, , drop = FALSE])
    tryCatch(
      as.vector(solve(XtX) %*% crossprod(X_dm[idx, , drop = FALSE], y_dm[idx])),
      error = function(e) rep(0, n_vars)
    )
  }
  idx1 <- which(ind1 == 1)
  idx2 <- which(ind2 == 1)
  idx3 <- which(ind3 == 1)
  beta1 <- ols_r(idx1); beta2 <- ols_r(idx2); beta3 <- ols_r(idx3)
  fv <- numeric(n_obs)
  if (length(idx1) > 0) fv[idx1] <- X_dm[idx1, , drop = FALSE] %*% beta1
  if (length(idx2) > 0) fv[idx2] <- X_dm[idx2, , drop = FALSE] %*% beta2
  if (length(idx3) > 0) fv[idx3] <- X_dm[idx3, , drop = FALSE] %*% beta3
  resid <- y_dm - fv
  list(beta1 = beta1, beta2 = beta2, beta3 = beta3, fitted = fv, resid = resid,
       sigma2 = sum(resid^2) / max(1L, n_obs - 3L * n_vars))
}

# --------------------------------------------------------------------------- #
#  computeSSR (2-regime grid search helper)                                    #
# --------------------------------------------------------------------------- #

#' Compute SSR for 2-Regime Grid Search
#' @param y_dm Demeaned dependent variable
#' @param X_dm Demeaned explanatory variables
#' @param g_vec Scalar or two-element vector of threshold candidate(s)
#' @param q Threshold variable (original, not demeaned)
#' @param buffer Logical
#' @param id Panel identifiers (required when \code{buffer = TRUE})
#' @return Numeric SSR (Inf if any regime is too small)
#' @export
computeSSR <- function(y_dm, X_dm, g_vec, q, buffer = FALSE, id = NULL) {
  if (!buffer) {
    if (length(g_vec) == 1L) {
      ind1 <- buildIndicators(q, g_vec); ind2 <- 1 - ind1
    } else {
      ind1 <- as.numeric(q <= g_vec[1])
      ind2 <- as.numeric(q > g_vec[1] & q <= g_vec[2])
      ind3 <- as.numeric(q > g_vec[2])
      ind1 <- ind1 + ind2; ind2 <- ind3
    }
  } else {
    g1 <- g_vec[1]; g2 <- g_vec[2]
    buf  <- buildBufferIndicators(q, g1, g2, NULL, id)
    ind2 <- buf; ind1 <- 1 - buf
  }
  nv <- ncol(X_dm)
  if (sum(ind1) < nv || sum(ind2) < nv) return(Inf)
  sum(concentratedOLS(y_dm, X_dm, ind1, ind2)$resid^2)
}

# --------------------------------------------------------------------------- #
#  validatePanel                                                               #
# --------------------------------------------------------------------------- #

#' Validate Panel Data Structure
#' @param data Data frame
#' @param id Cross-sectional ID variable name
#' @param time Time variable name
#' @return List with panel structure information
#' @export
validatePanel <- function(data, id, time) {
  if (!id   %in% names(data)) stop(paste("ID variable",   id,   "not found in data"))
  if (!time %in% names(data)) stop(paste("Time variable", time, "not found in data"))
  n_units <- length(unique(data[[id]])); n_periods <- length(unique(data[[time]]))
  n_obs <- nrow(data)
  num_cols <- vapply(data, is.numeric, logical(1))
  has_miss <- any(is.na(data[, num_cols, drop = FALSE]))
  if (has_miss) warning("Missing values detected in numeric columns of data.")
  list(n_units = n_units, n_periods = n_periods, n_obs = n_obs,
       balanced = (n_obs == n_units * n_periods), has_missing = has_miss)
}

# --------------------------------------------------------------------------- #
#  robustVcov / robustSE                                                       #
# --------------------------------------------------------------------------- #

#' Full Sandwich Variance-Covariance Matrix (HC estimator)
#'
#' Returns the complete p x p sandwich matrix rather than only the diagonal
#' standard errors.  Used internally by \code{\link{robustSE}} and stored
#' per-regime in \code{bptr} objects so that \code{\link{vcov.bptr}} can
#' produce a correct block-diagonal variance matrix.
#'
#' @param X Design matrix
#' @param resid Residual vector
#' @param type One of "HC0", "HC1", "HC2", "HC3"
#' @return Symmetric p x p variance-covariance matrix
#' @export
robustVcov <- function(X, resid, type = "HC3") {
  if (is.vector(X)) X <- matrix(X, ncol = 1)
  k <- ncol(X)
  if (length(resid) == 0 || k == 0) return(matrix(0, k, k))
  n <- nrow(X)
  if (n <= k) return(matrix(Inf, k, k))
  XTX_inv <- tryCatch(solve(crossprod(X)), error = function(e) NULL)
  if (is.null(XTX_inv)) return(matrix(Inf, k, k))
  if (type %in% c("HC2", "HC3")) {
    h <- pmin(rowSums((X %*% XTX_inv) * X), 1 - .Machine$double.eps)
  }
  omega <- switch(type,
    "HC0" = resid^2, "HC1" = resid^2 * n / (n - k),
    "HC2" = resid^2 / (1 - h), "HC3" = resid^2 / (1 - h)^2, resid^2)
  XTX_inv %*% crossprod(X, diag(omega) %*% X) %*% XTX_inv
}

#' Robust Standard Errors (HC sandwich estimator)
#' @param X Design matrix
#' @param resid Residual vector
#' @param type One of "HC0", "HC1", "HC2", "HC3"
#' @return Vector of standard errors
#' @importFrom stats df.residual
#' @export
robustSE <- function(X, resid, type = "HC3") {
  sqrt(diag(robustVcov(X, resid, type)))
}
