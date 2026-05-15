# =============================================================================
# Utility functions for bufferedThresholdPanel
# =============================================================================

#' Remove Fixed Effects via Within-Group Demeaning
#' @param y Numeric vector of dependent variable
#' @param X Numeric matrix of explanatory variables
#' @param id Vector of cross-sectional identifiers
#' @return List with elements \code{y_dm} (demeaned dependent variable) and
#'   \code{X_dm} (demeaned regressor matrix). Both are unnamed to prevent
#'   name-mismatch errors when combined with model-matrix output.
#' @details
#' Within-group demeaning is applied via \code{ave()}, which is O(N·T) rather
#' than O(N·N·T).  Units with only one observation (T = 1 singletons) demean
#' to zero and contribute nothing to estimation; a warning is issued reporting
#' the count of such units.  Group means are computed with
#' \code{na.rm = TRUE}: observations with \code{NA} in \code{y} or a column
#' of \code{X} are excluded from their unit's mean but their demeaned value
#' becomes \code{NA}; use \code{\link{validatePanel}} to detect missing values
#' before fitting.
#' @importFrom stats ave
#' @export
removeFE <- function(y, X, id) {
  if (is.vector(X)) X <- matrix(X, ncol = 1)
  uid    <- unique(id)
  counts <- tabulate(match(id, uid))
  n_singleton <- sum(counts == 1L)

  # Vectorised within-group demeaning: ave() is O(N*T), not O(N * N*T).
  # unname() and NULL rownames keep the output identical to the original
  # (unnamed numeric vectors), regardless of whether input vectors carry names.
  grp_mean <- function(x) mean(x, na.rm = TRUE)
  y_dm <- unname(y - ave(y, id, FUN = grp_mean))
  X_dm <- apply(X, 2L, function(col) col - ave(col, id, FUN = grp_mean))
  if (!is.matrix(X_dm)) X_dm <- matrix(X_dm, nrow = length(y))
  rownames(X_dm) <- NULL

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
#' For the first observation of each unit, regime is determined by the
#' distance to each boundary (nearest-boundary rule) when q lies in the
#' buffer zone, matching the original Matlab implementation.  At exact
#' equidistance (\eqn{|q - g_1| = |q - g_2|}) the tiebreaker uses strict
#' \code{<} (see Details).
#'
#' @details
#' The first-observation tiebreaker evaluates \code{dist_high < dist_low}:
#' \itemize{
#'   \item \code{dist_low = |q - g1|}, \code{dist_high = |q - g2|}
#'   \item If \code{dist_high < dist_low} (closer to the upper boundary):
#'         assign to regime 2 (code 1).
#'   \item Otherwise (including exact ties): assign to regime 1 (code 0).
#' }
#' This uses \emph{strict} less-than (\code{<}) to match the original Matlab
#' implementation, where exact equidistance defaults to the lower regime
#' (more conservative initialisation).
#'
#' @param q Numeric vector of threshold variable (original scale, not demeaned)
#' @param g1 Lower buffer boundary (rL)
#' @param g2 Upper buffer boundary (rU)
#' @param prev_d Unused legacy argument (kept for API compatibility)
#' @param id Panel identifiers
#' @return Numeric vector: 0 = regime 1, 1 = regime 2
#' @export
buildBufferIndicators <- function(q, g1, g2, prev_d, id) {
  buildBufferIndicators_cpp(q, g1, g2, match(id, unique(id)))
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
#' First observation in a buffer zone is assigned to the nearest boundary regime
#' (distance-based tiebreaker, matching the original Matlab implementation).  At
#' exact equidistance (\eqn{|q - rL| = |q - rU|}) the tiebreaker uses strict
#' \code{<}, assigning to the upper regime (see Details).
#'
#' @details
#' The first-observation tiebreaker for each buffer zone:
#' \itemize{
#'   \item Buffer zone 1 (\eqn{rL1 < q \le rU1}): evaluate
#'         \code{|q - rL1| < |q - rU1|}. If \code{TRUE}, assign to regime 1;
#'         otherwise (including exact ties) assign to regime 2.
#'   \item Buffer zone 2 (\eqn{rL2 < q \le rU2}): evaluate
#'         \code{|q - rL2| < |q - rU2|}. If \code{TRUE}, assign to regime 2;
#'         otherwise (including exact ties) assign to regime 3.
#' }
#' The strict less-than (\code{<}) ensures that at exact equidistance the
#' tie resolves to the \emph{upper} regime, matching the conservative
#' initialisation rule of the original Matlab implementation.
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
  buildBufferIndicators3_cpp(q, rL1, rU1, rL2, rU2, match(id, unique(id)))
}

# --------------------------------------------------------------------------- #
#  Concentrated OLS (2-regime)                                                 #
# --------------------------------------------------------------------------- #

#' Concentrated OLS for 2-Regime Threshold Regression
#' @param y_dm Demeaned dependent variable
#' @param X_dm Demeaned explanatory variables matrix
#' @param ind1 Indicator vector for regime 1
#' @param ind2 Indicator vector for regime 2
#' @return List with \code{beta1}, \code{beta2}, \code{fitted}, \code{resid},
#'   \code{sigma2}.
#' @details
#' When a regime sub-sample has fewer observations than regressors
#' (\code{length(idx) < ncol(X_dm)}), a warning is issued and zero coefficients
#' are returned for that regime.  If the sub-matrix is exactly identified or
#' over-determined but numerically singular, \code{solve()} is wrapped in
#' \code{tryCatch}: a warning is issued and zero coefficients are returned.
#' @export
concentratedOLS <- function(y_dm, X_dm, ind1, ind2) {
  n_obs <- length(y_dm); n_vars <- ncol(X_dm)
  ols_r <- function(idx) {
    if (length(idx) < n_vars) {
      warning(sprintf(
        "A regime has only %d observation(s) but %d regressor(s); returning zero coefficients. Adjust trim or grid_size.",
        length(idx), n_vars))
      return(rep(0, n_vars))
    }
    XtX <- crossprod(X_dm[idx, , drop = FALSE])
    tryCatch(
      as.vector(solve(XtX) %*% crossprod(X_dm[idx, , drop = FALSE], y_dm[idx])),
      error = function(e) {
        warning("Singular matrix in regime OLS; returning zero coefficients.")
        rep(0, n_vars)
      }
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
#' @return List with \code{beta1}, \code{beta2}, \code{beta3}, \code{fitted},
#'   \code{resid}, \code{sigma2}.
#' @details
#' When a regime sub-sample has fewer observations than regressors
#' (\code{length(idx) < ncol(X_dm)}), a warning is issued and zero coefficients
#' are returned for that regime.  If the sub-matrix is exactly identified or
#' over-determined but numerically singular, \code{solve()} is wrapped in
#' \code{tryCatch}: a warning is issued and zero coefficients are returned.
#' Increase \code{trim} or \code{grid_size_3} if this warning appears frequently.
#' @export
concentratedOLS3 <- function(y_dm, X_dm, ind1, ind2, ind3) {
  n_obs <- length(y_dm); n_vars <- ncol(X_dm)
  ols_r <- function(idx) {
    if (length(idx) < n_vars) {
      warning(sprintf(
        "A regime has only %d observation(s) but %d regressor(s); returning zero coefficients. Adjust trim or grid_size_3.",  # nocov
        length(idx), n_vars))
      return(rep(0, n_vars))
    }
    XtX <- crossprod(X_dm[idx, , drop = FALSE])
    tryCatch(
      as.vector(solve(XtX) %*% crossprod(X_dm[idx, , drop = FALSE], y_dm[idx])),
      error = function(e) {
        warning("Singular matrix in regime OLS; returning zero coefficients.")
        rep(0, n_vars)
      }
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
    ind1 <- buildIndicators(q, g_vec); ind2 <- 1 - ind1
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
#' @return A named list with elements \code{n_units}, \code{n_periods},
#'   \code{n_obs}, \code{balanced} (logical: whether N·T == n_obs), and
#'   \code{has_missing} (logical: whether any NA appears in any column).
#' @details
#' Two additional checks are performed beyond column-presence validation:
#' \itemize{
#'   \item \strong{Missing values}: all columns (numeric, character, factor)
#'     are scanned via \code{anyNA()}; a warning is issued if any NA is found.
#'   \item \strong{Temporal ordering}: within each cross-sectional unit the
#'     time variable must be non-decreasing; a warning is issued when this
#'     fails.  Correct ordering is critical for hysteresis (buffer) regime
#'     assignment, where the current regime depends on the previous observation.
#' }
#' @export
validatePanel <- function(data, id, time) {
  if (!id   %in% names(data)) stop(paste("ID variable",   id,   "not found in data"))
  if (!time %in% names(data)) stop(paste("Time variable", time, "not found in data"))
  n_units   <- length(unique(data[[id]]))
  n_periods <- length(unique(data[[time]]))
  n_obs     <- nrow(data)

  # Check all columns for NA (not just numeric — character/factor NAs matter too)
  has_miss <- anyNA(data)
  if (has_miss) warning("Missing values detected in the data.")

  # Warn when data are not sorted by time within units — critical for BTPD
  # hysteresis, where regime assignment depends on observation order.
  sorted_by_time <- all(tapply(data[[time]], data[[id]],
                               function(t) !is.unsorted(t, na.rm = TRUE)))
  if (!sorted_by_time)
    warning("Data are not sorted by time within some units. ",
            "Buffer (hysteresis) regime indicators depend on temporal order. ",
            "Sort data by id and time before fitting.")

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
  XTX_inv %*% crossprod(X, omega * X) %*% XTX_inv
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
