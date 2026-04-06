#' Buffered Panel Threshold Regression (BPTR / PTR)
#'
#' Estimates one- or two-threshold panel data models with individual fixed effects.
#' When \code{buffer = FALSE} the classical Hansen (1999) PTR model is estimated.
#' When \code{buffer = TRUE} the Belarbi et al. (2021) BTPD model with hysteresis
#' is estimated.
#'
#' \strong{Number of regimes and thresholds:}
#' \itemize{
#'   \item \code{n_thresh = 1}: 2 regimes, 1 threshold (2 buffer-zone parameters
#'     for BTPD)
#'   \item \code{n_thresh = 2}: 3 regimes, 2 thresholds (4 buffer-zone parameters
#'     for BTPD: rL1, rU1, rL2, rU2 with rL1 <= rU1 < rL2 <= rU2)
#' }
#'
#' \strong{Grid search computational note:} For the 3-regime BTPD the search is
#' over a 4-dimensional parameter space. The grid resolution is automatically
#' capped at \code{min(grid_size, 50)} for this case to keep computation
#' feasible (~260 k candidate models at 50 points).  Use \code{grid_size_3} to
#' override.
#'
#' @param formula A formula for the model (no intercept needed; fixed effects
#'   are removed by within-demeaning)
#' @param data A data frame containing the panel data
#' @param id Character string: cross-sectional identifier column
#' @param time Character string: time identifier column
#' @param q Character string: threshold variable column
#' @param n_thresh Integer 1 or 2. Number of threshold parameters (hence 2 or 3
#'   regimes)
#' @param buffer Logical. Use buffered (hysteresis) transition (\code{TRUE}) or
#'   standard abrupt transition (\code{FALSE})
#' @param trim Numeric trimming fraction for threshold grid (default 0.15)
#' @param grid_size Integer. Grid resolution for 1-threshold / 2-regime search
#'   (default 300)
#' @param grid_size_3 Integer. Grid resolution for 3-regime BTPD 4-D search
#'   (default \code{min(grid_size, 50)}). Increase for finer search at the
#'   cost of computation time
#' @param se_type Character. Robust SE type: "HC0", "HC1", "HC2", or "HC3"
#' @param ... Additional arguments (ignored)
#'
#' @return An object of class \code{"bptr"} — a named list containing:
#'   \code{gamma}, \code{thresholds}, \code{n_regimes}, \code{n_thresh},
#'   \code{beta1}, \code{beta2}, \code{beta3} (3-regime only),
#'   \code{coefficients} (p x K matrix), \code{std_errors} (p x K matrix),
#'   \code{fitted}, \code{fitted_values}, \code{residuals}, \code{ssr},
#'   \code{tss_within}, \code{df_residual}, \code{n_obs}, \code{n_groups},
#'   \code{n_obs_regime}, \code{regime_classification}, plus model metadata.
#'
#' @examples
#' set.seed(1)
#' n <- 40; tt <- 10
#' df <- data.frame(id=rep(1:n, each=tt), time=rep(1:tt, n),
#'                  x1=rnorm(n*tt), x2=rnorm(n*tt), q=rnorm(n*tt))
#' df$y <- 1.5*df$x1 - 0.8*df$x2 +
#'         (df$q > 0.3) * (-1.2*df$x1 + 0.5*df$x2) + rnorm(n*tt, 0, 0.5)
#'
#' # 2-regime PTR
#' m1 <- bptr(y ~ x1 + x2, data=df, id="id", time="time", q="q", n_thresh=1)
#' print(m1)
#'
#' # 2-regime BTPD (buffered)
#' m2 <- bptr(y ~ x1 + x2, data=df, id="id", time="time", q="q",
#'            n_thresh=1, buffer=TRUE)
#'
#' @importFrom stats model.frame model.matrix model.response quantile lm
#'   residuals fitted coef
#' @importFrom utils head tail
#' @export
bptr <- function(formula, data, id, time, q,
                 n_thresh   = 1,
                 buffer     = FALSE,
                 trim       = 0.15,
                 grid_size  = 300,
                 grid_size_3 = NULL,
                 se_type    = "HC3",
                 ...) {

  call <- match.call()

  # ---- Input validation --------------------------------------------------- #
  if (!inherits(formula, "formula")) stop("'formula' must be a formula object")
  if (!is.data.frame(data))          stop("'data' must be a data.frame")
  if (!(n_thresh %in% c(1, 2)))      stop("'n_thresh' must be 1 or 2")
  if (trim <= 0 || trim >= 0.5)      stop("'trim' must be between 0 and 0.5")
  if (!q %in% names(data))           stop(paste("Threshold variable", q, "not found in data"))

  g3 <- if (is.null(grid_size_3)) min(as.integer(grid_size), 50L) else as.integer(grid_size_3)

  # ---- Panel structure ---------------------------------------------------- #
  panel_info <- validatePanel(data, id, time)

  # ---- Extract matrices --------------------------------------------------- #
  mf <- stats::model.frame(formula, data)
  y  <- stats::model.response(mf)
  X  <- stats::model.matrix(formula, data)
  if ("(Intercept)" %in% colnames(X)) X <- X[, -1, drop = FALSE]
  if (ncol(X) == 0) stop("No explanatory variables found in formula")
  n_vars <- ncol(X)

  # ---- Within demeaning (remove fixed effects) ---------------------------- #
  fe   <- removeFE(y, X, data[[id]])
  y_dm <- fe$y_dm
  X_dm <- fe$X_dm
  q_var <- data[[q]]
  q_dm  <- removeFE(q_var, matrix(1, nrow = length(q_var)), data[[id]])$y_dm

  id_vec  <- data[[id]]
  q_range <- quantile(q_dm, c(trim, 1 - trim), na.rm = TRUE)

  # ======================================================================== #
  #  GRID SEARCH FOR OPTIMAL THRESHOLD(S)                                     #
  # ======================================================================== #

  if (n_thresh == 1L) {
    # ---- 2-regime model --------------------------------------------------- #
    if (!buffer) {
      # Standard PTR: 1-D grid
      g_grid   <- seq(q_range[1], q_range[2], length.out = grid_size)
      ssr_grid <- sapply(g_grid, function(g) computeSSR(y_dm, X_dm, g, q_dm, FALSE))
      gamma    <- g_grid[which.min(ssr_grid)]

    } else {
      # BTPD: 2-D grid over (rL, rU) with rL <= rU
      g_grid   <- seq(q_range[1], q_range[2], length.out = grid_size)
      best_ssr <- Inf
      gamma    <- c(q_range[1], q_range[2])
      for (i in seq_along(g_grid)) {
        for (j in i:length(g_grid)) {
          s <- computeSSR(y_dm, X_dm, c(g_grid[i], g_grid[j]), q_dm, TRUE)
          if (s < best_ssr) { best_ssr <- s; gamma <- c(g_grid[i], g_grid[j]) }
        }
      }
    }

  } else {
    # ---- 3-regime model --------------------------------------------------- #
    g_grid <- seq(q_range[1], q_range[2], length.out = if (!buffer) grid_size else g3)
    G      <- length(g_grid)

    if (!buffer) {
      # Standard PTR: 2-D grid over (gamma1, gamma2) with gamma1 < gamma2
      best_ssr <- Inf; gamma <- c(g_grid[1], g_grid[G])
      for (i in 1:(G - 1)) {
        for (j in (i + 1):G) {
          i1 <- as.numeric(q_dm <= g_grid[i])
          i2 <- as.numeric(q_dm > g_grid[i] & q_dm <= g_grid[j])
          i3 <- as.numeric(q_dm > g_grid[j])
          if (sum(i1) < n_vars || sum(i2) < n_vars || sum(i3) < n_vars) next
          s <- sum(concentratedOLS3(y_dm, X_dm, i1, i2, i3)$resid^2)
          if (s < best_ssr) { best_ssr <- s; gamma <- c(g_grid[i], g_grid[j]) }
        }
      }

    } else {
      # BTPD: 4-D grid over (rL1, rU1, rL2, rU2) with rL1<=rU1 < rL2<=rU2
      # (Belarbi 2021, Section 2.2)
      if (G < 4) stop("grid_size_3 must be >= 4 for the 3-regime BTPD search")
      best_ssr <- Inf; gamma <- rep(NA_real_, 4)
      for (i1 in 1:(G - 3)) {
        rL1 <- g_grid[i1]
        for (i2 in i1:(G - 2)) {
          rU1 <- g_grid[i2]
          for (i3 in (i2 + 1):(G - 1)) {      # rL2 must be strictly > rU1
            rL2 <- g_grid[i3]
            for (i4 in i3:G) {
              rU2 <- g_grid[i4]
              d   <- buildBufferIndicators3(q_dm, rL1, rU1, rL2, rU2, id_vec)
              i1v <- as.numeric(d == 1L)
              i2v <- as.numeric(d == 2L)
              i3v <- as.numeric(d == 3L)
              if (sum(i1v) < n_vars || sum(i2v) < n_vars || sum(i3v) < n_vars) next
              s <- sum(concentratedOLS3(y_dm, X_dm, i1v, i2v, i3v)$resid^2)
              if (s < best_ssr) {
                best_ssr <- s
                gamma    <- c(rL1, rU1, rL2, rU2)
              }
            }
          }
        }
      }
      if (anyNA(gamma)) stop("No valid 3-regime BTPD configuration found. ",
                              "Try increasing grid_size_3 or relaxing trim.")
    }
  }

  # ======================================================================== #
  #  BUILD REGIME INDICATORS AT OPTIMAL THRESHOLD(S)                          #
  # ======================================================================== #

  n_regimes <- n_thresh + 1L

  if (n_thresh == 1L) {
    if (!buffer) {
      ind1 <- buildIndicators(q_dm, gamma); ind2 <- 1 - ind1
    } else {
      buf  <- buildBufferIndicators(q_dm, gamma[1], gamma[2], NULL, id_vec)
      ind2 <- buf; ind1 <- 1 - buf
    }
    regime_classification <- ifelse(ind1 == 1, 1L, 2L)

  } else {
    if (!buffer) {
      ind1 <- as.numeric(q_dm <= gamma[1])
      ind2 <- as.numeric(q_dm > gamma[1] & q_dm <= gamma[2])
      ind3 <- as.numeric(q_dm > gamma[2])
    } else {
      d    <- buildBufferIndicators3(q_dm, gamma[1], gamma[2], gamma[3], gamma[4], id_vec)
      ind1 <- as.numeric(d == 1L)
      ind2 <- as.numeric(d == 2L)
      ind3 <- as.numeric(d == 3L)
    }
    regime_classification <- as.integer(ind1 * 1L + ind2 * 2L + ind3 * 3L)
  }

  # ======================================================================== #
  #  CONCENTRATED OLS AT OPTIMAL THRESHOLDS                                   #
  # ======================================================================== #

  if (n_thresh == 1L) {
    ols <- concentratedOLS(y_dm, X_dm, ind1, ind2)
  } else {
    ols <- concentratedOLS3(y_dm, X_dm, ind1, ind2, ind3)
  }

  # ======================================================================== #
  #  ROBUST STANDARD ERRORS                                                   #
  # ======================================================================== #

  se1 <- robustSE(X_dm[ind1 == 1, , drop = FALSE],
                  ols$resid[ind1 == 1], type = se_type)
  se2 <- robustSE(X_dm[ind2 == 1, , drop = FALSE],
                  ols$resid[ind2 == 1], type = se_type)

  if (n_thresh == 2L) {
    se3 <- robustSE(X_dm[ind3 == 1, , drop = FALSE],
                    ols$resid[ind3 == 1], type = se_type)
  }

  # ======================================================================== #
  #  ASSEMBLE COEFFICIENT / SE MATRICES                                       #
  # ======================================================================== #

  var_names <- colnames(X_dm)

  if (n_thresh == 1L) {
    coef_mat <- cbind(regime1 = ols$beta1, regime2 = ols$beta2)
    se_mat   <- cbind(regime1 = se1,       regime2 = se2)
    n_obs_regime <- c(sum(ind1), sum(ind2))
  } else {
    coef_mat <- cbind(regime1 = ols$beta1, regime2 = ols$beta2, regime3 = ols$beta3)
    se_mat   <- cbind(regime1 = se1,       regime2 = se2,       regime3 = se3)
    n_obs_regime <- c(sum(ind1), sum(ind2), sum(ind3))
  }
  rownames(coef_mat) <- var_names
  rownames(se_mat)   <- var_names

  # ======================================================================== #
  #  GOODNESS-OF-FIT                                                          #
  # ======================================================================== #

  n_obs    <- length(y_dm)
  n_groups <- panel_info$n_units
  n_params <- n_regimes * n_vars
  df_resid <- n_obs - n_params - n_groups
  ssr      <- sum(ols$resid^2)
  tss_w    <- sum((y_dm - mean(y_dm))^2)

  # ======================================================================== #
  #  RESULT OBJECT                                                             #
  # ======================================================================== #

  result <- list(
    # Core estimates
    gamma               = gamma,
    thresholds          = gamma,
    n_regimes           = n_regimes,
    n_thresh            = n_thresh,
    beta1               = ols$beta1,
    beta2               = ols$beta2,
    beta3               = if (n_thresh == 2L) ols$beta3 else NULL,
    coefficients        = coef_mat,
    std_errors          = se_mat,
    # Fit
    fitted              = ols$fitted,
    fitted_values       = ols$fitted,
    residuals           = ols$resid,
    # Goodness-of-fit
    ssr                 = ssr,
    tss_within          = tss_w,
    sigma2              = ols$sigma2,
    df_residual         = df_resid,
    # Dimensions
    n_obs               = n_obs,
    n_groups            = n_groups,
    n_obs_regime        = n_obs_regime,
    regime_classification = regime_classification,
    # Model metadata
    buffer              = buffer,
    trim                = trim,
    se_type             = se_type,
    var_names           = var_names,
    call                = call,
    formula             = formula,
    data                = data,
    id                  = id,
    time                = time,
    q_name              = q,
    panel_info          = panel_info
  )

  class(result) <- "bptr"
  result
}
