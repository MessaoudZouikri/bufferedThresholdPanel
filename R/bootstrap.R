#' Bootstrap Inference for BPTR Models
#'
#' Perform residual bootstrap to obtain confidence intervals for threshold
#' parameters and coefficients in buffered panel threshold regression models.
#'
#' @param x A fitted "bptr" object
#' @param n_boot Integer. Number of bootstrap replications (default 299)
#' @param workers Integer. Number of parallel workers (default: available cores - 1)
#' @param seed Integer. Random seed for reproducibility (default 42)
#'
#' @return A list of class "bptr_bootstrap" containing:
#' \itemize{
#'   \item \code{gamma_boot}  - Matrix of bootstrap threshold estimates (n_boot x n_thresh)
#'   \item \code{beta1_boot}  - Matrix of bootstrap regime-1 coefficients (n_boot x p)
#'   \item \code{beta2_boot}  - Matrix of bootstrap regime-2 coefficients (n_boot x p)
#'   \item \code{ci_lower}    - Named vector of lower 2.5\% confidence bounds
#'   \item \code{ci_upper}    - Named vector of upper 97.5\% confidence bounds
#'   \item \code{n_boot}      - Number of successful bootstrap replications
#'   \item \code{method}      - Bootstrap method used
#'   \item \code{original}    - Original fitted bptr object
#' }
#'
#' @details
#' The bootstrap procedure resamples residuals within each panel unit to preserve
#' cross-sectional dependence. For each replication the pseudo-outcome is
#' \eqn{y^* = \hat{y} + e^*} where \eqn{e^*} is drawn by sampling (with
#' replacement) the within-unit residuals. The threshold parameters are then
#' re-estimated on \eqn{y^*} using the same grid-search as the original model.
#' Confidence intervals are percentile-based at the 2.5\% and 97.5\% quantiles.
#'
#' @examples
#' \dontrun{
#' set.seed(42)
#' n <- 30; tt <- 8
#' df <- data.frame(
#'   id   = rep(1:n, each = tt),
#'   time = rep(1:tt, n),
#'   x1   = rnorm(n * tt),
#'   q    = rnorm(n * tt)
#' )
#' df$y <- 1.5 * df$x1 + (df$q > 0) * (-0.8 * df$x1) + rnorm(n * tt, 0, 0.5)
#'
#' model <- bptr(y ~ x1, data = df, id = "id", time = "time",
#'               q = "q", n_thresh = 1)
#'
#' boot_results <- bptr_bootstrap(model, n_boot = 99)
#' print(boot_results)
#' }
#'
#' @importFrom future plan sequential multisession
#' @importFrom furrr future_map furrr_options
#' @importFrom parallelly availableCores
#' @importFrom stats quantile
#' @export
bptr_bootstrap <- function(x, n_boot = 299, workers = NULL, seed = 42) {

  if (!inherits(x, "bptr")) {
    stop("Object must be of class 'bptr'")
  }

  # Set seed for reproducibility
  set.seed(seed)

  # Determine number of parallel workers
  w <- if (is.null(workers)) {
    max(1L, parallelly::availableCores() - 1L)
  } else {
    max(1L, as.integer(workers))
  }

  # Store original plan and switch to parallel
  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)

  if (w > 1L) {
    future::plan(future::multisession, workers = w)
  } else {
    future::plan(future::sequential)
  }

  # ------------------------------------------------------------------ #
  # Extract objects needed inside each bootstrap replication             #
  # ------------------------------------------------------------------ #
  fitted_vals <- x$fitted_values
  residuals_  <- x$residuals
  id_vec      <- x$data[[x$id]]
  unique_ids  <- unique(id_vec)
  formula_    <- x$formula
  data_orig   <- x$data
  id_name     <- x$id
  time_name   <- x$time
  q_name      <- x$q_name
  n_thresh_   <- x$n_thresh
  buffer_     <- x$buffer
  trim_       <- x$trim
  grid_size_  <- 300L        # keep same resolution as original fit

  # ------------------------------------------------------------------ #
  # Single bootstrap replication function                                #
  # ------------------------------------------------------------------ #
  one_boot <- function(b) {

    # Resample residuals within each panel unit (preserves cross-sectional
    # dependence structure as in Hansen 1999 / Belarbi et al. 2021)
    resid_star <- numeric(length(residuals_))
    for (i in unique_ids) {
      idx <- which(id_vec == i)
      resid_star[idx] <- sample(residuals_[idx], size = length(idx),
                                replace = TRUE)
    }

    # Construct pseudo-outcome
    data_b      <- data_orig
    y_star      <- fitted_vals + resid_star
    dep_var     <- as.character(formula_[[2]])
    data_b[[dep_var]] <- y_star

    # Re-estimate model on bootstrap sample
    fit_b <- tryCatch(
      bptr(
        formula   = formula_,
        data      = data_b,
        id        = id_name,
        time      = time_name,
        q         = q_name,
        n_thresh  = n_thresh_,
        buffer    = buffer_,
        trim      = trim_,
        grid_size = grid_size_,
        se_type   = x$se_type
      ),
      error = function(e) NULL
    )

    if (is.null(fit_b)) return(NULL)

    list(
      gamma = fit_b$gamma,
      beta1 = fit_b$beta1,
      beta2 = fit_b$beta2
    )
  }

  # ------------------------------------------------------------------ #
  # Run replications in parallel                                         #
  # ------------------------------------------------------------------ #
  boot_list <- furrr::future_map(
    .x = seq_len(n_boot),
    .f = one_boot,
    .options = furrr::furrr_options(seed = seed)
  )

  # Drop failed replications
  boot_list <- Filter(Negate(is.null), boot_list)
  n_success <- length(boot_list)

  if (n_success == 0L) {
    stop("All bootstrap replications failed. Check model specification.")
  }
  if (n_success < n_boot) {
    warning(sprintf("%d of %d bootstrap replications failed and were dropped.",
                    n_boot - n_success, n_boot))
  }

  # ------------------------------------------------------------------ #
  # Collect results into matrices                                        #
  # ------------------------------------------------------------------ #
  gamma_boot <- do.call(rbind, lapply(boot_list, function(r) r$gamma))
  beta1_boot <- do.call(rbind, lapply(boot_list, function(r) r$beta1))
  beta2_boot <- do.call(rbind, lapply(boot_list, function(r) r$beta2))

  # Assign column names
  n_thresh_cols <- if (is.null(ncol(gamma_boot))) 1L else ncol(gamma_boot)
  colnames(gamma_boot) <- paste0("gamma", seq_len(n_thresh_cols))
  colnames(beta1_boot) <- x$var_names
  colnames(beta2_boot) <- x$var_names

  # ------------------------------------------------------------------ #
  # Percentile confidence intervals (2.5% / 97.5%)                      #
  # ------------------------------------------------------------------ #
  all_boot <- cbind(gamma_boot, beta1_boot, beta2_boot)
  ci_lower <- apply(all_boot, 2, quantile, probs = 0.025, na.rm = TRUE)
  ci_upper <- apply(all_boot, 2, quantile, probs = 0.975, na.rm = TRUE)

  # Prefix beta names to distinguish regimes
  param_names <- c(
    colnames(gamma_boot),
    paste0("regime1_", x$var_names),
    paste0("regime2_", x$var_names)
  )
  names(ci_lower) <- param_names
  names(ci_upper) <- param_names

  # ------------------------------------------------------------------ #
  # Return object                                                        #
  # ------------------------------------------------------------------ #
  result <- list(
    gamma_boot  = gamma_boot,
    beta1_boot  = beta1_boot,
    beta2_boot  = beta2_boot,
    ci_lower    = ci_lower,
    ci_upper    = ci_upper,
    n_boot      = n_success,
    method      = "residual bootstrap (within-unit resampling)",
    original    = x
  )

  class(result) <- "bptr_bootstrap"
  return(result)
}

#' Print Method for bptr_bootstrap Objects
#'
#' @param x A "bptr_bootstrap" object
#' @param ... Additional arguments (ignored)
#' @export
print.bptr_bootstrap <- function(x, ...) {
  cat("\n--- Bootstrap Inference for Buffered Panel Threshold Regression ---\n")
  cat(sprintf("Method      : %s\n", x$method))
  cat(sprintf("Replications: %d\n\n", x$n_boot))

  cat("95% Confidence Intervals:\n")
  ci_df <- data.frame(
    lower = round(x$ci_lower, 4),
    upper = round(x$ci_upper, 4)
  )
  print(ci_df)
  invisible(x)
}
