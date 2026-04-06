# =============================================================================
# Package-level declarations and documentation
# =============================================================================

# --------------------------------------------------------------------------- #
#  Package overview help page                                                  #
#                                                                              #
#  This block generates man/bufferedThresholdPanel-package.Rd when            #
#  devtools::document() is run.  It is what populates:                        #
#    help(package = "bufferedThresholdPanel")                                  #
#    help.search("bufferedThresholdPanel")                                     #
#    ??bufferedThresholdPanel                                                  #
# --------------------------------------------------------------------------- #

#' bufferedThresholdPanel: Buffered Panel Threshold Regression with Hysteresis
#'
#' @description
#' Estimates panel data threshold regression models with individual fixed
#' effects.  The package implements two related models:
#'
#' \itemize{
#'   \item \strong{Panel Threshold Regression (PTR)} — abrupt regime transitions
#'     following Hansen (1999, 2000).
#'   \item \strong{Buffered Panel Threshold Data (BTPD)} — smooth,
#'     hysteresis-driven transitions via buffer zones, introduced by Belarbi
#'     et al. (2021).  When the threshold variable falls inside the buffer zone
#'     \eqn{(r_L, r_U]}, the regime indicator keeps its previous value rather
#'     than jumping immediately.
#' }
#'
#' Both two-regime (one buffer zone) and three-regime (two buffer zones) models
#' are supported.  The sequential bootstrap tests \eqn{F_{1,2}} and
#' \eqn{F_{2,3}} of Belarbi et al. (2021) determine the number of regimes.
#'
#' @section Main functions:
#' \describe{
#'   \item{\code{\link{bptr}}}{Estimate a PTR or BTPD model}
#'   \item{\code{\link{bptr_test}}}{F1,2 bootstrap test: linearity vs 2-regime}
#'   \item{\code{\link{bptr_test_23}}}{F2,3 bootstrap test: 2-regime vs 3-regime}
#'   \item{\code{\link{bptr_test_seq}}}{Full sequential testing procedure}
#'   \item{\code{\link{bptr_bootstrap}}}{Residual bootstrap confidence intervals}
#'   \item{\code{\link{bptr_table}}}{Publication-ready coefficient table (gt)}
#'   \item{\code{\link{bptr_latex}}}{Export table as LaTeX}
#'   \item{\code{\link{bptr_kable}}}{Export table as knitr::kable}
#'   \item{\code{\link{bptr_shiny}}}{Launch the interactive Shiny explorer}
#'   \item{\code{\link{threshold_tidy}}}{Tidy threshold estimates with CIs}
#' }
#'
#' @section S3 methods:
#' Objects of class \code{"bptr"} support the standard generics
#' \code{print}, \code{summary}, \code{coef}, \code{fitted}, \code{residuals},
#' \code{nobs}, \code{vcov}, \code{predict}, and \code{plot}, as well as the
#' \pkg{broom} generics \code{tidy}, \code{glance}, and \code{augment}.
#'
#' @section Built-in dataset:
#' \code{\link{panel_data}} — a balanced panel of 92 countries observed
#' annually 2002–2016 (1 380 observations), used in Hamdi et al. (2025).
#' Variables include GDP growth rate, oil rents, the Economic Complexity Index,
#' FDI, capital formation, inflation, population growth, industrial value added,
#' and trade openness.
#'
#' @section Vignette:
#' A full worked example using \code{panel_data} is available as a vignette:
#' \preformatted{
#'   vignette("bufferedThresholdPanel", package = "bufferedThresholdPanel")
#' }
#' The vignette covers: sequential testing, 2- and 3-regime estimation,
#' bootstrap CIs, tidy output, diagnostic plots, and publication tables.
#'
#' @references
#' Belarbi, Y., Hamdi, F., Khalfi, A., and Souam, S. (2021).
#' Growth, institutions and oil dependence: A buffered threshold panel approach.
#' \emph{Economic Modelling}, 99, 105477.
#' \doi{10.1016/j.econmod.2021.02.018}
#'
#' Hamdi, F., Souam, S., and Zouikri, M. (2025).
#' The heterogeneous effect of economic complexity on growth and human
#' development: New empirical evidence using buffered panel threshold regression.
#' \emph{The World Economy}, 48, 2561--2592.
#' \doi{10.1111/twec.70023}
#'
#' Hansen, B. E. (1999).
#' Threshold effects in non-dynamic panels: estimation, testing, and inference.
#' \emph{Journal of Econometrics}, 93(2), 345--368.
#' \doi{10.1016/S0304-4076(99)00025-1}
#'
#' Hansen, B. E. (2000).
#' Sample splitting and threshold estimation.
#' \emph{Econometrica}, 68(3), 575--603.
#' \doi{10.2307/2669382}
#'
#' @docType package
#' @name bufferedThresholdPanel-package
#' @aliases bufferedThresholdPanel
#' @keywords package
"_PACKAGE"


# --------------------------------------------------------------------------- #
#  Global variable declarations                                                #
#  Suppresses R CMD CHECK NOTEs for dplyr non-standard evaluation column       #
#  names used in bptr_table() (tables.R).                                      #
# --------------------------------------------------------------------------- #

utils::globalVariables(c(
  "p.value",    # tidy output / bptr_table
  "sig_stars",  # created inside bptr_table via dplyr::mutate
  "estimate",   # tidy output column
  "std.error",  # tidy output column
  "statistic",  # tidy output column
  "regime",     # tidy output column
  "term",       # tidy output column
  "conf.low",   # optional CI column
  "conf.high"   # optional CI column
))


# --------------------------------------------------------------------------- #
#  Startup message                                                             #
# --------------------------------------------------------------------------- #

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "bufferedThresholdPanel: Buffered Panel Threshold Regression (BTPD)\n",
    "  Belarbi et al. (2021) <doi:10.1016/j.econmod.2021.02.018>\n",
    "  Type vignette('bufferedThresholdPanel') for a worked example."
  )
}
