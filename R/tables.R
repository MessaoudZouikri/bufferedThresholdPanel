#' Create Publication-Ready Table for Panel Threshold Model
#'
#' @description
#' Generate a formatted table of panel threshold model results suitable for
#' academic publication, following conventions of major econometrics journals.
#'
#' @param x A \code{bptr} object from fitting a panel threshold model
#' @param digits Number of decimal places for coefficients (default 3)
#' @param stars Logical indicating whether to include significance stars
#' @param title Optional table title
#' @param notes Optional table notes
#' @param style Table style: \code{"default"}, \code{"AER"}, or
#'   \code{"JEconometrics"}
#'
#' @return A \code{gt_tbl} object for further customisation or rendering
#'
#' @details
#' The \code{AER} style follows American Economic Review conventions (no
#' vertical lines, bold headers, three-star significance levels). The
#' \code{JEconometrics} style reports t-statistics in parentheses instead of
#' standard errors.
#'
#' @import gt
#' @importFrom dplyr mutate case_when
#' @importFrom tibble tibble
#' @export
bptr_table <- function(x, digits = 3, stars = TRUE, title = NULL,
                       notes = NULL,
                       style = c("default", "AER", "JEconometrics")) {

  style <- match.arg(style)

  # ---- coefficient block ---------------------------------------------------
  coef_data      <- tidy.bptr(x, conf.int = TRUE)
  threshold_data <- threshold_tidy(x)

  # Significance stars
  if (stars) {
    coef_data <- dplyr::mutate(coef_data,
      sig_stars = dplyr::case_when(
        p.value < 0.01 ~ "***",
        p.value < 0.05 ~ "**",
        p.value < 0.10 ~ "*",
        TRUE           ~ ""
      )
    )
  } else {
    coef_data$sig_stars <- ""
  }

  # Format coefficient estimates with stars
  coef_data$coef_formatted <- paste0(
    format(round(coef_data$estimate, digits), nsmall = digits),
    coef_data$sig_stars
  )

  # Format standard errors or t-statistics
  if (style == "JEconometrics") {
    coef_data$se_formatted <- paste0(
      "(", format(round(coef_data$statistic, digits), nsmall = digits), ")"
    )
    se_label <- "t-statistics"
  } else {
    coef_data$se_formatted <- paste0(
      "(", format(round(coef_data$std.error, digits), nsmall = digits), ")"
    )
    se_label <- "Std. errors"
  }

  # ---- reshape to wide (one column pair per regime) ------------------------
  n_regimes <- max(coef_data$regime)
  var_names <- unique(coef_data$term)
  n_vars    <- length(var_names)

  rows <- vector("list", n_vars * 2)   # alternating coef / se rows
  for (v in seq_along(var_names)) {
    coef_row <- list(term = var_names[v], row_type = "coef")
    se_row   <- list(term = "",           row_type = "se")

    for (r in seq_len(n_regimes)) {
      sub <- coef_data[coef_data$term == var_names[v] &
                         coef_data$regime == r, ]
      coef_row[[paste0("regime", r)]] <- if (nrow(sub) > 0) sub$coef_formatted else ""
      se_row[[paste0("regime", r)]]   <- if (nrow(sub) > 0) sub$se_formatted   else ""
    }
    rows[[(v - 1) * 2 + 1]] <- coef_row
    rows[[(v - 1) * 2 + 2]] <- se_row
  }

  tbl_df <- do.call(rbind, lapply(rows, as.data.frame, stringsAsFactors = FALSE))

  # ---- threshold information row -------------------------------------------
  thresh_str <- paste(
    paste0("gamma", seq_len(nrow(threshold_data)),
           " = ", round(threshold_data$estimate, digits)),
    collapse = "; "
  )

  # ---- build gt table ------------------------------------------------------
  tbl_title <- if (!is.null(title)) {
    title
  } else {
    paste0("Buffered Panel Threshold Regression \u2014 ",
           n_regimes, " Regime(s)")
  }

  regime_labels <- stats::setNames(
    paste0("Regime ", seq_len(n_regimes)),
    paste0("regime", seq_len(n_regimes))
  )

  gt_tbl <- gt::gt(tbl_df) |>
    gt::tab_header(title = tbl_title) |>
    gt::cols_label(.list = c(list(term = "Variable"), as.list(regime_labels))) |>
    gt::tab_source_note(
      source_note = paste0("Threshold estimate(s): ", thresh_str)
    )

  if (stars) {
    gt_tbl <- gt_tbl |>
      gt::tab_footnote(
        footnote = "*** p<0.01, ** p<0.05, * p<0.10"
      )
  }

  if (!is.null(notes)) {
    gt_tbl <- gt_tbl |>
      gt::tab_footnote(footnote = notes)
  }

  # AER style: remove vertical borders, bold headers
  if (style == "AER") {
    gt_tbl <- gt_tbl |>
      gt::tab_style(
        style = gt::cell_text(weight = "bold"),
        locations = gt::cells_column_labels()
      )
  }

  return(gt_tbl)
}

#' Export Panel Threshold Results as LaTeX
#'
#' @description
#' Render the results of a \code{bptr} model as a LaTeX tabular environment
#' (using \pkg{gt}'s LaTeX backend).
#'
#' @param x A \code{bptr} object
#' @param file Optional file path to write the LaTeX output. If \code{NULL}
#'   the string is returned invisibly.
#' @param ... Additional arguments passed to \code{\link{bptr_table}}
#'
#' @return LaTeX string (invisibly) or writes to \code{file}
#' @export
bptr_latex <- function(x, file = NULL, ...) {
  tbl <- bptr_table(x, ...)
  tex <- gt::as_latex(tbl)

  if (!is.null(file)) {
    writeLines(as.character(tex), con = file)
    invisible(tex)
  } else {
    invisible(tex)
  }
}

#' Export Panel Threshold Results as kable
#'
#' @description
#' Render the results as a \code{knitr::kable} table, which is useful inside
#' R Markdown / Quarto documents.
#'
#' @param x A \code{bptr} object
#' @param digits Number of decimal places (default 3)
#' @param ... Additional arguments passed to \code{knitr::kable}
#'
#' @return A \code{knitr_kable} object
#' @export
bptr_kable <- function(x, digits = 3, ...) {
  if (!requireNamespace("knitr", quietly = TRUE)) {
    stop("Package 'knitr' is required for bptr_kable(). Please install it.")
  }

  coef_data <- tidy.bptr(x, conf.int = FALSE)
  n_regimes <- max(coef_data$regime)

  wide <- do.call(cbind, lapply(seq_len(n_regimes), function(r) {
    sub <- coef_data[coef_data$regime == r,
                     c("term", "estimate", "std.error", "statistic", "p.value")]
    colnames(sub)[-1] <- paste0(colnames(sub)[-1], "_r", r)
    sub
  }))

  # Drop duplicate term columns
  dup_cols <- which(duplicated(colnames(wide)))
  if (length(dup_cols) > 0) wide <- wide[, -dup_cols]

  knitr::kable(wide, digits = digits, ...)
}

#' Launch Shiny Explorer for BPTR Models
#'
#' @description
#' Open an interactive Shiny application to explore the fitted model,
#' inspect regime assignments, and visualise the threshold profile.
#'
#' @param x A \code{bptr} object (optional). If provided, the app is
#'   pre-loaded with this model's results.
#'
#' @return Launches a Shiny app; does not return a value.
#' @export
bptr_shiny <- function(x = NULL) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Package 'shiny' is required for bptr_shiny(). Please install it.")
  }

  ui <- shiny::fluidPage(
    shiny::titlePanel("Buffered Panel Threshold Regression Explorer"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::h4("Model Summary"),
        shiny::verbatimTextOutput("model_summary")
      ),
      shiny::mainPanel(
        shiny::tabsetPanel(
          shiny::tabPanel("Coefficients",
            shiny::tableOutput("coef_table")),
          shiny::tabPanel("Thresholds",
            shiny::tableOutput("thresh_table")),
          shiny::tabPanel("Fit Statistics",
            shiny::tableOutput("glance_table"))
        )
      )
    )
  )

  server <- function(input, output, session) {
    output$model_summary <- shiny::renderPrint({
      if (!is.null(x)) print(x) else cat("No model provided.")
    })
    output$coef_table <- shiny::renderTable({
      if (!is.null(x)) tidy.bptr(x) else data.frame()
    })
    output$thresh_table <- shiny::renderTable({
      if (!is.null(x)) threshold_tidy(x) else data.frame()
    })
    output$glance_table <- shiny::renderTable({
      if (!is.null(x)) glance.bptr(x) else data.frame()
    })
  }

  shiny::shinyApp(ui = ui, server = server)
}