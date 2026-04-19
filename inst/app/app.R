library(shiny)
library(bslib)
library(plotly)
library(DT)
library(dplyr)
library(tidyr)
library(readr)
library(readxl)
if (requireNamespace("writexl", quietly = TRUE)) library(writexl)
library(gt)
library(bufferedThresholdPanel)

# ── Theme ──────────────────────────────────────────────────────────────────
app_theme <- bs_theme(
  version    = 5,
  bootswatch = "flatly",
  primary    = "#2c5282",
  secondary  = "#6c757d",
  success    = "#198754",
  info       = "#0dcaf0",
  warning    = "#ffc107",
  danger     = "#dc3545",
  base_font  = font_google("Inter"),
  code_font  = font_google("JetBrains Mono")
)

sig_stars <- function(p) {
  ifelse(p < .01, "***", ifelse(p < .05, "**", ifelse(p < .10, "*", "")))
}

# ══════════════════════════════════════════════════════════════════════════════
# MODULES
# ══════════════════════════════════════════════════════════════════════════════

# ── 1. Data Preview ────────────────────────────────────────────────────────

dataPreviewUI <- function(id) {
  ns <- NS(id)
  tabsetPanel(
    tabPanel("Dataset",
      br(),
      DTOutput(ns("data_preview"))
    ),
    tabPanel("Descriptive Statistics",
      br(),
      DTOutput(ns("desc_stats"))
    ),
    tabPanel("Panel Structure",
      br(),
      plotlyOutput(ns("panel_overview"), height = "450px")
    )
  )
}

dataPreviewServer <- function(id, data_df, dep_var, q_var, time_var) {
  moduleServer(id, function(input, output, session) {

    output$data_preview <- renderDT({
      req(data_df())
      datatable(data_df(),
                options  = list(scrollX = TRUE, pageLength = 15,
                                lengthMenu = c(10, 15, 25, 50)),
                class    = "compact stripe hover",
                rownames = FALSE)
    })

    output$desc_stats <- renderDT({
      req(data_df())
      df_num    <- data_df() |> dplyr::select(where(is.numeric))
      stats_tbl <- data.frame(
        Variable = names(df_num),
        N        = sapply(df_num, function(x) sum(!is.na(x))),
        Mean     = round(sapply(df_num, mean,     na.rm = TRUE), 4),
        SD       = round(sapply(df_num, sd,       na.rm = TRUE), 4),
        Min      = round(sapply(df_num, min,      na.rm = TRUE), 4),
        Q25      = round(sapply(df_num, quantile, 0.25, na.rm = TRUE), 4),
        Median   = round(sapply(df_num, median,   na.rm = TRUE), 4),
        Q75      = round(sapply(df_num, quantile, 0.75, na.rm = TRUE), 4),
        Max      = round(sapply(df_num, max,      na.rm = TRUE), 4),
        row.names = NULL
      )
      datatable(stats_tbl,
                options  = list(scrollX = TRUE, pageLength = 20),
                class    = "compact stripe hover",
                rownames = FALSE)
    })

    output$panel_overview <- renderPlotly({
      req(data_df(), dep_var(), q_var(), time_var())
      df <- data_df()
      if (!all(c(time_var(), dep_var(), q_var()) %in% names(df))) return(NULL)

      df_agg <- df |>
        dplyr::group_by(.data[[time_var()]]) |>
        dplyr::summarise(
          dep_mean = mean(.data[[dep_var()]], na.rm = TRUE),
          q_mean   = mean(.data[[q_var()]],  na.rm = TRUE),
          .groups  = "drop"
        )
      names(df_agg) <- c("period", "dep_mean", "q_mean")

      plot_ly(df_agg) |>
        add_lines(x = ~period, y = ~dep_mean,
                  name  = paste("Mean", dep_var()),
                  yaxis = "y",
                  line  = list(color = "#2c5282", width = 2)) |>
        add_lines(x = ~period, y = ~q_mean,
                  name  = paste("Mean", q_var()),
                  yaxis = "y2",
                  line  = list(color = "#d97706", width = 2, dash = "dot")) |>
        layout(
          title  = "Panel Means Over Time",
          xaxis  = list(title = time_var()),
          yaxis  = list(title = dep_var(), side = "left"),
          yaxis2 = list(title = q_var(),  side = "right",
                        overlaying = "y", showgrid = FALSE),
          legend = list(orientation = "h", y = -0.15)
        )
    })
  })
}

# ── 2. Model Results ───────────────────────────────────────────────────────

modelResultsUI <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      value_box(title    = "Threshold(s) / Buffer zones",
                value    = textOutput(ns("vb_thresh")),
                showcase = icon("cut"),         theme = "primary"),
      value_box(title    = "Observations",
                value    = textOutput(ns("vb_n")),
                showcase = icon("table"),       theme = "secondary"),
      value_box(title    = "Within R\u00b2",
                value    = textOutput(ns("vb_r2")),
                showcase = icon("percent"),     theme = "info"),
      value_box(title    = "Regime sizes",
                value    = textOutput(ns("vb_regime_sizes")),
                showcase = icon("layer-group"), theme = "success")
    ),
    card(
      card_header(
        "Coefficient Estimates by Regime",
        div(class = "float-end",
            selectInput(ns("table_style"),
                        label    = NULL,
                        choices  = c("default", "AER", "JEconometrics"),
                        selected = "default",
                        width    = "160px"))
      ),
      card_body(gt_output(ns("coef_table")))
    ),
    card(
      card_header("Coefficient Forest Plot"),
      card_body(plotlyOutput(ns("forest_plot"), height = "400px"))
    ),
    card(
      card_header("Model Summary"),
      card_body(verbatimTextOutput(ns("model_summary")))
    )
  )
}

modelResultsServer <- function(id, model, model_coefs, q_var) {
  moduleServer(id, function(input, output, session) {

    output$vb_thresh <- renderText({
      req(model())
      paste(round(model()$thresholds, 4), collapse = "  /  ")
    })
    output$vb_n <- renderText({
      req(model()); format(model()$n_obs, big.mark = ",")
    })
    output$vb_r2 <- renderText({
      req(model())
      sprintf("%.4f", 1 - model()$ssr / model()$tss_within)
    })
    output$vb_regime_sizes <- renderText({
      req(model()); paste(model()$n_obs_regime, collapse = " / ")
    })

    output$coef_table <- render_gt({
      req(model())
      style <- if (!is.null(input$table_style)) input$table_style else "default"
      bptr_table(model(),
                 style = style,
                 stars = TRUE,
                 title = paste("BTPD \u2014", model()$n_regimes, "Regimes"),
                 notes = paste("Threshold variable:", q_var(),
                               "| SE:", model()$se_type))
    })

    output$forest_plot <- renderPlotly({
      req(model())
      mc  <- model_coefs()
      cm  <- mc$cm
      sm  <- mc$sm
      n_reg       <- ncol(cm)
      n_var       <- nrow(cm)
      regime_cols <- c("#2c5282", "#198754", "#d97706")[seq_len(n_reg)]

      p <- plot_ly()
      for (r in seq_len(n_reg)) {
        p <- p |>
          add_markers(
            x       = cm[, r], y = rownames(cm),
            error_x = list(array = 1.96 * sm[, r], color = regime_cols[r]),
            marker  = list(color = regime_cols[r], size = 8),
            name    = paste("Regime", r)
          )
      }
      p |> add_lines(
        x = c(0, 0), y = c(0.5, n_var + 0.5),
        line = list(color = "grey", dash = "dot"), showlegend = FALSE
      ) |>
        layout(
          title  = "Coefficient Estimates with 95% CI",
          xaxis  = list(title = "Coefficient"),
          yaxis  = list(title = "", autorange = "reversed"),
          legend = list(orientation = "h", y = -0.15)
        )
    })

    output$model_summary <- renderPrint({
      req(model()); summary(model())
    })
  })
}

# ── 3. Sequential Test ─────────────────────────────────────────────────────

seqTestUI <- function(id) {
  ns <- NS(id)
  tagList(
    card(card_header("F1,2 \u2014 Linearity vs 2-Regime"),
         card_body(verbatimTextOutput(ns("test_12_output")))),
    card(card_header("F2,3 \u2014 2-Regime vs 3-Regime"),
         card_body(verbatimTextOutput(ns("test_23_output")))),
    card(card_header("Sequential Decision"),
         card_body(uiOutput(ns("seq_decision")))),
    card(card_header("Bootstrap Null Distribution (F1,2)"),
         card_body(plotlyOutput(ns("lr_null_plot"), height = "320px")))
  )
}

seqTestServer <- function(id, test_seq) {
  moduleServer(id, function(input, output, session) {

    output$test_12_output <- renderPrint({
      req(test_seq()); print(test_seq()$test_12)
    })

    output$test_23_output <- renderPrint({
      req(test_seq())
      if (!is.null(test_seq()$test_23)) {
        print(test_seq()$test_23)
      } else {
        cat(sprintf(
          "F2,3 test not run.\nF1,2 p-value (%.4f) >= alpha (%.2f): linearity not rejected.\n",
          test_seq()$test_12$p_value, test_seq()$alpha))
      }
    })

    output$seq_decision <- renderUI({
      req(test_seq())
      n      <- test_seq()$n_regimes_selected
      colour <- switch(as.character(n),
                       "1" = "warning", "2" = "success", "3" = "primary")
      label  <- switch(as.character(n),
                       "1" = "Linear model selected (linearity not rejected)",
                       "2" = "2-regime BTPD selected",
                       "3" = "3-regime BTPD selected")
      p12 <- sprintf("F1,2 p-value: %.4f", test_seq()$test_12$p_value)
      p23 <- if (!is.null(test_seq()$test_23))
        sprintf(" | F2,3 p-value: %.4f", test_seq()$test_23$p_value) else ""
      div(class = paste0("alert alert-", colour),
          icon("check-circle"), " ", strong(label), br(),
          small(p12, p23, sprintf(" (\u03b1 = %.2f)", test_seq()$alpha)))
    })

    output$lr_null_plot <- renderPlotly({
      req(test_seq())
      bs  <- test_seq()$test_12$boot_stats
      obs <- test_seq()$test_12$stat
      if (is.null(bs) || length(bs) == 0) return(NULL)
      plot_ly(x = ~bs, type = "histogram", nbinsx = 40,
              marker = list(color = "#6c757d", opacity = 0.7),
              name   = "Bootstrap null") |>
        add_lines(x = c(obs, obs), y = c(0, length(bs) / 5),
                  line = list(color = "red", width = 2),
                  name = sprintf("Observed F1,2 = %.3f", obs)) |>
        layout(title = "Bootstrap Distribution of F1,2 Under H\u2080 (Linearity)",
               xaxis = list(title = "F1,2 statistic"),
               yaxis = list(title = "Frequency"))
    })
  })
}

# ── 4. Regime Analysis ──────────────────────────────────────────────────────

regimeAnalysisUI <- function(id) {
  ns <- NS(id)
  tagList(
    card(
      card_header(
        "Threshold Variable \u2014 Regime Classification",
        div(class = "float-end",
            selectInput(ns("regime_plot_x"),
                        label    = NULL,
                        choices  = c("Observation index" = "obs",
                                     "Time variable"     = "time"),
                        selected = "time",
                        width    = "180px"))
      ),
      card_body(plotlyOutput(ns("regime_plot"), height = "480px"))
    ),
    card(
      card_header("Dependent Variable vs Threshold Variable by Regime"),
      card_body(plotlyOutput(ns("dep_vs_q_plot"), height = "380px"))
    ),
    card(
      card_header("Regime Composition Table"),
      card_body(DTOutput(ns("regime_table")))
    )
  )
}

regimeAnalysisServer <- function(id, model, data_df,
                                  id_var, time_var, dep_var, q_var) {
  moduleServer(id, function(input, output, session) {

    output$regime_plot <- renderPlotly({
      req(model(), data_df())
      df       <- data_df()
      use_time <- !is.null(input$regime_plot_x) &&
        input$regime_plot_x == "time" &&
        time_var() %in% names(df)

      df_plot <- data.frame(
        x_val  = if (use_time) df[[time_var()]] else seq_len(model()$n_obs),
        q_val  = df[[q_var()]],
        regime = factor(model()$regime_classification)
      )
      x_title <- if (use_time) time_var() else "Observation index"

      p <- plot_ly(df_plot, x = ~x_val, y = ~q_val, color = ~regime,
                   type = "scatter", mode = "markers",
                   marker = list(size = 4, opacity = 0.55)) |>
        layout(title  = paste("Threshold variable:", q_var()),
               xaxis  = list(title = x_title),
               yaxis  = list(title = q_var()),
               legend = list(title = list(text = "Regime")))

      thr  <- model()$thresholds
      lbls <- if (model()$buffer && length(thr) == 2) c("rL", "rU")
      else   if (model()$buffer && length(thr) == 4) c("rL1", "rU1", "rL2", "rU2")
      else   paste0("\u03b3", seq_along(thr))

      for (k in seq_along(thr)) {
        p <- p |> add_lines(
          x    = range(df_plot$x_val),
          y    = rep(thr[k], 2),
          line = list(color = "black", dash = "dash", width = 1.5),
          name = lbls[k], showlegend = TRUE
        )
      }
      p
    })

    output$dep_vs_q_plot <- renderPlotly({
      req(model(), data_df())
      df      <- data_df()
      df_plot <- data.frame(
        y_val  = df[[dep_var()]],
        x_val  = df[[q_var()]],
        regime = factor(model()$regime_classification)
      )
      plot_ly(df_plot, x = ~x_val, y = ~y_val, color = ~regime,
              type = "scatter", mode = "markers",
              marker = list(size = 4, opacity = 0.5)) |>
        layout(title  = paste(dep_var(), "vs", q_var()),
               xaxis  = list(title = q_var()),
               yaxis  = list(title = dep_var()),
               legend = list(title = list(text = "Regime")))
    })

    output$regime_table <- renderDT({
      req(model(), data_df())
      df   <- data_df()
      keep <- intersect(c(id_var(), time_var(), q_var()), names(df))
      df_aug <- cbind(
        df[, keep, drop = FALSE],
        Regime = model()$regime_classification,
        Fitted = round(model()$fitted_values, 4),
        Resid  = round(model()$residuals,     4)
      )
      datatable(df_aug,
                options  = list(scrollX = TRUE, pageLength = 15),
                class    = "compact stripe hover",
                rownames = FALSE)
    })
  })
}

# ── 5. Bootstrap CI ────────────────────────────────────────────────────────

bootstrapUI <- function(id) {
  ns <- NS(id)
  tagList(
    card(
      card_header("Bootstrap Distribution of Threshold Parameter(s)"),
      card_body(plotlyOutput(ns("bootstrap_plot"), height = "380px"))
    ),
    card(
      card_header("95% Percentile Confidence Intervals"),
      card_body(verbatimTextOutput(ns("bootstrap_summary")))
    )
  )
}

bootstrapServer <- function(id, boot_result, model) {
  moduleServer(id, function(input, output, session) {

    output$bootstrap_plot <- renderPlotly({
      req(boot_result())
      gb <- boot_result()$gamma_boot
      if (!is.matrix(gb)) gb <- matrix(gb, ncol = 1)

      plots <- lapply(seq_len(ncol(gb)), function(k) {
        obs_val <- model()$thresholds[k]
        plot_ly(x = ~gb[, k], type = "histogram", nbinsx = 40,
                name   = paste0("\u03b3", k),
                marker = list(opacity = 0.7)) |>
          add_lines(x = c(obs_val, obs_val), y = c(0, nrow(gb) / 5),
                    line = list(color = "red", width = 2),
                    name = sprintf("Point est. = %.4f", obs_val)) |>
          layout(xaxis = list(title = paste0("Bootstrap \u03b3", k)))
      })

      if (ncol(gb) == 1) plots[[1]] else
        subplot(plots, nrows = 1, shareY = FALSE, titleX = TRUE)
    })

    output$bootstrap_summary <- renderPrint({
      req(boot_result()); print(boot_result())
    })
  })
}

# ── 6. Model Comparison ────────────────────────────────────────────────────

comparisonUI <- function(id) {
  ns <- NS(id)
  card(
    card_header("PTR vs BTPD \u2014 Fit Statistics"),
    card_body(
      p("Click below to fit the alternative model and compare."),
      actionButton(ns("run_comparison"), "Fit alternative model",
                   class = "btn-outline-primary",
                   icon  = icon("exchange-alt")),
      br(), br(),
      gt_output(ns("comparison_table"))
    )
  )
}

comparisonServer <- function(id, model, data_df,
                              id_var, time_var, q_var,
                              trim_frac, grid_size, grid_size_3, se_type) {
  moduleServer(id, function(input, output, session) {

    model_alt <- reactiveVal(NULL)

    observeEvent(input$run_comparison, {
      req(model(), data_df())
      tryCatch({
        fml        <- model()$formula
        df         <- data_df()
        alt_buffer <- !model()$buffer
        model_alt(bptr(
          formula     = fml, data = df,
          id          = id_var(),  time = time_var(),
          q           = q_var(),
          n_thresh    = model()$n_thresh,
          buffer      = alt_buffer,
          trim        = trim_frac(),
          grid_size   = grid_size(),
          grid_size_3 = grid_size_3(),
          se_type     = se_type()
        ))
        showNotification("Alternative model fitted.", type = "message")
      }, error = function(e) {
        showNotification(paste("Comparison error:", e$message), type = "error")
      })
    })

    output$comparison_table <- render_gt({
      req(model(), model_alt())

      make_row <- function(m, label) {
        data.frame(
          Model      = label,
          Type       = if (m$buffer) "BTPD (buffered)" else "PTR (abrupt)",
          Regimes    = m$n_regimes,
          SSR        = round(m$ssr, 4),
          Within_R2  = round(1 - m$ssr / m$tss_within, 4),
          AIC_approx = round(m$n_obs * log(m$ssr / m$n_obs) +
                               2 * (length(m$thresholds) +
                                    length(as.vector(m$coefficients))), 2),
          stringsAsFactors = FALSE, check.names = FALSE
        )
      }

      tbl <- rbind(
        make_row(model(),     "Current model"),
        make_row(model_alt(), "Alternative model")
      )
      colnames(tbl)[5] <- "Within R\u00b2"

      gt::gt(tbl) |>
        gt::tab_header(title = "Model Comparison: PTR vs BTPD") |>
        gt::tab_style(
          style     = gt::cell_text(weight = "bold"),
          locations = gt::cells_column_labels()
        ) |>
        gt::tab_footnote("Lower SSR and AIC indicates better fit.")
    })
  })
}

# ── 7. Diagnostics ─────────────────────────────────────────────────────────

diagnosticsUI <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(6, 6),
      card(card_header("Residuals vs Fitted"),
           card_body(plotlyOutput(ns("diag_resid_fitted"), height = "350px"))),
      card(card_header("Normal Q-Q Plot"),
           card_body(plotlyOutput(ns("diag_qq"),           height = "350px")))
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(card_header("Residual Distribution"),
           card_body(plotlyOutput(ns("diag_hist"), height = "350px"))),
      card(card_header("Residual ACF"),
           card_body(plotlyOutput(ns("diag_acf"),  height = "350px")))
    )
  )
}

diagnosticsServer <- function(id, model) {
  moduleServer(id, function(input, output, session) {

    output$diag_resid_fitted <- renderPlotly({
      req(model())
      df_d <- data.frame(fitted = model()$fitted_values,
                         resid  = model()$residuals,
                         regime = factor(model()$regime_classification))
      plot_ly(df_d, x = ~fitted, y = ~resid, color = ~regime,
              type = "scatter", mode = "markers",
              marker = list(size = 4, opacity = 0.55)) |>
        add_segments(x = min(df_d$fitted), xend = max(df_d$fitted),
                     y = 0, yend = 0,
                     line = list(color = "grey40", dash = "dash", width = 1),
                     showlegend = FALSE) |>
        layout(title = "Residuals vs Fitted",
               xaxis = list(title = "Fitted values"),
               yaxis = list(title = "Residuals"))
    })

    output$diag_qq <- renderPlotly({
      req(model())
      qq  <- stats::qqnorm(model()$residuals, plot.it = FALSE)
      rng <- range(c(qq$x, qq$y))
      plot_ly(x = ~qq$x, y = ~qq$y, type = "scatter", mode = "markers",
              marker = list(size = 4, color = "#2c5282", opacity = 0.6),
              name = "Residuals") |>
        add_lines(x = rng, y = rng,
                  line = list(color = "red", dash = "dot"),
                  name = "45\u00b0 line") |>
        layout(title = "Normal Q-Q Plot",
               xaxis = list(title = "Theoretical quantiles"),
               yaxis = list(title = "Sample quantiles"))
    })

    output$diag_hist <- renderPlotly({
      req(model())
      r <- model()$residuals
      plot_ly(x = ~r, type = "histogram", nbinsx = 40,
              marker = list(color = "#2c5282", opacity = 0.7)) |>
        layout(title = "Residual Distribution",
               xaxis = list(title = "Residuals"),
               yaxis = list(title = "Count"))
    })

    output$diag_acf <- renderPlotly({
      req(model())
      ac       <- stats::acf(model()$residuals, plot = FALSE, lag.max = 30)
      lag_vals <- as.numeric(ac$lag)
      acf_vals <- as.numeric(ac$acf)
      ci       <- 1.96 / sqrt(length(model()$residuals))
      plot_ly() |>
        add_segments(x = lag_vals, xend = lag_vals,
                     y = rep(0, length(lag_vals)), yend = acf_vals,
                     line = list(color = "#2c5282", width = 2),
                     showlegend = FALSE) |>
        add_lines(x = range(lag_vals), y = c(ci,  ci),
                  line = list(color = "red", dash = "dash"),
                  name = "+95% CI") |>
        add_lines(x = range(lag_vals), y = c(-ci, -ci),
                  line = list(color = "red", dash = "dash"),
                  name = "-95% CI") |>
        layout(title = "ACF of Residuals",
               xaxis = list(title = "Lag"),
               yaxis = list(title = "ACF"))
    })
  })
}

# ══════════════════════════════════════════════════════════════════════════════
# UI
# ══════════════════════════════════════════════════════════════════════════════

ui <- page_sidebar(
  title = tagList(
    span("bufferedThresholdPanel",
         style = "font-weight:700; color:#2c5282;"),
    span(" \u2014 Buffered Panel Threshold Regression",
         style = "font-weight:300; opacity:0.8;")
  ),
  theme = app_theme,

  # ── Sidebar ──────────────────────────────────────────────────────────────
  sidebar = sidebar(
    width = 340,

    card(
      card_header(icon("upload"), " Data",
                  class = "d-flex align-items-center"),
      card_body(
        fileInput("data_file", label = NULL,
                  accept      = c(".csv", ".xlsx", ".xls"),
                  placeholder = "Upload panel data (CSV / XLSX)..."),
        actionButton("use_example",
                     "Use package example data (panel_data)",
                     class = "btn-outline-secondary btn-sm w-100"),
        conditionalPanel(
          condition = "output.data_loaded",
          div(class = "text-success small mt-1",
              icon("check-circle"), " Data loaded successfully")
        )
      )
    ),

    conditionalPanel(
      condition = "output.data_loaded",

      card(
        card_header(icon("sliders-h"), " Variable Selection",
                    class = "d-flex align-items-center"),
        card_body(
          selectInput("id_var",   "Panel ID Variable",  choices = NULL, width = "100%"),
          selectInput("time_var", "Time Variable",       choices = NULL, width = "100%"),
          selectInput("dep_var",  "Dependent Variable", choices = NULL, width = "100%"),
          selectizeInput("reg_vars", "Regressors",
                         choices  = NULL, multiple = TRUE, width = "100%",
                         options  = list(placeholder = "Select one or more regressors")),
          selectInput("q_var", "Threshold Variable",    choices = NULL, width = "100%"),
          uiOutput("var_warning")
        )
      ),

      card(
        card_header(icon("cogs"), " Model Specification",
                    class = "d-flex align-items-center"),
        card_body(
          radioButtons("model_type", "Model Type",
                       choices  = list(
                         "Standard PTR (abrupt transition)" = "ptr",
                         "Buffered BTPD (hysteresis)"       = "bptr"
                       ),
                       selected = "bptr"),
          selectInput("n_thresh",
                      "Number of Thresholds / Regimes",
                      choices  = list("1 threshold \u2192 2 regimes" = 1,
                                      "2 thresholds \u2192 3 regimes" = 2),
                      selected = 1),
          sliderInput("grid_size",
                      "Grid Size (2-regime search)",
                      min = 50, max = 500, value = 300, step = 50),
          conditionalPanel(
            condition = "input.n_thresh == '2'",
            sliderInput("grid_size_3",
                        "Grid Size (3-regime BTPD 4-D search)",
                        min = 10, max = 100, value = 30, step = 5)
          ),
          selectInput("se_type", "Robust SE Type",
                      choices  = c("HC3", "HC2", "HC1", "HC0"),
                      selected = "HC3"),
          sliderInput("trim_frac", "Trimming Fraction",
                      min = 0.05, max = 0.30, value = 0.15, step = 0.05)
        )
      ),

      card(
        card_header(icon("random"), " Bootstrap / Test",
                    class = "d-flex align-items-center"),
        card_body(
          numericInput("n_boot", "Bootstrap Replications",
                       value = 199, min = 49, max = 1999, step = 50),
          numericInput("boot_seed", "Random Seed", value = 42),
          selectInput("alpha_level",
                      "Sequential Test Significance Level (\u03b1)",
                      choices  = c("0.01" = 0.01, "0.05" = 0.05, "0.10" = 0.10),
                      selected = 0.10),
          checkboxInput("run_seq_test",
                        "Run sequential test (F1,2 \u2192 F2,3)",
                        value = TRUE),
          conditionalPanel(
            condition = "input.n_thresh == '1'",
            checkboxInput("run_bootstrap_ci",
                          "Bootstrap confidence intervals",
                          value = FALSE)
          )
        )
      ),

      div(
        class = "d-grid gap-2",
        actionButton("run_model", "  Estimate Model",
                     class = "btn-primary btn-lg",
                     icon  = icon("play")),
        conditionalPanel(
          condition = "output.model_estimated",
          downloadButton("dl_results", "  Download Results (XLSX)",
                         class = "btn-outline-secondary",
                         icon  = icon("download")),
          downloadButton("dl_latex", "  Export LaTeX Table",
                         class = "btn-outline-secondary",
                         icon  = icon("file-alt"))
        )
      )
    )
  ),

  # ── Main panel ───────────────────────────────────────────────────────────
  navset_card_tab(

    nav_panel("Data Preview",   icon = icon("table"),
              dataPreviewUI("data_preview")),

    nav_panel("Model Results",  icon = icon("chart-line"),
              modelResultsUI("model_results")),

    nav_panel("Sequential Test", icon = icon("flask"),
              seqTestUI("seq_test")),

    nav_panel("Regime Analysis", icon = icon("layer-group"),
              regimeAnalysisUI("regime_analysis")),

    nav_panel("Bootstrap CI",   icon = icon("random"),
              bootstrapUI("bootstrap")),

    nav_panel("Model Comparison", icon = icon("balance-scale"),
              comparisonUI("comparison")),

    nav_panel("Diagnostics",    icon = icon("stethoscope"),
              diagnosticsUI("diagnostics"))
  )
)

# ══════════════════════════════════════════════════════════════════════════════
# SERVER
# ══════════════════════════════════════════════════════════════════════════════

server <- function(input, output, session) {

  rv <- reactiveValues(
    data        = NULL,
    model       = NULL,
    test_seq    = NULL,
    boot_result = NULL
  )

  # ── Precomputed shared reactives ─────────────────────────────────────────

  data_df <- reactive({
    req(rv$data)
    as.data.frame(rv$data)
  })

  model_r <- reactive(rv$model)

  model_coefs <- reactive({
    req(rv$model)
    cm <- rv$model$coefficients
    sm <- rv$model$std_errors
    df_r <- rv$model$df_residual
    t_m  <- cm / sm
    p_m  <- 2 * stats::pt(-abs(t_m), df = df_r)
    list(cm = cm, sm = sm, df_r = df_r, t_m = t_m, p_m = p_m)
  })

  test_seq_r  <- reactive(rv$test_seq)
  boot_result_r <- reactive(rv$boot_result)

  # ── Sidebar input wrappers (reactive, for module arguments) ──────────────
  r_id_var      <- reactive(input$id_var)
  r_time_var    <- reactive(input$time_var)
  r_dep_var     <- reactive(input$dep_var)
  r_q_var       <- reactive(input$q_var)
  r_trim_frac   <- reactive(input$trim_frac)
  r_grid_size   <- reactive(input$grid_size)
  r_grid_size_3 <- reactive({
    if (!is.null(input$grid_size_3)) input$grid_size_3 else 30L
  })
  r_se_type     <- reactive(input$se_type)

  # ── Module servers ───────────────────────────────────────────────────────

  dataPreviewServer("data_preview",
    data_df  = data_df,
    dep_var  = r_dep_var,
    q_var    = r_q_var,
    time_var = r_time_var
  )

  modelResultsServer("model_results",
    model       = model_r,
    model_coefs = model_coefs,
    q_var       = r_q_var
  )

  seqTestServer("seq_test",
    test_seq = test_seq_r
  )

  regimeAnalysisServer("regime_analysis",
    model    = model_r,
    data_df  = data_df,
    id_var   = r_id_var,
    time_var = r_time_var,
    dep_var  = r_dep_var,
    q_var    = r_q_var
  )

  bootstrapServer("bootstrap",
    boot_result = boot_result_r,
    model       = model_r
  )

  comparisonServer("comparison",
    model       = model_r,
    data_df     = data_df,
    id_var      = r_id_var,
    time_var    = r_time_var,
    q_var       = r_q_var,
    trim_frac   = r_trim_frac,
    grid_size   = r_grid_size,
    grid_size_3 = r_grid_size_3,
    se_type     = r_se_type
  )

  diagnosticsServer("diagnostics",
    model = model_r
  )

  # ── Status flags for conditionalPanel ───────────────────────────────────
  output$data_loaded <- reactive(!is.null(rv$data))
  outputOptions(output, "data_loaded", suspendWhenHidden = FALSE)

  output$model_estimated <- reactive(!is.null(rv$model))
  outputOptions(output, "model_estimated", suspendWhenHidden = FALSE)

  # ── Variable warning ────────────────────────────────────────────────────
  output$var_warning <- renderUI({
    req(input$q_var, input$reg_vars)
    valid_swap <- (input$q_var == "oilRentGDP" && "rle"        %in% input$reg_vars) ||
                  (input$q_var == "rle"        && "oilRentGDP" %in% input$reg_vars)
    if (!valid_swap && input$q_var %in% input$reg_vars)
      div(class = "alert alert-warning p-1 mt-1 small",
          icon("exclamation-triangle"),
          " Threshold variable is also selected as a regressor.")
  })

  # ── Helper: populate variable dropdowns ─────────────────────────────────
  populate_vars <- function(df) {
    nms <- names(df)
    updateSelectInput(session,    "id_var",   choices = nms, selected = nms[1])
    updateSelectInput(session,    "time_var", choices = nms, selected = nms[2])
    updateSelectInput(session,    "dep_var",  choices = nms, selected = nms[3])
    updateSelectizeInput(session, "reg_vars", choices = nms,
                         selected = nms[seq(4, min(9, length(nms)))])
    updateSelectInput(session,    "q_var",    choices = nms, selected = nms[4])
  }

  # ── Data: file upload ────────────────────────────────────────────────────
  observeEvent(input$data_file, {
    req(input$data_file)
    ext <- tools::file_ext(input$data_file$name)
    tryCatch({
      rv$data <- if (ext %in% c("xlsx", "xls"))
        readxl::read_excel(input$data_file$datapath)
      else if (ext == "csv")
        readr::read_csv(input$data_file$datapath, show_col_types = FALSE)
      else
        stop("Unsupported format. Please use CSV or XLSX.")
      populate_vars(rv$data)
      showNotification("Data loaded successfully!", type = "message")
    }, error = function(e) {
      showNotification(paste("Load error:", e$message), type = "error")
    })
  })

  # ── Data: example button ─────────────────────────────────────────────────
  observeEvent(input$use_example, {
    tryCatch({
      pd      <- get("panel_data", envir = asNamespace("bufferedThresholdPanel"))
      rv$data <- pd
      populate_vars(rv$data)
      updateSelectInput(session, "id_var",   selected = "countryId")
      updateSelectInput(session, "time_var", selected = "year")
      updateSelectInput(session, "dep_var",  selected = "growthRate")
      updateSelectizeInput(session, "reg_vars",
                           selected = c("rle", "eci", "initialGDP", "fdiGDP",
                                        "capFormGDP", "inflation",
                                        "popGrowth", "indVAGDP", "tradeOpenness"))
      updateSelectInput(session, "q_var", selected = "oilRentGDP")
      showNotification("panel_data loaded: 92 countries, 2002\u20132016.",
                       type = "message")
    }, error = function(e) {
      showNotification(paste("Could not load panel_data:", e$message), type = "error")
    })
  })

  # ── Model estimation ─────────────────────────────────────────────────────
  observeEvent(input$run_model, {
    req(data_df(), input$id_var, input$time_var, input$dep_var,
        input$reg_vars, input$q_var)

    if (input$dep_var %in% input$reg_vars) {
      showNotification("Dependent variable cannot also be a regressor.",
                       type = "error"); return()
    }
    if (length(input$reg_vars) == 0) {
      showNotification("Please select at least one regressor.",
                       type = "error"); return()
    }

    withProgress(message = "Estimating model...", value = 0, {
      tryCatch({
        fml   <- as.formula(paste(input$dep_var, "~",
                                  paste(input$reg_vars, collapse = " + ")))
        df    <- data_df()
        alpha <- as.numeric(input$alpha_level)
        g3    <- if (as.integer(input$n_thresh) == 2) input$grid_size_3 else 30L

        incProgress(0.2, detail = "Grid search for optimal threshold(s)...")
        rv$model <- bptr(
          formula     = fml, data = df,
          id          = input$id_var, time = input$time_var,
          q           = input$q_var,
          n_thresh    = as.integer(input$n_thresh),
          buffer      = (input$model_type == "bptr"),
          trim        = input$trim_frac,
          grid_size   = input$grid_size,
          grid_size_3 = g3,
          se_type     = input$se_type
        )

        if (input$run_seq_test) {
          incProgress(0.5, detail = "F1,2 bootstrap test...")

          fit_2reg <- if (as.integer(input$n_thresh) == 1) rv$model else
            bptr(formula = fml, data = df,
                 id = input$id_var, time = input$time_var,
                 q  = input$q_var, n_thresh = 1L,
                 buffer    = (input$model_type == "bptr"),
                 trim      = input$trim_frac,
                 grid_size = input$grid_size,
                 se_type   = input$se_type)

          test_12 <- bptr_test(
            formula   = fml, data = df,
            id        = input$id_var, time = input$time_var,
            q         = input$q_var,
            buffer    = (input$model_type == "bptr"),
            n_boot    = input$n_boot,
            trim      = input$trim_frac,
            grid_size = input$grid_size,
            seed      = input$boot_seed
          )

          test_23     <- NULL
          final_model <- fit_2reg
          n_sel       <- 1L

          if (test_12$p_value < alpha) {
            incProgress(0.7, detail = "F2,3 bootstrap test (3-regime)...")
            test_23 <- bptr_test_23(
              fit_2reg    = fit_2reg,
              n_boot      = input$n_boot,
              grid_size_3 = g3,
              seed        = input$boot_seed
            )
            if (test_23$p_value < alpha) {
              final_model <- test_23$model_3reg
              n_sel       <- 3L
            } else {
              final_model <- fit_2reg
              n_sel       <- 2L
            }
          }

          rv$test_seq <- structure(
            list(test_12            = test_12,
                 test_23            = test_23,
                 final_model        = final_model,
                 n_regimes_selected = n_sel,
                 alpha              = alpha),
            class = "bptr_test_seq"
          )
        }

        if (input$run_bootstrap_ci && as.integer(input$n_thresh) == 1) {
          incProgress(0.85, detail = "Bootstrap confidence intervals...")
          rv$boot_result <- bptr_bootstrap(
            rv$model, n_boot = input$n_boot, seed = input$boot_seed
          )
        }

        incProgress(1.0, detail = "Done!")
        showNotification("Estimation complete.", type = "message", duration = 3)

      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error", duration = 10)
      })
    })
  })

  # ── Download: XLSX ────────────────────────────────────────────────────────
  output$dl_results <- downloadHandler(
    filename = function() paste0("btpd_results_", Sys.Date(), ".xlsx"),
    content  = function(file) {
      req(rv$model)
      mc <- model_coefs()

      coef_sheet <- data.frame(
        Variable  = rep(rownames(mc$cm), ncol(mc$cm)),
        Regime    = rep(paste0("Regime ", seq_len(ncol(mc$cm))), each = nrow(mc$cm)),
        Estimate  = as.vector(mc$cm),
        Std.Error = as.vector(mc$sm),
        t.value   = as.vector(mc$t_m),
        p.value   = as.vector(mc$p_m),
        Stars     = as.vector(sig_stars(mc$p_m))
      )

      thresh_sheet <- data.frame(
        Parameter = if (rv$model$buffer && rv$model$n_thresh == 1)
          c("rL", "rU")
        else if (rv$model$buffer && rv$model$n_thresh == 2)
          c("rL1", "rU1", "rL2", "rU2")
        else
          paste0("gamma", seq_along(rv$model$thresholds)),
        Estimate  = rv$model$thresholds
      )

      fit_sheet <- data.frame(
        Obs    = seq_len(rv$model$n_obs),
        Regime = rv$model$regime_classification,
        Fitted = rv$model$fitted_values,
        Resid  = rv$model$residuals
      )

      sheets <- list(Coefficients = coef_sheet,
                     Thresholds   = thresh_sheet,
                     Fitted       = fit_sheet)

      if (!is.null(rv$test_seq)) {
        t12 <- rv$test_seq$test_12
        t23 <- rv$test_seq$test_23
        test_sheet <- data.frame(
          Test      = c("F1,2 (linearity vs 2-regime)",
                        if (!is.null(t23)) "F2,3 (2-regime vs 3-regime)" else NULL),
          Statistic = c(round(t12$stat, 4),
                        if (!is.null(t23)) round(t23$stat, 4) else NULL),
          p.value   = c(round(t12$p_value, 4),
                        if (!is.null(t23)) round(t23$p_value, 4) else NULL),
          n_boot    = c(t12$n_boot,
                        if (!is.null(t23)) t23$n_boot else NULL),
          Decision  = c(
            ifelse(t12$p_value < rv$test_seq$alpha, "Reject linearity", "Accept linearity"),
            if (!is.null(t23))
              ifelse(t23$p_value < rv$test_seq$alpha, "Reject 2-regime", "Accept 2-regime")
            else NULL
          )
        )
        sheets$SequentialTests <- test_sheet
      }

      if (requireNamespace("writexl", quietly = TRUE)) {
        writexl::write_xlsx(sheets, path = file)
      } else {
        writeLines(
          "writexl required. Install with: install.packages('writexl')",
          con = file
        )
      }
    }
  )

  # ── Download: LaTeX ───────────────────────────────────────────────────────
  output$dl_latex <- downloadHandler(
    filename = function() paste0("btpd_table_", Sys.Date(), ".tex"),
    content  = function(file) {
      req(rv$model)
      bptr_latex(rv$model, file = file,
                 style = "default", stars = TRUE)
    }
  )
}

shinyApp(ui = ui, server = server)
