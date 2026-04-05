mod_codynamics_ui <- function(id) {
  shiny::uiOutput(shiny::NS(id, "page"))
}

mod_codynamics_server <- function(id, filters, data_timestamp) {
  shiny::moduleServer(id, function(input, output, session) {
    page_data <- shiny::reactive(ea_calc_codynamics(filters()))

    mod_footer_notes_server(
      "footer_notes",
      notes = shiny::reactive(list(
        notes = page_data()$notes,
        assumptions = page_data()$assumptions,
        timestamp = data_timestamp()
      )),
      title = "Correlation notes"
    )

    output$page <- shiny::renderUI({
      ns <- session$ns
      current_filters <- filters()

      if (length(current_filters$commodities) == 0L) {
        return(htmltools::tagList(
          ea_selected_filters_ribbon(current_filters, data_timestamp()),
          ea_empty_state_card(
            title = "Select products to populate correlation views",
            body = "Co-dynamics aggregates rolling correlation, intercommodity spreads, PCA structure, and transmission diagnostics across the selected products.",
            hint = "Add at least two products or keep a benchmark product active."
          ),
          mod_footer_notes_ui(ns("footer_notes"))
        ))
      }

      htmltools::tagList(
        ea_selected_filters_ribbon(current_filters, data_timestamp()),
        mod_kpi_strip_ui(ns("kpi_strip")),
        # Row 1: Rolling Correlation | Correlation Matrix Heatmap
        bslib::layout_columns(
          col_widths = c(6, 6),
          ea_plotly_card(
            title = "Rolling Correlation with CI Bands",
            subtitle = "Sliding-window correlation with 2-sigma confidence bands.",
            output_id = ns("rolling_corr"),
            height = "300px"
          ),
          ea_plotly_card(
            title = "Correlation Matrix",
            subtitle = "Current-window pairwise dependencies.",
            output_id = ns("corr_heatmap"),
            height = "300px"
          )
        ),
        # Row 2: PCA Factor Structure | Treasury Factor Sensitivity
        bslib::layout_columns(
          col_widths = c(6, 6),
          ea_plotly_card(
            title = "PCA Factor Structure",
            subtitle = "Principal component loadings per market.",
            output_id = ns("pca_bars"),
            height = "300px"
          ),
          ea_plotly_card(
            title = "Treasury Factor Sensitivity",
            subtitle = "OLS betas on UST level, slope, and curvature changes.",
            output_id = ns("treasury_bars"),
            height = "300px"
          )
        ),
        # Row 3: Spread Time Series | Spread Z-Score Dashboard
        bslib::layout_columns(
          col_widths = c(7, 5),
          ea_plotly_card(
            title = "Spread Time Series",
            subtitle = "Price dislocation across active spread definitions.",
            output_id = ns("spread_ts"),
            height = "340px"
          ),
          ea_plotly_card(
            title = "Spread Z-Score Dashboard",
            subtitle = "Normalised spread extremity sorted by magnitude.",
            output_id = ns("spread_zscore"),
            height = "340px"
          )
        ),
        # Row 4: Rolling Beta Dynamics | Cointegration Residual with OU Bands
        bslib::layout_columns(
          col_widths = c(6, 6),
          ea_plotly_card(
            title = "Rolling Beta Dynamics",
            subtitle = "Sliding-window OLS beta per market pair.",
            output_id = ns("rolling_beta"),
            height = "300px"
          ),
          ea_plotly_card(
            title = "Cointegration Residual",
            subtitle = "OU-process mean-reversion bands on regression residual.",
            output_id = ns("coint_residual"),
            height = "300px"
          )
        ),
        mod_footer_notes_ui(ns("footer_notes"))
      )
    })

    mod_kpi_strip_server("kpi_strip", kpis = shiny::reactive(page_data()$kpis))

    # ---- Rolling Correlation with CI Bands ----
    output$rolling_corr <- plotly::renderPlotly({
      ct <- page_data()$correlation_timeseries
      palette <- c("#4da3a3", "#5a85c8", "#7f8b99", "#d2a157", "#c46e73", "#8ecf8e")

      fig <- plotly::plot_ly()

      for (i in seq_along(unique(ct$pair))) {
        pair_name <- unique(ct$pair)[[i]]
        df <- ct[ct$pair == pair_name, , drop = FALSE]
        col <- palette[[((i - 1L) %% length(palette)) + 1L]]

        fig <- fig |>
          plotly::add_ribbons(
            data = df,
            x = ~date,
            ymin = ~ci_lo,
            ymax = ~ci_hi,
            name = paste0(pair_name, " CI"),
            line = list(color = "transparent"),
            fillcolor = paste0(
              substr(col, 1, 7),
              "33"
            ),
            showlegend = FALSE,
            hoverinfo = "skip"
          ) |>
          plotly::add_lines(
            data = df,
            x = ~date,
            y = ~correlation,
            name = pair_name,
            line = list(color = col, width = 1.5),
            hovertemplate = "%{x|%d %b %Y}<br>%{y:.2f}<extra>%{fullData.name}</extra>"
          )
      }

      ea_plotly_layout(fig, x_title = NULL, y_title = "Correlation")
    })

    # ---- Correlation Matrix Heatmap ----
    output$corr_heatmap <- plotly::renderPlotly({
      cm <- page_data()$correlation_matrix
      dep_matrix <- stats::xtabs(correlation ~ market_y + market_x, data = cm)

      fig <- plotly::plot_ly(
        x = colnames(dep_matrix),
        y = rownames(dep_matrix),
        z = unclass(dep_matrix),
        type = "heatmap",
        colors = c("#b35c60", "#17202b", "#4da3a3"),
        showscale = FALSE,
        hovertemplate = "%{y} vs %{x}<br>%{z:.2f}<extra>correlation</extra>"
      ) |>
        plotly::layout(yaxis = list(autorange = "reversed"))

      ea_plotly_layout(fig, x_title = NULL, y_title = NULL, hovermode = "closest")
    })

    # ---- PCA Factor Structure ----
    output$pca_bars <- plotly::renderPlotly({
      pca <- page_data()$pca_decomposition
      pc_colors <- c("#4da3a3", "#5a85c8", "#d2a157")

      fig <- plotly::plot_ly()

      for (i in seq_len(3)) {
        pc_col <- paste0("PC", i)
        if (!pc_col %in% names(pca)) next
        fig <- fig |>
          plotly::add_bars(
            data = pca,
            x = ~market,
            y = stats::as.formula(paste0("~", pc_col)),
            name = pc_col,
            marker = list(color = pc_colors[[i]]),
            hovertemplate = paste0("%{x}<br>", pc_col, ": %{y:.3f}<extra></extra>")
          )
      }

      fig <- fig |>
        plotly::layout(barmode = "group")

      ea_plotly_layout(fig, x_title = NULL, y_title = "Loading")
    })

    # ---- Treasury Factor Sensitivity ----
    output$treasury_bars <- plotly::renderPlotly({
      tb <- page_data()$treasury_betas
      factor_colors <- c(
        "Level" = "#4da3a3",
        "Slope" = "#5a85c8",
        "Curvature" = "#d2a157"
      )

      fig <- plotly::plot_ly()

      for (fac in c("Level", "Slope", "Curvature")) {
        df_f <- tb[tb$factor == fac, , drop = FALSE]
        if (nrow(df_f) == 0L) next
        fig <- fig |>
          plotly::add_bars(
            data = df_f,
            x = ~market,
            y = ~beta,
            name = fac,
            marker = list(color = factor_colors[[fac]]),
            hovertemplate = paste0("%{x}<br>", fac, " beta: %{y:.4f}<extra></extra>")
          )
      }

      fig <- fig |>
        plotly::layout(barmode = "group")

      ea_plotly_layout(fig, x_title = NULL, y_title = "Beta")
    })

    # ---- Spread Time Series (faceted subplots) ----
    output$spread_ts <- plotly::renderPlotly({
      st <- page_data()$spread_timeseries
      spread_labels <- unique(st$spread_label)

      if (length(spread_labels) == 0L) {
        fig <- plotly::plot_ly() |>
          plotly::add_annotations(text = "No spread data available", showarrow = FALSE)
        return(ea_plotly_layout(fig, x_title = NULL, y_title = NULL))
      }

      palette <- c("#4da3a3", "#5a85c8", "#d2a157")
      sub_figs <- lapply(seq_along(spread_labels), function(i) {
        lbl <- spread_labels[[i]]
        df_sp <- st[st$spread_label == lbl, , drop = FALSE]
        col <- palette[[((i - 1L) %% length(palette)) + 1L]]
        plotly::plot_ly(
          data = df_sp,
          x = ~date,
          y = ~value,
          type = "scatter",
          mode = "lines",
          name = lbl,
          line = list(color = col, width = 1.8),
          hovertemplate = paste0(lbl, "<br>%{x|%d %b %Y}<br>%{y:.2f}<extra></extra>")
        )
      })

      fig <- plotly::subplot(sub_figs, nrows = length(spread_labels), shareX = TRUE, titleX = FALSE)
      ea_plotly_layout(fig, x_title = NULL, y_title = NULL)
    })

    # ---- Spread Z-Score Dashboard ----
    output$spread_zscore <- plotly::renderPlotly({
      sz <- page_data()$spread_zscore

      if (nrow(sz) == 0L) {
        fig <- plotly::plot_ly() |>
          plotly::add_annotations(text = "No spread data available", showarrow = FALSE)
        return(ea_plotly_layout(fig, x_title = NULL, y_title = NULL))
      }

      sz <- sz[order(abs(sz$zscore), decreasing = FALSE), , drop = FALSE]
      bar_colors <- ifelse(sz$zscore >= 0, "#4da3a3", "#b35c60")

      fig <- plotly::plot_ly(
        data = sz,
        x = ~zscore,
        y = ~spread_label,
        type = "bar",
        orientation = "h",
        marker = list(color = bar_colors),
        hovertemplate = "%{y}<br>Z-score: %{x:.2f}<extra></extra>"
      ) |>
        plotly::layout(shapes = list(
          list(type = "line", x0 = 0, x1 = 0, y0 = -0.5, y1 = nrow(sz) - 0.5,
               line = list(color = "#7f8b99", width = 1, dash = "dot"))
        ))

      ea_plotly_layout(fig, x_title = "Z-Score", y_title = NULL)
    })

    # ---- Rolling Beta Dynamics ----
    output$rolling_beta <- plotly::renderPlotly({
      rbt <- page_data()$rolling_beta_ts
      palette <- c("#4da3a3", "#5a85c8", "#7f8b99", "#d2a157", "#c46e73", "#8ecf8e")

      fig <- plotly::plot_ly()

      if (nrow(rbt) == 0L) {
        fig <- fig |>
          plotly::add_annotations(text = "Insufficient data for rolling beta", showarrow = FALSE)
        return(ea_plotly_layout(fig, x_title = NULL, y_title = NULL))
      }

      for (i in seq_along(unique(rbt$pair))) {
        pair_name <- unique(rbt$pair)[[i]]
        df <- rbt[rbt$pair == pair_name, , drop = FALSE]
        col <- palette[[((i - 1L) %% length(palette)) + 1L]]
        fig <- fig |>
          plotly::add_lines(
            data = df,
            x = ~date,
            y = ~beta,
            name = pair_name,
            line = list(color = col, width = 1.5),
            hovertemplate = "%{x|%d %b %Y}<br>Beta: %{y:.3f}<extra>%{fullData.name}</extra>"
          )
      }

      ea_plotly_layout(fig, x_title = NULL, y_title = "Beta")
    })

    # ---- Cointegration Residual with OU Bands ----
    output$coint_residual <- plotly::renderPlotly({
      cr <- page_data()$coint_residual

      if (is.null(cr) || nrow(cr) == 0L) {
        fig <- plotly::plot_ly() |>
          plotly::add_annotations(text = "Insufficient data for cointegration", showarrow = FALSE)
        return(ea_plotly_layout(fig, x_title = NULL, y_title = NULL))
      }

      # Use first pair available
      first_pair <- cr$pair[1]
      df_cr <- cr[cr$pair == first_pair, , drop = FALSE]

      mu_val <- df_cr$ou_mu[1]

      fig <- plotly::plot_ly(data = df_cr, x = ~date) |>
        plotly::add_ribbons(
          ymin = ~band_2_lo,
          ymax = ~band_2_hi,
          name = "2-sigma band",
          line = list(color = "transparent"),
          fillcolor = "rgba(77,163,163,0.15)",
          showlegend = TRUE,
          hoverinfo = "skip"
        ) |>
        plotly::add_ribbons(
          ymin = ~band_1_lo,
          ymax = ~band_1_hi,
          name = "1-sigma band",
          line = list(color = "transparent"),
          fillcolor = "rgba(77,163,163,0.30)",
          showlegend = TRUE,
          hoverinfo = "skip"
        ) |>
        plotly::add_lines(
          y = ~residual,
          name = paste0(first_pair, " residual"),
          line = list(color = "#4da3a3", width = 1.5),
          hovertemplate = "%{x|%d %b %Y}<br>Residual: %{y:.3f}<extra></extra>"
        ) |>
        plotly::layout(shapes = list(
          list(type = "line",
               x0 = min(df_cr$date), x1 = max(df_cr$date),
               y0 = mu_val, y1 = mu_val,
               line = list(color = "#d2a157", width = 1, dash = "dash"))
        ))

      ea_plotly_layout(fig, x_title = NULL, y_title = "Residual",
                       title = first_pair)
    })
  })
}
