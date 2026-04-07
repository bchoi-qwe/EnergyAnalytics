mod_overview_ui <- function(id) {
  shiny::uiOutput(shiny::NS(id, "page"))
}

mod_overview_server <- function(id, filters, data_timestamp) {
  shiny::moduleServer(id, function(input, output, session) {
    page_data <- shiny::reactive({ ea_calc_overview(filters()) })

    output$page <- shiny::renderUI({
      ns <- session$ns
      current_filters <- filters()

      if (length(current_filters$commodities) == 0L) {
        return(htmltools::tagList(
          ea_empty_state_card(
            title = "Select products to populate Overview",
            body = "The Overview consolidates outrights, curve shape, spreads, and benchmark context across the selected products.",
            hint = "All panels key off the shared product and benchmark selection."
          )
        ))
      }

      htmltools::tagList(
        bslib::layout_columns(
          col_widths = c(8, 4),
          ea_table_card(
            title = "Market Snapshot",
            subtitle = "Front-contract read across the selected set",
            output_id = ns("market_snapshot_table")
          ),
          ea_plotly_card(
            title = "Spread Monitor",
            subtitle = "Current cross-commodity and calendar spread dislocations",
            output_id = ns("spread_monitor_chart"),
            height = "320px"
          )
        ),
        bslib::layout_columns(
          col_widths = c(8, 4),
          ea_plotly_card(
            title = "Relative Performance",
            subtitle = "Indexed front-contract performance over the active lookback",
            output_id = ns("relative_performance"),
            height = "300px"
          ),
          ea_plotly_card(
            title = "Volatility Snapshot",
            subtitle = "20d realized volatility with current percentile context",
            output_id = ns("vol_snapshot_chart"),
            height = "300px"
          )
        )
      )
    })

    output$market_snapshot_table <- reactable::renderReactable({
      mst <- page_data()$market_snapshot_table
      if (nrow(mst) == 0L) return(ea_empty_reactable())
      market_order <- match(mst$market, filters()$commodities)
      reactable::reactable(
        mst %>%
          dplyr::mutate(display_order = dplyr::coalesce(market_order, dplyr::row_number())) %>%
          dplyr::arrange(.data$display_order) %>%
          dplyr::mutate(
            price      = round(.data$price, 2),
            chg_1d     = round(.data$chg_1d, 2),
            pct_1d     = scales::percent(.data$pct_1d, accuracy = 0.01),
            chg_1w     = round(.data$chg_1w, 2),
            pct_1w     = scales::percent(.data$pct_1w, accuracy = 0.01),
            chg_1m     = round(.data$chg_1m, 2),
            pct_1m     = scales::percent(.data$pct_1m, accuracy = 0.01),
            percentile_52w = scales::percent(.data$percentile_52w, accuracy = 1)
          ) %>%
          dplyr::select(.data$label, .data$price, .data$chg_1d, .data$pct_1d,
                        .data$chg_1w, .data$pct_1w, .data$chg_1m, .data$pct_1m,
                        .data$percentile_52w),
        theme = ea_reactable_theme(),
        compact = TRUE,
        striped = FALSE,
        highlight = TRUE,
        borderless = TRUE,
        sortable = FALSE,
        columns = list(
          label = reactable::colDef(name = "Market"),
          price = reactable::colDef(name = "Price"),
          chg_1d = reactable::colDef(name = "1D Chg"),
          pct_1d = reactable::colDef(name = "1D %"),
          chg_1w = reactable::colDef(name = "1W Chg"),
          pct_1w = reactable::colDef(name = "1W %"),
          chg_1m = reactable::colDef(name = "1M Chg"),
          pct_1m = reactable::colDef(name = "1M %"),
          percentile_52w = reactable::colDef(name = "52W %ile")
        )
      )
    })

    output$relative_performance <- plotly::renderPlotly({
      rp <- page_data()$relative_performance
      catalog <- ea_market_catalog()
      labels_map <- stats::setNames(catalog$label, catalog$market)
      palette <- ea_market_palette()
      fig <- plotly::plot_ly()
      if (nrow(rp) == 0L) return(ea_plotly_layout(fig, x_title = NULL, y_title = "Index"))
      for (mkt in unique(rp$market)) {
        df <- rp[rp$market == mkt, , drop = FALSE]
        col <- if (!is.null(palette[[mkt]])) unname(palette[[mkt]]) else "#4da3a3"
        lbl <- ea_coalesce(labels_map[mkt], mkt)
        fig <- fig %>%
          plotly::add_lines(data = df, x = ~date, y = ~indexed_value,
            name = lbl, line = list(color = col, width = 2),
            hovertemplate = "%{x|%d %b %Y}<br>%{y:.1f}<extra>%{fullData.name}</extra>")
      }
      ea_plotly_layout(fig, x_title = NULL, y_title = "Relative Index (100)")
    })

    output$anomaly_chart <- plotly::renderPlotly({
      ta <- page_data()$top_anomalies
      fig <- plotly::plot_ly()
      if (nrow(ta) == 0L) {
        fig <- fig %>% plotly::add_annotations(text = "No anomalies detected", showarrow = FALSE)
        return(ea_plotly_layout(fig, x_title = "Z-Score", y_title = NULL))
      }
      ta <- ta[order(abs(ta$zscore)), , drop = FALSE]
      bar_colors <- ifelse(ta$zscore >= 0, "#4da3a3", "#b35c60")
      fig <- plotly::plot_ly(data = ta, x = ~zscore, y = ~label,
        type = "bar", orientation = "h",
        marker = list(color = bar_colors),
        text = ~anomaly_type, textposition = "outside",
        hovertemplate = "%{y}<br>Z-score: %{x:.2f}<extra>%{text}</extra>") %>%
        plotly::layout(shapes = list(
          list(type = "line", x0 = 0, x1 = 0, y0 = -0.5, y1 = nrow(ta) - 0.5,
               line = list(color = "#7f8b99", width = 1, dash = "dot"))
        ))
      ea_plotly_layout(fig, x_title = "Z-Score", y_title = NULL, hovermode = "closest")
    })

    output$correlation_heatmap <- plotly::renderPlotly({
      cs <- page_data()$correlation_snapshot
      fig <- plotly::plot_ly()
      if (nrow(cs) == 0L) return(ea_plotly_layout(fig, x_title = NULL, y_title = NULL))
      mat <- stats::xtabs(correlation ~ market_y + market_x, data = cs)
      fig <- plotly::plot_ly(
        x = colnames(mat), y = rownames(mat), z = unclass(mat),
        type = "heatmap",
        colors = c("#b35c60", "#17202b", "#4da3a3"),
        zmin = -1, zmax = 1,
        hovertemplate = "%{y} vs %{x}<br>%{z:.2f}<extra>correlation</extra>"
      ) %>% plotly::layout(yaxis = list(autorange = "reversed"))
      ea_plotly_layout(fig, x_title = NULL, y_title = NULL, hovermode = "closest")
    })

    output$spread_monitor_chart <- plotly::renderPlotly({
      sm <- page_data()$spread_monitor
      fig <- plotly::plot_ly()
      if (nrow(sm) == 0L) return(ea_plotly_layout(fig, x_title = "Z-Score", y_title = NULL))
      sm <- sm[order(abs(sm$zscore)), , drop = FALSE]
      bar_colors <- ifelse(sm$zscore >= 0, "#4da3a3", "#b35c60")
      fig <- plotly::plot_ly(data = sm, x = ~zscore, y = ~spread_label,
        type = "bar", orientation = "h",
        marker = list(color = bar_colors),
        customdata = ~level,
        hovertemplate = "%{y}<br>Z-score: %{x:.2f}<br>Level: %{customdata:.2f}<extra></extra>") %>%
        plotly::layout(shapes = list(
          list(type = "line", x0 = 0, x1 = 0, y0 = -0.5, y1 = nrow(sm) - 0.5,
               line = list(color = "#7f8b99", width = 1, dash = "dot"))
        ))
      ea_plotly_layout(fig, x_title = "Z-Score", y_title = NULL, hovermode = "closest")
    })

    output$vol_snapshot_chart <- plotly::renderPlotly({
      vs <- page_data()$vol_snapshot
      fig <- plotly::plot_ly()
      if (nrow(vs) == 0L) return(ea_plotly_layout(fig, x_title = NULL, y_title = "Vol"))
      fig <- fig %>%
        plotly::add_bars(data = vs, x = ~market, y = ~realized_vol_20d,
          name = "20d RV", marker = list(color = "#4da3a3"),
          hovertemplate = "%{x}<br>20d RV: %{y:.1%}<extra></extra>") %>%
        plotly::add_lines(data = vs, x = ~market, y = ~vol_percentile,
          name = "Percentile", yaxis = "y2",
          line = list(color = "#d2a157", width = 2),
          hovertemplate = "%{x}<br>Percentile: %{y:.0%}<extra></extra>") %>%
        plotly::layout(yaxis2 = list(title = "Percentile", overlaying = "y", side = "right",
          color = "#9aa6b2", showgrid = FALSE, tickformat = ".0%"))
      ea_plotly_layout(fig, x_title = NULL, y_title = "Realized Vol (Ann.)")
    })
  })
}
