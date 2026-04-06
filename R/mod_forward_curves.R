mod_forward_curves_ui <- function(id) {
  shiny::uiOutput(shiny::NS(id, "page"))
}

mod_forward_curves_server <- function(id, filters, data_timestamp) {
  shiny::moduleServer(id, function(input, output, session) {
    page_data <- shiny::reactive(ea_calc_forward_curves(filters()))

    mod_kpi_strip_server("kpi_strip", kpis = shiny::reactive(page_data()$kpis))
    mod_footer_notes_server(
      "footer_notes",
      notes = shiny::reactive(list(
        notes = page_data()$notes,
        assumptions = page_data()$assumptions,
        timestamp = data_timestamp()
      )),
      title = "Term structure notes"
    )

    output$page <- shiny::renderUI({
      ns <- session$ns
      current_filters <- filters()
      catalog <- ea_market_catalog()

      if (length(current_filters$commodities) == 0L) {
        return(htmltools::tagList(
          ea_selected_filters_ribbon(current_filters, data_timestamp()),
          ea_empty_state_card(
            title = "Select products to populate term structure",
            body = "Term Structure focuses on curve shape, historical curve replay, and spread structure across the selected products.",
            hint = "Use the product multi-select, then refine curve date and tenor treatment with local controls."
          ),
          mod_footer_notes_ui(ns("footer_notes"))
        ))
      }

      htmltools::tagList(
        ea_selected_filters_ribbon(current_filters, data_timestamp()),
        mod_kpi_strip_ui(ns("kpi_strip")),
        # Row 1: Forward Curve Snapshot (8) | Structure Summary Table (4)
        bslib::layout_columns(
          col_widths = c(8, 4),
          ea_plotly_card(
            title = "Forward Curve Snapshot",
            subtitle = "Current forward curve by listed contract month.",
            output_id = ns("curve_snapshot"),
            height = "300px"
          ),
          ea_table_card(
            title = "Structure Summary",
            subtitle = "Curve shape metrics and regime classification.",
            output_id = ns("structure_table")
          )
        ),
        # Row 2: Historical Curve Overlay (6) | Curve Shift Heatmap (6)
        bslib::layout_columns(
          col_widths = c(6, 6),
          ea_plotly_card(
            title = "Historical Curve Overlay",
            subtitle = "Today vs 1W/1M/3M ago for the lead product.",
            output_id = ns("curve_history"),
            height = "270px"
          ),
          ea_plotly_card(
            title = "Curve Shift Heatmap",
            subtitle = "Daily change by tenor over past 60 days.",
            output_id = ns("curve_heatmap"),
            height = "270px"
          )
        ),
        # Row 3: Calendar Spread Strip (7) | Prompt Spread History (5)
        bslib::layout_columns(
          col_widths = c(7, 5),
          ea_plotly_card(
            title = "Calendar Spread Strip",
            subtitle = "Adjacent month spreads with z-score shading.",
            output_id = ns("spread_strip"),
            height = "260px"
          ),
          ea_plotly_card(
            title = "Prompt Spread History",
            subtitle = "M1-M2 spread with rolling percentile bands.",
            output_id = ns("prompt_spread"),
            height = "260px"
          )
        ),
        # Row 4: Roll Yield Strip (6) | PCA Factor Decomposition (6)
        bslib::layout_columns(
          col_widths = c(6, 6),
          ea_plotly_card(
            title = "Roll Yield Strip",
            subtitle = "Annualized roll yield by adjacent tenor pair.",
            output_id = ns("roll_yield_chart"),
            height = "260px"
          ),
          ea_plotly_card(
            title = "PCA Factor Decomposition",
            subtitle = "PC1/PC2/PC3 loadings by tenor (log returns).",
            output_id = ns("pca_chart"),
            height = "260px"
          )
        ),
        # Row 5: Treasury Overlay (12)
        bslib::layout_columns(
          col_widths = 12,
          ea_plotly_card(
            title = "Treasury Overlay",
            subtitle = "Commodity front-back spread vs Treasury 2s10s slope.",
            output_id = ns("treasury_overlay"),
            height = "260px"
          )
        ),
        mod_footer_notes_ui(ns("footer_notes"))
      )
    })

    # --- Row 1: Forward Curve Snapshot ---
    output$curve_snapshot <- plotly::renderPlotly({
      chart_data <- page_data()$curve_snapshot
      palette <- ea_market_palette()
      fig <- plotly::plot_ly()

      for (mkt in unique(chart_data$market)) {
        df <- chart_data[chart_data$market == mkt, , drop = FALSE]
        fig <- fig |>
          plotly::add_lines(
            data = df,
            x = ~curve_point_num,
            y = ~price,
            name = unique(df$label),
            line = list(width = 2.2, color = unname(palette[[mkt]])),
            hovertemplate = "M%{x}<br>%{y:.2f}<extra>%{fullData.name}</extra>"
          )
      }

      ea_plotly_layout(fig, x_title = "Contract Month", y_title = "Price")
    })

    # --- Row 1: Structure Summary Table ---
    output$structure_table <- reactable::renderReactable({
      ss <- page_data()$structure_summary
      if (is.null(ss) || nrow(ss) == 0) return(NULL)

      labels <- ea_market_labels(ss$market)
      display <- ss |>
        dplyr::mutate(
          market = labels,
          m1_price = scales::dollar(.data$m1_price, accuracy = 0.01),
          m1_m2 = scales::number(.data$m1_m2, accuracy = 0.01),
          m1_m6 = scales::number(.data$m1_m6, accuracy = 0.01),
          m1_m12 = scales::number(.data$m1_m12, accuracy = 0.01),
          slope = scales::number(.data$slope, accuracy = 0.01),
          curvature = scales::number(.data$curvature, accuracy = 0.01),
          m1_m12_percentile = ifelse(
            is.na(.data$m1_m12_percentile), "N/A",
            scales::percent(.data$m1_m12_percentile, accuracy = 1)
          ),
          roll_yield_ann = ifelse(
            is.na(.data$roll_yield_ann), "N/A",
            scales::percent(.data$roll_yield_ann, accuracy = 0.1)
          )
        ) |>
        dplyr::select("market", "m1_price", "m1_m2", "regime",
                       "m1_m12_percentile", "roll_yield_ann")

      reactable::reactable(
        display,
        compact = TRUE,
        striped = TRUE,
        theme = ea_reactable_theme(),
        columns = list(
          market = reactable::colDef(name = "Market", minWidth = 100),
          m1_price = reactable::colDef(name = "M1 Price", align = "right"),
          m1_m2 = reactable::colDef(name = "M1-M2", align = "right"),
          regime = reactable::colDef(name = "Regime", minWidth = 110),
          m1_m12_percentile = reactable::colDef(name = "Pctile", align = "right"),
          roll_yield_ann = reactable::colDef(name = "Roll Yld", align = "right")
        )
      )
    })

    # --- Row 2: Historical Curve Overlay ---
    output$curve_history <- plotly::renderPlotly({
      chart_data <- page_data()$curve_history
      colors <- c("Latest" = "#4da3a3", "1W Ago" = "#5a85c8",
                   "1M Ago" = "#d2a157", "3M Ago" = "#7f8b99")
      dashes <- c("Latest" = "solid", "1W Ago" = "dash",
                   "1M Ago" = "dash", "3M Ago" = "dot")
      opacities <- c("Latest" = 1.0, "1W Ago" = 0.7,
                      "1M Ago" = 0.6, "3M Ago" = 0.4)
      fig <- plotly::plot_ly()

      for (snap in unique(chart_data$snapshot_label)) {
        df <- chart_data[chart_data$snapshot_label == snap, , drop = FALSE]
        fig <- fig |>
          plotly::add_lines(
            data = df,
            x = ~curve_point_num,
            y = ~price,
            name = snap,
            line = list(
              width = if (identical(snap, "Latest")) 2.6 else 1.6,
              color = unname(colors[[snap]]),
              dash = unname(dashes[[snap]])
            ),
            opacity = unname(opacities[[snap]]),
            hovertemplate = "M%{x}<br>%{y:.2f}<extra>%{fullData.name}</extra>"
          )
      }

      ea_plotly_layout(fig, x_title = "Contract Month", y_title = "Price")
    })

    # --- Row 2: Curve Shift Heatmap ---
    output$curve_heatmap <- plotly::renderPlotly({
      heatmap_data <- page_data()$curve_change_heatmap
      if (is.null(heatmap_data) || nrow(heatmap_data) == 0) {
        return(plotly::plot_ly() |> ea_plotly_layout(x_title = NULL, y_title = NULL))
      }

      heatmap_matrix <- stats::xtabs(change ~ curve_point_num + date, data = heatmap_data)

      fig <- plotly::plot_ly(
        x = colnames(heatmap_matrix),
        y = rownames(heatmap_matrix),
        z = unclass(heatmap_matrix),
        type = "heatmap",
        colors = c("#b35c60", "#17202b", "#4da3a3"),
        hovertemplate = "Date %{x}<br>M%{y}<br>%{z:.2f}<extra>curve shift</extra>"
      )

      ea_plotly_layout(fig, x_title = NULL, y_title = "Contract Month", hovermode = "closest")
    })

    # --- Row 3: Calendar Spread Strip ---
    output$spread_strip <- plotly::renderPlotly({
      strip <- page_data()$calendar_spread_strip
      if (is.null(strip) || nrow(strip) == 0) {
        return(plotly::plot_ly() |> ea_plotly_layout(x_title = NULL, y_title = "Spread"))
      }

      # Color by z-score: green for positive, red for negative
      bar_colors <- dplyr::case_when(
        strip$zscore >  1 ~ "#4da3a3",
        strip$zscore >  0 ~ "#74a66e",
        strip$zscore > -1 ~ "#d2a157",
        TRUE              ~ "#b35c60"
      )

      fig <- plotly::plot_ly(
        data = strip,
        x = ~spread_label,
        y = ~current_spread,
        type = "bar",
        marker = list(color = bar_colors),
        customdata = ~zscore,
        hovertemplate = "%{x}<br>Spread: %{y:.3f}<br>Z-score: %{customdata:.2f}<extra></extra>"
      )

      ea_plotly_layout(fig, x_title = "Tenor Pair", y_title = "Spread", hovermode = "closest")
    })

    # --- Row 3: Prompt Spread History ---
    output$prompt_spread <- plotly::renderPlotly({
      ts_data <- page_data()$prompt_spread_ts
      if (is.null(ts_data) || nrow(ts_data) == 0) {
        return(plotly::plot_ly() |> ea_plotly_layout(x_title = "Date", y_title = "Spread"))
      }

      fig <- plotly::plot_ly(data = ts_data, x = ~date)

      # Add percentile ribbon bands if available
      if ("p5" %in% names(ts_data)) {
        fig <- fig |>
          plotly::add_ribbons(
            ymin = ~p5, ymax = ~p95,
            fillcolor = "rgba(77, 163, 163, 0.08)",
            line = list(color = "transparent"),
            showlegend = TRUE, name = "5-95 pctile"
          ) |>
          plotly::add_ribbons(
            ymin = ~p25, ymax = ~p75,
            fillcolor = "rgba(77, 163, 163, 0.18)",
            line = list(color = "transparent"),
            showlegend = TRUE, name = "25-75 pctile"
          )
      }

      fig <- fig |>
        plotly::add_lines(
          y = ~spread,
          name = "M1-M2",
          line = list(width = 2, color = "#4da3a3"),
          hovertemplate = "%{x}<br>Spread: %{y:.3f}<extra>M1-M2</extra>"
        )

      ea_plotly_layout(fig, x_title = "Date", y_title = "Spread")
    })

    # --- Row 4: Roll Yield Strip ---
    output$roll_yield_chart <- plotly::renderPlotly({
      ry <- page_data()$roll_yield
      if (is.null(ry) || nrow(ry) == 0) {
        return(plotly::plot_ly() |> ea_plotly_layout(x_title = NULL, y_title = "Roll Yield (Ann.)"))
      }

      bar_colors <- ifelse(ry$roll_yield_ann >= 0, "#74a66e", "#b35c60")

      fig <- plotly::plot_ly(
        data = ry,
        x = ~tenor_pair,
        y = ~roll_yield_ann,
        type = "bar",
        marker = list(color = bar_colors),
        hovertemplate = "%{x}<br>Roll yield: %{y:.1%}<extra></extra>"
      )

      ea_plotly_layout(fig, x_title = "Tenor Pair", y_title = "Roll Yield (Ann.)", hovermode = "closest")
    })

    # --- Row 4: PCA Factor Decomposition ---
    output$pca_chart <- plotly::renderPlotly({
      pca <- page_data()$curve_pca
      loadings <- pca$loadings
      var_exp <- pca$var_explained

      if (is.null(loadings) || nrow(loadings) == 0) {
        return(plotly::plot_ly() |> ea_plotly_layout(x_title = "Tenor", y_title = "Loading"))
      }

      pc_colors <- c(PC1 = "#4da3a3", PC2 = "#d2a157", PC3 = "#5a85c8")
      fig <- plotly::plot_ly(data = loadings, x = ~tenor)

      for (pc in c("PC1", "PC2", "PC3")) {
        if (pc %in% names(loadings) && !all(is.na(loadings[[pc]]))) {
          pct_label <- ""
          if (nrow(var_exp) > 0) {
            row_match <- var_exp[var_exp$component == pc, ]
            if (nrow(row_match) > 0) {
              pct_label <- paste0(" (", scales::percent(row_match$proportion[1], accuracy = 0.1), ")")
            }
          }
          fig <- fig |>
            plotly::add_lines(
              y = stats::as.formula(paste0("~", pc)),
              name = paste0(pc, pct_label),
              line = list(width = 2, color = unname(pc_colors[[pc]])),
              hovertemplate = paste0("%{x}<br>", pc, ": %{y:.3f}<extra></extra>")
            )
        }
      }

      ea_plotly_layout(fig, x_title = "Tenor", y_title = "Loading")
    })

    # --- Row 5: Treasury Overlay ---
    output$treasury_overlay <- plotly::renderPlotly({
      tov <- page_data()$treasury_overlay
      if (is.null(tov) || nrow(tov) == 0) {
        return(plotly::plot_ly() |> ea_plotly_layout(x_title = "Date", y_title = "Spread"))
      }

      fig <- plotly::plot_ly() |>
        plotly::add_lines(
          data = tov, x = ~date,
          y = ~cmdty_spread,
          name = "Commodity Spread",
          line = list(width = 2, color = "#4da3a3"),
          hovertemplate = "%{x}<br>Cmdty spread: %{y:.2f}<extra></extra>"
        ) |>
        plotly::add_lines(
          data = tov, x = ~date,
          y = ~slope_2s10s,
          name = "Treasury 2s10s",
          yaxis = "y2",
          line = list(width = 2, color = "#d2a157", dash = "dash"),
          hovertemplate = "%{x}<br>2s10s: %{y:.2f}%<extra></extra>"
        )

      fig <- fig |>
        plotly::layout(
          yaxis2 = list(
            title = "Treasury 2s10s (pp)",
            overlaying = "y",
            side = "right",
            color = "#9aa6b2",
            gridcolor = "rgba(0,0,0,0)",
            zerolinecolor = "rgba(148, 163, 184, 0.10)"
          )
        )

      ea_plotly_layout(fig, x_title = NULL, y_title = "Spread")
    })
  })
}
