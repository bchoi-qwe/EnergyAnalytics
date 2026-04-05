mod_options_greeks_ui <- function(id) {
  shiny::uiOutput(shiny::NS(id, "page"))
}

mod_options_greeks_server <- function(id, filters, data_timestamp) {
  shiny::moduleServer(id, function(input, output, session) {
    page_data <- shiny::reactive(ea_calc_surface_greeks(filters()))

    mod_kpi_strip_server("kpi_strip", kpis = shiny::reactive(page_data()$kpis))
    mod_footer_notes_server(
      "footer_notes",
      notes = shiny::reactive(list(
        notes = page_data()$notes,
        assumptions = page_data()$assumptions,
        timestamp = data_timestamp()
      )),
      title = "Options Greeks notes"
    )

    output$page <- shiny::renderUI({
      ns <- session$ns
      current_filters <- filters()

      if (length(current_filters$commodities) == 0L) {
        return(htmltools::tagList(
          ea_selected_filters_ribbon(current_filters, data_timestamp()),
          ea_empty_state_card(
            title = "Select products to populate Greeks views",
            body = "Options Greeks computes Black-76 delta, gamma, vega, vanna, charm, speed, zomma, and more across the vol surface for the selected products.",
            hint = "All Greeks are calculated natively in C++ via the Rcpp pricing engine."
          ),
          mod_footer_notes_ui(ns("footer_notes"))
        ))
      }

      htmltools::tagList(
        ea_selected_filters_ribbon(current_filters, data_timestamp()),
        mod_kpi_strip_ui(ns("kpi_strip")),
        # Row 1: Delta-Gamma Surface | Vega-Theta Surface
        bslib::layout_columns(
          col_widths = c(6, 6),
          ea_plotly_card(
            title = "Delta-Gamma Surface",
            subtitle = "ATM delta and gamma across tenor and moneyness.",
            output_id = ns("delta_gamma_surface"),
            height = "340px"
          ),
          ea_plotly_card(
            title = "Vega-Theta Surface",
            subtitle = "ATM vega and theta across tenor and moneyness.",
            output_id = ns("vega_theta_surface"),
            height = "340px"
          )
        ),
        # Row 2: ATM Greeks Term Structure | Greeks Concentration table
        bslib::layout_columns(
          col_widths = c(7, 5),
          ea_plotly_card(
            title = "ATM Greeks Term Structure",
            subtitle = "Delta, gamma, vega by contract month.",
            output_id = ns("atm_greeks_term"),
            height = "340px"
          ),
          ea_table_card(
            title = "Greeks Concentration",
            subtitle = "Summed gamma and vega by tenor bucket.",
            output_id = ns("greeks_concentration_table")
          )
        ),
        # Row 3: Vanna Surface | Charm Surface
        bslib::layout_columns(
          col_widths = c(6, 6),
          ea_plotly_card(
            title = "Vanna Surface",
            subtitle = "DdeltaDvol across tenor and moneyness.",
            output_id = ns("greeks_vanna"),
            height = "340px"
          ),
          ea_plotly_card(
            title = "Charm Surface",
            subtitle = "DdeltaDtime across tenor and moneyness.",
            output_id = ns("greeks_charm"),
            height = "340px"
          )
        ),
        # Row 4: Vol Smile Evolution | P&L Heatmap
        bslib::layout_columns(
          col_widths = c(7, 5),
          ea_plotly_card(
            title = "Vol Smile Evolution",
            subtitle = "Implied vol smile by tenor (faceted subplots).",
            output_id = ns("smile_evolution"),
            height = "340px"
          ),
          ea_plotly_card(
            title = "P&L Heatmap",
            subtitle = "Option P&L under spot \u00d7 vol shock.",
            output_id = ns("pnl_heatmap"),
            height = "340px"
          )
        ),
        # Row 5: 3rd Order Strike Profile (full width)
        bslib::layout_columns(
          col_widths = c(12),
          ea_plotly_card(
            title = "3rd Order Strike Profile",
            subtitle = "Speed and Zomma by strike.",
            output_id = ns("greeks_tails"),
            height = "300px"
          )
        ),
        mod_footer_notes_ui(ns("footer_notes"))
      )
    })

    # ---- Delta-Gamma Surface heatmap ----
    output$delta_gamma_surface <- plotly::renderPlotly({
      fg <- page_data()$full_grid
      primary <- fg$market[1]
      df <- fg |> dplyr::filter(.data$market == primary)
      mat <- stats::xtabs(gamma ~ curve_point_num + moneyness, data = df)
      fig <- plotly::plot_ly(
        x = colnames(mat), y = rownames(mat), z = unclass(mat),
        type = "heatmap",
        colors = c("#17202b", "#4da3a3", "#d2a157"),
        hovertemplate = "Moneyness %{x}<br>Tenor M%{y}<br>Gamma: %{z:.6f}<extra>gamma</extra>"
      )
      ea_plotly_layout(fig, x_title = "Moneyness", y_title = "Contract month", hovermode = "closest")
    })

    # ---- Vega-Theta Surface heatmap ----
    output$vega_theta_surface <- plotly::renderPlotly({
      fg <- page_data()$full_grid
      primary <- fg$market[1]
      df <- fg |> dplyr::filter(.data$market == primary)
      mat <- stats::xtabs(vega ~ curve_point_num + moneyness, data = df)
      fig <- plotly::plot_ly(
        x = colnames(mat), y = rownames(mat), z = unclass(mat),
        type = "heatmap",
        colors = c("#17202b", "#5a85c8", "#d36e70"),
        hovertemplate = "Moneyness %{x}<br>Tenor M%{y}<br>Vega: %{z:.4f}<extra>vega</extra>"
      )
      ea_plotly_layout(fig, x_title = "Moneyness", y_title = "Contract month", hovermode = "closest")
    })

    # ---- ATM Greeks Term Structure ----
    output$atm_greeks_term <- plotly::renderPlotly({
      tg <- page_data()$term_greeks
      palette <- ea_market_palette()
      fig <- plotly::plot_ly()
      if (nrow(tg) == 0L) return(ea_plotly_layout(fig, x_title = "Contract month", y_title = "Gamma"))
      for (mkt in unique(tg$market)) {
        df <- tg[tg$market == mkt, , drop = FALSE]
        col <- unname(palette[[mkt]])
        fig <- fig |>
          plotly::add_lines(data = df, x = ~curve_point_num, y = ~gamma,
            name = paste(unique(df$label), "Gamma"),
            line = list(color = col, width = 2), yaxis = "y",
            hovertemplate = "M%{x}<br>Gamma: %{y:.6f}<extra>%{fullData.name}</extra>") |>
          plotly::add_lines(data = df, x = ~curve_point_num, y = ~vega,
            name = paste(unique(df$label), "Vega"),
            line = list(color = col, width = 2, dash = "dash"), yaxis = "y2",
            hovertemplate = "M%{x}<br>Vega: %{y:.4f}<extra>%{fullData.name}</extra>")
      }
      fig <- fig |> plotly::layout(yaxis2 = list(title = "Vega", overlaying = "y", side = "right",
        color = "#9aa6b2", gridcolor = "rgba(148,163,184,0.04)", showgrid = FALSE))
      ea_plotly_layout(fig, x_title = "Contract month", y_title = "Gamma")
    })

    # ---- Greeks Concentration table ----
    output$greeks_concentration_table <- reactable::renderReactable({
      gc <- page_data()$greeks_concentration
      reactable::reactable(
        gc |> dplyr::select(market, tenor_bucket, delta, gamma, vega, theta) |>
          dplyr::mutate(
            delta  = round(delta, 3),
            gamma  = round(gamma, 6),
            vega   = round(vega, 3),
            theta  = round(theta, 4)
          ),
        theme = ea_reactable_theme(),
        striped = TRUE, highlight = TRUE,
        columns = list(
          market = reactable::colDef(name = "Market"),
          tenor_bucket = reactable::colDef(name = "Tenor"),
          delta = reactable::colDef(name = "Delta"),
          gamma = reactable::colDef(name = "Gamma"),
          vega = reactable::colDef(name = "Vega"),
          theta = reactable::colDef(name = "Theta")
        )
      )
    })

    # ---- Vanna heatmap ----
    output$greeks_vanna <- plotly::renderPlotly({
      chart_data <- page_data()$cross_greeks_vanna
      vanna_matrix <- stats::xtabs(vanna ~ curve_point_num + moneyness, data = chart_data)

      fig <- plotly::plot_ly(
        x = colnames(vanna_matrix),
        y = rownames(vanna_matrix),
        z = unclass(vanna_matrix),
        type = "heatmap",
        colors = c("#17202b", "#4da3a3", "#d2a157"),
        hovertemplate = "Moneyness %{x}<br>Tenor M%{y}<br>Vanna: %{z:.4f}<extra>vanna</extra>"
      )

      ea_plotly_layout(fig, x_title = "Moneyness", y_title = "Contract month", hovermode = "closest")
    })

    # ---- Charm heatmap ----
    output$greeks_charm <- plotly::renderPlotly({
      chart_data <- page_data()$cross_greeks_charm
      charm_matrix <- stats::xtabs(charm ~ curve_point_num + moneyness, data = chart_data)

      fig <- plotly::plot_ly(
        x = colnames(charm_matrix),
        y = rownames(charm_matrix),
        z = unclass(charm_matrix),
        type = "heatmap",
        colors = c("#17202b", "#5a85c8", "#d36e70"),
        hovertemplate = "Moneyness %{x}<br>Tenor M%{y}<br>Charm: %{z:.6f}<extra>charm</extra>"
      )

      ea_plotly_layout(fig, x_title = "Moneyness", y_title = "Contract month", hovermode = "closest")
    })

    # ---- Vol Smile Evolution ----
    output$smile_evolution <- plotly::renderPlotly({
      fg <- page_data()$full_grid
      if (nrow(fg) == 0L) return(ea_plotly_layout(plotly::plot_ly(), x_title = "Moneyness", y_title = "IV"))
      primary <- fg$market[1]
      df <- fg |> dplyr::filter(.data$market == primary)
      tenors <- sort(unique(df$curve_point_num))
      tenors <- utils::head(tenors, 4L)
      palette <- c("#4da3a3", "#5a85c8", "#d2a157", "#d36e70")
      sub_figs <- lapply(seq_along(tenors), function(i) {
        tn <- tenors[[i]]
        df_t <- df[df$curve_point_num == tn, , drop = FALSE]
        col <- palette[[i]]
        plotly::plot_ly(data = df_t, x = ~moneyness, y = ~iv, type = "scatter", mode = "lines",
          name = paste0("M", tn), line = list(color = col, width = 1.8),
          hovertemplate = paste0("M", tn, "<br>Moneyness: %{x:.0%}<br>IV: %{y:.1%}<extra></extra>"))
      })
      fig <- plotly::subplot(sub_figs, nrows = length(sub_figs), shareX = TRUE, titleX = FALSE)
      ea_plotly_layout(fig, x_title = "Moneyness", y_title = NULL)
    })

    # ---- P&L Heatmap ----
    output$pnl_heatmap <- plotly::renderPlotly({
      pg <- page_data()$pnl_grid
      if (nrow(pg) == 0L) return(ea_plotly_layout(plotly::plot_ly(), x_title = "Spot shock", y_title = "Vol shock"))
      mat <- stats::xtabs(pnl ~ vol_shock + spot_shock, data = pg)
      fig <- plotly::plot_ly(
        x = colnames(mat), y = rownames(mat), z = unclass(mat),
        type = "heatmap",
        colors = c("#b35c60", "#17202b", "#4da3a3"),
        hovertemplate = "Spot %{x:.0%}<br>Vol %{y:.0%}<br>P&L: %{z:.4f}<extra>pnl</extra>"
      )
      ea_plotly_layout(fig, x_title = "Spot shock", y_title = "Vol shock", hovermode = "closest")
    })

    # ---- 3rd order strike profile ----
    output$greeks_tails <- plotly::renderPlotly({
      chart_data <- page_data()$strike_profile
      fig <- plotly::plot_ly()

      fig <- fig |>
        plotly::add_lines(
          data = chart_data,
          x = ~moneyness,
          y = ~speed,
          name = "Speed (DgammaDspot)",
          line = list(color = "#4da3a3", width = 2),
          hovertemplate = "Moneyness %{x:.0%}<br>Speed: %{y:.6f}<extra>%{fullData.name}</extra>"
        ) |>
        plotly::add_lines(
          data = chart_data,
          x = ~moneyness,
          y = ~zomma,
          name = "Zomma (DgammaDvol)",
          line = list(color = "#d2a157", width = 2, dash = "dash"),
          yaxis = "y2",
          hovertemplate = "Moneyness %{x:.0%}<br>Zomma: %{y:.6f}<extra>%{fullData.name}</extra>"
        )

      fig <- fig |>
        plotly::layout(
          yaxis2 = list(
            title = "Zomma",
            overlaying = "y",
            side = "right",
            color = "#9aa6b2",
            gridcolor = "rgba(148, 163, 184, 0.04)",
            showgrid = FALSE
          )
        )

      ea_plotly_layout(fig, x_title = "Moneyness", y_title = "Speed", hovermode = "closest")
    })
  })
}
