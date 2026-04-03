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
        bslib::layout_columns(
          col_widths = c(6, 6),
          ea_plotly_card(
            title = "Vanna Surface",
            subtitle = "DdeltaDvol across tenor and moneyness (primary product).",
            output_id = ns("greeks_vanna"),
            height = "340px"
          ),
          ea_plotly_card(
            title = "Charm Surface",
            subtitle = "DdeltaDtime across tenor and moneyness (primary product).",
            output_id = ns("greeks_charm"),
            height = "340px"
          )
        ),
        bslib::layout_columns(
          col_widths = c(7, 5),
          ea_plotly_card(
            title = "Gamma & Vega Term Structure",
            subtitle = "ATM Gamma and Vega by contract month for each product.",
            output_id = ns("greeks_term"),
            height = "340px"
          ),
          ea_plotly_card(
            title = "3rd Order Strike Profile",
            subtitle = "Speed and Zomma by strike for the prompt month.",
            output_id = ns("greeks_tails"),
            height = "340px"
          )
        ),
        mod_footer_notes_ui(ns("footer_notes"))
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

    # ---- Gamma & Vega term structure ----
    output$greeks_term <- plotly::renderPlotly({
      chart_data <- page_data()$term_greeks
      palette <- ea_market_palette()
      fig <- plotly::plot_ly()

      for (mkt in unique(chart_data$market)) {
        df <- chart_data[chart_data$market == mkt, , drop = FALSE]

        fig <- fig |>
          plotly::add_lines(
            data = df,
            x = ~curve_point_num,
            y = ~gamma,
            name = paste(unique(df$label), "Gamma"),
            line = list(color = unname(palette[[mkt]]), width = 2),
            yaxis = "y",
            hovertemplate = "M%{x}<br>Gamma: %{y:.6f}<extra>%{fullData.name}</extra>"
          ) |>
          plotly::add_lines(
            data = df,
            x = ~curve_point_num,
            y = ~vega,
            name = paste(unique(df$label), "Vega"),
            line = list(color = unname(palette[[mkt]]), width = 2, dash = "dash"),
            yaxis = "y2",
            hovertemplate = "M%{x}<br>Vega: %{y:.4f}<extra>%{fullData.name}</extra>"
          )
      }

      fig <- fig |>
        plotly::layout(
          yaxis2 = list(
            title = "Vega",
            overlaying = "y",
            side = "right",
            color = "#9aa6b2",
            gridcolor = "rgba(148, 163, 184, 0.04)",
            showgrid = FALSE
          )
        )

      ea_plotly_layout(fig, x_title = "Contract month", y_title = "Gamma")
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
