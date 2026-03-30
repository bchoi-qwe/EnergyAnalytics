mod_hedging_ui <- function(id) {
  shiny::uiOutput(shiny::NS(id, "page"))
}

mod_hedging_server <- function(id, filters, data_timestamp) {
  shiny::moduleServer(id, function(input, output, session) {
    page_data <- shiny::reactive(ea_mock_hedging_data(filters()))

    mod_kpi_strip_server("kpi_strip", kpis = shiny::reactive(page_data()$kpis))
    mod_detail_table_server(
      "hedge_matrix",
      data = shiny::reactive(page_data()$hedge_matrix),
      title = "Cross-Hedge Matrix",
      subtitle = "Benchmark, beta, and effectiveness summary by product."
    )
    mod_footer_notes_server(
      "footer_notes",
      notes = shiny::reactive(list(
        notes = page_data()$notes,
        assumptions = page_data()$assumptions,
        timestamp = data_timestamp()
      )),
      title = "Cross-hedge notes"
    )

    output$page <- shiny::renderUI({
      ns <- session$ns
      current_filters <- filters()

      if (length(current_filters$commodities) == 0L) {
        return(htmltools::tagList(
          ea_selected_filters_ribbon(current_filters, data_timestamp()),
          ea_empty_state_card(
            title = "Select products to populate hedge analytics",
            body = "Cross-Hedge focuses on hedge ratio term structure, hedge history, and effectiveness versus the selected benchmark.",
            hint = "The benchmark selector is acting as the proxy hedge leg for now."
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
            title = "Cross-Hedge Ratio by Tenor",
            subtitle = "Hedge ratio across the listed curve.",
            output_id = ns("hedge_term"),
            height = "330px"
          ),
          ea_plotly_card(
            title = "Cross-Hedge Ratio History",
            subtitle = "Hedge ratio stability through time.",
            output_id = ns("hedge_time"),
            height = "330px"
          )
        ),
        bslib::layout_columns(
          col_widths = c(7, 5),
          mod_detail_table_ui(ns("hedge_matrix")),
          ea_plotly_card(
            title = "Hedge effectiveness",
            subtitle = "Effectiveness by contract bucket.",
            output_id = ns("hedge_effectiveness"),
            height = "360px"
          )
        ),
        bslib::layout_columns(
          col_widths = c(8, 4),
          ea_plotly_card(
            title = "Shock Sensitivity",
            subtitle = "Hedge ratio response to flat, vol, and spread shocks.",
            output_id = ns("hedge_sensitivity"),
            height = "300px"
          ),
          ea_standard_card(
            title = "Basis / Method",
            subtitle = "Benchmark caveats, basis risk, and client hedge framing.",
            class = "ea-card--commentary",
            shiny::uiOutput(ns("hedge_notes"))
          )
        ),
        mod_footer_notes_ui(ns("footer_notes"))
      )
    })

    output$hedge_term <- plotly::renderPlotly({
      chart_data <- page_data()$hedge_term
      palette <- ea_market_palette()
      fig <- plotly::plot_ly()

      for (market in unique(chart_data$market)) {
        df <- chart_data[chart_data$market == market, , drop = FALSE]
        fig <- fig |>
          plotly::add_lines(
            data = df,
            x = ~tenor,
            y = ~ratio,
            name = unique(df$label),
            line = list(color = unname(palette[[market]]), width = 2.2),
            hovertemplate = "Tenor %{x}<br>%{y:.2f}<extra>%{fullData.name}</extra>"
          )
      }

      ea_plotly_layout(fig, x_title = "Contract month", y_title = "Hedge ratio")
    })

    output$hedge_time <- plotly::renderPlotly({
      chart_data <- page_data()$hedge_time
      palette <- ea_market_palette()
      fig <- plotly::plot_ly()

      for (market in unique(chart_data$market)) {
        df <- chart_data[chart_data$market == market, , drop = FALSE]
        fig <- fig |>
          plotly::add_lines(
            data = df,
            x = ~date,
            y = ~ratio,
            name = unique(df$label),
            line = list(color = unname(palette[[market]]), width = 2),
            hovertemplate = "%{x|%d %b %Y}<br>%{y:.2f}<extra>%{fullData.name}</extra>"
          )
      }

      ea_plotly_layout(fig, x_title = NULL, y_title = "Hedge ratio")
    })

    output$hedge_effectiveness <- plotly::renderPlotly({
      chart_data <- page_data()$effectiveness

      fig <- plotly::plot_ly(
        data = chart_data,
        x = ~bucket,
        y = ~effectiveness,
        type = "bar",
        marker = list(color = "#4da3a3"),
        hovertemplate = "%{x}<br>%{y:.2%}<extra>effectiveness</extra>"
      )

      ea_plotly_layout(fig, x_title = NULL, y_title = "Hedge effectiveness", hovermode = "closest")
    })

    output$hedge_sensitivity <- plotly::renderPlotly({
      chart_data <- page_data()$sensitivity

      fig <- plotly::plot_ly(
        data = chart_data,
        x = ~shock,
        y = ~ratio,
        type = "scatter",
        mode = "lines+markers",
        line = list(color = "#d2a157", width = 2.2),
        marker = list(color = "#4da3a3", size = 8),
        hovertemplate = "%{x}<br>%{y:.2f}<extra>sensitivity</extra>"
      )

      ea_plotly_layout(fig, x_title = "Shock", y_title = "Hedge ratio", hovermode = "closest")
    })

    output$hedge_notes <- shiny::renderUI({
      htmltools::tags$div(
        class = "ea-commentary",
        htmltools::tags$ul(
          lapply(page_data()$notes, htmltools::tags$li)
        )
      )
    })
  })
}
