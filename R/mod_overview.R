mod_overview_ui <- function(id) {
  shiny::uiOutput(shiny::NS(id, "page"))
}

mod_overview_server <- function(id, filters, data_timestamp) {
  shiny::moduleServer(id, function(input, output, session) {
    overview_data <- shiny::reactive(ea_mock_overview_data(filters()))

    mod_kpi_strip_server("kpi_strip", kpis = shiny::reactive(overview_data()$kpis))
    mod_detail_table_server(
      "detail_table",
      data = shiny::reactive(overview_data()$detail_table),
      title = "Market Snapshot",
      subtitle = "Outright, curve, vol, and cross-hedge summary by product."
    )
    mod_footer_notes_server(
      "footer_notes",
      notes = shiny::reactive(list(
        notes = overview_data()$notes,
        assumptions = overview_data()$assumptions,
        timestamp = data_timestamp()
      )),
      title = "Market Monitor notes"
    )

    output$page <- shiny::renderUI({
      ns <- session$ns
      current_filters <- filters()

      if (length(current_filters$commodities) == 0L) {
        return(htmltools::tagList(
          ea_selected_filters_ribbon(current_filters, data_timestamp()),
          ea_empty_state_card(
            title = "Select products to populate Market Monitor",
            body = "Market Monitor consolidates outrights, curve shape, spreads, and benchmark context across the selected products.",
            hint = "All panels key off the shared product and benchmark selection."
          ),
          mod_footer_notes_ui(ns("footer_notes"))
        ))
      }

      htmltools::tagList(
        ea_selected_filters_ribbon(current_filters, data_timestamp()),
        mod_kpi_strip_ui(ns("kpi_strip")),
        bslib::layout_columns(
          col_widths = c(8, 4),
          ea_plotly_card(
            title = "Historical Analysis",
            subtitle = "Normalized price history across the selected product set.",
            output_id = ns("relative_performance"),
            height = "360px"
          ),
          ea_standard_card(
            title = "Market Color",
            subtitle = "Trader annotations, opening call notes, and benchmark framing.",
            class = "ea-card--commentary",
            shiny::uiOutput(ns("commentary"))
          )
        ),
        bslib::layout_columns(
          col_widths = c(7, 5),
          ea_plotly_card(
            title = "Correlation Matrix",
            subtitle = "Cross-product correlation monitor for the active complex.",
            output_id = ns("relationship_heatmap"),
            height = "330px"
          ),
          ea_plotly_card(
            title = "Calendar / Intercommodity Spreads",
            subtitle = "Intracommodity and intercommodity spread watchlist.",
            output_id = ns("spread_monitor"),
            height = "330px"
          )
        ),
        mod_detail_table_ui(ns("detail_table")),
        mod_footer_notes_ui(ns("footer_notes"))
      )
    })

    output$relative_performance <- plotly::renderPlotly({
      chart_data <- overview_data()$rel_perf
      palette <- ea_market_palette()
      series <- split(chart_data, chart_data$market)

      fig <- plotly::plot_ly()
      for (market in names(series)) {
        df <- series[[market]]
        fig <- fig |>
          plotly::add_lines(
            data = df,
            x = ~date,
            y = ~value,
            name = unique(df$label),
            line = list(width = 2, color = unname(palette[[market]])),
            hovertemplate = "%{x|%d %b %Y}<br>%{y:.2f} rel pts<extra>%{fullData.name}</extra>"
          )
      }

      ea_plotly_layout(fig, x_title = NULL, y_title = "Relative index")
    })

    output$relationship_heatmap <- plotly::renderPlotly({
      chart_data <- overview_data()$correlation
      corr_matrix <- stats::xtabs(value ~ y_label + x_label, data = chart_data)

      fig <- plotly::plot_ly(
        x = colnames(corr_matrix),
        y = rownames(corr_matrix),
        z = unclass(corr_matrix),
        type = "heatmap",
        colors = c("#b35c60", "#17202b", "#4da3a3"),
        zmin = -1,
        zmax = 1,
        hovertemplate = "%{y} vs %{x}<br>%{z:.2f}<extra>correlation</extra>"
      ) |>
        plotly::layout(
          yaxis = list(autorange = "reversed")
        )

      ea_plotly_layout(fig, x_title = NULL, y_title = NULL, hovermode = "closest")
    })

    output$spread_monitor <- plotly::renderPlotly({
      chart_data <- overview_data()$spread_monitor
      colors <- ifelse(chart_data$move >= 0, "#4da3a3", "#c96b6b")

      fig <- plotly::plot_ly(
        data = chart_data,
        x = ~tenor,
        y = ~level,
        type = "bar",
        marker = list(color = colors),
        customdata = ~move,
        hovertemplate = "%{x}<br>Level: %{y:.2f}<br>1D move: %{customdata:.2f}<extra></extra>"
      )

      ea_plotly_layout(fig, x_title = NULL, y_title = "Spread", hovermode = "closest")
    })

    output$commentary <- shiny::renderUI({
      commentary <- overview_data()$commentary

      htmltools::tags$div(
        class = "ea-commentary",
        htmltools::tags$h3(commentary$title),
        htmltools::tags$p(commentary$body),
        htmltools::tags$ul(lapply(commentary$bullets, htmltools::tags$li))
      )
    })
  })
}
