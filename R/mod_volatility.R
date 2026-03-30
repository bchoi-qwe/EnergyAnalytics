mod_volatility_ui <- function(id) {
  shiny::uiOutput(shiny::NS(id, "page"))
}

mod_volatility_server <- function(id, filters, data_timestamp) {
  shiny::moduleServer(id, function(input, output, session) {
    page_data <- shiny::reactive(ea_mock_volatility_data(filters()))

    mod_kpi_strip_server("kpi_strip", kpis = shiny::reactive(page_data()$kpis))
    mod_footer_notes_server(
      "footer_notes",
      notes = shiny::reactive(list(
        notes = page_data()$notes,
        assumptions = page_data()$assumptions,
        timestamp = data_timestamp()
      )),
      title = "Vol surface notes"
    )

    output$page <- shiny::renderUI({
      ns <- session$ns
      current_filters <- filters()

      if (length(current_filters$commodities) == 0L) {
        return(htmltools::tagList(
          ea_selected_filters_ribbon(current_filters, data_timestamp()),
          ea_empty_state_card(
            title = "Select products to populate vol views",
            body = "Vol Surface tracks ATMF term structure, vol history, and smile/skew across the selected products.",
            hint = "All vol views key off the same product selection and lookback."
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
            title = "ATMF Term Structure",
            subtitle = "ATMF volatility by contract month.",
            output_id = ns("vol_term"),
            height = "330px"
          ),
          ea_plotly_card(
            title = "Vol History",
            subtitle = "Implied and realized regime history.",
            output_id = ns("vol_time"),
            height = "330px"
          )
        ),
        bslib::layout_columns(
          col_widths = c(8, 4),
          ea_plotly_card(
            title = "Volatility Surface",
            subtitle = "Surface grid by contract month and date.",
            output_id = ns("vol_surface"),
            height = "360px"
          ),
          ea_plotly_card(
            title = "Skew / Smile",
            subtitle = "Strike-based smile view for selected products.",
            output_id = ns("vol_skew"),
            height = "360px"
          )
        ),
        mod_footer_notes_ui(ns("footer_notes"))
      )
    })

    output$vol_term <- plotly::renderPlotly({
      chart_data <- page_data()$vol_term
      palette <- ea_market_palette()
      fig <- plotly::plot_ly()

      for (market in unique(chart_data$market)) {
        df <- chart_data[chart_data$market == market, , drop = FALSE]
        fig <- fig |>
          plotly::add_lines(
            data = df,
            x = ~tenor,
            y = ~vol,
            name = unique(df$label),
            line = list(color = unname(palette[[market]]), width = 2),
            hovertemplate = "Tenor %{x}<br>%{y:.2%}<extra>%{fullData.name}</extra>"
          )
      }

      ea_plotly_layout(fig, x_title = "Contract month", y_title = "ATMF vol")
    })

    output$vol_time <- plotly::renderPlotly({
      chart_data <- page_data()$vol_time
      palette <- ea_market_palette()
      fig <- plotly::plot_ly()

      for (market in unique(chart_data$market)) {
        df <- chart_data[chart_data$market == market, , drop = FALSE]
        fig <- fig |>
          plotly::add_lines(
            data = df,
            x = ~date,
            y = ~vol,
            name = unique(df$label),
            line = list(color = unname(palette[[market]]), width = 2),
            hovertemplate = "%{x|%d %b %Y}<br>%{y:.2%}<extra>%{fullData.name}</extra>"
          )
      }

      ea_plotly_layout(fig, x_title = NULL, y_title = "ATMF vol")
    })

    output$vol_surface <- plotly::renderPlotly({
      chart_data <- page_data()$vol_surface
      surface_matrix <- stats::xtabs(vol ~ tenor + date, data = chart_data)

      fig <- plotly::plot_ly(
        x = colnames(surface_matrix),
        y = rownames(surface_matrix),
        z = unclass(surface_matrix),
        type = "heatmap",
        colors = c("#17202b", "#5a85c8", "#d2a157"),
        hovertemplate = "Date %{x}<br>Tenor %{y}<br>%{z:.2%}<extra>surface</extra>"
      )

      ea_plotly_layout(fig, x_title = NULL, y_title = "Contract month", hovermode = "closest")
    })

    output$vol_skew <- plotly::renderPlotly({
      chart_data <- page_data()$skew
      palette <- ea_market_palette()
      fig <- plotly::plot_ly()

      for (market in unique(chart_data$market)) {
        df <- chart_data[chart_data$market == market, , drop = FALSE]
        fig <- fig |>
          plotly::add_lines(
            data = df,
            x = ~moneyness,
            y = ~vol,
            name = unique(df$label),
            line = list(color = unname(palette[[market]]), width = 2),
            hovertemplate = "Moneyness %{x:.0%}<br>%{y:.2%}<extra>%{fullData.name}</extra>"
          )
      }

      ea_plotly_layout(fig, x_title = "Moneyness", y_title = "Implied vol", hovermode = "closest")
    })
  })
}
