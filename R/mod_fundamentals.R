mod_fundamentals_ui <- function(id) {
  shiny::uiOutput(shiny::NS(id, "page"))
}

mod_fundamentals_server <- function(id, filters, data_timestamp) {
  shiny::moduleServer(id, function(input, output, session) {
    page_data <- shiny::reactive(ea_mock_fundamentals_data(filters()))

    mod_kpi_strip_server("kpi_strip", kpis = shiny::reactive(page_data()$kpis))
    mod_detail_table_server(
      "release_table",
      data = shiny::reactive(page_data()$release_calendar),
      title = "Release Calendar",
      subtitle = "Scheduled data releases and market catalysts."
    )
    mod_detail_table_server(
      "signal_table",
      data = shiny::reactive(page_data()$physical_signals),
      title = "Physical Signals",
      subtitle = "Inventories, utilization, and prompt physical indicators."
    )
    mod_footer_notes_server(
      "footer_notes",
      notes = shiny::reactive(list(
        notes = page_data()$notes,
        assumptions = page_data()$assumptions,
        timestamp = data_timestamp()
      )),
      title = "Fundamentals notes"
    )

    output$page <- shiny::renderUI({
      ns <- session$ns
      current_filters <- filters()
      page_bundle <- page_data()

      if (length(current_filters$commodities) == 0L) {
        return(htmltools::tagList(
          ea_selected_filters_ribbon(current_filters, data_timestamp()),
          ea_empty_state_card(
            title = "Select products to populate fundamentals",
            body = "Fundamentals is built for inventories, storage, supply-demand context, headline flow, and scheduled releases.",
            hint = "Use the shared product selection to populate the storage, balance, and catalyst panels."
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
            title = page_bundle$inventory_title,
            subtitle = page_bundle$inventory_subtitle,
            output_id = ns("inventory_chart"),
            height = "320px"
          ),
          ea_standard_card(
            title = "Headline Monitor",
            subtitle = "News flow, policy catalysts, and infrastructure updates.",
            class = "ea-card--commentary",
            shiny::uiOutput(ns("headline_feed"))
          )
        ),
        bslib::layout_columns(
          col_widths = c(6, 6),
          ea_plotly_card(
            title = "Regional Inventory / Storage",
            subtitle = "Regional and hub positioning versus five-year norms.",
            output_id = ns("regional_chart"),
            height = "300px"
          ),
          ea_plotly_card(
            title = "Balance Monitor",
            subtitle = "Illustrative physical balance and utilization trend.",
            output_id = ns("balance_chart"),
            height = "300px"
          )
        ),
        bslib::layout_columns(
          col_widths = c(6, 6),
          mod_detail_table_ui(ns("release_table")),
          mod_detail_table_ui(ns("signal_table"))
        ),
        mod_footer_notes_ui(ns("footer_notes"))
      )
    })

    output$inventory_chart <- plotly::renderPlotly({
      chart_data <- page_data()$inventory_curve
      fig <- plotly::plot_ly()

      fig <- fig |>
        plotly::add_ribbons(
          data = chart_data,
          x = ~date,
          ymin = ~low_5y,
          ymax = ~high_5y,
          name = "5Y range",
          fillcolor = "rgba(77, 163, 163, 0.12)",
          line = list(color = "rgba(0,0,0,0)"),
          hoverinfo = "skip"
        ) |>
        plotly::add_lines(
          data = chart_data,
          x = ~date,
          y = ~avg_5y,
          name = "5Y avg",
          line = list(color = "#7f8b99", width = 1.5, dash = "dot"),
          hovertemplate = "%{x|%d %b %Y}<br>%{y:.1f}<extra>5Y avg</extra>"
        ) |>
        plotly::add_lines(
          data = chart_data,
          x = ~date,
          y = ~current,
          name = "Current",
          line = list(color = "#4da3a3", width = 2.4),
          hovertemplate = "%{x|%d %b %Y}<br>%{y:.1f}<extra>current</extra>"
        )

      ea_plotly_layout(fig, x_title = NULL, y_title = page_data()$inventory_axis)
    })

    output$regional_chart <- plotly::renderPlotly({
      chart_data <- page_data()$regional_storage
      colors <- ifelse(chart_data$delta_vs_5y >= 0, "#4da3a3", "#c96b6b")

      fig <- plotly::plot_ly(
        data = chart_data,
        x = ~location,
        y = ~delta_vs_5y,
        type = "bar",
        marker = list(color = colors),
        customdata = ~level,
        hovertemplate = "%{x}<br>vs 5Y: %{y:.1f}<br>Level: %{customdata:.1f}<extra></extra>"
      )

      ea_plotly_layout(fig, x_title = NULL, y_title = page_data()$regional_axis, hovermode = "closest")
    })

    output$balance_chart <- plotly::renderPlotly({
      chart_data <- page_data()$balance_monitor
      fig <- plotly::plot_ly() |>
        plotly::add_bars(
          data = chart_data,
          x = ~month,
          y = ~balance,
          name = "Implied balance",
          marker = list(color = "#4da3a3"),
          hovertemplate = "%{x}<br>%{y:.2f}<extra>balance</extra>"
        ) |>
        plotly::add_lines(
          data = chart_data,
          x = ~month,
          y = ~utilization,
          name = "Utilization",
          yaxis = "y2",
          line = list(color = "#d2a157", width = 2.1),
          hovertemplate = "%{x}<br>%{y:.1f}%<extra>utilization</extra>"
        ) |>
        plotly::layout(
          yaxis2 = list(
            overlaying = "y",
            side = "right",
            title = "Utilization (%)",
            color = "#9aa6b2",
            showgrid = FALSE
          )
        )

      ea_plotly_layout(fig, x_title = NULL, y_title = page_data()$balance_axis, hovermode = "x unified")
    })

    output$headline_feed <- shiny::renderUI({
      headlines <- page_data()$headlines

      htmltools::tags$div(
        class = "ea-headline-feed",
        lapply(seq_len(nrow(headlines)), function(i) {
          item <- headlines[i, ]

          htmltools::tags$div(
            class = "ea-headline-item",
            htmltools::tags$div(
              class = "ea-headline-item__meta",
              htmltools::tags$span(class = "ea-headline-item__time", item$time),
              ea_badge(item$source, tone = "neutral"),
              ea_badge(item$tag, tone = item$tone)
            ),
            htmltools::tags$div(class = "ea-headline-item__headline", item$headline),
            htmltools::tags$div(class = "ea-headline-item__summary", item$summary)
          )
        })
      )
    })
  })
}
