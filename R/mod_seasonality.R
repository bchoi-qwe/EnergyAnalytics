mod_seasonality_ui <- function(id) {
  shiny::uiOutput(shiny::NS(id, "page"))
}

mod_seasonality_server <- function(id, filters, data_timestamp) {
  shiny::moduleServer(id, function(input, output, session) {
    page_data <- shiny::reactive(ea_mock_seasonality_data(filters()))

    mod_footer_notes_server(
      "footer_notes",
      notes = shiny::reactive(list(
        notes = page_data()$notes,
        assumptions = page_data()$assumptions,
        timestamp = data_timestamp()
      )),
      title = "Seasonal notes"
    )

    output$page <- shiny::renderUI({
      ns <- session$ns
      current_filters <- filters()

      if (length(current_filters$commodities) == 0L) {
        return(htmltools::tagList(
          ea_selected_filters_ribbon(current_filters, data_timestamp()),
          ea_empty_state_card(
            title = "Select products to populate seasonal views",
            body = "Seasonality compares recurring structure in outrights, calendar spreads, and year-on-year paths.",
            hint = "Use the seasonal overlay toggle to keep calendar structure explicit."
          ),
          mod_footer_notes_ui(ns("footer_notes"))
        ))
      }

      htmltools::tagList(
        ea_selected_filters_ribbon(current_filters, data_timestamp()),
        bslib::layout_columns(
          col_widths = c(7, 5),
          ea_plotly_card(
            title = "Seasonal Curve",
            subtitle = "Monthly seasonal signature across the selected products.",
            output_id = ns("seasonal_profile"),
            height = "330px"
          ),
          ea_plotly_card(
            title = "Calendar Heatmap",
            subtitle = "Month-by-year seasonal structure.",
            output_id = ns("seasonal_heatmap"),
            height = "330px"
          )
        ),
        bslib::layout_columns(
          col_widths = c(8, 4),
          ea_plotly_card(
            title = "Year-on-Year",
            subtitle = "Relative path comparison across recent years.",
            output_id = ns("yoy_compare"),
            height = "330px"
          ),
          ea_standard_card(
            title = "Seasonal Color",
            subtitle = "Seasonal spread context and market color.",
            class = "ea-card--commentary",
            shiny::uiOutput(ns("interpretation"))
          )
        ),
        ea_plotly_card(
          title = "Seasonal Spread",
          subtitle = "Cross-month seasonal spread monitor.",
          output_id = ns("seasonal_spread"),
          height = "300px"
        ),
        mod_footer_notes_ui(ns("footer_notes"))
      )
    })

    output$seasonal_profile <- plotly::renderPlotly({
      chart_data <- page_data()$seasonal_profile
      palette <- ea_market_palette()
      fig <- plotly::plot_ly()

      for (market in unique(chart_data$market)) {
        df <- chart_data[chart_data$market == market, , drop = FALSE]
        fig <- fig |>
          plotly::add_lines(
            data = df,
            x = ~month,
            y = ~value,
            name = unique(df$label),
            line = list(color = unname(palette[[market]]), width = 2),
            hovertemplate = "%{x}<br>%{y:.2f}<extra>%{fullData.name}</extra>"
          )
      }

      ea_plotly_layout(fig, x_title = NULL, y_title = "Seasonal index")
    })

    output$seasonal_heatmap <- plotly::renderPlotly({
      chart_data <- page_data()$seasonal_heatmap
      season_matrix <- stats::xtabs(value ~ year + month, data = chart_data)

      fig <- plotly::plot_ly(
        x = colnames(season_matrix),
        y = rownames(season_matrix),
        z = unclass(season_matrix),
        type = "heatmap",
        colors = c("#b35c60", "#17202b", "#4da3a3"),
        hovertemplate = "%{y} %{x}<br>%{z:.2f}<extra>seasonality</extra>"
      ) |>
        plotly::layout(yaxis = list(autorange = "reversed"))

      ea_plotly_layout(fig, x_title = NULL, y_title = NULL, hovermode = "closest")
    })

    output$yoy_compare <- plotly::renderPlotly({
      chart_data <- page_data()$yoy_compare
      palette <- c("#4da3a3", "#d2a157", "#5a85c8", "#d36e70")
      fig <- plotly::plot_ly()

      for (i in seq_along(unique(chart_data$year))) {
        current_year <- unique(chart_data$year)[[i]]
        df <- chart_data[chart_data$year == current_year, , drop = FALSE]
        fig <- fig |>
          plotly::add_lines(
            data = df,
            x = ~week,
            y = ~value,
            name = as.character(current_year),
            line = list(color = palette[[i]], width = 2),
            hovertemplate = "Week %{x}<br>%{y:.2f}<extra>%{fullData.name}</extra>"
          )
      }

      ea_plotly_layout(fig, x_title = "Week of year", y_title = "Seasonal index")
    })

    output$seasonal_spread <- plotly::renderPlotly({
      chart_data <- page_data()$seasonal_spread

      fig <- plotly::plot_ly(
        data = chart_data,
        x = ~month,
        y = ~spread,
        type = "bar",
        marker = list(color = "#4da3a3"),
        hovertemplate = "%{x}<br>%{y:.2f}<extra>seasonal spread</extra>"
      )

      ea_plotly_layout(fig, x_title = NULL, y_title = "Spread", hovermode = "closest")
    })

    output$interpretation <- shiny::renderUI({
      htmltools::tags$div(
        class = "ea-commentary",
        htmltools::tags$ul(lapply(page_data()$interpretation, htmltools::tags$li))
      )
    })
  })
}
