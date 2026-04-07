ea_inventory_trace_color <- function(product) {
  colors <- c(
    crude = "#4da3a3",
    distillate = "#b98054",
    gasoline = "#d36e70",
    ng = "#5a85c8"
  )

  unname(colors[[product]] %||% ea_accent_color())
}

ea_inventory_history_figure <- function(df, meta, show_legend = TRUE) {
  color <- ea_inventory_trace_color(meta$product[[1]])
  reference <- df[!is.na(df$avg_5yr), , drop = FALSE]
  fig <- plotly::plot_ly()

  if (nrow(reference) > 0L) {
    fig <- fig %>%
      plotly::add_ribbons(
        data = reference,
        x = ~date,
        ymin = ~min_5yr,
        ymax = ~max_5yr,
        name = "Band",
        line = list(color = "transparent"),
        fillcolor = "rgba(77,163,163,0.12)",
        hoverinfo = "skip",
        showlegend = show_legend
      ) %>%
      plotly::add_lines(
        data = reference,
        x = ~date,
        y = ~avg_5yr,
        name = "5Y",
        line = list(color = "#7f8b99", width = 1.5, dash = "dot"),
        hovertemplate = "%{x|%d %b %Y}<br>5Y: %{y:,.0f}<extra></extra>",
        showlegend = show_legend
      )
  }

  fig %>%
    plotly::add_lines(
      data = df,
      x = ~date,
      y = ~current,
      name = "Level",
      line = list(color = color, width = 2.4),
      hovertemplate = "%{x|%d %b %Y}<br>Level: %{y:,.0f}<extra></extra>",
      showlegend = show_legend
    ) %>%
    ea_plotly_layout(fig, x_title = NULL, y_title = "Inventory")
}

ea_inventory_deviation_figure <- function(df, meta, show_legend = TRUE) {
  color <- ea_inventory_trace_color(meta$product[[1]])

  plotly::plot_ly() %>%
    plotly::add_lines(
      data = df,
      x = ~date,
      y = ~deviation,
      name = "Gap",
      line = list(color = color, width = 2.2),
      fill = "tozeroy",
      fillcolor = "rgba(77,163,163,0.12)",
      customdata = ~cbind(current, avg_5yr),
      hovertemplate = paste(
        "%{x|%d %b %Y}",
        "<br>Gap: %{y:,.0f}",
        "<br>Level: %{customdata[0]:,.0f}",
        "<br>5Y: %{customdata[1]:,.0f}",
        "<extra></extra>"
      ),
      showlegend = show_legend
    ) %>%
    plotly::layout(
      shapes = list(
        list(
          type = "line",
          x0 = min(df$date),
          x1 = max(df$date),
          y0 = 0,
          y1 = 0,
          line = list(color = "rgba(148,163,184,0.35)", width = 1, dash = "dash")
        )
      )
    ) %>%
    ea_plotly_layout(x_title = NULL, y_title = "Gap")
}

mod_fundamentals_ui <- function(id) {
  shiny::uiOutput(shiny::NS(id, "page"))
}

mod_fundamentals_server <- function(id, filters, data_timestamp) {
  shiny::moduleServer(id, function(input, output, session) {
    page_data <- shiny::reactive({ ea_calc_fundamentals(filters()) })

    selected_market <- shiny::reactive({
      inventory_markets <- page_data()$inventory_markets

      if (nrow(inventory_markets) == 0L) {
        return(NA_character_)
      }

      current <- ea_coalesce(input$fundamental_market, inventory_markets$market[[1]])
      if (current %in% inventory_markets$market) {
        current
      } else {
        inventory_markets$market[[1]]
      }
    })

    selected_meta <- shiny::reactive({
      inventory_markets <- page_data()$inventory_markets
      inventory_markets[inventory_markets$market == selected_market(), , drop = FALSE]
    })

    selected_series <- shiny::reactive({
      inventory_history <- page_data()$inventory_history
      inventory_history[inventory_history$market == selected_market(), , drop = FALSE]
    })

    output$page <- shiny::renderUI({
      ns <- session$ns
      inventory_markets <- page_data()$inventory_markets
      current_selection <- isolate(input$fundamental_market)
      selected_choice <- if (
        !is.null(current_selection) &&
          length(current_selection) == 1L &&
          current_selection %in% inventory_markets$market
      ) {
        current_selection
      } else if (nrow(inventory_markets) > 0L) {
        inventory_markets$market[[1]]
      } else {
        character(0)
      }

      if (nrow(inventory_markets) == 0L) {
        return(htmltools::tagList(
          ea_empty_state_card(
            title = "No inventory",
            body = "Adjust the market filter."
          )
        ))
      }

      htmltools::tagList(
        htmltools::tags$div(
          class = "ea-toolbar",
          htmltools::tags$div(
            class = "ea-toolbar__row",
            htmltools::tags$div(
              class = "ea-toolbar__field ea-toolbar__field--sm",
              ea_toolbar_select(
                input_id = ns("fundamental_market"),
                label = "Market",
                choices = stats::setNames(inventory_markets$market, inventory_markets$market),
                selected = selected_choice
              )
            )
          )
        ),
        bslib::layout_columns(
          col_widths = c(12),
          ea_plotly_card(
            "Inventory",
            subtitle = "Current inventory level versus the recent historical range",
            output_id = ns("inventory_history_chart"),
            height = "300px"
          )
        ),
        bslib::layout_columns(
          col_widths = c(12),
          ea_plotly_card(
            "Gap vs 5Y",
            subtitle = "Deviation from the rolling five-year average for the selected market",
            output_id = ns("inventory_deviation_chart"),
            height = "260px"
          )
        )
      )
    })

    output$inventory_history_chart <- plotly::renderPlotly({
      meta <- selected_meta()
      series <- selected_series()
      fig <- plotly::plot_ly()

      if (nrow(meta) == 0L || nrow(series) == 0L) {
        return(ea_plotly_layout(fig, x_title = NULL, y_title = "Inventory"))
      }

      ea_inventory_history_figure(series, meta, show_legend = TRUE)
    })

    output$inventory_deviation_chart <- plotly::renderPlotly({
      meta <- selected_meta()
      series <- selected_series()
      fig <- plotly::plot_ly()

      if (nrow(meta) == 0L || nrow(series) == 0L) {
        return(ea_plotly_layout(fig, x_title = NULL, y_title = "Gap"))
      }

      deviation <- series[!is.na(series$deviation), , drop = FALSE]
      if (nrow(deviation) == 0L) {
        return(ea_plotly_layout(fig, x_title = NULL, y_title = "Gap"))
      }

      ea_inventory_deviation_figure(deviation, meta, show_legend = FALSE)
    })
  })
}
