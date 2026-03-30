mod_codynamics_ui <- function(id) {
  shiny::uiOutput(shiny::NS(id, "page"))
}

mod_codynamics_server <- function(id, filters, data_timestamp) {
  shiny::moduleServer(id, function(input, output, session) {
    page_data <- shiny::reactive(ea_mock_codynamics_data(filters()))

    mod_footer_notes_server(
      "footer_notes",
      notes = shiny::reactive(list(
        notes = page_data()$notes,
        assumptions = page_data()$assumptions,
        timestamp = data_timestamp()
      )),
      title = "Correlation notes"
    )

    output$page <- shiny::renderUI({
      ns <- session$ns
      current_filters <- filters()

      if (length(current_filters$commodities) == 0L) {
        return(htmltools::tagList(
          ea_selected_filters_ribbon(current_filters, data_timestamp()),
          ea_empty_state_card(
            title = "Select products to populate correlation views",
            body = "Correlations aggregates rolling correlation, intercommodity spreads, and transmission diagnostics across the selected products.",
            hint = "Add at least two products or keep a benchmark product active."
          ),
          mod_footer_notes_ui(ns("footer_notes"))
        ))
      }

      htmltools::tagList(
        ea_selected_filters_ribbon(current_filters, data_timestamp()),
        bslib::layout_columns(
          col_widths = c(7, 5),
          ea_plotly_card(
            title = "Rolling correlation",
            subtitle = "Cross-product linkage through time.",
            output_id = ns("rolling_corr"),
            height = "330px"
          ),
          ea_plotly_card(
            title = "Correlation matrix",
            subtitle = "Cross-product correlation monitor.",
            output_id = ns("dependency_matrix"),
            height = "330px"
          )
        ),
        bslib::layout_columns(
          col_widths = c(8, 4),
          ea_plotly_card(
            title = "Intercommodity spread",
            subtitle = "Spread monitor for decoupling and reconnection.",
            output_id = ns("relative_value"),
            height = "330px"
          ),
          ea_standard_card(
            title = "Lead/Lag & Event Risk",
            subtitle = "Transmission diagnostics and event notes.",
            class = "ea-card--commentary",
            shiny::uiOutput(ns("codynamics_notes"))
          )
        ),
        mod_footer_notes_ui(ns("footer_notes"))
      )
    })

    output$rolling_corr <- plotly::renderPlotly({
      chart_data <- page_data()$rolling_corr
      palette <- c("#4da3a3", "#d2a157", "#5a85c8", "#d36e70")
      fig <- plotly::plot_ly()

      for (i in seq_along(unique(chart_data$pair))) {
        pair_name <- unique(chart_data$pair)[[i]]
        df <- chart_data[chart_data$pair == pair_name, , drop = FALSE]
        fig <- fig |>
          plotly::add_lines(
            data = df,
            x = ~date,
            y = ~value,
            name = pair_name,
            line = list(color = palette[[((i - 1L) %% length(palette)) + 1L]], width = 2),
            hovertemplate = "%{x|%d %b %Y}<br>%{y:.2f}<extra>%{fullData.name}</extra>"
          )
      }

      ea_plotly_layout(fig, x_title = NULL, y_title = "Correlation")
    })

    output$dependency_matrix <- plotly::renderPlotly({
      chart_data <- page_data()$dependency_matrix
      dep_matrix <- stats::xtabs(value ~ y_label + x_label, data = chart_data)

      fig <- plotly::plot_ly(
        x = colnames(dep_matrix),
        y = rownames(dep_matrix),
        z = unclass(dep_matrix),
        type = "heatmap",
        colors = c("#b35c60", "#17202b", "#4da3a3"),
        hovertemplate = "%{y} vs %{x}<br>%{z:.2f}<extra>correlation</extra>"
      ) |>
        plotly::layout(yaxis = list(autorange = "reversed"))

      ea_plotly_layout(fig, x_title = NULL, y_title = NULL, hovermode = "closest")
    })

    output$relative_value <- plotly::renderPlotly({
      chart_data <- page_data()$relative_value

      fig <- plotly::plot_ly(
        data = chart_data,
        x = ~date,
        y = ~spread,
        type = "scatter",
        mode = "lines",
        line = list(color = "#4da3a3", width = 2.2),
        hovertemplate = "%{x|%d %b %Y}<br>%{y:.2f}<extra>relative value</extra>"
      )

      ea_plotly_layout(fig, x_title = NULL, y_title = "Spread", hovermode = "closest")
    })

    output$codynamics_notes <- shiny::renderUI({
      note_bundle <- page_data()

      htmltools::tags$div(
        class = "ea-commentary",
        htmltools::tags$ul(
          lapply(note_bundle$narrative, htmltools::tags$li)
        ),
        reactable::reactable(
          data = note_bundle$lead_lag,
          theme = ea_reactable_theme(),
          compact = TRUE,
          pagination = FALSE,
          searchable = FALSE,
          sortable = FALSE
        )
      )
    })
  })
}
