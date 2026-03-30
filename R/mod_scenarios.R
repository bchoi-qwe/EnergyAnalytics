mod_scenarios_ui <- function(id) {
  shiny::uiOutput(shiny::NS(id, "page"))
}

mod_scenarios_server <- function(id, filters, data_timestamp) {
  shiny::moduleServer(id, function(input, output, session) {
    preset_table <- ea_mock_scenario_data(
      filters = ea_global_filter_defaults(),
      shocks = list(flat = 0, vol = 0, spread = 0)
    )$presets

    scenario_data <- shiny::reactive({
      ea_mock_scenario_data(
        filters = filters(),
        shocks = list(
          flat = ea_coalesce(input$flat_price_shock, 0),
          vol = ea_coalesce(input$vol_shock, 0),
          spread = ea_coalesce(input$spread_shock, 0)
        )
      )
    })

    mod_kpi_strip_server("kpi_strip", kpis = shiny::reactive(scenario_data()$kpis))
    mod_footer_notes_server(
      "footer_notes",
      notes = shiny::reactive(list(
        notes = scenario_data()$notes,
        assumptions = scenario_data()$assumptions,
        timestamp = data_timestamp()
      )),
      title = "Scenario analysis notes"
    )

    for (i in seq_len(nrow(preset_table))) {
      local({
        preset <- preset_table[i, ]

        shiny::observeEvent(input[[paste0("load_", preset$id)]], {
          shiny::updateSliderInput(session, "flat_price_shock", value = preset$flat)
          shiny::updateSliderInput(session, "vol_shock", value = preset$vol)
          shiny::updateSliderInput(session, "spread_shock", value = preset$spread)
        }, ignoreInit = TRUE)
      })
    }

    output$page <- shiny::renderUI({
      ns <- session$ns
      current_filters <- filters()

      if (length(current_filters$commodities) == 0L) {
        return(htmltools::tagList(
          ea_selected_filters_ribbon(current_filters, data_timestamp()),
          ea_empty_state_card(
            title = "Select products to populate scenario analysis",
            body = "Scenario Analysis applies preset and custom shocks across the selected curve and benchmark structure.",
            hint = "Preset shocks and manual inputs will feed impact and attribution views once backend analytics are attached."
          ),
          mod_footer_notes_ui(ns("footer_notes"))
        ))
      }

      htmltools::tagList(
        ea_selected_filters_ribbon(current_filters, data_timestamp()),
        shiny::uiOutput(ns("preset_cards")),
        ea_standard_card(
          title = "Shock Inputs",
          subtitle = "Custom flat price, volatility, and spread shocks.",
          class = "ea-card--controls",
          full_screen = FALSE,
          bslib::layout_columns(
            col_widths = c(4, 4, 4),
            shiny::sliderInput(
              inputId = ns("flat_price_shock"),
              label = "Flat price",
              min = -3,
              max = 3,
              value = 0,
              step = 0.25
            ),
            shiny::sliderInput(
              inputId = ns("vol_shock"),
              label = "Vol shift (pts)",
              min = -10,
              max = 10,
              value = 0,
              step = 1
            ),
            shiny::sliderInput(
              inputId = ns("spread_shock"),
              label = "Spread shift",
              min = -3,
              max = 3,
              value = 0,
              step = 0.25
            )
          )
        ),
        mod_kpi_strip_ui(ns("kpi_strip")),
        bslib::layout_columns(
          col_widths = c(8, 4),
          ea_plotly_card(
            title = "Shock Impact",
            subtitle = "Base versus shocked term structure.",
            output_id = ns("scenario_impact"),
            height = "360px"
          ),
          ea_plotly_card(
            title = "Impact Attribution",
            subtitle = "Contribution by risk driver.",
            output_id = ns("scenario_propagation"),
            height = "360px"
          )
        ),
        mod_footer_notes_ui(ns("footer_notes"))
      )
    })

    output$preset_cards <- shiny::renderUI({
      ns <- session$ns

      preset_cards <- lapply(seq_len(nrow(preset_table)), function(i) {
        preset <- preset_table[i, ]

        ea_standard_card(
          title = preset$title,
          subtitle = preset$description,
          class = "ea-card--preset",
          full_screen = FALSE,
          htmltools::tags$div(
            class = "ea-preset-card",
            htmltools::tags$div(
              class = "ea-preset-card__stats",
              ea_badge(paste("Flat", preset$flat), tone = "neutral"),
              ea_badge(paste("Vol shift", preset$vol), tone = "neutral"),
              ea_badge(paste("Spread shift", preset$spread), tone = "neutral")
            ),
            shiny::actionButton(
              inputId = ns(paste0("load_", preset$id)),
              label = "Load shock",
              class = "btn btn-sm btn-outline-secondary ea-preset-card__button"
            )
          )
        )
      })

      do.call(
        bslib::layout_columns,
        c(
          preset_cards,
          list(
            col_widths = c(4, 4, 4)
          )
        )
      )
    })

    output$scenario_impact <- plotly::renderPlotly({
      chart_data <- scenario_data()$impact_curve

      fig <- plotly::plot_ly() |>
        plotly::add_lines(
          data = chart_data,
          x = ~tenor,
          y = ~base,
          name = "Base",
          line = list(color = "#7f8b99", width = 1.7, dash = "dot"),
          hovertemplate = "Tenor %{x}<br>%{y:.2f}<extra>%{fullData.name}</extra>"
        ) |>
        plotly::add_lines(
          data = chart_data,
          x = ~tenor,
          y = ~impact,
          name = "Shocked",
          line = list(color = "#4da3a3", width = 2.4),
          hovertemplate = "Tenor %{x}<br>%{y:.2f}<extra>%{fullData.name}</extra>"
        )

      ea_plotly_layout(fig, x_title = "Contract month", y_title = "Curve level")
    })

    output$scenario_propagation <- plotly::renderPlotly({
      chart_data <- scenario_data()$propagation

      fig <- plotly::plot_ly(
        data = chart_data,
        x = ~factor,
        y = ~contribution,
        type = "bar",
        marker = list(color = c("#4da3a3", "#5a85c8", "#d2a157", "#d36e70")),
        hovertemplate = "%{x}<br>%{y:.2f}<extra>contribution</extra>"
      )

      ea_plotly_layout(fig, x_title = NULL, y_title = "Contribution", hovermode = "closest")
    })
  })
}
