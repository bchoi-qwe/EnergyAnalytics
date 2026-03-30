app_server <- function(input, output, session) {
  data_timestamp <- shiny::reactiveVal(ea_dashboard_timestamp())
  market_catalog <- shiny::reactive(ea_market_catalog())
  nav_registry <- ea_nav_registry()

  filters <- mod_global_filters_server(
    id = "global_filters",
    market_catalog = market_catalog,
    reset_trigger = shiny::reactive(input$reset_filters)
  )

  shiny::observeEvent(input$rail_nav_target, {
    shiny::updateTabsetPanel(
      session = session,
      inputId = "main_nav",
      selected = input$rail_nav_target
    )
  }, ignoreInit = TRUE)

  output$rail_nav <- shiny::renderUI({
    selected_page <- ea_coalesce(input$main_nav, "overview")

    htmltools::tags$div(
      class = "ea-rail__nav",
      lapply(seq_len(nrow(nav_registry)), function(i) {
        item <- nav_registry[i, ]

        htmltools::tags$button(
          type = "button",
          class = paste(
            "ea-rail__nav-btn",
            if (identical(item$value, selected_page)) "is-active"
          ),
          onclick = sprintf(
            "Shiny.setInputValue('rail_nav_target', '%s', {priority: 'event'})",
            item$value
          ),
          htmltools::tags$span(class = "ea-rail__nav-code", item$code),
          htmltools::tags$span(
            class = "ea-rail__nav-copy",
            htmltools::tags$span(class = "ea-rail__nav-label", item$label)
          )
        )
      })
    )
  })

  output$topbar_page <- shiny::renderUI({
    selected_page <- ea_coalesce(input$main_nav, "overview")
    page_row <- nav_registry[nav_registry$value == selected_page, , drop = FALSE]

    htmltools::tags$div(
      class = "ea-topbar__page",
      htmltools::tags$span(class = "ea-topbar__page-code", page_row$code[[1]]),
      htmltools::tags$span(class = "ea-topbar__page-label", page_row$label[[1]])
    )
  })

  output$header_timestamp <- shiny::renderText({
    ea_format_timestamp(data_timestamp())
  })

  shiny::observeEvent(input$refresh_data, {
    shiny::showNotification(
      "Refresh placeholder. Connect this action to the backend snapshot/update pipeline later.",
      type = "message"
    )
  })

  shiny::observeEvent(input$export_view, {
    shiny::showNotification(
      "Export placeholder. Attach report, image, or table exports once backend outputs are available.",
      type = "message"
    )
  })

  mod_overview_server("overview", filters = filters, data_timestamp = data_timestamp)
  mod_forward_curves_server("forward_curves", filters = filters, data_timestamp = data_timestamp)
  mod_volatility_server("volatility", filters = filters, data_timestamp = data_timestamp)
  mod_codynamics_server("codynamics", filters = filters, data_timestamp = data_timestamp)
  mod_seasonality_server("seasonality", filters = filters, data_timestamp = data_timestamp)
  mod_hedging_server("hedging", filters = filters, data_timestamp = data_timestamp)
  mod_scenarios_server("scenarios", filters = filters, data_timestamp = data_timestamp)
}
