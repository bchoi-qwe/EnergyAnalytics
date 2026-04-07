mod_global_filters_ui <- function(id) {
  ns <- shiny::NS(id)
  catalog <- ea_market_catalog()
  date_bounds <- ea_available_date_bounds()

  htmltools::tags$div(
    class = "ea-toolbar ea-toolbar--global",
    htmltools::tags$div(
      class = "ea-toolbar__row",
      htmltools::tags$div(
        class = "ea-toolbar__field ea-toolbar__field--lg",
        ea_toolbar_multiselect(
          input_id = ns("commodities"),
          label = "Products",
          choices = stats::setNames(catalog$market, catalog$label),
          selected = catalog$market
        )
      ),
      htmltools::tags$div(
        class = "ea-toolbar__field ea-toolbar__field--date",
        shiny::dateRangeInput(
          inputId = ns("date_range"),
          label = "Date range",
          start = max(date_bounds[2] - 365, date_bounds[1]),
          end = date_bounds[2],
          min = date_bounds[1],
          max = date_bounds[2]
        )
      )
    )
  )
}

mod_global_filters_server <- function(id, market_catalog, reset_trigger = shiny::reactive(NULL)) {
  shiny::moduleServer(id, function(input, output, session) {
    defaults <- shiny::reactive(ea_global_filter_defaults(market_catalog()))

    apply_defaults <- function() {
      catalog <- market_catalog()
      default_values <- defaults()
      date_bounds <- ea_available_date_bounds()

      shiny::updateDateRangeInput(
        session = session,
        inputId = "date_range",
        start = default_values$date_range[1],
        end = default_values$date_range[2],
        min = date_bounds[1],
        max = date_bounds[2]
      )

      shiny::updateSelectizeInput(
        session = session,
        inputId = "commodities",
        choices = stats::setNames(catalog$market, catalog$label),
        selected = default_values$commodities,
        server = FALSE
      )
    }

    shiny::observeEvent(market_catalog(), {
      apply_defaults()
    }, ignoreInit = TRUE)

    shiny::observeEvent(reset_trigger(), {
      apply_defaults()
    }, ignoreInit = TRUE)

    shiny::reactive({
      default_values <- defaults()

      list(
        market_complex = default_values$market_complex,
        commodities = ea_coalesce(input$commodities, default_values$commodities),
        comparison_commodity = default_values$comparison_commodity,
        date_range = ea_coalesce(input$date_range, default_values$date_range),
        rolling_window = default_values$rolling_window,
        tenor_bucket = default_values$tenor_bucket,
        expiry_range = default_values$expiry_range,
        regime = default_values$regime
      )
    })
  })
}
