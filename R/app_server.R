app_server <- function(input, output, session) {
  data_timestamp <- shiny::reactiveVal(ea_dashboard_timestamp())
  market_catalog <- shiny::reactive(ea_market_catalog())
  nav_registry <- ea_nav_registry(available_only = TRUE)
  if (nrow(nav_registry) == 0L) {
    nav_registry <- ea_nav_registry()
  }
  supported_pages <- nav_registry$value
  default_page <- supported_pages[[1]]

  filters <- mod_global_filters_server(
    id = "global_filters",
    market_catalog = market_catalog
  )

  selected_page <- shiny::reactive({
    main_nav <- input$main_nav
    if (!is.null(main_nav) && length(main_nav) == 1L && main_nav %in% supported_pages) {
      return(main_nav)
    }

    default_page
  })

  output$topbar_page <- shiny::renderUI({
    page_row <- nav_registry[nav_registry$value == selected_page(), , drop = FALSE]
    if (nrow(page_row) == 0L) {
      page_row <- nav_registry[1, , drop = FALSE]
    }

    htmltools::tags$section(
      class = "ea-shell-page",
      htmltools::tags$div(
        class = "ea-shell-page__headline",
        page_row$label[[1]]
      )
    )
  })

  output$topbar_scope <- shiny::renderUI({
    current_filters <- filters()
    selected_count <- length(current_filters$commodities)
    date_range <- current_filters$date_range
    range_label <- if (length(date_range) == 2L && !any(is.na(date_range))) {
      paste(format(date_range[[1]], "%d %b %Y"), "\u2192", format(date_range[[2]], "%d %b %Y"))
    } else {
      "Date range pending"
    }

    htmltools::tags$section(
      class = "ea-shell-scope",
      htmltools::tags$div(
        class = "ea-shell-scope__badges",
        ea_badge(sprintf("%d markets", selected_count), tone = if (selected_count >= 2L) "accent" else "warning"),
        ea_badge(range_label, tone = "neutral")
      )
    )
  })

  output$header_timestamp <- shiny::renderText({
    ea_format_timestamp(data_timestamp())
  })

  if ("forward_curves" %in% supported_pages) mod_forward_curves_server("forward_curves", filters = filters, data_timestamp = data_timestamp)
  if ("volatility" %in% supported_pages) mod_volatility_server("volatility", filters = filters, data_timestamp = data_timestamp)
  if ("options" %in% supported_pages) mod_options_greeks_server("options", filters = filters, data_timestamp = data_timestamp)
  if ("codynamics" %in% supported_pages) mod_codynamics_server("codynamics", filters = filters, data_timestamp = data_timestamp)
  if ("seasonality" %in% supported_pages) mod_seasonality_server("seasonality", filters = filters, data_timestamp = data_timestamp)
  if ("hedging" %in% supported_pages) mod_hedging_server("hedging", filters = filters, data_timestamp = data_timestamp)
}
