mod_global_filters_ui <- function(id) {
  ns <- shiny::NS(id)

  htmltools::tags$section(
    class = "ea-toolbar",
    htmltools::tags$div(
      class = "ea-toolbar__row ea-toolbar__row--primary",
      htmltools::tags$div(
        class = "ea-toolbar__field ea-toolbar__field--sm",
        shinyWidgets::pickerInput(
          inputId = ns("market_complex"),
          label = "Complex",
          choices = "Cross-Commodity",
          selected = "Cross-Commodity",
          options = list(`style` = "btn-dark")
        )
      ),
      htmltools::tags$div(
        class = "ea-toolbar__field ea-toolbar__field--lg",
        shinyWidgets::pickerInput(
          inputId = ns("commodities"),
          label = "Products",
          choices = character(0),
          multiple = TRUE,
          options = list(
            `selected-text-format` = "values"
          )
        )
      ),
      htmltools::tags$div(
        class = "ea-toolbar__field ea-toolbar__field--sm",
        shinyWidgets::pickerInput(
          inputId = ns("comparison_commodity"),
          label = "Benchmark",
          choices = character(0)
        )
      ),
      htmltools::tags$div(
        class = "ea-toolbar__field ea-toolbar__field--date",
        shiny::dateRangeInput(
          inputId = ns("date_range"),
          label = "Date range",
          start = Sys.Date() - 365,
          end = Sys.Date()
        )
      ),
      htmltools::tags$div(
        class = "ea-toolbar__field ea-toolbar__field--xs",
        shinyWidgets::pickerInput(
          inputId = ns("rolling_window"),
          label = "Lookback",
          choices = c("21D", "63D", "126D", "252D"),
          selected = "63D"
        )
      ),
      htmltools::tags$div(
        class = "ea-toolbar__controls",
        htmltools::tags$button(
          type = "button",
          class = "btn btn-sm btn-outline-secondary ea-toolbar__toggle",
          `data-bs-toggle` = "collapse",
          `data-bs-target` = paste0("#", ns("advanced_filters")),
          `aria-expanded` = "false",
          shiny::icon("sliders"),
          "More"
        )
      )
    ),
    htmltools::tags$div(
      id = ns("advanced_filters"),
      class = "collapse ea-toolbar__advanced",
      htmltools::tags$div(
        class = "ea-toolbar__row ea-toolbar__row--advanced",
        htmltools::tags$div(
          class = "ea-toolbar__field ea-toolbar__field--sm",
          shinyWidgets::pickerInput(
            inputId = ns("tenor_bucket"),
            label = "Tenor set",
            choices = c("Front", "Quarterly", "Seasonal", "Calendar"),
            selected = c("Front", "Quarterly"),
            multiple = TRUE
          )
        ),
        htmltools::tags$div(
          class = "ea-toolbar__field ea-toolbar__field--range",
          shiny::sliderInput(
            inputId = ns("expiry_range"),
            label = "Contract months",
            min = 1,
            max = 36,
            value = c(1, 12)
          )
        ),
        htmltools::tags$div(
          class = "ea-toolbar__field ea-toolbar__field--lg",
          shinyWidgets::pickerInput(
            inputId = ns("regime"),
            label = "Regime",
            choices = c("All Regimes", "Contango", "Backwardation", "Vol Shock", "Inventory Stress", "Shoulder Season"),
            selected = "All Regimes",
            multiple = TRUE
          )
        ),
        htmltools::tags$div(
          class = "ea-toolbar__field ea-toolbar__field--sm",
          shinyWidgets::pickerInput(
            inputId = ns("scenario_preset"),
            label = "Shock set",
            choices = c("Base Case", "Supply Squeeze", "Refined Product Dislocation", "Gas Vol Shock"),
            selected = "Base Case"
          )
        )
      )
    )
  )
}

mod_global_filters_server <- function(id, market_catalog, reset_trigger = shiny::reactive(NULL)) {
  shiny::moduleServer(id, function(input, output, session) {
    defaults <- shiny::reactive(ea_global_filter_defaults(market_catalog()))

    update_comparison_choices <- function(selected_commodities) {
      catalog <- market_catalog()
      choices <- stats::setNames(catalog$market, catalog$label)
      selected <- shiny::isolate(input$comparison_commodity)
      candidate <- if (!is.null(selected) && nzchar(selected) && !selected %in% selected_commodities) {
        selected
      } else {
        ea_default_comparison(selected_commodities, catalog)
      }

      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "comparison_commodity",
        choices = choices,
        selected = candidate
      )
    }

    apply_defaults <- function() {
      catalog <- market_catalog()
      default_values <- defaults()
      date_bounds <- ea_available_date_bounds()

      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "market_complex",
        choices = c("Cross-Commodity", unique(catalog$complex)),
        selected = default_values$market_complex
      )

      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "commodities",
        choices = stats::setNames(catalog$market, catalog$label),
        selected = default_values$commodities
      )

      update_comparison_choices(default_values$commodities)

      shiny::updateDateRangeInput(
        session = session,
        inputId = "date_range",
        start = default_values$date_range[1],
        end = default_values$date_range[2],
        min = date_bounds[1],
        max = date_bounds[2]
      )

      shinyWidgets::updatePickerInput(session, "rolling_window", selected = default_values$rolling_window)
      shinyWidgets::updatePickerInput(session, "tenor_bucket", selected = default_values$tenor_bucket)
      shiny::updateSliderInput(session, "expiry_range", value = default_values$expiry_range)
      shinyWidgets::updatePickerInput(session, "regime", selected = default_values$regime)
      shinyWidgets::updatePickerInput(session, "scenario_preset", selected = default_values$scenario_preset)
    }

    shiny::observeEvent(market_catalog(), {
      apply_defaults()
    }, ignoreInit = FALSE)

    shiny::observeEvent(reset_trigger(), {
      apply_defaults()
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$market_complex, {
      catalog <- market_catalog()
      selected_complex <- ea_coalesce(input$market_complex, defaults()$market_complex)

      available <- if (identical(selected_complex, "Cross-Commodity")) {
        catalog
      } else {
        dplyr::filter(catalog, .data$complex == selected_complex)
      }

      selected <- intersect(ea_coalesce(input$commodities, character(0)), available$market)
      if (length(selected) == 0L && nrow(available) > 0L) {
        selected <- if (identical(selected_complex, defaults()$market_complex)) {
          defaults()$commodities
        } else {
          utils::head(available$market, min(2, nrow(available)))
        }
      }

      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "commodities",
        choices = stats::setNames(available$market, available$label),
        selected = selected
      )

      update_comparison_choices(selected)
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$commodities, {
      update_comparison_choices(ea_coalesce(input$commodities, character(0)))
    }, ignoreInit = TRUE)

    shiny::reactive({
      default_values <- defaults()

      list(
        market_complex = ea_coalesce(input$market_complex, default_values$market_complex),
        commodities = ea_coalesce(input$commodities, default_values$commodities),
        comparison_commodity = ea_coalesce(
          input$comparison_commodity,
          ea_default_comparison(ea_coalesce(input$commodities, default_values$commodities), market_catalog())
        ),
        date_range = ea_coalesce(input$date_range, default_values$date_range),
        rolling_window = ea_coalesce(input$rolling_window, default_values$rolling_window),
        tenor_bucket = ea_coalesce(input$tenor_bucket, default_values$tenor_bucket),
        expiry_range = ea_coalesce(input$expiry_range, default_values$expiry_range),
        regime = ea_coalesce(input$regime, default_values$regime),
        scenario_preset = ea_coalesce(input$scenario_preset, default_values$scenario_preset)
      )
    })
  })
}
