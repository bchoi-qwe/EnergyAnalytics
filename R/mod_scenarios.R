mod_scenarios_ui <- function(id) {
  shiny::uiOutput(shiny::NS(id, "page"))
}

mod_scenarios_server <- function(id, filters, data_timestamp) {
  shiny::moduleServer(id, function(input, output, session) {
    empty_scenario_data <- function(error_message = NULL) {
      list(
        impact_curve = tibble::tibble(tenor = integer(), base = numeric(), impact = numeric()),
        propagation = tibble::tibble(factor = character(), contribution = numeric()),
        price_simulations = tibble::tibble(t = numeric(), market = character(), sim_id = character(), price = numeric()),
        historical_analog = tibble::tibble(
          analog_id = character(),
          day = integer(),
          cumulative_return = numeric(),
          match_date = as.Date(character()),
          match_corr = numeric()
        ),
        presets = tibble::tibble(
          id = character(),
          title = character(),
          description = character(),
          flat = numeric(),
          vol = numeric(),
          spread = numeric()
        ),
        .error = error_message
      )
    }

    available_markets <- shiny::reactive({
      markets <- unique(ea_coalesce(filters()$commodities, character(0)))
      markets[nzchar(markets)]
    })

    focus_market <- shiny::reactive({
      market_choices <- available_markets()
      current_selection <- ea_coalesce(input$scenario_focus_market, character(0))

      if (length(current_selection) == 1L && current_selection %in% market_choices) {
        current_selection
      } else if (length(market_choices) > 0L) {
        market_choices[[1]]
      } else {
        character(0)
      }
    })

    scenario_filters <- shiny::reactive({
      current_filters <- filters()
      selected_focus <- focus_market()
      market_set <- unique(ea_coalesce(current_filters$commodities, character(0)))

      if (length(selected_focus) == 1L && nzchar(selected_focus) && selected_focus %in% market_set) {
        market_set <- c(selected_focus, setdiff(market_set, selected_focus))
      }

      current_filters$commodities <- market_set
      current_filters
    })

    scenario_data <- shiny::reactive({
      tryCatch(
        {
          result <- ea_calc_scenarios(
            filters = scenario_filters(),
            shocks = list(
              flat = ea_coalesce(input$flat_price_shock, 0),
              vol = ea_coalesce(input$vol_shock, 0),
              spread = ea_coalesce(input$spread_shock, 0)
            )
          )
          result$.error <- NULL
          result
        },
        error = function(e) empty_scenario_data(conditionMessage(e))
      )
    })

    shiny::observe({
      presets <- scenario_data()$presets
      for (i in seq_len(nrow(presets))) {
        local({
          preset <- presets[i, ]
          shiny::observeEvent(input[[paste0("load_", preset$id)]], {
            shiny::updateSliderInput(session, "flat_price_shock", value = preset$flat)
            shiny::updateSliderInput(session, "vol_shock", value = preset$vol)
            shiny::updateSliderInput(session, "spread_shock", value = preset$spread)
          }, ignoreInit = TRUE)
        })
      }
    })

    output$page <- shiny::renderUI({
      ns <- session$ns
      current_filters <- filters()

      if (length(current_filters$commodities) == 0L) {
        return(htmltools::tagList(
          ea_empty_state_card(
            title = "Select products to populate scenario analysis",
            body = "Scenario Analysis applies shocks, simulation, and analog views across the selected products.",
            hint = "Choose one or more products, then set the focus market inside the page."
          )
        ))
      }

      if (!is.null(scenario_data()$.error)) {
        return(htmltools::tagList(
          ea_empty_state_card(
            title = "Scenario analysis is available, but the payload failed",
            body = "The page is still enabled, but the current scenario response could not populate the charts.",
            hint = scenario_data()$.error
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
                input_id = ns("scenario_focus_market"),
                label = "Focus Market",
                choices = stats::setNames(available_markets(), ea_market_labels(available_markets())),
                selected = focus_market()
              )
            )
          )
        ),
        shiny::uiOutput(ns("preset_cards")),
        ea_standard_card(
          title = "Shock Inputs",
          class = "ea-card--controls",
          full_screen = FALSE,
          bslib::layout_columns(
            col_widths = c(4, 4, 4),
            shiny::sliderInput(ns("flat_price_shock"), "Flat price (%)", min = -30, max = 30, value = 0, step = 1),
            shiny::sliderInput(ns("vol_shock"), "Vol shift (pts)", min = -20, max = 20, value = 0, step = 1),
            shiny::sliderInput(ns("spread_shock"), "Spread shift (%)", min = -10, max = 10, value = 0, step = 1)
          )
        ),
        bslib::layout_columns(
          col_widths = c(8, 4),
          ea_plotly_card(
            "Shock Impact",
            ns("scenario_impact"),
            subtitle = if (nzchar(focus_market())) {
              paste(focus_market(), "base curve versus shocked curve")
            } else {
              "Focused market base curve versus shocked curve"
            },
            height = "300px"
          ),
          ea_plotly_card(
            "Impact Attribution",
            ns("scenario_propagation"),
            subtitle = "Factor contribution of the active shock",
            height = "300px"
          )
        ),
        bslib::layout_columns(
          col_widths = c(7, 5),
          ea_plotly_card(
            "Monte Carlo Simulation",
            ns("mc_fan"),
            subtitle = if (nzchar(focus_market())) {
              paste(focus_market(), "fan chart over the active horizon")
            } else {
              "Focused market fan chart over the active horizon"
            },
            height = "300px"
          ),
          ea_plotly_card(
            "Historical Analogs",
            ns("historical_analog"),
            subtitle = if (nzchar(focus_market())) {
              paste(focus_market(), "closest historical analog windows")
            } else {
              "Focused market closest historical analog windows"
            },
            height = "300px"
          )
        )
      )
    })

    output$preset_cards <- shiny::renderUI({
      ns <- session$ns
      presets <- scenario_data()$presets
      if (nrow(presets) == 0L) return(NULL)

      preset_cards <- lapply(seq_len(nrow(presets)), function(i) {
        preset <- presets[i, ]
        ea_standard_card(
          title = preset$title,
          class = "ea-card--preset",
          full_screen = FALSE,
          htmltools::tags$div(
            class = "ea-preset-card",
            htmltools::tags$div(
              class = "ea-preset-card__stats",
              ea_badge(paste("Flat", preset$flat), tone = "neutral"),
              ea_badge(paste("Vol", preset$vol), tone = "neutral"),
              ea_badge(paste("Spread", preset$spread), tone = "neutral")
            ),
            shiny::actionButton(
              ns(paste0("load_", preset$id)),
              "Load Shock",
              class = "btn btn-sm btn-outline-secondary ea-preset-card__button"
            )
          )
        )
      })

      cols <- min(3L, nrow(presets))
      col_widths <- rep(12L %/% cols, cols)
      do.call(bslib::layout_columns, c(preset_cards, list(col_widths = col_widths)))
    })

    output$scenario_impact <- plotly::renderPlotly({
      ic <- scenario_data()$impact_curve
      fig <- plotly::plot_ly()

      if (nrow(ic) == 0L) {
        return(ea_plotly_layout(fig, x_title = "Contract Month", y_title = "Price"))
      }

      fig <- fig %>%
        plotly::add_lines(
          data = ic,
          x = ~tenor,
          y = ~base,
          name = "Base",
          line = list(color = "#7f8b99", width = 1.7, dash = "dot"),
          hovertemplate = "Tenor %{x}<br>%{y:.2f}<extra>%{fullData.name}</extra>"
        ) %>%
        plotly::add_lines(
          data = ic,
          x = ~tenor,
          y = ~impact,
          name = "Shocked",
          line = list(color = "#4da3a3", width = 2.4),
          hovertemplate = "Tenor %{x}<br>%{y:.2f}<extra>%{fullData.name}</extra>"
        )

      ea_plotly_layout(fig, x_title = "Contract Month", y_title = "Curve Level")
    })

    output$scenario_propagation <- plotly::renderPlotly({
      prop <- scenario_data()$propagation
      if (nrow(prop) == 0L) {
        return(ea_plotly_layout(plotly::plot_ly(), x_title = NULL, y_title = "Contribution"))
      }

      bar_colors <- ifelse(prop$contribution >= 0, "#4da3a3", "#b35c60")
      fig <- plotly::plot_ly(
        data = prop,
        x = ~factor,
        y = ~contribution,
        type = "bar",
        marker = list(color = bar_colors),
        hovertemplate = "%{x}<br>%{y:.2f}<extra>contribution</extra>"
      )

      ea_plotly_layout(fig, x_title = NULL, y_title = "Contribution", hovermode = "closest")
    })

    output$mc_fan <- plotly::renderPlotly({
      ps <- scenario_data()$price_simulations
      fig <- plotly::plot_ly()

      if (nrow(ps) == 0L) {
        fig <- fig %>% plotly::add_annotations(text = "Insufficient data for simulation", showarrow = FALSE)
        return(ea_plotly_layout(fig, x_title = "Days", y_title = "Price"))
      }

      primary_mkt <- focus_market()
      if (!nzchar(primary_mkt) || !primary_mkt %in% ps$market) {
        primary_mkt <- ps$market[[1]]
      }

      df <- ps[ps$market == primary_mkt, , drop = FALSE]
      if (nrow(df) == 0L) {
        return(ea_plotly_layout(plotly::plot_ly(), x_title = "Days", y_title = "Price"))
      }

      bands <- df %>%
        dplyr::group_by(.data$t) %>%
        dplyr::summarise(
          p10 = stats::quantile(.data$price, 0.10, na.rm = TRUE),
          p25 = stats::quantile(.data$price, 0.25, na.rm = TRUE),
          p50 = stats::quantile(.data$price, 0.50, na.rm = TRUE),
          p75 = stats::quantile(.data$price, 0.75, na.rm = TRUE),
          p90 = stats::quantile(.data$price, 0.90, na.rm = TRUE),
          .groups = "drop"
        )

      fig <- fig %>%
        plotly::add_ribbons(
          data = bands,
          x = ~t,
          ymin = ~p10,
          ymax = ~p90,
          name = "10-90%",
          line = list(color = "transparent"),
          fillcolor = "rgba(77,163,163,0.12)",
          showlegend = TRUE,
          hoverinfo = "skip"
        ) %>%
        plotly::add_ribbons(
          data = bands,
          x = ~t,
          ymin = ~p25,
          ymax = ~p75,
          name = "25-75%",
          line = list(color = "transparent"),
          fillcolor = "rgba(77,163,163,0.25)",
          showlegend = TRUE,
          hoverinfo = "skip"
        ) %>%
        plotly::add_lines(
          data = bands,
          x = ~t,
          y = ~p50,
          name = "Median",
          line = list(color = "#4da3a3", width = 2),
          hovertemplate = "Day %{x:.0f}<br>Median: %{y:.2f}<extra></extra>"
        )

      ea_plotly_layout(fig, x_title = "Days", y_title = "Price")
    })

    output$historical_analog <- plotly::renderPlotly({
      ha <- scenario_data()$historical_analog
      palette <- c("#4da3a3", "#5a85c8", "#d2a157")
      fig <- plotly::plot_ly()

      if (nrow(ha) == 0L) {
        fig <- fig %>% plotly::add_annotations(text = "Insufficient history for analog matching", showarrow = FALSE)
        return(ea_plotly_layout(fig, x_title = "Days forward", y_title = "Cumulative return"))
      }

      analog_ids <- unique(ha$analog_id)
      for (i in seq_along(analog_ids)) {
        aid <- analog_ids[[i]]
        df <- ha[ha$analog_id == aid, , drop = FALSE]
        col <- palette[[((i - 1L) %% length(palette)) + 1L]]
        lbl <- if (nrow(df) > 0L && !is.na(df$match_date[1])) {
          paste0(aid, " (", format(df$match_date[1], "%b %Y"), ")")
        } else {
          aid
        }

        fig <- fig %>%
          plotly::add_lines(
            data = df,
            x = ~day,
            y = ~cumulative_return,
            name = lbl,
            line = list(color = col, width = 1.8),
            hovertemplate = "Day %{x}<br>Cumulative return: %{y:.2%}<extra>%{fullData.name}</extra>"
          )
      }

      ea_plotly_layout(fig, x_title = "Days forward", y_title = "Cumulative return")
    })
  })
}
