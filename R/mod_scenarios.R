mod_scenarios_ui <- function(id) {
  shiny::uiOutput(shiny::NS(id, "page"))
}

mod_scenarios_server <- function(id, filters, data_timestamp) {
  shiny::moduleServer(id, function(input, output, session) {

    scenario_data <- shiny::reactive({
      ea_calc_scenarios(
        filters = filters(),
        shocks = list(
          flat   = ea_coalesce(input$flat_price_shock, 0),
          vol    = ea_coalesce(input$vol_shock, 0),
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

    # Preset observer \u2014 use presets from calc layer
    shiny::observe({
      presets <- scenario_data()$presets
      for (i in seq_len(nrow(presets))) {
        local({
          preset <- presets[i, ]
          shiny::observeEvent(input[[paste0("load_", preset$id)]], {
            shiny::updateSliderInput(session, "flat_price_shock", value = preset$flat)
            shiny::updateSliderInput(session, "vol_shock",        value = preset$vol)
            shiny::updateSliderInput(session, "spread_shock",     value = preset$spread)
          }, ignoreInit = TRUE)
        })
      }
    })

    output$page <- shiny::renderUI({
      ns <- session$ns
      current_filters <- filters()

      if (length(current_filters$commodities) == 0L) {
        return(htmltools::tagList(
          ea_selected_filters_ribbon(current_filters, data_timestamp()),
          ea_empty_state_card(
            title = "Select products to populate scenario analysis",
            body = "Scenario Analysis applies Monte Carlo simulation, historical analogs, and stress tests across the selected products.",
            hint = "Preset shocks and manual inputs feed the impact and attribution views."
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
            shiny::sliderInput(ns("flat_price_shock"), "Flat price (%)", min = -30, max = 30, value = 0, step = 1),
            shiny::sliderInput(ns("vol_shock"),        "Vol shift (pts)",   min = -20, max = 20, value = 0, step = 1),
            shiny::sliderInput(ns("spread_shock"),     "Spread shift (%)", min = -10, max = 10, value = 0, step = 1)
          )
        ),
        mod_kpi_strip_ui(ns("kpi_strip")),
        # Row 1: Shock Impact | Factor Attribution
        bslib::layout_columns(
          col_widths = c(8, 4),
          ea_plotly_card("Shock Impact", "Base versus shocked term structure.", ns("scenario_impact"), height = "360px"),
          ea_plotly_card("Impact Attribution", "Contribution by risk driver.", ns("scenario_propagation"), height = "360px")
        ),
        # Row 2: GBM Fan Chart | Return Distribution
        bslib::layout_columns(
          col_widths = c(7, 5),
          ea_plotly_card("Monte Carlo Simulation", "GBM price fan chart with percentile bands.", ns("mc_fan"), height = "340px"),
          ea_plotly_card("Return Distribution", "Terminal return histogram from simulation.", ns("return_dist"), height = "340px")
        ),
        # Row 3: Historical Analogs | Stress Table
        bslib::layout_columns(
          col_widths = c(7, 5),
          ea_plotly_card("Historical Analogs", "3 most similar prior periods \u2014 forward path replay.", ns("historical_analog"), height = "340px"),
          ea_plotly_card("Stress VaR", "Normal vs stress correlation regime.", ns("stress_var"), height = "340px")
        ),
        mod_footer_notes_ui(ns("footer_notes"))
      )
    })

    output$preset_cards <- shiny::renderUI({
      ns <- session$ns
      presets <- scenario_data()$presets
      if (nrow(presets) == 0) return(NULL)
      preset_cards <- lapply(seq_len(nrow(presets)), function(i) {
        preset <- presets[i, ]
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
              ea_badge(paste("Vol", preset$vol), tone = "neutral"),
              ea_badge(paste("Spread", preset$spread), tone = "neutral")
            ),
            shiny::actionButton(
              ns(paste0("load_", preset$id)), "Load Shock",
              class = "btn btn-sm btn-outline-secondary ea-preset-card__button"
            )
          )
        )
      })
      cols <- min(3L, nrow(presets))
      col_widths <- rep(12L %/% cols, cols)
      do.call(bslib::layout_columns, c(preset_cards, list(col_widths = col_widths)))
    })

    # ---- Shock Impact ----
    output$scenario_impact <- plotly::renderPlotly({
      ic <- scenario_data()$impact_curve
      fig <- plotly::plot_ly()
      if (nrow(ic) == 0L) return(ea_plotly_layout(fig, x_title = "Contract month", y_title = "Price"))
      fig <- fig |>
        plotly::add_lines(data = ic, x = ~tenor, y = ~base, name = "Base",
          line = list(color = "#7f8b99", width = 1.7, dash = "dot"),
          hovertemplate = "Tenor %{x}<br>%{y:.2f}<extra>%{fullData.name}</extra>") |>
        plotly::add_lines(data = ic, x = ~tenor, y = ~impact, name = "Shocked",
          line = list(color = "#4da3a3", width = 2.4),
          hovertemplate = "Tenor %{x}<br>%{y:.2f}<extra>%{fullData.name}</extra>")
      ea_plotly_layout(fig, x_title = "Contract Month", y_title = "Curve Level")
    })

    # ---- Factor Attribution ----
    output$scenario_propagation <- plotly::renderPlotly({
      prop <- scenario_data()$propagation
      if (nrow(prop) == 0L) return(ea_plotly_layout(plotly::plot_ly(), x_title = NULL, y_title = "Contribution"))
      bar_colors <- ifelse(prop$contribution >= 0, "#4da3a3", "#b35c60")
      fig <- plotly::plot_ly(data = prop, x = ~factor, y = ~contribution, type = "bar",
        marker = list(color = bar_colors),
        hovertemplate = "%{x}<br>%{y:.2f}<extra>contribution</extra>")
      ea_plotly_layout(fig, x_title = NULL, y_title = "Contribution", hovermode = "closest")
    })

    # ---- Monte Carlo Fan Chart ----
    output$mc_fan <- plotly::renderPlotly({
      ps <- scenario_data()$price_simulations
      fig <- plotly::plot_ly()
      if (nrow(ps) == 0L) {
        fig <- fig |> plotly::add_annotations(text = "Insufficient data for simulation", showarrow = FALSE)
        return(ea_plotly_layout(fig, x_title = "Days", y_title = "Price"))
      }
      primary_mkt <- ps$market[1]
      df <- ps[ps$market == primary_mkt, , drop = FALSE]
      if (nrow(df) == 0L) return(ea_plotly_layout(plotly::plot_ly(), x_title = "Days", y_title = "Price"))
      # Compute percentile bands across sims at each t
      bands <- df |>
        dplyr::group_by(.data$t) |>
        dplyr::summarise(
          p10 = stats::quantile(.data$price, 0.10, na.rm = TRUE),
          p25 = stats::quantile(.data$price, 0.25, na.rm = TRUE),
          p50 = stats::quantile(.data$price, 0.50, na.rm = TRUE),
          p75 = stats::quantile(.data$price, 0.75, na.rm = TRUE),
          p90 = stats::quantile(.data$price, 0.90, na.rm = TRUE),
          .groups = "drop"
        )
      fig <- fig |>
        plotly::add_ribbons(data = bands, x = ~t, ymin = ~p10, ymax = ~p90,
          name = "10-90%", line = list(color = "transparent"),
          fillcolor = "rgba(77,163,163,0.12)", showlegend = TRUE, hoverinfo = "skip") |>
        plotly::add_ribbons(data = bands, x = ~t, ymin = ~p25, ymax = ~p75,
          name = "25-75%", line = list(color = "transparent"),
          fillcolor = "rgba(77,163,163,0.25)", showlegend = TRUE, hoverinfo = "skip") |>
        plotly::add_lines(data = bands, x = ~t, y = ~p50,
          name = "Median", line = list(color = "#4da3a3", width = 2),
          hovertemplate = "Day %{x:.0f}<br>Median: %{y:.2f}<extra></extra>")
      ea_plotly_layout(fig, x_title = "Days", y_title = "Price")
    })

    # ---- Return Distribution Histogram ----
    output$return_dist <- plotly::renderPlotly({
      rd <- scenario_data()$return_distribution
      fig <- plotly::plot_ly()
      if (nrow(rd) == 0L) {
        fig <- fig |> plotly::add_annotations(text = "No simulation data", showarrow = FALSE)
        return(ea_plotly_layout(fig, x_title = "Log return", y_title = "Count"))
      }
      primary_mkt <- rd$market[1]
      rd_row <- rd[rd$market == primary_mkt, , drop = FALSE]
      if (nrow(rd_row) == 0 || length(rd_row$returns[[1]]) == 0) {
        return(ea_plotly_layout(fig, x_title = "Log return", y_title = "Count"))
      }
      ret_vals <- rd_row$returns[[1]]
      var_line <- rd_row$var_95[1]
      fig <- plotly::plot_ly() |>
        plotly::add_histogram(x = ret_vals, name = "Returns",
          marker = list(color = "#4da3a3", line = list(color = "#17202b", width = 0.5)),
          hovertemplate = "%{x:.3f}<br>Count: %{y}<extra></extra>") |>
        plotly::layout(shapes = list(
          list(type = "line", x0 = var_line, x1 = var_line, y0 = 0, y1 = 1,
               yref = "paper", line = list(color = "#b35c60", width = 1.5, dash = "dash"))
        ))
      ea_plotly_layout(fig, x_title = "Log return", y_title = "Count")
    })

    # ---- Historical Analogs ----
    output$historical_analog <- plotly::renderPlotly({
      ha <- scenario_data()$historical_analog
      palette <- c("#4da3a3", "#5a85c8", "#d2a157")
      fig <- plotly::plot_ly()
      if (nrow(ha) == 0L) {
        fig <- fig |> plotly::add_annotations(text = "Insufficient history for analog matching", showarrow = FALSE)
        return(ea_plotly_layout(fig, x_title = "Days forward", y_title = "Cum. return"))
      }
      for (i in seq_along(unique(ha$analog_id))) {
        aid <- unique(ha$analog_id)[[i]]
        df <- ha[ha$analog_id == aid, , drop = FALSE]
        col <- palette[[((i - 1L) %% length(palette)) + 1L]]
        lbl <- paste0(aid, " (", format(df$match_date[1], "%b %Y"), ")")
        fig <- fig |>
          plotly::add_lines(data = df, x = ~day, y = ~cumulative_return,
            name = lbl, line = list(color = col, width = 1.8),
            hovertemplate = "Day %{x}<br>Cum. return: %{y:.2%}<extra>%{fullData.name}</extra>")
      }
      ea_plotly_layout(fig, x_title = "Days forward", y_title = "Cumulative return")
    })

    # ---- Stress VaR (correlation stress table as scatter) ----
    output$stress_var <- plotly::renderPlotly({
      cs <- scenario_data()$correlation_stress
      fig <- plotly::plot_ly()
      if (nrow(cs) == 0L) {
        fig <- fig |> plotly::add_annotations(text = "Need 3+ markets for correlation stress", showarrow = FALSE)
        return(ea_plotly_layout(fig, x_title = "Normal corr", y_title = "Stress corr"))
      }
      # Filter to cross pairs only
      cs_cross <- cs |> dplyr::filter(.data$market_x != .data$market_y)
      if (nrow(cs_cross) == 0L) return(ea_plotly_layout(fig, x_title = "Normal corr", y_title = "Stress corr"))
      cs_cross <- cs_cross |>
        dplyr::mutate(pair_label = paste(.data$market_x, "vs", .data$market_y))
      fig <- plotly::plot_ly(data = cs_cross, x = ~corr_normal, y = ~corr_stress,
        type = "scatter", mode = "markers+text",
        text = ~pair_label, textposition = "top center",
        marker = list(color = "#4da3a3", size = 8),
        hovertemplate = "%{text}<br>Normal: %{x:.2f}<br>Stress: %{y:.2f}<extra></extra>") |>
        plotly::layout(shapes = list(
          list(type = "line", x0 = -1, x1 = 1, y0 = -1, y1 = 1,
               line = list(color = "#7f8b99", width = 1, dash = "dot"))
        ))
      ea_plotly_layout(fig, x_title = "Normal corr", y_title = "Stress corr", hovermode = "closest")
    })
  })
}
